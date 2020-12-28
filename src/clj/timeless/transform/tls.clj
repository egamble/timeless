(ns timeless.transform.tls
  "Transform an AST to produce TLS code."
  (:require [timeless.transform.utils :refer :all]
            [instaparse.core :as insta]
            [clojure.string :as str]))


;; Note: UUIDs are used for gensyms, for now, to avoid collisions not only in the current file,
;; but with other TLS files that could be concatenated. Shorter gensym names are also possible, but
;; require more complicated mechanisms for avoiding collisions.


;; TODO:

;; - Test various combinations of guards, arrows and embeddeds, and check the metadata.
;; - Rewrite make-gensym-comparison.
;; - Clean up and comment code.


(defn break-off-guards [exps]
  (if (has-type :guard-op (second exps))
    (let [[guard-exps exps-after-guards]
          (break-off-guards (third-on exps)) ; recur with the remaining exps, starting with the third exp,
                                        ; i.e. the exp after the :guard-op
          ]
      [(cons (first exps)
             guard-exps)
       exps-after-guards])
    [(list (first exps))
     (rest exps)]))


(defn make-and [exps]
  (v :and
     (get-meta (first exps))
     exps))


(defn build-leading-guard [exps]
  (let [[guard-exps exps-after-guards] (break-off-guards exps)
        [exp & guards] guard-exps]
    (if (seq guards)
      (cons [:apply-guard (get-meta exp)
             [:name (get-meta (second exps)) "|"]
             exp
             (if (second guards)
               (make-and guards)
               (first guards))]
            exps-after-guards)
      exps)))


(defn reform-clause [exps]
  (let [exps (build-leading-guard exps)
        m (get-meta (first exps))
        arrow-op (second exps)]
    [:bind m [] ; the names will be filled in later, and if there are no names, the :bind will be removed
     (if arrow-op
       (let [remaining-exps (third-on exps) ; remaining exps, starting with the third exp,
                                        ; i.e. the exp after the :arrow-op
             ]
         [:apply-arrow m
          [:name (get-meta arrow-op) "->"]
          (first exps)
          (reform-clause remaining-exps)])
       (first exps))]))


(defn count-binds [clause]
  (cond
    (= :bind (first clause))
    (let [bind-exp (fourth clause)]
      (if (= :apply-arrow (first bind-exp))
        (+ 1 (count-binds (fifth bind-exp))) ; (fifth bind-exp) is the :apply-arrow's right-exp
        1))

    (= :values (first clause))
    (let [exps (all-args clause)]
      (count-binds (last exps)))

    :else
    (error-meta (get-meta clause)
                "internal error counting binds")))


(defn join-with-clause [reformed-clause joinable-clause index]
  (let [[_ m & exps] reformed-clause
        is-bind (has-type :bind reformed-clause)]
    (if (= 0 index)
      (v :values m
         (if is-bind
           (list reformed-clause
                 joinable-clause)
           
           ;; else it's a :values expression
           (cons-at-end exps
                        joinable-clause)))

      ;; else index > 0
      (let [f (fn [[_ m1 _ ; :bind m1 []
                   [_ m2 op left-arg next-clause]]] ; :apply-arrow
                [:bind m1 []
                 [:apply-arrow m2
                  op
                  left-arg
                  (join-with-clause next-clause
                                    joinable-clause
                                    (- index 1))]])]
        (if is-bind
          (f reformed-clause)

          ;; else it's a :values expression
          (let [exps-but-last (butlast exps)
                last-exp (last exps)]
            [:values m (cons-at-end exps-but-last
                                    (f last-exp))]))))))


(defn guard-assertions [guard-exp]
  (let [right-exp (fifth guard-exp)]
    (if (has-type :and right-exp)
      (all-args right-exp)
      (list right-exp))))


(defn break-up-prev-guard [prev-guard]
  (let [[_ _ op left-exp _] prev-guard]
    (cond
      (has-type :apply-guard prev-guard)
      [op left-exp (guard-assertions prev-guard)]
      
      (has-type :apply-arrow prev-guard)
      (break-up-prev-guard left-exp)

      :else
      [nil prev-guard ()])))


(defn get-prev-guard [reformed-clause index]
  (let [[_ m & exps] reformed-clause
        is-bind (has-type :bind reformed-clause)]
    (if (= 0 index)
      (second ; skip the <names> of the :bind
       (if is-bind
         exps

         ;; else it's a :values expression containing :bind expressions, so get the arguments of the last :bind
         (all-args (last exps))))

      ;; else index > 0
      (let [f (fn [[_ _ _ ; :bind m []
                   [_ _ _ _ next-clause] ; :apply-arrow m op left-arg, then next-clause is the right-arg
                   ]]
                (get-prev-guard next-clause
                                (- index 1)))]
        (if is-bind
          (f reformed-clause)

          ;; else it's a :values expression
          (f (last exps)))))))


(defn copy-and-build-guard [prev-reformed-clause index assertions]
  (let [prev-guard
        (get-prev-guard prev-reformed-clause index)

        [prev-op prev-left-exp prev-assertions]
        (break-up-prev-guard prev-guard)

        num-diff (- (count prev-assertions)
                    (count assertions))]
    (when (< num-diff 0)
      (error-meta (get-meta (first assertions))
                  "truncated clause has more guard assertions than previous clause"))
    (let [all-assertions (concat (take num-diff prev-assertions)
                                 assertions)
          m (get-meta prev-left-exp)]
      [:apply-guard m
       (or prev-op
           [:name m "|"])
       prev-left-exp
       (if (> (count all-assertions) 1)
         (make-and all-assertions)
         (first all-assertions))])))


(defn break-off-assertions [exps]
  (if (has-type :guard-op (first exps))
    (let [assertion (second exps)

          [assertions remaining-exps]
          (break-off-assertions (third-on exps))]
      [(cons assertion assertions)
       remaining-exps])

    ;; else it's an :arrow-op or empty
    [() exps]))


(defn reform-clause-with-truncated-guard [prev-reformed-clause index exps]
  (let [[assertions remaining-exps] (break-off-assertions exps)
        guard (copy-and-build-guard prev-reformed-clause index assertions)]
    (reform-clause (cons guard remaining-exps))))


(defn reform-truncated-clause [prev-reformed-clause next-clause]
  (let [num-binds (count-binds prev-reformed-clause)
        [_ m & exps] next-clause
        num-arrow-ops (reduce #(if (= :arrow-op (first %2))
                                 (+ 1 %1)
                                 %1)
                              0
                              exps)
        
        _ (when (> (+ 1 num-arrow-ops) num-binds)
            (error-meta m "truncated clause has more arrows than previous clause"))

        is-truncated-guard (has-type :guard-op (first exps))
        join-index (- num-binds (if is-truncated-guard
                                  (+ 1 num-arrow-ops)
                                  num-arrow-ops))
        joinable-clause (if is-truncated-guard
                          (reform-clause-with-truncated-guard prev-reformed-clause join-index exps)
                          (reform-clause (rest exps)) ; omit the :arrow-op
                          )]
    (join-with-clause prev-reformed-clause joinable-clause join-index)))


(defn reduce-clauses [reformed-clauses next-clause]
  (let [[_ m & exps] next-clause]
    (if (has-types #{:arrow-op :guard-op} (first exps))
      (if (seq reformed-clauses)
        (let [prev-reformed-clause (first reformed-clauses)]
          (cons (reform-truncated-clause prev-reformed-clause next-clause)
                (rest reformed-clauses)))
        (error-meta m "first clause can't be truncated"))
      (cons (reform-clause exps)
            reformed-clauses))))


(defn transform-set [m & clauses]
  (let [new-clauses (reduce reduce-clauses () clauses)]
    (v :set m
       (reverse new-clauses))))


(defn rebuild-sets [assertions]
  (let [trans-map {:set transform-set}]
    (map (partial insta/transform trans-map) assertions)))


(defn operation->apply [m left-exp op right-exp]
  [:apply m
   [:name (get-meta op)
    (first-arg op)]
   left-exp
   right-exp])


(def arrow-or-guard->key {:arrow-op :apply-arrow
                          :guard-op :apply-guard})

(def arrow-or-guard->name {:arrow-op "->"
                           :guard-op "|"})


(defn make-op-name [op]
  [:name (get-meta op)
   (or (arrow-or-guard->name (first op))
       (first-arg op))])


(defn transform-ast-operation [m left-exp op right-exp]
  [(or (arrow-or-guard->key (first op))
       :apply)
   m
   (make-op-name op)
   left-exp
   right-exp])


(defn transform-ast-left-section [m left-exp op]
  [:apply m
   (make-op-name op)
   left-exp])


(defn transform-ast-right-section [m op right-exp]
  (if (and (> (count op) 2)
           (= "-" (first-arg op)))
    [:apply m
     [:neg (get-meta op)]
     right-exp]
    [:apply m
     [:flip m]
     (make-op-name op)
     right-exp]))


(defn transform-ast-prefix-op [m op]
  (make-op-name op))


;; TODO: rewrite this with reverse, for efficiency
(defn make-gensym-comparison [[new-exps extra-comparisons] pair]
  (let [[exp op] pair

        [new-pair new-extra-comparisons]
        (if (has-type :name exp)
          [pair ()]
          (let [new-name (uuid)
                m (get-meta exp)]
            [(list [:name m new-name] op)
             (list
              [:apply m
               [:name m "="]
               [:name m new-name]
               exp])]))]
    [(concat new-exps new-pair)
     (concat extra-comparisons new-extra-comparisons)]))


(defn transform-chain [m & exps]
  (let [[new-exps extra-comparisons]
        (reduce make-gensym-comparison
                  [() ()]
                  (partition 2 exps))

        comparison-triples (partition 3 2
                                      (cons-at-end new-exps
                                                   (last exps)))]
    (v :and m
       (concat extra-comparisons
               (map (fn [comparison-triple]
                      (apply operation->apply (get-meta (first comparison-triple))
                             comparison-triple))
                    comparison-triples)))))


(defn transform-embedded [m left-exp op right-exp]
  (let [is-name (has-type :name left-exp)
        is-underscore (and is-name
                           (= "_" (first-arg left-exp)))
        name-exp (if (or (not is-name)
                         is-underscore)
                   [:name m (uuid)]
                   left-exp)]
    [:apply-guard m
     [:name m "|"]
     name-exp
     (if is-name
       [:apply m
        op
        name-exp
        right-exp]
       [:and m
        [:apply m
         [:name m "="]
         name-exp
         left-exp]
        [:apply m
         op
         name-exp
         right-exp]])]))


(defn combine-guards [m op left-exp right-exp]
  (let [guard [:apply-guard m
               op
               left-exp
               right-exp]]
    (if (has-type :apply-guard left-exp)
      (let [[_ _ inner-op inner-left] left-exp]
        [:apply-guard m
         inner-op
         inner-left
         (make-and (concat (guard-assertions left-exp)
                           (guard-assertions guard)))])
      guard)))


(defn remove-flip [exps]
  (if (and (has-type :flip (first exps))
           (> (count exps) 3))
    ;; remove the :flip and swap the third and fourth expressions
    (let [[_ e2 e3 e4 & r] exps]
      (apply list e2 e4 e3 r))
    exps))


(defn combine-applys [m & exps]
  (let [first-exp (first exps)]
    (v :apply m
       (remove-flip
        (if (has-type :apply first-exp)
          (concat (all-args first-exp)
                  (rest exps))
          exps)))))


(defn combine-ands [m & exps]
  (make-and (mapcat (fn [exp]
                      (if (has-type :and exp)
                        (all-args exp)
                        (list exp)))
                    exps)))


;; If the right side of an arrow application within a :bind is not already wrapped in a :bind, wrap it.
(defn bind-arrow-right [m _ bind-arg]
  (if (and (has-type :apply bind-arg)
           (has-type :name (first-arg bind-arg))
           (= "->" (first-arg (first-arg bind-arg)))
           (not (has-type :bind (third-arg bind-arg))))
    (let [[_ m2 op left-exp right-exp] bind-arg
          new-bind [:bind (get-meta right-exp) []
                    right-exp]]
      [:bind m []
       [:apply m2
        op
        left-exp
        (apply bind-arrow-right (rest new-bind))]])
    [:bind m [] bind-arg]))


(defn transformations-1 [assertions]
  (let [trans-map {:operation transform-ast-operation
                   :left-section transform-ast-left-section
                   :right-section transform-ast-right-section
                   :prefix-op transform-ast-prefix-op
                   :embedded transform-embedded
                   :chain transform-chain}]
    (map (partial insta/transform trans-map) assertions)))


(defn transformations-2 [assertions]
  (let [trans-map {:apply-guard combine-guards
                   :and combine-ands}]
    (map (partial insta/transform trans-map) assertions)))


(defn transformations-3 [assertions]
  (let [trans-map {:apply-guard (fn [m & r]
                                  (apply vector :apply m r))
                   :apply-arrow (fn [m & r]
                                  (apply vector :apply m r))}]
    (map (partial insta/transform trans-map) assertions)))


(defn transformations-4 [assertions]
  (let [trans-map {:apply combine-applys

                   ;; Can't be done earlier, o.w. | {((a ->) b)} doesn't get an inner :bind.
                   :bind bind-arrow-right}]
    (map (partial insta/transform trans-map) assertions)))


(defn transformations-5 [assertions]
  (let [trans-map {:apply (fn [m & exps]
                            ;; The :apply expression becomes a list.
                            ;; Can't just return exps, because it's not a list.
                            (apply list exps))}]
    (map (partial insta/transform trans-map) assertions)))


(defn find-names-to-bind [outer-name-set exp]
  (cond
    (has-type :name exp)
    (let [name (first-arg exp)]
      (if (outer-name-set name)
        #{}
        #{name}))

    (has-type :bind exp)
    #{}

    (sequential? exp)
    (reduce (fn [inner-name-set sub-exp]
              (into inner-name-set
                    (find-names-to-bind outer-name-set sub-exp)))
            #{}
            exp)

    :else
    #{}))


;; For the first call, exp is all the assertions.
(defn fill-bind-names [outer-name-set exp]
  (cond
    (has-type :bind exp)
    (let [[_ m _ bind-exp] exp
          inner-name-set (find-names-to-bind outer-name-set bind-exp)
          new-bind-exp (fill-bind-names (into outer-name-set inner-name-set)
                                        bind-exp)]
      (if (empty? inner-name-set)
        new-bind-exp
        [:bind m
         (into [] inner-name-set)
         new-bind-exp]))

    (seq? exp)
    (apply list (map (partial fill-bind-names outer-name-set)
                     exp))

    (vector? exp)
    (apply vector (map (partial fill-bind-names outer-name-set)
                       exp))

    :else
    exp))


(defn top-level-fill-bind-names [assertions]
  (let [name-set (find-names-to-bind #{} assertions)]
    (fill-bind-names name-set assertions)))


;; Returns: <assertions>
(defn ast->tls [assertions]
  (->> assertions
       rebuild-sets
       transformations-1
       transformations-2
       transformations-3
       transformations-4
       transformations-5
       top-level-fill-bind-names))
