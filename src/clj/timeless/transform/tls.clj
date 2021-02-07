(ns timeless.transform.tls
  "Transform an AST to produce TLS code."
  (:require [timeless.transform.grammar :refer [predefined-names]]
            [timeless.utils :refer :all]
            [clojure.string :as str]))


;; Note: UUIDs are used for gensyms, for now, to avoid collisions not only in the current file,
;; but with other TLS files that could be concatenated. Shorter gensym names are also possible, but
;; require more complicated mechanisms for avoiding collisions.


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
  (with-meta
    (into [:and] exps)
    (meta (first exps))))


(defn build-leading-guard [exps]
  (let [[guard-exps exps-after-guards] (break-off-guards exps)
        [exp & guards] guard-exps]
    (if (seq guards)
      (cons (with-meta
              [:apply-guard
               (with-meta
                 [:name "|"]
                 (meta (second exps)))
               exp
               (if (second guards)
                 (make-and guards)
                 (first guards))]
              (meta exp))
            exps-after-guards)
      exps)))


(defn reform-clause [exps]
  (let [exps (build-leading-guard exps)
        m (meta (first exps))
        arrow-op (second exps)]
    (with-meta
      [:bind [] ; the names will be filled in later, and if there are no names, the :bind will be removed
       (if arrow-op
         (let [remaining-exps (third-on exps) ; remaining exps, starting with the third exp,
                                        ; i.e. the exp after the :arrow-op
               ]
           (with-meta
             [:apply-arrow
              (with-meta
                [:name "->"]
                (meta arrow-op))
              (first exps)
              (reform-clause remaining-exps)]
             m))
         (first exps))]
      m)))


(defn count-binds [clause]
  (cond
    (has-type :bind clause)
    (let [bind-exp (second-arg clause)]
      (if (has-type :apply-arrow bind-exp)
        (+ 1 (count-binds (third-arg bind-exp))) ; (third-arg bind-exp) is the :apply-arrow's right-exp
        1))

    (has-type :vals clause)
    (let [exps (all-args clause)]
      (count-binds (last exps)))

    :else
    (error-at "internal error counting binds"
              clause)))


(defn join-with-clause [reformed-clause joinable-clause index]
  (let [m (meta reformed-clause)
        exps (all-args reformed-clause)
        is-bind (has-type :bind reformed-clause)]
    (if (= 0 index)
      (with-meta
        (into [:vals]
           (if is-bind
             (list reformed-clause
                   joinable-clause)
             
             ;; else it's a :vals expression
             (cons-at-end exps
                          joinable-clause)))
        m)

      ;; else index > 0
      (let [f (fn [bind]
                (let [apply-arrow (second-arg bind)
                      [_ op left-arg next-clause] apply-arrow]
                  (with-meta
                    [:bind []
                     (with-meta
                       [:apply-arrow
                        op
                        left-arg
                        (join-with-clause next-clause
                                          joinable-clause
                                          (- index 1))]
                       (meta apply-arrow))]
                    (meta bind))))]
        (if is-bind
          (f reformed-clause)

          ;; else it's a :vals expression
          (let [exps-but-last (butlast exps)
                last-exp (last exps)]
            (with-meta
              [:vals (cons-at-end exps-but-last
                                  (f last-exp))]
              m)))))))


(defn guard-assertions [guard-exp]
  (let [right-exp (third-arg guard-exp)]
    (if (has-type :and right-exp)
      (all-args right-exp)
      (list right-exp))))


(defn break-up-prev-guard [prev-guard]
  (let [[_ op left-exp _] prev-guard]
    (cond
      (has-type :apply-guard prev-guard)
      [op left-exp (guard-assertions prev-guard)]
      
      (has-type :apply-arrow prev-guard)
      (break-up-prev-guard left-exp)

      :else
      [nil prev-guard ()])))


(defn get-prev-guard [reformed-clause index]
  (let [exps (all-args reformed-clause)
        is-bind (has-type :bind reformed-clause)]
    (if (= 0 index)
      (second ; skip the <names> of the :bind
       (if is-bind
         exps

         ;; else it's a :vals expression containing :bind expressions, so get the arguments of the last :bind
         (all-args (last exps))))

      ;; else index > 0
      (let [f (fn [[_ _ ; :bind []
                   [_ _ _ next-clause] ; :apply-arrow op left-arg, then next-clause is the right-arg
                   ]]
                (get-prev-guard next-clause
                                (- index 1)))]
        (if is-bind
          (f reformed-clause)

          ;; else it's a :vals expression
          (f (last exps)))))))


(defn copy-and-build-guard [prev-reformed-clause index assertions]
  (let [prev-guard
        (get-prev-guard prev-reformed-clause index)

        [prev-op prev-left-exp prev-assertions]
        (break-up-prev-guard prev-guard)

        num-diff (- (count prev-assertions)
                    (count assertions))]
    (when (< num-diff 0)
      (error-at "truncated clause has more guard assertions than previous clause"
                (first assertions)))
    (let [all-assertions (concat (take num-diff prev-assertions)
                                 assertions)
          m (meta prev-left-exp)]
      (with-meta
        [:apply-guard
         (or prev-op
             (with-meta
               [:name "|"]
               m))
         prev-left-exp
         (if (> (count all-assertions) 1)
           (make-and all-assertions)
           (first all-assertions))]
        m))))


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
        exps (all-args next-clause)
        num-arrow-ops (reduce #(if (= :arrow-op (first %2))
                                 (+ 1 %1)
                                 %1)
                              0
                              exps)
        
        _ (when (> (+ 1 num-arrow-ops) num-binds)
            (error-at "truncated clause has more arrows than previous clause"
                      next-clause))

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
  (let [exps (all-args next-clause)]
    (if (has-types #{:arrow-op :guard-op} (first exps))
      (if (seq reformed-clauses)
        (let [prev-reformed-clause (first reformed-clauses)]
          (cons (reform-truncated-clause prev-reformed-clause next-clause)
                (rest reformed-clauses)))
        (error-at "first clause can't be truncated"
                  next-clause))
      (cons (reform-clause exps)
            reformed-clauses))))


(defn transform-set [exp]
  (let [clauses (all-args exp)
        new-clauses (reduce reduce-clauses () clauses)]
    (with-meta
      (into [:set]
            (reverse new-clauses))
      (meta exp))))


(def arrow-or-guard->key {:arrow-op :apply-arrow
                          :guard-op :apply-guard})

(def arrow-or-guard->name {:arrow-op "->"
                           :guard-op "|"})


(defn make-op-name [op]
  (with-meta
    [:name
     (or (arrow-or-guard->name (first op))
         (first-arg op))]
    (meta op)))


(defn transform-ast-operation [exp]
  (let [[_ left-exp op right-exp] exp]
    (with-meta
      [(or (arrow-or-guard->key (first op))
           :apply)
       (make-op-name op)
       left-exp
       right-exp]
      (meta exp))))


(defn transform-ast-left-section [exp]
  (let [[_ left-exp op] exp]
    (with-meta
      [:apply
       (make-op-name op)
       left-exp]
      (meta exp))))


(defn transform-ast-right-section [exp]
  (let [[_ op right-exp] exp
        m (meta exp)]
    (if (and (> (count op) 2)
             (= "-" (first-arg op)))
      (with-meta
        [:apply
         (with-meta
           [:neg]
           (meta op))
         right-exp]
        m)
      (with-meta
        [:apply
         (with-meta
           [:name "flip"]
           m)
         (make-op-name op)
         right-exp]
        m))))


(defn transform-ast-prefix-op [exp]
  (make-op-name (first-arg exp)))


(defn make-gensym-comparison [[new-exps extra-comparisons] pair]
  (let [[exp op] pair]

    (if (has-type :name exp)
      [(concat new-exps pair)
       extra-comparisons]
      
      (let [new-name (uuid)
            m (meta exp)]
        [(concat new-exps
                 (list
                  (with-meta
                    [:name new-name]
                    m)
                  op))
         (cons (with-meta
                 [:apply
                  (with-meta
                    [:name "="]
                    m)
                  (with-meta
                    [:name new-name]
                    m)
                  exp]
                 m)
               extra-comparisons)]))))


(defn transform-chain [exp]
  (let [exps (all-args exp)

        [new-exps extra-comparisons]
        (reduce make-gensym-comparison
                  [() ()]
                  (partition 2 exps))

        extra-comparisons (reverse extra-comparisons)

        comparison-triples (partition 3 2
                                      (cons-at-end new-exps
                                                   (last exps)))]
    (with-meta
      (into [:and]
            (concat extra-comparisons
                    (map (fn [[left-exp op right-exp]]
                           (with-meta
                             [:apply
                              (with-meta
                                [:name
                                 (first-arg op)]
                                (meta op))
                              left-exp
                              right-exp]
                             (meta left-exp)))
                         comparison-triples)))
      (meta exp))))


(defn transform-embedded [exp]
  (let [m (meta exp)
        [_ left-exp op right-exp] exp
        is-name (has-type :name left-exp)
        is-underscore (and is-name
                           (= "_" (first-arg left-exp)))
        name-exp (if (or (not is-name)
                         is-underscore)
                   (with-meta
                     [:name (uuid)]
                     m)
                   left-exp)]
    (with-meta
      [:apply-guard
       (with-meta
         [:name "|"]
         m)
       name-exp
       (with-meta
         (if is-name
           [:apply
            op
            name-exp
            right-exp]
           [:and
            (with-meta
              [:apply
               (with-meta
                 [:name "="]
                 m)
               name-exp
               left-exp]
              m)
            (with-meta
              [:apply
               op
               name-exp
               right-exp]
              m)])
         m)]
      m)))


(defn combine-guards [guard]
  (let [[_ op left-exp right-exp] guard]
    (if (has-type :apply-guard left-exp)
      (let [[_ inner-op inner-left] left-exp]
        (with-meta
          [:apply-guard
           inner-op
           inner-left
           (make-and (concat (guard-assertions left-exp)
                             (guard-assertions guard)))]
          (meta guard)))
      guard)))


(defn remove-flip [exps]
  (if (and (has-type :name (first exps))
           (= "flip" (first-arg (first exps)))
           (> (count exps) 3))
    ;; remove the flip and swap the third and fourth expressions
    (let [[_ e2 e3 e4 & r] exps]
      (apply list e2 e4 e3 r))
    exps))


(defn combine-applys [exp]
  (let [exps (all-args exp)
        first-exp (first exps)]
    (with-meta
      (into [:apply]
         (remove-flip
          (if (has-type :apply first-exp)
            (concat (all-args first-exp)
                    (rest exps))
            exps)))
      (meta exp))))


(defn combine-ands [exp]
  (let [exps (all-args exp)]
    (make-and (mapcat (fn [subexp]
                        (if (has-type :and subexp)
                          (all-args subexp)
                          (list subexp)))
                      exps))))


;; Convert multiple arguments to a single sequence argument.
(defn args->seq [exp]
  (with-meta
    [(first exp) (all-args exp)]
    (meta exp)))


;; If the right side of an arrow application within a :bind is not already wrapped in a :bind, wrap it.
(defn bind-arrow-right [exp]
  (let [m (meta exp)
        bind-arg (second-arg exp)]
    (if (and (has-type :apply bind-arg)
             (has-type :name (first-arg bind-arg))
             (= "->" (first-arg (first-arg bind-arg)))
             (not (has-type :bind (third-arg bind-arg))))
      (let [[_ op left-exp right-exp] bind-arg
            new-bind (with-meta
                       [:bind []
                        right-exp]
                       (meta right-exp))]
        (with-meta
          [:bind []
           (with-meta
             [:apply
              op
              left-exp
              (bind-arrow-right new-bind)]
             (meta bind-arg))]
          m))
      (with-meta
        [:bind []
         bind-arg]
        m))))


(declare find-names-to-bind)

(defn get-included-top-level-names [exp]
  (let [path (first-arg exp)
        source (slurp (str (strip-tl-filepath path)
                           ".tls"))
        assertions (read-string (str "(" source ")"))]
    (find-names-to-bind #{} assertions)))


;; For the first call, exp is all the assertions.
(defn find-names-to-bind [outer-names exp]
  (cond
    (has-type :include exp)
    (get-included-top-level-names exp)

    (has-type :name exp)
    (let [name (first-arg exp)]
      (if (outer-names name)
        #{}
        #{name}))

    (has-type :bind exp)
    #{}

    (sequential? exp)
    (reduce (fn [inner-names sub-exp]
              (into inner-names
                    (find-names-to-bind outer-names
                                        sub-exp)))
            #{}
            exp)

    :else
    #{}))


;; For the first call, exp is all the assertions.
(defn fill-bind-names [outer-names exp]
  (cond
    (has-type :bind exp)
    (let [bind-exp (second-arg exp)
          inner-names (find-names-to-bind outer-names
                                          bind-exp)
          new-bind-exp (fill-bind-names (into outer-names inner-names)
                                        bind-exp)]
      (if (empty? inner-names)
        new-bind-exp
        (with-meta
          [:bind
           (into [] inner-names)
           new-bind-exp]
          (meta exp))))

    (vector? exp)
    (with-meta
      (into [(first exp)]
            (map (partial fill-bind-names outer-names)
                 (all-args exp)))
      (meta exp))

    (seq? exp)
    (map (partial fill-bind-names outer-names)
         exp)

    :else
    exp))


(defn top-level-fill-bind-names [assertions]
  (let [top-level-names (->> assertions
                             (find-names-to-bind #{})
                             (into predefined-names))]
    (fill-bind-names top-level-names
                     assertions)))


(defn transformations-1 [assertions]
  (let [trans-map {:set transform-set}]
    (map (partial transform trans-map) assertions)))


(defn transformations-2 [assertions]
  (let [trans-map {:operation transform-ast-operation
                   :left-section transform-ast-left-section
                   :right-section transform-ast-right-section
                   :prefix-op transform-ast-prefix-op
                   :embedded transform-embedded
                   :chain transform-chain}]
    (map (partial transform trans-map) assertions)))


(defn transformations-3 [assertions]
  (let [trans-map {:apply-guard combine-guards
                   :and combine-ands}]
    (map (partial transform trans-map) assertions)))


(defn transformations-4 [assertions]
  (let [trans-map {:apply-guard (partial change-key :apply)
                   :apply-arrow (partial change-key :apply)}]
    (map (partial transform trans-map) assertions)))


(defn transformations-5 [assertions]
  (let [trans-map {:apply combine-applys

                   ;; Can't be done earlier, o.w. | {((a ->) b)} doesn't get an inner :bind.
                   :bind bind-arrow-right}]
    (map (partial transform trans-map) assertions)))


(defn transformations-6 [assertions]
  (let [trans-map {:apply args->seq
                   :set args->seq
                   :seq args->seq
                   :and args->seq
                   :vals args->seq}]
    (map (partial transform trans-map) assertions)))


(defn build-include-expressions [includes]
  (map #(vector :include %)
       includes))


;; Returns: <assertions>
(defn ast->tls [includes assertions]
 (->> assertions
      transformations-1
      transformations-2
      transformations-3
      transformations-4
      transformations-5
      transformations-6
      (concat (build-include-expressions includes))
      top-level-fill-bind-names))
