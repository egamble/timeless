(ns timeless.transform.tls
  "Transform an AST to produce TLS code."
  (:require [timeless.transform.utils :refer :all]
            [instaparse.core :as insta]
            [clojure.string :as str]))


;; TODO:

;; gensyms:
;; - Use UUIDs, for now, for gensyms, to avoid collisions not only in the current file,
;;   but with other TLS files that can be concatenated. Maybe find another solution later, such as:
;;   - adding the gensyms to the nearest :bind to prevent collusions,
;;     while using simple gensyms such as "g1", "g2", etc., or
;;   - using a keyword other than :name, with the disadvantage of complicating context maps.
p
;; - Gensym for _ in :embeddeds. Reuse as much code as possible in the two different transformations of :embeddeds.
;; - If the left side of an embedded is not just a :name, put a gensym there and add an equality assertion to an :and.

;; - Wrap the right side of :arrows that are within a :bind with a :bind, if not already wrapped. What about :values?
;; - Fill in the names of :binds.

;; - Test various combinations of guards, arrows and embeddeds.



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
  (make-exp :and
            (get-meta (first exps))
            exps))


(defn build-leading-guard [exps]
  (let [[guard-exps exps-after-guards]
        (break-off-guards exps)

        [exp & guards] guard-exps]
    (if (seq guards)
      (cons [:guard (get-meta exp)
             exp
             (if (second guards)
               (make-and guards)
               (first guards))]
            exps-after-guards)
      exps)))


(defn reform-clause [exps]
  (let [exps (build-leading-guard exps)
        m (get-meta (first exps))
        op (second exps)]
    [:bind m [] ; the names will be filled in later, and if there are no names, the :bind will be removed
     (if (second exps) ; the :arrow-op
       (let [remaining-exps (third-on exps) ; remaining exps, starting with the third exp,
                                        ; i.e. the exp after the :arrow-op
             ]
         [:arrow-apply m
          (first exps)
          (reform-clause remaining-exps)])
       (first exps))]))


(defn count-binds [clause]
  (if (= :bind (first clause))
    (let [bind-exp (fourth clause)]
      (if (= :arrow-apply (first bind-exp))
        (+ 1 (count-binds (fourth bind-exp)))
        1))
    (if (= :values (first clause))
      (let [exps (exp-args clause)]
        (count-binds (last exps)))
      (error-meta (get-meta clause)
                  "internal error counting binds"))))


(defn join-with-clause [clause joinable-clause index]
  (let [[_ m & exps] clause
        is-bind (has-type :bind clause)]
    (if (= 0 index)
      (make-exp :values m
                (if is-bind
                  (list clause
                        joinable-clause)

                  ;; else it's a :values expression
                  (cons-at-end exps
                               joinable-clause)))

      ;; else index > 0
      (let [f (fn [[_ m1 names1 [_ m2 names2 next-clause]]]
                [:bind m1 names1
                 [:arrow-apply m2 names2
                  (join-with-clause next-clause
                                    joinable-clause
                                    (- index 1))]])]
        (if is-bind
          (f clause)

          ;; else it's a :values expression
          (let [exps-but-last (butlast exps)
                last-exp (last exps)]
            [:values m (cons-at-end exps-but-last
                                    (f last-exp))]))))))


(defn guard-assertions [right-exp]
  (if (has-type :and right-exp)
    (exp-args right-exp)
    (list right-exp)))


(defn break-up-prev-guard [prev-guard]
  (if (has-type :guard-apply prev-guard)
    (let [[_ _ left-exp right-exp] prev-guard]
      [left-exp
       (guard-assertions right-exp)])
    (if (has-type :arrow-apply prev-guard)
      (break-up-prev-guard (third prev-guard))
      [prev-guard ()])))


(defn get-prev-guard [clause index]
  (let [[_ m & exps] clause
        is-bind (has-type :bind clause)]
    (if (= 0 index)
      (second ; skip the :names of the :bind
       (if is-bind
         exps

         ;; else it's a :values expression containing :bind expressions, so get the arguments of the last :bind
         (exp-args (last exps))))

      ;; else index > 0
      (let [f (fn [[_ _ _ ; :bind
                   [_ _ _ next-clause] ; :arrow-apply
                   ]]
                (get-prev-guard next-clause
                                (- index 1)))]
        (if is-bind
          (f clause)

          ;; else it's a :values expression
          (f (last exps)))))))


(defn copy-and-build-guard [prev-clause index assertions]
  (let [prev-guard
        (get-prev-guard prev-clause index)

        [prev-left-exp prev-assertions]
        (break-up-prev-guard prev-guard)

        num-diff (- (count prev-assertions)
                    (count assertions))]
    (when (< num-diff 0)
      (error-meta (get-meta (first assertions))
                  "truncated clause has more guard assertions than previous clause"))
    (let [all-assertions (concat (take num-diff prev-assertions)
                                 assertions)]
      [:guard-apply (get-meta prev-left-exp)
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


(defn reform-clause-with-truncated-guard [prev-clause index exps]
  (let [[assertions remaining-exps] (break-off-assertions exps)
        guard (copy-and-build-guard prev-clause index assertions)]
    (reform-clause (cons guard remaining-exps))))


(defn reform-truncated-clause [prev-clause next-clause]
  (let [num-binds (count-binds prev-clause)
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
                          (reform-clause-with-truncated-guard prev-clause join-index exps)
                          (reform-clause (rest exps)) ; omit the :arrow-op
                          )]
    (join-with-clause prev-clause joinable-clause join-index)))


(defn reduce-clauses [new-clauses next-clause]
  (let [[_ m & exps] next-clause]
    (if (has-types #{:arrow-op :guard-op} (first exps))
      (if (seq new-clauses)
        (let [prev-clause (first new-clauses)]
          (cons (reform-truncated-clause prev-clause next-clause)
                (rest new-clauses)))
        (error-meta m "first clause can't be truncated"))
      (cons (reform-clause exps)
            new-clauses))))


(defn transform-set [m & clauses]
  (let [new-clauses (reduce reduce-clauses () clauses)]
    (make-exp :set m
              (reverse new-clauses))))


(defn rebuild-sets [assertions]
  (let [trans-map {:set transform-set}]
    (map (partial insta/transform trans-map) assertions)))


(defn operation->apply [m left-exp op right-exp]
  [:apply m
   (make-exp :name (get-meta op)
             (exp-args op))
   left-exp
   right-exp])


(def arrow-or-guard->key {:arrow-op :apply-arrow
                          :guard-op :apply-guard})

(def arrow-or-guard->name {:arrow-op (list "->")
                           :guard-op (list "|")})


(defn make-op-name [op]
  (make-exp :name (get-meta op)
             (or (arrow-or-guard->name (first op))
                 (exp-args op))))


(defn transform-ast-operation [m left-exp op right-exp]
  [(or (arrow-or-guard->key (first op))
       :apply)
   m
   left-exp
   (make-op-name op)
   right-exp])


(defn transform-ast-left-section [m left-exp op]
  [:apply m
   (make-op-name op)
   left-exp])


(defn transform-ast-right-section [m op right-exp]
  (if (= "-" (third op))
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
    (make-exp :and m
              (concat extra-comparisons
                      (map (fn [comparison-triple]
                             (apply operation->apply (get-meta (first comparison-triple))
                                    comparison-triple))
                           comparison-triples)))))


;; TODO: gensym for _ in embedded. Reuse as much code as possible from the earlier transformation of :embeddeds.
(defn transform-embedded [m left-exp op right-exp]
  [:apply-guard m
   left-exp
   [:name m "|"]
   (operation->apply m left-exp op right-exp)])


;; TODO: gensym for _ in embedded
(defn combine-guards [m left-exp op right-exp]
  (make-exp :apply-guard m
            (if (has-type :apply-guard left-exp)
              (let [inner-guards (guard-assertions (fourth left-exp))
                    outer-guards (guard-assertions right-exp)]
                (list
                 (third left-exp)
                 (fourth left-exp)
                 (make-and (concat inner-guards
                                   outer-guards))))
              (if (has-type :embedded left-exp)
                (list
                 (third left-exp)
                 op
                 (make-and (cons (apply operation->apply (rest left-exp))
                                 (guard-assertions right-exp))))
                (list
                 left-exp
                 op
                 right-exp)))))


(defn combine-applys [m & exps]
  (let [first-exp (first exps)]
    (make-exp :apply m
              (if (has-type :apply first-exp)
                (concat (exp-args first-exp)
                        (rest exps))
                exps))))


(defn combine-ands [m & exps]
  (make-and (mapcat (fn [exp]
                      (if (has-type :and exp)
                        (exp-args exp)
                        (list exp)))
                    exps)))


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
  (let [trans-map {:apply combine-applys}]
    (map (partial insta/transform trans-map) assertions)))


(defn transformations-5 [assertions]
  (let [trans-map {:apply (fn [m & r]
                            (apply list r))}]
    (map (partial insta/transform trans-map) assertions)))


;; Returns: <assertions>
(defn ast->tls [assertions]
  (->> assertions
       rebuild-sets
       transformations-1
       transformations-2
       transformations-3
       transformations-4
       transformations-5))
