(ns timeless.transform.tls
  "Transform an AST to produce TLS code."
  (:require [timeless.transform.utils :refer :all]
            [instaparse.core :as insta]
            [clojure.string :as str]))


;; TODO:
;; - Truncated clauses.
;; - Convert arrow and guard operations to :arrow and :guard. Wrap the right side of arrow operations that are within the right-most part of clauses with :bind.
;; - Fill in the names of :binds.


(defn get-meta [exp]
  (second exp))


(defn break-off-guards [exps]
  (if (= :guard-op (first (second exps)))
    (let [[guard-exps exps-after-guards]
          (break-off-guards (rest (rest exps))); recur with the remaining exps, starting with the third exp,
                                        ; i.e. the exp after the :guard-op
          ]
      [(cons (first exps)
             guard-exps)
       exps-after-guards])
    [(list (first exps))
     (rest exps)]))


(defn build-leading-guard [exps]
  (let [[guard-exps exps-after-guards]
        (break-off-guards exps)

        [exp & guards] guard-exps]
    (if (seq guards)
      (cons [:guard (get-meta exp)
             exp
             (if (second guards)
               (apply vector :and
                      (get-meta (first guards))
                      guards)
               (first guards))]
            exps-after-guards)
      exps)))


(defn reform-clause [exps]
  (let [exps (build-leading-guard exps)
        m (get-meta (first exps))
        op (second exps)]
    [:bind m [:names m] ; the names will be filled in later, and if there are no names, the :bind will be removed
     (if (second exps) ; the :arrow-op
       (let [remaining-exps (rest (rest exps)) ; remaining exps, starting with the third exp,
                                        ; i.e. the exp after the :arrow-op
             ]
         [:arrow m
          (first exps)
          (reform-clause remaining-exps)])
       (first exps))]))


(defn count-binds [clause]
  (if (= :bind (first clause))
    (let [bind-exp (fourth clause)]
      (if (= :arrow (first bind-exp))
        (+ 1 (count-binds (fourth bind-exp)))
        1))
    (if (= :values (first clause))
      (let [exps (rest (rest clause))]
        (count-binds (last exps)))
      (error-meta (get-meta clause)
                  "internal error counting binds"))))


(defn join-with-clause [clause joinable-clause join-index]
  (let [[k m & exps] clause]
    (if (= 0 join-index)
      (apply vector :values m (if (= :bind k)
                                (list clause
                                      joinable-clause)

                                ;; else it's a :values expression
                                (concat exps
                                        (list joinable-clause))))

      ;; else join-index > 0
      (let [f (fn [[_ m1 names1 [_ m2 names2 next-clause]]]
                [:bind m1 names1
                 [:arrow m2 names2
                  (join-with-clause next-clause
                                    joinable-clause
                                    (- join-index 1))]])]
        (if (= :bind k)
          (f clause)

          ;; else it's a :values expression
          (let [exps-but-last (butlast exps)
                last-exp (last exps)]
            [:values m (concat exps-but-last
                               (list (f last-exp)))]))))))


;; TODO: copy missing guard exp(s) from prev-clause, then call reform-clause

(defn reform-clause-with-truncated-guard [prev-clause exps]
  (apply vector :foo (get-meta (first exps)) exps))


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

        is-truncated-guard (= :guard-op (first (first exps)))
        joinable-clause (if is-truncated-guard
                          (reform-clause-with-truncated-guard prev-clause exps)
                          (reform-clause (rest exps)) ; omit the :arrow-op
                          )
        join-index (- num-binds (if is-truncated-guard
                                  (+ 1 num-arrow-ops)
                                  num-arrow-ops))]
    (join-with-clause prev-clause joinable-clause join-index)))


(defn reduce-clauses [new-clauses next-clause]
  (let [[_ m & exps] next-clause]
    (if (#{:arrow-op :guard-op} (first (first exps)))
      (if (seq new-clauses)
        (let [prev-clause (first new-clauses)]
          (cons (reform-truncated-clause prev-clause next-clause)
                (rest new-clauses)))
        (error-meta m "first clause can't be truncated"))
      (cons (reform-clause exps)
            new-clauses))))


(defn transform-set [m & clauses]
  (let [new-clauses (reduce reduce-clauses () clauses)]
    (apply vector :set m (reverse new-clauses))))


(defn rebuild-sets [assertions]
  (let [trans-map {:set transform-set}]
    (map (partial insta/transform trans-map) assertions)))


;; Returns: <assertions>
(defn ast->tls [assertions]
  (rebuild-sets assertions))
