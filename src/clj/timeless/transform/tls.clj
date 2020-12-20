(ns timeless.transform.tls
  "Transform an AST to produce TLS code."
  (:require [timeless.transform.utils :refer :all]
            [instaparse.core :as insta]
            [clojure.string :as str]))


;; TODO:
;; - check that metadata is correct
;; - truncated clauses


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


(defn reduce-clauses [clauses next-clause]
  (let [[_ m & exps] next-clause
        prev-clause (first clauses)]
    (if (#{:arrow-op :guard-op} (first (first exps)))
      (if prev-clause
        nil ; TODO handle truncated clauses
        (error-meta m "first clause can't be truncated"))
      (cons (reform-clause exps)
            clauses))))


(defn transform-set [m & clauses]
  (let [new-clauses (reduce reduce-clauses () clauses)]
    (apply vector :set m (reverse new-clauses))))


(defn rebuild-sets [assertions]
  (let [trans-map {:set transform-set}]
    (map (partial insta/transform trans-map) assertions)))


;; Returns: <assertions>
(defn ast->tls [assertions]
  (rebuild-sets assertions))
