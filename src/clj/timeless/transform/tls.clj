(ns timeless.transform.tls
  "Transform an AST to produce TLS code."
  (:require [timeless.transform.utils :refer :all]
            [instaparse.core :as insta]
            [clojure.string :as str]))


;; TODO:
;; - Find out why != and !@ don't parse correctly.
;; - Wrap the right side of :arrows that are within a :bind with a :bind, if not already wrapped. What about :values?
;; - Combine nested :applys.
;; - Deal with :embedded. This is tricky, because converting to :guard before dealing with truncation messes up the truncation alignment. So, if the :embedded is within a :guard, combine it with the :guard, otherwise make a new :guard.
;; - Fill in the names of :binds.
;; - Decide whether :chain is legal TLS.


(defn get-meta [exp]
  (second exp))


(defn break-off-guards [exps]
  (if (= :guard-op (first (second exps)))
    (let [[guard-exps exps-after-guards]
          (break-off-guards (rest (rest exps))) ; recur with the remaining exps, starting with the third exp,
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


(defn join-with-clause [clause joinable-clause index]
  (let [[k m & exps] clause]
    (if (= 0 index)
      (apply vector :values m (if (= :bind k)
                                (list clause
                                      joinable-clause)

                                ;; else it's a :values expression
                                (concat exps
                                        (list joinable-clause))))

      ;; else index > 0
      (let [f (fn [[_ m1 names1 [_ m2 names2 next-clause]]]
                [:bind m1 names1
                 [:arrow m2 names2
                  (join-with-clause next-clause
                                    joinable-clause
                                    (- index 1))]])]
        (if (= :bind k)
          (f clause)

          ;; else it's a :values expression
          (let [exps-but-last (butlast exps)
                last-exp (last exps)]
            [:values m (concat exps-but-last
                               (list (f last-exp)))]))))))


(defn break-up-prev-guard [prev-guard]
  (let [k (first prev-guard)]
    (if (= :guard k)
      (let [[_ _ guard-exp assertion] prev-guard]
        [guard-exp
         (if (= :and (first assertion))
           (rest (rest assertion))
           (list assertion))])
      (if (= :arrow k)
        (break-up-prev-guard (third prev-guard))
        [prev-guard ()]))))


(defn get-prev-guard [clause index]
  (let [[k m & exps] clause]
    (if (= 0 index)
      (second ; skip the :names of the :bind
       (if (= :bind k)
         exps

         ;; else it's a :values expression containing :bind expressions, so get the exps of the last :bind
         (rest (rest (last exps)))))

      ;; else index > 0
      (let [f (fn [[_ _ _ ; :bind
                   [_ _ _ next-clause] ; :arrow
                   ]]
                (get-prev-guard next-clause
                                (- index 1)))]
        (if (= :bind k)
          (f clause)

          ;; else it's a :values expression
          (f (last exps)))))))


(defn copy-and-build-guard [prev-clause index assertions]
  (let [prev-guard
        (get-prev-guard prev-clause index)

        [prev-guard-exp prev-assertions]
        (break-up-prev-guard prev-guard)

        num-diff (- (count prev-assertions)
                    (count assertions))]
    (when (< num-diff 0)
      (error-meta (get-meta (first assertions))
                  "truncated clause has more guard assertions than previous clause"))
    (let [all-assertions (concat (take num-diff prev-assertions)
                                 assertions)]
      [:guard (get-meta prev-guard-exp)
       prev-guard-exp
       (if (> (count all-assertions) 1)
         (apply vector
                :and (get-meta (first all-assertions))
                all-assertions)
         (first all-assertions))])))


(defn break-off-assertions [exps]
  (if (= :guard-op (first (first exps)))
    (let [assertion (second exps)

          [assertions remaining-exps]
          (break-off-assertions (rest (rest exps)))]
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

        is-truncated-guard (= :guard-op (first (first exps)))
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


(defn transform-ast-operation [m left-exp op right-exp]
  (let [new-op ({:arrow-op :arrow
                 :guard-op :guard
                 :tag-op :tag}
                (first op))]
    (if new-op
      [new-op m
       left-exp
       right-exp]
      [:apply m
       (apply vector :name (rest op))
       left-exp
       right-exp])))


(defn transform-operations [assertions]
  (let [trans-map {:operation transform-ast-operation}]
    (map (partial insta/transform trans-map) assertions)))


;; Returns: <assertions>
(defn ast->tls [assertions]
  (->> assertions
       rebuild-sets
       transform-operations))
