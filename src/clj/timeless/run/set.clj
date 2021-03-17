(ns timeless.run.set
  "Evaluate set-related TLS expressions."
  (:require [timeless.utils :refer :all]))

;; TODO: rewrite this like eval-union
(defn eval-inter [ctx exp]
  (let [exps (->> exp
                  all-args
                  (mapcat (fn [subexp]
                            (let [v (eval-tls ctx subexp)]
                              (if (has-type :inter v)
                                (all-args v)
                                (list v)))))
                  (remove (partial has-name "Any")))]
    (cond
      (empty? exps)
      (with-meta
        [:name "Any"]
        (meta exp))

      (some (partial has-type :num)
            exps)
      (with-meta
        [:vals]
        (meta exp))

      (empty? (rest exps))
      (first exps)
      
      :else
      (with-meta
        (into [:inter] exps)
        (meta exp)))))

(defn merge-union-pair [ctx exp1 exp2]
  (cond
    (has-no-value exp1)
    exp1
        
    (has-no-value exp2)
    exp2

    (is-empty-set exp1)
    exp2

    (is-empty-set exp2)
    exp1

    (has-name "Any" exp1)
    exp1

    (has-name "Any" exp2)
    exp2

    (and (has-name "Int" exp1)
         (has-name "Num" exp2))
    exp2

    (and (has-name "Num" exp1)
         (has-name "Int" exp2))
    exp1

    (and (has-type :set exp1)
         (has-type :set exp2))
    (with-meta
      (into [:set] (set (concat (all-args exp1)
                                (all-args exp2)))))

    ;; TODO: more cases

    :else nil))

;; TODO: reduce on (rest exps), each time returning two values: boolean whether the pairwise merge of (first exps) and the next exp worked, and all the exps checked, whether merged or not. Then call this function recursively on (rest exps), and either cons (first exps) on the head or not, depending on the boolean.
(defn pairwise-reduce-union [exps]
)

(defn eval-union [ctx exp]
  (let [exps (->> exp
                  all-args
                  (mapcat (fn [subexp]
                            (let [v (eval-tls ctx subexp)]
                              (if (has-type :union v)
                                (all-args v)
                                (list v))))))]
    (cond
      (empty? exps)
      (with-meta
        [:set]
        (meta exp))

      (empty? (rest exps))
      (first exps)

      :else
      (pairwise-reduce-union exps))))



(defn eval-set [ctx exp]
  (let [exps (->> exp
                  all-args
                  (map (partial eval-tls ctx))
                  set ; Deduplicate, ignoring differences in metadata.
                  seq)
        {in-exps true other-exps false}
        (group-by (partial has-type :in)
                  exps)
        
        new-set (with-meta
                  (into [:set] other-exps)
                  (meta exp))]
    (if in-exps
      (eval-tls ctx
                (with-meta
                  (into [:union new-set]
                        (map first-arg in-exps))
                  (meta exp)))
      new-set)))
