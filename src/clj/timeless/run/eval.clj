(ns timeless.run.eval
  "Evaluate TLS expressions."
  (:require [timeless.utils :refer :all]))


;; TODO:
;; - "+" and "++" can work with :vals, in either or both arguments.


(defn eval-++ [exp1 exp2]
  (cond
    (and (has-type :str exp1)
         (has-type :str exp2))
    (with-meta
      [:str (str (first-arg exp1)
                 (first-arg exp2))]
      (meta exp1))

    (and (has-type :seq exp1)
         (has-type :seq exp2))
    (with-meta
      [:seq (concat (first-arg exp1)
                    (first-arg exp2))]
      (meta exp1))

    :else [:vals]))


(defn eval-+ [exp1 exp2]
  (cond
    (and (has-type :num exp1)
         (has-type :num exp2))
    (with-meta
      [:num (+ (first-arg exp1)
               (first-arg exp2))]
      (meta exp1))

    :else [:vals]))


(defn eval-tls [ctx exp]
  (cond
    (has-type :name exp)
    (if-let [v (ctx (first-arg exp))]
      (eval-tls ctx v)
      exp)

    (has-type :apply exp)
    (let [exps (map (partial eval-tls ctx)
                    (first-arg exp))]
      (if (and (= 3 (count exps))
               (has-type :name (first exps)))
        (if-let [f ({"+" eval-+
                     "++" eval-++}
                    (first-arg (first exps)))]
          (apply f (rest exps))
          exp)))

    :else
    exp))
