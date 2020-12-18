(ns timeless.transform.misc
  "Various transformations of Timeless S-expressions."
  (:require [timeless.common :refer :all]
            [timeless.transform.clause :refer [transform-clause]]))

(defn transform-name
  "Convert (:name <name str>) to a symbol and make gensyms for underscores."
  [expr]
  (condf expr
   (par op-isa? :name)
   (symbol (second expr))

   (par = '_)
   (new-name)

   expr))

(defn restore-string
  [expr]
  (if (and (op-isa? :seq expr)
           (every? char? (rest expr)))
    (apply str (rest expr))
    expr))

(defn pre-eval-walk
  [expr]
  (-> (if (op? expr)
        (map pre-eval-walk expr)
        expr)
      transform-name
      transform-clause))

(defn post-eval-walk
  [expr]
  (-> (if (op? expr)
        (map post-eval-walk expr)
        expr)
      restore-string))
