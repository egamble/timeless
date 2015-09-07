(ns timeless.interpreter
  "Interpreter for an S-expression form of Timeless."
  (:require [timeless.common :refer :all]
            [timeless.transform
             [misc   :refer [misc-transforms]]
             [clause :refer [transform-clause]]]))

(defn read-top-level
  "Reads top-level assertions from stream, which defaults to *in*.
  Ignores assertions other than equality assertions, because this interpreter can't use them.
  Returns a context map."
  ([]
   (read-top-level *in*))
  ([stream]
   (let [asserts (->> (repeatedly #(read stream false nil))
                      (take-while not-nil?)
                      (filter (par op-isa? '=)) ;; keep only equality assertions
                      (map misc-transforms)
                      (doall))]
     (into {} (map rest asserts)))))

(defn read-top-level-string
  "Same as read-top-level, except reads from a string."
  [s]
  (with-in-str s
    (read-top-level)))

(defn eval-expr
  [expr context]
  (condf expr
   (par op-isa? #{:fn_ :set_})
   (or (:transformed (meta expr))
       (let [clause (transform-clause expr context)]
         (do (vary-meta expr assoc :transformed clause)
             clause)))

   symbol?
   (if (context expr)
     (eval-expr (context expr) context)
     expr)

   expr))

(def e eval-expr)

