(ns timeless.interpreter
  "Interpreter for an S-expression form of Timeless."
  (:require [timeless.common :refer :all]
            [timeless.transform
             [misc   :refer [transform-recursively]]
             [clause :refer [reorder-assertions-recursively]]]
            [timeless.eval :refer [eval-expr]]
            [clojure.set :as set]))

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
                      (map transform-recursively)
                      (doall))
         names (into #{} (map second asserts))
         asserts (map (par reorder-assertions-recursively
                           (set/union predefined names))
                      asserts)]
     (into {} (map (fn [[_ nam expr]]
                     [nam expr])
                   asserts)))))

(defn read-top-level-string
  "Same as read-top-level, except reads from a string."
  [s]
  (with-in-str s
    (read-top-level)))

(defn transform
  ([expr]
   (transform expr {}))
  ([expr context]
   (reorder-assertions-recursively (set/union predefined (keys context))
                                   (transform-recursively expr))))

(def t transform)

(defn transform-and-eval
  ([expr]
   (transform-and-eval expr {}))
  ([expr context]
   (eval-expr (transform expr context) context)))

(def e transform-and-eval)
