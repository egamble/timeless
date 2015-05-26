(ns timeless.interpreter
  "Interpreter for an S-expression form of Timeless."
  (:require [let-else :refer [let?]]))

(defn read-top-level
  "Reads top-level sexprs from stream, which defaults to *in*.
Returns a context."
  ([]
   (read-top-level *in*))
  ([stream]
   (let [sexprs (->> (repeatedly #(read stream false nil))
                     (take-while not-empty)
                     (filter #(= (first %) '=))
                     (doall))
         context "hashmap with keys that are LHS names, vals that are atoms"]
     sexprs))) ; the atoms contain {:context <this context>, :expr}

(defn read-top-level-string
  "Reads top-level sexprs from string s.
Returns a context."
  [s]
  (with-in-str s
    (read-top-level)))

(defn eval-in-context
  [sexpr context]
  nil)
