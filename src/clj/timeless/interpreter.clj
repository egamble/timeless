(ns timeless.interpreter
  "Interpreter for an S-expression form of Timeless."
  (:require [let-else :refer [let?]]))

(defn read-top-level
  "Reads top-level assertions from stream, which defaults to *in*.
  Ignores assertions other than equality assertions.
  Returns a context map.
  Don't try to print the returned context, it's circular!"
  ([]
   (read-top-level *in*))
  ([stream]
   (let [asserts (->> (repeatedly #(read stream false nil))
                      (take-while not-empty)
                      (filter #(= (first %) '=))
                      (doall))
         context (into {}
                       (map (fn [[_ name v]]
                              [name (atom v)])
                            asserts))]
     (doseq [a (vals context)]
       (swap! a (fn [v] {:context context :expr v})))
     context)))

(defn read-top-level-string
  "Same as read-top-level, except reads from a string."
  [s]
  (with-in-str s
    (read-top-level)))

(defn eval-in-context
  [sexpr context]
  nil)
