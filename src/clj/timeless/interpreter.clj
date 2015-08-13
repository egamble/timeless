(ns timeless.interpreter
  "Interpreter for an S-expression form of Timeless."
  (:require [timeless
             [common :refer :all]
             [transform :refer [transform-comprehension]]]))


(defn read-top-level
  "Reads top-level assertions from stream, which defaults to *in*.
  Ignores assertions other than equality assertions, because this interpreter can't use them.
  Returns a context map.
  Don't try to print the returned context, it's circular!"
  ([]
   (read-top-level *in*))
  ([stream]
   (let [asserts (->> (repeatedly #(read stream false nil))
                      (take-while not-nil?)
                      (filter (partial op-isa? '=)) ;; keep only equality assertions
                      (doall))
         context (into {}
                       (map (fn [[_ sym expr]]
                              [sym (atom expr)])
                            asserts))]
     (doseq [a (vals context)]
       (swap! a (fn [expr] {:expr expr :context context})))
     context)))

(defn read-top-level-string
  "Same as read-top-level, except reads from a string."
  [s]
  (with-in-str s
    (read-top-level)))

(defn eval-in-context
  [expr context]
  nil)
