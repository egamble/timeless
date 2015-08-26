(ns timeless.interpreter
  "Interpreter for an S-expression form of Timeless."
  (:require [timeless
             [common :refer :all]
             [transform :refer [transform-comprehension]]]))

(defn set-all-names-in-tree
  "Throughout an expression, convert (:name <name str>) to a symbol, make gensyms for underscores, and tag all subexpressions with the set of all names used within."
  [expr]
  (cond (op-isa? :name expr)
        (let [nam (symbol (second expr))]
          (tag-name nam))

        (= '_ expr)
        (new-name) ; also sets :all-names tag

        (symbol? expr)
        (tag-name expr)

        (op? expr)
        (let [expr (map set-all-names-in-tree expr)
              names (collect-all-names expr)]
          (set-all-names expr names))
        
        :else expr))

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
                      (map set-all-names-in-tree)
                      (doall))
         context (into {}
                       (map (fn [[_ nam expr]]
                              [nam (atom expr)])
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
