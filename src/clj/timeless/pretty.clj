(ns timeless.pretty
  "Prettify a AST or TLS form."
  (:require [timeless.utils :refer :all]))


(defn pretty [indent show-metadata? simplify? initial-indent? form]
  (let [next-indent (str indent "  ")]
    (cond (seq? form)
          (str (when initial-indent? indent) "( "
               (pretty next-indent
                       show-metadata?
                       simplify?
                       false ; no initial indent
                       (first form))
               (when (next form)
                 (->> (next form)
                      (map #(pretty next-indent
                                    show-metadata?
                                    simplify?
                                    true ; initial indent
                                    %))
                      (interleave (repeat "\n"))
                      (apply str)))
               " )")

          (has-type :bind form)
          (let [exps (all-args form)]
            (str (when initial-indent? indent)
                 "[:bind"
                 (when (and
                        show-metadata?
                        (meta form))
                   (str " " (dissoc (meta form) :evaled)))
                 " "
                 (pr-str (first exps))
                 (apply str (map #(str "\n"
                                       (pretty next-indent
                                               show-metadata?
                                               simplify?
                                               true ; initial indent
                                               %))
                                 (rest exps)))
                 "]"))

          (and simplify?
               (has-types #{:num :str} form))
          (pretty indent
                  show-metadata?
                  simplify?
                  initial-indent?
                  (first-arg form))

          (and simplify?
               (has-type :apply form))
          (pretty indent
                  show-metadata?
                  simplify?
                  initial-indent?
                  (all-args form))

          (and simplify?
               (has-type :name form)
               (not= \:
                     (nth (first-arg form) 0)))
          (pretty indent
                  show-metadata?
                  simplify?
                  initial-indent?
                  (symbol(first-arg form)))
          
          (vector? form)
          (let [exps (all-args form)]
            (str (when initial-indent? indent)
                 "[" (first form)
                 (when (and
                        show-metadata?
                        (meta form))
                   (str " " (dissoc (meta form) :evaled)))
                 (when-not (empty? exps)
                   (let [multi-line? (and (sequential? (first exps))
                                          (not (and simplify?
                                                    (has-types #{:name :num :str}
                                                               (first exps)))))]
                     (->> exps
                          (map #(pretty next-indent
                                        show-metadata?
                                        simplify?
                                        multi-line? ; initial indent
                                        %))
                          (interleave (repeat (if multi-line? "\n" " ")))
                          (apply str))))
                 "]"))

          :else
          (str (when initial-indent? indent)
               (pr-str form)))))
