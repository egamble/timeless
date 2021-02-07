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
                      insert-newlines
                      butlast
                      (apply str "\n")))
               " )")

          (has-type :bind form)
          (let [subforms (rest form)]
            (str (when initial-indent? indent)
                 "[:bind"
                 (when (and
                        show-metadata?
                        (meta form))
                   (str " " (meta form)))
                 " "
                 (pr-str (first subforms))
                 (apply str (map #(str "\n"
                                       (pretty next-indent
                                               show-metadata?
                                               simplify?
                                               true ; initial indent
                                               %))
                                 (rest subforms)))
                 "]"))

          (and simplify?
               (has-types #{:apply :num :str} form))
          (pretty indent
                  show-metadata?
                  simplify?
                  initial-indent?
                  (first-arg form))

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
          (let [subforms (rest form)]
            (str (when initial-indent? indent)
                 "[" (first form)
                 (when (and
                        show-metadata?
                        (meta form))
                   (str " " (meta form)))
                 (when-not (empty? subforms)
                   (let [subform1 (first subforms)]
                     (str
                      (if (sequential? subform1) "\n" " ")
                      (pretty next-indent
                              show-metadata?
                              simplify?
                              (sequential? subform1) ; initial indent
                              subform1)
                      (apply str (map #(pretty next-indent
                                               show-metadata?
                                               simplify?
                                               true ; initial indent
                                               %)
                                      (rest subforms))))))
                 "]"))

          :else
          (str (when initial-indent? indent)
               (pr-str form)))))
