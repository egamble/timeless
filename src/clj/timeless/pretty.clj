(ns timeless.pretty
  "Prettify a AST or TLS form."
  (:require [timeless.utils :refer :all]))


(defn pretty [indent show-metadata? initial-indent? form]
  (let [next-indent (str indent "  ")]
    (cond (list? form)
          (str (when initial-indent? indent) "( "
               (pretty next-indent
                       show-metadata?
                       false
                       (first form))
               (->> (rest form)
                    (map #(pretty next-indent
                                  show-metadata?
                                  true
                                  %))
                    insert-newlines
                    butlast
                    (apply str "\n"))
               ")")

          (has-type :bind form)
          (let [subforms (rest form)]
            (str (when initial-indent? indent)
                 "[:bind"
                 (when show-metadata?
                   (str " " (meta form)))
                 " "
                 (pr-str (first subforms))
                 (apply str (map #(str "\n"
                                       (pretty next-indent
                                               show-metadata?
                                               true
                                               %))
                                 (rest subforms)))
                 "]"))

          (vector? form)
          (let [subforms (rest form)]
            (str (when initial-indent? indent)
                 "[" (first form)
                 (when show-metadata?
                   (str " " (meta form)))
                 (when-not (empty? subforms)
                   (let [p (if (sequential? (first subforms))
                             "\n"
                             " ")]
                     (apply str (map #(str p
                                           (pretty next-indent
                                                   show-metadata?
                                                   true
                                                   %))
                                     subforms))))
                 "]"))

          :else
          (pr-str form))))
