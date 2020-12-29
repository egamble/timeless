(ns timeless.transform.pretty
  "Prettify a AST or TLS form."
  (:require [timeless.transform.utils :refer :all]))


(defn pretty [indent suppress-metadata? initial-indent? form]
  (let [next-indent (str indent "  ")]
    (cond (list? form)
          (str (when initial-indent? indent) "( "
               (pretty next-indent
                       suppress-metadata?
                       false
                       (first form))
               (->> (rest form)
                    (map #(pretty next-indent
                                  suppress-metadata?
                                  true
                                  %))
                    insert-newlines
                    butlast
                    (apply str "\n"))
               ")")

          (has-type :bind form)
          (let [m (second form)
                subforms (if (map? m)
                           (rest (rest form))
                           (rest form))]
            (str (when initial-indent? indent)
                 "[:bind"
                 (when (and (not suppress-metadata?)
                            (map? m))
                   (str " " m))
                 " "
                 (pr-str (first subforms))
                 (apply str (map #(str "\n"
                                       (pretty next-indent
                                               suppress-metadata?
                                               true
                                               %))
                                 (rest subforms)))
                 "]"))

          (vector? form)
          (let [m (second form)
                subforms (if (map? m)
                           (rest (rest form))
                           (rest form))]
            (str (when initial-indent? indent)
                 "[" (first form)
                 (when (and (not suppress-metadata?)
                            (map? m))
                   (str " " m))
                 (when-not (empty? subforms)
                   (let [p (if (sequential? (first subforms))
                             "\n"
                             " ")]
                     (apply str (map #(str p
                                           (pretty next-indent
                                                   suppress-metadata?
                                                   true
                                                   %))
                                     subforms))))
                 "]"))

          :else
          (pr-str form))))
