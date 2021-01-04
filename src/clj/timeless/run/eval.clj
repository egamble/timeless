(ns timeless.run.eval
  "Evaluate TLS expressions."
  (:require [timeless.utils :refer :all]))


;; TODO:
;; - Instead of str for ++, write a fn that throws an error if the args are not both strings or both seqs.
;; - Don't convert :num, :str or :name, so the metadata is still available for errors. Have the pretty function do that, optionally. (:names are printed without quotes.)
;; - "+" and "++" can work with :values, in either or both arguments.g

(defn eval-tls [ctx exp]
  (cond
    (has-types #{:num :str} exp)
    (first-arg exp)

    (has-type :name exp)
    (if-let [v (ctx (first-arg exp))]
      (eval-tls ctx v)
      exp)

    (list? exp)
    (let [exp (apply list
                     (map (partial eval-tls ctx)
                          exp))]
      (if (and (= 3 (count exp))
               (has-type :name (first exp)))
        (if-let [f ({"+" +
                     "++" str}
                    (first-arg (first exp)))]
          (apply f (rest exp))
          exp)))

    :else
    exp))
