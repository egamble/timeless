(ns timeless.refactor
  "Refactor set and fn comprehensions for the Timeless interpreter."
  (:require [timeless.common :refer :all]))


(defn new-=
  [left right]
  (list '= left right))

(defn normalize-fn-set
  "Normalizes a $$fn or $$set expr."
  [expr context]
  (let [[type pattern asserts v] expr

        [pattern asserts]
        (cond (constant? pattern context) [(list '$const pattern)
                                           asserts]
              (symbol? pattern) [pattern asserts]
              :else (let [sym (gensym)]
                      [sym (cons (new-= pattern sym)
                                 asserts)]))
        [v asserts]
        (if (list? v)
          (let [sym (gensym)]
            [sym (concat asserts
                         (list (new-= sym v)))])
          [v asserts])]
    (list type pattern asserts v)))

(defn decompose-fn-set
  "Decomposes destructuring assertions in a $$fn or $$set expr."
  [expr context]
  expr)

(defn reorder-fn-set
  "Reorders assertions in a $$fn or $$set expr."
  [expr context]
  expr)

(defn refactor-fn-set
  "Refactors a $fn or $set expr, returning a $$fn or $$set expr."
  [expr context]
  (let [[type pattern guard v] expr
        asserts (if (is-type? '∧ guard)
                  (rest guard) ; assuming all ∧ ops were collapsed into one
                  (list guard))
        type (if (= type '$fn)
               '$$fn
               '$$set)]
    (-> (list type
              pattern
              asserts        ; just a list of assertions without a ∧ op
              v)
        (normalize-fn-set context)
        (decompose-fn-set context)
        (reorder-fn-set context))))
