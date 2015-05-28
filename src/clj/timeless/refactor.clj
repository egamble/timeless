(ns timeless.refactor
  "Refactor set and fn comprehensions for the Timeless interpreter."
  (:require [timeless.common :refer :all]))


(defn new-=
  [left right]
  (list '= left right))

(defn normalize-fn-set
  "Normalizes a $$fn or $$set expr.
  Normalize means:
  (1) ensure the pattern is either a free name or a constant expr w.r.t. the context, and
  (2) ensure the value is not a list, i.e. it's a name, an atomic constant, or nil (for $$set)."
  [expr context]
  (let [[type pattern asserts v] expr
        ;; ensure the pattern is a free name or a constant w.r.t. context
        [pattern asserts]
        (cond (constant? pattern context) [(list '$const pattern)
                                           asserts]
              (symbol? pattern) [pattern asserts]
              :else (let [sym (gensym)]
                      [sym (cons (new-= pattern sym)
                                 asserts)]))
        ;; ensure the value is not a list
        [v asserts]
        (if (list? v)
          (let [sym (gensym)]
            [sym (concat asserts
                         (list (new-= sym v)))])
          [v asserts])]
    (list type pattern asserts v)))

(defn decompose-assertion
  "Decompose an equality assertion if it contains destructuring patterns,
  i.e. $seq, $tup, ++, :."
  [assert]
  (if (is-type? '= assert)
    (let [[_ left right] assert]
      nil
      )
    [assert]))

(defn decompose-fn-set
  "Decomposes destructuring assertions in a $$fn or $$set expr."
  [expr]
  (let [[type pattern asserts v] expr
        asserts (mapcat decompose-assertion asserts)]
    (list type pattern asserts v)))

;; TODO: detect and change non-binding equality assertions to ==
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
        (decompose-fn-set)
        (reorder-fn-set context))))
