(ns timeless.refactor
  "Refactor set and fn comprehensions for the Timeless interpreter."
  (:require [timeless.common :refer :all]))


(def new-op list)

(defn new-=
  [left right]
  (list '= left right))

(defn extract-embedded-assertions'
  "Extract embedded assertions from expr and remove :opfn wrappers.
  Return the new expr and the list of embedded assertions."
  [expr]
  (cond (is-op? #{'∈ '∊ '⊂ '=} expr)
        (let [[op left right] expr]
          (if (symbol? left)
            [left (list expr)]

            (let [nam (gensym)]
              [nam (list (new-= nam left)
                         (new-op op nam right))])))

        (is-op? :opfn expr)
        [(second expr) '()]

        (list? expr)
        (let [r (map extract-embedded-assertions' expr)]
          [(map first r)
           (mapcat second r)])

        :else [expr '()]))

(defn extract-embedded-assertions
  "Extract embedded assertions from a clause pattern."
  [[op pattern asserts v]]
  (let [[pattern new-asserts] (extract-embedded-assertions' pattern)
        asserts (concat new-asserts asserts)]
    (new-op op pattern asserts v)))

(defn normalize-clause-pattern
  "Ensure the clause pattern is a free name or a constant w.r.t. context."
  [[op pattern asserts v] context]
  (let [[pattern asserts]
        (cond (constant? pattern context) [(new-op :const pattern)
                                           asserts]
              (symbol? pattern) [pattern asserts]
              :else (let [nam (gensym)]
                      [nam (cons (new-= pattern nam)
                                 asserts)]))]
    (new-op op pattern asserts v)))

(defn normalize-clause-value
  "Ensure the clause value is not a list, i.e. it's a name, an atomic constant, or nil (for :_set)."
  [[op pattern asserts v]]
  (let [[v asserts]
        (if (list? v)
          (let [nam (gensym)]
            [nam (concat asserts
                         (list (new-= nam v)))])
          [v asserts])]
    (new-op op pattern asserts v)))

(defn normalize-clause
  "Normalizes a clause, i.e. a :_fn or :_set expr."
  [clause context]
  (-> clause
      (normalize-clause-pattern context)
      (normalize-clause-value)))

(defn decompose-assertion
  "Decompose an equality assertion if it contains destructuring patterns,
  i.e. :seq, :tup, :cons, ++."
  [assert]
  (if (is-op? '= assert)
    (let [destr-ops #{:seq :tup :cons '++}
          [op left right] assert]
      (cond (is-op? destr-ops left)
            (if (list? right)
              (let [nam (gensym)]
                (mapcat decompose-assertion (list (new-= left nam)
                                                  (new-= nam right))))
              (let [f (fn [expr]
                        (if (list? expr)
                          (let [nam (gensym)]
                            [nam (decompose-assertion (new-= nam expr))])
                          [expr '()]))
                    r (map f (rest left))]
                (cons (new-= (apply new-op (first left) (map first r))
                             right)
                      (mapcat second r))))

            (is-op? destr-ops right)
            (decompose-assertion (new-op op right left))

            :else (list assert)))
    (list assert)))

(defn decompose-assertions
  "Decomposes destructuring assertions in a :_fn or :_set expr."
  [[op pattern asserts v]]
  (let [asserts (mapcat decompose-assertion asserts)]
    (new-op op pattern asserts v)))

;; TODO: detect and change non-binding equality assertions to ==
(defn reorder-assertions
  "Reorders assertions in a :_fn or :_set expr."
  [[op pattern asserts v] context]



  (new-op op pattern asserts v))

(defn make-clause
  "Make a :_fn or :_set expr, depending on op.
  The guard is split into a list of assertions."
  [[pattern guard v] op]
  (let [asserts (if (is-op? '∧ guard)
                  (rest guard) ; assuming all ∧ ops have been collapsed into one
                  (list guard))
        clause-op (if (= op :fn) :_fn :_set)]
    (new-op clause-op pattern asserts v)))

(defn refactor-fn-set
  "Refactors a :fn or :set expr."
  [expr context]
  (let [op (first expr)
        clauses (map #(-> %
                          (make-clause op)
                          (extract-embedded-assertions)
                          (normalize-clause context)
                          (decompose-assertions)
                          (reorder-assertions context))
                     (rest expr))]
    (if (second clauses)
      (cons '∪ clauses)
      (first clauses))))
