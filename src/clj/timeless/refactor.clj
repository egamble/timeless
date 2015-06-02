(ns timeless.refactor
  "Refactor set and fn comprehensions for the Timeless interpreter."
  (:require [timeless.common :refer :all]))


(def op?
  "Is an expr an operation, rather than a name, an atomic constant, or nil (for the value of a :_set)?"
  list?)

(def new-op list)

(defn new-=
  [left right]
  (list '= left right))

(defn extract-embedded-assertions'
  "Extract embedded assertions from pattern and remove :opfn wrappers.
  Return the new pattern and the list of embedded assertions."
  [pattern]
  (cond (op-isa? #{'∈ '∊ '⊂ '=} pattern)
        ;; pattern is an embedded assertion
        (let [[op left right] pattern]
          (if (symbol? left)
            ;; left side is a name; extract the assertion but don't generate a new name
            [left (list pattern)]

            ;; left side is an op or atomic constant; generate a new name and add assertions for the left and right sides
            (let [nam (gensym)]
              [nam (list (new-= nam left)
                         (new-op op nam right))])))

        (op-isa? :opfn pattern)
        ;; remove the :opfn wrapper without adding any assertions
        [(second pattern) '()]

        (op? pattern)
        ;; pattern is an op, so recursively search for embedded assertions
        (let [r (map extract-embedded-assertions' pattern)]
          [(map first r)
           (mapcat second r)])

        :else [pattern '()]))

(defn extract-embedded-assertions
  "Extract embedded assertions from a clause pattern."
  [clause]
  (let [[op pattern asserts v] clause
        [pattern new-asserts] (extract-embedded-assertions' pattern)
        asserts (concat new-asserts asserts)]
    (new-op op pattern asserts v)))

(defn normalize-clause-pattern
  "Ensure the clause pattern is a free name or a constant w.r.t. context."
  [clause context]
  (let [[op pattern asserts v] clause
        [pattern asserts]
        (cond (constant? pattern context) [(new-op :const pattern)
                                           asserts]
              (symbol? pattern) [pattern asserts]
              :else (let [nam (gensym)]
                      [nam (cons (new-= pattern nam)
                                 asserts)]))]
    (new-op op pattern asserts v)))

(defn normalize-clause-value
  "Ensure the clause value is not an op."
  [clause]
  (let [[op pattern asserts v] clause
        [v asserts]
        (if (op? v)
          (let [nam (gensym)]
            [nam (concat asserts
                         (list (new-= nam v)))])
          [v asserts])]
    (new-op op pattern asserts v)))

(defn normalize-clause
  "Normalizes a clause (a :_fn or :_set expr)."
  [clause context]
  (-> clause
      (normalize-clause-pattern context)
      (normalize-clause-value)))

(defn decompose-assertion
  "See the description of decompose-assertions.
  Returns a list of assertions."
  [assert]
  (if (op-isa? '= assert)
    (let [destr-ops #{:seq :tup :cons '++}
          [_ left right] assert]

      (cond (op-isa? destr-ops left)
            ;; left side is a destructuring op
            (if (op? right)
              ;; the right side is an op; now separate right and left sides into separate assertions
              (let [nam (gensym)]
                (mapcat decompose-assertion (list (new-= left nam)
                                                  (new-= nam right))))
              ;; right side is not an op; now pull out any ops from the left-side destructuring op into separate assertions
              (let [f (fn [expr]
                        (if (op? expr)
                          (let [nam (gensym)]
                            [nam (decompose-assertion (new-= nam expr))])
                          [expr '()]))
                    r (map f (rest left))]
                (cons (new-= (apply new-op (first left) (map first r))
                             right)
                      (mapcat second r))))

            ;; left side is not a destructuring op; if the right side is such an op, reverse sides and try decomposing again
            (op-isa? destr-ops right)
            (decompose-assertion (new-= right left))

            ;; neither side is a destructuring op; just return the assertion
            :else (list assert)))
    ;; not an equality assertion, just return it
    (list assert)))

(defn decompose-assertions
  "Decompose the equality assertions (of clause) that contain destructuring ops,
  so that destructuring ops don't contain ops, and the other side is also not an op.
  The destructuring ops are :seq, :tup, :cons, ++."
  [clause]
  (let [[op pattern asserts v] clause
        asserts (mapcat decompose-assertion asserts)]
    (new-op op pattern asserts v)))

;; TODO: detect and change non-binding equality assertions to ==
(defn reorder-assertions
  "Reorders assertions in a :_fn or :_set expr."
  [[op pattern asserts v] context]



  (new-op op pattern asserts v))

(defn make-clause
  "Make a :_fn or :_set clause expr, depending on op.
  The guard is split into a list of assertions."
  [[pattern guard v] op]
  (let [asserts (if (op-isa? '∧ guard)
                  ;; multiple assertions; assumes nested ∧ ops have already been collapsed into one
                  (rest guard)
                  ;; the guard is just one assertion
                  (list guard))
        clause-op (if (= op :fn) :_fn :_set)]
    (new-op clause-op pattern asserts v)))

(defn refactor-fn-set
  "Refactors a :fn or :set expr."
  [expr context]
  (let [op (first expr)
        clauses-info (rest expr)
        clauses (map #(-> %
                          (make-clause op)
                          (extract-embedded-assertions)
                          (normalize-clause context)
                          (decompose-assertions)
                          (reorder-assertions context))
                     clauses-info)]
    (if (second clauses)
      (cons '∪ clauses)
      (first clauses))))
