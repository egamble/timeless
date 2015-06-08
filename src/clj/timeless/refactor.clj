(ns timeless.refactor
  "Refactor set and fn comprehensions for the Timeless interpreter."
  (:require [timeless.common :refer :all]))


(def op?
  "Is an expr an operation, rather than a name, an atomic constant, or nil (for the value of a :_set)?"
  list?)

(def make-op list)

(defn make-=
  [left right]
  (list '= left right))

(defn extract-embedded-assertions*
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
              [nam (list (make-= nam left)
                         (make-op op nam right))])))

        (op-isa? :opfn pattern)
        ;; remove the :opfn wrapper without adding any assertions
        [(second pattern) '()]

        (op? pattern)
        ;; pattern is an op, so recursively search for embedded assertions
        (let [r (map extract-embedded-assertions* pattern)]
          [(map first r)
           (mapcat second r)])

        :else [pattern '()]))

(defn extract-embedded-assertions
  "Extract embedded assertions from a clause pattern."
  [clause]
  (let [[op pattern asserts v] clause
        [pattern new-asserts] (extract-embedded-assertions* pattern)
        asserts (concat new-asserts asserts)]
    (make-op op pattern asserts v)))

(defn normalize-clause
  "Ensure the clause pattern is a free name or a constant w.r.t. context."
  [clause context]
  (let [[op pattern asserts v] clause
        [pattern asserts]
        (cond (constant? pattern context) [(make-op :const pattern)
                                           asserts]
              (symbol? pattern) [pattern asserts]
              :else (let [nam (gensym)]
                      [nam (cons (make-= pattern nam)
                                 asserts)]))]
    (make-op op pattern asserts v)))

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
                (mapcat decompose-assertion (list (make-= left nam)
                                                  (make-= nam right))))
              ;; right side is not an op; now pull out any ops from the left-side destructuring op into separate assertions
              (let [f (fn [expr]
                        (if (op? expr)
                          (let [nam (gensym)]
                            [nam (decompose-assertion (make-= nam expr))])
                          [expr '()]))
                    r (map f (rest left))]
                (cons (make-= (apply make-op (first left) (map first r))
                             right)
                      (mapcat second r))))

            ;; left side is not a destructuring op; if the right side is such an op, reverse sides and try decomposing again
            (op-isa? destr-ops right)
            (decompose-assertion (make-= right left))

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
    (make-op op pattern asserts v)))

(defn annotate-assertion
  "TODO"
  [bound assert]
  nil)

;; selected note gets these fields: :selected true :bound <names>
;; modify the :assertion field to reverse if necessary, and convert = to := if necessary
(defn select-next-note
  "TODO"
  [notes bound]
  nil)

(defn reorder-assertions*
  "TODO"
  [notes bound]
  (loop [notes notes
         bound bound
         asserts []]
    (if (seq notes)
      (let [notes (select-next-note notes bound)
            [before [note & after]] (split-with (comp not :selected) notes)]
        (if note
          (recur (concat before after)
                 (into bound (:bound note))
                 (conj asserts (:assertion note)))
          (throw (Exception. "can't reorder assertions"))))
      (seq asserts))))

(defn reorder-assertions
  "Reorders the assertions in a clause."
  [clause context]
  (let [[op pattern asserts v] clause
        bound (set (concat (keys context)
                           (when-not (op? pattern)
                             ;; pattern is not a :const op, so it must be a free name
                             [pattern])))
        notes (map (partial annotate-assertion bound)
                   asserts)
        asserts (reorder-assertions* notes bound)]
    (make-op op pattern asserts v)))

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
    (make-op clause-op pattern asserts v)))

(defn refactor-comprehension
  "Refactors a :fn or :set comprehension."
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
