(ns timeless.refactor
  "Refactor set and fn comprehensions for the Timeless interpreter."
  (:require [timeless.common :refer :all]
            [clojure.set :refer [difference]]))


(def make-op list)

(defn make-=
  [left right]
  (make-op '= left right))

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
        (if (empty? (free-names pattern context))
          (if (or (symbol? pattern) (op? pattern))
            [(vary-meta pattern #(assoc % :const true)) asserts]
            [pattern asserts])

          (if (symbol? pattern)
            [pattern asserts]
            (let [nam (gensym)]
              [nam (cons (make-= pattern nam)
                         asserts)])))]
    (make-op op pattern asserts v)))

(def destruct-ops #{:seq :tup :cons '++})

(defn decompose-assertion
  "See the description of decompose-assertions.
  Returns a list of assertions."
  [assert]
  (if (op-isa? '= assert)
    (let [[_ left right] assert]

      (cond (op-isa? destruct-ops left)
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
            (op-isa? destruct-ops right)
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
  (let [f #(free-names-set % bound)
        assert (vary-meta assert assoc :free (f assert))]
    (if (op-isa? '= assert)
      (let [[_ left right] assert
            g (fn [side]
                (if (or (symbol? side)
                        (op-isa? destruct-ops side))
                  (vary-meta side assoc :can-bind (f side))
                  side))]
        (make-= (g left) (g right)))
      ;; must be a symbol or list
      assert)))

(defn update-assert-free
  "TODO"
  [newly-bound assert]
  (let [f #(difference % newly-bound)
        assert (vary-meta assert update-in [:free] (f assert))]
    (if (op-isa? '= assert)
      (let [[_ left right] assert
            g (fn [side]
                (if (or (op? side)
                        (symbol? side))
                  (vary-meta side update-in [:can-bind] (f side))
                  side))]
        (make-= (g left) (g right)))
      ;; must be a symbol or list
      assert)))

(defn deannotate-assertion
  "Remove temporary metadata, leaving e.g. :row and :column."
  [assert]
  (let [assert (vary-meta assert dissoc :free)]
    (if (op-isa? #{'= :=} assert)
      (let [[op left right] assert
            g (fn [side]
                (if (or (op? side)
                        (symbol? side))
                  (vary-meta side dissoc :can-bind)
                  side))]
        (make-op op (g left) (g right)))
      ;; must be a symbol or list
      assert)))

;; assert tests
;; selected note gets meta kv: :bound <names>
;; modify the :assertion field to reverse if necessary, and convert = to := if necessary
(defn assert-no-free?
  [assert _]
  (when (empty? (:free (meta assert)))
    assert))

(defn assert-binds-one-side?
  [assert _]
  (when (op-isa? '= assert)
    (let [[_ left right] assert
          f (fn [side] (-> side meta :can-bind seq))
          left-binds? (f left)
          right-binds? (f right)]
      (cond (and left-binds?
                 (not right-binds?))
            (make-op := left right)

            (and right-binds?
                 (not left-binds?))
            (make-op := right left)

            ;; otherwise nil
            ))))

(defn reorder-assertions*
  "TODO"
  [asserts bound]
  (loop [asserts asserts
         bound bound
         reordered-asserts []]
    (if (seq asserts)
      (let [[assert remaining-asserts]
            (some (fn [assert-test]
                    (some-rest (fn [assert]
                                 (assert-test assert bound))
                               asserts))
                  [assert-no-free?
                   assert-binds-one-side?])]
        (if assert
          (let [m (meta assert)
                newly-bound (:bound m)]
            (recur (map (partial update-assert-free newly-bound) remaining-asserts)
                   (into bound newly-bound)
                   (conj reordered-asserts (deannotate-assertion assert))))
          (throw (Exception. "can't reorder assertions"))))
      (sequence reordered-asserts))))

(defn reorder-assertions
  "Reorders the assertions in a clause."
  [clause context]
  (let [[op pattern asserts v] clause
        bound (set (concat (keys context)
                           (when (and (symbol? pattern)
                                      (not (:const (meta pattern))))
                             [pattern])))
        asserts (reorder-assertions*
                 (map (partial annotate-assertion bound)
                      asserts)
                 bound)]
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
