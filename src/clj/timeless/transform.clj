(ns timeless.transform
  "Transform set and fn comprehensions for the Timeless interpreter."
  (:require [timeless.common :refer :all]
            [clojure.set :as set]))

;;; ---------------------------------------------------------------------------
;;; transformations unrelated to comprehensions
;;;
(defn transform-names
  "Convert (:name <name str>) to a symbol, make a gensym for an underscore, and tag the expression with the set of all names used within.
  More names will be generated during comprehension transformations, but those won't need to be visible outside of the comprehension."
  [expr]
  (cond (op-isa? :name expr)
        (let [nam (symbol (second expr))]
          (tag-name nam))

        (= '_ expr)
        (new-name) ; also sets :all-names tag

        (symbol? expr)
        (tag-name expr)

        (op? expr)
        (set-all-names expr (collect-all-names expr))
        
        :else expr))

(defn transform-nested-applies
  [expr]
  (if (and (op? expr)
           (op? (first expr)))
    (let [[[opr & args1] & args2] expr]
      (if (keyword? opr)
        expr
        (apply make-op opr (concat args1 args2))))
    expr))

(defn transform-nested-ops
  [expr]
  (cond (and (op-isa? #{'/ '-} expr)
             (op? (second expr)))
        (let [[opr2 [opr1 & args1] & args2] expr]
          (if (= opr1 opr2)
            (apply make-op opr1 (concat args1 args2))
            expr))

        (op-isa? (symbol ":") expr)
        (let [[opr2 & args2] expr]
          (if (op-isa? (symbol ":") (last args2))
            (let [[opr1 & args1] (last args2)]
              (apply make-op opr1 (concat (butlast args2) args1)))
            expr))

        (op-isa? #{'* '+ '++ '∩ '∪ '∧ '∨} expr)
        (let [[opr & args] expr
              args (mapcat (fn [sub-expr]
                             (if (op-isa? opr sub-expr)
                               (rest sub-expr)
                               (list sub-expr)))
                           args)]
          (apply make-op opr args))

        :else expr))

(defn misc-transforms
  "Make various transformations unrelated to comprehensions."
  [expr]
  (-> (if (op? expr)
        (map misc-transforms expr)
        expr)
      transform-names
      transform-nested-applies
      transform-nested-ops
;      transform-chains
      ))
;;; ---------------------------------------------------------------------------

(defn make-clause
  [pattern asserts v]
  (list pattern asserts v))

;;; ---------------------------------------------------------------------------
(defn split-assertions
  "The guard of a clause is split into a list of assertions."
  [[pattern guard v]]
  (let [asserts (if (op-isa? '∧ guard)
                  ;; multiple assertions; assumes nested ∧ ops have already been collapsed into one
                  (rest guard)
                  ;; the guard is just one assertion
                  (list guard))]
    (make-clause pattern asserts v)))
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
(defn extract-embedded-assertions*
  "Extract embedded assertions from a pattern.
  Return the new pattern and the list of embedded assertions."
  [pattern]
  (cond (op-isa? #{'= '≠ '< '> '≤ '≥ '∉ '∈ '⊂} pattern)
        ;; pattern is an embedded assertion
        (let [[op-name a b] pattern]
          (cond (nil? b) ; pattern is a right section
                (let [nam (new-name)]
                  [nam (list (make-op op-name nam a))])

                (symbol? a) ; left side is a name; extract the assertion but don't generate a new name
                [a (list pattern)]

                :else ; left side is an op or atomic constant; generate a new name and add assertions for the left and right sides
                (let [nam (new-name)]
                  [nam (list (make-= nam a)
                             (make-op op-name nam b))])))

        (op? pattern)
        ;; pattern is an op, so recursively search for embedded assertions
        (let [r (map extract-embedded-assertions* pattern)]
          [(map first r)
           (mapcat second r)])

        :else [pattern '()]))

(defn extract-embedded-assertions
  "Extract embedded assertions from a clause pattern."
  [[pattern asserts v]]
  (let [[pattern new-asserts] (extract-embedded-assertions* pattern)
        asserts (concat new-asserts asserts)]
    (make-clause pattern asserts v)))
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
(defn normalize-clause
  "Ensure the clause pattern is a free name or a constant w.r.t. bound names."
  [[pattern asserts v] bound-names]
  (let [[pattern asserts]
        (if (empty? (set/difference (:all-names (meta pattern))
                                    bound-names))
          (if (or (symbol? pattern) (op? pattern))
            [(vary-meta pattern assoc :const true) asserts]
            [pattern asserts])

          (if (symbol? pattern)
            [pattern asserts]
            (let [nam (new-name)]
              [nam (cons (make-= pattern nam)
                         asserts)])))]
    (make-clause pattern asserts v)))
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; decompose assertions
;;;
(defn destructuring-op?
  "The destructuring operators are :seq, :tup, :, and ++."
  [expr]
  (op-isa? #{:seq :tup (symbol ":") '++} expr))

(defn decompose-assertion
  "See the description of decompose-assertions.
  Returns a list of assertions."
  [assert]
  (if (op-isa? '= assert)
    (let [[_ a b] assert]

      (cond (destructuring-op? a)
            ;; left side is a destructuring op
            (if (op? b)
              ;; the right side is an op; now separate right and left sides into separate assertions
              (let [nam (new-name)]
                (mapcat decompose-assertion (list (make-= a nam)
                                                  (make-= nam b))))
              ;; right side is not an op; now pull out any ops from the left-side destructuring op into separate assertions
              (let [f (fn [expr]
                        (if (op? expr)
                          (let [nam (new-name)]
                            [nam (decompose-assertion (make-= nam expr))])
                          [expr '()]))
                    r (map f (rest a))]
                (cons (make-= (apply make-op (first a) (map first r))
                              b)
                      (mapcat second r))))

            ;; left side is not a destructuring op; if the right side is such an op, reverse sides and try decomposing again
            (destructuring-op? b)
            (decompose-assertion (make-= b a))

            ;; neither side is a destructuring op; just return the assertion
            :else (list assert)))
    ;; not an equality assertion, just return it
    (list assert)))

(defn decompose-assertions
  "Decompose the equality assertions (of clause) that contain destructuring ops,
  so that destructuring ops don't contain ops, and the other side is also not an op.
  The destructuring operators are :seq, :tup, :, and ++."
  [[pattern asserts v]]
  (let [asserts (mapcat decompose-assertion asserts)]
    (make-clause pattern asserts v)))
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; reorder assertions
;;;
(defn local-bindables
  [assert bindables]
  (set/intersection bindables (:all-names (meta assert))))

(defn test-no-bindables
  [assert bindables]
  (when (empty? (local-bindables assert bindables))
    assert))

(defn test-bindables-on-one-side
  [assert bindables]
  (when (op-isa? '= assert)
    (let [[_ a b] assert
          left-binds? (seq (local-bindables a bindables))
          right-binds? (seq (local-bindables b bindables))]
      (cond (and left-binds?
                 (not right-binds?))
            (make-op := a b)

            (and right-binds?
                 (not left-binds?))
            (make-op := b a)))))

(defn test-assert-singly-recursive
  [assert bindables]
  (when (op-isa? '= assert)
    (let [[_ a b] assert]
      (cond (and (symbol? a)
                 (= #{a} (local-bindables b bindables)))
            (make-op := a b)

            (and (symbol? b)
                 (= #{b} (local-bindables a bindables)))
            (make-op := b a)))))

(defn new-bindables
  [bound-names assert]
  (let [f (fn [side]
            (when (or (symbol? side)
                      (destructuring-op? side))
              (set/difference (:all-names (meta side)) bound-names)))]
    (when (op-isa? '= assert)
      (let [[_ a b] assert]
        (set/union (f a) (f b))))))

(defn reorder-assertions*
  [asserts bindables]
  (loop [asserts asserts
         reordered-asserts []
         bindables bindables]
    (if (seq asserts)
      (let [[assert remaining-asserts]
            (some (fn [assert-test]
                    (some-rest (fn [assert]
                                 (assert-test assert bindables))
                               asserts))
                  [test-no-bindables
                   test-bindables-on-one-side
                   test-assert-singly-recursive])]
        (if assert
          (recur remaining-asserts
                 (conj reordered-asserts assert)
                 (set/difference bindables (when (op-isa? := assert)
                                             (:all-names (meta (second assert))))))
          (throw (Exception. "can't reorder assertions"))))
      (sequence reordered-asserts))))

(defn reorder-assertions
  "Reorders the assertions in a clause."
  [[pattern asserts v] bound-names]
  (let [ ;; add the pattern name to the bound names if the pattern hasn't been tagged :const
        bound-names (if (and (symbol? pattern)
                             (not (:const (meta pattern))))
                      (set/union bound-names #{pattern})
                      bound-names)
        bindables (apply set/union
                         (map (partial new-bindables bound-names)
                              asserts))
        asserts (reorder-assertions* asserts bindables)]
    (make-clause pattern asserts v)))
;;; ---------------------------------------------------------------------------

(defn transform-comprehension
  "Transforms a :fn or :set comprehension."
  [comprehension context]
  (let [bound-names (set/union (keys context) predefined)
        clauses (map (fn [clause]
                       (-> clause
                           (split-assertions)
                           (extract-embedded-assertions)
                           (normalize-clause bound-names)
                           (decompose-assertions)
                           (reorder-assertions bound-names)))
                     (rest comprehension))]
    (cons (first comprehension) clauses)))
