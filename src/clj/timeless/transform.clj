(ns timeless.transform
  "Transform set and fn comprehensions for the Timeless interpreter."
  (:require [timeless.common :refer :all]
            [clojure.set :as set]))

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
;;; assertion tests
;;;
;;; reverse if necessary, and convert = to := if necessary
;;;
;; TODO
(defn assert-no-free?
  [assert bound-names can-be-bound]
  (when (empty? (:free (meta assert)))
    assert))

;; TODO
(defn assert-binds-one-side?
  [assert bound-names can-be-bound]
  (when (op-isa? '= assert)
    (let [[_ a b] assert
          f (fn [side] (-> side meta :can-bind seq))
          left-binds? (f a)
          right-binds? (f b)]
      (cond (and left-binds?
                 (not right-binds?))
            (make-op := a b)

            (and right-binds?
                 (not left-binds?))
            (make-op := b a)

            ;; otherwise nil
            ))))

;; TODO
(defn assert-singly-recursive?
  [assert bound-names can-be-bound]
  nil)
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; reorder assertions
;;;
(defn free-names
  [bound-names expr]
  (set/difference (:all-names (meta expr)) bound-names))

(defn can-bind
  [bound-names assert]
  (let [f (fn [side]
            (when (or (symbol? side)
                      (destructuring-op? side))
              (free-names bound-names side)))]
    (when (op-isa? '= assert)
      (let [[_ a b] assert]
        (set/union (f a) (f b))))))

(defn does-bind
  [bound-names assert]
  (if (op-isa? := assert)
    (let [[_ a _] assert]
      (free-names bound-names a))))

(defn reorder-assertions*
  [asserts bound-names can-be-bound]
  (loop [asserts asserts
         reordered-asserts []
         bound-names bound-names
         can-be-bound can-be-bound]
    (if (seq asserts)
      (let [[assert remaining-asserts]
            (some (fn [assert-test]
                    (some-rest (fn [assert]
                                 (assert-test assert bound-names can-be-bound))
                               asserts))
                  [assert-no-free?
                   assert-binds-one-side?])]
        (if assert
          (let [newly-bound (does-bind bound-names assert)]
            (recur remaining-asserts
                   (conj reordered-asserts assert)
                   (set/union bound-names newly-bound)
                   (set/difference can-be-bound newly-bound)))
          (throw (Exception. "can't reorder assertions"))))
      (sequence reordered-asserts))))

(defn reorder-assertions
  "Reorders the assertions in a clause."
  [[pattern asserts v] bound-names]
  (let [;; add a pattern name to the bound names if the pattern hasn't been tagged :const
        bound-names (if (and (symbol? pattern)
                             (not (:const (meta pattern))))
                      (set/union bound-names #{pattern})
                      bound-names)
        can-be-bound (apply set/union
                            (map (partial can-bind bound-names)
                                 asserts))
        asserts (reorder-assertions* asserts bound-names can-be-bound)]
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
