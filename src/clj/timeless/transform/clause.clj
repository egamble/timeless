(ns timeless.transform.clause
  "Various transformations of Timeless clauses."
  (:require [timeless.common :refer :all]
            [clojure.set :as set]))

;;; ---------------------------------------------------------------------------
(defn split-assertions
  "The guard of a clause is split into a list of assertions."
  [[pattern guard v]]
  (let [asserts (cond (nil? guard)
                      '()

                      (op-isa? '∧ guard) ; multiple assertions; assumes nested ∧ ops have already been collapsed into one
                      (rest guard)

                      :else (list guard) ; the guard is just one assertion
                      )]
    (list pattern asserts v)))
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

                (name? a) ; left side is a name; extract the assertion but don't generate a new name
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
    (list pattern asserts v)))
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
(defn normalize-clause
  "Ensure the clause pattern is a free name or a constant w.r.t. bound names."
  [[pattern asserts v] bound-names]
  (prn (str ">>" pattern (:all-names (meta pattern))))
  (let [[pattern asserts]
        (if (empty? (set/difference (:all-names (meta pattern))
                                    bound-names))
          (if (or (name? pattern) (op? pattern))
            [(vary-meta pattern assoc :const true) asserts]
            [pattern asserts])

          (if (name? pattern)
            [pattern asserts]
            (let [nam (new-name)]
              [nam (cons (make-= pattern nam)
                         asserts)])))]
    (list pattern asserts v)))
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
    (list pattern asserts v)))
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
      (cond (and (name? a)
                 (= #{a} (local-bindables b bindables)))
            (make-op := a b)

            (and (name? b)
                 (= #{b} (local-bindables a bindables)))
            (make-op := b a)))))

(defn new-bindables
  [bound-names assert]
  (let [f (fn [side]
            (when (or (name? side)
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
          (error "can't reorder assertions")))
      (sequence reordered-asserts))))

(defn reorder-assertions
  "Reorders the assertions in a clause."
  [[pattern asserts v] bound-names]
  (let [;; add the pattern name to the bound names if the pattern hasn't been tagged :const
        bound-names (if (and (name? pattern)
                             (not (:const (meta pattern))))
                      (set/union bound-names #{pattern})
                      bound-names)
        bindables (apply set/union
                         (map (par new-bindables bound-names)
                              asserts))
        asserts (reorder-assertions* asserts bindables)]
    (list pattern asserts v)))
;;; ---------------------------------------------------------------------------

(defn transform-clause
  "Transforms a :fn or :set clause"
  [clause context]
  (let [bound-names (set/union (keys context) predefined)
        [opr & parts] clause
        parts (-> parts
                  (split-assertions)
                  (extract-embedded-assertions)
                  (normalize-clause bound-names)
                  (decompose-assertions)
                  (reorder-assertions bound-names))]
    (apply make-op opr parts)))
