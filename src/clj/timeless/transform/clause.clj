(ns timeless.transform.clause
  "Various transformations of Timeless clauses."
  (:require [timeless.common :refer :all]
            [clojure.set :as set]))

;;; ---------------------------------------------------------------------------
(defn split-assertions
  "The guard of a clause is split into a list of assertions."
  [guard]
  (cond (nil? guard)
        '()

        (op-isa? '∧ guard) ; multiple assertions; assumes nested ∧ ops have already been collapsed into one
        (rest guard)

        :else (list guard)           ; the guard is just one assertion
        ))
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
        (let [r (map extract-embedded-assertions* pattern)
              pattern (set-maybe-free-names (map first r))
              new-asserts (mapcat second r)]
          [pattern new-asserts])

        :else [pattern '()]))

(defn extract-embedded-assertions
  "Extract embedded assertions from a clause pattern."
  [[pattern asserts]]
  (let [[pattern new-asserts] (extract-embedded-assertions* pattern)
        asserts (concat new-asserts asserts)]
    [pattern asserts]))
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
(defn normalize-clause
  "Ensure the clause pattern is a name (free or bound) or an atomic constant."
  [[pattern asserts]]
  (if (op? pattern)
    (let [nam (new-name)]
      [nam (cons (make-= nam pattern)
                 asserts)])
    [pattern asserts]))
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
  [[pattern asserts]]
  (let [asserts (mapcat decompose-assertion asserts)]
    [pattern asserts]))
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
(defn transform-clauses
  "Transforms :fn and :set clauses, except for reordering assertions which is done
  in a second pass using the finalized :maybe-free-names tags."
  [expr]
  (if (op-isa? #{:fn :set} expr)
   (let [[opr pattern & r] expr
         [v guard] (if (= opr :fn)
                     r
                     [nil (first r)]    ; :set has no return value
                     )
         [pattern asserts]
         (-> [pattern (split-assertions guard)]
             (extract-embedded-assertions)
             (normalize-clause)
             (decompose-assertions))]
     (apply make-op opr (if (= opr :fn)
                          (apply list pattern v asserts)
                          (apply list pattern asserts))))
   expr))
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; reorder assertions
;;;
(defn local-free-names
  [assert free-names]
  (set/intersection free-names (get-maybe-free-names assert)))

(defn test-no-free-names
  [assert free-names]
  (when (empty? (local-free-names assert free-names))
    assert))

(defn test-free-names-on-one-side
  [assert free-names]
  (when (op-isa? '= assert)
    (let [[_ a b] assert
          left-binds? (seq (local-free-names a free-names))
          right-binds? (seq (local-free-names b free-names))]
      (cond (and left-binds?
                 (not right-binds?))
            (make-op := a b)

            (and right-binds?
                 (not left-binds?))
            (make-op := b a)))))

(defn test-assert-singly-recursive
  [assert free-names]
  (when (op-isa? '= assert)
    (let [[_ a b] assert]
      (cond (and (name? a)
                 (= #{a} (local-free-names b free-names)))
            (make-op := a b)

            (and (name? b)
                 (= #{b} (local-free-names a free-names)))
            (make-op := b a)))))

(defn reorder-assertions
  [asserts free-names]
  (loop [asserts asserts
         reordered-asserts []
         free-names free-names]
    (if (seq asserts)
      (let [[assert remaining-asserts]
            (some (fn [assert-test]
                    (some-rest (fn [assert]
                                 (assert-test assert free-names))
                               asserts))
                  [test-no-free-names
                   test-free-names-on-one-side
                   test-assert-singly-recursive])]
        (if assert
          (recur remaining-asserts
                 (conj reordered-asserts assert)
                 (set/difference free-names (when (op-isa? := assert)
                                              (get-maybe-free-names (second assert)))))
          (error "can't reorder assertions")))
      (sequence reordered-asserts))))

(defn reorder-assertions-recursively
  [bound-names expr]
  (condf expr
   (par op-isa? #{:fn :set})
   (let [free-names (set/difference (get-maybe-free-names expr)
                                    bound-names)
         [opr pattern & r] expr
         [v & asserts] (if (= opr :fn)
                         r
                         (cons nil r)   ; :set has no return value
                         )
         asserts (reorder-assertions asserts
                                     (if (name? pattern)
                                       (set/difference free-names #{pattern})
                                       free-names))
         bound-names (set/union bound-names free-names)

         expr
         (apply make-op opr (map (par reorder-assertions-recursively bound-names)
                                 (if (= opr :fn)
                                   (apply list pattern v asserts)
                                   (apply list pattern asserts))))]
     (set-free-names expr free-names))

   op?
   (apply make-op
    (map (par reorder-assertions-recursively bound-names)
         expr))

   expr))
;;; ---------------------------------------------------------------------------