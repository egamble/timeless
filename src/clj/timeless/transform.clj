(ns timeless.transform
  "Transform set and fn comprehensions for the Timeless interpreter."
  (:require [timeless.common :refer :all]
            [clojure.set :as set]))


(def make-op list)

(defn make-=
  [left right]
  (make-op '= left right))

(defn split-assertions
  "The guard of a clause is split into a list of assertions."
  [[pattern guard v]]
  (let [asserts (if (op-isa? '∧ guard)
                  ;; multiple assertions; assumes nested ∧ ops have already been collapsed into one
                  (rest guard)
                  ;; the guard is just one assertion
                  (list guard))]
    (list pattern asserts v)))

(defn extract-embedded-assertions*
  "Extract embedded assertions from a pattern.
  Return the new pattern and the list of embedded assertions."
  [pattern]
  (cond (op-isa? #{'= '≠ '< '> '≤ '≥ '∉ '∈ '⊂} pattern)
        ;; pattern is an embedded assertion
        (let [[op-name a b] pattern]
          (cond (nil? b) ; pattern is a right section
                (let [nam (gensym)]
                  [nam (list (make-op op-name nam a))])

                (symbol? a) ; left side is a name; extract the assertion but don't generate a new name
                [a (list pattern)]

                :else ; left side is an op or atomic constant; generate a new name and add assertions for the left and right sides
                (let [nam (gensym)]
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

(defn normalize-clause
  "Ensure the clause pattern is a free name or a constant w.r.t. context."
  [[pattern asserts v] context]
  (let [[pattern asserts]
        (if (empty? (free-names pattern context))
          (if (or (symbol? pattern) (op? pattern))
            [(vary-meta pattern #(assoc % :const true)) asserts]
            [pattern asserts])

          (if (symbol? pattern)
            [pattern asserts]
            (let [nam (gensym)]
              [nam (cons (make-= pattern nam)
                         asserts)])))]
    (list pattern asserts v)))

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
              (let [nam (gensym)]
                (mapcat decompose-assertion (list (make-= a nam)
                                                  (make-= nam b))))
              ;; right side is not an op; now pull out any ops from the left-side destructuring op into separate assertions
              (let [f (fn [expr]
                        (if (op? expr)
                          (let [nam (gensym)]
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
  [clause]
  (let [[pattern asserts v] clause
        asserts (mapcat decompose-assertion asserts)]
    (make-op pattern asserts v)))

(defn tag-assert-free
  "TODO"
  [bound-names assert]
  (let [f #(set-of-free-names % bound-names)
        assert (vary-meta assert assoc :free (f assert))]
    (if (op-isa? '= assert)
      (let [[_ a b] assert
            g (fn [side]
                (if (or (symbol? side)
                        (destructuring-op? side))
                  (vary-meta side assoc :can-bind (f side))
                  side))]
        (make-= (g a) (g b)))
      ;; must be a symbol or list
      assert)))

(defn update-assert-free
  "TODO"
  [newly-bound-names assert]
  (let [f #(set/difference % newly-bound-names)
        assert (vary-meta assert update-in [:free] (f assert))]
    (if (op-isa? '= assert)
      (let [[_ a b] assert
            g (fn [side]
                (if (or (op? side)
                        (symbol? side))
                  (vary-meta side update-in [:can-bind] (f side))
                  side))]
        (make-= (g a) (g b)))
      ;; must be a symbol or list
      assert)))

(defn untag-assert-free
  "Remove temporary metadata for :free and :can-bind, but leave e.g. :row and :column."
  [assert]
  (let [assert (vary-meta assert dissoc :free)]
    (if (op-isa? #{'= :=} assert)
      (let [[op-name a b] assert
            g (fn [side]
                (if (or (op? side)
                        (symbol? side))
                  (vary-meta side dissoc :can-bind)
                  side))]
        (make-op op-name (g a) (g b)))
      ;; must be a symbol or list
      assert)))


;; -----------------------------------------------------
;; assert tests

;; selected annotation gets meta key value: :bound <names>
;; modify the :assertion field to reverse if necessary, and convert = to := if necessary

(defn assert-no-free?
  [assert _]
  (when (empty? (:free (meta assert)))
    assert))

(defn assert-binds-one-side?
  [assert _]
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
;; -----------------------------------------------------


(defn reorder-assertions*
  "TODO"
  [asserts bound-names]
  (loop [asserts asserts
         bound-names bound-names
         reordered-asserts []]
    (if (seq asserts)
      (let [[assert remaining-asserts]
            (some (fn [assert-test]
                    (some-rest (fn [assert]
                                 (assert-test assert bound-names))
                               asserts))
                  [assert-no-free?
                   assert-binds-one-side?])]
        (if assert
          (let [m (meta assert)
                newly-bound-names (:bound m)]
            (recur (map (partial update-assert-free newly-bound-names) remaining-asserts)
                   (into bound-names newly-bound-names)
                   (conj reordered-asserts (untag-assert-free assert))))
          (throw (Exception. "can't reorder assertions"))))
      (sequence reordered-asserts))))

(defn reorder-assertions
  "Reorders the assertions in a clause."
  [[pattern asserts v] context]
  (let [ ;; The current bound names are the ones in the context together with
        ;; the pattern name if the pattern hasn't been tagged :const.
        bound-names (set (concat (keys context)
                                 (when (and (symbol? pattern)
                                            (not (:const (meta pattern))))
                                   [pattern])))
        asserts (reorder-assertions*
                 (map (partial tag-assert-free bound-names)
                      asserts)
                 bound-names)]
    (list pattern asserts v)))

(defn transform-comprehension
  "Transforms a :fn or :set comprehension."
  [comprehension context]
  (let [clauses (map (fn [clause]
                       (-> clause
                           (split-assertions)
                           (extract-embedded-assertions)
                           (normalize-clause context)
                           (decompose-assertions)
                           (reorder-assertions context)))
                     (rest comprehension))]
    (cons (first comprehension) clauses)))
