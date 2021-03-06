(ns timeless.transform.clause
  "Various transformations of Timeless clauses."
  (:require [timeless.common :refer :all]
            [clojure.set :as set]))

(defn get-maybe-free-names
  "Collect all names except those in contained comprehensions."
  [expr]
  (prn expr)
  (condf expr
   op? 
   (reduce (fn [name-set sub-expr]
             (if (op-isa? :fn sub-expr)
               name-set
               (set/union name-set (get-maybe-free-names sub-expr))) )
           #{}
           expr)

   name? #{expr}

   nil))

(defn split-assertions
  "The guard of a clause is split into a list of assertions, if it isn't already."
  [asserts]
  (mapcat #(if (op-isa? '∧ %)
             (split-assertions (rest %))
             (list %))
          asserts))

(defn make-=
  [a b]
  (list '= a b))

(defn extract-embedded-assertions*
  "Extract embedded assertions from a pattern.
  Return the new pattern and the list of embedded assertions."
  [pattern]
  (cond (op-isa? #{'= '≠ '< '> '≤ '≥ '∉ '∈ '⊂} pattern)
        ;; pattern is an embedded assertion
        (let [[op-name a b] pattern]
          (cond (nil? b) ; pattern is a right section
                (let [nam (new-name)]
                  [nam (list (list op-name nam a))])

                (name? a) ; left side is a name; extract the assertion but don't generate a new name
                [a (list pattern)]

                :else ; left side is an op or atomic constant; generate a new name and add assertions for the left and right sides
                (let [nam (new-name)]
                  [nam (list (make-= nam a)
                             (list op-name nam b))])))

        (op? pattern)
        ;; pattern is an op, so recursively search for embedded assertions
        (let [r (map extract-embedded-assertions* pattern)
              pattern (map first r)
              new-asserts (mapcat second r)]
          [pattern new-asserts])

        :else [pattern '()]))

(defn extract-embedded-assertions
  "Extract embedded assertions from a clause pattern."
  [pattern asserts]
  (let [[pattern new-asserts] (extract-embedded-assertions* pattern)
        asserts (concat new-asserts asserts)]
    [pattern asserts]))

(defn normalize-clause
  "Make the clause pattern a new name."
  [pattern asserts]
  (let [nam (new-name)]
    [nam (cons (make-= nam pattern)
               asserts)]))

(defn destructuring-op?
  "The destructuring operators are :seq, :tup, :, and ++."
  [expr]
  (op-isa? #{:seq :tup (symbol ":") '++} expr))

(defn decompose-assertion
  "Decompose assertions so that any equality op containing a destructuring operator consists of the destructuring op on the left side, containing only free names, and a single name or an atomic literal on the right side.
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
                        (let [nam (new-name)]
                          [nam (decompose-assertion (make-= nam expr))]))
                    r (map f (rest a))]
                (cons (make-= (apply list (first a) (map first r))
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
  [asserts]
  (mapcat decompose-assertion asserts))

(defn transform-clause
  "Transforms a clause, except for reordering assertions which is done
  in a second pass using the finalized :maybe-free-names tags.
  The guard is split into assertions, if it isn't split already."
  [expr]
  (if (op-isa? :fn expr)
   (let [[_ pattern v & asserts] expr
         [pattern asserts] (extract-embedded-assertions pattern (split-assertions asserts))
         [nam asserts] (normalize-clause pattern asserts)
         asserts (decompose-assertions asserts)]
     (apply list :fn nam v asserts))
   expr))

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
            (list := a b)

            (and right-binds?
                 (not left-binds?))
            (list := b a)))))

(defn test-assert-singly-recursive
  [assert free-names]
  (when (op-isa? '= assert)
    (let [[_ a b] assert]
      (cond (and (name? a)
                 (= #{a} (local-free-names b free-names)))
            (list := a b)

            (and (name? b)
                 (= #{b} (local-free-names a free-names)))
            (list := b a)))))

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
          (error "Can't reorder assertions")))
      (sequence reordered-asserts))))

(defn reorder-assertions-walk
  [bound-names expr]
  (condf expr
   (par op-isa? :fn)
   (let [free-names (set/difference (get-maybe-free-names expr)
                                    bound-names)
         [head nam v & asserts] expr
         asserts (reorder-assertions asserts
                                     (set/difference free-names #{nam}))
         bound-names (set/union bound-names free-names)]
     (apply list head nam
            (map (par reorder-assertions-walk bound-names)
                 (cons v asserts))))

   op?
   (map (par reorder-assertions-walk bound-names)
        expr)

   expr))
