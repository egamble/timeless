(ns timeless.eval
  "Eval the S-expression form of Timeless expressions."
  (:require [timeless.common :refer :all]))

;; In most cases where an expression can't be evaluated, fail by returning nil rather than throw an error,
;; because the failure could be an implicit type failure that is an intentional part of the program control flow.
;; Throw an error when the evaluation could never succeed, e.g. when the expression is an unbound name.
;; Also throw an error when the interpreter doesn't yet know how to evaluate the expression.

;; TODO: Figure out why 0 .. ∞ is not lazy.

;; TODO: Modify interpreter to use Haskell-style thunks for all fn applications, and go back to atoms in context maps to cache values.

(declare eval')

;; This produces a lazy sequence of splits (into n pieces), even when coll is lazy and indefinitely long.
(defn splits*
  [n coll]
  (if (= n 1)
    (list (list coll))
    (lazy-cat (for [ss (splits* (dec n) coll)]
                (cons '() ss))
              (when (seq coll)
                (let [x (first coll)]
                  (for [[s & ss] (splits* n (rest coll))]
                    (cons (cons x s) ss)))))))

(defn splits
  [n v]
  (for [ss (splits* n (rest v))]
    (for [s ss]
      (cons :seq s))))

;; TODO: This is broken because (:cons a b) is now ((:cons a) b), etc.

(defn get-pattern-contexts
  "Returns one context (in a list for mapcatting) for v if pattern is a name, or a cons, :seq, or :tup op.
  Returns a list of contexts for each split of v if pattern is a '++ op."
  [pattern v context]
  (if (name? pattern)
    (list (assoc context pattern v))

    ;; must be destructuring op
    (let [[opr & names] pattern
          n (count names)
          k (dec n)]
      (case opr
        '++ (when (op-isa? :seq v)
              (map #(merge context (zipmap names %))
                   (splits (count names) v)))
        :cons (when (and (op-isa? :seq v)
                         (>= (count (rest v)) k))
                (list (merge context
                             (zipmap names (concat (take k (rest v))
                                                   (list (cons :seq (drop k (rest v)))))))))
        (:seq :tup) (when (and (op-isa? opr v)
                               (= (count (rest v)) n))
                      (list (merge context (zipmap names (rest v)))))))))

(defn get-assignment-contexts
  [assignment context]
  (let [[_ a b] assignment
        v (eval' b context)]
    (if (op-isa? :multi v)
      (mapcat #(get-pattern-contexts a % context) (rest v))
      (get-pattern-contexts a v context))))

(defn eval-asserts
  [v asserts context]
  (if (seq asserts)
    (let [[assert & r] asserts]
      (if (op-isa? := assert)
        (let [contexts (get-assignment-contexts assert context)
              vs (map #(eval-asserts v r %) contexts)
              vs (remove nil? vs)]
          (when (seq vs)
            (if (seq (rest vs))
              (cons :multi vs)
              (first vs))))
        (when (eval' assert context)
          (eval-asserts v r context))))
    (eval' v context)))

(declare apply')

(defn apply-clause
  [[clause x]]
  (let [[_ nam v & asserts] clause]
    (eval-asserts v asserts (assoc (:context (meta clause))
                                   nam x))))

(defn cons'
  [x s]
  (when (op-isa? :seq s) ; fail if s isn't a seq
    (apply list :seq x (rest s))))

(defn concat'
  [s1 s2]
  ;; fail if either arg is not a seq
  (when (and (op-isa? :seq s1)
             (op-isa? :seq s2))
    (when-let [cat (cons :seq (concat (rest s1) (rest s2)))]
      cat)))

(defn bool? [x]
  (or (true? x) (false? x)))

(defn and' [x y]
  (when (and (bool? x) (bool? y))
    (and x y)))

(defn or' [x y]
  (when (and (bool? x) (bool? y))
    (or x y)))

(defn member?
  [x S]
  (let [efn #(error (str "Can't check member of: " S))]
    (if (op? S)
      (let [[opr & ys] S]
        (cond
          (and (= :fn opr) (= true (second ys)))
          (let [[nam v & asserts] ys]
            (boolean
             (eval-asserts true asserts (assoc (:context (meta S))
                                               nam x))))

          (= :intersection opr) (every? (par member? x) ys)
          (= :union opr) (boolean (some (par member? x) ys))

          (and (= 'Im opr)
               (op-isa? :seq (first ys)))
          (boolean (some #{x} (rest (first ys))))

          :else (efn)))

      ;; so S is not itself an op
      (condp = S
        'Int (or (integer? x) (= '∞ x)) ; TODO: check doubles too
        'Char (char? x)
        'Seq (op-isa? :seq x)
        (efn)))))

(defn not-member?
  [x S]
  (let [v (member? x S)]
    (when (bool? v)
      (not v))))

(defn union
  [S1 S2]
  (cons :union
   (concat (if (op-isa? :union S1)
             (rest S1)
             (list S1))
           (if (op-isa? :union S2)
             (rest S2)
             (list S2)))))

(defn intersection
  [S1 S2]
  (cons :intersection
   (concat (if (op-isa? :intersection S1)
             (rest S1)
             (list S1))
           (if (op-isa? :intersection S2)
             (rest S2)
             (list S2)))))

(defn error-apply
  [expr]
  (error (str "Can't apply: " expr)))

(defn apply-section
  [[opr x] y]
  (let [n? (fn [g]
             (fn [x y]
               (when (and (number? x)
                          (number? y))
                 (g x y))))
        f (case opr
            = =, ≠ not=
            :cons cons'
            ++ concat'
            + (n? +), - (n? -)
            * (n? *), / (n? /)
            < (n? <), > (n? >)
            ≤ (n? <=), ≥ (n? >=)
            ∧ and', ∨ or'
            ∈ member?, ∉ not-member?
            ∩ intersection, ∪ union
            (error-apply (list opr x y)))]
    (f x y)))

(defn apply-op
  [expr]
  (let [[[opr & xs] y] expr] 
    (condf opr-isa? opr
     :fn
     (apply-clause expr)

     :union
     (some #(apply' (list % y)) xs) ; nil if all nil

     :right
     (apply' (list (first xs) y (second xs)))

     predefined-ops
     (apply-section expr)

     (error-apply (cons opr xs)))))

(defn intChar [x]
  (when (integer? x) ; TODO: check doubles too
    (char x)))

(defn charInt [x]
  (when (char? x)
    (int x)))

(defn len [s]
  (when (op-isa? :seq s)
    (count (rest s))))

(defn apply'
  [expr]
  (let [[opr x] expr]
    (if (op? opr)
      (apply-op expr)
      (condp opr-isa? opr
             predefined-ops expr ; section
             :neg (- x)
             'intChar (intChar x)
             'charInt (charInt x)
             'len (len x)
             (error-apply expr)))))

(defn eval'
  ([expr]
   (eval' expr {}))

  ([expr context]
   (let [f (fn []
             (let [s (map #(eval' % context) expr)]
               (when (every? not-nil? s)
                 s)))]
     (condf expr
            (par op-isa? :fn)
            (if (:context (meta expr))
              expr
              (with-meta expr {:context context}))

            (par op-isa? #{:seq :tup :right}) (f)

            op?
            (let [s (f)]
              (when s (apply' s)))

            name?
            (cond
              (not-nil? (context expr))
              (eval' (context expr) context)

              (= expr 'true) true
              (= expr 'false) false

              (predefined expr)
              expr

              :else (error (str "Undefined name: " expr)))

            string? (cons :seq expr)

            (par = :nospace) \u200B

            expr))))
