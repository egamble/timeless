(ns timeless.eval
  "Eval the S-expression form of Timeless expressions."
  (:require [timeless.common :refer :all]))

;; In most cases where an expression can't be evaluated, fail by returning nil rather than throw an error,
;; because the failure could be an implicit type failure that is an intentional part of the program control flow.
;; Throw an error when the evaluation could never succeed, e.g. when the expression is an unbound name.
;; Also throw an error when the interpreter doesn't yet know how to evaluate the expression.

;; TODO: Figure out when eval results can be cached. Should they be only cached in context maps?

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
                                                   (list (apply make-op :seq (drop k (rest v)))))))))
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

(defn apply-fn-clause
  [[clause & args] context]
  (if (seq (rest args))
    ;; repeated eval if multiple args
    (apply'
     (apply make-op
            (apply-fn-clause (make-op clause (first args))
                             context)
            (rest args)))
    (let [[_ nam v & asserts] clause]
      (eval-asserts v asserts (assoc context nam (first args))))))

(defn cons'
  [x y & xs]
  (if (seq xs)
    (cons' x (apply cons' y xs))
    (when (op-isa? :seq y) ; fail if y isn't a seq
      (apply make-op :seq x (rest y)))))

(defn concat'
  [s1 s2 & ss]
  ;; fail if either arg is not a seq
  (when (and (op-isa? :seq s1)
             (op-isa? :seq s2))
    (let [cat (apply make-op :seq (concat (rest s1) (rest s2)))]
      (when cat
        (if (seq ss)
          (apply concat' cat ss)
          cat)))))

(defn bool? [x]
  (or (true? x) (false? x)))

(defn and' [& xs]
  (when (every? bool? xs) ; fail if not all booleans
    (every? true? xs)))

(defn or' [& xs]
  (when (every? bool? xs) ; fail if not all booleans
    (boolean (some true? xs))))

(defn member?
  [x S]
  (let [efn #(error (str "Can't check member of: " S))]
    (if (op? S)
      (let [[opr & ys] S]
        (cond
          (= :set opr)
          (let [[_ nam & asserts] S]
            (boolean
             (eval-asserts true asserts (assoc (:context (meta S))
                                               nam x))))

          (= '∩ opr) (every? (par member? x) ys)
          (= '∪ opr) (boolean (some (par member? x) ys))

          (and (= 'Img opr)
               (op-isa? :seq (first ys)))
          (boolean (some #{x} (rest (first ys))))

          :else (efn)))

      ;; so S is not itself an op
      (condp = S
        'Int (or (integer? x) (= '∞ x)) ; TODO: check doubles too
        'Char (char? x)
        (efn)))))

(defn not-member?
  [x S]
  (let [v (member? x S)]
    (when (bool? v)
      (not v))))

(defn intChar [x]
  (when (integer? x) ; TODO: check doubles too
    (char x)))

(defn charInt [x]
  (when (char? x)
    (int x)))

(defn apply'
  [expr]
  (let [[opr & r] expr
        [x & xs] r
        efn #(error (str "Can't apply: " opr))]
    (if (op? opr)
      (let [[opr' & r'] opr]
        (cond
          (= :fn opr')
          (apply-fn-clause expr (:context (meta opr)))

          (= '∪ opr')
          (when-let [v (some #(apply' (make-op % x))
                             r')]
            (if (seq xs)
              (apply' (apply make-op v xs))
              v))

          (= :right opr')
          (apply' (apply make-op (first r') x (second r') xs))

          (predefined-ops opr')
          ;; opr must be a section, otherwise it would already be eval'ed
          (apply' (concat opr r))

          :else (efn)))

      ;; so opr is not itself an op
      (cond
        (predefined-ops opr)
        (if (and (seq xs)
                 (not (#{'∩ '∪} opr)))
          
          ;; so not a section or ∩ ∪
          (let [n? (fn [g]
                     (fn [& ys]
                       (when (every? number? ys) ; fail if args aren't numbers
                         (apply g ys))))
                f (case opr
                    = =, ≠ not=
                    :cons cons'
                    ++ concat'
                    + (n? +), - (n? -)
                    * (n? *), / (n? /)
                    < (n? <), > (n? >)
                    ≤ (n? <=), ≥ (n? >=)
                    ∧ and', ∨ or'
                    ∈ member? ∉ not-member?
                    (efn))]
            (apply f r))

          expr)

        (= :neg opr) (- x)

        (= 'intChar opr) (intChar x)
        (= 'charInt opr) (charInt x)

        (predefined opr) expr

        :else (efn)))))

(defn eval'
  ([expr]
   (eval' expr {}))

  ([expr context]
   (let [f (fn []
             (let [s (map #(eval' % context) expr)]
               (when (every? not-nil? s)
                 (apply make-op s))))]
     (condf expr
            (par op-isa? #{:fn :set})
            (with-meta expr {:context context})

            (par op-isa? #{:seq :tup}) (f)

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
