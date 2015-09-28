(ns timeless.eval
  "Eval the S-expression form of Timeless expressions."
  (:require [timeless.common :refer :all]))

;; In most cases where an expression can't be evaluated, fail by returning nil rather than throwing an error,
;; because the failure could be an implicit type failure that is an intentional part of the program control flow.

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
      (if (= :cons opr)
        (if (and (op-isa? :seq v)
                 (>= (count (rest v)) k))
          (list (merge context
                       (zipmap names (concat (take k (rest v))
                                             (list (apply make-op :seq (drop k (rest v))))))))
          '())
        (case opr
          (:seq :tup) (if (and (op-isa? opr v)
                               (= (count (rest v)) n))
                        (list (merge context (zipmap names (rest v))))
                        '())
          '++ (if (op-isa? :seq v)
                (map #(merge context (zipmap names %))
                     (splits (count names) v))
                '()))))))

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

(defn apply-fn
  [[clause & args] context]
  (if (seq (rest args))
    ;; repeated eval if multiple args
    (apply'
     (apply make-op
            (apply-fn (make-op clause (first args))
                      context)
            (rest args))
     context)
    (let [[_ nam v & asserts] clause]
      (eval-asserts v asserts (assoc context nam (first args))))))

(defn seq-str?
  [s]
  (boolean ; coerce nil to false so result can be equality tested
   (when (char? (second s))
     (every? char? (rest s)))))

(defn cons'
  [x y & xs]
  (if (seq xs)
    (cons' x (apply cons' y xs))
    (when (and (op-isa? :seq y) ; fail if y isn't a seq
               (or (not (seq-str? y)) ; fail if y is a string and x isn't a char
                   (char? x)))
      (apply make-op :seq x (rest y)))))

(defn concat'
  [s1 s2 & ss]
  ;; fail if either arg is not a seq
  (when (and (op-isa? :seq s1)
             (op-isa? :seq s2))
    (let [cat
          ;; fail if string is concatenated with non-string
          (when (= (seq-str? s1)
                   (seq-str? s2))
            (apply make-op :seq (concat (rest s1) (rest s2))))]
      (when cat
        (if (seq ss)
          (apply concat' cat ss)
          cat)))))

(defn bool?
  [x]
  (or (true? x) (false? x)))

(defn and'
  [& xs]
  (when (every? bool? xs) ; fail if not all booleans
    (every? true? xs)))

(defn or'
  [& xs]
  (when (every? bool? xs) ; fail if not all booleans
    (boolean (some true? xs))))

(defn apply'
  [expr context]
  (let [[opr & args] expr
        ]
    (cond
      (op-isa? :fn opr)
      (apply-fn expr context)

      (op-isa? '∪ opr)
      (some #(apply' (apply make-op % args)
                     context)
            (rest opr))

      (op-isa? predefined-ops opr)
      ;; must be a section, otherwise opr would already be eval'ed
      (apply' (apply make-op (concat opr args))
              context)

      ;; TODO fail on type failure for arithmetic and set ops rather than throwing error
      (predefined-ops opr)
      (if (seq (rest args))                ; if not a section
        (let [f 
              (case opr
                + +, - -, / /
                :cons cons'
                ++ concat'
                = =, ≠ not=
                < <, > >, ≤ <=, ≥ >=
                ∧ and', ∨ or'
                nil                ; TODO should never be nil
                )]
          (if f
            (apply f args)
            expr))
        expr)

      (= :neg opr)
      (- (first args))

      (predefined opr)
      expr

      :else
      nil ; fail if impossible to apply
      )))

(defn eval'
  ([expr]
   (eval' expr {}))
  ([expr context]
   (condf expr
    (par op-isa? #{:fn :set})
    expr

    (par op-isa? #{:seq :tup})
    (apply make-op (map #(eval' % context) expr))

    op?
    (apply' (apply make-op (map #(eval' % context) expr))
            context)

    name?
    (cond
      (context expr)
      (eval' (context expr) context)

      (= expr 'true) true
      (= expr 'false) false

      (predefined expr)
      expr

      :else (error "undefined name"))

    string?
    (cons :seq expr)

    expr)))
