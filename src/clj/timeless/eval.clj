(ns timeless.eval
  "Eval the S-expression form of Timeless expressions."
  (:require [timeless.common :refer :all]))

;; In most cases where an expression can't be evaluated, fail by returning nil rather than throwing an error,
;; because the failure could be an implicit type failure that is an intentional part of the program control flow.

(declare eval-expr)

;; This isn't actually lazy, but it won't matter until the rest of the interpreter is lazy.
(defn splits
  [n coll]
  (if (= n 1)
    (list (list coll))
    (concat (for [ss (splits (dec n) coll)]
              (cons '() ss))
            (when (seq coll)
              (let [x (first coll)]
                (for [[s & ss] (splits n (rest coll))]
                  (cons (cons x s) ss)))))))

;; An ordering over all the tuples of monotonically increasing indices, where the max of indices is mx.
;; Takes such a tuple and returns the next one.
;; Increment the leftmost possible index, i.e. the leftmost index such that it's less than the index to its right.
(defn inc-split-indices [indices n mx]
  )

(defn splits-str-or-seq
  [n v]
  (if (string? v)
    (map #(map (par apply str) %)
         (splits n (seq v)))
    (map #(map (par cons :seq) %)
         (splits n (rest v)))))

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
      (if (= opr cons-op)
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
          '++ (if (or (string? v)
                      (op-isa? :seq v))
                (map #(merge context (zipmap names %))
                     (splits-str-or-seq (count names) v))
                '()))))))

(defn get-assignment-contexts
  [assignment context]
  (let [[_ a b] assignment
        v (eval-expr b context)]
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
            (if (second vs)
              (cons :multi vs)
              (first vs))))
        (when (eval-expr assert context)
          (eval-asserts v r context))))
    (eval-expr v context)))

(declare eval-apply)

(defn eval-fn
  [[clause & args] context]
  (if (second args)
    ;; repeated eval if multiple args
    (eval-apply
     (apply make-op
            (eval-fn (make-op clause (first args))
                     context)
            (rest args))
     context)
    (let [[_ nam v & asserts] clause]
      (eval-asserts v asserts (assoc context nam (first args))))))

(defn concat-seqs
  [& seqs]
  (if (some string? seqs)
    (when (every? string? seqs) ; fail if string is concatenated with non-string
      (apply str seqs))
    (if (every? (par op-isa? :seq) seqs) ; fail if seq is concatenated with non-seq
      (apply make-op :seq (mapcat rest seqs)))))

;; TODO refactor
(defn eval-apply
  [expr context]
  (let [[opr & args] expr]
    (cond
      (op-isa? :fn opr)
      (eval-fn expr context)

      (op-isa? '∪ opr)
      (some #(eval-apply (apply make-op % args)
                         context)
            (rest opr))

      (op-isa? #{'+ '- '* '/ '++} opr)
      ;; must be a section
      (eval-apply (apply make-op (concat opr args))
                  context)

      (#{'+ '- '* '/ '++} opr)
      (if (second args) ; if not a section
        (apply (case opr
                 + +
                 - -
                 * *
                 / /
                 ++ concat-seqs
                 )
               args)
        expr)

      (= '* opr)
      (if (second args) ; if not a section
        (apply * args)
        expr)

      (= :neg opr)
      (- (first args))

      (predefined opr)
      expr

      :else
      nil ; fail if impossible to apply
      )))

(defn eval-expr
  ([expr]
   (eval-expr expr {}))
  ([expr context]
   (condf expr
    (par op-isa? #{:fn :set})
    expr

    (par op-isa? :seq)
    (let [elts (map #(eval-expr % context)
                    (rest expr))]
      (if (every? char? elts)
        (apply str elts)
        (apply make-op :seq elts)))

    op?
    (eval-apply (apply make-op (map #(eval-expr % context) expr))
                context)

    name?
    (cond
      (context expr)
      (eval-expr (context expr) context)

      (predefined expr)
      expr

      :else (error "undefined name"))

    expr)))
