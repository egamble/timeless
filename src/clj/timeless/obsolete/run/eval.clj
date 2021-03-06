(ns timeless.tls.eval
  "Eval TLS expressions."
  (:require [timeless.common :refer :all]))

;; In most cases where an expression can't be evaluated, fail (return nil) rather than throw an error,
;; because the failure could be an implicit type failure that is an intentional part of the program control flow.
;; Throw an error when the evaluation could never succeed, e.g. when the expression is an unbound name.
;; Also throw an error when the interpreter doesn't (yet) know how to evaluate the expression.


;; TODO:
;; ∩ ∪ → ⇸ = ∈ ⊂ :∈
;; U, Num, Int, Bool, Char, Set, Fn, Seq, Str
;; Dm, Im


(declare eval')
(declare eval-for)
(declare equal*)

(defn splits
  "Returns a lazy sequence of all the splits of coll into two sequences."
  [coll]
  (lazy-seq
   (cons (list '() coll)
         (when (seq coll)
           (for [[s & ss] (splits (rest coll))]
             (cons (cons (first coll) s) ss))))))

(defn bind-name [nam v context]
  (let [a (atom nil)
          context (assoc context nam a)
          v (set-context v context)]
      (reset! a v)
      context))

(declare make-binding)

(defn make-bindings
  "Recursively call make-binding for lists of patterns and values.
The lists of patterns and values are known to be equal length."
  [patterns vs context]
  (if (seq patterns)
    (some->> context
             (make-binding (first patterns) (first vs))
             (make-bindings (rest patterns) (rest vs)))
    context))

(defn make-map-binding [pattern v context strict?]
  (when-let [v (eval-for :atom-map v context)]
    (let [f (fn [clauses]
              (into {} (map (par into [])
                            (partition 2 clauses))))
          mp (f (rest pattern))
          mv (f (rest v))]
      (when (or (not strict?)
                (= (count mp) (count mv)))
        (when-let [[patterns vs]
                   (reduce-kv (fn [[patterns vs] k p]
                                (if-some [vv (mv k)]
                                  [(cons p patterns)
                                   (cons vv vs)]
                                  (reduced nil)))
                              [nil nil]
                              mp)]
          (make-bindings patterns vs context))))))

(defn make-binding
  "Make a new context or contexts with the names in pattern bound to parts of the eval of v.
v is not eval'ed if the pattern is a single name. Return nil if evaluation of v fails.
Multiple contexts are returned if pattern contains ++.
The returned context or contexts are returned in a list."
  [pattern v context]
  (if (seq? context)
    (mapcat #(make-binding pattern v %) context)
    (if (name? pattern)
      (list (bind-name pattern v context))
      (let [[head & xs] pattern
            ef #(error (str "Unknown destructuring pattern: " pattern))]
        (case head
          :tup (when-let [[_ & ys] (eval-for (count xs) v context)]
                 (make-bindings xs ys context))
          :cons (when-let [[_ & ys] (eval-for :cons v context)]
                  (make-bindings xs ys context))
          :seq (when-let [[_ & ys] (eval-for :seq v context)]
                 (when (= (count xs) (count ys))
                   (make-bindings xs ys context)))
          ++ (when-let [[_ & ys] (eval-for :seq v context)]
               (mapcat (fn [split]
                         (make-bindings xs
                                        (map #(cons :seq %) split)
                                        context))
                       (splits ys)))
          :map (make-map-binding pattern v context true)
          ∪ (let [x (first xs)]
              (if (op-isa? :map x)
                (make-map-binding x v context false)
                (ef)))
          :neg (when-let [y (eval-for :num v context)]
                 (make-binding (first xs) (- y) context))
          (+ - * /) (when-let [y (eval-for :num v context)]
                      (let [[x1 x2] xs]
                        (if (number? x1)
                          (case head
                            + (make-binding x2 (- y x1) context)
                            - (make-binding x2 (- x1 y) context)
                            * (when (not (zero? x1))
                                (make-binding x2 (/ y x1) context))
                            / (when (not (zero? y))
                                (make-binding x2 (/ x1 y) context)))
                          (case head
                            + (make-binding x1 (- y x2) context)
                            - (make-binding x1 (+ y x2) context)
                            * (when (not (zero? x2))
                                (make-binding x1 (/ y x2) context))
                            / (make-binding x1 (* y x2) context)))))
          (ef))))))

(defn apply-fn [expr]
  (let [context (get-context expr)
        [head x & xs] expr              ; head is already eval'ed
        [_ pattern body] head]
    (if (seq xs)
      (eval' (apply list
                    (apply-fn (set-context (list head x) context))
                    xs)
             context)
      (when-some [x (eval' x context)]
        (let [f (fn [s]
                  (let [ss (mapcat (fn [v]
                                     (when v
                                       (if (op-isa? :values v)
                                         (rest v)
                                         (list v))))
                                   s)]
                    (when (seq ss)
                      (set-context (if (seq (rest ss))
                                     (cons :values ss)
                                     (first ss))
                                   context))))]
          (if (op-isa? :values x)
            (f (map #(apply-fn (set-context (list head %)
                                            context))
                    (rest x)))
            (let [contexts (make-binding pattern (set-tag x :evaled true)
                                         context)]
              ;; body should not already have a :context metatag
              (f (map #(eval' body %) contexts)))))))))

(defn apply-seq [expr]
  (let [context (get-context expr)
        [head n & xs] expr ; head is already eval'ed
        [_ & ys] head]
    (if (seq xs)
      (eval' (apply list
                    (apply-seq (set-context (list head n) context))
                    xs)
             context)
      (when-let [n (eval-for :int n context)]
        (when (and (>= n 0) (not= n '∞))
          (try (nth ys n)
               (catch Exception e)))))))

(defn apply-cons [expr]
  (let [context (get-context expr)
        [head n & xs] expr ; head is already eval'ed
        [_ y s] head]
    (if (seq xs)
      (eval' (apply list
                    (apply-cons (set-context (list head n) context))
                    xs)
             context)
      (when-let [n (eval-for :int n context)]
        (if (= n 0)
          (set-context y context)
          (when (and (> n 0) (not= n '∞))
            (when-let [s (eval-for :seq s context)]
              (eval' (list s (dec n)) context))))))))

;; TODO: doesn't work when equal* returns nil
(defn apply-map* [clauses x context]
  (when (seq? clauses)
    (let [[[k v] & r] clauses]
      (when-let [[p _ x] (equal* k x context)]
        (if p
          v
          (recur r x context))))))

(defn apply-map [expr]
  (let [context (get-context expr)
        [head x & xs] expr ; head is already eval'ed
        [_ & ys] head]
    (if (seq xs)
      (eval' (apply list
                    (apply-map (set-context (list head x) context))
                    xs)
             context)
      (when-some [v (apply-map* (partition 2 ys) x context)]
        (eval' v context)))))

(defn apply-alt [expr]
  (let [context (get-context expr)
        [head & xs] expr ; head is already eval'ed
        [_ & ys] head]
    (set-context (cons :alt (map #(apply list % xs) ys))
                 context)))

(defn apply-values [expr]
  (let [context (get-context expr)
        [head & xs] expr ; head is already eval'ed
        [_ & ys] head]
    (set-context (cons :values (map #(apply list % xs) ys))
                 context)))

(defn eval-let
  "Eval a let construct by making bindings and eval'ing the body in the new context.
Bindings are not immediately eval'ed, so they can be recursive."
  [expr]
  (let [[_ bindings body] expr
        context (get-context expr)]
    (if (seq bindings)
      (let [[nam v & bs] bindings
            context (bind-name nam v context)]
        (eval-let
         (set-context (list :let bs body) ; assumes body doesn't have a :context metatag
                      context)))
      (eval' body context))))

(defn eval-guard [expr]
  (let [context (get-context expr)
        [_ guard body] expr]
    (when (eval-for :bool guard context) ; fail if the eval'ed guard is nil or false
      (eval' body context))))

(defn apply' [expr]
  (let [[head & xs] expr
        context (get-context expr)
        head (eval' head context)
        expr (set-context (cons head xs) context)]
    (if (op? head)
      (let [[op-head & op-xs] head]
        (cond
          (keyword? op-head)
          (let [f (case op-head
                    (:fn :set) apply-fn
                    :map apply-map
                    :seq apply-seq
                    :cons apply-cons
                    :alt apply-alt
                    :values apply-values
                    nil)]
            (when f (f expr)))

          ;; Collapse an applied section, but don't eval further, unless the collapsed applied section is itself applied.
          (and (predefined-ops op-head)
               (nil? (second op-xs)))
          (let [op (list op-head (first op-xs) (first xs))]
            (if (nil? (second xs)) ; check if the collapsed applied section is itself applied
              (set-context op context)
              (apply' (set-context (cons op (rest xs))
                                   context))))

          (= '∪ head)
          (set-context (list :alt
                             (cons (first op-xs) xs)
                             (cons (second op-xs) xs)))))

      (case head
        :name (symbol (first xs))
        :let (eval-let expr)
        :guard (eval-guard expr)
        ;; Everything else is uneval'ed.
        expr))))

(defn eval'
  "Common eval regardless of what type is expected.
The context argument is only used if the expr doesn't have a :context metatag.
Returned expressions have a :context metatag if possible."
  ([expr]
   (eval' expr {}))

  ([expr context]
   (let [x (get-context expr)
         [expr context] (if x
                          [expr x]
                          [(set-context expr context) context])]
     (if (op? expr)
       (apply' expr)
       (if (and (name? expr)
                (not (predefined expr)))
         (let [a (context expr)
               v @a]
           (if (nil? v)
             (error (str "Undefined name: " expr))
             (if (and (taggable? v)
                      (not (:evaled (meta v))))
               (let [v (eval' v)]
                 (reset! a (set-tag v :evaled true))
                 v)
               v)))
         ;; Return atomic literals, predefined names, and keywords unchanged.
         expr)))))

(defn eval-for-seq [expr]
  (let [context (get-context expr)]
    (lazy-seq
     (cond
       (op? expr)
       (let [[head & xs] expr
             f #(eval-for :seq % context)]
         (case head
           :seq expr
           :cons (when-let [[_ & s] (f (second xs))]
                   (set-context (apply list :seq (first xs) s)
                                context))
           ++ (when-let [[_ & s1] (f (first xs))]
                (when-let [[_ & s2] (f (second xs))]
                  (set-context (cons :seq (concat s1 s2))
                               context)))
           nil))

       (= :empty expr)
       (list :seq)

       (string? expr)
       (cons :seq expr)

       :else nil))))

(defn eval-for-cons [expr]
  (let [context (get-context expr)
        f #(when (first %)
             (set-context (list :cons (first %) (cons :seq (rest %)))
                          context))]
    (if (op? expr)
      (let [[head & xs] expr]
        (case head
          :cons expr
          :seq (f xs)
          ++ (when-let [[_ x y] (eval-for :cons (first xs) context)]
               (set-context (list :cons x (list '++ y (second xs)))
                            context))
          nil))
      (when (string? expr)
        (f (seq expr))))))

;; TODO: make len work for other kinds of sets
(defn len [expr context]
  (condf expr
    string? (count expr)
    op? (let [[head & xs] expr
              f #(eval-for :seq % context)]
          (case head
            :seq (count xs)
            ++ (when-let [[_ & s1] (f (first xs))]
                 (when-let [[_ & s2] (f (second xs))]
                   (+ (count s1) (count s2))))
            :cons (when-let [[_ & s] (f (second xs))]
                    (inc (count s)))
            nil))
    predefined-sets (set-context '∞ context)))

(defn eval-for-num [expr]
  (if (or (number? expr) (= '∞ expr))
    expr
    (when (op? expr)
      (let [context (get-context expr)
            [head x y] expr]
        (case head
          charInt (when-let [c (eval-for :char x context)]
                    (int c))
          len (len x context)
          :neg (when-let [x (eval-for :num x context)]
                 (- x))
          (+ - * /)
          (when-let [x (eval-for :num x context)]
            (when-let [y (eval-for :num y context)]
              (if (= head '/)
                (when (not (zero? y))
                  (/ x y))
                (case head
                  + (+ x y)
                  - (- x y)
                  * (* x y)))))
          nil)))))

(defn eval-for-int [expr]
  (when-let [x (eval-for-num expr)]
    (if (integer? x)
      x
      (let [n (int x)]
        (when (= x (float n))
          n)))))

(defn bool? [x]
  (or (true? x) (false? x)))

;; TODO
(defn equal*
  "Eval's x and y as necessary. Returns [p x y] or nil.
p is true if x equals y, o.w. false. Returns nil if either x or y eval to nil.
The eval'ed x and y are returned so they can be used in other comparisons."  
  [x y context]
  nil)

(defn equal? [expr]
  (let [context (get-context expr)
        [_ x y] expr]
    (when-let [[p _ _] (equal* x y context)]
      p)))

(defn not-equal? [expr]
  (let [v (equal? expr)]
    (when (bool? v)
      (not v))))

;; TODO
(defn member? [expr]
  ;; TODO: :set, :fn returning true, ∩, ∪, predefined sets, Im of :seq
  nil

#_(if (op? S)
    (let [[head & ys] S]
      (cond
        (and (= :fn head) (= true (second ys)))
        (let [[nam v & asserts] ys]
          (boolean
           (eval-asserts true asserts (assoc (:context (meta S))
                                             nam x))))

        (= '∩ head) (every? (par member? x) ys)
        (= '∪ head) (boolean (some (par member? x) ys))

        (and (= 'Im head)
             (op-isa? :seq (first ys)))
        (boolean (some #{x} (rest (first ys))))

        :else nil))

    ;; so S is not itself an op
    (condp = S
      'Int (or (integer? x) (= '∞ x)) ; TODO: use eval-for :int
      'Char (char? x)
      'Seq (op-isa? :seq x)
      nil)))

(defn not-member? [expr]
  (let [v (member? expr)]
    (when (bool? v)
      (not v))))

;; TODO
(defn subset? [expr]
  nil)

(defn eval-for-bool [expr]
  ;; TODO: optional comparison operations
  ;; TODO: apply of sets and set-producing fns
  (if (bool? expr)
    expr
    (when (op? expr)
      (let [context (get-context expr)
            [head x y] expr]
        (case head
          (∧ ∨) (when-some [x (eval-for :bool x context)]
                  (when-some [y (eval-for :bool y context)]
                    (case head
                      ∧ (and x y)
                      ∨ (or x y))))
          (< > ≤ ≥) (when-let [x (eval-for :num x context)]
                      (when-let [y (eval-for :num y context)]
                        (case head
                          < (< x y)
                          > (> x y)
                          ≤ (<= x y)
                          ≥ (>= x y))))
          = (equal? expr)
          ≠ (not-equal? expr)
          ∈ (member? expr)
          ∉ (not-member? expr)
          ⊂ (subset? expr)
          nil)))))

(defn eval-for-char [expr]
  (if (char? expr)
    expr
    (let [context (get-context expr)]
      (when (op? expr)
        (let [[x n] expr]
          (when-let [n (eval-for :int n context)]
            ;; No need to check for ((:seq ...) n) because of apply-seq.
            (cond
              (= 'intChar x) (when (< n 65536)
                               (char n))
              (string? x) (try (nth x n)
                               (catch Exception e))
              :else nil)))))))

(defn eval-for-str [expr]
  (if (string? expr)
    expr
    (when-let [[_ & s] (eval-for-seq expr)]
      (when (every? char? s) ; stops when an element is not a char, so works with infinite non-char seqs
        (apply str s)))))

(defn eval-for-atom [expr]
  (some-not-nil ((juxt eval-for-str
                       eval-for-num
                       eval-for-char
                       eval-for-bool)
                 expr)))

(defn eval-for-atom-map [expr]
  (when (op-isa? :map expr)
    (let [context (get-context expr)
          kvs (partition 2 (rest expr))]
      (when-let [s (reduce (fn [s [k v]]
                             (if-let [k (eval-for-atom (set-context k context))]
                               (cons k (cons v s))
                               (reduced nil)))
                           '() kvs)]
        (set-context (cons :map s)
                     context)))))

(defn eval-for-tuple [n expr]
  (when (and (op-isa? :tup expr)
             (= n (count (rest expr))))
    expr))

;; TODO: add eval-for :set
(defn eval-for
  "Eval for a particular type. Eval's expr and checks for :alt and :values before dispatching.
The type argument is a keyword describing the type, or an integer n for a tuple of arity n.
The context argument is only used if the expr doesn't have a :context metatag.
Returned expressions have a :context metatag if possible.

:alt exprs are eval'ed here rather than in eval' because they have to be
fully eval-for-<type>'ed before returning the first non-nil value.

:values exprs are treated like :alt exprs, because the contract of this function is one value of the given type or nil."
  ([type expr]
   (eval-for type expr {}))
  
  ([type expr context]
   (let [expr (eval' expr context)]
     (if (op-isa? #{:alt :values} expr)
       (some-not-nil (map #(eval-for type % context)
                          (rest expr)))
       (if (integer? type)
         (eval-for-tuple type expr)
         ((case type
            :seq eval-for-seq
            :cons eval-for-cons
            :num eval-for-num
            :int eval-for-int
            :bool eval-for-bool
            :char eval-for-char
            :atom-map eval-for-atom-map)
          expr))))))
