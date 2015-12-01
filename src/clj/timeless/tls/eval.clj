(ns timeless.tls.eval
  "Eval TLS expressions."
  (:require [timeless.common :refer :all]))

;; In most cases where an expression can't be evaluated, fail (return nil) rather than throw an error,
;; because the failure could be an implicit type failure that is an intentional part of the program control flow.
;; Throw an error when the evaluation could never succeed, e.g. when the expression is an unbound name.
;; Also throw an error when the interpreter doesn't (yet) know how to evaluate the expression.

(declare eval')
(declare eval-for)

(defn make-binding
  "Make a new context with names in pattern bound to parts of the eval of v.
v is not eval'ed if the pattern is a single name. Return nil if evaluation of v fails.
Returns a list of contexts for the ++ pattern."
  [pattern v context]
  (if (name? pattern)
    (let [a (atom nil)
          context (assoc context pattern a)
          v (set-context v context)]
      (reset! a v)
      context)
    (let [[head & xs] pattern]
      (if (seq xs)
        (case head
          :tup (when-let [[_ & ys] (eval-for (count xs) v context)]
                 (some->> context
                          (make-binding (first xs) (first ys))
                          (make-binding (cons :tup (rest xs)) (cons :tup (rest ys)))))
          :cons (when-let [[_ x y] (eval-for :cons v context)]
                  (some->> context
                           (make-binding (first xs) x)
                           (make-binding (second xs) y)))
          :seq (when-let [[_ & ys] (eval-for :seq v context)]
                 (when (= (count xs) (count ys))
                   (some->> context
                            (make-binding (first xs) (first ys))
                            (make-binding (cons :seq (rest xs)) (cons :seq (rest ys))))))
          ++ nil ; TODO; what about (:cons a (++ x y)) or (++ a (++ b c))?
          :map nil ; TODO
          ∪ nil ; TODO
          (+ - * /) nil ; TODO
          (error (str "Unknown destructuring pattern: " pattern)))
        context ; so :seq and :tup bindings can call make-binding recursively
        ))))

;; TODO: make this work with multiple binding contexts
(defn apply-fn [expr]
  (let [context (get-context expr)
        [f x & xs] expr
        [_ pattern body] f]
    (if (seq xs)
      (eval' (apply list
                    (apply-fn (set-context (list f x) context))
                    xs)
             context)
      (let [v (eval' x context)]
        (if (op-isa? :values v)
          (let [rs (mapcat #(let [w (apply-fn (set-context (apply list f %)
                                                           context))]
                              (if (op-isa? :values w)
                                (rest w)
                                (list w)))
                           (rest v))]
            (when (seq rs)
              (if (seq (rest rs))
                (cons :values rs)
                (first rs))))
          (if-let [v (vary-meta v assoc :evaled true)]
            (eval' body ; body should not already have a :context metatag
                   (make-binding pattern v context))))))))

(defn apply-seq [expr]
  (let [context (get-context expr)
        [f n & xs] expr
        [_ & ys] f]
    (if (seq xs)
      (eval' (apply list
                    (apply-seq (set-context (list f n) context))
                    xs)
             context)
      (when-let [n (eval-for :int n context)]
        (when (and (>= n 0) (not= n '∞))
          (try (nth ys n)
               (catch java.lang.IndexOutOfBoundsException e)))))))

(defn apply-cons [expr]
  (let [context (get-context expr)
        [f n & xs] expr
        [_ y s] f]
    (if (seq xs)
      (eval' (apply list
                    (apply-cons (set-context (list f n) context))
                    xs)
             context)
      (when-let [n (eval-for :int n context)]
        (if (= n 0)
          (set-context y context)
          (when (and (> n 0) (not= n '∞))
            (when-let [s (eval-for :seq s context)]
              (eval' (list s (dec n)) context))))))))

(defn apply-map [expr]
  (let [context (get-context expr)
        [f x & xs] expr
        [_ & clauses] f]
    (if (seq xs)
      (eval' (apply list
                    (apply-map (set-context (list f x) context))
                    xs)
             context)
      (when (seq clauses)
        (let [[k v & kvs] clauses]
          (eval' (list :alt
                       (list :guard (list '= k x) v)
                       (list (cons :map kvs) x))
                 context))))))

(defn eval-let
  "Eval a let construct by making bindings and eval'ing the body in the new context.
Bindings of patterns other than single names are immediately eval'ed with eval-for <type>,
which means that the eval-for <type> can only depend on bindings earlier in the bindings list. Subsequent evaluation can depend on all the bindings."
  [expr]
  (let [[_ bindings body] expr
        context (get-context expr)]
    (if (seq bindings)
      (let [[pattern v & bs] bindings
            context (make-binding pattern v context)]
        (if (seq? context)
          (error (str "Multivalued let binding: pattern " pattern " value " v))
          (eval-let
           (set-context (list :let bs body) ; assumes body doesn't have a :context metatag
                        context))))
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
   (eval' expr nil))

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
                 (reset! a (if (taggable? v)
                             (vary-meta v assoc :evaled true)
                             v))
                 v)
               v)))
         ;; Return atomic literals, predefined names, and keywords unchanged.
         expr)))))

(defn eval-for-seq [expr]
  (let [context (get-context expr)]
    (lazy-seq
     (cond
       (op? expr)
       (let [f #(eval-for :seq % context)
             [head & xs] expr]
         (case head
           :seq expr
           :cons (if-let [[_ & s] (f (second xs))]
                   (set-context (apply list :seq (first xs) s)
                                context))
           ++ (if-let [[_ & s1] (f (first xs))]
                (if-let [[_ & s2] (f (second xs))]
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

(defn eval-for-empty [expr]
  (if (= :empty expr)
    :empty
    (when (and (op-isa? :seq expr)
               (nil? (second expr)))
      :empty)))

(defn eval-for-set [expr] nil)


;; TODO >>>>>

(defn charInt [c]
  (when (char? c)
    (int c)))

(defn len [expr]
  (let [context (get-context expr)]
    (condf expr
      string? (count expr)
      op? (let [[head xs] expr
                f #(eval-for :seq % context)]
            (case head
              :seq (count xs)
              ++ (when-let [[_ & s1] (f (first xs))]
                   (when-let [[_ & s2] (f (second xs))]
                     (+ (count s1) (count s2))))
              :cons (when-let [[_ & s] (f (second xs))]
                      (inc (count s)))
              nil))
      predefined-sets (set-context '∞ context)

      ;; TODO: make len work for other kinds of sets
      )))

(defn eval-for-num [expr]
  ;; TODO: apply of :neg, len, charInt, arith ops
  (when (or (number? expr) (= '∞ expr))
    expr))

;; TODO <<<<<


(defn eval-for-int [expr]
  (when-let [x (eval-for-num expr)]
    (if (integer? x)
      x
      (let [n (int x)]
        (when (= x (float n))
          n)))))


;; TODO >>>>>

(defn bool? [x]
  (or (true? x) (false? x)))

(defn and' [x y]
  (when (and (bool? x) (bool? y))
    (and x y)))

(defn or' [x y]
  (when (and (bool? x) (bool? y))
    (or x y)))

;; TODO: make member? work with uneval'ed set ops and fns, including :set
;; TODO: eval args of uneval'ed ops and fns
#_(defn member?
  [x S]
  (if (op? S)
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

;; TODO: make it work with uneval'ed set ops and fns, including :set
;; TODO: eval args of uneval'ed ops and fns
#_(defn not-member?
  [x S]
  (let [v (member? x S)]
    (when (bool? v)
      (not v))))

(defn eval-for-bool [expr]
  ;; TODO: apply of sets and set-producing fns
  ;; TODO: apply of bool ops
  ;; member and equal will be difficult, because it isn't clear what eval type to use
  nil)

;; TODO <<<<<


(defn eval-for-char [expr]
  (if (char? expr)
    expr
    (let [context (get-context expr)]
      (when (op? expr)
        (let [f #(eval-for :int % context)
              [x n] expr]
          (cond
            (= 'intChar x) (if-let [n (f n)]
                             (when (< n 65536)
                               (char n)))
            (string? x) (if-let [n (f n)]
                          (when (and (>= n 0) (not= n '∞))
                            (try (nth x n)
                                 (catch java.lang.IndexOutOfBoundsException e))))
            :else nil))))))

(defn eval-for-tuple [n expr]
  (when (and (op-isa? :tup expr)
             (= n (count (rest expr))))
    expr))

(defn eval-for
  "Eval for a particular type. Use this function rather than calling the specific eval-for-<type> functions.
The type argument is a keyword describing the type, or an integer n for a tuple of arity n.
The context argument is only used if the expr doesn't have a :context metatag.
Returned expressions have a :context metatag if possible."
  ([type expr]
   (eval-for type expr nil))
  
  ([type expr context]
   (let [expr (eval' expr context)]
     (cond
       (op-isa? :alt expr)
       (some #(eval-for type % context) (rest expr))

       (op-isa? :values expr)
       (cons :values (map #(eval-for type % context) (rest expr)))

       (integer? type)
       (eval-for-tuple type expr)

       :else ((case type
                :seq eval-for-seq
                :cons eval-for-cons
                :empty eval-for-empty
                :set eval-for-set
                :num eval-for-num
                :int eval-for-int
                :bool eval-for-bool
                :char eval-for-char)
              expr)))))

(defn eval1-for
  "Like eval-for, but if multiple values are returned, use the first one that isn't nil."
  ([type expr]
   (eval1-for type expr nil))

  ([type expr context]
   (let [v (eval-for type expr context)]
     (if (op-isa? :values v)
       (some identity (rest v))
       v))))
