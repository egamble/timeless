(ns timeless.interpreter
  "Interpreter for an S-expression form of Timeless."
  (:require [timeless.common :refer :all]
            [timeless.transform
             [misc   :refer [transform-recursively]]
             [clause :refer [reorder-assertions-recursively]]]
            [clojure.set :as set]))

(defn read-top-level
  "Reads top-level assertions from stream, which defaults to *in*.
  Ignores assertions other than equality assertions, because this interpreter can't use them.
  Returns a context map."
  ([]
   (read-top-level *in*))
  ([stream]
   (let [asserts (->> (repeatedly #(read stream false nil))
                      (take-while not-nil?)
                      (filter (par op-isa? '=)) ;; keep only equality assertions
                      (map transform-recursively)
                      (doall))
         names (into #{} (map second asserts))
         asserts (map (par reorder-assertions-recursively
                           (set/union predefined names))
                      asserts)]
     (into {} (map (fn [[_ nam expr]]
                     [nam expr])
                   asserts)))))

(defn read-top-level-string
  "Same as read-top-level, except reads from a string."
  [s]
  (with-in-str s
    (read-top-level)))

(defn eval-fn
  [clause & args]
  (if (second args)
    (eval-fn
     (apply make-op
            (eval-fn (make-op clause (first args)))
            (rest args)))
    (let [[_ pattern v & asserts] clause]
      nil)))

(defn concat-seqs
  [& seqs]
  (if (some string? seqs)
    (if (every? string? seqs)
      (apply str seqs)
      (error "can only concat strings with other strings"))
    (if (every? (par op-isa? :seq) seqs)
      (apply make-op :seq (mapcat rest seqs))
      (error "can only concat sequences"))))

;; TODO refactor
(defn eval-apply
  [expr]
  (let [[opr & args] expr]
    (cond
      (op-isa? :fn opr)
      (eval-fn expr)

      (op-isa? '∪ opr)
      (some #(eval-apply (apply make-op % args))
            (rest opr))

      (op-isa? #{'+ '- '* '/ '++} opr)
      ;; must be a section
      (eval-apply (apply make-op (concat opr args)))

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
      (error "can't apply"))))

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
    (eval-apply (apply make-op (map #(eval-expr % context) expr)))

    name?
    (cond
      (context expr)
      (eval-expr (context expr) context)

      (predefined expr)
      expr

      :else (error "undefined name"))

    expr)))

(defn transform
  ([expr]
   (transform expr {}))
  ([expr context]
   (reorder-assertions-recursively (set/union predefined (keys context))
                                   (transform-recursively expr))))

(def t transform)

(defn transform-and-eval
  ([expr]
   (transform-and-eval expr {}))
  ([expr context]
   (eval-expr (transform expr context) context)))

(def e transform-and-eval)
