(ns timeless.eval
  "Eval the S-expression form of Timeless expressions."
  (:require [timeless.common :refer :all]))

(declare eval-expr)

(defn eval-asserts
  [v asserts context]
  (if (seq asserts)
    
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
    (if (every? string? seqs)
      (apply str seqs)
      (error "can only concat strings with other strings"))
    (if (every? (par op-isa? :seq) seqs)
      (apply make-op :seq (mapcat rest seqs))
      (error "can only concat sequences"))))

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
