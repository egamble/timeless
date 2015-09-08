(ns timeless.interpreter
  "Interpreter for an S-expression form of Timeless."
  (:require [timeless.common :refer :all]
            [timeless.transform
             [misc   :refer [misc-transforms]]
             [clause :refer [transform-clauses]]]
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
                      (map misc-transforms)
                      (doall))
         bound-names (into #{} (map second asserts))
         bound-names (set/union predefined bound-names)
         asserts (map (par transform-clauses bound-names) asserts)]
     (into {} (map (fn [[_ nam expr]]
                     [nam (if (taggable? expr)
                            ;; Tag expr with the bound names, so at evaluation time it will be clear
                            ;; which names are free in the original context, rather than the evaluation context.
                            (vary-meta expr assoc :bound-names bound-names)
                            expr)])
                   asserts)))))

(defn read-top-level-string
  "Same as read-top-level, except reads from a string."
  [s]
  (with-in-str s
    (read-top-level)))

(defn eval-fn
  [expr]
  nil)

(defn eval-apply
  [expr context]
  (let [[opr & args] expr]
    (if (op-isa? #{:fn '∪} opr)
      (eval-fn expr)
      (condp = opr
        '+ (apply + args)
        (error "can't apply")))))

(defn eval-expr
  ([expr]
   (eval-expr expr {}))
  ([expr context]
   (condf expr
          (par op-isa? #{:fn :set})
          expr

          (par op-isa? #{:seq :tup})
          (apply make-op (first expr)
                 (map #(eval-expr % context) (rest expr)))

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

(defn transform-and-eval
  ([expr]
   (transform-and-eval expr {}))
  ([expr context]
   (eval-expr (transform-clauses (set/union predefined (keys context))
                                 (misc-transforms expr))
              context)))

(def e transform-and-eval)

