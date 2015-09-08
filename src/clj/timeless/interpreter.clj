(ns timeless.interpreter
  "Interpreter for an S-expression form of Timeless."
  (:require [timeless.common :refer :all]
            [timeless.transform
             [misc   :refer [misc-transforms]]
             [clause :refer [transform-clause]]]))

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
                      (doall))]
     (into {} (map rest asserts)))))

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
          (or (:transformed (meta expr))
              (let [clause (transform-clause expr context)]
                (do (vary-meta expr assoc :transformed clause)
                    clause)))

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
   (eval-expr (misc-transforms expr) context)))

(def e eval-expr)
(def t misc-transforms)
(def te transform-and-eval)

