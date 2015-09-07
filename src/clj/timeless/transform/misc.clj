(ns timeless.transform.misc
  "Various transformations of Timeless S-expressions."
  (:require [timeless.common :refer :all]))

(defn transform-names
  "Convert (:name <name str>) to a symbol, make a gensym for an underscore, and tag the expression with the set of all names used within.
  More names will be generated during comprehension transformations, but those won't need to be visible outside of the comprehension."
  [expr]
  (condf expr
   (par op-isa? :name)
   (let [nam (symbol (second expr))]
     (tag-name nam))

   (par = '_)
   (new-name)                           ; also sets :all-names tag

   symbol?
   (tag-name expr)

   op?
   (set-all-names expr (collect-all-names expr))
        
   expr))

(defn transform-nested-applies
  [expr]
  (if (and (op? expr)
           (op? (first expr)))
    (let [[[opr & args1] & args2] expr]
      (if (keyword? opr)
        expr
        (apply make-op opr (concat args1 args2))))
    expr))

(defn transform-to-clause-unions
  [expr]
  (condf expr
   (par op-isa? :fn)
   (apply make-op '∪ (map (par cons :fn_) (rest expr)))

   (par op-isa? :set)
   (apply make-op '∪ (map (par cons :set_) (rest expr)))

   expr))

(defn transform-nested-ops
  [expr]
  (cond (and (op-isa? #{'/ '-} expr) ; left associative
             (op? (second expr)))
        (let [[opr1 [opr2 & args2] & args1] expr]
          (if (= opr1 opr2)
            (apply make-op opr1 (concat args2 args1))
            expr))

        (op-isa? (symbol ":") expr) ; right associative
        (let [[opr1 & args1] expr]
          (if (op-isa? (symbol ":") (last args1))
            (let [[opr2 & args2] (last args1)]
              (apply make-op opr1 (concat (butlast args1) args2)))
            expr))

        (op-isa? #{'* '+ '++ '∩ '∪ '∧ '∨} expr) ; associative
        (let [[opr & args] expr
              args (mapcat (fn [sub-expr]
                             (if (op-isa? opr sub-expr)
                               (rest sub-expr)
                               (list sub-expr)))
                           args)]
          (apply make-op opr args))

        :else expr))

(defn transform-chains
  [expr]
  (let [chain-oprs #{'= '≠ '< '> '≤ '≥}]
    (if (op-isa? chain-oprs expr)
      (let [[opr1 a1 b1] expr]
        (cond (op-isa? chain-oprs a1)
              (let [[opr2 a2 b2] a1]
                (if (op? b2)
                  (let [nam (new-name)]
                    (make-op '∧ (make-= nam b2) (make-op opr2 a2 nam) (make-op opr1 nam b1)))
                  (make-op '∧ a1 (make-op opr1 b2 b1))))

              (and (op-isa? '∧ a1) (op-isa? chain-oprs (last a1)))
              (let [[opr2 a2 b2] (last a1)]
                (if (op? b2)
                  (let [nam (new-name)]
                    (apply make-op (concat (butlast a1) (list (make-= nam b2)
                                                              (make-op opr2 a2 nam)
                                                              (make-op opr1 nam b1)))))
                  (apply make-op (concat a1 (list (make-op opr1 b2 b1))))))

              :else expr))
      expr)))

(defn misc-transforms
  [expr]
  (-> (if (op? expr)
        (map misc-transforms expr)
        expr)
      transform-names
      transform-nested-applies
      transform-to-clause-unions ; before transform-nested-ops, so nested unions will combine
      transform-nested-ops
      transform-chains))
