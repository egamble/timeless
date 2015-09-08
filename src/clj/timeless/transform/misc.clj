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
      (if (or (keyword? opr)
              ;; don't collapse unary -
              (and (= opr '-)
                   (not (second args1))))
        expr
        (apply make-op opr (concat args1 args2))))
    expr))

(defn transform-nested-ops
  [expr]
  (cond
    (and (op-isa-not-section? #{'/ '-} expr)        ; left associative
         (op-not-section? (second expr)))
    (let [[opr1 [opr2 & args2] & args1] expr]
      (if (= opr1 opr2)
        (apply make-op opr1 (concat args2 args1))
        expr))

    (op-isa-not-section? (symbol ":") expr)         ; right associative
    (let [[opr1 & args1] expr]
      (if (op-isa-not-section? (symbol ":") (last args1))
        (let [[opr2 & args2] (last args1)]
          (apply make-op opr1 (concat (butlast args1) args2)))
        expr))

    (op-isa-not-section? #{'* '+ '++ '∩ '∪ '∧ '∨} expr) ; associative
    (let [[opr & args] expr
          args (mapcat (fn [sub-expr]
                         (if (op-isa-not-section? opr sub-expr)
                           (rest sub-expr)
                           (list sub-expr)))
                       args)]
      (apply make-op opr args))

    :else expr))

(defn transform-chains
  [expr]
  (let [chain-oprs #{'= '≠ '< '> '≤ '≥}]
    (if (op-isa-not-section? chain-oprs expr)
      (let [[opr1 a1 b1] expr]
        (cond
          (op-isa-not-section? chain-oprs a1)
          (let [[opr2 a2 b2] a1]
            (if (op? b2)
              (let [nam (new-name)]
                (make-op '∧ (make-= nam b2) (make-op opr2 a2 nam) (make-op opr1 nam b1)))
              (make-op '∧ a1 (make-op opr1 b2 b1))))

          (and (op-isa? '∧ a1) (op-isa-not-section? chain-oprs (last a1)))
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
      transform-nested-ops
      transform-chains))
