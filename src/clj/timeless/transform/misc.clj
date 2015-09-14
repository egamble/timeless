(ns timeless.transform.misc
  "Various transformations of Timeless S-expressions."
  (:require [timeless.common :refer :all]
            [timeless.transform.clause :refer [transform-clauses]]))

(defn transform-names
  "Convert (:name <name str>) to a symbol, make a gensym for an underscore, and tag the expression with the set of all names within the expression except those in comprehensions."
  [expr]
  (condf expr
   (par op-isa? :name)
   (let [nam (symbol (second expr))]
     (set-maybe-free-names nam))

   (par = '_)
   (new-name)                           ; also sets :maybe-free-names tag

   (set-maybe-free-names expr)))

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

(defn transform-recursively
  [expr]
  (-> (if (op? expr)
        (apply make-op (map transform-recursively expr))
        expr)
      transform-names
      transform-nested-ops
      transform-chains
      transform-clauses))
