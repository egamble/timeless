(ns timeless.common
  "Generally useful defs for the Timeless interpreter."
  (:require [clojure.set :as set]))

;;; ---------------------------------------------------------------------------
;;; miscellany
;;;
(defmacro condf
  "Variant of cond that takes an expr and several clauses, where the test in
  each clause is a pred to be applied to the expr value. A single default
  expression can follow the clauses, as in condp."
  [expr & clauses]
  `(let [expr# ~expr]
     (condp (fn [pred# _#] (pred# expr#)) nil
       ~@clauses)))

(defn third [s] (second (rest s)))

;; because some? is an awful name for this function
(def not-nil? some?)

(defn some-rest
  "Similar to some, but also returns the rest of the elements.
  When pred x is truthy for some x in s, returns [<pred x> <all of s other than x>].
  Otherwise just returns nil."
  [pred s]
  (loop [before []
         after s]
    (when (seq after)
      (let [[x & r] after]
        (if-let [v (pred x)]
          [v (concat before r)]
          (recur (conj before x)
                 r))))))
;;; ---------------------------------------------------------------------------

(def predefined
  #{'Obj 'Num 'Int 'Bool 'Char 'Str 'Set 'Seq 'Fn
    '* '/ '+ '- (symbol ":") '++ '∩ '∪ '= '≠ '< '> '≤ '≥ '∈ '∉ '⊂ '∧ '∨
    'Dom 'Img 'card 'charInt 'stdin
    'true 'false '∞})

(def op?
  "Is an expr an operation, rather than a name, an atomic constant, or nil (for the value of a :set clause)?"
  list?)

(defn op-isa?
  "Returns true if expr is a list and the first element is either equal to op-name or a member of op-name when op-name is a set."
  [op-name expr]
  (and (seq? expr)
       (if (set? op-name)
         (not-nil? (op-name (first expr)))
         (= op-name (first expr)))))

(defn set-all-names
  [expr name-set]
  (vary-meta expr assoc :all-names name-set))

(defn tag-name
  [nam]
  (set-all-names nam #{nam}))

(defn new-name
  []
  (let [nam (gensym "__")]
    (tag-name nam)))

(defn collect-all-names
  [exprs]
  (reduce #(set/union %1 (:all-names (meta %2)) )
          #{}
          exprs))

(defn make-op
  [op-name &rest exprs]
  (when (symbol? op-name) ; as opposed to a keyword such as :=
    (tag-name op-name))
  (let [op (apply list op-name exprs)
        names (collect-all-names op)]
    (set-all-names op)))

(defn make-=
  [a b]
  (make-op '= a b))
