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

(def par partial)

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

(defn error
  [msg]
  (throw (Exception. msg)))

(def cons-op (symbol ":"))

(def predefined
  #{'Obj 'Num 'Int 'Bool 'Char 'Str 'Set 'Seq 'Fn
    '* '/ '+ '- cons-op '++ '∩ '∪ '= '≠ '< '> '≤ '≥ '∈ '∉ '⊂ '∧ '∨
    'Dom 'Img 'card 'charInt 'stdin
    'true 'false '∞})

(def op?
  "Is an expr an operation, rather than a name, an atomic constant, or nil (for an empty guard or the value of a :set_ clause)?"
  seq?)

(defn op-isa?
  "Returns true if expr is a list and the first element is either equal to op-name or a member of op-name when op-name is a set."
  [op-name expr]
  (and (seq? expr)
       (if (set? op-name)
         (not-nil? (op-name (first expr)))
         (= op-name (first expr)))))

(defn op-not-section?
  [expr]
  (and (op? expr)
       (second (rest expr))))

(defn op-isa-not-section?
  [op-name expr]
  (and (op-isa? op-name expr)
       (second (rest expr))))

(def name? symbol?)

(defn taggable?
  [expr]
  (or (name? expr) (op? expr)))

(defn get-maybe-free-names
  "Collect all names except those in contained comprehensions."
  [expr]
  (condf expr
   op? 
   (reduce (fn [name-set sub-expr]
             (if (op-isa? #{:fn :set} sub-expr)
               name-set
               (set/union name-set (get-maybe-free-names sub-expr))) )
           #{}
           expr)

   name? #{expr}

   nil))

(defn new-name
  []
  (gensym "__"))

(defn make-op
  [opr & exprs]
  (apply list opr exprs))

(defn make-=
  [a b]
  (make-op '= a b))
