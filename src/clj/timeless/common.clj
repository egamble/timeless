(ns timeless.common
  "Generally useful defs for the Timeless interpreter."
  (:require [clojure.set :as set]))

(def predefined-ops
  '#{* / + - :cons ++ ∩ ∪ → ⇸ = ≠ < > ≤ ≥ ∈ ∉ ⊂ :∈ ∧ ∨
     =. ≠. <. >. ≤. ≥. ∈. ∉. ⊂. :∈.})

(def predefined-fns
  '#{:neg Dm Im len charInt intChar})

(def predefined-sets
  '#{U Num Int Bool Char Str Set Seq Fn})

(def predefined
  (set/union
   predefined-ops
   predefined-fns
   predefined-sets
   '#{stdin ∞ :empty}))

(def op?
  "Is an expr an operation, rather than a name or an atomic constant?"
  seq?)

(declare not-nil?)

(defn op-isa?
  "Returns true if expr is a list and the first element is either equal to op-name-s or a member of op-name-s when op-name-s is a set."
  [op-name-s expr]
  (and (seq? expr)
       (let [head (first expr)]
         (if (set? op-name-s)
           (not-nil? (op-name-s head))
           (= op-name-s head)))))

(def name? symbol?)

(def taggable? (some-fn op? name?))

(defn get-context [expr]
  (when (taggable? expr)
    (:context (meta expr))))

(defn set-context [expr context]
  (if (taggable? expr)
    (vary-meta expr assoc :context context)
    expr))

(defn new-name [] (gensym "__"))

;;; miscellany

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

(defn error
  [msg]
  (throw (Exception. msg)))
