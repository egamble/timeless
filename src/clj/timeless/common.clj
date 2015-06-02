(ns timeless.common
  "Generally useful defs for the Timeless interpreter.")


;;; condf: the missing cond macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro condf
  "Variant of cond that takes an expr and several clauses, where the test in
  each clause is a pred to be applied to the expr value. A single default
  expression can follow the clauses, as in condp."
  [expr & clauses]
  `(let [expr# ~expr]
     (condp (fn [pred# _#] (pred# expr#)) nil
       ~@clauses)))


;;; map fns
;;;;;;;;;;;

(defn update-vals
  "\"Update\" all values in m by calling f on every value to get the new value."
  [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn remove-vals
  "Returns a map excluding entries in `m` for which `(pred v)` is logical false"
  [pred m]
  (into {} (remove (fn [[_ v]] (pred v)) m)))


;;; miscellany
;;;;;;;;;;;;;;

;; because some? is an awful name for this function
(def not-nil? some?)

(defn third [s] (second (rest s)))


;;; checking exprs
;;;;;;;;;;;;;;;;;;

(defn op-isa?
  "Returns true if expr is a list and the first element is either equal to op or a member of op when op is a set."
  [op expr]
  (and (seq? expr)
       (if (set? op)
         (not-nil? (op (first expr)))
         (= op (first expr)))))

;; TODO: add all ops, ∞, and $<symbol>s; alternate characters also
(def predefined
  #{'Obj 'Num 'Int 'Bool 'Char 'Str 'Set 'Seq 'Fn 'Dom 'Img 'len 'charToInt 'stdin})

(defn constant?
  "Determines whether expr is constant w.r.t. context."
  [expr context]
  (condf expr
         list? (every? constant? expr)
         ;; the not-nil? is not strictly necessary, just coerces to boolean
         symbol? (not-nil? (or (predefined expr)
                               (context expr)))
         ;; nums, strs, chars, bools
         :else true))
