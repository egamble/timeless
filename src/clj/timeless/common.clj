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


;;; miscellany
;;;;;;;;;;;;;;

;; because some? is an awful name for this function
(def not-nil? some?)

(defn third [s] (second (rest s)))


;;; checking exprs
;;;;;;;;;;;;;;;;;;

(def op?
  "Is an expr an operation, rather than a name, an atomic constant, or nil (for the value of a :_set)?"
  list?)

(defn op-isa?
  "Returns true if expr is a list and the first element is either equal to op or a member of op when op is a set."
  [op expr]
  (and (seq? expr)
       (if (set? op)
         (not-nil? (op (first expr)))
         (= op (first expr)))))

;; TODO: add all ops, ∞; alternate characters also; don't need keywords
(def predefined
  #{'Obj 'Num 'Int 'Bool 'Char 'Str 'Set 'Seq 'Fn 'Dom 'Img 'len 'charToInt 'stdin})

(defn free-names
  "Return the names in expr that are not in context and are not predefined.
  The context can be a set or map."
  [expr context]
  (condf expr
         op? (mapcat free-names expr)
         symbol? (when-not (or (predefined expr)
                               (context expr))
                   (list expr))
         ;; nums, strs, chars, bools, keywords
         :else nil))

(defn free-names-set
  "Like free-names, but returns a set."
  [expr context]
  (set (free-names expr context)))
