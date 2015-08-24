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


;;; checking exprs
;;;;;;;;;;;;;;;;;;

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

;; TODO: add all predefined operators, ∞; don't need keywords
(def predefined
  #{'Obj 'Num 'Int 'Bool 'Char 'Str 'Set 'Seq 'Fn 'Dom 'Img 'card 'charInt 'stdin})

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

(defn set-of-free-names
  "Like free-names, but returns a set."
  [expr context]
  (set (free-names expr context)))

(defn new-name
  []
  (gensym "__"))
