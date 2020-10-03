(ns timeless.tl.ops
  "Build an operator grammar from declarations."
  (:require [clojure.string :as str]))


(def predefined-op-declarations
  '(("#opl" 18 "*" "/")
    ("#opl" 17 "+" "-")
    ("#opr" 16 ";")
    ("#opr" 15 ":" "++")
    ("#opl" 12 "∩" "<>")
    ("#opl" 11 "∪" "><")
    ("#op" 10 "=" "≠" "<" ">" "≤" "≥" "⊂" "⊃" "∈" "∉" "!=" "<=" ">=" "<<" ">>" "@" "!@")
    ("#opl" 1 "|")
    ("#opr" 0 "->" "→")))

(defn build-op-declarations [declarations]
  (map (fn [declaration]
         (let [declaration (rest declaration)]
           (cons (first declaration)
                 (cons (read-string (second declaration))
                       (rest (rest declaration))))))
       (filter #(#{"#op" "#opr" "#opl"} (second %))
               declarations)))


;; TODO: check syntax of declarations, including that operators of the same precedence must have the same associativity, and that declaring a predefined name is an error.

(defn build-operator-grammar [declarations]
  (let [op-declarations (sort-by second
                                 (concat (build-op-declarations declarations)
                                         predefined-op-declarations))]
    ""))
