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

(defn build-associative-grammar-for-each-op [declaration]
  (let [pr (second declaration)]
    (when (not (#{0 1} pr))
      (case (first declaration)
        ("#op" "#opr")
        (str
         (format "<left-%d> = left-paren gt-%d op-%d right-paren\n" pr pr pr)
         (format "<right-%d> = left-paren op-%d gte-%d right-paren\n" pr pr pr)
         (format "operation-%d = gt-%d op-%d gte-%d\n" pr pr pr pr))

        "#opl"
        (str
         (format "<left-%d> = left-paren gte-%d op-%d right-paren\n" pr pr pr)
         (format "<right-%d> = left-paren op-%d gt-%d right-paren\n" pr pr pr)
         (format "operation-%d = gte-%d op-%d gt-%d\n" pr pr pr pr))))))


(defn build-grammar-for-each-op-but-last [[declaration next-declaration]]
  (let [pr (second declaration)
        next-pr (second next-declaration)]
    (str
     (format "<gte-%d> = exp | _gte-%d\n" pr pr)
     (format "<gt-%d> = exp | _gt-%d\n" pr pr)
     (format "<_gte-%d> = operation-%d | _gt-%d\n" pr pr pr)
     (format "<_gt-%d> = _gte-%d\n" pr next-pr))))

(defn build-grammar-for-last-op [declaration]
  (let [pr (second declaration)]
    (str
     (format "<gte-%d> = exp | _gte-%d\n" pr pr)
     (format "<gt-%d> = exp\n" pr)
     (format "<_gte-%d> = operation-%d" pr pr))))


(defn p [x] (println x) x)

;; TODO: check syntax of declarations, including that operators of the same precedence must have the same associativity, and that declaring a predefined name is an error.

(defn build-operator-grammar [declarations]
  (let [op-declarations (sort-by second
                                 (concat (build-op-declarations declarations)
                                         predefined-op-declarations))]
    (p (apply str
              (concat (map build-associative-grammar-for-each-op op-declarations)
                      (map build-grammar-for-each-op-but-last
                           (partition 2 1 op-declarations))
                      (list (build-grammar-for-last-op (last op-declarations))))))))
