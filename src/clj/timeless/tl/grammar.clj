(ns timeless.tl.grammar
  "Build a grammar from declarations."
  (:require [timeless.tl.utils :refer :all]
            [clojure.string :as str]))


(def predefined-op-declarations
  '(("#opr" 0 "->" "→")
    ("#opl" 1 "|")
    ("#op" 10 "=" "≠" "<" ">" "≤" "≥" "⊂" "⊃" "∈" "∉" "!=" "<=" ">=" "<<" ">>" "@" "!@")
    ("#opl" 11 "∪" "><")
    ("#opl" 12 "∩" "<>")
    ("#opr" 15 ":" "++")
    ("#opr" 16 ";")
    ("#opl" 17 "+" "-")
    ("#opl" 18 "*" "/")))

(def predefined-names
  #{"->" "→"
    "|"
    "=" "≠" "<" ">" "≤" "≥" "⊂" "⊃" "∈" "∉" "!=" "<=" ">=" "<<" ">>" "@" "!@"
    "∪" "><"
    "∩" "<>"
    ":" "++"
    ";"
    "+" "-"
    "*" "/"
    "Any" "Num" "Int" "Bool" "Sym" "Tag" "Arr" "Set" "Fn" "Seq" "Str" "Char"
    "Dm" "Im"
    "size" "infinity" "∞"
    "_"
    })

(defn build-op-declarations [declarations]
  (map (fn [[_ assoc precedence-str & names]]
         (let [pr (read-string precedence-str)
               op-declaration `(~assoc ; associativity: "#op" or "#opr" or "#opl"
                                ~pr ; numeric precedence
                                ~@names ; names of the operators
                                )
               redefined-name (some predefined-names names)]

           (when redefined-name
             (error (str "can't redefine the predefined name " (pr-str redefined-name))))

           (when (or (<= pr 1))
             (error (str "op declaration must have precedence > 1: "
                         (pr-str op-declaration))))
           op-declaration))
       (filter #(#{"#op" "#opr" "#opl"} (second %))
               declarations)))

(defn interleave-with-bar [terminals]
  (->> (interleave terminals
                   (repeat " | "))
       butlast
       (apply str)))

(defn build-associative-grammar-for-each-op [assoc pr names] ; associativity, numeric precedence and op names
  (str
   (case assoc
     ("#op" "#opr")
     (str
      (format "<left-%d> = left-paren gt-%d op-%d right-paren\n" pr pr pr)
      (format "<right-%d> = left-paren op-%d gte-%d right-paren\n" pr pr pr)
      (format "operation-%d = gt-%d op-%d gte-%d\n" pr pr pr pr))

     "#opl"
     (str
      (format "<left-%d> = left-paren gte-%d op-%d right-paren\n" pr pr pr)
      (format "<right-%d> = left-paren op-%d gt-%d right-paren\n" pr pr pr)
      (format "operation-%d = gte-%d op-%d gt-%d\n" pr pr pr pr)))
   
   (format "\nop-%d = ws (%s) ws\n" pr (interleave-with-bar
                                        (map (partial format "'%s'") names)))))


(def large-gap "\n\n\n")

(defn build-grammar-for-each-op-but-last [[[assoc pr & names] [_ next-pr & _]]]
  ;; pr is the numeric precedence of the current op declaration and
  ;; next-pr is the precedence of the next op declaration
  (when (not (#{0 1} pr))
    (str
     large-gap
     (build-associative-grammar-for-each-op assoc pr names)
     "\n"
     (format "<gte-%d> = exp | _gte-%d\n" pr pr)
     (format "<gt-%d> = exp | _gt-%d\n" pr pr)
     (format "<_gte-%d> = operation-%d | _gt-%d\n" pr pr pr)
     (format "<_gt-%d> = _gte-%d\n" pr next-pr))))

(defn build-grammar-for-last-op [[assoc pr & names]]
  ;; pr is the numeric precedence
  (str
   large-gap
   (build-associative-grammar-for-each-op assoc pr names)
   "\n"
   (format "<gte-%d> = exp | _gte-%d\n" pr pr)
   (format "<gt-%d> = exp\n" pr)
   (format "<_gte-%d> = operation-%d\n" pr pr)))


(defn build-complete-op-rules [op-declarations]
  (let [precedences (map second op-declarations)
        f (fn [prefix]
            (->> precedences
                 (map #(format "%s-%d" prefix %))
                 interleave-with-bar))
        complete-op-rules (str
                           large-gap
                           (format "right-section = %s" (f "right"))
                           large-gap
                           (format "left-section = %s" (f "left"))
                           large-gap
                           (format "<op> = %s" (f "op")))]
    [complete-op-rules precedences]))


;; Assumes all the given op-declarations have the same precedence.
(defn reduce-op-declarations [op-declarations]
  (let [pr (second (first op-declarations))
        assocs (map first op-declarations)]
    ;; Check all the given op-declarations have the same associativity.
    (when (> (count (set assocs)) 1)
      (error (str "op declarations with precedence " pr
                  " have multiple associativities " (pr-str assocs))))
    (let [all-names (mapcat (fn [[_ _ & names]]
                              names)
                            op-declarations)]
      `(~(first assocs) ~pr ~@all-names))))


(defn combine-and-sort-op-declarations [declarations]
  (->> declarations
       build-op-declarations
       (concat predefined-op-declarations)
       (group-by second) ; group by the numeric precedence
       vals
       (map reduce-op-declarations)
       (sort-by second) ; sort by the numeric precedence
       ))

;; TODO: check for collisions with op names
(defn build-name-rule [declarations]
  nil)

(defn build-operator-grammar [declarations]
  (let [op-declarations (combine-and-sort-op-declarations declarations)
        [complete-op-rules precedences] (build-complete-op-rules op-declarations)
        op-grammar (str 
                    (apply str (map build-grammar-for-each-op-but-last
                                    (partition 2 1 op-declarations)))
                    (build-grammar-for-last-op (last op-declarations))
                    complete-op-rules
                    (build-declared-name-rule declarations))]
    [op-grammar precedences]))
