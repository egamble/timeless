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
    "true" "false"
    "null"
    "_"
    })

(defn build-op-declarations [declarations]
  (map (fn [[assoc precedence-str & names]]
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
       (filter #(#{"#op" "#opr" "#opl"} (first %))
               declarations)))

(defn interleave-with-bar [terminals]
  (->> (interleave terminals
                   (repeat " | "))
       butlast
       (apply str)))

(defn build-associative-grammar-for-each-op [assoc pr names] ; associativity, encoded precedence and op names

  ;; Force the associativity of comparison operators, i.e. those with precedence level 10, to be
  ;; right associative so they can form chains, despite being formally non-associative.
  ;; The associativity of comparison operators is only right associative for full operations, not sections.
  (let [assoc (if (= pr "10")
                "comparison"
                assoc)]
    (str
     (case assoc

       "comparison"
       (str
        (format "<left-10> = left-paren gt-10 op-10 right-paren\n")
        (format "<right-10> = left-paren op-10 gt-10 right-paren\n")
        (format "operation-10 = gt-10 op-10 gte-10\n"))

       "#op"
       (str
        (format "<left-%s> = left-paren gt-%s op-%s right-paren\n" pr pr pr)
        (format "<right-%s> = left-paren op-%s gt-%s right-paren\n" pr pr pr)
        (format "operation-%s = gt-%s op-%s gt-%s\n" pr pr pr pr))

       "#opr"
       (str
        (format "<left-%s> = left-paren gt-%s op-%s right-paren\n" pr pr pr)
        (format "<right-%s> = left-paren op-%s gte-%s right-paren\n" pr pr pr)
        (format "operation-%s = gt-%s op-%s gte-%s\n" pr pr pr pr))

       "#opl"
       (str
        (format "<left-%s> = left-paren gte-%s op-%s right-paren\n" pr pr pr)
        (format "<right-%s> = left-paren op-%s gt-%s right-paren\n" pr pr pr)
        (format "operation-%s = gte-%s op-%s gt-%s\n" pr pr pr pr)))

     (format "\nop-%s = ws (%s) ws\n" pr (interleave-with-bar
                                          (map (partial format "'%s'") names))))))

(def large-gap "\n\n\n")

(defn build-grammar-for-each-op-but-last [[[assoc pr & names] [_ next-pr & _]]]
  ;; pr is the encoded precedence of the current op declaration and
  ;; next-pr is the encoded precedence of the next op declaration
  (when (not (#{"0" "1"} pr))
    (str
     large-gap
     (build-associative-grammar-for-each-op assoc pr names)
     "\n"
     (format "<gte-%s> = exp | _gte-%s\n" pr pr)
     (format "<gt-%s> = exp | _gt-%s\n" pr pr)
     (format "<_gte-%s> = operation-%s | _gt-%s\n" pr pr pr)
     (format "<_gt-%s> = _gte-%s\n" pr next-pr))))

(defn build-grammar-for-last-op [[assoc pr & names]]
  ;; pr is the encoded precedence
  (str
   large-gap
   (build-associative-grammar-for-each-op assoc pr names)
   "\n"
   (format "<gte-%s> = exp | _gte-%s\n" pr pr)
   (format "<gt-%s> = exp\n" pr)
   (format "<_gte-%s> = operation-%s\n" pr pr)))


(defn build-complete-op-rules [op-declarations]
  (let [precedences (map second op-declarations)
        f (fn [prefix]
            (->> precedences
                 (map #(format "%s-%s" prefix %))
                 interleave-with-bar))
        complete-op-rules (str
                           large-gap
                           (format "right-section = %s\n" (f "right"))
                           "\n"
                           (format "left-section = %s\n" (f "left"))
                           "\n"
                           (format "<op> = %s\n" (f "op")))]
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


;; Convert a numeric precedence, either integer or float, to a string that satisfies
;; the syntax of instaparse terminals.
(defn encode-precedence [pr]
  (str/replace (str pr) \. \-))

(defn encode-declaration-precedence [[assoc pr & op-names]]
  `(~assoc ~(encode-precedence pr) ~@op-names))

(defn combine-and-sort-op-declarations [declarations]
  (->> declarations
       build-op-declarations
       (concat predefined-op-declarations)
       (group-by second) ; group by the numeric precedence
       vals
       (map reduce-op-declarations)
       (sort-by second) ; sort by the numeric precedence
       (map encode-declaration-precedence)))

(defn build-declared-name-rule [declarations op-declarations]
  (let [op-names (into #{} (mapcat (fn [[_ _ & r]] ; ignore assoc and precedence
                                     r)
                                   op-declarations))
        names (mapcat (fn [[_ & r]]
                        r)
                (filter #(= "#name" (first %))
                        declarations))
        names-including-predefined (cons "∞" names)]

    (doall
     (map (fn [name]
            (when (op-names name)
              (error (format "'%s' is declared as both name and operator" name))))
          names))

    (->> names-including-predefined
         (map (partial format "'%s'"))
         interleave-with-bar
         (format "<declared-name> = ws (%s) ws\n")
         (str large-gap))))

(defn build-operator-grammar [declarations]
  (let [op-declarations (combine-and-sort-op-declarations declarations)
        [complete-op-rules encoded-precedences] (build-complete-op-rules op-declarations)
        op-grammar (str
                    (format "<_gt-1> = _gte-%s\n" (third encoded-precedences)) ; skip precedences 0 and 1
                    (apply str (map build-grammar-for-each-op-but-last
                                    (partition 2 1 op-declarations)))
                    (build-grammar-for-last-op (last op-declarations))
                    complete-op-rules
                    (build-declared-name-rule declarations op-declarations))]
    [op-grammar encoded-precedences]))
