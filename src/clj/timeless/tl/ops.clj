(ns timeless.tl.ops
  "Build an operator grammar from declarations."
  (:require [clojure.string :as str]))


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

(defn third [s] (nth s 2))

(defn build-op-declarations [declarations]
  (map (fn [[_ assoc priority-str & names]]
         `(~assoc ; associativity: "#op" or "#opr" or "#opl"
           ~(read-string priority-str) ; numeric priority
           ~@names ; names of the operators
           ))
       (filter #(#{"#op" "#opr" "#opl"} (second %))
               declarations)))

(defn stringify-utf8 [s]
  (->> (.getBytes s "UTF-8")
       (map (partial format "%02x"))
       (apply str)))

(defn make-op-terminal [name]
  (str (if (re-matches #"[a-zA-Z]\w*" name)
         name
         (str "_" (stringify-utf8 name)))
       "-op"))

(defn interleave-with-bar [terminals]
  (->> (interleave terminals
                   (repeat " | "))
       butlast
       (apply str)))

(defn build-associative-grammar-for-each-op [assoc pr names] ; associativity, numeric priority and op names
  (let [op-terminals (map make-op-terminal names)]
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
     
     (format "\n<op-%d> = %s\n" pr (interleave-with-bar op-terminals))
     "\n"
     (->> (map (fn [op-terminal name]
                 (format "%s = ws <'%s'> ws\n" op-terminal name))
               op-terminals names)
          (apply str)))))


(def large-gap "\n\n\n")

(defn build-grammar-for-each-op-but-last [[[assoc pr & names] [_ next-pr & _]]]
  ;; pr is the numeric priority of the current op declaration and
  ;; next-pr is the priority of the next op declaration
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
  ;; pr is the numeric priority
  (str
   large-gap
   (build-associative-grammar-for-each-op assoc pr names)
   "\n"
   (format "<gte-%d> = exp | _gte-%d\n" pr pr)
   (format "<gt-%d> = exp\n" pr)
   (format "<_gte-%d> = operation-%d\n" pr pr)))


(defn build-complete-op-rules [op-declarations]
  (let [priorities (map second op-declarations)
        f (fn [prefix]
            (->> priorities
                 (map #(format "%s-%d" prefix %))
                 interleave-with-bar))]
    (str
     large-gap
     (format "<right-section> = %s" (f "right"))
     large-gap
     (format "<left-section> = %s" (f "left"))
     large-gap
     (format "<op> = %s" (f "op")))))


;; Assumes all the given op-declarations have the same priority.
(defn reduce-op-declarations [op-declarations]
  (let [pr (second (first op-declarations))
        assocs (map first op-declarations)]
    ;; Check all the given op-declarations have the same associativity.
    (when (> (count (set assocs)) 1)
      (throw (Exception. (str "op declarations with priority " pr
                              " have multiple associativities " (pr-str assocs)))))
    (let [all-names (mapcat (fn [[_ _ & names]]
                              names)
                            op-declarations)]
      `(~(first assocs) ~pr ~@all-names))))


(defn combine-and-sort-op-declarations [declarations]
  (->> declarations
       build-op-declarations
       (concat predefined-op-declarations)
       (group-by second) ; group by the numeric priority
       vals
       (map reduce-op-declarations)
       (sort-by second) ; sort by the numeric priority
       ))

;; TODO: check syntax of declarations, including that declaring a predefined name is an error.

(defn build-operator-grammar [declarations]
  (let [op-declarations (combine-and-sort-op-declarations declarations)]
    (str 
     (apply str (map build-grammar-for-each-op-but-last
                     (partition 2 1 op-declarations)))
     (build-grammar-for-last-op (last op-declarations))
     (build-complete-op-rules op-declarations))))
