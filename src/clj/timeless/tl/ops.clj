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

(defn build-associative-grammar-for-each-op [[assoc pr & names]] ; associativity, numeric priority and op names
  (when (not (#{0 1} pr))
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
       
       (format "<op-%d> = %s\n"
               pr
               (->> (interleave op-terminals
                                 (repeat " | "))
                    butlast
                    (apply str)))

       (->> (map (fn [op-terminal name]
                         (format "%s = ws <'%s'> ws\n" op-terminal name))
                 op-terminals names)
            (apply str))))))


(defn build-grammar-for-each-op-but-last [[[_ pr & _] [_ next-pr & _]]]
  ;; pr is the numeric priority of the current op declaration and
  ;; next-pr is the priority of the next op declaration
  (str
   (format "<gte-%d> = exp | _gte-%d\n" pr pr)
   (format "<gt-%d> = exp | _gt-%d\n" pr pr)
   (format "<_gte-%d> = operation-%d | _gt-%d\n" pr pr pr)
   (format "<_gt-%d> = _gte-%d\n" pr next-pr)))

(defn build-grammar-for-last-op [[_ pr & _]]
  ;; pr is the numeric priority
  (str
   (format "<gte-%d> = exp | _gte-%d\n" pr pr)
   (format "<gt-%d> = exp\n" pr)
   (format "<_gte-%d> = operation-%d" pr pr)))


(defn p [x] (println x) x)


;; TODO
(defn combine-same-priorities [op-declarations] op-declarations)

;; TODO: check syntax of declarations, including that operators of the same precedence must have the same associativity, and that declaring a predefined name is an error.

(defn build-operator-grammar [declarations]
  (let [op-declarations (->> (concat (build-op-declarations declarations)
                                     predefined-op-declarations)
                             (sort-by second) ; sort by the numeric priority
                             combine-same-priorities)]
    (p (apply str
              (concat (map build-associative-grammar-for-each-op op-declarations)
                      (map build-grammar-for-each-op-but-last
                           (partition 2 1 op-declarations))
                      (list (build-grammar-for-last-op (last op-declarations))))))))
