(ns timeless.tl.tokenize
  "Tokenize TL code."
  (:require [clojure.string :as str]))


;;; Make tokenization pattern.

(def name-declaring-tokens #{"#name" "#op" "#opa" "#opr" "#opl"})

(defn declared-names [declarations]
  (mapcat #(when (name-declaring-tokens (second %))
             (rest (rest %)))
          declarations))

(def predefined-op-names '("*" "/" "+" "-" ":" "++" "∩" "∪" "=" "≠" "<" ">" "≤" "≥" "⊂" "⊃" "∈" "∉" "|" ";" "->" "→" "<>" "><" "!=" "<=" ">=" "<<" ">>" "@" "!@"))

(def other-tokens '("," "~" "'" ".." "(" ")" "[" "]" "{" "}" "∞"))

(def predefined-patterns
  (list "\\\"\\d+\\\"" ; string literal
        "[a-zA-Z_]\\w*" ; regular name
        "\\d+(?:\\.\\d+)?" ; number literal
        "\\s+" ; whitespace
        "#.*" ; comment
   ))

(defn escape-for-regex [s]
  (str/replace s #"[^\w]" #(str "\\" %)))

(defn make-tokenize-pattern [declarations]
  (let [fixed-tokens (map escape-for-regex
                          (-> (concat predefined-op-names
                                      other-tokens
                                      (declared-names declarations))
                              sort ; sort and then reverse allows, e.g., << to have precedence over <
                              reverse))]
    (re-pattern
     (str/join "|" (map #(str "^" %)
                        (concat fixed-tokens ; fixed-tokens must precede predefined-patterns so that, e.g.,
                                        ; a declared name foo* has precedence over foo
                                predefined-patterns))))))


;;; Initial "raw" tokenization.

(defn tokenize-line [pattern path index line]
  (if (str/blank? line)
    nil
    (let [line-num (+ index 1)
          token (re-find pattern line)]
      (if token
        (let [n (count token)]
          (cons {:token token
                 :line-num line-num}
                (tokenize-line pattern path index (subs line n))))
        (throw (Exception. (str "Can't tokenize line " line-num " of file " path ".")))))))

;; Returns: <annotated tokens>
(defn tokenize-raw [pattern path source]
  (apply concat
         (map-indexed (partial tokenize-line pattern path)
                      (str/split-lines source))))


;;; Restore string literals.

(defn restore-string-literal [strings annotated-token]
  (let [token (:token annotated-token)
        new-token (when (= (first token) \")
                    (let [index (read-string
                                 (read-string token))]
                      (nth strings index)))]
    (into annotated-token
          (when new-token
            {:token new-token
             :type :string}))))

(defn restore-string-literals [strings annotated-tokens]
 (map (partial restore-string-literal strings)
      annotated-tokens))


;;; Tokenize, restore string literals, and remove whitespace and comments.

(defn comment? [annotated-token]
  (re-matches #"#.*" (:token annotated-token)))

(defn whitespace-or-comment? [annotated-token]
  (re-matches #"[ \t]*|#.*" (:token annotated-token)))

;; Returns: <annotated tokens>
(defn tokenize [declarations path source strings]
  (let [pattern (make-tokenize-pattern declarations)
        annotated-tokens (->> (tokenize-raw pattern path source)
                              (restore-string-literals strings)
                              (remove whitespace-or-comment?))]
    annotated-tokens))
