(ns timeless.tl.convert
  "Convert TL code to TLS code."
  (:require [clojure.string :as str]))


;;; Replace string literals with place holders, to avoid problems with tokenizing multi-line strings.

;; Return: [<extracted string> <replacement string literal>]
(defn make-string-literal-replacement [index s]
  (if (odd? index)
       ;; Put the same number of newlines, as in the original string literal,
       ;; after the replacement to keep the original line numbering.
    [s (str "\"" (/ (- index 1) 2) "\"" (str/replace s #"[^\n]" "")
            )]
       [nil s]))

(defn replace-string-literals [path source]
  (let [split-on-quote (str/split source #"^\"|(?<!\\)\"")
        n (count split-on-quote)]
    (cond (= 1 n)
          [nil source]
          
          (even? n)
          (throw (Exception. (str "File " path " has an unterminated string literal.")))

          :default
          (let [strings-and-replacements (map-indexed make-string-literal-replacement split-on-quote)
                strings (remove nil? (map first strings-and-replacements))
                source (str/join (map second strings-and-replacements))]
            [strings source]))))


;;; Extract declarations and get included files.

(def declare-tokens #{"#name" "#op" "#opa" "#opr" "#opl" "#pr<" "#pr=" "#pr>"})

(defn split-source-line [line]
  (remove str/blank?
          (str/split (str/trim line)
                 #"[ \t]")))

;; Return: [<annotated declare line> <annotated include line> <source line>]
(defn classify-line [path index line]
  (let [split-line (split-source-line line)
        annotated-split-line {:file path
                              :line-num (inc index)
                              :split-line split-line}
        first-token (first split-line)]
    (cond (declare-tokens first-token)
          [annotated-split-line nil ""]
 
          (= "#include" first-token)
          [nil annotated-split-line ""]

          :default
          [nil nil line])))

;; Return: [<path> <source>]
(defn get-include [include-line]
  (let [path (second (:split-line include-line))
        source (try
                 (slurp path)
                 (catch Exception e
                   (throw (Exception. (str "Line " (:line-num include-line)
                                           " of file " (:file include-line)
                                           ": #include " (.getMessage e))))))]
    [path source]))

;; Return: [<annotated declare lines> <annotated include lines> <source lines>]
(defn extract-declare-and-include-lines [path source]
  (let [lines (str/split-lines source)
        classified-lines (map-indexed (partial classify-line path)
                                      lines)]
    (map (partial remove nil?)
         (apply map list classified-lines) ; This works because str/split-lines will always
                                           ; generate at least one line, even if source is empty.
         )))

;; Return:
;; [<annotated declare lines>
;;  <list of {:file <path>
;;            :lines <source lines>
;;            :strings <string literals>}>]
(defn get-declare-lines-and-annotated-files [path source]
  (let [source (str (str/trimr source) "\n") ; ensure source ends with a newline,
                                        ; otherwise string literal replacement will fail
        [strings source] (replace-string-literals path source)
        [declare-lines include-lines source-lines] (extract-declare-and-include-lines path source)
        annotated-file {:file path :lines source-lines :strings strings}]
    (map (partial apply concat)
         (apply map list
                [declare-lines (list annotated-file)]
                (map #(apply get-declare-lines-and-annotated-files (get-include %))
                     include-lines)))))


;;; Tokenize.

(def name-declaring-tokens #{"#name" "#op" "#opa" "#opr" "#opl"})

(defn declared-names [declare-lines]
  (mapcat #(when (name-declaring-tokens (first (:split-line %)))
             (rest (:split-line %)))
          declare-lines))

(def predefined-op-names '("*" "/" "+" "-" ":" "++" "∩" "∪" "=" "≠" "<" ">" "≤" "≥" "⊂" "⊃" "∈" "∉" "|" ";" "→" "<>" "><" "<<<" "!=" "<=" ">=" "<<" ">>" "@" "!@"))

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

(defn make-tokenize-pattern [declare-lines]
  (let [fixed-tokens (map escape-for-regex
                          (-> (concat predefined-op-names
                                      other-tokens
                                      (declared-names declare-lines))
                              sort ; sort and then reverse allows, e.g., << to have precedence over <
                              reverse))]
    (re-pattern
     (str/join "|" (map #(str "^" %)
                        (concat fixed-tokens ; fixed-tokens must precede predefined-patterns so that, e.g.,
                                        ; a declared name foo* has precedence over foo
                                predefined-patterns))))))

(defn tokenize-line [pattern path index line]
  (if (str/blank? line)
    nil
    (let [token (re-find pattern line)]
      (if token
        (cons token (tokenize-line pattern path index
                                   (subs line (count token))))
        (throw (Exception. (str "Can't tokenize line " (+ index 1) " of file " path ".")))))))

(defn tokenize [[declare-lines annotated-files]]
  (let [pattern (make-tokenize-pattern declare-lines)
        annotated-files (map (fn [annotated-file]
                               (into annotated-file
                                     {:tokenized-lines
                                      (map-indexed (partial tokenize-line pattern (:file annotated-file))
                                                   (:lines annotated-file))}))
                             annotated-files)]
    [declare-lines annotated-files]))


;;; Convert from TL to TLS.

(defn tl->tls [in-path out-path]
  (let [source (slurp in-path)

        [declare-lines annotated-files]
        (-> (get-declare-lines-and-annotated-files in-path source)
            tokenize)

        tokenized-lines (mapcat :tokenized-lines annotated-files)]
    (spit out-path
          (str (str/join "\n" (map #(str/join " " %)
                                   tokenized-lines))
               "\n"))))

(defn -main [in-file out-file]
  (tl->tls in-file out-file))
