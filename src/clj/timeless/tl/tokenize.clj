(ns timeless.tl.tokenize
  "Make annotated tokens from TL code."
  (:require [clojure.string :as str]))


;;; Get include files, extract declaration lines, and tokenize source.

(declare get-declaration-lines-and-annotated-files
         whitespace-or-comment?
         restore-string-literals
         tokenize-annotated-files)

;; Returns: [<annotated declaration lines> <annotated tokens>]
(defn tokenize [path source]
  (let [[declaration-lines annotated-files] (get-declaration-lines-and-annotated-files path source)
        annotated-tokens (->> (mapcat restore-string-literals
                                      (tokenize-annotated-files declaration-lines annotated-files))
                              (remove whitespace-or-comment?))]
    [declaration-lines annotated-tokens]))


;;; Replace string literals with place holders, to avoid problems with tokenizing multi-line strings.

;; Returns: [<extracted string> <replacement string literal>]
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

;; Returns: [<annotated declare line> <annotated include line> <source line>]
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

;; Returns: [<path> <source>]
(defn get-include [include-line]
  (let [path (second (:split-line include-line))
        source (try
                 (slurp path)
                 (catch Exception e
                   (throw (Exception. (str "Line " (:line-num include-line)
                                           " of file " (:file include-line)
                                           ": #include " (.getMessage e))))))]
    [path source]))

;; Returns: [<annotated declare lines> <annotated include lines> <source lines>]
(defn extract-declaration-and-include-lines [path source]
  (let [lines (str/split-lines source)
        classified-lines (map-indexed (partial classify-line path)
                                      lines)]
    (map (partial remove nil?)
         (apply map list classified-lines) ; This works because str/split-lines will always
                                           ; generate at least one line, even if source is empty.
         )))

;; Returns:
;; [<annotated declare lines>
;;  <list of {:file <path>
;;            :source <source with declaration lines removed and string literals replaced>
;;            :strings <string literals>}>]
(defn get-declaration-lines-and-annotated-files [path source]
  (let [source (str (str/trimr source) "\n") ; ensure source ends with a newline,
                                        ; otherwise string literal replacement will fail
        [declaration-lines include-lines source-lines] (extract-declaration-and-include-lines path source)
        [strings source] (replace-string-literals path (str/join "\n" source-lines))
        annotated-file {:file path
                        :source source
                        :strings strings}]
    (map (partial apply concat)
         (apply map list
                [declaration-lines (list annotated-file)]
                (map #(apply get-declaration-lines-and-annotated-files (get-include %))
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
    (let [line-num (+ index 1)
          token (re-find pattern line)]
      (if token
        (let [n (count token)]
          (cons {:token token
                 :line-num line-num
                 :file path}
                (tokenize-line pattern path index (subs line n))))
        (throw (Exception. (str "Can't tokenize line " line-num " of file " path ".")))))))

;; Returns: a list of annotated tokens from all the annotated files.
(defn tokenize-annotated-files [declaration-lines annotated-files]
  (let [pattern (make-tokenize-pattern declaration-lines)]
    (map (fn [annotated-file]
           (into annotated-file
                 {:tokens
                  (apply concat
                   (map-indexed (partial tokenize-line pattern (:file annotated-file))
                                (str/split-lines (:source annotated-file))))}))
         annotated-files)))


;;; Restore string literals

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

(defn restore-string-literals [{annotated-tokens :tokens strings :strings}]
  (map (partial restore-string-literal strings)
       annotated-tokens))


;;; Remove whitespace and comments.

(defn whitespace-or-comment? [annotated-token]
  (re-matches #"[ \t]*|#.*" (:token annotated-token)))
