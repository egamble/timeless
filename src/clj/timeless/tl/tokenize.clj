(ns timeless.tl.tokenize
  "Make annotated tokens from TL code."
  (:require [clojure.string :as str]))


;;; Get include files, extract declaration lines, and tokenize source.

(declare get-annotated-files
         tokenize-annotated-file
         extract-comments-remove-whitespace
         check-first-token
         restore-string-literals)

;; Returns: {:declarations <annotated declaration lines>
;;           :comments <annotated comment tokens>
;;           :tokens <annotated tokens>}
(defn tokenize [path source]
  (let [annotated-files (get-annotated-files path source)
        declaration-lines (mapcat :declarations annotated-files)
        pattern (make-tokenize-pattern declaration-lines)
        annotated-files (map (fn [annotated-file]
                               (->> annotated-file
                                    (tokenize-annotated-file pattern)
                                    extract-comments-remove-whitespace
                                    check-first-token
                                    restore-string-literals))
                             annotated-files)]
    {:declarations declaration-lines
     :comments (mapcat :comments annotated-files)
     :tokens   (mapcat :tokens   annotated-files)}))


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
;;  <list of {:file <path>
;;            :source <source with declaration lines removed and string literals replaced>
;;            :strings <string literals>
;;            :declarations <declaration lines>}>
(defn get-annotated-files [path source]
  (let [source (str (str/trimr source) "\n") ; ensure source ends with a newline,
                                        ; otherwise string literal replacement will fail
        [declaration-lines include-lines source-lines] (extract-declaration-and-include-lines path source)
        [strings source] (replace-string-literals path (str/join "\n" source-lines))
        annotated-file {:file path
                        :source source
                        :strings strings
                        :declarations declaration-lines}]
    (cons annotated-file
          (map #(apply get-annotated-files (get-include %))
               include-lines))))


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

;; Returns: the annotated file with annotated tokens added to it.
(defn tokenize-annotated-file [pattern annotated-file]
  (let [annotated-tokens
        (apply concat
               (map-indexed (partial tokenize-line pattern (:file annotated-file))
                            (str/split-lines (:source annotated-file))))]

    (into annotated-file
          {:tokens annotated-tokens})))


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

(defn restore-string-literals [annotated-file]
  (let [{annotated-tokens :tokens strings :strings} annotated-file]
    (into annotated-file
          {:tokens (map (partial restore-string-literal strings)
                        annotated-tokens)})))


;;; Extract comments and remove whitespace.

(defn comment? [annotated-token]
  (re-matches #"#.*" (:token annotated-token)))

(defn whitespace-or-comment? [annotated-token]
  (re-matches #"[ \t]*|#.*" (:token annotated-token)))

(defn extract-comments-remove-whitespace [annotated-file]
  (let [tokens (:tokens annotated-file)
        comments (filter comment? tokens)]
    (into annotated-file
          {:comments comments
           :tokens (remove whitespace-or-comment? tokens)})))


;;; Ensure that the first token of a file is the guard operator.

(defn check-first-token [annotated-file]
  (when (not (= "|" (:token (first (:tokens annotated-file)))))
    (throw (Exception. (str "In file " (:file annotated-file)
                            " the first token is not \"|\".")))))
