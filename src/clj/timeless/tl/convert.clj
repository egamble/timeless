(ns timeless.tl.convert
  "Convert TL code to TLS code."
  (:require [clojure.string :as str]))


;;; Annotate and classify lines and get #includes.

(def declare-tokens #{"#name" "#op" "#opa" "#opr" "#opl" "#pr<" "#pr=" "#pr>"})

(defn split-source-line [line]
  (remove str/blank?
          (str/split (str/trim line)
                 #"[ \t]")))

;; Returns [<annotated declare line> <annotated include line> <source line>].
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

;; Returns [<path> <source>].
(defn get-include [include-line]
  (let [path (second (:split-line include-line))
        source (try
                 (slurp path)
                 (catch Exception e
                   (throw (Exception. (str "Line " (:line-num include-line)
                                           " of file " (:file include-line)
                                           ": #include " (.getMessage e))))))]
    [path source]))

;; Returns [<annotated declare lines> <annotated include lines> <source lines>].
(defn extract-declare-and-include-lines [[path source]]
  (let [lines (str/split-lines source)
        classified-lines (map-indexed (partial classify-line path)
                                      lines)]
    (map (partial remove nil?)
         (apply map list classified-lines) ; This works because str/split-lines will always
                                           ; generate at least one line, even if source is empty.
         )))

;; Returns [<annotated declare lines> <list of {:file <path> :lines <source lines>}>].
(defn get-declare-lines-and-annotated-files [[path source]]
  (let [[declare-lines include-lines source-lines] (extract-declare-and-include-lines [path source])
        annotated-file {:file path :lines source-lines}]
    (map (partial apply concat)
         (apply map list
                [declare-lines (list annotated-file)]
                (map #(get-declare-lines-and-annotated-files (get-include %))
                     include-lines)))))


;;; Replace string literals with place holders, to avoid tokenizing problems with multi-line strings.



;;; Tokenize.

(def name-declaring-tokens #{"#name" "#op" "#opa" "#opr" "#opl"})

(def predefined-op-names #{"*" "/" "+" "-" ":" "++" "∩" "∪" "=" "≠" "<" ">" "≤" "≥" "⊂" "⊃" "∈" "∉" "|" ";" "→" "<>" "><" "!=" "<=" ">=" "<<" ">>" "@" "!@"})

(def other-tokens #{"," "~" "'" ".." "(" ")" "[" "]" "{" "}" "∞"})

(defn declared-names [declare-lines]
  (mapcat #(when (name-declaring-tokens (first (:split-line %)))
             (rest (:split-line %)))
          declare-lines))

(defn tokenize [{declare-lines :declare-lines
                 source-lines :source-lines}]
  (println (declared-names declare-lines)))


;;; Convert from TL to TLS.

(defn tl->tls [in-path out-path]
  (let [source (slurp in-path)
        [declare-lines annotated-files] (get-declare-lines-and-annotated-files [in-path source])
        all-lines (mapcat :lines annotated-files)]
    (spit out-path
          (str (str/join "\n" all-lines)
               "\n"))))

(defn -main [in-file out-file]
  (tl->tls in-file out-file))
