(ns timeless.tl.extract
  "Extract declarations from TL source."
  (:require [clojure.string :as str]))


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

;; Returns: [<declaration> <annotated includes> <source line>]
(defn classify-line [path index line]
  (let [tokens (split-source-line line)
        first-token (first tokens)]
    (cond (declare-tokens first-token)
          [(apply list :declare tokens)
           nil
           ""]
 
          (= "#include" first-token)
          [nil
           {:line-num (inc index)
            :paths (rest tokens)}
           ""]

          :default
          [nil nil line])))

;; Returns:
;; [<declarations>
;;  <annotated includes>
;;  <source lines>]
(defn split-and-classify-lines [path source]
  (let [lines (str/split-lines source)
        classified-lines (map-indexed (partial classify-line path)
                                      lines)]
    (map (partial remove nil?)
         (apply map list classified-lines) ; This works because str/split-lines will always
                                           ; generate at least one line, even if source is empty.
         )))

;; Returns:
;; [<declarations>
;;  <annotated includes in the form {:line-num <> :paths <>}>
;;  <source with declaration lines removed and string literals replaced>
;;  <strings>]
(defn extract [path source]
  (let [source (str (str/trimr source) "\n") ; ensure source ends with a newline,
                                        ; otherwise string literal replacement will fail
        [declarations
         annotated-includes
         source-lines]
        (split-and-classify-lines path source)

        [strings source] (replace-string-literals path (str/join "\n" source-lines))]
    [declarations
     annotated-includes
     source
     strings]))
