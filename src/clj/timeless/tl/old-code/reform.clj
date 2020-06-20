(ns timeless.tl.reform
  "Reform tokens to produce TLS S-expressions."
  (:require [timeless.tl.ops :refer [build-pr-matrix annotate-ops]]
            [clojure.string :as str]))


(defn third [s]
  (nth s 2))


;;;

(def special-tokens #{"," "~" "'" ".."})

(defn annotate-special-tokens [tokens]
  (map (fn [token]
         (if (:type token)
           token
           (let [val (:value token)]
             (into token
                   (when (special-tokens val)
                     {:type :special})))))
       tokens))


;;; Check that all brackets are balanced.

(def left-brackets #{"(" "{" "["})
(def right-brackets #{")" "}" "]"})

(defn check-stack-top [path token stack]
  (if (= (condp = (:value token)
           ")" "("
           "}" "{"
           "]" "[")
         (:value (first stack)))
    (rest stack)

    (let [{val :value
           line-num :line-num}
          (or (first stack)
              token)]
      (throw (Exception. (str "Unbalanced \"" val
                              "\" at line " line-num
                              " in file " path "."))))))

(defn check-next-token [path stack token]
  (if (= :string (:type token))
    stack
    (let [val (:value token)]
      (cond
        (left-brackets val) (cons token stack)
        (right-brackets val) (check-stack-top path token stack)
        :default stack))))

(defn check-balance [path tokens]
  (let [stack (reduce (partial check-next-token path) nil tokens)]
    (when (seq stack)
      (let [{val :value line-num :line-num} (first stack)]
        (throw (Exception. (str "Unbalanced \"" val
                                "\" at line " line-num
                                " in file " path "."))))))
  tokens)


;;; Build parentheses.

(defn build-parens-next-token [stack token]
  (let [new-head (cons token
                       (first stack))]
    (if (= :string (:type token))
      (cons new-head (rest stack))

      (let [val (:value token)]
        (cond
          (= token "(") (cons nil stack)
          (= token "{") (cons '(:set) stack)
          (= token "[") (cons '(:seq) stack)

          (right-brackets val)
          (let [new-head  (cons (reverse (first stack))
                                (second stack))
                new-tail (rest (rest stack))]
            (cons new-head new-tail))

          :default (cons new-head (rest stack)))))))

(defn build-parens [tokens]
  (-> (reduce build-parens-next-token '(()) tokens)
      first
      reverse))


;;; Wrap forms between operators.

(defn token-of-type? [type form val]
  (and (map? form)
       (= (:type form) type)
       (or (not val)
           (= (:value form) val))))

(defn op? [form & [val]]
  (token-of-type? :op form val))

(defn special? [form & [val]]
  (token-of-type? :special form val))

(defn op-or-special? [form]
  (or (op? form)
      (special? form)))

(defn unwrap-singleton [form]
  (if (and (seq? form)
           (not (second form))
           (not (op? (first form))) ; leave a wrapped singleton op, as it has a special meaning
           )
    (unwrap-singleton (first form))
    form))

(defn wrap-applications [forms]
  (let [wrapped-forms
        (reverse
         (reduce
          (fn [result form]
            (if (op-or-special? form)
              (cons form result)
              (if (or (empty? result)
                      (op-or-special? (first result)))
                (cons (list form) result)
                (cons (cons form (first result))
                      (rest result)))))
          '()
          forms))]
    (unwrap-singleton
     (map (fn [form]
            (if (op-or-special? form)
              form
              (reverse form)))
          wrapped-forms))))


;;; Wrap elements of tuples, sets and sequences.

(defn set-seq-or-comma? [form]
  (or (= :set form)
      (= :seq form)
      (special? form ",")))

;; (defn wrap-elements [forms]
;;   (let [wrapped-forms
;;         (reverse
;;          (reduce
;;           (fn [result form]
;;             (if (set-seq-or-comma? form)
;;               (cons form result)
;;               (if (or (empty? result)
;;                       (set-seq-or-comma? (first result)))
;;                 (cons (list form) result)
;;                 (cons (cons form (first result))
;;                       (rest result)))))
;;           '()
;;           forms))]
;;     (
;;      (map (fn [form]
;;             (if (op-or-special? form)
;;               form
;;               (reverse form)))
;;           wrapped-forms))))


;;;

(defn build-guard-recursive [form]
  (if (op? (first form) "|")
    (concat (build-guard-recursive (second form))
            (list (third form)))
    form))

(defn build-guard [form]
  (if (op? (first form) "|")
    (list :guard (build-guard-recursive form))
    form))


;;;

(defn convert-ops-to-prefix-fns [pr-matrix forms]
  forms)


;;;

(defn reform-deeper [pr-matrix form]
  (if (seq? form)
    (let [wrapped (wrap-applications form)]
      (if (seq? wrapped)
        (->> (map (partial reform-deeper pr-matrix)
                  wrapped)
             (convert-ops-to-prefix-fns pr-matrix)
             build-guard)
        wrapped))
    form))


;;;

(defn reform-top-level [pr-matrix path forms]
  (when (not (op? (first forms) "|"))
      (throw (Exception. (str "First token of file " path " is not \"|\"."))))

  ;; Easier to let reform-deeper catch various errors, rather than
  ;; call wrap-applications directly and look for errors
  ;; such as adjacent operator tokens, etc.
  (let [forms (reform-deeper pr-matrix (cons :dummy forms))
        first-form (first forms)]

    forms

    ;; TODO: Uncomment this after all other reform code is written.
    ;; (if (= first-form :guard)
    ;;   (rest (rest forms))
    ;;   (throw (Exception.
    ;;           (if (op? first-form)
    ;;             ;; Could be an operator with lower precedence than |.
    ;;             (str "Outermost operator must not be \"" (:value first-form)
    ;;                  "\" at line " (:line-num first-form)
    ;;                  " in file " path ".")

    ;;             ;; Shouldn't be possible, but just in case.
    ;;             (str "Outermost operator must be \"|\" in file " path ".")))))
    ))


;;; Remove annotations from tokens.
;;; Wrap names that aren't Clojure symbols, or that would be Clojure keywords.

(defn unannotate-tokens [form]
  (cond (seq? form)
        (map unannotate-tokens form)

        (map? form)
        (let [val (:value form)]
          (cond (= (:type form) :string)
                val

                (re-find #"(^\.)|[,;:\\/'~]" val)
                (list :name val)

                (= val "∞")
                (read-string "infinity")
                
                :default (read-string val)))

        :default form))


;;;

;; Returns: <assertions>
(defn reform [path declarations tokens]
  (let [pr-matrix (build-pr-matrix declarations)]
    (->> tokens
         annotate-special-tokens
         (check-balance path)
         (annotate-ops declarations)
         build-parens
         (reform-top-level pr-matrix path)
         unannotate-tokens)))




;;; Old code from extract.clj:

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


;;; Old code from ops.clj:

;;; Annotate operator tokens.

(def predefined-op-declarations
  '((:declare "#opa" "*" "+" "++" "∩" "∪" "<>" "><")
    (:declare "#op" "=" "≠" "<" ">" "≤" "≥" "⊂" "⊃" "∈" "∉" "!=" "<=" ">=" "<<" ">>" "@" "!@")
    (:declare "#opl" "/" "-" "|")
    (:declare "#opr" ":" "->" "→" ";")))

(defn make-template-ops-from-declaration [declaration]
  (let [assoc-keyword (case (second declaration)
                        "#op" :none
                        "#opr" :right
                        "#opl" :left
                        "#opa" :assoc
                        nil)]
    (when assoc-keyword
      (map #(do {:value %
                 :type :op
                 :assoc assoc-keyword})
           (rest (rest declaration))))))

;; Returns:
;; <list of
;;  {:value <op token value>
;;   :type :op
;;   :assoc :assoc|:left|:right|:none}>
(defn make-template-ops [declarations]
  (mapcat make-template-ops-from-declaration
          (concat predefined-op-declarations
                  declarations)))

(defn annotate-op-token [template-ops token]
  (if (= (:type token) :string)
    token
    (let [val (:value token)
          annotated-op (some #(when (= (:value %) val)
                                %)
                             template-ops)]
      (into token annotated-op))))

(defn annotate-ops [declarations tokens]
  (map (partial annotate-op-token
                (make-template-ops declarations))
       tokens))
