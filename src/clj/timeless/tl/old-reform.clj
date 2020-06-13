(ns timeless.tl.reform
  "Parse tokens to produce TLS S-expressions."
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
(println "foo" forms)  (when (not (op? (first forms) "|"))
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
