(ns timeless.tl.reform
  "Reform tokens into TLS S-expressions."
  (:require [timeless.tl.ops :refer [build-pr-matrix annotate-ops]]
            [clojure.string :as str]))


(defn third [s]
  (nth s 2))


;;;

(def special-tokens #{"," "~" "'" ".."})

(defn annotate-special-tokens [annotated-tokens]
  (map (fn [annotated-token]
         (let [token (:token annotated-token)]
           (into annotated-token
                 (when (special-tokens token)
                   {:type :special}))))
       annotated-tokens))


;;; Check that all brackets are balanced.

(def left-brackets #{"(" "{" "["})
(def right-brackets #{")" "}" "]"})

(defn check-stack-top [path annotated-token stack]
  (if (= (condp = (:token annotated-token)
           ")" "("
           "}" "{"
           "]" "[")
         (:token (first stack)))
    (rest stack)

    (let [{token :token
           line-num :line-num}
          (or (first stack)
              annotated-token)]
      (throw (Exception. (str "Unbalanced \"" token
                              "\" at line " line-num
                              " in file " path "."))))))

(defn check-next-token [path stack annotated-token]
  (if (= :string (:type annotated-token))
    stack
    (let [token (:token annotated-token)]
      (cond
        (left-brackets token) (cons annotated-token stack)
        (right-brackets token) (check-stack-top path annotated-token stack)
        :default stack))))

(defn check-balance [path annotated-tokens]
  (let [stack (reduce (partial check-next-token path) nil annotated-tokens)]
    (when (seq stack)
      (let [{token :token line-num :line-num} (first stack)]
        (throw (Exception. (str "Unbalanced \"" token
                                "\" at line " line-num
                                " in file " path "."))))))
  annotated-tokens)


;;; Build parentheses.

(defn build-parens-next-token [stack annotated-token]
  (let [new-head (cons annotated-token
                       (first stack))]
    (if (= :string (:type annotated-token))
      (cons new-head (rest stack))

      (let [token (:token annotated-token)]
        (cond
          (= token "(") (cons nil stack)
          (= token "{") (cons '(:set) stack)
          (= token "[") (cons '(:seq) stack)

          (right-brackets token)
          (let [new-head  (cons (reverse (first stack))
                                (second stack))
                new-tail (rest (rest stack))]
            (cons new-head new-tail))

          :default (cons new-head (rest stack)))))))

(defn build-parens [annotated-tokens]
  (-> (reduce build-parens-next-token '(()) annotated-tokens)
      first
      reverse))


;;; Wrap forms between operators.

(defn token-of-type? [type form token]
  (and (map? form)
       (= (:type form) type)
       (or (not token)
           (= (:token form) token))))

(defn op? [form & [token]]
  (token-of-type? :op form token))

(defn special? [form & [token]]
  (token-of-type? :special form token))

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
  nil)


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
    (if (= first-form :guard)
      (rest (rest forms))
      (throw (Exception.
              (if (op? first-form)
                ;; Could be an operator with lower precedence than |.
                (str "Outermost operator must not be \"" (:token first-form)
                     "\" at line " (:line-num first-form)
                     " in file " path ".")

                ;; Shouldn't be possible, but just in case.
                (str "Outermost operator must be \"|\" in file " path ".")))))))


;;; Remove annotations from tokens.
;;; Wrap names that aren't Clojure symbols, or that would be Clojure keywords.

(defn unannotate-tokens [form]
  (cond (seq? form)
        (map unannotate-tokens form)

        (map? form)
        (let [token (:token form)]
          (cond (= (:type form) :string)
                token

                (re-find #"(^\.)|[,;:\\/'~]" token)
                (list :name token)

                (= token "∞")
                (read-string "infinity")
                
                :default (read-string token)))

        :default form))


;;;

;; Returns: <assertions>
(defn reform [path declarations annotated-tokens]
  (let [pr-matrix (build-pr-matrix declarations)]
    (->> annotated-tokens
         annotate-special-tokens
         (check-balance path)
         (annotate-ops declarations)
         build-parens
         (reform-top-level pr-matrix path)
         unannotate-tokens)))
