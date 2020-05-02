(ns timeless.tl.prefixize
  "Convert infix operators to prefix functions."
  (:require [clojure.string :as str]))


;;; Convert infix operators to prefix functions.

(declare check-balance
         build-parens
         build-top-level
         wrap-non-clj-names)

;; Returns: <annotated tokens>
(defn prefixize [[declaration-lines annotated-tokens]]
  (-> annotated-tokens
      check-balance
      build-parens
      build-top-level
      wrap-non-clj-names))


;;; Check that all brackets are balanced.

(def left-brackets #{"(" "{" "["})
(def right-brackets #{")" "}" "]"})

(defn check-stack-top [annotated-token stack]
  (if (= (condp = (:token annotated-token)
           ")" "("
           "}" "{"
           "]" "[")
         (:token (first stack)))
    (rest stack)
    (let [{err-token :token
           err-line :line-num
           err-path :file}
          (or (first stack)
              annotated-token)]
      (throw (Exception. (str "Unbalanced \"" err-token
                              "\" at line " err-line
                              " in file " err-path "."))))))

(defn check-next-token [stack annotated-token]
  (if (= :string (:type annotated-token))
    stack
    (let [token (:token annotated-token)]
      (cond
        (left-brackets token) (cons annotated-token stack)
        (right-brackets token) (check-stack-top annotated-token stack)
        :default stack))))

(defn check-balance [annotated-tokens]
  (let [stack (reduce check-next-token nil annotated-tokens)]
    (when (seq stack)
      (let [{token :token line :line-num path :file} (first stack)]
        (throw (Exception. (str "Unbalanced \"" token
                                "\" at line " line
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
          (= token "{") (cons (list :set) stack)
          (= token "[") (cons (list :seq) stack)

          (right-brackets token)
          (let [new-head  (cons (reverse (first stack))
                                (second stack))
                new-tail (rest (rest stack))]
            (cons new-head new-tail))

          :default (cons new-head (rest stack)))))))

(defn build-parens [annotated-tokens]
  (-> (reduce build-parens-next-token (list '()) annotated-tokens)
      first
      reverse))


;;; Build top level.

(defn build-top-level [form]
  form)


;;; Wrap names that aren't Clojure symbols, or that would be Clojure keywords.

(defn wrap-non-clj-names [form]
  (cond
    (list? form) (map wrap-non-clj-names form)
    (keyword? form) form

    (and (not (= (:type form) :string))
         (re-find #"[\.,;:\\/'~]" (:token form)))
    (list :name form)

    :default form))
