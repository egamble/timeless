(ns timeless.tl.prefixize
  "Convert infix operators to prefix functions."
  (:require [clojure.string :as str]))


;;; Convert infix operators to prefix functions.

(declare check-balance)

;; Returns: <annotated tokens>
(defn prefixize [[declaration-lines annotated-tokens]]
  (check-balance annotated-tokens)
  annotated-tokens)


;;; Check that all brackets are balanced.

(def left-brackets #{"(" "{" "["})
(def right-brackets #{")" "}" "]"})

(defn check-stack-top [{token :token line :line-num path :file} stack]
  (if (= (condp = token
           ")" "("
           "}" "{"
           "]" "[")
         (:token (first stack)))
    (rest stack)
    (throw (Exception. (str "Unbalanced \"" token
                            "\" at line " line
                            " in file " path ".")))))

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
                                " in file " path ".")))))))
