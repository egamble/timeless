(ns timeless.tl.parse
  "Parse tokens to produce TLS S-expressions."
  (:require [timeless.tl.ops :refer [build-pr-matrix annotate-ops]]
            [clojure.string :as str]))


;;;

(defn parse [pr-matrix tokens]
  tokens)


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
(defn parse [path declarations tokens]
  (let [pr-matrix (build-pr-matrix declarations)]
    (->> tokens
         (annotate-ops declarations)
         (parse pr-matrix)
         unannotate-tokens)))
