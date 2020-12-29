(ns timeless.transform.main
  "Transform TL to TLS code."
  (:require [timeless.transform.ast :refer [tl->ast tl-exp->ast]]
            [timeless.transform.tls :refer [ast->tls]]
            [timeless.transform.pretty :refer :all]
            [timeless.transform.utils :refer :all]
            [clojure.string :as str]))


(def declare-tokens #{"#name" "#op" "#opr" "#opl"})

(defn split-source-line [line]
  (remove str/blank?
          (str/split (str/trim line)
                 #"[ \t]")))

(defn tl-filepath [path]
  (str (str/replace path #"\.tls?$" "")
       ".tl"))


(declare extract-declarations)

(defn make-declaration [[includes declarations] line]
  (let [tokens (split-source-line line)
        t (first tokens)]
    (cond
      (declare-tokens t)
      [includes
       (cons-at-end declarations tokens)]

      (= "#include" t)
      (let [path (second tokens)
            source (slurp (tl-filepath path))

            [included-includes included-declarations]
            (extract-declarations source)]
        [(concat includes (list path) included-includes)
         (concat declarations included-declarations)])

      :else
      [includes declarations])))


;; Returns: [<included filepaths> <declarations>]
(defn extract-declarations [source]
  ;; Add a space to ensure the source has at least one non-newline character,
  ;; otherwise str/split-lines will return nil.
  (->> (str source " ")
       str/split-lines
       (reduce make-declaration [() ()])))


(defn tl-exp->tls [source in-path-grammar]
  (let [exp (->> (tl-exp->ast source in-path-grammar)
                 (ast->tls nil)
                 first)]
    (when exp
      (println
       (pretty ""
               true
               false
               exp))
      nil)))


(defn write-tls-file [out-path assertions suppress-metadata?]
  (->> assertions
       (map (partial pretty
                     ""
                     suppress-metadata?
                     false))
       insert-newlines
       (apply str)
       (spit out-path)))


(defn tl->tls [in-path-tl out-path-tls & [out-path-grammar suppress-metadata?]]
  (let [source (slurp in-path-tl)
        
        [includes declarations]
        (extract-declarations source)

        assertions (->> (tl->ast declarations source out-path-grammar)
                        (ast->tls includes))]
    (write-tls-file out-path-tls assertions suppress-metadata?)))


(defn -main [in-path-tl out-path-tls & [out-path-generated-grammar suppress-metadata?]]
  (tl->tls in-path-tl out-path-tls out-path-generated-grammar suppress-metadata?))
