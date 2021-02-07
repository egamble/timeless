(ns timeless.main
  "Transform TL to TLS code."
  (:require [timeless.transform.ast :refer [tl->ast tl-exp->ast make-tl-exp-parser]]
            [timeless.transform.tls :refer [ast->tls]]
            [timeless.run.load :refer [build-top-level-context]]
            [timeless.run.eval :refer [eval-tls]]
            [timeless.pretty :refer :all]
            [timeless.utils :refer :all]
            [clojure.string :as str]))


(def declare-tokens #{"#name" "#op" "#opr" "#opl"})

(defn split-source-line [line]
  (remove str/blank?
          (str/split (str/trim line)
                 #"[ \t]")))


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
            source (slurp (str (strip-tl-filepath path)
                               ".tl"))

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


(defn eval-exp* [parser context tl-exp show-metadata?]
  (let [exp (->> (tl-exp->ast parser tl-exp)
                 (ast->tls ())
                 first
                 (eval-tls context))]
    (when exp
      (println
       (pretty ""
               show-metadata?
               (not show-metadata?) ; simplify
               false ; no initial indent
               exp))
      nil)))


(defn get-parser-and-context [in-path]
  (let [stripped-path (strip-tl-filepath in-path)
        grammar (slurp (str stripped-path ".gmr"))]
    [(make-tl-exp-parser grammar)
     (build-top-level-context in-path)]))


(defn eval-exp [in-path tl-exp & [show-metadata?]]
  (let [[parser context] (get-parser-and-context in-path)]
    (eval-exp* parser
               context
               tl-exp
               show-metadata?)))


(defn repl [in-path & [show-metadata?]]
  (let [[parser context] (get-parser-and-context in-path)
        prompt (fn []
                 (print "> ")
                 (flush))
        _ (println "Press return twice to evaluate.\nCtrl-d to quit.")
        _ (prompt)
        lines (line-seq (java.io.BufferedReader. *in*))

        ws? (partial re-find #"^\s*$")]
    (dorun
     (map (fn [exp-lines]
            (let [tl-exp (str/join "\n" exp-lines)]
              (when-not (ws? tl-exp)
                (eval-exp* parser
                           context
                           tl-exp
                           show-metadata?)
                (prompt))))
          (partition-by ws? lines)))))


(defn write-tls-file [out-path assertions]
  (->> assertions
       (map (partial pretty
                     ""
                     true ; show metadata
                     false ; don't simplify
                     false ; no initial indent
                     ))
       insert-newlines
       (apply str)
       (spit out-path)))


(defn tl->tls [in-path]
  (let [stripped-path (strip-tl-filepath in-path)
        source (slurp (str stripped-path ".tl"))
        
        [includes declarations]
        (extract-declarations source)

        assertions (->> (tl->ast declarations source (str stripped-path ".gmr"))
                        (ast->tls includes))]
    (write-tls-file (str stripped-path ".tls")
                    assertions)))


(defn -main [in-path]
  (tl->tls in-path))
