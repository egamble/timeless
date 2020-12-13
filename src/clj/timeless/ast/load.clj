(ns timeless.ast.load
  "Load TL code."
  (:require [timeless.ast.parse :refer [parse]]
            [timeless.ast.utils :refer :all]
            [clojure.string :as str]))


(def declare-tokens #{"#name" "#op" "#opr" "#opl"})

(defn split-source-line [line]
  (remove str/blank?
          (str/split (str/trim line)
                 #"[ \t]")))

(defn make-declaration [line]
  (let [tokens (split-source-line line)]
    (when (declare-tokens (first tokens))
      tokens)))

(defn extract-declarations [source]
  ;; Add a space to ensure the source has at least one non-newline character,
  ;; otherwise str/split-lines will return nil.
  (->> (str source " ")
       str/split-lines
       (map make-declaration)
       (remove nil?)))

(defn pretty [indent [k m & subforms]]
  (str indent "[" k " " m
       (when-not (empty? subforms)
         (if (some vector? subforms)
             (str "\n"
                  (apply str (map (partial pretty (str indent "  "))
                                  subforms))
                  indent)
             (apply str (map #(str " " (pr-str %))
                             subforms))))
       "]\n"))

(defn write-ast-file [out-path assertions]
  (->> assertions
       (map (partial pretty ""))
       (apply str)
       (spit out-path)))

(defn tl->ast [in-path out-path generated-grammar-file]
  (let [source (slurp in-path)
        declarations (extract-declarations source)
        assertions (parse declarations source generated-grammar-file)]
    (write-ast-file out-path assertions)))

(defn -main [in-file out-file generated-grammar-file]
  (tl->ast in-file out-file generated-grammar-file))
