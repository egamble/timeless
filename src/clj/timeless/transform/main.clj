(ns timeless.transform.main
  "Transform TL to TLS code."
  (:require [timeless.transform.parse :refer [tl->ast]]
            [timeless.transform.utils :refer :all]
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

(defn pretty [indent form]
  (let [m (second form)
        subforms (if (map? m)
                   (rest (rest form))
                   (rest form))]
    (str indent "[" (first form)
         (when (map? m)
           (str " " m))
         (when-not (empty? subforms)
           (let [f (if (some vector? subforms)
                     #(str "\n"
                           (pretty (str indent "  ") %))
                     #(str " "
                           (pr-str %)))]
             (apply str (map f subforms))))
         "]")))

(defn write-tls-file [out-path assertions]
  (let [insert-newlines #(interleave % (repeat "\n"))]
    (->> assertions
         (map (partial pretty ""))
         insert-newlines
         (apply str)
         (spit out-path))))

(defn tl->tls [in-path out-path generated-grammar-file]
  (let [source (slurp in-path)
        declarations (extract-declarations source)
        assertions (tl->ast declarations source generated-grammar-file)]
    (write-tls-file out-path assertions)))

(defn -main [in-file out-file generated-grammar-file]
  (tl->tls in-file out-file generated-grammar-file))
