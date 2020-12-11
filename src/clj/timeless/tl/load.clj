(ns timeless.tl.load
  "Load TL or TLS code."
  (:require [timeless.tl.parse :refer [parse]]
            [timeless.tl.utils :refer :all]
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

(defn write-tls-file [out-path forms]
  (spit out-path
        (str (str/join "\n"
                       (map pr-str forms))
             "\n")))

(defn tl->tls [in-path out-path generated-grammar-file]
  (let [source (slurp in-path)
        declarations (extract-declarations source)
        assertions (parse declarations source generated-grammar-file)]
    (write-tls-file out-path assertions)))

(defn -main [in-file out-file generated-grammar-file]
  (tl->tls in-file out-file generated-grammar-file))
