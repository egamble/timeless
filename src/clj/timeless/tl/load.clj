(ns timeless.tl.load
  "Load TL or TLS code."
  (:require [timeless.tl.tokenize :refer [tokenize]]
            [timeless.tl.reform :refer [reform]]
            [timeless.tl.extract :refer [extract]]
            [clojure.string :as str]))




;; TODO:
;; - an empty file throws a misleading error
;; - a file with just "foo" says unterminated string literal








;;; Load included TL or TLS files.

(declare read-tl-source read-tls-source)

(defn drop-extension [path]
  (let [split-path (str/split path #"\.")]
    (if (second split-path)
      (str/join "."
                (drop-last split-path))
      path)))

;; Returns:
;; {:declarations <declarations>
;;  :assertions <assertions>}
(defn load-included-file [path parent-path line-num]
  (let [without-extension (drop-extension path)]
    (or
     (let [try-path (str without-extension ".tls")
           tls-source (try
                        (slurp try-path)
                        (catch Exception e nil))]
       (when tls-source
         (read-tls-source tls-source)))

     (let [try-path (str without-extension ".tl")
           tl-source (try
                      (slurp try-path)
                      (catch Exception e nil))]
       (when tl-source
         (read-tl-source try-path tl-source)))

     (let [tl-source (try
                       (slurp path)
                       (catch Exception e
                         (throw (Exception. (str "#include error at line " line-num
                                                 " in file " parent-path
                                                 ": " (.getMessage e))))))]
       (read-tl-source path tl-source)))))

(defn load-include [parent-path annotated-include]
  (let [{line-num :line-num paths :paths} annotated-include]
    (reduce #(into %1 (load-included-file %2 parent-path line-num))
            {}
            paths)))

(defn load-includes [parent-path annotated-includes]
  (reduce #(into %1 (load-include parent-path %2))
          {}
          annotated-includes))


;;; Read source strings.

(defn read-tls-source [source]
  (let [forms (read-string (str "(" source ")"))
        grouped-forms (group-by #(= :declare (first %))
                                forms)]
    {:declarations (grouped-forms true)
     :assertions (grouped-forms false)}))

(defn read-tl-source [path source]
  (let [[declarations annotated-includes source strings]
        (extract path source)

        {included-declarations :declarations
         included-assertions :assertions}
        (load-includes path annotated-includes)

        declarations (concat declarations included-declarations)
        annotated-tokens (tokenize declarations path source strings)
        assertions (concat (reform path declarations annotated-tokens)
                           included-assertions)]
    {:declarations declarations
     :assertions assertions}))


;;; Write TLS code.

(defn write-file [out-path forms]
  (spit out-path
        (str (str/join "\n"
                       (map pr-str forms))
             "\n")))


;;; Load TL file, convert to TLS and write.

(defn convert [in-path out-path]
  (let [{declarations :declarations
         assertions :assertions}
        (read-tl-source in-path (slurp in-path))]
    (write-file out-path
                (concat declarations assertions))))

(defn -main [in-file out-file]
  (convert in-file out-file))
