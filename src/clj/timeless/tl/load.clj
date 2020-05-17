(ns timeless.tl.load
  "Load TL or TLS code."
  (:require [timeless.tl.tokenize :refer [tokenize]]
            [timeless.tl.reform :refer [reform]]
            [timeless.tl.extract :refer [extract]]
            [clojure.string :as str]))




Returns: [<path> <source>]
(defn get-include [include-line]
  (let [path (second (:split-line include-line))
        source (try
                 (slurp path)
                 (catch Exception e
                   (throw (Exception. (str "Line " (:line-num include-line)
                                           " of file " (:file include-line)
                                           ": #include " (.getMessage e))))))]
    [path source]))





;;; Read source strings.

(defn read-tls-source [source]
  (let [forms (read-string (str "(" source ")"))
        grouped-forms (group-by #(= :declare (first %))
                                forms)]
    {:declarations (grouped-forms true)
     :assertions (grouped-forms false)}))

(defn load-includes [annotated-includes]
  (reduce ...))

(defn read-tl-source [path source]
  (let [[declarations annotated-includes source strings]
        (extract path source)

        {included-declarations :declarations
         included-assertions :assertions}
        (load-includes annotated-includes)

        declarations (concat declarations included-declarations)
        annotated-tokens (tokenize declarations path source strings)
        assertions (concat (reform declarations annotated-tokens)
                           included-assertions)]
    {:declarations declarations
     :assertions assertions}))


;;; Load TL or TLS code.

(defn drop-extension [path]
  (let [split-path (str/split path #"\.")]
    (if (second split-path)
      (str/join "."
                (drop-last split-path))
      path)))

;; Returns:
;; {:declarations <declarations>
;;  :assertions <assertions>}
(defn load-source-file [path &optional include-line]
  (let [without-extension (drop-extension path)
        tls-source (try
                     (slurp (str without-extension ".tls"))
                     (catch Exception e nil))
        tl-source (when (not tls-source)
                    (try
                      (slurp (str without-extension ".tl"))
                      (catch Exception e nil)))
        tl-source  (or tl-source
                       (when (not tls-source)
                         (try
                           (slurp path)
                           (catch Exception e
                             (throw (if include-line
                                      (Exception. (if include-line
                                                    (str "Line " include-line
                                                         ": #include " (.getMessage e))))
                                      e))))))]
    (if tls-source
      (read-tls-source tls-source)
      (read-tl-source path tl-source))))


;;; Write TLS code.

(defn write-file [out-path forms]
  (spit out-path
        (str (str/join "\n"
                       (map pr-str forms))
             "\n")))


;;; Load TL or TLS file and write TLS file.

(defn convert [in-path out-path]
  (let [{declarations :declarations
         assertions :assertions}
        (load-source-file in-path)]
    (write-file out-path
                (concat declarations assertions))))

(defn -main [in-file out-file]
  (convert in-file out-file))
