(ns timeless.tl.load
  "Load TL or TLS code."
  (:require [timeless.tl.tokenize :refer [tokenize]]
            [timeless.tl.reform :refer [reform]]
            [timeless.tl.extract :refer [extract]]
            [clojure.string :as str]))




;; Returns: [<path> <source>]
;; (defn get-include [include-line]
;;   (let [path (second (:split-line include-line))
;;         source (try
;;                  (slurp path)
;;                  (catch Exception e
;;                    (throw (Exception. (str "Line " (:line-num include-line)
;;                                            " of file " (:file include-line)
;;                                            ": #include " (.getMessage e))))))]
;;     [path source]))





;;; Read source strings.

;; Returns: [<declarations> <assertions>]
(defn read-tls-source [source]
  (let [forms (read-string (str "(" source ")"))
        grouped-forms (group-by #(= :declare (first %))
                                forms)]
    [(grouped-forms true)
     (grouped-forms false)]))

;; TODO: load annotated-includes

(defn read-tl-source [path source]
  (let [[declarations annotated-includes source strings]
        (extract path source)

        annotated-tokens (tokenize declarations path source strings)
        assertions (reform declarations annotated-tokens)]
    [declarations assertions]))


;;; Load TL or TLS code.

(defn drop-extension [path]
  (let [split-path (str/split path #"\.")]
    (when (second split-path)
      (str/join "."
                (drop-last split-path)))))

(defn load-source-file [path]
  (let [without-extension (drop-extension path)
        tls-source (when without-extension
                     (try
                       (slurp (str path ".tls"))
                       (catch Exception e nil)))
        tl-source (when (and (not tls-source)
                             without-extension)
                    (try
                      (slurp (str path ".tl"))
                      (catch Exception e nil)))
        tl-source (when (not (or tls-source tl-source))
                    (slurp path))]
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

(defn load-and-write-file [in-path out-path]
  (let [[declarations assertions] (load-source-file in-path)]
    (write-file out-path
                (concat declarations assertions))))

(defn -main [in-file out-file]
  (load-and-write-file in-file out-file))
