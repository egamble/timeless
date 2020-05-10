(ns timeless.tl.convert
  "Convert TL code to TLS code."
  (:require [timeless.tl.load :refer [load-file]]
            [timeless.tl.reform :refer [reform]]
            [clojure.string :as str]))


(defn write-file [forms]
  (spit out-path
        (str (str/join "\n"
                       (map pr-str forms))
             "\n")))


;;; Convert from TL to TLS.

(defn tl->tls [in-path out-path]
  (let [[tokens declarations included-assertions] (load-file in-path)
        [assertions declarations] (reform tokens declarations)]
    (write-file out-path
                (concat assertions
                        included-assertions
                        declarations))))

(defn -main [in-file out-file]
  (tl->tls in-file out-file))
