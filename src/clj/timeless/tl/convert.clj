(ns timeless.tl.convert
  "Convert TL code to TLS code."
  (:require [timeless.tl.tokenize :refer [tokenize]]
            [timeless.tl.prefixize :refer [prefixize]]
            [clojure.string :as str]))


;;; Convert from TL to TLS.

(defn tl->tls [in-path out-path]
  (let [source (slurp in-path)
        annotated-tokens (->> in-path
                              slurp
                              (tokenize in-path)
                              prefixize)]
    (spit out-path
          (str (str/join "\n" annotated-tokens)
               "\n"))))

(defn -main [in-file out-file]
  (tl->tls in-file out-file))
