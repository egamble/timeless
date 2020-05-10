(ns timeless.tl.convert
  "Convert TL code to TLS code."
  (:require [timeless.tl.load :refer [load-file]]
            [timeless.tl.reform :refer [reform]]
            [clojure.string :as str]))



;;; Convert from TL to TLS.

(defn tl->tls [in-path out-path]
  (let [[tokens comments declarations included-assertions] (load-file in-path)
        assertions ()
        form (->> in-path
                  load-file
                  reform)]
    (spit out-path
          (pr-str form))))

(defn -main [in-file out-file]
  (tl->tls in-file out-file))
