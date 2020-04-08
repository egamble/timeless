(ns timeless.tl.compile
  "Run TLS code from the command line."
  ;; (:require [timeless.common :refer :all])
  )

(defn tl->tls [in-file out-file]
  (spit out-file (slurp in-file)))

(defn -main [in-file out-file]
  (tl->tls in-file out-file))
