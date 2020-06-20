(ns timeless.tl.parse
  "Reform tokens to produce TLS S-expressions."
  (:require [timeless.tl.ops :refer [build-pr-matrix]]
            [instaparse.core :as insta]
            [clojure.string :as str]))


(def as-and-bs
  (insta/parser
    "S = (AB | WS)*
     AB = A B
     A = 'a'+
     B = 'b'+
     WS = '\n'"))

;; Returns: <assertions>
(defn parse [path declarations source]
  (let [pr-matrix (build-pr-matrix declarations)]
    (println (as-and-bs "aaaaabbb\naaabb"))
    nil))
