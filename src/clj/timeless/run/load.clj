(ns timeless.run.load
  "Load TLS assertions."
  (:require [timeless.utils :refer :all]))


(declare load-tls-file)

;; Moves the metadata map, if any, from the second element of each vector, to the Clojure metadata of the vector.
;; Adds the path to the metadata.
;; For the first call, exp is all the assertions.
;; Each returned value, except for an included assertions list, is wrapped in an extra list,
;; so that included assertions are concatenated with the other assertions.
(defn set-metadata [in-path exp]
  (cond
    (has-type :include exp)
    (let [include-path (if (map? (second exp))
                         (third exp)
                         (second exp))]
      (load-tls-file include-path))

    (vector? exp)
    (let [m (into (if (map? (second exp))
                    (second exp)
                    {})
                  [[:path in-path]])
          args (if (map? (second exp))
                 (third-on exp)
                 (rest exp))]
      (list
       (with-meta
         (into [(first exp)]
               (mapcat (partial set-metadata in-path)
                       args))
         m)))

    (seq? exp)
    (list
     (apply list (mapcat (partial set-metadata in-path)
                         exp)))

    :else
    (list exp)))


(defn load-tls-file [in-path]
  (let [source (slurp (str (strip-tl-filepath in-path)
                           ".tls"))
        assertions (read-string (str "(" source ")"))]
    (first
     (set-metadata in-path assertions))))


(defn strip-lists [x]
  (println "blaz" x "bloof" (meta x))
  (if (list? x)
    (strip-lists (first x))
    x))

(defn reduce-assertions [context assertion]
  (cond
    (and (list? assertion)
         (= 3 (count assertion))
         (has-type :name (first assertion))
         (= "=" (first-arg (first assertion)))
         (has-type :name (second assertion)))
    (into context
          [[(first-arg (second assertion))
            (third assertion)]])

    (and (vector? assertion)
         (has-type :and assertion))
    (reduce reduce-assertions context (all-args assertion))

    :else
    (error-at "A top-level assertion must, for now, be an equals operation, or a chain of equals, with a name on the left side."
              (strip-lists assertion))))


(defn build-top-level-context [in-path]
  (let [assertions (load-tls-file in-path)]
    (reduce reduce-assertions
            {}
            assertions))) 
