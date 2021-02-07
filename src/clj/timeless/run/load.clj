(ns timeless.run.load
  "Load TLS assertions."
  (:require [timeless.utils :refer :all]))


(declare load-tls-file)


;; set-metadata:
;; - Moves the metadata map, if any, from the second element of each vector, to the Clojure metadata of the vector.
;; - Adds the path to the metadata.
;; Each returned value, except for an included assertions sequence, is wrapped in an extra list,
;; so that included assertions are concatenated with the other assertions.
(defn set-metadata [in-path exp]
  (if (has-type :include exp)
    (let [include-path (if (map? (second exp))
                         (third exp)
                         (second exp))]
      (load-tls-file include-path))

    (list
     (cond
       (vector? exp)
       (let [m (into (if (map? (second exp))
                       (second exp)
                       {})
                     [[:path in-path]])
             args (if (map? (second exp))
                    (third-on exp)
                    (rest exp))]
         (with-meta
           (into [(first exp)]
                 (mapcat (partial set-metadata in-path)
                         args))
           m))

       (seq? exp)
       (mapcat (partial set-metadata in-path)
               exp)

       :else exp))))


(defn load-tls-file [in-path]
  (let [source (slurp (str (strip-tl-filepath in-path)
                           ".tls"))
        assertions (read-string (str "(" source ")"))]
    (mapcat (partial set-metadata in-path)
            assertions)))


(defn reduce-assertions [context assertion]
  (cond
    (and (has-type :apply assertion)
         (let [exps (first-arg assertion)]
           (and (seq? exps)
                (= 3 (count exps))
                (let [[op name-exp _] exps]
                  (and (has-type :name op)
                       (= "=" (first-arg op))
                       (has-type :name name-exp))))))
    (into context
          (let [[op name-exp exp] (first-arg assertion)]
            [[(first-arg name-exp)
              exp]]))

    (has-type :and assertion)
    (reduce reduce-assertions context (all-args assertion))

    :else
    (error-at "A top-level assertion must, for now, be an equals operation, or a chain of equals, with a name on the left side."
              assertion)))


(defn build-top-level-context [in-path]
  (let [assertions (load-tls-file in-path)]
    (reduce reduce-assertions
            {}
            assertions))) 
