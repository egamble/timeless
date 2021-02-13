(ns timeless.run.load
  "Load TLS assertions."
  (:require [timeless.utils :refer :all]))


(declare load-tls-file)


;; process-form:
;; - For vector forms, moves the metadata map, if present, from the second element to the Clojure metadata of the vector.
;; - Adds the path to the metadata.
;; - Converts simplified TLS forms to vector form, without metadata.
;; Each returned value, except for an included assertions sequence, is wrapped in an extra list,
;; so that included assertions are concatenated with the other assertions.
(defn process-form [in-path exp]
  (if (has-type :include exp)
    (let [include-path (if (map? (second exp))
                         (third exp)
                         (second exp))]
      (load-tls-file include-path))

    (let [f (partial mapcat
                     (partial process-form
                              in-path))]
      (list
       (cond
         (vector? exp)
         (let [type (first exp)
               m (into (if (map? (second exp))
                         (second exp)
                         {})
                       [[:path in-path]])
               args (if (map? (second exp))
                      (third-on exp)
                      (rest exp))
               args (cond
                      (#{:name :num :str} type)
                      args

                      (= :bind type)
                      (cons (first args)
                            (f (rest args)))

                      :else (f args))]
           (with-meta
             (into [(first exp)] args)
             m))

         (seq? exp)
         (into [:apply] (f exp))

         (symbol? exp)
         [:name (str exp)]
         
         (number? exp)
         [:num exp]

         (string? exp)
         [:str exp]
         
         :else exp)))))


(defn load-tls-file [in-path]
  (let [source (slurp (str (strip-tl-filepath in-path)
                           ".tls"))
        assertions (read-string (str "(" source ")"))]
    (mapcat (partial process-form in-path)
            assertions)))


(defn reduce-assertions [context assertion]
  (cond
    (and (has-type :apply assertion)
         (let [exps (all-args assertion)]
           (and (= 3 (count exps))
                (let [[op name-exp _] exps]
                  (and (has-type :name op)
                       (= "=" (first-arg op))
                       (has-type :name name-exp))))))
    (into context
          (let [[op name-exp exp] (all-args assertion)]
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
