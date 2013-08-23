(ns timeless.bootstrap.parser
  "Parser for Timeless. Mainly intended to parse a self-hosting Timeless compiler."
  (:require [name.choi.joshua.fnparse :as p]))


;;; Character rules
;;;;;;;;;;;;;;;;;;;

(defn nb-char [c]
  (p/invisi-conc (p/lit c) (p/update-info :column inc)))

(def digit (p/lit-alt-seq "0123456789" nb-char))

(def hyphen (nb-char \-))

(def letter
  (p/lit-alt-seq (map char (concat (range (int \A) (inc (int \Z)))
                                   (range (int \a) (inc (int \z)))))
                 nb-char))

(def underscore (nb-char \_))

(def single-quote (nb-char \'))

(def double-quote (nb-char \"))

(def period (nb-char \.))


;;; Node builders
;;;;;;;;;;;;;;;;;

(defn make-node [type v]
  {:type type
   :val v})


;;; Atomic rules
;;;;;;;;;;;;;;;;

(def name
  (p/complex
   [c (p/alt letter underscore)
    s (p/rep* (p/alt letter digit underscore))]

   (make-node :name
              (apply str c s))))

(def number
  (p/complex
   [s (p/opt hyphen)
    n (p/rep+ digit)
    m (p/opt (p/conc period (p/rep+ digit)))]

   (make-node :number
              (read-string
               (apply str (flatten [s n m]))))))

(def quoted-name
  (p/complex
   [_ single-quote
    s (p/rep*
       (p/alt
        (p/lit-conc-seq "\\\\" )
        (p/lit-conc-seq "\\'")
        (p/except p/anything single-quote)))
    _ single-quote]

   (let [f #(if (seq? %) (second %) %)]
     (make-node :name
                (apply str (map f s))))))

(def string
  (p/complex
   [_ double-quote
    s (p/rep*
       (p/alt
        (p/lit-conc-seq "\\\\" )
        (p/lit-conc-seq "\\\"")
        (p/except p/anything double-quote)))
    _ double-quote]

   (let [f #(if (seq? %) (second %) %)]
     (make-node :string
                (apply str (map f s))))))

(def atom (p/alt number name quoted-name string))


;;; Run the parser
;;;;;;;;;;;;;;;;;;

(defn pos [state]
  (str "(" (:line state) "," (:column state) ")"))

(defn parse [program]
  (let [init-state {:remainder program
                    :line 1
                    :column 1}]

    (p/rule-match atom
                  (fn [_] (println "no match"))
                  (fn [_ state] (println (str "match failure at " (pos state))))
                  init-state)))
