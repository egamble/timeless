(ns timeless.parser
  "Parser for Timeless. Mainly intended to parse a self-hosting Timeless compiler."
  (:require [name.choi.joshua.fnparse  :as p]
            [clojure.core.memoize      :refer [memo memo-clear!]]
            [timeless.common :refer [error node? update-vals remove-vals show]]
            [instaparse.core :as insta]))


;;; memoize rules
;;;;;;;;;;;;;;;;;

(def memoized-rules (atom nil))

(defn memoize-rule [rule]
  (let [rule-m (memo rule)]
    (swap! memoized-rules conj rule-m)
    rule-m))

(defmacro complex-m
  [steps & product-expr]
  `(memoize-rule (p/complex ~steps ~@product-expr)))

(defmacro alt-m
  [& subrules]
  `(memoize-rule (p/alt ~@subrules)))

(defn clear-rule-memos! []
  (doseq [rule-m @memoized-rules]
    (memo-clear! rule-m)))


;;; lexer character rules
;;;;;;;;;;;;;;;;;;;;;;;;;

(defn nb-char-rule [rule]
  (p/invisi-conc (p/except rule (p/lit \newline))
                 (p/update-info :col inc)))

(defn nb-char [c] (nb-char-rule (p/lit c)))

(def any-nb-char (nb-char-rule p/anything))

(def newline-lex
  (p/invisi-conc (p/lit \newline)
                 (p/set-info :col 1)
                 (p/update-info :line inc)))

(def digit-lex (p/lit-alt-seq "0123456789" nb-char))

(def sign-lex (p/alt (nb-char \+) (nb-char \-)))

(def underscore-lex (nb-char \_))

(def double-quote-lex (nb-char \"))

(def comment-lex (p/conc (nb-char \#)
                         (p/rep* any-nb-char)))

(def ws (p/rep* (p/alt comment-lex newline-lex (p/lit-alt-seq " \t" nb-char))))


;;; error reporting
;;;;;;;;;;;;;;;;;;;

(defn failpoint-error-fn [msg pos-map]
  (fn [_ _] (error msg pos-map)))


;;; node builders
;;;;;;;;;;;;;;;;;

(defn make-node [type pos-map val & [other-fields]]
  (merge other-fields
         {:type type
          :val val
          :line (:line pos-map)
          :col (:col pos-map)}))

(defmacro deflex
  "Node builder for the lex phase.
   Binds the unqualified name 'pos-state' to the initial state.
   The name 'pos-state' may be used within the step forms.
   Returns a node of the given type and value."
  [name type steps val]
  `(def ~name
     (complex-m [~'pos-state p/get-state ~@steps]
                (make-node ~type ~'pos-state ~val))))


;;; lexer token rules
;;;;;;;;;;;;;;;;;;;;;

(def forbidden-name-chars
  (str " \t[](){}\".,:+-*/<>=&|#\\"
        \u2192 \u21A6 \u27F6 \u27FC
        \u2264 \u2265 \u2227 \u2228
        \u2229 \u222A \u2282 \u2286
        \u2283 \u2287 \u2208 \u220A \u2260))

(def name-char-lex
  (p/except any-nb-char (p/lit-alt-seq forbidden-name-chars nb-char)))

(deflex name-lex :name
  [c (p/except name-char-lex digit-lex)
   cs (p/rep* name-char-lex)]
  (apply str c cs))

(deflex string-lit :str
  [_ double-quote-lex
   cs (p/rep*
       (p/alt
        (p/lit-conc-seq "\\\\" nb-char)
        (p/lit-conc-seq "\\\"" nb-char)
        (p/except (p/alt any-nb-char newline-lex) double-quote-lex)))
   _ (p/failpoint double-quote-lex
                  (failpoint-error-fn "Unmatched double quote"
                                      pos-state))]
  (let [f #(if (seq? %) (second %) %)]
    (apply str (map f cs))))

(deflex char-lit :char
  [_ (nb-char \\)
   cs (p/alt
       (p/lit-conc-seq "tab" nb-char)
       (p/lit-conc-seq "space" nb-char)
       (p/lit-conc-seq "newline" nb-char)
       (p/alt any-nb-char newline-lex))]
  (if (seq? cs)
    (read-string (apply str (cons \\ cs)))
    cs))

(def num-magnitude (p/conc (p/rep+ digit-lex)
                           (p/opt (p/conc (nb-char \.) (p/rep+ digit-lex)))))

(deflex signed-number-lit :num
  [_ ws
   sn (p/opt sign-lex)
   d num-magnitude
   _ ws]
  (read-string
   (apply str (flatten [sn d]))))

(deflex unsigned-number-lit :num
  [_ ws
   d num-magnitude
   _ ws]
  (read-string
   (apply str (flatten d))))

(def non-num-atom-lex
  (complex-m
   [_ ws
    a (p/alt name-lex string-lit char-lit)
    _ ws]
   a))

(deflex left-lex :bracket
  [_ ws
   c (p/lit-alt-seq "([{" nb-char)
   _ ws]
  (str c))

(deflex right-lex :bracket
  [_ ws
   c (p/lit-alt-seq ")]}" nb-char)
   _ ws]
  (str c))

(defmacro alt-lex
  "strs is a collection of op strings, or of tuples of op str and one or more unicode alternatives.
   Expands into a rule whose value on success is the matched op str."
  [strs]
  (let [rules (map (fn [s]
                     (let [s (if (coll? s) s [s])
                           [s1 & cs] s
                           cs (map #(do `(nb-char ~%)) cs)
                           r `(p/lit-conc-seq ~s1 nb-char)
                           r (if (seq cs)
                               `(p/alt ~r ~@cs)
                               r)]
                       `(p/semantics ~r (constantly ~s1))))
                   strs)]
    `(p/alt ~@rules)))

(deflex sep-lex :sep
  [_ ws
   s (alt-lex [["->" \u2192 \u21A6 \u27F6 \u27FC] ":" ","])
   _ ws]
  s)

(deflex op-lex :op
  [_ ws
   s (alt-lex
      [["<=" \u2264] [">=" \u2265]
       ["&&" \u2227] ["||" \u2228]
       ["&" \u2229] ["|" \u222A]
       ["<<" \u2282 \u2286] [">>" \u2283 \u2287]
       ["<-" \u2208 \u220A] ["/=" \u2260]
       "++" "+" "--" "-" "*" "/" "<" ">" "=" "." "..." ".."])
   _ ws]
  s)

(def tokens
  (p/semantics
   (p/rep*
    (p/alt (p/conc (p/alt left-lex sep-lex op-lex) (p/opt signed-number-lit))
           unsigned-number-lit
           non-num-atom-lex
           right-lex))
   (comp (partial remove nil?) flatten)))


;;; tokenize
;;;;;;;;;;;;

(defn tokenize [src]
  (let [e (fn [state]
            (error "Failure to tokenize" state))]
    (p/rule-match tokens
                  (fn [s] (e s))
                  (fn [_ s] (e s))
                  {:remainder src, :line 1, :col 1})))


;;; token detector
;;;;;;;;;;;;;;;;;;

(defn token?
  "Detects a token node of the given type(s) (a keyword or a set of keywords)
   and, optionally, with the given value or one of a set of values."
  [type & [val]]
  (p/term #(node? % type val)))


;;; shatter tokens into groups, one group for each top-level assertion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare expr-shatter)

(defmacro defunit-shatter
  [name left right]
  `(def ~name
     (complex-m
      [n1# (token? :bracket ~left)
       s# (p/rep* expr-shatter)
       n2# (p/failpoint
            (token? :bracket ~right)
            (failpoint-error-fn (str "Unmatched \"" ~left "\"")
                                n1#))]
      [n1# s# n2#])))

(defunit-shatter paren-shatter   "(" ")")
(defunit-shatter bracket-shatter "[" "]")
(defunit-shatter brace-shatter   "{" "}")

(def expr-shatter (p/alt (token? #{:name :str :num :char :sep :op})
                         paren-shatter
                         bracket-shatter
                         brace-shatter))

(defn shatter
  "Split tokens into groups corresponding to top-level assertions.
  The body of each top-level assertion is wrapped with parens, because '=' is not the lowest precedence op.
  '=' has relatively high precedence so that e.g. {b : a = b && c} is interpreted as {b : (a = b) && c}
  rather than {b : a = (b && c)}."
  [tokens]
  (let [e (fn [state]
            (error "Failure to shatter into top-level assertions"
                   (first (:remainder state))))
        m (p/rule-match
           (p/rep+ (p/conc (token? :name)
                           (token? :op "=")
                           (p/rep+ (p/invisi-conc expr-shatter
                                                  (p/not-followed-by (token? :op "="))))))
           (fn [s] (e s))
           (fn [_ s] (e s))
           {:remainder tokens})

        wrap-group (fn [[name op & rest]]
                     (let [f #(merge op {:type :bracket :val %})]
                       `[~name ~op ~(f "(") ~@rest ~(f ")")]))]
    (map (fn [group]
           (->> group
                flatten
                (remove nil?) ; remove nils because p/rep* can return nil
                wrap-group))
         m)))


;;; vec literal
;;;;;;;;;;;;;;;

(declare expr)

(def vec-lit
  (complex-m
   [pos-node (token? :bracket "[")
    es (p/rep* (p/invisi-conc expr (token? :sep ",")))
    e (p/opt expr)
    _ (token? :bracket "]")]
   (make-node :vec pos-node (vec (if e (conj es e) es)))))


;;; function and set literals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def clause
  (complex-m
   [k expr
    st (p/opt
        (complex-m [_ (token? :sep ":")
                    st expr]
                   st))
    v (p/opt
       (complex-m [_ (token? :sep "->")
                   v expr]
                  v))]
   (make-node :clause k v {:key k, :st st})))

(def fn-lit
  (complex-m
   [pos-node (token? :bracket "{")
    es (p/rep* (p/invisi-conc clause (token? :sep ",")))
    e (p/opt clause)
    _ (token? :bracket "}")]

   (let [clauses (vec (if e (conj es e) es))
         type (if (not-any? :val clauses)
                :set
                (if (every? :val clauses)
                  :fn
                  (error "Mixed set and fn clauses" pos-node)))]
     (make-node type pos-node clauses))))


;;; units (atoms and bracketed exprs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def parens
  (complex-m
   [_ (token? :bracket "(")
    e (p/alt (token? :op) expr)
    _ (token? :bracket ")")]
   e))

(def unit (p/alt (token? #{:name :str :char :num})
                 parens vec-lit fn-lit))


;;; function and set application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def application
  (p/semantics
   (p/rep+ unit)

   #(let [e (first %)
          r (rest %)]
      (if (seq r)
        (make-node :apply e
                   (vec
                    (if (= :apply (:type e))
                      (concat (:val e) r)
                      (cons e r))))
        e))))


;;; infix operations
;;;;;;;;;;;;;;;;;;;;

(defn right-assoc [pairs e1]
  (let [[o e2] (first pairs)
        r (rest pairs)
        e2 (if (seq r)
             (right-assoc r e2)
             e2)]
    (make-node :apply e1 [o e1 e2])))

(defmacro defop-right
  "Makes right associative infix parse rule."
  [op-name op-str higher-rule]
  `(def ~op-name
     (complex-m
      [e# ~higher-rule
       s# (p/rep* (p/conc (token? :op ~op-str) ~higher-rule))]
      (if (seq s#)
        (right-assoc s# e#)
        e#))))


(defn left-assoc [reversed-pairs e2 pos-node]
  (let [[e1 o] (first reversed-pairs)
        r (rest reversed-pairs)
        e1 (if (seq r)
             (left-assoc r e1 pos-node)
             e1)]
    (make-node :apply pos-node [o e1 e2])))

(defmacro defop
  "Makes left associative infix parse rule. op-strs can be a str or coll of strs."
  [op-name op-strs higher-rule]
  `(def ~op-name
     (alt-m
      (complex-m
       [s# (p/rep+ (p/conc ~higher-rule (token? :op ~op-strs)))
        :let [pos-node# (-> s# first first)]
        e# (p/failpoint ~higher-rule
                        (failpoint-error-fn
                         (str "Invalid infix op \"" (-> s# last second :val) "\"")
                         pos-node#))]
       (left-assoc (reverse s#) e# pos-node#))
      ~higher-rule)))


;; op-+     is higher than op-range so that  (a+1 .. b)  works
;; op-range is higher than op-cons  so that  (a . 1..10) works
;; op-+     is higher than op-cons  so that  (a+b . s)   works
;; op-cons  is higher than op-++    so that  (a.b.[] ++ [c,d]) is concat of two vecs
;; op-++    is higher than op-|     so that  (a++b) can be unioned with other fns

;; op-++ is right-associative to make backtracking easier

(def eq-ops #{"=" "/=" "<=" ">=" "<" ">" "<<" ">>" "<-"})

(defop        op-*       #{"*" "/"}    application)
(defop        op-+       #{"+" "-"}    op-*)
(defop        op-range   #{".." "..."} op-+)
(defop-right  op-cons    "."           op-range)
(defop-right  op-++      "++"          op-cons)
(defop        op---      "--"          op-++)
(defop        op-&       "&"           op---)
(defop        op-|       "|"           op-&)
(defop        op-=       eq-ops        op-|)
(defop        op-&&      "&&"          op-=)
(defop        op-||      "||"          op-&&)

(def expr op-||)


;;; parse
;;;;;;;;;

(defn parse1
  [src-str]
  (let [e (fn [state]
            (error "Failure to parse"
                   (first (:remainder state))))]
    (map (fn [token-group]
           (clear-rule-memos!)
           (p/rule-match expr
                         (fn [s] (e s))
                         (fn [_ s] (e s))
                         {:remainder token-group}))
         (shatter (tokenize src-str)))))

;; TODO: allow missing left expr on embedded <-
;; TODO: detect embedded = and <- (not top level of such that, not inside ||, &&, or not)

(def parse (insta/parser (clojure.java.io/resource "tl.bnf")))

