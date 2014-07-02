(ns timeless.parser
  (:require [instaparse.core :as insta]))


(def tl-parser (insta/parser (clojure.java.io/resource "tl.bnf")))

;; TODO:

;; write common fn that finds line and col from the start and end pos's so that checks can give good error messages

;; check for stdout

;; :or-no-eq :and-no-eq :compare-no-eq go to :or :and :compare

;; :compare is broken up into conjunction of compares with gensyms

;; :unsigned-num and :signed-num both become :num and the string num is evaluated

;; make key gensym for every set or fn clause; add to the such that
;; make val gensym for every fn clause; add to the such that

;; replace correctly embedded = and <- (unary or binary) with gensyms; add to the such that

;; error msg for unary member in the wrong place

;; associate with every set or fn node a coll of all the names bound outside that set or fn

;; check that a top-level name isn't bound twice and that it isn't reserved

