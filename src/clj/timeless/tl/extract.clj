(ns timeless.tl.extract
  "Extract declarations from TL source."
  (:require [clojure.string :as str]))


;;; Extract declarations and get included files.

(def declare-tokens #{"#name" "#op" "#opr" "#opl"})

(defn split-source-line [line]
  (remove str/blank?
          (str/split (str/trim line)
                 #"[ \t]")))

;; Returns: [<declaration> <annotated includes>]
(defn classify-line [index line]
  (let [tokens (split-source-line line)
        first-token (first tokens)]
    (cond (declare-tokens first-token)
          [(apply list :declare tokens)
           nil]
 
          (= "#include" first-token)
          [nil
           {:line-num (inc index)
            :paths (rest tokens)}]

          :default
          [nil nil])))

;; Returns:
;; [<declarations>
;;  <annotated includes in the form {:line-num <> :paths <>}>]
(defn extract-declarations [source]
  (let [;; Add a space to ensure the source has at least one non-newline character,
        ;; otherwise str/split-lines will return nil.
        lines (str/split-lines (str source " "))
        classified-lines (map-indexed classify-line lines)]

    (map (partial remove nil?)
         ;; This works because str/split-lines will always generate at least one line.
         (apply map list classified-lines))))
