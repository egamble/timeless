(ns timeless.tl.convert
  "Convert TL code to TLS code."
  (:refer-clojure :exclude [replace])
  (:require [clojure.string :refer :all :exclude [reverse]]))

(def declare-tokens #{"#name" "#op" "#opa" "#opr" "#opl" "#pr<" "#pr=" "#pr>"})

(defn split-annotated-line [annotated-line]
  (split
   (trim (:line annotated-line))
   #"[ \t]"))

(defn classify-line [annotated-line]
  (let [first-token (first (split-annotated-line annotated-line))]
    (cond (= "#include" first-token) {:include-lines (list annotated-line)
                                      :source-lines (list (merge annotated-line {:line ""}))}
          (declare-tokens first-token) {:declare-lines (list annotated-line)
                                        :source-lines (list (merge annotated-line {:line ""}))}
          :default {:source-lines (list annotated-line)})))

(defn get-include [include-line]
  ;; TODO: throw useful error here with line number
  (let [path (second (split-annotated-line include-line))]
    [path (slurp path)]))

;; Returns {:declare-lines <annotated lines> :source-lines <annotated lines>}.
(defn tag-and-separate-lines [[path source]]
  (let [annotated-lines (mapv #(do
                                 {:file path
                                  :line %1
                                  :line-num %2})
                             (split-lines source)
                             (range))
        classified-lines (apply merge-with into
                                (map classify-line annotated-lines))
        classified-lines (apply merge-with into classified-lines
                                (map #(tag-and-separate-lines (get-include %))
                                     (:include-lines classified-lines)))]
    {:declare-lines (reverse (:declare-lines classified-lines))
     :source-lines (reverse (:source-lines classified-lines))}))

(defn tl->tls [in-path out-path]
  (let [source (slurp in-path)
        file-data (tag-and-separate-lines [in-path source])]
    (spit out-path
          (str (join "\n" (map :line (concat (:source-lines file-data)
                                             (:declare-lines file-data))))
               "\n"))))

(defn -main [in-file out-file]
  (tl->tls in-file out-file))
