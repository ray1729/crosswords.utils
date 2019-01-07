(ns user
  (:require [crosswords.utils.grid :as g]
            [crosswords.utils.fill-grid :as fg]
            [crosswords.utils.io :refer [file-or-resource]]
            [crosswords.utils.dictionary :as dict]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [medley.core :as medley]))

(defn read-words
  [s]
  (with-open [r (io/reader (file-or-resource s))]
    (vec (line-seq r))))

(def words (read-words "ft16056.txt"))

(def grids (g/read-grids "dev-resources/grids"))

(def dict (dict/read-dict "/usr/share/dict/words"))

(defn all-completions
  [word-list dict grids]
  (mapcat (partial fg/complete-grid dict)
          (fg/assign-words word-list grids)))

(comment

  (def xs (all-completions words dict grids))

  (g/print (first xs))

  )
