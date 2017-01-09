(ns crosswords.utils.grid
  (:require [clojure.string :as str]
            [crosswords.utils.io :refer [file-or-resource]]
            [clojure.java.io :as io]))

(defn black? [c] (= c \$))

(defn unfilled? [c] (= c \.))

(defn parse-grid
  [s]
  (mapv vec (str/split-lines s)))

(defn transpose
  [xs]
  (apply (partial map vector) xs))

(defn row-word-groups
  "Extract the runs of two or more letters from a row."
  [row]
  (into []
        (comp
         (remove (comp black? first))
         (remove #(<= (count %) 2))
         (map vec))
        (partition-by black? row)))

(defn word-groups
  [cells]
  (concat (mapcat row-word-groups cells)
          (mapcat row-word-groups (transpose cells))))

(defn read-grid
  [s]
  (parse-grid (slurp (file-or-resource s))))

(defn complete?
  [xs]
  (not-any? unfilled? xs))

(defn words-to-complete
  [cells]
  (remove complete? (word-groups cells)))

(defn grid->string
  [cells]
  (str/join \newline (map (partial str/join "") cells)))

(defn word-frequencies
  [cells]
  (frequencies (map count (word-groups cells))))

(defn read-grids
  [dir]
  (into {}
        (comp
         (filter (memfn isFile))
         (map (juxt (memfn getName) read-grid)))
        (file-seq (io/file dir))))

(comment

  (def xs (read-grids "dev-resources/grids"))

  (def x (first (vals xs)))

  (def freqs (frequencies (map word-frequencies (vals xs))))

  (clojure.pprint/pprint freqs)

  )
