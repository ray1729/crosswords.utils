(ns crosswords.utils.grid
  (:require [clojure.string :as str]
            [crosswords.utils.io :refer [file-or-resource]]
            [clojure.java.io :as io]))

(defn black? [c] (= c \$))

(defn unfilled? [c] (= c \.))

(defn parse-grid
  [s]
  (mapv vec (str/split-lines s)))

(defn read-grid
  [s]
  (parse-grid (slurp (file-or-resource s))))

(defn get-cell
  [grid row col]
  (get-in grid [row col] \$))

(defn get-cells
  [grid coords]
  (map (fn [[row col]] (get-cell grid row col)) coords))

(defn word-groups
  "Returns a list of lists of coordinates of words in the grid."
  [grid]
  (mapcat (fn [[row col]]
                 (cond-> []
                   (and (black? (get-cell grid row (dec col)))
                        (not (black? (get-cell grid row col)))
                        (not (black? (get-cell grid row (inc col)))))
                   (conj (map #(vector row %) (take-while #(not (black? (get-cell grid row %))) (iterate inc col))))

                   (and (black? (get-cell grid (dec row) col))
                        (not (black? (get-cell grid row col)))
                        (not (black?  (get-cell grid (inc row) col))))
                   (conj (map #(vector % col) (take-while #(not (black? (get-cell grid % col))) (iterate inc row))))))
               (for [row (range (count grid)) col (range (count (first grid)))] [row col])))

(defn complete?
  [grid coords]
  (not-any? unfilled? (get-cells grid coords)))

#_(defn words-to-complete
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
  (into []
        (comp
         (filter (memfn isFile))
         (map read-grid))
        (file-seq (io/file dir))))
