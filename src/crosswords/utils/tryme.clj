(ns crosswords.utils.tryme
  (:require [crosswords.utils.grid :as grid]
            [crosswords.utils.fill-grid :as fg]
            [crosswords.utils.scraper.guardian :as guardian]))


;; Guardian cryptic 27089
;; This happens to be dev-resources/grids/0020.txt
(def grid (grid/read-grid "grids/0020.txt"))

(def words ["sturdy" "bridge" "in the first place" "tehran" "clarinet"
            "tinder" "helpless" "levies" "achiever" "penchant"
            "satrap" "temperamentally" "treaty" "waited"
            "sincere" "defence" "usher" "retract" "dalliance"
            "excrete" "precis" "principle" "element" "sea fret"
            "asthma" "hosanna" "enabled" "toast"])

(def grids (grid/read-grids "dev-resources/grids"))

(def candidates (fg/candidate-grids words (vals grids)))

(def row-names (range 1 16))
(def col-names (map str "ABCDEFGHIJKLMNO"))

(def row-index (zipmap row-names (range)))
(def col-index (zipmap col-names (range)))

(defn grid->map
  [grid]
  (reduce (fn [m [col row]]
            (assoc-in m [col row] \$))
          {}
          (for [row row-names
                col col-names
                :when (grid/black? (get-in grid [(row-index row) (col-index col)]))]
            [col row])))

(def m (grid->map grid))

(defn get-words-in-row
  [m row]
  (map (fn [cols]
         (map (fn [col] [col row]) cols))
       (remove (fn [[col & cols]]
                 (or (empty? cols) (grid/black? (get-in m [col row]))))
               (partition-by (fn [col] (grid/black? (get-in m [col row]))) col-names))))

(defn get-words-in-col
  [m col]
  (map (fn [rows]
         (map (fn [row] [col row]) rows))
       (remove (fn [[row & rows]]
                 (or (empty? rows) (grid/black? (get-in m [col row]))))
               (partition-by (fn [row] (grid/black? (get-in m [col row]))) row-names))))

(defn word-groups
  [grid]
  (concat
   (mapcat (partial get-words-in-row m) row-names)
   (mapcat (partial get-words-in-col m) col-names)))
