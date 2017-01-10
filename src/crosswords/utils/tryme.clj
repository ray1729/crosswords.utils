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

(defn grid-words
  [grid]
  (concat
   (mapcat (partial get-words-in-row m) row-names)
   (mapcat (partial get-words-in-col m) col-names)))

(defn word-fits?
  [grid word cells]
  (and (= (count word) (count cells))
       (every? true? (map (fn [char [col row]]
                            (let [v (get-in grid [col row])]
                              (or (nil? v) (= v char))))
                          word cells))))

(defn assign-word-to-grid
  [grid word]
  (map (fn [cells]
         (reduce (fn [grid [col row char]]
                   (assoc-in grid [col row] char))
                 grid
                 (map (fn [cell char] (conj cell char)) cells word)))
       (filter (partial word-fits? grid word)(grid-words grid))))

(defn next-filled-grid
  [stack]
  (loop [stack stack]
    (if (empty? stack)
      nil
      (let [{:keys [word-groups grid]} (peek stack)]
        (if (empty? word-groups)
          [grid (pop stack)]
          (let [[w & ws] word-groups]
            (recur (into (pop stack)
                         (map (fn [grid]
                                {:grid grid :word-groups ws})
                              (assign-word-to-grid grid w))))))))))

(defn fill-grid
  [{:keys [word-groups grid] :as candidate}]
  (letfn [(step [stack]
            (lazy-seq
             (if-let [[grid stack] (next-filled-grid stack)]
               (cons grid (step stack)))))]
    (step [(update candidate :grid grid->map)])))

#_(defn fill-grid
  [{:keys [word-groups grid] :as candidate}]
  (loop [stack [candidate] accum []]
    (if (empty? stack)
      accum
      (let [{:keys [word-groups grid]} (peek stack)]
        (if (empty? word-groups)
          (recur (pop stack) (conj accum grid))
          (let [[w & ws] word-groups
                next-grids (assign-word-to-grid grid w)]
            (if (empty? next-grids)
              (recur (pop stack) accum)
              (recur (into (pop stack)
                           (map (fn [grid] {:grid grid :word-groups ws}))
                           next-grids)
                     accum))))))))



(comment
  (def candidate (first candidates))
  (def w (first (:word-groups candidate)))
  (word-fits? (:grid candidate) w (nth (grid-words grid) 1))
  (assign-word-to-grid (grid->map (:grid candidate)) w)


  (first (fill-grid (update candidate :grid grid->map)))

  )
