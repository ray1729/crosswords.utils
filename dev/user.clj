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

(defn try-find-grids
  [word-list]
  (let [words (read-words word-list)]
    (fg/assign-words words grids)))

(def dict (dict/read-dict "/usr/share/dict/words"))

(defn complete-grid
  [grid dict]
  (loop [frontier [{:grid grid :todo (remove (partial g/complete? grid) (g/word-groups grid))}]
         solutions []]
    (if (empty? frontier)
      solutions
      (let [{:keys [grid todo]} (peek frontier)]
        (if (empty? todo)
          (recur (pop frontier) (conj solutions grid))
          (let [coords (first todo)
                cells (g/get-cells grid coords)]
            (if (not-any? g/unfilled? cells)
              (if (dict/has? dict cells)
                (recur (conj (pop frontier) {:grid grid :todo (rest todo)}) solutions)
                (recur (pop frontier) solutions))
              (recur (into (pop frontier)
                           (map (fn [word] {:grid (fg/try-assign grid word coords) :todo (rest todo)}))
                           (dict/matches dict cells))
                     solutions))))))))

(defn complete-grid-lazy
  [grid dict]
  (letfn [(complete-grid [frontier]
            (when (seq frontier)
              (let [{:keys [grid todo]} (peek frontier)]
                (if (empty? todo)
                  (cons grid (lazy-seq (complete-grid (pop frontier))))
                  (let [coords (first todo)
                        cells (g/get-cells grid coords)]
                    (if (not-any? g/unfilled? cells)
                      (if (dict/has? dict cells)
                        (recur (conj (pop frontier) {:grid grid :todo (rest todo)}))
                        (recur (pop frontier)))
                      (recur (into (pop frontier)
                                   (map (fn [word] {:grid (fg/try-assign grid word coords)
                                                    :todo (rest todo)}))
                                   (dict/matches dict cells)))))))))]
    (complete-grid [{:grid grid :todo (remove (partial g/complete? grid) (g/word-groups grid))}])))

(comment
  (count (try-find-grids "ft16056.txt"))

  (def xs (mapcat #(complete-grid-lazy % dict)
                  (fg/assign-words (remove #{"BEHIND" "ACROBATIC" "APHID"} words)
                                   grids)))

  (run! (comp println println g/grid->string) xs)
  )
