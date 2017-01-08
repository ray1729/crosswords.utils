(ns crosswords.utils.fill-grid
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [crosswords.utils.grid :as grid]
            [crosswords.utils.string :refer [normalize]]))

(defn expand-combinations
  "Helper function for generating possbile word groups for a single
  phrase."
  [words]
  (cond
    (empty? words) nil
    (= (count words) 1) [words]
    :else (let [w (first words)
                xs (expand-combinations (rest words))]
            (mapcat (fn [[x & xs]]
                      [(cons (str w x) xs)
                       (cons w (cons x xs))])
                    xs))))

(defn phrase-combinations
  "If the words we are trying to fit into the grid include multi-word
  phrases, then we need to consider all possible ways of splitting the
  phrase into word groups. For example, `CAT OF NINE TAILS` could fit
  into a grid in 6 different ways, including `CATOFNINETAILS (14)` and
  `CATOFNINE TAILS (9,5). Given a word or phrase, this function retuns
  all possible ways of splitting it up."
  ([s]
   (phrase-combinations s 3))
  ([s min-word-len]
   (let [words (str/split (normalize s) #"\s")]
     (filter (fn [xs]
               (every? (comp (partial <= min-word-len) count) xs))
             (expand-combinations words)))))

(defn word-groups
  "Given a list of words or phrases, this function returns the
  cartesian product of all possbile ways of splitting each multi-word
  phrase into word groups."
  [words-or-phrases]
  (map (partial apply concat)
       (apply combo/cartesian-product
              (map phrase-combinations words-or-phrases))))

(defn viable-grid?
  "Returns true if the given grid can accommodate all of words. This
  function only checks the word length frequencies, it does not check
  that the words can actually be arranged in the grid."
  [grid words]
  (let [grid-freqs (grid/word-frequencies grid)
        word-freqs (frequencies (map count words))]
    (every? (fn [word-len word-count]
              (>= (get grid-freqs word-len 0) word-count))
            word-freqs)))

(defn candidate-grids
  "Returns a map of `:word-groups` and `:grid` for each of the grids
  that have compatible word length frequencies for some grouping of
  `words-or-phrases`."
  [words-or-phrases grids]
  (for [words (word-groups words-or-phrases)
        grid  grids
        :when (viable-grid? grid words)]
    {:word-groups words :grid grid}))
