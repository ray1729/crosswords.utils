(ns crosswords.utils.scraper.guardian
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [crosswords.utils.grid :as grid]
            [net.cgrand.enlive-html :as html])
  (:import java.net.URL))

(defn crossword-url
  "Return the URL for the accessible version of Guardian cryptic crossword `n`."
  [n]
  (URL.
   (str "https://www.theguardian.com/crosswords/accessible/cryptic/" n)))

(def crossword
  "Return the accessible version of Guardian cryptic crossword `n` as
  an Enlive HTML resource."
  (memoize
   (fn [n]
     (Thread/sleep (rand-int 10000))
     (html/html-resource (crossword-url n)))))

(defn parse-row
  [row]
  (let [[_ row-num blanks] (re-find #"^Line\s+(\d+)\s*:\s*(.+)" row)]
    [(Long/parseLong row-num)
     (zipmap (str/split blanks #"\s+") (repeat true))]))

(defn parse-grid
  [node]
  (into {}
        (comp
         (map html/text)
         (map parse-row))
        (html/select node [:ul.crossword__accessible-blank-data
                           :li.crossword__accessible-row-data])))

(defn grid->str
  [m]
  (str/join \newline
            (map (fn [row-num]
                   (reduce (fn [accum col-name]
                             (if (get-in m [row-num col-name])
                               (str accum "$")
                               (str accum ".")))
                           ""
                           (map str "ABCDEFGHIJKLMNO")))
                 (range 1 16))))

(defn str->grid
  [s]
  (let [cells (mapv vec (str/split-lines s))]
    (reduce (fn [m [r c]]
              (if (= \$ (get-in cells [r c]))
                (assoc-in m [(inc r) (nth "ABCDEFGHIJKLMNO" c)] true)
                m))
            {}
            (for [r (range 15) c (range 15)] [r c]))))

(defn get-grid
  [n]
  (println (str "Fetching grid " n))
  (try
    (->> (crossword n)
         (parse-grid)
         (grid->str))
    (catch Exception e
      (println (format "Error fetching grid %d: %s" n (.getMessage e))))))

#_(defn scrape-guardian-grids
  ([n c]
   (scrape-guardian-grids n c #{}))
  ([n c seen]
   (if (zero? c)
     seen
     (let [grid (get-grid n)]
       (if (or (not grid) (seen grid))
         (recur (dec n) c seen)
         (recur (dec n) (dec c) (conj seen grid)))))))

(def grids (atom #{}))

(defn scrape-guardian-grids
  [n c]
  (when-not (zero? c)
    (if-let [grid (get-grid n)]
      (do
        (swap! grids conj grid)
        (recur (dec n) (dec c)))
      (recur (dec n) c))))

(defn write-grids
  [grids dir]
  (loop [ix 1 grids (seq grids)]
    (when grids
      (spit (io/file dir (format "%04d.txt" ix)) (first grids))
      (recur (inc ix) (next grids)))))

(comment
  (write-grids @grids "/home/ray/Workspace/crosswords.utils/dev-resources/grids"))
