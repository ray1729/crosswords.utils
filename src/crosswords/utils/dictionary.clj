(ns crosswords.utils.dictionary
  (:require [crosswords.utils.string :refer [normalize]]
            [crosswords.utils.io :refer [file-or-resource]]
            [clojure.java.io :as io]))

(defn read-dict
  [source]
  (with-open [r (io/reader (file-or-resource source))]
    (transduce (map normalize)
               (completing (fn [accum x]
                             (update accum (count x) (fnil conj #{}) x)))
               {}
               (line-seq r))))

(defn has?
  [dict word]
  (contains? (dict (count word)) (apply str word)))

(defn matches-pattern?
  [pattern word]
  (every? true? (map (fn [p c] (or (= p \.) (= p c))) pattern word)))

(defn matches
  [dict pattern]
  (filter (partial matches-pattern? pattern) (dict (count pattern))))
