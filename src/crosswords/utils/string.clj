(ns crosswords.utils.string
  (:require [clojure.string :as str])
  (:import [org.apache.commons.lang3 StringUtils]))

(defn strip-accents
  "Strip accents from a string."
  [s]
  (StringUtils/stripAccents s))

(defn normalize-whitespace
  "Replace runs of whitespace characters with a single space."
  [s]
  (str/replace s #"\s+" " "))

(defn strip-non-alphabetic
  [s]
  (str/replace s #"[^A-Z\s]" ""))

(def normalize (comp strip-non-alphabetic
                     str/upper-case
                     strip-accents
                     normalize-whitespace
                     str/trim))
