(ns crosswords.utils.io
  (:require [clojure.java.io :as io]))

(defn file-or-resource
  [s]
  (or (let [f (io/file s)] (when (.exists f) f))
      (io/resource s)
      (throw (Exception. (str "File or resource not found: " s)))))
