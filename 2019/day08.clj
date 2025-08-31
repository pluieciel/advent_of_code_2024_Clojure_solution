;advent-of-code-2019.day08
(ns day08
  (:require [clojure.string :as str]))

(def W 25)
(def H 6)
(def data
  (->> (slurp "2019/in08")
       (partition (* W H))))

;; part1
(->> data
     (map frequencies)
     (apply min-key #(get % \0))
     (#(let [{one \1 two \2} %] (* one two))))

;; part2
(->> data
     (apply map list)
     (map (fn [lst] (some #(when (not= \2 %) %) lst)))
     (partition 25)
     (map #(-> (apply str %) (str/replace "0" " ")))
     ((fn [rows]
        (doseq [r rows]
          (println r)))))