;advent-of-code-2023.day14
(ns day14
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (mapv vec)))
;part 1
(defn calcol [col h]
  (->> (map vector col (range 1 (inc h)))
       (partition-by #(= \# (first %)))
       (remove #(= \# (ffirst %)))
       (map (fn [lst]
              (let [end (second (last lst))
                    n (count (filter #(= \O (first %)) lst))]
                (->> (range end (- end n) -1)
                     (reduce +)))))
       (reduce +)))

(defn cal [Map]
  (let [h (count Map)
        iMap (apply map vector (reverse Map))]
    (->> iMap
         (map #(calcol % h))
         (reduce +))))

(let [Map (parse "./2023/in14")]
  (cal Map))

;part 2
(def toleft #(> (int %1) (int %2)))
(def toright #(< (int %1) (int %2)))

(defn tilt [row sort-fn]
  (->> row
       (partition-by #(= \# %))
       (map #(sort sort-fn %))
       (apply concat)))

(defn north [Map]
  (let [iMap (apply map vector Map)]
    (->> iMap
         (mapv #(tilt % toleft))
         (apply map vector))))

(defn south [Map]
  (let [iMap (apply map vector Map)]
    (->> iMap
         (mapv #(tilt % toright))
         (apply map vector))))

(defn west [Map]
  (->> Map
       (mapv #(tilt % toleft))))

(defn east [Map]
  (->> Map
       (mapv #(tilt % toright))))

(defn oneround [Map]
  (->> Map
       north
       west
       south
       east))

(defn calcurr [Map]
  (let [h (count Map)
        m (reverse Map)]
    (->> m
         (map
          (fn [n row] (* n (count (filter #(= \O %) row))))
          (range 1 (inc h)))
         (reduce +))))

(let [Map (parse "./2023/in14")
      Maps (iterate oneround Map)]

  ;(map calcurr (take 200 (drop 200 Maps)))
  ;(+ (mod (- 1000000000 200) 36) 200)
  (calcurr (nth Maps 208)))