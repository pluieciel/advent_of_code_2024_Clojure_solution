;advent-of-code-2021.day20
(ns day20
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (str/split (slurp i) #"\n\n")
       ((fn [[algo img]]
          [(vec algo)
           (->> img
                str/split-lines
                (mapv vec))]))))

(let [[algo img] (parse "2021/in20")]
  (def algo algo)
  (def img img))

(def neighbors (for [dy [-1 0 1] dx [-1 0 1]] [dy dx]))

(defn expand [default img]
  (let [w (count (first img)) h (count img)]
    (vec
     (for [y (range (+ h 2))]
       (vec
        (for [x (range (+ w 2))]
          (->> (mapv #(mapv + % [(dec y) (dec x)]) neighbors)
               (map #(get-in img % default))
               (map {\. 0 \# 1})
               (apply str)
               (#(Integer/parseInt % 2))
               algo)))))))

(defn twice [img]
  (->> img
       ((partial expand \.))
       ((partial expand \#))))

(defn expand-times [n]
  (->> (iterate twice img)
       (#(nth % n))
       (apply concat)
       (filter #{\#})
       count))

;part 1
(expand-times 1)

;part 2
(expand-times 25)