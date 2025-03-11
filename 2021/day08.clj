;advent-of-code-2021.day08
(ns day08
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map (fn [line]
              (->> (str/split line #" \| ")
                   (map (fn [part]
                          (->> (str/split part #" ")
                               (map set)))))))))

(def data (parse "2021/in08"))

;part 1
(->> data
     (mapcat second)
     (filter #(#{2 3 4 7} (count %)))
     count)

;part 2
(defn cal [[ten target]]
  (let [nums (atom {})
        groups (group-by count ten)]
    (swap! nums assoc 1 (first (groups 2)))
    (swap! nums assoc 4 (first (groups 4)))
    (swap! nums assoc 7 (first (groups 3)))
    (swap! nums assoc 8 (first (groups 7))) 
    (->> (groups 6)
         (filter #(set/subset? (@nums 4) %))
         first
         (swap! nums assoc 9))
    (->> (groups 6)
         (remove #(set/subset? (@nums 1) %))
         first
         (swap! nums assoc 6))
    (->> (groups 6)
         (remove #{(@nums 6) (@nums 9)})
         first
         (swap! nums assoc 0))
    (->> (groups 5)
         (remove #(set/subset? (set/difference (@nums 8) (@nums 6)) %))
         first
         (swap! nums assoc 5))
    (->> (groups 5)
         (filter #(set/subset? (set/difference (@nums 8) (@nums 9)) %))
         first
         (swap! nums assoc 2))
    (->> (groups 5)
         (remove #{(@nums 2) (@nums 5)})
         first
         (swap! nums assoc 3))
    
    (->> target
         (map #(* %1 ((set/map-invert @nums) %2)) [1000 100 10 1])
         (reduce +))))

(->> data
     (map cal)
     (reduce +))