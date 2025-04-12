;advent-of-code-2020.day07
(ns day07
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map
        (fn [line]
          (let [[root children] (str/split line #" bags contain ")
                lst (->> (re-seq #"\d \w+ \w+(?= bags?)" children)
                         (map (fn [pair]
                                (let [[n c] (str/split pair #" " 2)]
                                  {c (read-string n)})))
                         (apply merge))]
            {root lst})))
       (apply merge)))

(def data (parse "2020/in07"))

;part 1
(defn hold-shiny? [bag]
  (let [children (get data bag)]
    (if (nil? children)
      false
      (or (contains? children "shiny gold")
          (some (fn [child] (hold-shiny? child)) (keys children))))))

(->> data
     keys
     (filter #(hold-shiny? %))
     count)

;part 2
(defn total [bag]
  (let [children (get data bag)]
    (if (nil? children)
      0
      (+ (apply + (map (fn [[k v]] (* v (total k))) children))
         (apply + (vals children))))))

(total "shiny gold")