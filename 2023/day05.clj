;advent-of-code-2023.day05
(ns day05
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [input]
  (->> (slurp input)
       (#(str/split % #"\n\n"))))

(let [[seeds & maps] (parse "./2023/in05")
      seeds (->> seeds (#(str/split % #" ")) rest (map read-string))
      maps (->> maps 
                (map (fn [one]
                       (->> (str/split one #"\n")
                            rest
                            (map (fn [line]
                                   (->> (str/split line #" ")
                                        (map read-string))))))))]
  
  (def cal
    (memoize
     (fn [seed]
       (reduce
        (fn [curr ms]
          (let [target (filter #(let [[_ ss len] %] (<= ss curr (+ ss (dec len)))) ms)]
            (if (empty? target)
              curr
              (let [[ds ss _] (first target)]
                (+ ds (- curr ss))))))
        seed maps))))
  
  (->> (for [seed (->> seeds (partition 2) (mapcat #(let [[s l] %] (range s (+ s l)))))]
         (cal seed))
       (apply min))
  
  (->> (for [seed seeds]
         (reduce
          (fn [curr ms]
            (let [target (filter #(let [[_ ss len] %] (<= ss curr (+ ss (dec len)))) ms)]
              (if (empty? target)
                curr
                (let [[ds ss _] (first target)]
                  (+ ds (- curr ss))))))
          seed maps))
       (apply min)))