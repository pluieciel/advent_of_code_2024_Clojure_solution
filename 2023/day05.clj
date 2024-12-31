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
  (->> (for [seed seeds]
         (reduce
          (fn [curr ms]
            (reduce
             (fn [curr m]
               (let [[ds ss len] m]
                 (if (<= ss curr (+ ss (dec len)))
                   (+ ds (- curr ss))
                   curr)))
             curr ms))
         seed maps))
       (apply min)))