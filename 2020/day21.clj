;advent-of-code-2020.day21
(ns day21
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map (fn [l]
              (->> (str/split l #"\(contains")
                   (map #(re-seq #"\w+" %)))))))

(def ins-als (parse "2020/in21"))

(defn step [m]
  (let [done (->> (vals m)
                  (filter #(= 1 (count %)))
                  (apply set/union))]
    (update-vals m #(if (= 1 (count %)) % (set/difference % done)))))

(def al->in
  (->> ins-als
       (map (fn [[ins als]]
              (->> (mapcat #(vector % (set ins)) als)
                   (apply hash-map))))
       (apply merge-with set/intersection)
       (iterate step)
       (partition 2 1)
       (drop-while #(apply not= %))
       ffirst))

;;part 1
(->> ins-als
     (mapcat first)
     (remove (->> al->in
                  vals
                  (apply set/union)))
     count)

;;part 2
(->> al->in
     (sort-by first)
     (map #(->> % second first))
     (str/join ","))