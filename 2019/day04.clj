;advent-of-code-2019.day04
(ns day04)

(let [[s e] (->> (slurp "2019/in04")
                 (re-seq #"\d+")
                 (map read-string))]
  (def lower s)
  (def upper e))

;; part1
(->> (for [n (range lower (inc upper))
           :let [lst (->> n str (map #(- (int %) (int \0))))]
           :when (and (apply <= lst)
                      (->> lst
                           (partition 2 1)
                           (some #(apply = %))))]
       n)
     count)

;; part2
(->> (for [n (range lower (inc upper))
           :let [lst (->> n str (map #(- (int %) (int \0))))]
           :when (and (apply <= lst)
                      (->> lst
                           (partition-by identity)
                           (some #(= 2 (count %)))))]
       n)
     count)