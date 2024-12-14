;advent-of-code-2024.day-14
(ns day14
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (slurp input)
       str/split-lines
       (map #(map read-string (re-seq #"-?\d+" %)))))

(defn step [x vx w]
  (mod (+ x vx) w))

(defn step-vh [[h w] [x y vx vy]]
  [(step x vx w) (step y vy h) vx vy])

;part 1
(let [[h w] [103 101] ;[7 11]
      [hh hw] [(quot h 2) (quot w 2)]
      step1 (partial step-vh [h w])
      step100 #(nth (iterate step1 %) 100)
      quad #(vector (< (first %) hw) (< (second %) hh))]
  (->> (parse "input")
       (map step100) ;run 100 times
       (remove #(or (= (first %) hw) (= (second %) hh))) ;remove middle row and middle col
       (group-by quad) ;get 4 quads
       (map #(count (val %)))
       (reduce *)
       println))

;part 2
(defn myprint "print to see the tree" [vs] 
  (->> vs
       (map #(vec (take 2 %)))
       set
       (#(doseq [y (range 103)]
           (doseq [x (range 101)]
             (print (if (% [x y]) \1 \.)))
           (println)))))

(defn check?
  "find the longest consecutive robots, and compare with 10"
  [vs]
  (->> vs
       (group-by first) ;group by same x
       (apply max-key #(count (val %))) ;get the row with most robots
       second
       (map second) ;get ys
       sort
       (map-indexed (fn [idx n] [n (- n idx)]))
       (partition-by second) ;consecutive ones in same group
       (map count)
       (apply max)
       (< 10)))

(let [[h w] [103 101] ;[7 11]
      [hh hw] [(quot h 2) (quot w 2)]
      step1 (partial step-vh [h w])
      start (parse "input")]
  (loop [curr start n 0]
    (if (check? curr)
      (do (println n)
          (myprint curr))
      (recur (map step1 curr) (inc n)))))