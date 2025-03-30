;advent-of-code-2021.day21
(ns day21
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map #(read-string (re-find #"\d$" %)))))

(let [[p1 p2] (parse "2021/in21")] (def p1 p1) (def p2 p2))

(def dice3
  (->> (for [a (range 1 4) b (range 1 4) c (range 1 4)]
         (+ a b c))
       frequencies))

;part 1
(loop [pos1 p1 pos2 p2 score1 0 score2 0 turn 0 dice (cycle (range 1 101))]
  (if (or (>= score1 1000) (>= score2 1000))
    (* turn 3 (min score1 score2))
    (let [roll (->> dice (take 3) (reduce +))]
      (if (even? turn)
        (let [newpos1 (mod (+ pos1 roll) 10)
              newpos1 (if (zero? newpos1) 10 newpos1)]
          (recur newpos1 pos2 (+ score1 newpos1) score2 (inc turn) (drop 3 dice)))
        (let [newpos2 (mod (+ pos2 roll) 10)
              newpos2 (if (zero? newpos2) 10 newpos2)]
          (recur pos1 newpos2 score1 (+ score2 newpos2) (inc turn) (drop 3 dice)))))))

;part 2
(def done (atom {:p1 0 :p2 0}))

(loop [todo {[p1 p2 0 0] 1} turn 0]
  (if (not-empty todo)
    (let [even (if (even? turn) 0 1)]
      (recur
       (->> (for [[[p1 p2 s1 s2] n] todo]
              (for [[roll nroll] dice3
                    :let [newpos1 (mod (+ ([p1 p2] even) roll) 10)
                          newpos1 (if (zero? newpos1) 10 newpos1)
                          news1 (+ ([s1 s2] even) newpos1)]
                    :when (let [finish (>= news1 21)]
                            (when finish
                              (swap! done update ([:p1 :p2] even) #(+ % (* n nroll))))
                            (not finish))]
                {([[newpos1 p2 news1 s2] [p1 newpos1 s1 news1]] even) (* n nroll)}))
            (apply concat)
            (apply merge-with +))
       (inc turn)))
    @done))