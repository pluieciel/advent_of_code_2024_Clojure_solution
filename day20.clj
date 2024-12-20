;advent-of-code-2024.day-20
(ns day20
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (slurp input) str/split-lines (mapv vec)))

(def dirs [[0 1] [1 0] [0 -1] [-1 0]])
(def Map (parse "input"))
(def w (count (first Map)))
(def h (count Map))

(defn cal "BFS" [start end] 
  (loop [todo {start 0} done {}]
    (if (empty? todo)
      done
      (let [[pos score] (first todo)
            newposs (for [[y x :as npos] (map #(mapv + pos %) dirs)
                          :when (and (<= 0 x w)
                                     (<= 0 y h)
                                     (not (get done npos))
                                     (not (get todo npos))
                                     (not= \# (get-in Map npos)))]
                      [npos (inc score)])]
        (recur (into (dissoc todo pos) newposs) (assoc done pos score))))))

(let [{start \S end \E} (->> (for [x (range (count (first Map))) y (range (count Map))
                                   :when (#{\S \E} (get-in Map [y x]))]
                               {(get-in Map [y x]) [y x]})
                             (apply merge))
      Scores (cal start end)]

  ;part 1
  (->> (for [x (range w) y (range h) :when (= \# (get-in Map [y x]))]
         (->> (map #(mapv + [y x] %) dirs)
              (keep #(get Scores %))
              (#(if (empty? %)
                  0
                  (- (apply max %) (apply min %) 2)))))
       (filter #(>= % 100))
       count
       println)

  ;part 2
  (->> (for [[[y x :as pos] score] Scores]
         (for [dx (range -20 21) dy (range -20 21)
               :when (let [cost (+ (if (pos? dx) dx (- dx)) (if (pos? dy) dy (- dy)))
                           npos [(+ dy y) (+ dx x)]]
                       (and (<= cost 20) 
                            (>= (- (get Scores npos 0) score cost) 100)))]
           1))
       flatten
       (reduce +)
       println))