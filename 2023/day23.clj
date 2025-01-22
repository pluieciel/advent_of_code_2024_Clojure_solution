;advent-of-code-2023.day23
(ns day23
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (mapv vec)))

(def dirs [[1 0] [-1 0] [0 1] [0 -1]])

(let [Map (parse "./2023/in23")
      h (count Map) w (count (first Map))
      [start] (for [x (range w) :when (= \. (get-in Map [0 x]))] [0 x])
      [end] (for [x (range w) :when (= \. (get-in Map [(dec h) x]))] [(dec h) x])]
  (defn longest-path [pos visited]
    (if (= pos end)
      (let [temp (count visited)] (when (> temp 6500) (println temp)) 0)
      (let [new-visited (conj visited pos)
            candidates (->> (map #(mapv + pos %) dirs)
                            (remove new-visited)
                            (remove #(= \# (get-in Map % \#))))]
        (if (empty? candidates)
          -10000000
          (inc (apply max (map #(longest-path % new-visited) candidates)))))))
  (longest-path start #{}))



(let [Map (parse "./2023/in23")
      h (count Map) w (count (first Map))
      [start] (for [x (range w) :when (= \. (get-in Map [0 x]))] [0 x])
      [end] (for [x (range w) :when (= \. (get-in Map [(dec h) x]))] [(dec h) x])
      visited (atom #{})]
  (def dp
    (memoize
     (fn [[y x :as pos]]
       ;(println y x)
       (let [mark (get-in Map pos)]
         (swap! visited conj pos)
         (let [res (cond
                     (= pos start) 0
                     (= mark \#) -1000
                     (= mark \.) (let [up [(dec y) x]
                                       fup (if (and (not (@visited up)) (#{\. \v} (get-in Map up))) (inc (dp up)) -1000)
                                       down [(inc y) x]
                                       fdown (if (and (not (@visited down)) (#{\. \^} (get-in Map down))) (inc (dp down)) -1000)
                                       left [y (dec x)]
                                       fleft (if (and (not (@visited left)) (#{\. \>} (get-in Map left))) (inc (dp left)) -1000)
                                       right [y (inc x)]
                                       fright (if (and (not (@visited right)) (#{\. \<} (get-in Map right))) (inc (dp right)) -1000)]
                                   (max fup fdown fleft fright))
                     (= mark \>) (let [left [y (dec x)]
                                       fleft (if (and (not (@visited left)) (#{\. \>} (get-in Map left))) (inc (dp left)) -1000)]
                                   (max fleft))
                     (= mark \<) (let [right [y (inc x)]
                                       fright (if (and (not (@visited right)) (#{\. \<} (get-in Map right))) (inc (dp right)) -1000)]
                                   (max fright))
                     (= mark \v) (let [up [(dec y) x]
                                       fup (if (and (not (@visited up)) (#{\. \v} (get-in Map up))) (inc (dp up)) -1000)]
                                   (max fup))
                     (= mark \^) (let [down [(inc y) x]
                                       fdown (if (and (not (@visited down)) (#{\. \^} (get-in Map down))) (inc (dp down)) -1000)]
                                   (max fdown)))]
                                   ;(println pos res)
           (swap! visited disj pos)
           res)))))
  (dp end))