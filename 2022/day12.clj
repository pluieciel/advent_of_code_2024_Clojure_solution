;advent-of-code-2022.day12
(ns day12
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (mapv vec)))

;part 1
(let [Map (parse "./2022/in12")
      h (count Map) w (count (first Map))
      {start \S end \E} (apply merge
                               (for [y (range h) x (range w)
                                     :let [mark (get-in Map [y x])]
                                     :when (#{\S \E} mark)]
                                 {mark [y x]}))
      dirs [[0 1] [0 -1] [1 0] [-1 0]]
      dict (fn [m] (case m \S 0 \E 25 (- (int m) 97)))]
  (loop [todo (priority-map start 0)
         visited #{}]
    (let [[pos score] (peek todo)
          nxt (pop todo)
          new (->> (map #(->> (mapv + pos %)) dirs)
                   (remove (fn [p]
                             (let [mark (get-in Map p)]
                               (or (nil? mark)
                                   (visited p)
                                   (> (- (dict (get-in Map p))
                                         (dict (get-in Map pos))) 1)))))
                   (map #(vector % (inc score))))]
      (if (= pos end)
        score
        (recur (into nxt new) (conj visited pos))))))

;part 2
(let [Map (parse "./2022/in12")
       h (count Map) w (count (first Map))
       {start \S end \E} (apply merge
                                (for [y (range h) x (range w)
                                      :let [mark (get-in Map [y x])]
                                      :when (#{\S \E} mark)]
                                  {mark [y x]}))
       dirs [[0 1] [0 -1] [1 0] [-1 0]]
       dict (fn [m] (case m \S 0 \E 25 (- (int m) 97)))]
  (loop [todo (priority-map end 0)
         visited #{}]
    (let [[pos score] (peek todo)
          nxt (pop todo)
          new (->> (map #(->> (mapv + pos %)) dirs)
                   (remove (fn [p]
                             (let [mark (get-in Map p)]
                               (or (nil? mark)
                                   (visited p)
                                   (< (- (dict (get-in Map p))
                                         (dict (get-in Map pos))) -1)))))
                   (map #(vector % (inc score))))]
      (if (#{\a \S} (get-in Map pos))
        score
        (recur (into nxt new) (conj visited pos))))))