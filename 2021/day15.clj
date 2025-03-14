;advent-of-code-2021.day15
(ns day15
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (mapv #(mapv read-string (str/split % #"")))))

(def Map (parse "2021/in15"))
(def w (count (first Map)))
(def h (count Map))
(def start [0 0])
(def end [(dec h) (dec w)])
(def dirs [[0 1] [0 -1] [1 0] [-1 0]])

;part 1
(defn dijkstra []
  (loop [todo (priority-map start 0)
         visited #{}]
    (let [[pos score] (peek todo)
          nxt (pop todo)
          new (->> (map #(mapv + pos %) dirs)
                   (remove (fn [p]
                             (or (visited p)
                                 (nil? (get-in Map p)))))
                   (map #(vector % (min (get todo % Double/POSITIVE_INFINITY) (+ score (get-in Map %))))))]
      (if (= pos end)
        score
        (recur (into nxt new) (conj visited pos))))))

(dijkstra)

;part 2
(def end2 [(dec (* 5 h)) (dec (* 5 w))])

(defn in? [[y x]]
  (and (<= 0 y (dec (* 5 h)))
       (<= 0 x (dec (* 5 w)))))

(defn get-in2 [[y x]]
  (let [y2 (quot y h)
        x2 (quot x w)
        y3 (mod y h)
        x3 (mod x w)
        v (+ (get-in Map [y3 x3]) x2 y2)]
    (if (> v 9) (- v 9) v)))

(defn dijkstra2 []
  (loop [todo (priority-map start 0)
         visited #{}]
    (let [[pos score] (peek todo)
          nxt (pop todo)
          new (->> (map #(mapv + pos %) dirs)
                   (remove (fn [p]
                             (or (visited p)
                                 (not (in? p)))))
                   (map #(vector % (min (get todo % Double/POSITIVE_INFINITY) (+ score (get-in2 %))))))]
      (if (= pos end2)
        score
        (recur (into nxt new) (conj visited pos))))))

(dijkstra2)