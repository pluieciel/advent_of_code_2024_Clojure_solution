;advent-of-code-2022.day24
(ns day24
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (mapv vec)))

(def M (parse "2022/in24"))
(def h (count M))
(def w (count (first M)))
(def dirs [[0 1] [0 -1] [1 0] [-1 0]])
(def s-dirs {\> [0 1]
             \< [0 -1]
             \v [1 0]
             \^ [-1 0]})

(defn next-state [state]
  (->> state
       (map (fn [[k v]]
              [k
               (set
                (map (fn [pos]
                       (let [[nr nc] (map + (s-dirs k) pos)
                             nr (cond (= nr 0) (- h 2) (= nr (dec h)) 1 :else nr)
                             nc (cond (= nc 0) (- w 2) (= nc (dec w)) 1 :else nc)]
                         [nr nc]))
                     v))]))
       (into {})))

(let [start [0 1]
      end [(dec h) (- w 2)]
      state (->> (for [r (range 1 (dec h))
                       c (range 1 (dec w))
                       :when (not= \. (get-in M [r c]))]
                   {(get-in M [r c]) [r c]})
                 (apply merge-with conj {\> #{} \v #{} \< #{} \^ #{}}))]
  (loop [poss [start] state state steps 0]
    (let [newstate (next-state state)
          candis (->> (mapcat (fn [pos]
                                (->> (map (fn [dir] (mapv + pos dir)) dirs)
                                     (remove (fn [pos] (or (= \# (get-in M pos))
                                                           (neg? (first pos)))))))
                              poss)
                      (reduce conj poss)
                      distinct)
          newposs (remove (fn [candi] (some #(% candi) (vals newstate))) candis)]
      (if (some #(= end %) newposs)
        (+ steps 1)
        (recur newposs newstate (inc steps))))))