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
(def s-dirs {\> [0 1] \< [0 -1] \v [1 0] \^ [-1 0]})
(def start [0 1])
(def end [(dec h) (- w 2)])
(def state (->> (for [r (range 1 (dec h))
                      c (range 1 (dec w))
                      :when (not= \. (get-in M [r c]))]
                  {(get-in M [r c]) [r c]})
                (apply merge-with conj {\> #{} \v #{} \< #{} \^ #{}})))

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

(defn cal [start end state]
  (loop [poss [start] state state steps 1]
    (let [newstate (next-state state)
          candis (->> (mapcat (fn [pos]
                                (->> (map (fn [dir] (mapv + pos dir)) dirs)
                                     (remove (fn [pos] (or (= \# (get-in M pos))
                                                           (neg? (first pos))
                                                           (= (first pos) h))))))
                              poss)
                      (reduce conj poss)
                      distinct)
          newposs (remove (fn [candi] (some #(% candi) (vals newstate))) candis)]
      (if (some #(= end %) newposs)
        [steps newstate]
        (recur newposs newstate (inc steps))))))

;part 1
(first (cal start end state))

;part 2
(let [[A stateA] (cal start end state)
      [B stateB] (cal end start stateA)
      [C _] (cal start end stateB)]
  (+ A B C))