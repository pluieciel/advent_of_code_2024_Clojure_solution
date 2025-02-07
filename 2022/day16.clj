;advent-of-code-2022.day16
(ns day16
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map #(re-seq #"[A-Z]{2}|\d+" %))))
;part 1
(let [data (parse "./2022/in16")
      dict (atom {})]
  (doseq [[pos rate & dests] data]
    (swap! dict assoc pos {:rate (read-string rate) :dests dests}))
  (defn BFS [start]
    (loop [visited #{} todo (priority-map start 0) res {}]
      (if (empty? todo)
        res
        (let [[pos score] (peek todo)
              nxt (pop todo)
              nbs (->> (get-in @dict [pos :dests])
                       (remove visited))]
          (recur (conj visited pos)
                 (reduce (fn [acc one]
                           (assoc acc one (inc score))) nxt nbs)
                 (reduce (fn [acc one]
                           (let [rate (get-in @dict [one :rate])]
                             (if (zero? rate)
                               acc
                               (assoc acc one [(inc score) rate])))) res nbs))))))
  (let [dists (->> (for [node (keys @dict)]
                     {node (BFS node)})
                   (into {}))]
    (defn cal [curr remain visited]
      (let [nbs (get dists curr)
            todo (remove #(or (visited (key %)) (>= (inc (first (val %))) remain)) nbs)]
        (cond
          (or (<= remain 1) (empty? todo)) 0
          :else
          (apply max
                 (map (fn [[node [dis rate]]]
                        (let [nrem (- remain dis 1)]
                          (+ (* nrem rate) (cal node nrem (conj visited node))))) todo)))))
    (cal "AA" 30 #{})))