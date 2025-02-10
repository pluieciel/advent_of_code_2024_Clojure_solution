;advent-of-code-2022.day16
(ns day16
  (:require [clojure.string :as str]
            [clojure.set :as set]
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
                             (if (or (zero? rate) (get res one))
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

;part 2
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
                             (if (or (zero? rate) (get res one))
                               acc
                               (assoc acc one [(inc score) rate])))) res nbs))))))
  (let [dists (->> (for [node (keys @dict)]
                     {node (BFS node)})
                   (into {}))
        ;best #{"QR" "OW" "AW" "SV" "PH" "LX" "IN"} 1741
        ;worst #{"HH" "HX" "FY" "BE" "RM"} 1083
        ]
    (defn cal [curr remain visited]
      (let [nbs (get dists curr)
            todo (remove #(or ;(best (key %))
                              (visited (key %))
                              (>= (inc (first (val %))) remain)) nbs)]
        (cond
          (or (<= remain 1) (empty? todo)) [[0 visited]]
          :else
          (concat
           (mapcat (fn [[node [dis rate]]]
                     (let [nrem (- remain dis 1)
                           temp (cal node nrem (conj visited node))]
                       (map (fn [[sc vi]] [(+ (* nrem rate) sc) vi])
                            temp)))
                    todo)
           (cal curr 0 visited)))))
    (->> (cal "AA" 26 #{})
         (map #(->> % reverse (apply hash-map)))
         (apply merge-with (fn [a b] (max a b)))
         (remove #(< (val %) 1083))
         (sort-by val >)
         (#(let [biggest (atom 0)]
             (loop [todo %]
               (if (or (< (second (first todo)) (/ @biggest 2)) (< (count todo) 2))
                 @biggest
                 (do
                   (let [[n1 s1] (first todo)]
                     (doseq [[n2 s2] (rest todo)]
                       (when (and (empty? (set/intersection n1 n2)) (> (+ s1 s2) @biggest))
                         (reset! biggest (+ s1 s2)))))
                   (recur (rest todo))))))))))