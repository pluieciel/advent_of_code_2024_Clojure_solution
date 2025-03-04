;advent-of-code-2020.day20
(ns day20
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (str/split (slurp i) #"\n\n")
       (map (fn [tile]
              (->> tile
                   str/split-lines
                   ((fn [[no & lines]]
                      [(read-string (re-find #"\d+" no)) (mapv vec lines)])))))
       (into {})))

(def data (parse "2020/in20"))

(defn get-bandaries [tile]
  (let [trans (apply map vector tile)]
    [(first tile) (last tile) (first trans) (last trans)]))

(let [get-bans (fn [data] (map (fn [[k v]] [k (get-bandaries v)]) data))
      match (fn [bandaries]
              (for [[k bs] bandaries]
                [k (for [b bs]
                     (conj (apply concat
                                  (for [[k2 bs2] bandaries :when (not= k2 k)]
                                    (for [[i b2] (map-indexed (fn [i v] [i v]) bs2)
                                          :let [nor (= b b2) rev (= b (reverse b2))]
                                          :when (or nor rev)]
                                      [k2 i nor]))) b))]))
      newdata (atom data)
      topleft (->> data get-bans match
                   (filter (fn [[k v]] (= [1 2 1 2] (map count v)))))
      dict (atom {[0 0] (ffirst topleft)})]
  
  ;part 1
  (->> data get-bans match
       (filter (fn [[k v]] (= 2 (count (filter (fn [one] (= 2 (count one))) v)))))
       (map first)
       (reduce *))
  
  ;part 2
  ;; do first col
  (loop [r 0 pre (get @dict [0 0])]
    (let [bandaries (->> (get-bans @newdata) (into {}))
          bottom (get (bandaries pre) 1)
          nxt (apply concat
                     (for [[k2 bs2] bandaries :when (not= k2 pre)]
                       (for [[i b2] (map-indexed (fn [i v] [i v]) bs2)
                             :let [nor (= bottom b2) rev (= bottom (reverse b2))]
                             :when (or nor rev)]
                         [k2 i nor])))]
      (when-not (empty? nxt)
        (let [[target idx normal] (first nxt)]
          (swap! dict assoc [(inc r) 0] target)
          (case [idx normal]
            [0 true] nil
            [0 false] (swap! newdata update target (fn [M] (mapv (fn [row] (vec (reverse row))) M)))
            [1 true] (swap! newdata update target (fn [M] (vec (reverse M))))
            [1 false] (swap! newdata update target (fn [M] (vec (reverse (map (fn [row] (vec (reverse row))) M)))))
            [2 true] (swap! newdata update target (fn [M] (vec (apply map vector M))))
            [2 false] (swap! newdata update target (fn [M] (vec (apply map vector (reverse M)))))
            [3 true] (swap! newdata update target (fn [M] (vec (reverse (apply map vector M)))))
            [3 false] (swap! newdata update target (fn [M] (vec (reverse (apply map vector (reverse M)))))))
          (recur (inc r) target)))))
  ;; fill whole table
  (doseq [row (range (count @dict))]
    (loop [c 0 pre (get @dict [row 0])]
      (let [bandaries (->> (get-bans @newdata) (into {}))
            right (get (bandaries pre) 3)
            nxt (apply concat
                       (for [[k2 bs2] bandaries :when (not= k2 pre)]
                         (for [[i b2] (map-indexed (fn [i v] [i v]) bs2)
                               :let [nor (= right b2) rev (= right (reverse b2))]
                               :when (or nor rev)]
                           [k2 i nor])))]
        (when-not (empty? nxt)
          (let [[target idx normal] (first nxt)]
            (swap! dict assoc [row (inc c)] target)
            (case [idx normal]
              [2 true] nil
              [2 false] (swap! newdata update target (fn [M] (vec (reverse M))))
              [3 true] (swap! newdata update target (fn [M] (mapv (fn [row] (vec (reverse row))) M)))
              [3 false] (swap! newdata update target (fn [M] (vec (reverse (map (fn [row] (vec (reverse row))) M)))))
              [0 true] (swap! newdata update target (fn [M] (vec (apply map vector M))))
              [0 false] (swap! newdata update target (fn [M] (vec (reverse (apply map vector M)))))
              [1 true] (swap! newdata update target (fn [M] (vec (apply map vector (reverse M)))))
              [1 false] (swap! newdata update target (fn [M] (vec (reverse (apply map vector (reverse M)))))))
            (recur (inc c) target))))))
  ;; build 2d map
  (let [len (->> @dict keys (map first) (apply max) inc)
        final (atom (vec (repeat (* 8 len) (vec (repeat (* 8 len) nil)))))]
    (doseq [row (range len)]
      (doseq [col (range len)]
        (let [doing (@dict [row col])
              M (@newdata doing)]
          (doseq [r (range 8)]
            (doseq [c (range 8)]
              (swap! final assoc-in [(+ (* row 8) r) (+ (* col 8) c)] (get-in M [(inc r) (inc c)])))))))
    (def finish @final))
  
  (def monster [[0 18]
                [1 0] [1 5] [1 6] [1 11] [1 12] [1 17] [1 18] [1 19]
                [2 1] [2 4] [2 7] [2 10] [2 13] [2 16]])
  
  (defn check [M [r c]]
    (when (every? #(= \# (get-in M (mapv + [r c] %))) monster) 1))
  
  (defn rot [fin]
    ((juxt identity
           (fn [M] (vec (reverse M)))
           (fn [M] (mapv (fn [row] (vec (reverse row))) M))
           (fn [M] (vec (reverse (map (fn [row] (vec (reverse row))) M))))
           (fn [M] (vec (apply map vector M)))
           (fn [M] (vec (reverse (apply map vector M))))
           (fn [M] (vec (apply map vector (reverse M))))
           (fn [M] (vec (reverse (apply map vector (reverse M)))))) fin))

  (let [len (count finish) w (- len 19) h (- len 2)
        num-mon (->> (for [M (rot finish)]
                       (count
                        (for [r (range h) c (range w) :when (check M [r c])] 1)))
                     (reduce +))
        tot-mon (* num-mon (count monster))]
    (->> finish
         flatten
         frequencies
         (#(get % \#))
         (#(- % tot-mon)))))