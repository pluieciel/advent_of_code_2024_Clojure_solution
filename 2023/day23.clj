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
      [end] (for [x (range w) :when (= \. (get-in Map [(dec h) x]))] [(dec h) x])
      ;dict (read-string (slurp "temp"))
      ;g_visited (atom #{})
      ;dict (atom {})
      dict
      (;;    [[0 1] {[7 15] 130}]
    ;;    [[7 15] {[29 13] 105, [17 33] 205}]
       [[0 1] {[29 13] 235, [17 33] 335}]
       [[7 101] {[19 89] 129, [39 101] 233, [41 125] 575}]
       [[17 33] {[7 15] 205, [41 35] 115, [19 65] 327}]
       [[19 65] {[17 33] 327, [31 65] 29, [19 89] 261}]
       [[19 89] {[19 65] 261, [39 79] 119, [7 101] 129}]
       [[29 13] {[7 15] 105, [67 11] 285, [41 35] 183}]
       [[31 65] {[41 35] 217, [19 65] 29, [65 61] 135, [39 79] 75}]
       [[39 79] {[31 65] 75, [19 89] 119, [53 77] 57, [39 101] 91}]
       [[39 101] {[39 79] 91, [7 101] 233, [57 103] 69, [41 125] 111}]
       [[41 35] {[29 13] 183, [17 33] 115, [57 33] 91, [31 65] 217}]
       [[41 125] {[7 101] 575, [39 101] 111, [65 135] 215}]
       [[53 77] {[65 61] 105, [39 79] 57, [77 79] 107, [57 103] 187}]
       [[57 33] {[67 11] 121, [41 35] 91, [89 33] 157, [65 61] 209}]
       [[57 103] {[53 77] 187, [39 101] 69, [81 103] 97, [65 135] 185}]
       [[65 61] {[57 33] 209, [31 65] 135, [85 65] 61, [53 77] 105}]
       [[65 135] {[41 125] 215, [57 103] 185, [79 133] 77}]
       [[67 11] {[29 13] 285, [87 19] 177, [57 33] 121}]
       [[77 79] {[85 65] 95, [53 77] 107, [113 79] 201, [81 103] 185}]
       [[79 133] {[65 135] 77, [81 103] 149, [101 127] 165}]
       [[81 103] {[77 79] 185, [57 103] 97, [111 111] 219, [79 133] 149}]
       [[85 65] {[89 33] 237, [65 61] 61, [105 59] 127, [77 79] 95}]
    ;;    [[87 19] {[67 11] 177, [113 19] 323, [89 33] 85}]
       [[87 19] {[113 19] 323, [89 33] 85}]
       [[89 33] {[87 19] 85, [57 33] 157, [111 39] 93, [85 65] 237}]
    ;;    [[101 127] {[79 133] 165, [111 111] 103, [127 135] 240}]
       [[101 127] {[111 111] 103, [127 135] 240}]
       [[105 59] {[111 39] 99, [85 65] 127, [131 59] 99, [113 79] 117}]
       [[111 39] {[113 19] 99, [89 33] 93, [127 39] 109, [105 59] 99}]
       [[111 111] {[113 79] 235, [81 103] 219, [137 109] 121, [101 127] 103}]
    ;;    [[113 19] {[87 19] 323, [127 39] 459, [111 39] 99}]
       [[113 19] {[127 39] 459, [111 39] 99}]
       [[113 79] {[105 59] 117, [77 79] 201, [127 81] 41, [111 111] 235}]
    ;;    [[127 39] {[113 19] 459, [111 39] 109, [131 59] 165}]
       [[127 39] {[111 39] 109, [131 59] 165}]
    ;;    [[127 81] {[131 59] 175, [113 79] 41, [137 109] 211}]
       [[127 81] {[113 79] 41, [137 109] 211}]
    ;;    [[127 135] {[137 109] 222, [101 127] 240}]
       [[127 135] {}]
    ;;    [[131 59] {[127 39] 165, [105 59] 99, [127 81] 175}]
       [[131 59] {[105 59] 99, [127 81] 175}]
    ;;    [[137 109] {[127 81] 211, [111 111] 121, [127 135] 222}]
       [[137 109] {[111 111] 121, [127 135] 222}])
      ]
;;   (defn getpart [from visited]
;;     (loop [pos from visited visited cnt 0 mem nil]
;;       (let [cand (->> (map #(mapv + pos %) dirs)
;;                       (remove visited)
;;                       (remove #(= \# (get-in Map % \#))))
;;             ]
;;         (if (or (= 1 (count cand)) (= pos end))
;;           (recur (first cand) (conj visited pos) (inc cnt) pos)
;;           [pos (map #(vector % #{pos}) cand) cnt mem]))))
;;   ;(getpart [7 16] #{[7 15]})
;;   (loop [todo [[start #{}]]]
;;     (when (not-empty todo)
;;       (let [[s v] (first todo)
;;             [e lst step last2] (getpart s v)]
;;         (println v e)
;;         (swap! g_visited conj last2)
;;         (cond
;;           (empty? v) (swap! dict assoc start {e (inc step)})
;;           (nil? e) (swap! dict assoc [127 135] (reduce-kv (fn [m k v] (assoc m k (+ step v))) {} (get @dict [127 135])))
;;           :else (let [beg (first v)]
;;                   (swap! dict assoc beg (assoc (get @dict beg {}) e (+ 2 step)))
;;                   (swap! dict assoc e (assoc (get @dict e {}) beg (+ 2 step)))))
;;         (recur (distinct (concat (rest todo) (remove #(or (contains? @g_visited (first %)) (empty? (first %))) lst)))))))
;;   (spit "temp" @dict)
  (->> dict
       (sort-by first)
       )
  (let [bitmap (apply merge (for [k (keys dict)] {k false}))]
    (loop [todo [[start 0 bitmap]]]
      (let [[[r c :as node] step bit] (first todo)
            newbit (assoc bit node true)
            cand (get dict node)]
        ;(println node step) (flush)
        (cond
          (= node [127 135]) (do (when (> step 6500) (println step)) (recur (rest todo)))
          (= 3 (count cand))
          (let [newc (cond
                       (< r 29) (->> (rest (sort-by #(second (first %)) cand))
                                     (remove (fn [[k v]]
                                               (get newbit k))))
                       (< c 33) (->> (rest (sort-by #(first (first %)) cand))
                                     (remove (fn [[k v]]
                                               (get newbit k))))
                       (> c 109) (->> (rest (sort-by #(first (first %)) cand))
                                      (remove (fn [[k v]]
                                                (get newbit k))))
                       (> r 113) (->> (rest (sort-by #(second (first %)) cand))
                                      (remove (fn [[k v]]
                                                (get newbit k)))))]
            ;(println newc) (flush)
            (recur (concat (rest todo) (map (fn [[k v]] [k (+ step v) newbit]) newc))))
          :else
          (let [newc (remove (fn [[k v]]
                               (get newbit k)) cand)]
            ;(println newc) (flush)
           (recur (concat (rest todo) (map (fn [[k v]] [k (+ step v) newbit]) newc))))
          ))))
  )



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