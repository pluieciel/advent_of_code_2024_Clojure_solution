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
      ;dict (atom {})
      dict {[126 135] [[102 127] 172], [20 89] [[38 79] 116], [85 66] [[77 78] 92], [112 39] [[126 39] 106], [57 102] [[53 78] 184], [128 135] [[140 139] 64], [39 78] [[31 66] 72], [87 20] [[89 32] 82], [42 35] [[56 33] 88], [39 102] [[41 124] 108], [38 79] [[20 89] 116], [81 104] [[79 132] 146], [66 61] [[84 65] 58], [65 60] [[57 34] 206], [130 59] [[106 59] 96], [86 19] [[68 11] 174], [40 125] [[7 102] 572], [86 65] [[104 59] 124], [67 12] [[57 32] 118], [68 11] [[86 19] 174], [104 59] [[86 65] 124], [111 38] [[113 20] 96], [80 103] [[58 103] 94], [137 108] [[127 82] 208], [106 59] [[130 59] 96], [56 103] [[40 101] 66], [101 126] [[111 112] 100], [88 33] [[58 33] 154], [113 80] [[111 110] 232], [32 65] [[64 61] 132], [66 135] [[78 133] 74], [102 127] [[126 135] 172], [131 60] [[127 80] 172], [20 65] [[30 65] 26], [7 100] [[19 90] 126], [89 34] [[85 64] 234], [57 32] [[67 12] 118], [110 39] [[90 33] 90], [53 78] [[57 102] 184], [111 110] [[113 80] 232], [40 79] [[52 77] 54], [131 58] [[127 40] 162], [81 102] [[77 80] 182], [40 101] [[56 103] 66], [127 38] [[114 19] 456], [111 40] [[105 58] 96], [19 90] [[7 100] 126], [38 101] [[8 101] 230], [19 88] [[19 66] 258], [80 133] [[100 127] 162], [136 109] [[112 111] 118], [112 111] [[136 109] 118], [127 40] [[131 58] 162], [105 58] [[111 40] 96], [19 66] [[19 88] 258], [113 78] [[105 60] 114], [140 139] [[128 135] 64], [54 77] [[76 79] 104], [137 110] [[127 134] 154], [19 64] [[17 34] 324], [31 64] [[41 36] 214], [52 77] [[40 79] 54], [100 127] [[80 133] 162], [111 112] [[101 126] 100], [77 80] [[81 102] 182], [41 34] [[29 14] 180], [31 66] [[39 78] 72], [89 32] [[87 20] 82], [126 81] [[114 79] 38], [8 15] [[28 13] 102], [39 80] [[39 100] 88], [57 104] [[65 134] 182], [56 33] [[42 35] 88], [127 82] [[137 108] 208], [79 132] [[81 104] 146], [112 79] [[78 79] 198], [6 15] [[0 1] 128], [58 103] [[80 103] 94], [41 36] [[31 64] 214], [30 65] [[20 65] 26], [53 76] [[65 62] 102], [7 102] [[40 125] 572], [88 19] [[112 19] 320], [8 101] [[38 101] 230], [127 80] [[131 60] 172], [110 111] [[82 103] 216], [105 60] [[113 78] 114], [29 14] [[41 34] 180], [30 13] [[66 11] 282], [17 32] [[7 16] 202], [64 135] [[42 125] 212], [40 35] [[18 33] 112], [78 133] [[66 135] 74], [77 78] [[85 66] 92], [7 16] [[17 32] 202], [82 103] [[110 111] 216], [66 11] [[30 13] 282], [113 20] [[111 38] 96], [42 125] [[64 135] 212], [76 79] [[54 77] 104], [41 124] [[39 102] 108], [78 79] [[112 79] 198], [85 64] [[89 34] 234], [28 13] [[8 15] 102], [58 33] [[88 33] 154], [64 61] [[32 65] 132], [84 65] [[66 61] 58], [18 33] [[40 35] 112], [17 34] [[19 64] 324], [114 79] [[126 81] 38], [90 33] [[110 39] 90], [114 19] [[127 38] 456], [57 34] [[65 60] 206], [112 19] [[88 19] 320], [127 134] [[137 110] 154], [65 62] [[53 76] 102], [65 134] [[57 104] 182], [126 39] [[112 39] 106], [0 1] [[6 15] 128], [39 100] [[39 80] 88]}
      ]
  (defn getpart [from visited]
    (loop [pos from visited visited cnt 0 mem nil]
      (let [cand (->> (map #(mapv + pos %) dirs)
                      (remove visited)
                      (remove #(= \# (get-in Map % \#))))
            realcand (remove #(contains? @dict %) cand)]
        (if (or (= 1 (count realcand) (count cand)) (= pos end))
          (recur (first cand) (conj visited pos) (inc cnt) pos)
          [pos (map #(vector % #{pos}) cand) cnt mem]))))
  ;(getpart start #{})
  (comment loop [todo [[start #{}]]]
    (when (not-empty todo)
      (let [[s v] (first todo)
            [e lst step last2] (getpart s v)]
        (println s e step lst)
        (swap! dict assoc s [last2 (dec step)])
        (swap! dict assoc last2 [s (dec step)])
        (recur (concat (rest todo) (remove #(or (contains? @dict (first %)) (empty? (first %))) lst))))))
  ;(spit "temp" @dict)
  (count dict)
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