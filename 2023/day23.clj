;advent-of-code-2023.day23
(ns day23
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (mapv vec)))

(def dirs [[1 0] [-1 0] [0 1] [0 -1]])
;part 1
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

;part 2
(let [Map (parse "./2023/in23")
      h (count Map) w (count (first Map))
      [start] (for [x (range w) :when (= \. (get-in Map [0 x]))] [0 x])
      [end] (for [x (range w) :when (= \. (get-in Map [(dec h) x]))] [(dec h) x])
      ;dict (read-string (slurp "temp"))
      ;g_visited (atom #{})
      ;dict (atom {})
      dict {[65 135] {[57 103] 184, [79 133] 76},
            [137 109] {[111 111] 120, [127 135] 156},
            [111 111] {[113 79] 234, [81 103] 218, [137 109] 120, [101 127] 102},
            [19 65] {[31 65] 28, [19 89] 260},
            [127 135] {[140 139] 64},
            [113 19] {[127 39] 458, [111 39] 98},
            [111 39] {[113 19] 98, [89 33] 92, [127 39] 108, [105 59] 98},
            [57 33] {[67 11] 120, [41 35] 90, [89 33] 156, [65 61] 208},
            [131 59] {[105 59] 98, [127 81] 174},
            [79 133] {[81 103] 148, [101 127] 164},
            [127 81] {[113 79] 40, [137 109] 210},
            [41 125] {[39 101] 110, [65 135] 214},
            [41 35] {[29 13] 182, [17 33] 114, [57 33] 90, [31 65] 216},
            [113 79] {[105 59] 116, [77 79] 200, [127 81] 40, [111 111] 234},
            [81 103] {[77 79] 184, [57 103] 96, [111 111] 218, [79 133] 148},
            [65 61] {[57 33] 208, [31 65] 134, [85 65] 60, [53 77] 104},
            [101 127] {[111 111] 102, [127 135] 174},
            [19 89] {[39 79] 118, [7 101] 128},
            [7 101] {[39 101] 232, [41 125] 574},
            [127 39] {[111 39] 108, [131 59] 164},
            [53 77] {[65 61] 104, [39 79] 56, [77 79] 106, [57 103] 186},
            [67 11] {[87 19] 176, [57 33] 120},
            [39 79] {[31 65] 74, [19 89] 118, [53 77] 56, [39 101] 90},
            [57 103] {[53 77] 186, [39 101] 68, [81 103] 96, [65 135] 184},
            [89 33] {[87 19] 84, [57 33] 156, [111 39] 92, [85 65] 236},
            [105 59] {[111 39] 98, [85 65] 126, [131 59] 98, [113 79] 116},
            [77 79] {[85 65] 94, [53 77] 106, [113 79] 200, [81 103] 184},
            [39 101] {[39 79] 90, [7 101] 232, [57 103] 68, [41 125] 110},
            [85 65] {[89 33] 236, [65 61] 60, [105 59] 126, [77 79] 94},
            [31 65] {[41 35] 216, [19 65] 28, [65 61] 134, [39 79] 74},
            [87 19] {[113 19] 322, [89 33] 84},
            [29 13] {[67 11] 284, [41 35] 182},
            [0 1] {[29 13] 234, [17 33] 334},
            [17 33] {[41 35] 114, [19 65] 326}}]
  
  ; use following code to generate dict of a graph, manually adjust the peripheral nodes
  ;(nodes with 3 neighbors, prune to 2 neighbors, since go back from peripheral node is not allowed)

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
  (let [bitmap (apply merge (for [k (keys dict)] {k false}))
        biggest (atom 0)]
    (loop [todo [[start 0 bitmap]]]
      (let [results (pmap (fn [[[r c :as node] step bit]]
                            (let [newbit (assoc bit node true)
                                  cand (get dict node)]
                              (cond
                                (= node end) (do (when (> step @biggest)
                                                         (println step)
                                                         (reset! biggest step))
                                                       nil)
                                :else
                                (let [newc (remove (fn [[k v]]
                                                     (get newbit k)) cand)]
                                  (map (fn [[k v]] [k (+ step v) newbit]) newc)))))
                          todo)
            new-todo (apply concat (remove nil? results))]
        (when (seq new-todo)
          (recur new-todo))))))