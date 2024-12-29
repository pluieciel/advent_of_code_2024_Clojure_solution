;advent-of-code-2024.day-15
(ns day15
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [input]
  (->> (slurp input)
       (#(str/split % #"\n\n"))
       (#(let [[Map ops] %]
           [(->> Map str/split-lines (mapv vec))
            (->> ops str/split-lines (apply str) (apply list))]))))

(defn move1 [walls boxs pos op]
  (let [dir ({\v [1 0] \> [0 1] \^ [-1 0] \< [0 -1]} op)]
    (loop [curr (mapv + pos dir) metbox #{}]
      (cond
        (walls curr) [boxs pos]
        (boxs curr) (recur (mapv + curr dir) (conj metbox curr))
        :else [(set/union (set/difference boxs metbox) (->> metbox (map #(mapv + % dir)) set))
               (mapv + pos dir)]))))

(defn move2 [walls boxs pos op]
  (let [dir ({\v [1 0] \> [0 1] \^ [-1 0] \< [0 -1]} op)]
    (if (#{\< \>} op)
      (loop [curr (mapv + pos dir) metbox #{}] 
        (cond
          (or (walls curr) (walls (mapv + curr [0 -1]))) [boxs pos]
          (or (boxs curr) (boxs (mapv + curr [0 -1]))) (recur (mapv + curr (map #(* 2 %) dir))
                                                              (conj metbox (if (boxs curr) curr (mapv + curr [0 -1]))))
          :else [(set/union (set/difference boxs metbox) (->> metbox (map #(mapv + % dir)) set))
                 (mapv + pos dir)]))
      (loop [curr #{(mapv + pos dir)} metbox #{}]
        (cond
          (some #(or (walls %) (walls (mapv + % [0 -1]))) curr)
          [boxs pos]
          (some #(or (boxs %) (boxs (mapv + % [0 -1]))) curr)
          (let [overlap (keep #(cond
                                 (boxs %) [% (mapv + % [0 1])]
                                 (boxs (mapv + % [0 -1])) [(mapv + % [0 -1]) %]) curr)
                newboxs (->> (map first overlap) set)
                newcurr (->> (apply concat overlap) set)]
           (recur (->> newcurr (map #(mapv + % dir)) set) (set/union metbox newboxs)))
          :else
          [(set/union (set/difference boxs metbox) (->> metbox (map #(mapv + % dir)) set))
           (mapv + pos dir)])))))

(defn step [move]
  (fn [walls boxs pos todo]
    (let [op (first todo)
          [newboxs newpos] (move walls boxs pos op)]
      [newboxs newpos (rest todo)])))

(defn cal-all [move]
  (fn [walls boxs start ops]
    (loop [[boxs pos todo] [boxs start ops]]
      (if (empty? todo)
        boxs
        (recur ((step move) walls boxs pos todo))))))

(let [[Map ops] (parse "input")
      dict (->> (for [x (range (count (first Map))) y (range (count Map))] [y x])
                (group-by #(get-in Map %)))
      {walls \# boxs \O [start] \@} dict
      [walls boxs] [(set walls) (set boxs)]
      GPS #(transduce (map (fn [v] (let [[y x] v] (+ (* 100 y) x)))) + %)]
  ;part 1
  (->> ((cal-all move1) walls boxs start ops)
       GPS
       println)
  ;part 2
  (let [double #(let [[y x] %] [y (* 2 x)])
        start (double start)
        [walls boxs] [(set (map double walls)) (set (map double boxs))]]
    (->> ((cal-all move2) walls boxs start ops)
         GPS
         println)))