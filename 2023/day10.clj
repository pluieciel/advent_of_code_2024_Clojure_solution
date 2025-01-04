;advent-of-code-2023.day10
(ns day10
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (mapv vec)))

(defn step [curr Map]
  (let [[[y x :as pos] from] curr]
    (case from
      :s (case (get-in Map pos)
           \7 [[y (dec x)] :e]
           \F [[y (inc x)] :w]
           \| [[(dec y) x] :s])
      :n (case (get-in Map pos)
           \J [[y (dec x)] :e]
           \L [[y (inc x)] :w]
           \| [[(inc y) x] :n])
      :e (case (get-in Map pos)
           \L [[(dec y) x] :s]
           \F [[(inc y) x] :n]
           \- [[y (dec x)] :e])
      :w (case (get-in Map pos)
           \J [[(dec y) x] :s]
           \7 [[(inc y) x] :n]
           \- [[y (inc x)] :w]))))

(let [Map (parse "./2023/in10")
      h (count Map) w (count (first Map))
      [y x :as start] (first (for [y (range h) x (range w) :when (= \S (get-in Map [y x]))] [y x]))
      stepone (cond
                (#{\7 \F \|} (get-in Map [(dec y) x])) [[(dec y) x] :s]
                (#{\L \J \|} (get-in Map [(inc y) x])) [[(inc y) x] :n]
                (#{\L \F \-} (get-in Map [y (dec x)])) [[y (dec x)] :e]
                (#{\7 \J \-} (get-in Map [y (inc x)])) [[y (inc x)] :w])
      path (atom #{start})]
  ;part 1
  (loop [curr stepone cnt 1]
    (swap! path conj (first curr))
    (if (= (first curr) start)
      (quot cnt 2)
      (recur (step curr Map) (inc cnt))))
  ;part 2
  (let [Map (assoc-in Map start \L)] ;replace \S by the right pipe for input
    (->> (for [y (range h)]
           (loop [flag false x 0 cnt 0]
             (if (>= x w)
               cnt
               (cond
                 (and (@path [y x]) (= (get-in Map [y x]) \|))
                 (recur (not flag) (inc x) cnt)
                 (and (@path [y x]) (= (get-in Map [y x]) \L))
                 (let [[nf nx] (loop [x (inc x)]
                                 (case (get-in Map [y x])
                                   \- (recur (inc x))
                                   \7 [(not flag) (inc x)]
                                   \J [flag (inc x)]))]
                   (recur nf nx cnt))
                 (and (@path [y x]) (= (get-in Map [y x]) \F))
                 (let [[nf nx] (loop [x (inc x)]
                                 (case (get-in Map [y x])
                                   \- (recur (inc x))
                                   \J [(not flag) (inc x)]
                                   \7 [flag (inc x)]))]
                   (recur nf nx cnt))
                 flag
                 (recur flag (inc x) (inc cnt))
                 :else
                 (recur flag (inc x) cnt)))))
         (reduce +))))