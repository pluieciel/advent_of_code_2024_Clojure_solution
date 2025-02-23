;advent-of-code-2022.day19
(ns day19
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map]])
  (:import [org.ojalgo.optimisation ExpressionsBasedModel Variable]))

(defn cal [ore-o clay-o obs-o obs-c geo-o geo-obs]
  (defn optimize []
    (let [model (ExpressionsBasedModel.)
          xs (vec (for [i (range 100)]
                    (doto (Variable/make (str "x" i))
                      (.integer true)
                      (.lower 0)
                      (.upper 24))))]
      ;objective function
      (doseq [i (range 75 99)]
        (.weight (get xs i) 1.0))
      ;add variables to the model
      (doseq [x xs]
        (.addVariable model x))
      (doseq [robot [0 25 50 75]]
        ;init
        (doto (.addExpression model (str "init" robot 0))
          (.lower (if (zero? robot) 1 0))
          (.upper (if (zero? robot) 1 0))
          (.set (get xs robot) 1.0))
        ;onestep
        (doseq [i (range 1 25)]
          (doto (.addExpression model (str "onestep" robot i))
            (.lower 0)
            (.upper 1)
            (.set (get xs (+ robot i)) 1.0)
            (.set (get xs (+ robot (dec i))) -1.0))))
      ;produce one of four robot at most each step
      (doseq [i (range 1 25)]
        (let [constr (doto (.addExpression model (str "allonestep" i))
                       (.lower 0)
                       (.upper 1))]
          (doseq [robot [0 25 50 75]]
            (doto constr
              (.set (get xs (+ robot i)) 1.0)
              (.set (get xs (+ robot (dec i))) -1.0)))))
      ;ore constraint
      (doseq [i (range 1 25)]
        (let [constr (doto (.addExpression model (str "ore" i))
                       (.lower (- ore-o)))]
          (doto constr (.set (get xs (+ i 0)) (- ore-o))
                       (.set (get xs (+ i 25)) (- clay-o))
                       (.set (get xs (+ i 50)) (- obs-o))
                       (.set (get xs (+ i 75)) (- geo-o)))
          (doseq [j (range (dec i))]
            (doto constr (.set (get xs j) 1.0)))))
      ;clay constraint
      (doseq [i (range 1 25)]
        (let [constr (doto (.addExpression model (str "clay" i))
                       (.lower 0))]
          (doto constr (.set (get xs (+ i 50)) (- obs-c)))
          (doseq [j (range (dec i))]
            (doto constr (.set (get xs (+ j 25)) 1.0)))))
      ;obs constraint
      (doseq [i (range 1 25)]
        (let [constr (doto (.addExpression model (str "obs" i))
                       (.lower 0))]
          (doto constr (.set (get xs (+ i 75)) (- geo-obs)))
          (doseq [j (range (dec i))]
            (doto constr (.set (get xs (+ j 50)) 1.0)))))

      (let [result (.maximise model)]
        ;; {:value (.getValue result)
        ;;  :xs (partition 25 (map #(.getValue %) xs))}
        (Math/round (.getValue result))
        )))
  (optimize))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map #(map read-string (re-seq #"\d+" %)))))

(let [bps (parse "./2022/in19")]
  (->> (for [[idx ore-o clay-o obs-o obs-c geo-o geo-obs] bps]
         (* idx (cal ore-o clay-o obs-o obs-c geo-o geo-obs)))
       (reduce +)))
