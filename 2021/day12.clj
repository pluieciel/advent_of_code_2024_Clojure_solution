;advent-of-code-2021.day12
(ns day12
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map #(str/split % #"-"))))

(def dict (atom {}))

(doseq [[a b] (parse "2021/in12")]
  (let [la (get @dict a #{})
        lb (get @dict b #{})]
    (swap! dict assoc a (conj la b))
    (swap! dict assoc b (conj lb a))))

;part 1
(loop [todo [[["start"] #{}]] res []]
  (if (seq todo)
    (let [[path visited] (first todo)
          curr (last path)]
      (if (= curr "end")
        (recur (rest todo) (conj res path))
        (let [nxt (remove visited (get @dict curr))
              newvisited (if (Character/isLowerCase (first curr)) (conj visited curr) visited)]
          (recur (into (rest todo) (for [newnode nxt] [(conj path newnode) newvisited])) res))))
    (count res)))

;part 2
(loop [todo [[["start"] #{} nil]] res []]
  (if (seq todo)
    (let [[path visited twice] (first todo)
          curr (last path)]
      (if (= curr "end")
        (recur (rest todo) (conj res path))
        (let [newtwice (if (and (nil? twice) (visited curr)) curr twice)
              nxt (if (nil? newtwice) (remove #{"start"} (get @dict curr)) (remove visited (get @dict curr)))
              newvisited (if (Character/isLowerCase (first curr)) (conj visited curr) visited)]
          (recur (into (rest todo)
                       (for [newnode nxt]
                         [(conj path newnode) newvisited newtwice])) res))))
    (count res)))