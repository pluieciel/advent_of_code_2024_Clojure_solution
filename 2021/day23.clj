;advent-of-code-2021.day23
(ns day23
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map]]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (mapv vec)))

(def rowplace [1 2 4 6 8 10 11])
(def cost {\A 1 \B 10 \C 100 \D 1000})
(def final {\A #{[2 3] [3 3]} \B #{[2 5] [3 5]}
            \C #{[2 7] [3 7]} \D #{[2 9] [3 9]}})

(defn get-poss [state]
  (->> state
       vals
       (apply set/union)))

(defn get-colocc [poss]
  (->> poss
       (filter #(= 1 (first %)))
       (map second)))

(defn get-ava-col [c rowocc]
  (let [{l true r false} (group-by #(< % c) (remove #{c} rowocc))
        l (if (nil? l) 0 (apply max l))
        r (if (nil? r) 12 (apply min r))]
    (->> (filter #(< l % r) rowplace)
         set)))

(defn get-targets [tp [r c] state]
  (let [poss (get-poss state)
        rowocc (get-colocc poss)]
    (case r
      2 (if (= (state tp) (final tp))
          []
          (->> (get-ava-col c rowocc)
               (map #(do [1 %]))))
      3 (if (or (poss [2 c]) ((final tp) [r c]))
          []
          (->> (get-ava-col c rowocc)
               (map #(do [1 %]))))
      1 (let [dest-col (->> (final tp) first second)
              avacols (get-ava-col c rowocc)
              cannot (not-empty (let [[l r] (sort [dest-col c])]
                              (->> (filter #(< l % r) rowplace)
                                   (remove avacols))))]
          (if (or cannot
                  (poss [2 dest-col])
                  (and (poss [3 dest-col]) (not ((state tp) [3 dest-col]))))
            []
            (cond
              (not (poss [3 dest-col])) [[3 dest-col]]
              :else [[2 dest-col]]))))))

(defn canmove [state]
  (let [all (for [[k v] state pos v
                  :let [targets (get-targets k pos state)]
                  :when (not-empty targets)]
              [k pos targets])]
    all))

(def Map (parse "2021/in23"))

;part 1 ;;dijkstra
(let [init-state (->> (for [r [2 3] c [3 5 7 9]]
                        {(get-in Map [r c]) #{[r c]}})
                      (apply merge-with #(set/union %1 %2)))]
  (loop [todo (priority-map init-state 0)]
    (let [[state score] (peek todo)
          nxt (pop todo)]
      (if (= final state)
        score
        (->> (let [possibilities (canmove state)]
               (mapcat
                (fn [[tp olopos targets]]
                  (map
                   (fn [newpos]
                     (let [newscore (* (cost tp)
                                       (apply +
                                              (map Math/abs
                                                   (map - olopos newpos))))
                           newstate (update state tp #(-> % (disj olopos) (conj newpos)))]
                       [newstate (+ score newscore)]))
                   targets))
                possibilities))
             (reduce
              (fn [acc [state score]]
                (if-let [oldscore (acc state)]
                  (if (< score oldscore)
                    (assoc acc state score)
                    acc)
                  (assoc acc state score)))
              nxt)
             (recur))))))

;part 2 ;;same algo, need 5 min to finish calculation, not efficient
(def Map2
  (let [insert "  #D#C#B#A#\n  #D#B#A#C#"
        ins-vec (->> insert str/split-lines (mapv vec))]
    (vec (concat (take 3 Map) ins-vec (drop 3 Map)))))

(def final2 {\A #{[2 3] [3 3] [4 3] [5 3]}
             \B #{[2 5] [3 5] [4 5] [5 5]}
             \C #{[2 7] [3 7] [4 7] [5 7]}
             \D #{[2 9] [3 9] [4 9] [5 9]}})

(defn get-targets2 [tp [r c] state]
  (let [poss (get-poss state)
        rowocc (get-colocc poss)]
    (case r
      2 (if (= (state tp) (final tp))
          []
          (->> (get-ava-col c rowocc)
               (map #(do [1 %]))))
      3 (if (or (poss [2 c])
                (and (= c (->> (final2 tp) first second))
                     (empty? (set/difference #{[3 c] [4 c] [5 c]} (state tp)))))
          []
          (->> (get-ava-col c rowocc)
               (map #(do [1 %]))))
      4 (if (or (poss [3 c])
                (and (= c (->> (final2 tp) first second))
                     (empty? (set/difference #{[4 c] [5 c]} (state tp)))))
          []
          (->> (get-ava-col c rowocc)
               (map #(do [1 %]))))
      5 (if (or (poss [4 c])
                (and (= c (->> (final2 tp) first second))
                     (empty? (set/difference #{[5 c]} (state tp)))))
          []
          (->> (get-ava-col c rowocc)
               (map #(do [1 %]))))
      1 (let [dest-col (->> (final2 tp) first second)
              avacols (get-ava-col c rowocc)
              cannot (not-empty (let [[l r] (sort [dest-col c])]
                                  (->> (filter #(< l % r) rowplace)
                                       (remove avacols))))]
          (if (or cannot
                  (poss [2 dest-col])
                  (and (poss [3 dest-col]) (not ((state tp) [3 dest-col])))
                  (and (poss [4 dest-col]) (not ((state tp) [4 dest-col])))
                  (and (poss [5 dest-col]) (not ((state tp) [5 dest-col]))))
            []
            (cond
              (not (poss [5 dest-col])) [[5 dest-col]]
              (not (poss [4 dest-col])) [[4 dest-col]]
              (not (poss [3 dest-col])) [[3 dest-col]]
              :else [[2 dest-col]]))))))

(defn canmove2 [state]
  (let [all (for [[k v] state pos v
                  :let [targets (get-targets2 k pos state)]
                  :when (not-empty targets)]
              [k pos targets])]
    all))

(def pflag (atom 0))

(let [init-state (->> (for [r [2 3 4 5] c [3 5 7 9]]
                        {(get-in Map2 [r c]) #{[r c]}})
                      (apply merge-with #(set/union %1 %2)))]
  (loop [todo (priority-map init-state 0)]
    (let [[state score] (peek todo)
          nxt (pop todo)]
      (when (> score @pflag) (swap! pflag #(+ % 10000)) (println score))
      (if (= final2 state)
        score
        (->> (let [possibilities (canmove2 state)]
               (mapcat
                (fn [[tp olopos targets]]
                  (map
                   (fn [newpos]
                     (let [newscore (* (cost tp)
                                       (apply +
                                              (map Math/abs
                                                   (map - olopos newpos))))
                           newstate (update state tp #(-> % (disj olopos) (conj newpos)))]
                       [newstate (+ score newscore)]))
                   targets))
                possibilities))
             (reduce
              (fn [acc [state score]]
                (if-let [oldscore (acc state)]
                  (if (< score oldscore)
                    (assoc acc state score)
                    acc)
                  (assoc acc state score)))
              nxt)
             (recur))))))