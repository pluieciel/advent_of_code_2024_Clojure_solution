;advent-of-code-2023.day18
(ns day18
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i) str/split-lines (map #(str/split % #" "))))

(def dirs {"R" [0 1] "D" [1 0] "L" [0 -1] "U" [-1 0]})

(let [actions (parse "./2023/ex18")
      start [0 0]
      path (->> (reduce (fn [done action]
                          (let [pos (last done)
                                [d step _] action
                                step (read-string step)
                                newpos (mapv + pos (map #(* % step) (get dirs d)))]
                            (conj done newpos)))
                        [start] actions)
                rest)
      rows (->> path
                (group-by first)
                (#(for [[r lst] %] [r (partition 2 (sort (map second lst)))]))
                (sort-by first))]
  rows
  )



















(let [edge (loop [todo (parse "./2023/ex18") pos [0 0] done #{[0 0]}]
             (println pos)
             (if (empty? todo)
               done
               (let [;[_ _ color] (first todo)
                     ;step (read-string (str "0x" (subs color 2 7)))
                     ;dir (get (vec (vals dirs)) (read-string (subs color 7 8)))
                     [d step _] (first todo)
                     step (read-string step)
                     dir (get dirs d)
                     newpos (mapv + pos (map (fn [n] (* n step)) dir))]
                 (recur (rest todo) newpos (conj done newpos)))))
      edge (for [[r lst] (sort-by first (group-by first edge))] [r (partition 2 (sort-by last lst))])
      ]
  edge
  (defn get-len [lst]
    (->> lst
         (map (fn [[[_ a] [_ b]]] (inc (- b a))))
         (reduce +)))
  
  (loop [todo (rest edge) curr (second (first edge)) tot 0 prerow (first (first edge)) prelen 0]
    (println curr tot prerow prelen)
    (if (empty? todo)
      tot
      (let [[row lst] (first todo)
            len (get-len curr)
            toadd (* (- row prerow) len)
            [newrow mu] (loop [res [] now curr nxt lst makeup 0]
                          (println "*" res now nxt makeup)
                          (if (empty? nxt)
                            [(concat res now) makeup]
                            (let [now (if-let [newnow (and (> (count now) 1)
                                                           (let [[[a b] [c d] & r] now]
                                                             (if (= (last b) (last c)) (do (println "**" (conj r [a d])) (conj r [a d])) now)))]
                                        newnow
                                        now)
                                  [s e :as old] (first now) [ns ne :as new] (first nxt)]
                              (println "***" res now nxt makeup)
                              (cond
                                (empty? now) (recur (vec (concat res nxt)) now '() makeup)
                                (< (last ne) (last s)) (recur (conj res new) now (rest nxt) makeup)
                                (< (last e) (last ns)) (recur (conj res old) (rest now) nxt makeup)
                                (and (= (last s) (last ns)) (= (last e) (last ne))) (recur res (rest now) (rest nxt) (+ makeup (inc (- (last ne) (last ns)))))
                                (= (last s) (last ns)) (recur res (conj (rest now) [ne e]) (rest nxt) (+ makeup (- (last ne) (last ns))))
                                (= (last s) (last ne)) (recur res (conj (rest now) [ns e]) (rest nxt) makeup)
                                (= (last e) (last ne)) (recur res (conj (rest now) [s ns]) (rest nxt) (+ makeup (- (last ne) (last ns))))
                                (= (last e) (last ns)) (recur res (conj (rest now) [s ne]) (rest nxt) makeup)
                                (apply < (map last [s ns ne e])) (recur (conj res [s ns]) (conj (rest now) [ne e]) (rest nxt) (+ makeup (- (last ns) (last s)) (- (last e) (last ne))))))))]
        ;(println mu)
        (recur (rest todo) newrow (+ tot toadd mu) row len)))))

(def dirs {"R" [0 1] "D" [1 0] "L" [0 -1] "U" [-1 0]})

(let [edge (loop [todo (parse "./2023/in18") pos [0 0] done #{}]
             (if (empty? todo)
               done
               (let [[d step _] (first todo)
                     step (read-string step)
                     dir (get dirs d)
                     newposs (map #(mapv + pos (map (fn [n] (* n %)) dir)) (range (inc step)))
                     newpos (last newposs)]
                 (recur (rest todo) newpos (apply conj done newposs)))))
      edge (apply merge-with set/union (map (fn [[k v]] {k #{v}}) edge))]
  (->> (for [[r cols] (sort-by first edge)]
         (->> cols
              sort
              (map-indexed (fn [i v] [v (- v i)]))
              (partition-by second)
              (map #(map first %))
              ((fn [lst]
                 (loop [lst lst cnt 0 flag false pre nil]
                   (if (empty? lst)
                     cnt
                     (let [doing (first lst)
                           s (first doing) e (last doing)
                           prer (get edge (dec r) #{})
                           nxtr (get edge (inc r) #{})]
                       (recur (rest lst)
                              (+ cnt (count doing) (if flag (- s pre 1) 0))
                              (if (or (and (prer s) (nxtr e)) (and (prer e) (nxtr s)))
                                (not flag)
                                flag)
                              e))))))))
       (reduce +)))