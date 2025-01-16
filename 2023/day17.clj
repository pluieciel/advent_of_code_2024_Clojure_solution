;advent-of-code-2023.day17
(ns day17
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (mapv #(mapv (fn [d] (Character/digit d 10)) %))))

(def dirs [[-1 0] [0 1] [1 0] [0 -1]]) ;0 ^ 1 > 2 v 3 <

;part 1
(let [Map (parse "./2023/in17")
      h (count Map) w (count (first Map))
      end [(dec h) (dec w)]
      visited (atom #{})]
  
  (defn newdirs [pos score pq dir]
    (let [d (get dirs dir)
          npos (mapv + pos d)]
      (if-let [ns (get-in Map npos)]
        (let [ns (+ score ns) nkey [npos dir 1] nows (get pq nkey)]
          (if (or (nil? nows) (< ns nows))
            (assoc pq nkey ns)
            pq))
        pq)))
  
  (defn getnew [curr pq]
    (let [[[pos dir steps] score] curr
          ld (if (= dir 0) 3 (dec dir))
          rd (if (= dir 3) 0 (inc dir))
          pq (reduce (partial newdirs pos score) pq [ld rd])]
      (if (< steps 3)
        (let [npos (mapv + pos (get dirs dir))]
          (if-let [ns (get-in Map npos)]
            (let [ns (+ score ns) nkey [npos dir (inc steps)] nows (get pq nkey)]
              (if (or (nil? nows) (< ns nows))
                (assoc pq nkey ns)
                pq))
            pq))
        pq)))

  (defn dijk [start]
    (loop [pq (priority-map [start -1 0] 0)]
      ;(println pq)
      (let [[[pos dir steps] score :as curr] (peek pq)
            pq (pop pq)]
        (if (contains? @visited [pos dir steps])
          (recur pq)
          (do
            (swap! visited conj [pos dir steps])
            (if (= pos end)
              score
              (if (= dir -1)
                (recur (reduce (partial newdirs pos score) pq [1 2]))
                (recur (getnew curr pq)))))))))
  
  (dijk [0 0]))

;part 2
(let [Map (parse "./2023/in17")
      h (count Map) w (count (first Map))
      end [(dec h) (dec w)]
      visited (atom #{})]
  
  (defn newdirs2 [pos score pq dir]
    (let [d (get dirs dir)
          npos (mapv + pos (map #(* 4 %) d))]
      (if-let [ns (get-in Map npos)]
        (let [ns (+ score ns
                    (reduce +
                            (map #(get-in Map
                                          (mapv + pos (map
                                                       (fn [x]
                                                         (* x %)) d))) [1 2 3])))
              nkey [npos dir 4]
              nows (get pq nkey)]
          (if (or (nil? nows) (< ns nows))
            (assoc pq nkey ns)
            pq))
        pq)))
  
  (defn getnew2 [curr pq]
    (let [[[pos dir steps] score] curr
          ld (if (= dir 0) 3 (dec dir))
          rd (if (= dir 3) 0 (inc dir))
          pq (reduce (partial newdirs2 pos score) pq [ld rd])]
      (if (< steps 10)
        (let [npos (mapv + pos (get dirs dir))]
          (if-let [ns (get-in Map npos)]
            (let [ns (+ score ns) nkey [npos dir (inc steps)] nows (get pq nkey)]
              (if (or (nil? nows) (< ns nows))
                (assoc pq nkey ns)
                pq))
            pq))
        pq)))

  (defn dijk2 [start]
    (loop [pq (priority-map [start -1 0] 0)]
      ;(println pq)
      (let [[[pos dir steps] score :as curr] (peek pq)
            pq (pop pq)]
        (if (contains? @visited [pos dir steps])
          (recur pq)
          (do
            (swap! visited conj [pos dir steps])
            (if (= pos end)
              score
              (if (= dir -1)
                (recur (reduce (partial newdirs2 pos score) pq [1 2]))
                (recur (getnew2 curr pq)))))))))
  
  (dijk2 [0 0]))