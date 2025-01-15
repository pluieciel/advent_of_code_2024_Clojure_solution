;advent-of-code-2023.day17
(ns day17
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (mapv #(mapv (fn [d] (Character/digit d 10)) %))))

(def dirs [[-1 0] [0 1] [1 0] [0 -1]]) ;0 ^ 1 > 2 v 3 <

(let [Map (parse "./2023/ex17")
      h (count Map) w (count (first Map))
      end [(dec h) (dec w)]]
  (defn dijk [start] ; [:pos '(op op op) :score]
    (loop [done {} todo [[[start [1]] 0]] cnt 0]
      ;(println done todo)
      (if ;(= cnt 200)
      (empty? todo)
        ;(filter (fn [[[pos d] v]] (= pos end)) done)
        done
        (let [[[pos ds] score] (first todo)
              currdir (first ds)
              rdir (-> (inc currdir) (mod 4))
              ldir (-> (dec currdir) (mod 4))
              newdirs (if (and (= 3 (count ds)) (apply = ds))
                        [rdir ldir]
                        [rdir currdir ldir])
              newcand (for [ndir newdirs
                            :let [npos (mapv + pos (get dirs ndir))
                                  newscore (get-in Map npos)]
                            :when newscore]
                        [[npos (take 3 (conj ds ndir))] (+ score newscore)])
              toadd (remove #(get-in done (let [[[pos ds] s] %] [pos (if (and (= 3 (count ds)) (apply = ds)) (first ds) nil)])) newcand)
              toadd (map (fn [[[pos ds] s]] [[pos (first (partition-by identity ds))] s]) toadd)]
          (recur
           (->> (into {} (map (fn [[[pos ds] s]] [[pos (if (and (= 3 (count ds)) (apply = ds)) (first ds) nil)] s]) newcand))
                (merge-with #(min %1 %2) done))
           (sort-by last (concat (rest todo) toadd))
           (inc cnt))))))
  (dijk [0 0])
  )