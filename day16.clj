;advent-of-code-2024.day-16
(ns day16
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [input]
  (->> (slurp input) 
       str/split-lines
       (mapv vec)))

;data {[pos ndir] {:from #{} :score 0 :exp (+ score distE)}}
(let [Map (parse "input")
      {start \S end \E} (->> (for [x (range (count (first Map))) y (range (count Map))
                                   :when (#{\S \E} (get-in Map [y x]))]
                               {(get-in Map [y x]) [y x]})
                             (apply merge))
      distE (fn [pos] (->> (map #(Math/abs (- %1 %2)) pos end) (reduce +)))
      dirs [[0 1] [1 0] [0 -1] [-1 0]]
      mymerge (fn [v1 v2]
                (cond
                  (< (get v1 :score) (get v2 :score)) v1
                  (> (get v1 :score) (get v2 :score)) v2
                  :else (assoc v1 :from (set/union (get v1 :from) (get v2 :from)))))
      [fin data] (loop [todo [[start 0]] done #{} data {[start 0] {:from #{} :score 0 :exp (distE start)}}]
                   (let [sortedtodo (sort-by #(get-in data [% :exp]) todo)
                         [pos ndir :as doing] (first sortedtodo)
                         left [pos (mod (dec ndir) 4)]
                         right [pos (mod (inc ndir) 4)]
                         forw (let [npos (mapv + pos (get dirs ndir))]
                                (when (and (not= \# (get-in Map npos))
                                           (not (done [npos (mod (+ ndir 2) 4)])))
                                  [npos ndir]))
                         newpos (remove nil? [left right forw])
                         newdata (apply merge
                                        (for [k newpos]
                                          (let [score (+ (get-in data [doing :score])
                                                         (if (= ndir (k 1)) 1 1000))]
                                            {k {:from #{doing}
                                                :score score
                                                :exp (+ score (distE (k 0)))}})))]
                     (if (some #(= end (% 0)) newpos)
                       [forw (merge-with mymerge data newdata)]
                       (let [newdone (conj done doing)]
                         (recur (remove newdone (reduce conj (rest sortedtodo) newpos))
                                newdone
                                (merge-with mymerge data newdata))))))]
  ;part 1
  (->> (get-in data [fin :score])
       println)
  ;part 2
  (->> (loop [todo [fin] res #{}]
         (if (empty? todo)
           (count res)
           (let [doing (first todo)
                 from (get-in data [doing :from])]
             (recur (reduce conj (rest todo) from) (conj res (doing 0))))))
       println))