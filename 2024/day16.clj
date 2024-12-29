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
      dirs [[0 1] [1 0] [0 -1] [-1 0]]
      mymerge (fn [v1 v2]
                (cond
                  (< (get v1 :score) (get v2 :score)) v1
                  (> (get v1 :score) (get v2 :score)) v2
                  :else (assoc v1 :from (set/union (get v1 :from) (get v2 :from)))))
      [fin data] (loop [todo [[start 0]]
                        done #{}
                        data (transient {[start 0] {:from #{} :score 0}})]
                   (let [[pos ndir :as doing] (first todo)
                         curscore (get-in data [doing :score])
                         lnpos [pos (mod (dec ndir) 4)]
                         rnpos [pos (mod (inc ndir) 4)]
                         revpos [pos (mod (+ ndir 2) 4)]
                         left (when (not (get done rnpos))
                                [lnpos {:from #{doing} :score (+ curscore 1000)}])
                         right (when (not (get done lnpos))
                                 [rnpos {:from #{doing} :score (+ curscore 1000)}])
                         forw (let [npos (mapv + pos (get dirs ndir))]
                                (when (and (not= \# (get-in Map npos))
                                           (not (done [npos (mod (+ ndir 2) 4)])))
                                  [[npos ndir] {:from #{doing} :score (inc curscore)}])) 
                         adddata [forw left right]
                         newdata (reduce #(if-let [[k v2] %2]
                                            (if-let [v1 (get %1 k)]
                                              (assoc! %1 k (mymerge v1 v2))
                                              (assoc! %1 k v2))
                                            %1) data adddata)]
                     ;(println doing)
                     (if (and forw (= end (ffirst forw)))
                       [(first forw) newdata]
                       (let [newdone (conj done doing)
                             settodo (disj (set todo) doing)]
                         (recur (->> (rest todo)
                                     (#(if (and forw (not (settodo (first forw)))) (conj % (first forw)) %))
                                     (#(if (and right (not (settodo (first right)))) (concat % [(first right)]) %))
                                     (#(if (and left (not (settodo (first left)))) (concat % [(first left)]) %))
                                     (remove #(get newdone %)))
                                newdone
                                newdata)))))]
  ;part 1
  (->> (get-in data [fin :score])
       println)
  ;part 2
  (->> (loop [todo [fin] res (transient #{})]
         (if (empty? todo)
           (count res)
           (let [doing (first todo)
                 from (get-in data [doing :from])]
             (recur (reduce conj (rest todo) from) (conj! res (doing 0))))))
       println))