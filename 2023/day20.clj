;advent-of-code-2023.day20
(ns day20
  (:require [clojure.string :as str]))

(defn parse [i]
  (let [dict (->> (slurp i)
                  str/split-lines
                  (map (fn [line]
                         (->> (str/split line #" -> ")
                              (#(let [[key vs] %
                                      k (re-seq #"%|&|\w+" key)
                                      type (first k)
                                      dests (str/split vs #", ")
                                      b-flag (= 1 (count k))]
                                  {(if b-flag (first k) (second k))
                                   {:type type
                                    :dest dests
                                    :state (case type
                                             "&" nil
                                             (atom false))}})))))
                  (apply merge))]
    (->> (let [todo (for [[k v] dict :when (= "&" (get v :type))] k)]
           (map (fn [d] [d (->> (for [[k v] dict :when (some #(= d %) (get v :dest))]
                                  {k (atom false)})
                                (apply merge))]) todo))
         (reduce (fn [d [k state]] (assoc-in d [k :state] state)) dict))))

(defn snapshot [dict]
  (apply merge
         (for [[k v] dict :when (= (get v :type) "%")]
           {k @(get v :state)})))
;part 1
(let [dict (parse "./2023/in20")
      dict (assoc dict "button" {:type "button" :dest ["broadcaster"] :state (atom false)})
      initsnap (snapshot dict)
      high (atom 0) low (atom 0) cnt (atom 1)]

  (loop [todo ["button"]]
    (if (empty? todo)
      (if (or (= initsnap (snapshot dict)) (= @cnt 1000))
        [@high @low @cnt]
        (do (swap! cnt inc)
            (recur ["button"])))
      (let [doing (first todo)
            type (get-in dict [doing :type])]

        (println [doing @high @low @cnt]) (flush)

        (if (nil? (get dict doing))
          (recur (rest todo))
          (case type
            "button"
            (do
              (swap! low inc)
              (reset! (get-in dict ["broadcaster" :state]) false)
              (recur ["broadcaster"]))
            "broadcaster"
            (let [sig @(get-in dict ["broadcaster" :state])
                  dest (get-in dict ["broadcaster" :dest])
                  nxt (atom dest)]
              (doseq [node dest :let [v (get dict node)
                                      {desttype :type deststate :state} v]]
                (case desttype
                  "%" (if (true? sig)
                        (do (swap! high inc) (reset! nxt (remove #(= node %) @nxt)))
                        (do (swap! low inc) (swap! deststate not)))
                  "&" (do (swap! (if (true? sig) high low) inc)
                          (reset! (get-in dict [node :state doing]) sig))
                  (swap! (if (true? sig) high low) inc) ;(println [doing node])
                  ))
              (recur (concat (rest todo) @nxt)))
            "%"
            (let [sig @(get-in dict [doing :state])
                  dest (get-in dict [doing :dest])
                  nxt (atom dest)]
              (doseq [node dest :let [v (get dict node)
                                      {desttype :type deststate :state} v]]
                (case desttype
                  "%" (if (true? sig)
                        (do (swap! high inc) (reset! nxt (remove #(= node %) @nxt)))
                        (do (swap! low inc) (swap! deststate not)))
                  "&" (do (swap! (if (true? sig) high low) inc)
                          (reset! (get-in dict [node :state doing]) sig))
                  "broadcaster" (do (swap! (if (true? sig) high low) inc)
                                    (reset! (get-in dict ["broadcaster" :state]) sig))
                  (swap! (if (true? sig) high low) inc) ;(println [doing node])
                  ))
              (recur (concat (rest todo) @nxt)))
            "&"
            (let [sig (not (every? true? (map deref (vals (get-in dict [doing :state])))))
                  dest (get-in dict [doing :dest])
                  nxt (atom dest)]
              (doseq [node dest :let [v (get dict node)
                                      {desttype :type deststate :state} v]]
                (case desttype
                  "%" (if (true? sig)
                        (do (swap! high inc) (reset! nxt (remove #(= node %) @nxt)))
                        (do (swap! low inc) (swap! deststate not)))
                  "&" (do (swap! (if (true? sig) high low) inc)
                          (reset! (get-in dict [node :state doing]) sig))
                  "broadcaster" (do (swap! (if (true? sig) high low) inc)
                                    (reset! (get-in dict ["broadcaster" :state]) sig))
                  (swap! (if (true? sig) high low) inc) ;(println [doing node])
                  ))
              (recur (concat (rest todo) @nxt)))))))))

;part 2
(let [dict (parse "./2023/in20")
      dict (assoc dict "button" {:type "button" :dest ["broadcaster"] :state (atom false)})
      ;initsnap (snapshot dict)
      ;high (atom 0) low (atom 0) 
      cnt (atom 1)
      run (atom true)]

  (loop [todo ["button"]]
    (if (empty? todo)
      (do 
          (let [s (get-in dict ["zg" :state])
                res (for [[k v] s] [k @v])]
            (when (zero? (mod @cnt 50000)) (println @cnt) (flush))
            (when @run
              (swap! cnt inc)
              (recur ["button"])
              )))
      (let [doing (first todo)
            type (get-in dict [doing :type])]

        (if (nil? (get dict doing))
          (recur (rest todo))
          (case type
            "button"
            (do
              ;(swap! low inc)
              (reset! (get-in dict ["broadcaster" :state]) false)
              (recur ["broadcaster"]))
            "broadcaster"
            (let [sig @(get-in dict ["broadcaster" :state])
                  dest (get-in dict ["broadcaster" :dest])
                  nxt (atom dest)]
              (doseq [node dest :let [v (get dict node)
                                      {desttype :type deststate :state} v]]
                (case desttype
                  "%" (if (true? sig)
                        (do ;(swap! high inc)
                            (reset! nxt (remove #(= node %) @nxt)))
                        (do ;(swap! low inc)
                            (swap! deststate not)))
                  "&" (do ;(swap! (if (true? sig) high low) inc)
                          (reset! (get-in dict [node :state doing]) sig))
                  nil ;(swap! (if (true? sig) high low) inc) ;(println [doing node])
                  ))
              (recur (concat (rest todo) @nxt)))
            "%"
            (let [sig @(get-in dict [doing :state])
                  dest (get-in dict [doing :dest])
                  nxt (atom dest)]
              (doseq [node dest :let [v (get dict node)
                                      {desttype :type deststate :state} v]]
                (case desttype
                  "%" (if (true? sig)
                        (do ;(swap! high inc)
                            (reset! nxt (remove #(= node %) @nxt)))
                        (do ;(swap! low inc)
                            (swap! deststate not)))
                  "&" (do ;(swap! (if (true? sig) high low) inc)
                          (reset! (get-in dict [node :state doing]) sig))
                  "broadcaster" (do ;(swap! (if (true? sig) high low) inc)
                                    (reset! (get-in dict ["broadcaster" :state]) sig))
                  nil ;(swap! (if (true? sig) high low) inc) ;(println [doing node])
                  ))
              (recur (concat (rest todo) @nxt)))
            "&"
            (let [sig (not (every? true? (map deref (vals (get-in dict [doing :state])))))
                  dest (get-in dict [doing :dest])
                  nxt (atom dest)]
              
              ;check ql, hl, hq, bc separatedly here, and get lcm for them
              (when (and (= doing "bc") (not sig)) (reset! run false) (println "***" @cnt) (flush))

              (doseq [node dest :let [v (get dict node)
                                      {desttype :type deststate :state} v]]
                (case desttype
                  "%" (if (true? sig)
                        (do ;(swap! high inc)
                            (reset! nxt (remove #(= node %) @nxt)))
                        (do ;(swap! low inc)
                            (swap! deststate not)))
                  "&" (do ;(swap! (if (true? sig) high low) inc)
                          (reset! (get-in dict [node :state doing]) sig))
                  "broadcaster" (do ;(swap! (if (true? sig) high low) inc)
                                    (reset! (get-in dict ["broadcaster" :state]) sig))
                  nil ;(swap! (if (true? sig) high low) inc) ;(println [doing node])
                  ))
              (recur (concat (rest todo) @nxt)))))))))