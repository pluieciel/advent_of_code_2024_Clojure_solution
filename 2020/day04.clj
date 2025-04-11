;advent-of-code-2020.day04
(ns day04
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [i]
  (->> (str/split (slurp i) #"\n\n")
       (map (fn [line]
              (->> (str/split line #"\n| ")
                   (map (fn [kv]
                          (str/split kv #":")))
                   (into {}))))))

(def target (set '("ecl" "pid" "eyr" "hcl" "byr" "iyr" "hgt")))
(def data (parse "2020/in04"))

;part 1
(reduce
 (fn [acc dict]
   (let [keys (set (keys dict))
         ok? (empty? (set/difference target keys))]
     (if ok? (inc acc) acc)))
 0
 data)

;part 2
(reduce
 (fn [acc dict]
   (let [{ecl "ecl" pid "pid" eyr "eyr" hcl "hcl"
          byr "byr" iyr "iyr" hgt "hgt"} dict]
     (if (and
          ecl pid eyr hcl byr iyr hgt
          (re-find #"amb|blu|brn|gry|grn|hzl|oth" ecl)
          (re-find #"^[0-9]{9}$" pid)
          (re-find #"^202[0-9]$|^2030$" eyr)
          (re-find #"^#[0-9a-f]{6}$" hcl)
          (re-find #"^19[2-9][0-9]$|^200[0-2]$" byr)
          (re-find #"^201[0-9]$|^2020$" iyr)
          (re-find #"^1[5-8][0-9]cm$|^19[0-3]cm$|^59in$|^6[0-9]in$|^7[0-6]in$" hgt))
       (inc acc)
       acc)))
 0
 data)