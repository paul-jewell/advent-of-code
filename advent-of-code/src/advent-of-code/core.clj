(ns advent-of-code.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn -main
  "Solve Advent of Code 2018 day 1"
  [& args]
  (printf "Calibration frequency: %d\n" (day1a))
  (printf "First repeating frequency: %d/n" (day1b)))

(defn day1a
  "Solve part a - calibrate the device"
  []
  (reduce + (map read-string (str/split (slurp "../day1-input.txt") #"\n"))))

(defn day1b
  "Find the repeating frequency"
  []
  (loop [calib-freq 0
         frequencies #{}
         changelist (map read-string (str/split (slurp "../day1-input.txt") #"\n"))]
    (let [new-calib-freq (+ calib-freq (first changelist))]
      (if (frequencies new-calib-freq)
        new-calib-freq
        (recur new-calib-freq
               (conj frequencies new-calib-freq)
               (if (= (count (rest changelist)) 0)
                 (map read-string (str/split (slurp "../day1-input.txt") #"\n"))
                 (rest changelist)))))))

(defn day2a
  []
  (let [input (str/split (slurp "../day2-input.txt") #"\n")]
    (* ((frequencies (map count2 input)) true)
       ((frequencies (map count3 input)) true))))

(defn mycount
  [count id]
  (some identity (map #(= count %) (vals (frequencies id)))))

(defn count2
  [id]
  (mycount 2 id))

(defn count3
  [id]
  (mycount 3 id))

(defn day2b
  []
  (let [input (str/split (slurp "../day2-input.txt") #"\n")]
    
    ))

(defn common
  "Output string of common characters from two input strings"
  [str1 str2]
  (loop [outstr []
         s1 str1
         s2 str2]
    (if (= (count s1) 0)
      outstr
      (if (= (first s1) (first s2))
        (conj outstr (first s1))
        (recur outstr (rest s1) (rest s2))))))
