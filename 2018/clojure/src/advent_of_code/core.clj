(ns advent-of-code.core
  (:require [clojure.string :as str])
  (:gen-class))


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

(defn mycount
  [count id]
  (some identity (map #(= count %) (vals (frequencies id)))))

(defn count2
  [id]
  (mycount 2 id))

(defn count3
  [id]
  (mycount 3 id))


(defn day2a
  []
  (let [input (str/split (slurp "../day2-input.txt") #"\n")]
    (* ((frequencies (map count2 input)) true)
       ((frequencies (map count3 input)) true))))




(defn common
  "Output string of common characters from two input strings"
  [str1 str2]
  (loop [outstr []
         s1 str1
         s2 str2]
    (if (<= (count s1) 0)
      outstr
      (if (= (first s1) (first s2))
        (recur (conj outstr (first s1)) (rest s1) (rest s2))
        (recur outstr (rest s1) (rest s2))))))

(defn list-compare
  [leftstr rightlist]
  (loop [rlist rightlist]
    (if (= (count rlist) 0)
      nil
      (let [result-str (apply str (common leftstr (first rlist)))]
               (if (= (count result-str) 25) ; We have a solution
                 result-str
                 (recur (next rlist)))))))

(defn day2b
  []
  (let [input (str/split (slurp "../day2-input.txt") #"\n")]
    (loop
        [leftstr input
         rightstr input]
      (let [result-str (list-compare (first leftstr) rightstr)]
        (if result-str
          result-str
          (recur (rest leftstr) (rest rightstr)))))))

;;;;===========================Day 3===============================
(defn parse-claim [s]
  (->> (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" s)
       (drop 1)
       (map read-string)
       (zipmap [:id :x :y :w :h])))

(defn parse-input [s]
  (map parse-claim (str/split-lines s)))

(defn claim-to-points [{:keys [id x y w h] :as claim}]
  (for [px (range w)
        py (range h)]
    {:id id :x (+ x px) :y (+ y py)}))

(defn overlap-points [claims]
  (let [points (mapcat claim-to-points claims)]
    (->> points
         (map (juxt :x :y))
         frequencies
         (remove #(= (val %) 1))
         count)))

(defn non-overlap-claim [claims]
  (let [points (mapcat claim-to-points claims)
        overlapping-ids (->> points
                             (group-by (juxt :x :y))
                             (filter #(> (count (val %)) 1))
                             vals
                             (mapcat #(map :id %))
                             set)]
    (some #(when-not (contains? overlapping-ids (:id %))
             (:id %)) claims)))

(defn day3a
  []
  (let [input (parse-input (slurp "../day3-input.txt"))]
    (overlap-points input)))

(defn day3b
  []
  (let [input (parse-input (slurp "../day3-input.txt"))]
    (non-overlap-claim input)))

;;;;==============================================================


(defn -main
  "Solve Advent of Code 2018"
  [& args]
  (printf "Day1a: Calibration frequency: %d\n" (day1a))
  (printf "Day1b: First repeating frequency: %d\n" (day1b))
  (printf "Day2a: Box ID checksum: %d\n" (day2a))
  (printf "Day2b: Common box ID letters: %s\n" (apply str (day2b)))
  (printf "Day3a: ...")
  )


