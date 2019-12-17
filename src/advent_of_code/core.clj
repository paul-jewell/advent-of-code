(ns advent-of-code.core
  (:require [clojure.set :as set])
  (:gen-class))

(defn fuel
  "Calculate fuel requirement based on mass"
  [mass]
  (int (- (Math/floor (/ mass 3)) 2)))
  
;; Read data file into vector
(defn day1-input [filename]
  (map read-string (into [] (clojure.string/split (slurp filename) #"\n"))))

(defn day1a
  []
  (reduce + (map fuel (day1-input "day1-input.txt"))))

(defn adjusted-fuel
  "Calculate the fuel required for the module mass including fuel"
  [mass]
  (loop [m mass
         acc 0]
    (if (<= (fuel m) 0)
      acc
      (recur (fuel m) (+ acc (fuel m))))))

(defn day1b
  []
  (reduce + (map adjusted-fuel (day1-input "day1-input.txt"))))

(defn day2-input
  [filename]
  (into []  (map read-string (into [] (clojure.string/split (slurp filename) #",")))))


(defn intcomp
  ([prog]
   (intcomp prog 0))
  ([p i]
   (loop [prog p
          index i]
;    (println "Initial values: " prog index)
       (if (= (nth prog index) 99)
         prog
         (let [opcode (nth prog index)
               func (case opcode
                      1 #'+
                      2 #'*)
               op1 (nth prog (+ index 1))
               op2 (nth prog (+ index 2))
               dest (nth prog (+ index 3))]
;           (println opcode op1 op2 dest)
           (recur (assoc prog dest (func (prog op1) (prog  op2))) (+ index 4)))))))


;; Once you have a working computer, the first step is to restore the
;; gravity assist program (your puzzle input) to the "1202 program alarm"
;; state it had just before the last computer caught fire. To do this, before
;; running the program, replace position 1 with the value 12 and replace
;; position 2 with the value 2. What value is left at position 0 after the program halts?

(defn day2a
  []
  ((intcomp (assoc (assoc (day2-input "day2-input.txt") 1 12) 2 2)) 0))


(defn day2b
  []
  (let [target 19690720
        prog (day2-input "day2-input.txt")]
    (for [noun (range 100)
          verb (range 100)
          :when (== target (nth (intcomp (assoc (assoc prog 1 noun) 2 verb)) 0))]
      (+ (* 100 noun) verb))))

;; Day3 - Manhatten distance measurement
;;
;; Points are vectors [x y]

(defn manhatten
  [point-a point-b]
  (+ (Math/abs (- (first point-a) (first point-b)))
     (Math/abs (- (second point-a) (second  point-b)))))

(defn up
  [curr-pos qty]
  (for [n (range qty)]
    [(first curr-pos) (+ (second curr-pos) (+ n 1))]))

(defn down
  [curr-pos qty]
  (for [n (range qty)]
    [(first curr-pos) (- (second curr-pos) (+ n 1))]))

(defn right
  [curr-pos qty]
  (for [n (range qty)]
    [(+ (first curr-pos) (+ n 1)) (second curr-pos)]))

(defn left
  [curr-pos qty]
  (for [n (range qty)]
    [(- (first curr-pos) (+ n 1)) (second curr-pos)]))


(defn str-to-path
  [p-str]
  (loop [curr-pos [0 0]
         routes (into [] (clojure.string/split p-str #","))
         path '([0 0])]
    (if (=  (count routes) 0)
      path
      (let [next-pos-str (first routes)
            dir (first next-pos-str)
            qty (read-string (subs next-pos-str 1))
            path' (case dir
                    \U (up curr-pos qty)
                    \D (down curr-pos qty)
                    \R (right curr-pos qty)
                    \L (left curr-pos qty))]
        (recur (last path') (rest routes) (concat path path'))))))

(defn day3-input
  [filename]
  (into [] (clojure.string/split (slurp filename) #"\n")))



(defn day3a
 []
 (let [lines (day3-input "day3-input.txt")
       first-line (first lines)
       second-line (second lines)]
   ;; drop to remove [0 0] from the intersection of the two lines
   (apply min (drop 1  (map #(manhatten [0 0] %1)
                            (clojure.set/intersection (into #{} (str-to-path first-line))
                                                      (into #{} (str-to-path second-line))))))))

(defn steps-to-point
  [point path]
  (loop [cur-point (first path)
         route (rest path)
         step-num 0]
;    (println step-num "Route: " route "; Cur-point: " cur-point "; point: " point)
    (if (= 0 (count route))
         -1
         (if (and (= (first point) (first cur-point)) (= (second point) (second cur-point)))
           step-num
           (recur (first route) (rest route) (+ 1 step-num))))))

;; This works, but is slow. It walks the path from the beginning for each point.
;; It would be more efficient to check for each point as the path is walked.
;; The correct answer is 8684 (identified by this routine)
(defn day3b
  []
  (let [lines (day3-input "day3-input.txt")
        first-line (first lines)
        second-line (second lines)
        first-line-path (str-to-path first-line)
        second-line-path (str-to-path second-line)
        intersections (clojure.set/intersection (into #{} first-line-path)
                                                (into #{} second-line-path))]
;    (steps-to-point (first (drop 1 intersections)) first-line-path)
    (apply min (map +
                    (map #(steps-to-point %1 first-line-path) (drop 1 intersections))
                    (map #(steps-to-point %1 second-line-path) (drop 1 intersections))))))

;; --- Day 4: Secure Container ---

;; You arrive at the Venus fuel depot only to discover it's protected by a password.
;; The Elves had written the password on a sticky note, but someone threw it out.

;; However, they do remember a few key facts about the password:

;;     - It is a six-digit number.
;;     - The value is within the range given in your puzzle input.
;;     - Two adjacent digits are the same (like 22 in 122345).
;;     - Going from left to right, the digits never decrease; they only ever increase
;;       or stay the same (like 111123 or 135679).

;; Other than the range rule, the following are true:

;;     111111 meets these criteria (double 11, never decreases).
;;     223450 does not meet these criteria (decreasing pair of digits 50).
;;     123789 does not meet these criteria (no double).

;; How many different passwords within the range given in your puzzle input meet these criteria?

;; Input range 197487-673251

(def day4-min 197487)
(def day4-max 673251) ; 666999 is actually the highest valid input

(defn contains-double?
  [password]
  (loop [pass (str password)]
    (if (= 1 (count pass))
      false
      (if (= (first pass) (second pass))
        true
        (recur (rest pass))))))


(defn increasing?
  [password]
  (loop [pass (str password)]
    (if (= (count pass) 1)
      true
      (if (<= (int (first pass)) (int (second  pass)))
        (recur (rest pass))
        false))))

(defn valid-password?
  [password]
  (and (>= password day4-min)
       (<= password day4-max)
       (contains-double? password)
       (increasing? password)))

(defn day4a
  []
  (count (for [n (range day4-min (+ day4-max 1))
               :when (valid-password? n)]
           n)))

(defn treble?
  [a b c]
  (= a b c))

(defn contains-valid-double?
  [password]
  (loop [pass (str password)]
    (if (<= (count pass) 1)
      false
      (if (and (> (count pass) 2)
               (= (first pass) (second pass))
               (not (treble? (first pass) (second pass) (nth pass 2))))
        true  ; valid double without being part of larger group
        (if (and (> (count pass) 2)
                 (treble? (first pass) (second pass) (nth pass 2)))    
          (recur (drop-while #(= % (first pass)) pass))
          (if (= (first pass) (second pass))
            true
            (recur (rest pass))))))))


(defn my-test2
  []
  (drop-while #(= %1 \1) "111233"))

(defn day4b
  []
  (count (for [n (range day4-min (+ day4-max 1))
               :when (and (valid-password? n)
                          (contains-valid-double? n))]
           n)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Advent of Code 2019 Solutions in clojure")
  (println "----------------------------------------")
  (println "Day 1a: " (day1a))
  (println "Day 1b: " (day1b))
  (println "Day 2a: " (day2a))
  (println "Day 2b: " (first (day2b)))
  (println "Day 3a: " (day3a))
  (println "Day 3b: " (day3b))
  (println "Day 4a: " (day4a))
  (println "Day 4b: " (day4b)))


