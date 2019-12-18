(ns advent-of-code.core
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
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

(defn intcomp-input
  [filename]
  (into []  (map read-string (into [] (clojure.string/split (slurp filename) #",")))))

(defn input-value ;; Get value from user - no validation...
  []
  (print "Input value: ") (Integer/parseInt (read-line)))

(defn output-value
  [value]
  (println "Program output" value))

(defn decode-opcode
  [opcode]
  (let [opstring (format "%05d" opcode)]
    #_(println opstring)
    [(Integer/parseInt (subs opstring 3))
     (read-string (subs opstring 2 3))
     (read-string (subs opstring 1 2))
     (read-string (subs opstring 0 1))]))

(defn opcode1
  [prog pc]
  (let [[opcode op1mode op2mode op3mode] (decode-opcode (nth prog pc))

        op1value (if (= op1mode 0)
                   (nth prog (nth prog (+ pc 1)))  ; Lookup in program 
                   (nth prog (+ pc 1)))       ; Take value directly
        op2value (if (= op2mode 0)
                   (nth prog (nth prog (+ pc 2)))
                   (nth prog (+ pc 2)))
        destination (nth prog (+ pc 3))]
    #_(println "opcode: " opcode "op1: " op1value " (" op1mode "); op2: " op2value " (" op2mode "); dest: " destination)
    [(assoc prog destination (+ op1value op2value)) (+ pc 4)]))

(defn opcode2
  [prog pc]
  (let [[opcode op1mode op2mode op3mode] (decode-opcode (nth prog pc))
        op1value (if (= op1mode 0)
                   (nth prog (nth prog (+ pc 1)))
                   (nth prog (+ pc 1)))
        op2value (if (= op2mode 0)
                   (nth prog (nth prog (+ pc 2)))
                   (nth prog (+ pc 2)))
        destination (nth prog (+ pc 3))]
    #_(println "opcode: " opcode "op1: " op1value " (" op1mode "); op2: " op2value " (" op2mode "); dest: " destination)
    [(assoc prog destination (* op1value op2value)) (+ pc 4)]))

(defn opcode3
  [prog pc]
  (let [[opcode op1mode _ _] (decode-opcode (nth prog pc))
        destination (nth prog (+ pc 1))]
    #_(println "opcode: " opcode " (input) dest: " destination)
    [(assoc prog destination (input-value)) (+ pc 2)]))

(defn opcode4
  [prog pc]
  (let [[opcode op1mode _ _] (decode-opcode (nth prog pc))
        value (if (= op1mode 0)
                (nth prog (nth prog (+ pc 1)))
                (nth prog (+ pc 1)))]
    (output-value value)
    #_(println "opcode: " opcode  " (output) pc: " pc "; output-value: " value)
    #_(if (= pc 24) (println "0:" (prog 0) "; 2: " (prog 2) "; 223: " (prog 223) "; 224: " (prog 224) "; 225: " (prog 225)))
    [prog (+ pc 2)]))

(defn opcode5
  "implement jump-if-true - if the value in op1 is not zero, change the PC to the address referenced in op2"
  [prog pc]
  (let [[opcode op1mode op2mode _] (decode-opcode (nth prog pc))
        predicate-value (if (=  op1mode 0)
                          (nth prog (nth prog (+ pc 1)))
                          (nth prog (+ pc 1)))
        jump-address (if (= op2mode 0)
                       (nth prog (nth prog (+ pc 2)))
                       (nth prog (+ pc 2)))]
    (if (not= predicate-value 0)
      [prog jump-address] ; update the PC to the jump address
      [prog (+ pc 3)]))) ; advance the PC by 3

(defn opcode6
  "implement jump-if-false"
  [prog pc]
  (let [[opcode op1mode op2mode _] (decode-opcode (nth prog pc))
        predicate-value (if (= op1mode 0)
                          (nth prog (nth prog (+ pc 1)))
                          (nth prog (+ pc 1)))
        jump-address (if (= op2mode 0)
                       (nth prog (nth prog (+ pc 2)))
                       (nth prog (+ pc 2)))]
    #_(println "Predicate-value: " predicate-value "; jump address: " jump-address)
    (if (= predicate-value 0)
      [prog jump-address]  ; update the PC to the jump address
      [prog (+ pc 3)])))   ; advance the PC past the instruction

(defn opcode7
  "implement less-than: if [opcode1] < [opcode2] 1 => [destination] else 0 => [destination]"
  [prog pc]
  (let [[opcode op1mode op2mode op3mode] (decode-opcode (nth prog pc))
        op1value (if (= op1mode 0)
                   (nth prog (nth prog (+ pc 1)))
                   (nth prog (+ pc 1)))
        op2value (if (= op2mode 0)
                   (nth prog (nth prog (+ pc 2)))
                   (nth prog (+ pc 2)))
        destination (nth prog (+ pc 3))]
    (if (< op1value op2value)
      [(assoc prog destination 1) (+ pc 4)]
      [(assoc prog destination 0) (+ pc 4)])))

(defn opcode8
  "implement equals - if [opcode1] == [opcode2] 1 => [destination] else 0 => [destination]"
    [prog pc]
  (let [[opcode op1mode op2mode op3mode] (decode-opcode (nth prog pc))
        op1value (if (= op1mode 0)
                   (nth prog (nth prog (+ pc 1)))
                   (nth prog (+ pc 1)))
        op2value (if (= op2mode 0)
                   (nth prog (nth prog (+ pc 2)))
                   (nth prog (+ pc 2)))
        destination (nth prog (+ pc 3))]
    #_(println "pc: " pc " op1value: " op1value " op2value: " op2value " destination: " destination)
    (if (= op1value op2value)
      [(assoc prog destination 1) (+ pc 4)]
      [(assoc prog destination 0) (+ pc 4)])))


(defn intcomp
  ([prog]
   (intcomp prog 0))
  ([p i]
   (loop [prog p
          index i]
     (if (or (= (nth prog index) 99)
             (>= index (count prog))) ; exit code
         prog
         (let [[opcode _ _ _] (decode-opcode (nth prog index))]
           (case opcode
             1 (let [[prog' index'] (opcode1 prog index)]
                 (recur prog' index'))
             2 (let [[prog' index'] (opcode2 prog index)]
                 (recur prog' index'))
             3 (let [[prog' index'] (opcode3 prog index)]
                 (recur prog' index'))
             4 (let [[prog' index'] (opcode4 prog index)]
                 (recur prog' index'))
             5 (let [[prog' index'] (opcode5 prog index)]
                 (recur prog' index'))
             6 (let [[prog' index'] (opcode6 prog index)]
                 (recur prog' index'))
             7 (let [[prog' index'] (opcode7 prog index)]
                 (recur prog' index'))
             8 (let [[prog' index'] (opcode8 prog index)]
                 (recur prog' index'))))))))


;; Once you have a working computer, the first step is to restore the
;; gravity assist program (your puzzle input) to the "1202 program alarm"
;; state it had just before the last computer caught fire. To do this, before
;; running the program, replace position 1 with the value 12 and replace
;; position 2 with the value 2. What value is left at position 0 after the program halts?

(defn day2a
  []
  ((intcomp (assoc (assoc (intcomp-input "day2-input.txt") 1 12) 2 2)) 0))


(defn day2b
  []
  (let [target 19690720
        prog (intcomp-input "day2-input.txt")]
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

(defn day4b
  []
  (count (for [n (range day4-min (+ day4-max 1))
               :when (and (valid-password? n)
                          (contains-valid-double? n))]
           n)))

(defn day5
  []
  (intcomp (intcomp-input "day5-input.txt"))
  println "Completed")

;;--------------------------------------------------------------------
;; Solution to day6 from ceronman/adventofcode on github
;; For educational purposes for me. Very concise, neat solution
;; - Thanks Manuel!

(def input (slurp "day6-input.txt"))

(defn load-orbits [input]
  (->> input
       (str/split-lines)
       (map #(reverse (str/split % #"\)")))
       (reduce (fn [orbits [from to]] (assoc orbits from to)) {})))


(defn count-orbits [orbits object]
  (loop [o object
         count 0]
    (if (= o "COM")
      count
      (recur (get orbits o) (inc count)))))

(defn day6a
 []
 (let [orbits (load-orbits input)
       objects (keys orbits)
       counts (map #(count-orbits orbits %) objects)]
   (reduce + counts)))

;;--------------------------------------------------------------------
;; Part 2 solution also from Manuel

(defn add-orbit [orbits [from to]]
  (-> orbits
      (update from (fnil conj #{}) to)
      (update to (fnil conj #{}) from)))

(defn load-orbits2 [input]
  (->> input
       (str/split-lines)
       (map #(reverse (str/split % #"\)")))
       (reduce add-orbit {})))

(defn shortest-path [orbits start end]
  (loop [queue [[0 start #{}]]]
    (let [[steps object visited] (first queue)]
      (if (= object end)
        steps
        (recur (into 
                (rest queue)
                (->> (get orbits object)
                     (filter (complement visited))
                     (map #(vector (inc steps) % (conj visited object))))))))))


;; without threading macro
(defn shortest-path2 [orbits start end]
  (loop [queue [[0 start #{}]]]
    (let [[steps object visited] (first queue)]
      (if (= object end)
        steps
        (recur (into (rest queue)
                     (map #(vector (inc steps) % (conj visited object))
                          (filter (complement visited)
                                  (get orbits object)))))))))

(defn day6b
  []
  (let [orbits (load-orbits2 input)
           start (first (get orbits "YOU"))
           end (first (get orbits "SAN"))]
       (shortest-path2 orbits start end)))
;;--------------------------------------------------------------------

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


