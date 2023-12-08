(ns day7b
  "Integer computer for advent of code 2019"
  (:require [clojure.string :as str]
            [clojure.set :as set]
            ))

;; My coding for the intcomp found in core.clj works, but is shocking.
;; Following review of others contributions for advent of code 2019,
;; I found ceronman's implementation which I find is very concise
;; and readable.
;; The version here is written out following the code review, and
;; I thank Manuel for making his code visible on github > top marks!

(defn create-computer
  "create the integer computer with the provided program loaded"
  [program input]
  (let [memory (map read-string (str/split program #","))]
    {:pc 0
     :memory (vec memory)
     :output []
     :input input
     :halted false}))

(defn read-word
  "Read the word in the <computer> at <address>"
  [computer address]
  (get-in computer [:memory address]))

(defn write-word
  "Write <value> into the <address> in the <computer>"
  [computer address value]
  (assoc-in computer [:memory address] value))

(defn read-word-relative
  "Read the word in <computer> at the address offset
   referred to in address"
  [computer offset]
  (read-word computer (+ (:pc computer) offset)))

(defn parse-opcode
  "Parse the opcode for instruction and address modes"
  [computer]
  (let [opcode (read-word-relative computer 0)
        instruction (mod opcode 100)
        modes (quot opcode 100)]
    {:instruction instruction :modes modes}))

(defn immediate-mode?
  [modes pos]
  (= 1.0 (mod (quot modes (Math/pow 10 (dec pos))) 10)))

(defn parameter-value
  "Fetch value for parameter"
  [computer position]
  (if (immediate-mode? (:modes (parse-opcode computer)) position)
    (read-word-relative computer position)
    (read-word computer (read-word-relative computer position))))


(defn advance
  [computer size]
  (update computer :pc (partial + size)))

(defn binary-op
  [computer f]
  (let [value1 (parameter-value computer 1)
        value2 (parameter-value computer 2)
        result (f value1 value2)
        dest (read-word-relative computer 3)]
    (-> computer
        (write-word dest result)
        (advance 4))))

(defn input-op
  [computer]
  (let [input-value (first (:input computer))]
    (println input-value)
    (-> (update computer :input rest)
        (write-word (read-word-relative computer 1) input-value)
        (advance 2))))

;; without threading macro...
(defn input-op2
  [computer]
  (let [input-value (first (:input computer))]
    (println input-value)
    (advance (write-word (update computer :input rest)
                         (read-word-relative computer 1) input-value)
             2)))

(defn output-op
  [computer]
  (-> computer
       (update :output conj (parameter-value computer 1))
       (advance 2)))

(defn jump-op
  [computer pred]
  (if (pred (parameter-value computer 1))
    (assoc computer :pc (parameter-value computer 2))
    (advance computer 3)))

(defn run [computer]
  (loop [computer computer]
    (let [opcode (parse-opcode computer)]
      #_(println "Opcode: " opcode)
      (case (:instruction opcode)
        1 (recur (binary-op computer +))
        2 (recur (binary-op computer *))
        3 (if (empty? (:input computer))
            computer ; Yield to the scheduler
            (recur (input-op computer)))
        4 (recur (output-op computer))
        5 (recur (jump-op computer (complement zero?)))
        6 (recur (jump-op computer zero?))
        7 (recur (binary-op computer #(if (< %1 %2) 1 0)))
        8 (recur (binary-op computer #(if (= %1 %2) 1 0)))
        (assoc computer :halted true)))))

(defn add-input [computer input]
  (update computer :input conj input))

(defn scheduler-run [computers]
  (loop [index 0
         computers computers]
    (let [current-computer (run (computers index))
          next-index (mod (inc index) (count computers))
          next-computer (add-input (computers next-index) (last (:output  current-computer)))]
      (if (every? :halted computers)
        (last computers)
        (recur
         next-index
         (-> computers
             (assoc index current-computer)
             (assoc next-index next-computer)))))))

(defn create-computers [program sequence]
  (into [] (map #(create-computer program [%]) sequence)))


(defn run-sequence [program sequence]
  (let [computers (create-computers program sequence)
        computers (update computers 0 add-input 0)]
    (last (:output (scheduler-run computers)))))


(defn permutations [colls]
  (if (= 1 (count colls))
    (list colls)
    (for [head colls
          tail (permutations (disj (set colls) head))]
      (cons head tail))))

(def input (slurp "day7-input.txt"))

(println "Max: " (->> (permutations (range 5 10))
                      (map #(run-sequence input %))
                      (apply max)))
