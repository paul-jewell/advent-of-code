(ns day9a
  "Integer computer for advent of code 2019"
  (:require [clojure.string :as str]
            [clojure.set :as set]
            ))

;; My coding for the intcomp found in core.clj works, but is shocking.
;; Following review of others contributions for advent of code 2019,
;; I found ceronman's implementation which I find is very concise
;; and readable.
;; The version here is written out following the code review, and
;; I thank Manuel for making your code visible on github > top marks!

(defn create-computer
  "create the integer computer with the provided program loaded"
  [program input]
  (let [instructions (map read-string (str/split program #","))]
    {:pc 0
     :memory (into {} (map-indexed vector (map bigint instructions)))
     :relative-base 0
     :halted false
     :output []
     :input input}))

(defn read-word [computer address]
  (get-in computer [:memory address] 0))

(defn write-word [computer address value]
  (assoc-in computer [:memory address] (bigint value)))

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
    {:instruction instruction
     :modes modes}))

(defn parameter-mode [modes pos]
  (int (mod (quot modes (Math/pow 10 (dec pos))) 10)))

(defn parameter-address [computer position]
  (case (parameter-mode (:modes (parse-opcode computer)) position)
    0 (read-word-relative computer position)
    2 (+ (:relative-base computer) (read-word-relative computer position))
    (throw (Exception. "Unknown parameter address"))))

(defn parameter-value [computer position]
  (case (parameter-mode (:modes (parse-opcode computer)) position)
    0 (read-word computer (parameter-address computer position))
    1 (read-word-relative computer position)
    2 (read-word computer (parameter-address computer position))
    (throw (Exception. "Unknown parameter address"))))

(defn advance
  [computer size]
  (update computer :pc (partial + size)))

(defn binary-op
  [computer f]
  (let [value1 (parameter-value computer 1)
        value2 (parameter-value computer 2)
        result (f value1 value2)
        dest (parameter-address computer 3)]
    (-> computer
        (write-word dest result)
        (advance 4))))

(defn input-op
  [computer]
  (let [input-value (first (:input computer))]
    (-> (update computer :input rest)
        (write-word (parameter-address computer 1) (bigint  input-value))
        (advance 2))))

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

(defn rel-base-update-op
  [computer]
  (-> computer
      (update :relative-base + (parameter-value computer 1))
      (advance 2)))

(defn run [computer]
  (loop [computer computer]
    (let [opcode (parse-opcode computer)]
      #_(println "Opcode: " opcode)
      (case (:instruction opcode)
        1 (recur (binary-op computer +))
        2 (recur (binary-op computer *))
        3 (if (empty? (:input computer))
            computer  ; yield to scheduler
            (recur (input-op computer)))
        4 (recur (output-op computer))
        5 (recur (jump-op computer (complement zero?)))
        6 (recur (jump-op computer zero?))
        7 (recur (binary-op computer #(if (< %1 %2) 1 0)))
        8 (recur (binary-op computer #(if (= %1 %2) 1 0)))
        9 (recur (rel-base-update-op computer))
        99 (assoc computer :halted true)
        (throw (Exception. "Unknown instruction"))))))

(defn add-input
  [computer input]
  (update computer :input conj input))

(def input (slurp "day9-input.txt"))
;(def input "1102,34915192,34915192,7,4,7,99,0")

(defn day9a [] (:output (run (create-computer input [2]))))
