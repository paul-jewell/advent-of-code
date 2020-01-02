(ns day10
  (:require [clojure.string :as str]))

(def input  (str/split-lines (slurp "day10-input.txt")))

(def input-vec (vec (map vec input)))
(def my-vec (into [] (map vec  (partition 10 (range 100)))))

(defn in-field? [field x y]
  (and (>= x 0)
         (>= y 0)
         (< x (count (field 0)))
         (< y (count field))))

(defn test-vec-fill []
  (let [x-offset -1
        y-offset -1]
    (loop [asteroid-field my-vec
           curr-x 8
           curr-y 7]
      (if (not (in-field? asteroid-field curr-x curr-y))
        asteroid-field
        (recur (assoc-in asteroid-field [curr-x curr-y] ".")
               (+ curr-x x-offset)
               (+ curr-y y-offset))))))

(defn count-asteroids [field]
  (apply + (map #(% \# 0) (map frequencies field))))

(defn on-line?
  [col row col1 row1 col2 row2]
  (if (= col1 col)
    (= col2 col)
    (if (= row1 row)
      (= row2 row)
      (let [m (/ (- row1 row) (- col1 col))]
        (= row2 (+ (* m col2) (- row (* m col))))))))

(defn same-direction?
  "Confirm the point is along the same line direction"
  [col row col1 row1 col2 row2]
  (let [x1 (- col1 col)
        y1 (- row1 row)
        x2 (- col2 col)
        y2 (- row2 row)]
    #_(println "x1: " x1 " ; x2: " x2 " ; y1: " y1 " ; y2: " y2)
    (and  (= (Math/signum (float x1)) (Math/signum (float x2)))
          (= (Math/signum (float y1)) (Math/signum (float y2))))))

(defn point-distance [x y x1 y1]
  (Math/sqrt (+ (* (- x1 x) (- x1 x)) (* (- y1 y) (- y1 y)))))

(defn hide-asteroids
  "Hide asteroids masked by visible asteroid when viewed from base"
  [field base-col base-row vis-col vis-row]
  (let [visible-distance (point-distance base-col base-row vis-col vis-row)]
    (loop [field field
           curr-col 0
           curr-row 0]
      (if (not (in-field? field curr-col curr-row))
        field
        (recur (if (and (on-line? base-col base-row vis-col vis-row curr-col curr-row)
                        (same-direction? base-col base-row vis-col vis-row curr-col curr-row)
                        (> (point-distance base-col base-row curr-col curr-row) visible-distance))
                 (assoc-in field [curr-row curr-col] \-) ;; Mask asteroid
                 field)
               (if (>= curr-col (- (count (field 0)) 1))
                 0
                 (+ curr-col 1))
               (if (>= curr-col (- (count (field 0)) 1))
                 (+ curr-row 1)
                 curr-row))))))

;; Need a function which masks off all asteroids for a given position
;; so the number of asteroids visible can be counted (function above)

(defn mask-asteroids-row [field row base-col base-row]
  (loop [field field
         col 0]
    (if (>= col (count (field row)))
      field
      (recur (if (and (not (and (= col base-col) (= row base-row))) ;; Don't process asteroid at proposed base
                      (= (get-in field [row col]) \#))          ;; we have an asteroid here
               (hide-asteroids field base-col base-row col row)
               field)
             (+ col 1)))))

(defn mask-asteroids [field base-col base-row]
  (loop [field field
         row 0]
    (if (>=  row (count field))
      field ;; return field
      (recur
       (mask-asteroids-row field row base-col base-row) (+ row 1)))))



;; Walks through the whole field, testing the asteroids for each base
(defn walk-field [field]
  (for [base-col (range (count (field 0)))
        base-row (range (count field))
        :when (= (get-in field [base-row base-col]) \#)] ;; Base is on an asteroid
    [base-col base-row (- (count-asteroids (mask-asteroids field base-col base-row)) 1)]))

(def test-input ["......#.#."
                 "#..#.#...."
                 "..#######."
                 ".#.#.###.."
                 ".#..#....."
                 "..#....#.#"
                 "#..#....#."
                 ".##.#..###"
                 "##...#..#."
                 ".#....####"])
(def test-input-vec (vec (map vec test-input)))



(def test-input-2 [".#..#"
                   "....."
                   "#####"
                   "....#"
                   "...##"])
(def test-input-2-vec (vec (map vec test-input-2)))

(def test-input-3 ["###"
                   "###"
                   "###"])


(def test-input-3-vec (vec (map vec test-input-3)))

(def test-input-4
  ["#.#...#.#."
   ".###....#."
   ".#....#..."
   "##.#.#.#.#"
   "....#.#.#."
   ".##..###.#"
   "..#...##.."
   "..##....##"
   "......#..."
   ".####.###."])

(def test-input-4-vec (vec (map vec test-input-4)))


;;================================================================================
;; Below is Ceronman's solution - for education purposes
;; slightly modified where I have used similar code above
;; Key points -
;;   * Use of map to hold the data
;;   * Creating a list of asteroid coordinates. No need to read
;;     the text information multiple times
;;   * Clever use of distances to confirm if an asteroid is in the way
;;   * map & apply not loop and for
;;   * Delivering the base location as well as the count

(defn parse-map [input]
  (let [lines input
        width (count (first lines))
        height (count lines)
        asteroids (into #{} (for [x (range width)
                                  y (range height)
                                  :when (= (first "#") (nth (nth lines y) x))]
                              [x y]))]
    {:width width :height height :asteroids asteroids}))

(defn abs [n] (max n (- n)))

(defn distance [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2)))))
;; Clever - use the distances to judge whether point c is in between a and b!
;; Avoid all the issues I had above with ensuring the point is the correct side
;; of the line
(defn point-in-between? [a b c]
  (> 0.0000001 (abs (- (+ (distance a c) (distance c b)) (distance a b)))))

(defn blocked? [map [x1 y1] [x2 y2]]
  (let [minx (min x1 x2)
        maxx (max x1 x2)
        miny (min y1 y2)
        maxy (max y1 y2)]
    
    (not-empty (for [x (range minx (inc maxx))
                     y (range miny (inc maxy))
                     :when (and (point-in-between? [x1 y1] [x2 y2] [x y])
                                (contains? (:asteroids map) [x y])
                                (not= [x y] [x1 y1])
                                (not= [x y] [x2 y2]))]
                 [x y]))))


(defn reachable [map point]
  (count (remove #(blocked? map point %) (disj (:asteroids map) point))))

(time (let [m (parse-map input)]
        (apply max-key second (map #(vector % (reachable m %)) (:asteroids m)))))

