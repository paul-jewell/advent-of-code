(ns day10)

(def input (str/split-lines (slurp "day10-input.txt")))

(def base [13 17])

(defn parse-map [input]
  (let [lines input
        width (count (first lines))
        height (count lines)
        asteroids (into #{} (for [x (range width)
                                  y (range height)
                                  :when (= (first "#") (nth (nth lines y) x))]
                              [x y]))]
    {:width width :height height :asteroids asteroids}))

(defn answer [point]
  (+ (* (point 0) 100) (point 1)))

(defn angle [[x1 y1] [x2 y2]]
  (let [theta (Math/toDegrees (Math/atan2 (- x2 x1) (- y1 y2)))]
    (if (< theta 0.0)
      (+ 360 theta)
      theta)))

(defn my-distance [a b]
  (let [x (- (b 0) (a 0))
        y (- (b 1) (a 1))]
    (Math/sqrt (+ (* x x) (* y y)))))

(defn distance [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2)))))



(defn point-in-between? [a b c]
  (> 0.0000001 (abs (- (+ (distance a c) (distance c b)) (distance a b)))))

(defn blocked? [asteroids [x1 y1] [x2 y2]]
  (let [minx (min x1 x2)
        maxx (max x1 x2)
        miny (min y1 y2)
        maxy (max y1 y2)]

    (not-empty (for [x (range minx (inc maxx))
                     y (range miny (inc maxy))
                     :when (and (point-in-between? [x1 y1] [x2 y2] [x y])
                                (contains? asteroids [x y])
                                (not= [x y] [x1 y1])
                                (not= [x y] [x2 y2]))]
                 [x y]))))

(defn reachable [asteroids point]
  (remove #(blocked? asteroids point %) (disj asteroids point)))

(answer (let [asteroids (:asteroids (parse-map input))
              point  [13 17]]
          (loop [asteroids asteroids
                 vaporized []]
            (if (= (count asteroids) 1)
              (nth vaporized 199)
              (recur (apply disj asteroids (reachable asteroids point))
                     (into vaporized (sort-by #(angle point %) (reachable asteroids point))))))))
