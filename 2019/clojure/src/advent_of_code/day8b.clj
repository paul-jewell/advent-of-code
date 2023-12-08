(ns day8b
  (:require [clojure.string :as str]))

;; We want to read the vectors of data in sequence, and store the
;; first non-2 value in that position in the image

(def input (map #(Character/digit % 10) (slurp "day8-input2.txt")))


(def width 25)
(def height 6)

(def layers (partition (* width height) input))


(defn pixel-set [a b]
  (if (= a 2)
    b
    a))

(defn calculate-image [layers]
  (loop [layers layers
         image (vec (replicate (* width height) 2))]
    #_(println (take 10 image))
    (if (empty? layers)
      image
      (recur (rest layers) (map pixel-set image (first layers))))))

(map println (map #(apply str %) (partition width (apply str (replace {2 " " 0 " " 1 \█} (calculate-image layers))))))
