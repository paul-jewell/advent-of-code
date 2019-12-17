(ns advent-of-code.core-test
  (:require [clojure.test :refer :all]
            [advent-of-code.core :refer :all]))

;; (deftest a-test
;;   (testing "FIXME, I fail."
;;     (is (= 0 1))))

(deftest fuel-test
  (testing "Fuel calculation according to mass"
    (is (== 2 (fuel 12)))
    (is (== 2 (fuel 14)))
    (is (== 654 (fuel 1969)))
    (is (== 33583 (fuel 100756)))))

(deftest adjusted-fuel-test
  (testing "Corrected fuel calculation according to mass with added fuel"
    (is (== 2 (adjusted-fuel 14)))
    (is (== 966 (adjusted-fuel 1969)))
    (is (== 50346 (adjusted-fuel 100756)))))

(deftest intcomputer-test
  (testing "Test the integer computer"
    (is (= (intcomp [1,0,0,0,99]) [2,0,0,0,99]))
    (is (= (intcomp [2,3,0,3,99]) [2,3,0,6,99]))
    (is (= (intcomp [2,4,4,5,99,0]) [2,4,4,5,99,9801]))
    (is (= (intcomp [1 1 1 4 99 5 6 0 99]) [30 1 1 4 2 5 6 0 99]))))

;; (deftest short-distance-test
;;   (testing "Shortest distance calculation for wire crossing"
;;     (is (== short-distance (R75,D30,R83,U83,L12,D49,R71,U7,L72
;;                                U62,R66,U55,R34,D71,R55,D58,R83) 159))
;;     (is (== short-distance (R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
;;                                U98,R91,D20,R16,D67,R40,U7,R15,U6,R7)) 135)))

(deftest manhatten-test
  (testing "Manhatten distance between two points"
    (is (== 0 (manhatten [3 3] [3 3])))
    (is (== 2 (manhatten [4 3] [2 3])))
    (is (== 2 (manhatten [4 3] [4 5])))))

(deftest direction-test
  (testing "direction functions"
    (is (= '([0 1] [0 2] [0 3] [0 4]) (up [0 0] 4)))
    (is (= '([1 0] [2 0] [3 0] [4 0]) (right [0 0] 4)))
    (is (= '([0 -1] [0 -2] [0 -3] [0 -4]) (down [0 0] 4)))
    (is (= '([-1 0] [-2 0] [-3 0] [-4 0] (left [0 0] 4))))))

(deftest valid-password-test
  (testing "Password validity function"
    (is (contains-double? 197777))
    (is (not (contains-double? 197486)))
    (is (contains-valid-double? 111122))))

#_(deftest advent-of-code-test
  (testing "Correct results are maintained as further refinement is done"
    (is (== 3348909 (day1a)))
    (is (== 5020494 (day1b)))
    (is (== 10566835 (day2a)))
    (is (== 2347 (day2a)))
    (is (== 1640 (day4a)))
    (is (== 1126 (day4b)))))

