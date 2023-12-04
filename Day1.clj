(ns AdventOfCode2023.Day1
  "Day 1 Avent of Code 2023. Recalibrate the coordinates.
   
   Part 1: Get the first and last digit and combine them.
   Ex: one1twothree4five; 14
   one1twothreefourfive; 11
   
   Part 2: Include one-zero text representations of values.
   Ex: twonethree53seven; 27
   twoabcdefg; 22
   eighthreeight4; 84"
  (:require
    [clojure.string :as str]))


;; ;; TODO: this is gross :) 
(defn convert-text-numbers
  [file-string]
  (-> (str/replace file-string "one" "one1one")
      (str/replace "two" "two2two")
      (str/replace "three" "three3three")
      (str/replace "four" "four4four")
      (str/replace "five" "five5five")
      (str/replace "six" "six6six")
      (str/replace "seven" "seven7seven")
      (str/replace "eight" "eight8eight")
      (str/replace "nine" "nine9nine")))


(defn get-numbers
  [file-string]
  (re-seq #"\d+" file-string))


(defn convert-to-int
  [number-string]
  (Integer. (str number-string)))


(defn get-first-last
  [number-string]
  (let [first (convert-to-int (first number-string))
        last (convert-to-int (last number-string))]
    (+ (* 10 first)
       last)))


(defn get-coordinates
  [list-of-numbers]
  (map get-first-last list-of-numbers))


(defn part1-answer
  []
  (->>
    (slurp "adventofcodeday1.txt")
    (str/split-lines)
    (map get-numbers)
    (map #(apply str %))
    (get-coordinates)
    (reduce +)))


(defn part2-answer
  []
  (->>
    (slurp "adventofcodeday1.txt")
    (str/split-lines)
    (map convert-text-numbers)
    (map get-numbers)
    (map #(apply str %))
    (get-coordinates)
    (reduce +)))


(comment 
  
    (part1-answer)
  (part2-answer) 

  (-> (convert-text-numbers 
       "mkskfeighthreeightsldfkjsninetwonine29twone")
      (get-numbers))  ;; 8389292921
  )


