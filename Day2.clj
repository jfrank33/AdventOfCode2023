(ns AdventOfCode2023.Day2
  "Day 2 Avent of Code 2023. Color Cube Game. 
   
   Part 1: Determine which color cube games
   would be possible if if the bag contained only 12 red cubes, 13 green
   cubes, and 14 blue cubes given an input of known colors and counts
   pulled from a bag per game. The requested output is the sum of all 
   possible game IDs. For example if we are provided games 1-10 and only 
   1, 3, and 7 are possible the output would be 11.
   
   Part 2: Determine what the lowest number of cubes is possible per game,
   then get the power per page and add all games. 
   i.e. If Game 1 has 2 pulls; 2 red, 1 blue, 3 green, 3 red, 4 blue
   The minimum output would be 4 blue, 3 red, 3 green, power of 4*3*3=36"
  (:require
    [clojure.string :as str]))


(def game-regex #"Game (\d+):.*")

(def game-number-regex #"Game (\d+):")


(def color-maxes
  {:red 12
   :green 13
   :blue 14})


(defn read-file
  [input-file]
  (slurp input-file))


(defn split-per-line
  [file-string]
  (str/split-lines file-string))


(defn put-into-color-keys
  [color-pulls]
  (->> color-pulls
       (re-seq #"(?:(\d+) (\w+)(?:,|$))")
       (map (fn [[_ count color]]
              [(keyword color) (Integer. count)]))
       (into {})))


(defn organize-games
  [game]
  (let [vector-of-color-pulls (str/split (str/replace game game-number-regex "") #";")]
    {(second (re-matches game-regex game))
     (map put-into-color-keys vector-of-color-pulls)}))


(defn over-color-max?
  [color-maxes game-pull]

  (every? (fn [[color-max color-count]]
            (if (and color-max color-count)
              (>= color-max color-count)
              true))
          (map (juxt color-maxes game-pull) (keys color-maxes))))


(defn is-possible?
  [color-maxes game]
  ;; there has to be a nicer way to organize the data but im rolling with this for now 
  (let [game-pulls (second (first game))
        game-id (first (first game))]
    (if (every? true? (map (partial over-color-max? color-maxes) game-pulls))
      game-id
      "0")))


(defn get-lowest-possible-cubes
  [game-pull-1 game-pull-2]

  ;; future improvement: instead of using color-maxes keys to get the possible colors
  ;; we could change it to figure out the colors
  (let [vector-comparison (map (fn [color]
                                 {:color color
                                  :count-comparison [(color game-pull-1) (color game-pull-2)]}) (keys color-maxes))]
    (->> (map (fn [game-pull-comparison]
                (let [color (:color game-pull-comparison)
                      [game-pull-1-color-count game-pull-2-color-count] (:count-comparison game-pull-comparison)]
                  (if (and game-pull-1-color-count game-pull-2-color-count)
                    {color (max game-pull-1-color-count game-pull-2-color-count)}
                    ;; takes the only value if one is nil or if they're both nil takes 0 
                    {color (or game-pull-1-color-count game-pull-2-color-count 0)})))
              vector-comparison)
         (reduce merge))))


(defn sum-of-lowest-power-of-cubes
  [game]
  (let [game-pulls (second (first game))
        list-of-lowest-possible-cubes (reduce get-lowest-possible-cubes {} game-pulls)]
    (->> (map second list-of-lowest-possible-cubes)
         (remove nil?)
         (reduce *))))


(defn part1-answer
  []
  (let [organized-games (->> (read-file "adventofcodeday2.txt")
                             (split-per-line)
                             (map organize-games))]
    (->> (map (partial is-possible? color-maxes) organized-games)
         (map #(Integer. (str %)))
         (reduce +))))


(defn part2-answer
  []
  (let [organized-games (->> (read-file "adventofcodeday2.txt")
                             (split-per-line)
                             (map organize-games))]
    (->> (map sum-of-lowest-power-of-cubes organized-games)
         (reduce +))))


(comment 
  (part1-answer)
(part2-answer) 
  )

