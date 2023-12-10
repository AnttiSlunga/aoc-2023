(ns aoc-2023.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; seeds: 858905075 56936593 947763189 267019426 206349064 252409474 660226451 92561087 752930744 24162055 75704321 63600948 3866217991 323477533 3356941271 54368890 1755537789 475537300 1327269841 427659734

;; seeds: 79 14 55 13
;
;seed-to-soil map:
;50 98 2
;52 50 48
;
;soil-to-fertilizer map:
;0 15 37
;37 52 2
;39 0 15
;
;fertilizer-to-water map:
;49 53 8
;0 11 42
;42 0 7
;57 7 4
;
;water-to-light map:
;88 18 7
;18 25 70
;
;light-to-temperature map:
;45 77 23
;81 45 19
;68 64 13
;
;temperature-to-humidity map:
;0 69 1
;1 0 69
;
;humidity-to-location map:
;60 56 37
;56 93 4

(defn find-destination [input seed]
  (let [seed-to-soil
        (->> input
             (mapv (fn [value]
                     (let [[destination source range-lenght] value]
                       (if (and (>= seed source) (<= seed (+ source range-lenght)))
                         [destination source]))))
             (remove nil?)
             flatten
             (into []))]
    (if (empty? seed-to-soil)
      seed
      (+ (- seed (second seed-to-soil)) (first seed-to-soil)))))

(defn find-destination-loop [input seed]
  (let [seed-to-soil
        (->>
          (loop
            [input input
             vals []]
            (if (empty? input)
              vals
              (let [[destination source range-lenght] (first input)]
                (recur (rest input)
                       (if (and (>= seed source) (<= seed (+ source range-lenght)))
                         (conj vals [destination source])
                         vals)))))
          (remove nil?)
          flatten
          (into []))]
    (if (empty? seed-to-soil)
      seed
      (+ (- seed (second seed-to-soil)) (first seed-to-soil)))))

(defn read-resource [name]
  (->> (mapv #(clojure.string/split % #"\n")
             (-> (slurp (io/resource name)) (clojure.string/split #"\n")))
       (mapv (fn [value]
               (let [[destination source range-lenght] (mapv #(Long/parseLong %) (clojure.string/split (clojure.string/trim (first value)) #" "))]
                 [destination source range-lenght])))))

(defn part1 []
  (let [seeds [858905075 56936593 947763189 267019426 206349064 252409474 660226451 92561087 752930744 24162055 75704321 63600948 3866217991 323477533 3356941271 54368890 1755537789 475537300 1327269841 427659734]
        seed-soil (read-resource "d5_seed_to_soil")
        soil-ferti (read-resource "soil_to_ferti")
        ferti-water (read-resource "ferti_to_water")
        water-light (read-resource "water_to_light")
        light-temp (read-resource "light_to_temp")
        temp-humi (read-resource "temp_to_humi")
        humi-location (read-resource "humi_to_location")

        location-id
        (mapv
          #(->> %
               (find-destination-loop seed-soil)
               (find-destination-loop soil-ferti)
               (find-destination-loop ferti-water)
               (find-destination-loop water-light)
               (find-destination-loop light-temp)
               (find-destination-loop temp-humi)
               (find-destination-loop humi-location))
          seeds)]
    (sort location-id)))

(def test-seeds [[79 14] [55 13]])

;; 1 000 000 = 26097 ms
;; yhteensä siemeniä 2037733040
;; eli noi 2038 miljoonaa
;; joten kestää 53185686 ms => 53 185 sekuntia => 886 min => noin 15 h
(defn part2 []
  (let [seeds [[858905075 56936593]
               [947763189 267019426]
               [206349064 252409474]
               [660226451 92561087]
               [752930744 24162055]
               [75704321 63600948]
               [3866217991 323477533]
               [3356941271 54368890]
               [1755537789 475537300]
               [1327269841 427659734]]
        seeda-with-ranges (->> seeds
                               (mapcat (fn [[start end]]
                                         (range start (+ start end)))))
        seed-soil (read-resource "d5_seed_to_soil")
        soil-ferti (read-resource "soil_to_ferti")
        ferti-water (read-resource "ferti_to_water")
        water-light (read-resource "water_to_light")
        light-temp (read-resource "light_to_temp")
        temp-humi (read-resource "temp_to_humi")
        humi-location (read-resource "humi_to_location")

        start (System/currentTimeMillis)

        location-id
        (mapv
          #(->> %
                (find-destination-loop seed-soil)
                (find-destination-loop soil-ferti)
                (find-destination-loop ferti-water)
                (find-destination-loop water-light)
                (find-destination-loop light-temp)
                (find-destination-loop temp-humi)
                (find-destination-loop humi-location))
          seeda-with-ranges)
        _ (println "took" (- (System/currentTimeMillis) start))]
    (first (sort location-id))))
