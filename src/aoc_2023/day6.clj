(ns aoc-2023.day6)
; Time:        53     89     76     98
; Distance:   313   1090   1214   1201

; Time:      7  15   30
; Distance:  9  40  200

(defn ways-to-win [time distance]
  (->> (range 0 (inc distance))
       (map (fn [ms]
              (let [speed (* ms 1)
                    moves (* speed (- time ms))]
                (if (> moves distance)
                  ms))))
       (remove nil?)
       count))

(defn part1 []
  (let [race1 (ways-to-win 53 313)
        race2 (ways-to-win 89 1090)
        race3 (ways-to-win 76 1214)
        race4 (ways-to-win 98 1201)
        ways [race1 race2 race3 race4]]
    (apply * ways)))

;Time:      71530    53897698
;Distance:  940200   313109012141201
(defn part2 []
  (let [lower-limit
        (loop
          [ms 0
           first-win 0]
          (if (not (= 0 first-win))
            first-win
            (let [speed (* ms 1)
                  moves (* speed (- 53897698 ms))]
              (recur
                (inc ms)
                (if (> moves 313109012141201)
                  ms 0)))))
        upper-limit
        (loop
          [ms 53897698
           first-win 0]
          (if (not (= 0 first-win))
            first-win
            (let [speed (* ms 1)
                  moves (* speed (- 53897698 ms))]
              (recur
                (dec ms)
                (if (> moves 313109012141201)
                  ms 0)))))]
    (inc (- upper-limit lower-limit))))