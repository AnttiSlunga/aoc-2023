(ns aoc-2023.day7
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:import (java.util.regex Pattern)))

;Five of a kind, where all five cards have the same label: AAAAA
;Four of a kind, where four cards have the same label and one card has a different label: AA8AA
;Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
;Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
;Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
;One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
;High card, where all cards' labels are distinct: 23456

(def games-order
  {:five 25
   :four 16
   :fullhouse 13
   :three 9
   :two-pair  8
   :pair 4
   :high 0})

(defn game [hand]
  (let [games
        (if (= hand "JJJJJ")
          [25]
          (->> (loop
                 [hand hand
                  game []]
                 (if (or (empty? hand) (= hand "J") (= hand "JJ") (= hand "JJJ") (= hand "JJJJ"))
                   game
                   (let [card (loop
                                [card nil
                                 hand hand]
                                (if (some? card)
                                  card
                                  (let [card (if-not (= "J" (str (first hand)))
                                               (str (first hand)))]
                                    (recur card
                                           (rest hand)))))
                         wjscore (->> hand
                                    (re-seq (Pattern/compile card))
                                    count)
                         jokers (->> hand
                                     (re-seq (Pattern/compile "J"))
                                     count)
                         score (+ wjscore jokers)
                         score (if (> score 1) (* score score) 0)
                         score (if (and (= 1 wjscore) (= 1 jokers) (> (count (str/replace hand (Pattern/compile "J") "")) 1)) 0 score)]
                     (recur (if (and (= 2 wjscore) (= 1 jokers))
                              (-> hand
                                  (str/replace (Pattern/compile card) "")
                                  (str/replace (Pattern/compile "J") ""))
                              (str/replace hand (Pattern/compile card) ""))
                            (conj game score)))))
               (sort >)))
        total-score (if (> (first games) 15)
                      (first games)
                      (if (and ((set games) 9) ((set games) 4))
                        13
                        (if ((set games) 9)
                          9
                          (if (> (apply + games) 7)
                            8
                            (if (> (apply + games) 3)
                              4
                              0)))))]
    total-score))

(defn card-value [card]
  (case card
    "A" 14
    "K" 13
    "Q" 12
    "J" 1
    "T" 10
    (Integer/parseInt card)))

(defn resolve-tie [hand1 hand2]
  (let [original-hand1 hand1
        original-hand2 hand2]
    (loop
      [hand1 hand1
       hand2 hand2
       winner nil]
      (if (some? winner)
        winner
        (let [card1 (card-value (str (first hand1)))
              card2 (card-value (str (first hand2)))]
          (recur (rest hand1)
                 (rest hand2)
                 (if (not (= card1 card2))
                   (if (< card1 card2) original-hand2 original-hand1))))))))

(defn winner [hand1 hand2]
  (if (zero? (- (game hand1) (game hand2)))
    (resolve-tie hand1 hand2)
    (if (neg? (- (game hand1) (game hand2)))
      hand2
      hand1)))

(defn sort-by-winner [hand1 hand2]
  (if (= (winner (first hand1) (first hand2)) (first hand1)) 1 -1))

;; part 1 253933213
;; part 2 253473930
(defn part12 []
  (let [input (-> (slurp (io/resource "day7"))
                  (str/split #"\r\n"))
        input (mapv #(-> %
                         (str/split #" ")) input)
        sorted (sort sort-by-winner input)
        _ (clojure.pprint/pprint sorted)]
    (->> (for [i (range (count sorted))]
           (* (inc i) (Integer/parseInt (second (nth sorted i)))))
         (apply +))))
