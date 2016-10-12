(ns hearts-lecture.core
  (:require [hearts-lecture.utils :refer [spy]]))

(comment

  "DOCS:
  Entities:
  - deck
  - cards
  - players
    - scores
    - hands (collection of cards)
    - cards taken
  - trick
  - turn (datatype?)
  "

  (def state
    {:players [{:name "string"
                :hand []
                :taken []
                :score 0}]
     :turn 0
     :trick []
     }))

;; ==== BASIC CARD ==== ;;
(def suits #{"H" "D" "S" "C"})
(def ranks (concat (map str (range 2 10)) ["T" "J" "Q" "K" "A"]))

(def deck (for [suit suits
                rank ranks]
            (str rank suit)))

(defn card->suit [[rank suit]]
  suit)

(defn card->rank [[rank suit]]
  rank)

(defn deal-cards [n deck]
  (let [hand-size (quot (count deck) n)]
   (partition hand-size deck)))

(defn draw-card [n deck]
  (split-at n deck))

(def card-scores
  (let [hearts (for [rank ranks]
                 (str rank "H"))]
    (-> hearts
      (zipmap (repeat 1))
      (assoc "QS" 13))))

;; ==== BASIC PLAYER ==== ;;

(defn init-player [-name]
  {:name -name
   :hand []
   :taken []
   :score 0})

(defn make-players [names]
  (map init-player names))

(defn init-hands [players deck]
  (let [n-players (count players)
        hands (deal-cards n-players deck)]
    (mapv #(assoc %1 :hand %2) players hands)))

(defn init-game-state [names]
  {:turn 0
   :players (-> names
              (make-players)
              (init-hands (shuffle deck)))
   :trick []})

;; ===== GAMEPLAY ===== ;;
(defn starting-player
  "input: vector of player maps
  output: index of player with 2C
  plan: convert player maps to hands
  convert hands to presence of 2C
  find index with true"
  [players]
  (map (fn [player index]
         [index (some #{"2C"} (:hand player))])
       players (range)))

(comment
  (def names ["allan" "adi" "jerry" "claire"])
  (def players (:players (init-game-state players)))
  )


