(ns vladimir.core
  (:gen-class)
  (:require [vladimir.board :refer [start-fen make-game]]
            [vladimir.move-gen :refer [make-alg-moves]]))

(defn send-id
  "Sends Vladimir's ID information as required by UCI protocol."
  [state]
  (println "id name Vladimir 0.0")
  (println "id author loganmhb")
  state)

(defn set-option
  "No options currently supported."
  [cmd state])

(defn setup-engine
  "No options currently supported."
  [state]
  (println "uciok")
  state)

(defn new-game
  "Currently, nothing needs to be reset for a new game (no saved game state)."
  [state]
  (assoc state :position nil))

(defn set-position
  "Sets position var."
  [cmd state]
  (let [[cmd pos & moves] (clojure.string/split cmd #" ")]
    (assoc state :position
           (make-alg-moves (make-game (if (= pos "startpos") start-fen pos))
                           (rest moves)))))

(defn -main
  "Interface with UCI."
  [& args]
  (loop [input (read-line)
         state {:debug false
                :analyze false
                :position nil
                :options {}}]
    (let [new-state (case (first (clojure.string/split input #" "))
                      "uci" (do (send-id state)
                                (setup-engine state))
                      "isready" (println "readyok")
                      "setoption" (set-option input state)
                      "ucinewgame" (new-game state)
                      "position" (set-position input state)
                      "go" "calculating..."
                      "stop" "Here's a move."
                      "quit" (System/exit 0))]
      (recur (read-line) new-state))))
