(ns jelly-puzzle.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [jelly-puzzle.stage-select :as stage-select]
            [jelly-puzzle.puzzle :as puzzle])
  (:import [jelly_puzzle.puzzle Puzzle]
           [jelly_puzzle.stage_select StageSelect]))

;; Data of a whole game
;; scene       : current game scene (StageSelect or Puzzle)
;; key-pressed : a set of last pressed keys (keyword)
;; grid-size   : a size of single grid (number)
(defrecord Game [scene key-pressed grid-size])

(defprotocol Scene
  "A protocol for game scenes."
  (update-scene [this key-pressed]
    "Returns the updated scene.")
  (switch-scene [this key-pressed]
    "Returns the next game scene if the scene is switched, otherwise nil.")
  (adjust-sketch [this grid-size]
    "Resizes the sketch with this game scene")
  (draw-scene [this grid-size]
    "Draws this scene."))

(defn switch-from-puzzle [puzzle key-pressed]
  "Switches from the puzzle game scene."
  (when (and (puzzle/cleared? puzzle) (key-pressed :z))
    (stage-select/make-stage-select)))

(defn switch-from-stage-select [stage-select key-pressed]
  "Switches from the stage select scene."
  (when (key-pressed :z)
    (let [file-name (get-in (:stage-list stage-select)
                            [(:index stage-select) :file])
          stage-file (str "stage/" file-name)]
      (puzzle/load-puzzle stage-file))))

(extend-protocol Scene
  Puzzle
  (update-scene [puzzle key-pressed]
    (puzzle/update-puzzle puzzle key-pressed))
  (switch-scene [puzzle key-pressed]
    (switch-from-puzzle puzzle key-pressed))
  (adjust-sketch [puzzle grid-size]
    (puzzle/adjust-puzzle-sketch puzzle grid-size))
  (draw-scene [puzzle grid-size]
    (puzzle/draw-puzzle puzzle grid-size))

  StageSelect
  (update-scene [stage-select key-pressed]
    (stage-select/update-stage-select stage-select key-pressed))
  (switch-scene [stage-select key-pressed]
    (switch-from-stage-select stage-select key-pressed))
  (adjust-sketch [stage-select grid-size]
    (stage-select/adjust-stage-select-sketch grid-size))
  (draw-scene [stage-select grid-size]
    (stage-select/draw-stage-select stage-select grid-size)))

(defn setup-handler []
  "Sets up the sketch."
  (q/color-mode :hsb 1.0)
  (let [grid-size 32
        init-scene (stage-select/make-stage-select)]
    (adjust-sketch init-scene grid-size)
    (Game. init-scene #{} grid-size)))

(defn key-pressed-handler [game event]
  "Listens key inputs."
  (let [listen-keys #{:up :down :left :right :z :r :u}]
    (cond-> game
      (listen-keys (:key event)) (update :key-pressed conj (:key event)))))

(defn update-handler [game]
  "Updates the sketch."
  (let [key-pressed (:key-pressed game)]
    (as-> game game'
      (update game' :scene update-scene key-pressed)
      (if-let [next-scene (switch-scene (:scene game') key-pressed)]
        (do (adjust-sketch next-scene (:grid-size game'))
            (assoc game' :scene next-scene))
        game')
      (assoc game' :key-pressed #{}))))

(defn draw-handler [game]
  "Draws the sketch."
  (draw-scene (:scene game) (:grid-size game)))

(q/defsketch jelly-puzzle
  :title "Clojellyのパズル"
  :setup setup-handler
  :update update-handler
  :draw draw-handler
  :key-pressed key-pressed-handler
  :features [:keep-on-top :no-bind-output]
  :middleware [m/fun-mode m/pause-on-error])
