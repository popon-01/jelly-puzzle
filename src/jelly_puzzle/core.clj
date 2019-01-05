(ns jelly-puzzle.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [jelly-puzzle.state]
            [jelly-puzzle.puzzle :as puzzle])
  (:import [jelly_puzzle.state Puzzle StageSelect Game]))


(defrecord StageData [name file])
(defn make-stage-list [& name-and-files]
  "make stage data list."
  (assert (even? (count name-and-files)))
  (reduce (fn [v [name file]] (conj v (StageData. name file)))
          [] (partition 2 name-and-files)))

(def stage-list (make-stage-list "sandbox" "sandbox.txt"
                                 "stage1" "stage1.txt"))

;; Width and height of stage select scene
(def stage-select-grid [10 10])

(defprotocol Scene
  "A protocol for game scenes."
  (update-scene [this key-pressed]
    "Returns updated scene.")
  (switch-scene [this key-pressed]
    "Returns the next game scene, or nil if scene switching do not happened.")
  (adjust-sketch [this grid-size]
    "Resize the sketch with this game scene")
  (draw-scene [this grid-size]
    "Draws Scene."))

(extend-protocol Scene
  Puzzle
  (update-scene [puzzle key-pressed]
    (puzzle/update-puzzle puzzle key-pressed))
  (switch-scene [puzzle key-pressed]
    (when (and (puzzle/cleared? puzzle) (key-pressed :z))
      (StageSelect. 0)))
  (adjust-sketch [puzzle grid-size]
    (q/resize-sketch (* (:width puzzle) grid-size)
                     (* (:height puzzle) grid-size)))
  (draw-scene [puzzle grid-size]
    (puzzle/draw-puzzle puzzle grid-size))

  StageSelect
  (update-scene [stage-select key-pressed]
    (let [num-of-stage (count stage-list)]
      (cond-> stage-select
        (key-pressed :left)  (update :stage #(mod (+ (dec %) num-of-stage)
                                                  num-of-stage))
        (key-pressed :right) (update :stage #(mod (inc %) num-of-stage)))))
  (switch-scene [stage-select key-pressed]
    (when (key-pressed :z)
      (let [file-name (get-in stage-list [(:stage stage-select) :file])
            stage-file (str "stage/" file-name)]
        (puzzle/load-puzzle stage-file))))
  (adjust-sketch [stage-select grid-size]
    (let [[width height] stage-select-grid]
      (q/resize-sketch (* width grid-size) (* height grid-size))))
  (draw-scene [stage-select grid-size]
    (q/background 0.0 0.0 1.0)
    (let [[width height] stage-select-grid
          sketch-width (* width grid-size)
          sketch-height (* height grid-size)
          stage-name (get-in stage-list [(:stage stage-select) :name])]
      (q/fill 0.0 0.0 0.0)
      (q/text-align :center :center)
      (q/text-size 32)
      (q/text stage-name (quot sketch-width 2) (quot sketch-height 2)))))

(defn setup-handler []
  (q/color-mode :hsb 1.0)
  (let [grid-size 32
        init-scene (StageSelect. 0)]
    (adjust-sketch init-scene grid-size)
    (Game. init-scene #{} grid-size)))

(defn key-pressed-handler [game event]
  (let [listen-keys #{:up :down :left :right :z :r :u}]
    (cond-> game
      (listen-keys (:key event)) (update :key-pressed conj (:key event)))))

(defn update-handler [game]
  (let [key-pressed (:key-pressed game)]
    (as-> game game'
      (update game' :scene update-scene key-pressed)
      (if-let [next-scene (switch-scene (:scene game') key-pressed)]
        (do (adjust-sketch next-scene (:grid-size game'))
            (assoc game' :scene next-scene))
        game')
      (assoc game' :key-pressed #{}))))

(defn draw-handler [game]
  (draw-scene (:scene game) (:grid-size game)))

(q/defsketch jelly-puzzle
  :title "Clojellyのパズル"
  :setup setup-handler
  :update update-handler
  :draw draw-handler
  :key-pressed key-pressed-handler
  :features [:keep-on-top :no-bind-output]
  :middleware [m/fun-mode m/pause-on-error])
