(ns jelly-puzzle.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [jelly_puzzle.state]
            [jelly-puzzle.puzzle :as puzzle])
  (:import [jelly_puzzle.state Game]))


(defn init-stage [state stage-id]
  (let [puzzle (puzzle/load-puzzle "stage/stage1.txt")]
    (q/resize-sketch (* (:width puzzle) (:grid-size state))
                     (* (:height puzzle) (:grid-size state)))
    (doseq [[k v] (:blocks puzzle)]
      (println k v))
    (assoc state :puzzle puzzle)))

(defn setup-handler []
  (q/color-mode :hsb 1.0)
  (-> (Game. nil #{} 32) (init-stage 1)))

(defn key-pressed-handler [state event]
  (let [listen-keys #{:up :down :left :right :z :x}]
    (cond-> state
      (listen-keys (:key event)) (update :key-pressed conj (:key event)))))

(defn update-handler [state]
  (as-> state st
    (update st :puzzle puzzle/update-with-key (:key-pressed st))
    (assoc st :key-pressed #{})))

(defn draw-handler [state]
  (puzzle/draw-puzzle (:puzzle state) (:grid-size state)))

(q/defsketch jelly-puzzle
  :title "sandbox"
  :size [500 500]
  :setup setup-handler
  :update update-handler
  :draw draw-handler
  :key-pressed key-pressed-handler
  :features [:keep-on-top :no-bind-output]
  :middleware [m/fun-mode m/pause-on-error])
