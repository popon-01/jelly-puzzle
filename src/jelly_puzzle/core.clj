(ns jelly-puzzle.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defrecord Game [puzzle key-pressed grid-size])
(defrecord Puzzle [width height cursor])
(defrecord Cursor [x y focusing])


(def grid-size 32)

(defn setup-handler []
  (q/color-mode :hsb 1.0)
  (q/resize-sketch (* 14 grid-size) (* 8 grid-size))
  (let [init-cursor (Cursor. 0 0 false)
        init-puzzle (Puzzle. 14 8 init-cursor)]
    (Game. init-puzzle #{} grid-size)))

(defn key-pressed-handler [state event]
  (let [listen-keys #{:up :down :left :right :z :x}]
    (cond-> state
      (listen-keys (:key event)) (update :key-pressed conj (:key event)))))

(declare update-with-key move-cursor)

(defn update-handler [state]
  (-> state
      update-with-key
      (assoc :key-pressed #{})))

(defn update-with-key [state]
  (let [key-pressed (:key-pressed state)]
    (as-> state st
      (cond-> st
        (key-pressed :z) (assoc-in [:puzzle :cursor :focusing] true)
        (key-pressed :x) (assoc-in [:puzzle :cursor :focusing] false))
      (if-not (get-in st [:puzzle :cursor :focusing])
        (move-cursor st) st))))

(defn move-cursor [state]
  (let [width (get-in state [:puzzle :width])
        height (get-in state [:puzzle :height])
        key-pressed (:key-pressed state)]
    (cond-> state
      (key-pressed :left)  (update-in [:puzzle :cursor :x]
                                      (fn [x] (max 0 (dec x))))
      (key-pressed :right) (update-in [:puzzle :cursor :x]
                                      (fn [x] (min (dec width) (inc x))))
      (key-pressed :up)    (update-in [:puzzle :cursor :y]
                                      (fn [y] (max 0 (dec y))))
      (key-pressed :down)  (update-in [:puzzle :cursor :y]
                                      (fn [y] (min (dec height) (inc y)))))))
  
(defn draw-handler [state]
  (q/background 255)
  (let [gsize (:grid-size state)
        half-gsize (quot gsize 2)
        cursor (get-in state [:puzzle :cursor])
        left (* (:x cursor) gsize)
        top (* (:y cursor) gsize)]
    (q/stroke 0.32 1.0 1.0)
    (q/no-fill)
    (q/rect left top 32 32)
    (when (:focusing cursor)
      (q/fill 0.32 1.0 1.0)
      (q/ellipse (+ left half-gsize) (+ top half-gsize)
                 half-gsize half-gsize))))

(q/defsketch jelly-puzzle
  :title "sandbox"
  :size [500 500]
  :setup setup-handler
  :update update-handler
  :draw draw-handler
  :key-pressed key-pressed-handler
  :features [:keep-on-top :no-bind-output]
  :middleware [m/fun-mode m/pause-on-error])
