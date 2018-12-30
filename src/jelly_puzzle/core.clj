(ns jelly-puzzle.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defrecord Game [puzzle key-pressed grid-size])
(defrecord Puzzle [width height stage now cursor])
(defrecord Cursor [x y focusing])


(def grid-size 32)

(defn init-stage [state stage-id]
  (let [fseq (-> (io/resource "stage/stage1.txt")
                 slurp
                 string/split-lines)
        [width height] (->> (string/split (first fseq) #"\s")
                            (map #(Integer/parseInt %)))]
    (q/resize-sketch (* width grid-size) (* height grid-size))
    (assoc state :puzzle
           (Puzzle. width height (rest fseq) (rest fseq)
                    (Cursor. 0 0 false)))))

(defn setup-handler []
  (q/color-mode :hsb 1.0)
  (-> (Game. nil #{} grid-size)
      (init-stage 1)))

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
  "Update state with keyboad input."
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
  
(declare draw-cursor draw-puzzle)

(defn draw-handler [state]
  (q/background 255)
  (draw-puzzle state)
  (draw-cursor state))


(defn draw-block [left top size color]
  (let [cfill (q/current-fill)
        cstroke (q/current-stroke)]
    (apply q/fill color)
    (q/stroke 0.0 0.0 0.0) ;; black
    (q/rect left top size size)
    ;; restore fill and stroke
    (q/fill cfill)
    (q/stroke cstroke)))

(defn draw-puzzle [state]
  (doseq [[y row] (map vector (range) (get-in state [:puzzle :now]))]
    (doseq [[x elem] (map vector (range) row)]
      (let [gsize (:grid-size state)
            left (* x gsize)
            top (* y gsize)]
        (case elem
          \# (draw-block left top gsize [0.0 0.0 0.5])
          \r (draw-block left top gsize [0.0 0.2 1.0])
          \g (draw-block left top gsize [0.33 0.2 1.0])
          \b (draw-block left top gsize [0.66 0.2 1.0])
          nil)))
    (println)))

(defn draw-cursor [state]
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
