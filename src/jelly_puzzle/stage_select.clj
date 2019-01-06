(ns jelly-puzzle.stage-select
  (:require [quil.core :as q]))

;; A stage of puzzle
;; name : a name of this stage (string)
;; file : a name of puzzle data file (string, not includes path)
(defrecord Stage [name file])

;; A stage select scene
;; index      : a current selected index of a stage list (number)
;; stage-list : a list of stage data (StageData)
(defrecord StageSelect [index stage-list])

(def stage-select-grid
  "Width and height of stage select scene"
  [10 10])

(defn make-stage-list [& name-and-files]
  "Makes a stage data list."
  (assert (even? (count name-and-files)))
  (reduce (fn [v [name file]] (conj v (Stage. name file)))
          [] (partition 2 name-and-files)))

(def stage-list (make-stage-list "sandbox" "sandbox.txt"
                                 "stage1" "stage1.txt"))

(defn make-stage-select []
  "Makes a stage select scene"
  (StageSelect. 0 stage-list))

(defn adjust-stage-select-sketch [grid-size]
  "Adjusts sketch size to the stage select window size."
  (let [[width height] stage-select-grid]
    (q/resize-sketch (* width grid-size) (* height grid-size))))

(defn update-stage-select [stage-select key-pressed]
  "Updates the stage select scene."
  (let [num-of-stage (count stage-list)]
    (cond-> stage-select
      (key-pressed :left)  (update :index #(mod (+ (dec %) num-of-stage)
                                                num-of-stage))
      (key-pressed :right) (update :index #(mod (inc %) num-of-stage)))))

(defn draw-stage-select [stage-select grid-size]
  "Draws the stage select scene."
  (q/background 0.0 0.0 1.0)
  (let [[width height] stage-select-grid
        sketch-width (* width grid-size)
        sketch-height (* height grid-size)
        stage-name (get-in (:stage-list stage-select)
                           [(:index stage-select) :name])]
    (q/fill 0.0 0.0 0.0)
    (q/text-align :center :center)
    (q/text-size 32)
    (q/text stage-name (quot sketch-width 2) (quot sketch-height 2))))
