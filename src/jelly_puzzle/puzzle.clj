(ns jelly-puzzle.puzzle
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [quil.core :as q]
            [jelly_puzzle.state])
  (:import [jelly_puzzle.state Puzzle Grid Coord Cursor]))

(defn update-with-col [val col f]
  (reduce f val col))

(defn update-block [puzzle]
  "Update blocks with current grids."
  (letfn [;; Traverse single block.
          (dfs [dfs-state coord grid-type]
            (-> dfs-state
                (update :visit conj coord)
                (update :block conj coord)
                (update-with-col
                 [[1 0] [0 1] [-1 0] [0 -1]]
                 (fn [dfs-state' [dx dy]]
                   (let [ncoord (-> coord (update :x + dx) (update :y + dy))]
                     (if (and (some? (get-in dfs-state' [:puzzle :grids ncoord]))
                              (= (get-in dfs-state' [:puzzle :grids ncoord :type])
                                 grid-type)
                              (nil? ((:visit dfs-state') ncoord)))
                       (dfs dfs-state' ncoord grid-type) dfs-state'))))))
          ;; Compose grids into block.
          (grids-to-block [puzzle]
            (-> (update-with-col
                 {:puzzle puzzle :visit #{} :next-id 0} (:grids puzzle)
                 (fn [now [coord {grid-type :type}]]
                   (if-not (and (nil? ((:visit now) coord))
                                (not (= grid-type :floor)))
                     now
                     (let [dfs-state {:puzzle (:puzzle now)
                                      :visit (:visit now)
                                      :block #{}}
                           dfs-result (dfs dfs-state coord grid-type)
                           block-id (-> (:next-id now) str keyword)]
                       (-> now
                           (assoc-in [:puzzle :blocks block-id]
                                     (:block dfs-result))
                           (assoc :visit (:visit dfs-result))
                           (update :next-id inc))))))
                :puzzle))
          ;; Update grid ids with current blocks.
          (update-grid-id [puzzle]
            (update-with-col
             puzzle (:blocks puzzle)
             (fn [puzzle' [id grids]]
               (update-with-col
                puzzle' grids
                (fn [puzzle'' coord]
                  (assoc-in puzzle'' [:grids coord :id] id))))))]
    (-> puzzle grids-to-block update-grid-id)))

(defn load-puzzle [stage-txt]
  (let [fseq (-> (io/resource stage-txt)
                 slurp
                 string/split-lines)
        [width height] (->> (string/split (first fseq) #"\s")
                            (map #(Integer/parseInt %)))
        stage-body (rest fseq)]
    (-> (Puzzle. width height stage-txt {} {} (Cursor. (Coord. 0 0) false))
        ;; register all grid
        (update-with-col
         (map vector (range) stage-body)
         (fn [puzzle' [y row]]
           (update-with-col
            puzzle' (map vector (range) row)
            (fn [puzzle'' [x elem]]
              (case elem
                \# (assoc-in puzzle'' [:grids (Coord. x y)] (Grid. :floor nil))
                \r (assoc-in puzzle'' [:grids (Coord. x y)] (Grid. :red nil))
                \g (assoc-in puzzle'' [:grids (Coord. x y)] (Grid. :green nil))
                \b (assoc-in puzzle'' [:grids (Coord. x y)] (Grid. :blue nil))
                puzzle'')))))
        ;; init block state
        update-block)))

(declare move-cursor)
(defn update-with-key [puzzle key-pressed]
  "Update state with keyboad input."
  (-> puzzle
      (cond-> (key-pressed :z) (assoc-in [:cursor :focusing] true)
              (key-pressed :x) (assoc-in [:cursor :focusing] false))
      (as-> puzzle'
          (if-not (get-in puzzle' [:cursor :focusing])
            (move-cursor puzzle' key-pressed) puzzle'))))

(defn move-cursor [puzzle key-pressed]
  "move the cursor according to the key input."
  (let [width (:width puzzle)
        height (:height puzzle)]
    (update-in puzzle [:cursor :coord]
               (fn [c]
                 (cond-> c
                   (key-pressed :left)  (update :x #(max 0 (dec %)))
                   (key-pressed :right) (update :x #(min (dec width) (inc %)))
                   (key-pressed :up)    (update :y #(max 0 (dec %)))
                   (key-pressed :down)  (update :y #(min (dec height) (inc %))))))))

;;;;;;;;;; draw function ;;;;;;;;;;
(declare draw-grids draw-cursor)
(defn draw-puzzle [puzzle grid-size]
  (q/background 255)
  (draw-grids (:grids puzzle) grid-size)
  (draw-cursor (:cursor puzzle) grid-size))

(defn draw-grid-square [left top size color]
  (let [cfill (q/current-fill)
        cstroke (q/current-stroke)]
    (apply q/fill color)
    (q/stroke 0.0 0.0 0.0) ;; black
    (q/rect left top size size)
    ;; restore fill and stroke
    (q/fill cfill)
    (q/stroke cstroke)))

(defn draw-grids [grids grid-size]
  (doseq [[coord grid] grids]
    (let [left (* (:x coord) grid-size)
          top (* (:y coord) grid-size)]
      (case (:type grid)
        :floor (draw-grid-square left top grid-size [0.0 0.0 0.5])
        :red (draw-grid-square left top grid-size [0.0 0.2 1.0])
        :green (draw-grid-square left top grid-size [0.33 0.2 1.0])
        :blue (draw-grid-square left top grid-size [0.66 0.2 1.0])
        nil))))

(defn draw-cursor [cursor grid-size]
  (let [half-gsize (quot grid-size 2)
        left (* (get-in cursor [:coord :x]) grid-size)
        top (* (get-in cursor [:coord :y]) grid-size)]
    (q/stroke 0.32 1.0 1.0)
    (q/no-fill)
    (q/rect left top grid-size grid-size)
    (when (:focusing cursor)
      (q/fill 0.32 1.0 1.0)
      (q/ellipse (+ left half-gsize) (+ top half-gsize)
                 half-gsize half-gsize))))
