(ns jelly-puzzle.state)

(defrecord Game [puzzle key-pressed grid-size])
(defrecord Puzzle [width height stage-txt grids blocks cursor])
(defrecord Grid [type block-id])
(defrecord Coord [x y])
(defrecord Cursor [coord focusing])
