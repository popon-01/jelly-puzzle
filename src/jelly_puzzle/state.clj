(ns jelly-puzzle.state)

(defrecord Coord [x y])
(defrecord Cursor [coord target-block])
(defrecord Grid [type block-id])
(defrecord Puzzle [width height stage-txt grids blocks cursor])
(defrecord Game [puzzle key-pressed grid-size])
