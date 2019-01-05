(ns jelly-puzzle.state)

(defrecord Coord [x y])
(defrecord Cursor [coord target-block])
(defrecord Grid [type block-id])
(defrecord Puzzle [width height stage-file grids blocks cursor histrory])
(defrecord StageSelect [stage])
(defrecord Game [scene key-pressed grid-size])
