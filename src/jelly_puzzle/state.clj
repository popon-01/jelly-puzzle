(ns jelly-puzzle.state)

;; A coordinate in a grid of a puzzle grid.
;; x : x-coordinate (number)
;; y : y-coordinate (number)
(defrecord Coord [x y])

;; A puzzle cursor.
;; coord        : a coordinate of cursor (Coord)
;; target-block : a current selected block id (keyword),
;;                or nil if no block is selected.
(defrecord Cursor [coord target-block])

;; A single grid of puzzle.
;; type     : a type of grid (keyword)
;; block-id : a block id includes this grid (keyword)
(defrecord Grid [type block-id])

;; A puzzle board.
;; width      : width of a puzzle grid (number)
;; height     : height of a puzzle grid (number)
;; stage-file : a path of puzzle data file (string)
;; grids      : a mapping of coordinates (Coord) into grids (Grid)
;; blocks     : a mapping of block ids (keyword) into sets of coorinates(Coord)
;; cursor     : a puzzle cursor (Cursor)
;; history    : a list of previous puzzle board data (saves grids, blocks, cursor)
(defrecord Puzzle [width height stage-file grids blocks cursor histrory])

;; A stage select scene
;; stage : a current selected index of a stage list (number)
(defrecord StageSelect [stage])

;; Data of a whole game
;; scene       : current game scene (StageSelect or Puzzle)
;; key-pressed : a set of last pressed keys (keyword)
;; grid-size   : a size of single grid (number)
(defrecord Game [scene key-pressed grid-size])
