(ns jelly-puzzle.puzzle
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]
            [quil.core :as q]
            [jelly_puzzle.state])
  (:import [jelly_puzzle.state Puzzle Grid Coord Cursor]))

(defn update-with-col [val col f]
  (reduce f val col))

(defn move-block [puzzle block-id direction]
  "Move the block with specified id.
  Specify a direction with :left, :right or :down
  Returns map :
    `:puzzle` updated puzzle state
    `:moved?` whether this block is moved or not."
  (let [[dx dy] (case direction :left [-1 0] :right [1 0] :down [0 1])]
    (loop [puzzle' puzzle
           new-block-grid {}
           target (get-in puzzle [:blocks block-id])]
      (if (nil? target)
        (let [old-block (get-in puzzle' [:blocks block-id])
              new-block (-> new-block-grid keys set)
              new-grids (as-> (:grids puzzle') grids
                          (apply (partial dissoc grids) (vec old-block))
                          (merge grids new-block-grid))]
          {:puzzle (-> puzzle'
                       (assoc-in [:blocks block-id] new-block)
                       (assoc :grids new-grids))
           :moved? true})
        (let [coord (first target)
              ncoord (-> coord (update :x + dx) (update :y + dy))]
          (cond
            (or (nil? (get-in puzzle' [:grids ncoord]))
                (= (get-in puzzle' [:grids ncoord :block-id]) block-id))
            (recur puzzle'
                   (assoc new-block-grid ncoord
                          (get-in puzzle' [:grids coord]))
                   (next target))

            (not (= (get-in puzzle' [:grids ncoord :type]) :floor))
            (let [adj-block-id (get-in puzzle' [:grids ncoord :block-id])
                  try (move-block puzzle' adj-block-id direction)]
              (if-not (:moved? try)
                {:puzzle puzzle :moved? false}
                (recur (:puzzle try)
                       (assoc new-block-grid ncoord
                              (get-in (:puzzle try) [:grids coord]))
                       (next target))))

            :else
            {:puzzle puzzle :moved? false}))))))

(defn move-cursor [puzzle direction]
  "Move the cursor to the direction specified with :left, :right, :up, or :down."
  (let [width (:width puzzle)
        height (:height puzzle)]
    (update-in puzzle [:cursor :coord]
               (fn [c]
                 (case direction
                   :left  (update c :x #(max 0 (dec %)))
                   :right (update c :x #(min (dec width) (inc %)))
                   :up    (update c :y #(max 0 (dec %)))
                   :down  (update c :y #(min (dec height) (inc %))))))))

(defn move-block-and-cursor [puzzle block-id direction]
  "Move the block with specified id and adjust cursor position.
  Specify a direction with :left or :right.
  Returns map :
    `:puzzle` updated puzzle state
    `:moved?` whether this block is moved or not."
  (let [result (move-block puzzle block-id direction)]
    (if-not (:moved? result)
      result (update result :puzzle move-cursor direction))))

(defn drop-all-block [puzzle]
  "Drop all blocks by gravity"
  (letfn [;; update loop
          (drop-1 [puzzle']
            (update-with-col
             {:puzzle puzzle' :updated? false}
             (-> puzzle' :blocks keys)
             (fn [now block-id]
               (let [now-puzzle (:puzzle now)
                     try (if (= block-id
                                (get-in now-puzzle [:cursor :target-block]))
                           (move-block-and-cursor now-puzzle block-id :down)
                           (move-block now-puzzle block-id :down))]
                 (if (:moved? try)
                   (assoc now
                          :puzzle (:puzzle try)
                          :updated? true)
                   now)))))]
    (loop [puzzle' puzzle]
      (let [try (drop-1 puzzle')]
        (if-not (:updated? try)
          puzzle' (recur (:puzzle try)))))))

(defn merge-block [puzzle]
  "Merge the grids into block."
  (letfn [;; Traverse single block.
          ;; dfs-state :
          ;; `:puzzle` current puzzle state
          ;; `:visit` a set which contains coordinates of already visited grids
          ;; `:grids` a set which includes grids contained in a block
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
            (as-> puzzle puzzle'
              (assoc puzzle' :blocks {})
              (-> (update-with-col
                   {:puzzle puzzle' :visit #{} :next-id 0} (:grids puzzle)
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
                  (get :puzzle))))
          ;; Update block-id of each grid with current blocks.
          (update-grid-id [puzzle]
            (update-with-col
             puzzle (:blocks puzzle)
             (fn [puzzle' [id grids]]
               (update-with-col
                puzzle' grids
                (fn [puzzle'' coord]
                  (assoc-in puzzle'' [:grids coord :block-id] id))))))
          ;; Update the cursor's target block id with new block id.
          (update-cursor-target-block [puzzle]
            (if (nil? (get-in puzzle [:cursor :target-block]))
              puzzle
              (let [coord (get-in puzzle [:cursor :coord])
                    new-target-block (get-in puzzle [:grids coord :block-id])]
                (assoc-in puzzle [:cursor :target-block]
                          new-target-block))))]
    (-> puzzle
        grids-to-block
        update-grid-id
        update-cursor-target-block)))

(defn load-puzzle [stage-file]
  (let [fseq (-> (io/resource stage-file)
                 slurp
                 string/split-lines)
        [width height] (->> (string/split (first fseq) #"\s")
                            (map #(Integer/parseInt %)))
        stage-body (rest fseq)]
    (-> (Puzzle. width height stage-file {} {}
                 (Cursor. (Coord. 0 0) nil) nil)
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
        merge-block)))

(defn save-history [puzzle old-puzzle]
  "Saves the old state to the histroy"
  (let [limit 10]
    (assoc puzzle :history
           (as-> (:history puzzle) new-histroy
             (conj new-histroy
                   {:grids  (:grids old-puzzle)
                    :blocks (:blocks old-puzzle)
                    :cursor (:cursor old-puzzle)})
             (take limit new-histroy)))))

(defn undo-puzzle [puzzle]
  "Undoes the puzzle"
  (if (empty? (:history puzzle))
    puzzle
    (let [{:keys [grids blocks cursor]} (first (:history puzzle))]
      (-> puzzle
          (assoc :grids grids :blocks blocks :cursor cursor)
          (update :history rest)))))

(defn cleared? [puzzle]
  "Returns cleared or not"
  (loop [grids (-> puzzle :grids vals)
         color->blocks {}]
    (if (nil? grids)
      (reduce (fn [res blocks] (and res (<= (-> blocks distinct count) 1)))
              true (vals color->blocks))
      (let [grid (first grids)]
        (recur (next grids)
               (cond-> color->blocks
                 (not (= (:type grid) :floor))
                 (update (:type grid) conj (:block-id grid))))))))

;;;;;;;;;; controller ;;;;;;;;;;
(defn handle-block-select-key [puzzle]
  "Handler for the block-select key input."
  (let [cursor (:cursor puzzle)]
    (cond
      (some? (:target-block cursor))
      (assoc-in puzzle [:cursor :target-block] nil)

      (and (some? (get-in puzzle [:grids (:coord cursor)]))
           (not (= (get-in puzzle [:grids (:coord cursor) :type]) :floor)))
      (assoc-in puzzle [:cursor :target-block]
                (get-in puzzle [:grids (:coord cursor) :block-id]))

      :else puzzle)))

(defn move-cursor-with-key [puzzle key-pressed]
  "Move the cursor according to the key input."
  (cond-> puzzle
    (key-pressed :left)  (move-cursor :left)
    (key-pressed :right) (move-cursor :right)
    (key-pressed :up)    (move-cursor :up)
    (key-pressed :down)  (move-cursor :down)))

(defn proceed-puzzle [puzzle key-pressed]
  "Move the block and modify puzzle state according to the key input."
  (if-not (or (key-pressed :left) (key-pressed :right))
    puzzle
    (let [block-id (get-in puzzle [:cursor :target-block])
          try (cond (key-pressed :left)
                    (move-block-and-cursor puzzle block-id :left)
                    (key-pressed :right)
                    (move-block-and-cursor puzzle block-id :right))]
      (cond-> (:puzzle try)
        (:moved? try) (save-history puzzle)))))

(defn handle-key [puzzle key-pressed]
  "Update state with keyboad input."
  (-> puzzle
      (cond-> (key-pressed :z) handle-block-select-key)
      (as-> puzzle'
          (if (some? (get-in puzzle' [:cursor :target-block]))
            (proceed-puzzle puzzle' key-pressed)
            (move-cursor-with-key puzzle' key-pressed)))))

(defn update-puzzle [puzzle key-pressed]
  (cond
    (cleared? puzzle) puzzle
    (key-pressed :r) (load-puzzle (:stage-file puzzle))
    (key-pressed :u) (undo-puzzle puzzle)
    :else
    (-> puzzle
        (handle-key key-pressed)
        drop-all-block
        merge-block)))

;;;;;;;;;; draw function ;;;;;;;;;;
(declare draw-grids draw-cursor draw-clear)
(defn draw-puzzle [puzzle grid-size]
  (q/background 0.0 0.0 1.0)
  (draw-grids puzzle grid-size)
  (draw-cursor (:cursor puzzle) grid-size)
  (when (cleared? puzzle)
    (draw-clear (:width puzzle) (:height puzzle) grid-size)))

(defn draw-grids [puzzle grid-size]
  (doseq [[coord grid] (:grids puzzle)]
    (let [left   (* (:x coord) grid-size)
          right  (+ left grid-size)
          top    (* (:y coord) grid-size)
          bottom (+ top grid-size)
          color  (case (:type grid)
                   :floor [0.0 0.0 0.5]
                   :red   [0.0 0.2 1.0]
                   :green [0.33 0.2 1.0]
                   :blue  [0.66 0.2 1.0])]
      (apply q/fill color)
      (q/no-stroke)
      (q/rect left top grid-size grid-size)
      (q/stroke 0.0 0.0 0.0)
      (doseq [[dx dy begin end] [[1  0 [right top]    [right bottom]]
                                 [-1 0 [left  top]    [left  bottom]]
                                 [0  1 [left  bottom] [right bottom]]
                                 [0 -1 [left  top]    [right top]   ]]]
        (let [adj-coord (-> coord (update :x + dx) (update :y + dy))]
          (when-not (and (some? (get-in puzzle [:grids adj-coord :block-id]))
                         (= (get-in puzzle [:grids adj-coord :block-id])
                            (:block-id grid)))
            (q/line begin end)))))))

(defn draw-cursor [cursor grid-size]
  (let [half-gsize (quot grid-size 2)
        left (* (get-in cursor [:coord :x]) grid-size)
        top (* (get-in cursor [:coord :y]) grid-size)]
    (q/no-stroke)
    (if (some? (:target-block cursor))
      (q/fill 0.0 1.0 1.0) (q/fill 0.33 1.0 1.0))
    (q/ellipse (+ left half-gsize) (+ top half-gsize)
               half-gsize half-gsize)))

(defn draw-clear [width height grid-size]
  (let [sketch-width (* width grid-size)
        sketch-height (* height grid-size)]
    (q/fill 0.0 1.0 1.0)
    (q/text-align :center :center)
    (q/text-size 32)
    (q/text "CLEAR" (quot sketch-width 2) (quot sketch-height 2))))
