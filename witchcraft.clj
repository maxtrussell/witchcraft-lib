(ns net.maxtrussell.witchcraft
  (:require [lambdaisland.witchcraft :as wc]))

(use 'clojure.string)

(defn get-inv-contents [player]
  (def contents (hash-map))
  (let [inv (remove nil? (get (wc/inventory player) :contents))]
    (doseq [item inv]
      (let [material (get item :material)]
            (def contents (assoc contents material (+ (get contents material 0) (get item :amount))))))
    contents))

(defn player-has-materials? [player materials]
  (def remaining (get-inv-contents player))
  (doseq [[material qty] materials]
    (when (not (nil? material))
      (def remaining (assoc remaining material (- (get remaining material 0) qty)))))
  (reduce (fn [acc [material qty]] (and acc (>= qty 0))) true remaining))

(defn get-materials [materials]
  (def total (hash-map))
  (doseq [item materials]
    (def total (assoc total (nth item 3) (+ (get total (nth item 3) 0) 1))))
  total)

(defn build [player pos blocks]
  "Build a structure for a player, using blocks from their inventory."
  (let [materials (get-materials blocks)
        offset-blocks (map #(wc/add % pos) blocks)]
    (if (player-has-materials? player materials)
      (do 
        (doseq [[material qty] materials]
          (wc/remove-inventory player material qty))
        (wc/set-blocks offset-blocks))
      "Insufficient materials")))

(defn deconstruct [player blocks]
  "Deconstruct the last structure and refund blocks to player."
  (let [materials (get-materials blocks)]
    ;; TODO: When coy has a cut function use this instead of undo
    (wc/undo!)
    (doseq [[material amount] materials]
      (wc/add-inventory me material amount))))

(defn add-maps [maps]
  (def sum-map (hash-map))
  (doseq [m maps]
    (doseq [[k v] m]
      (def sum-map (assoc sum-map k (+ (get sum-map k 0) v)))))
  sum-map)

(defn dfs [loc pred? func!]
  "Performs a depth first search of all blocks matching pred? and applies func. Returns map of found blocks."
  (def found (hash-map))
  (def seen (set '()))
  (let* [block (wc/block loc)
         material (get block :material)]
    (if (pred? block)
      (do (def found (assoc found material (+ (get found material 0) 1)))
          (func! block)
          (doseq [n (get-neighbors loc)]
            (when (nil? (seen n))
              (def found (add-maps [found (dfs n pred? func!)]))
              (def seen (conj seen n)))))))
  found))

(defn get-neighbors [loc]
  "Find all neighbors of a given block"
  (def neighbors [])
  (let [deltas [[1 0 0] [0 1 0] [0 0 1] [-1 0 0] [0 -1 0] [0 0 -1]]]
    (doseq [delta deltas]
      (def neighbors (conj neighbors (into {} (map (fn [d [k v]] {k (+ v d)}) delta loc))))))
  neighbors)

(defn fell-tree [player loc]
  "Use dfs to remove all logs of a tree and give to player."
  (letfn [(is-log? [block] (ends-with? (get block :material) "-log"))
          (remove-block! [block] (wc/set-block block :air))]
    (let [wood (dfs loc is-log? remove-block!)]
      (doseq [[material amount] wood]
        (wc/add-inventory player material amount))
      wood)))

;; Building schematic
(defn house [dx dy dz material]
  (letfn [(is-wall? [x dx] (or (= x 0) (= x (dec dx))))
          (is-roof? [y dy] (= y (dec dy)))]
    (for [x (range dx)
          y (range dy)
          z (range dz)
          :when (or (is-wall? x dx) (is-wall? z dz) (is-roof? y dy))]
      [x y z material])))


(fell-tree me {:x -7 :y 72 :z -54})
(wc/undo!)

(def me (wc/player "maximus1233"))
(def pos {:x -7 :y 72 :z -62})
(def my-house (house 7 4 7 :dark-oak-planks))
(build me pos my-house)
(deconstruct me my-house)
