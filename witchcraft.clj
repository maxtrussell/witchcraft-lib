(ns net.maxtrussell.witchcraft
  (:require [lambdaisland.witchcraft :as wc]
            [clojure.set :as set]
            [clojure.string :as string]))

(defn get-inv-contents [player]
  (let [inv (remove nil? (get (wc/inventory player) :contents))]
    (reduce (fn [acc item]
              (let [material (get item :material)
                    qty (get item :amount)]
                (assoc acc material (+ (get acc material 0) qty))))
            {} inv)))

(defn player-has-materials? [player materials]
  (let* [inv (get-inv-contents player)
         remaining (into {} (map (fn [[material qty]]
                                   {material (- (get inv material 0) qty)}) materials))]
  (reduce (fn [acc [material qty]] (and acc (>= qty 0))) true remaining)))

(defn get-materials [blocks]
  (reduce (fn [acc block]
            (let [material (nth block 3)]
              (assoc acc material (inc (get acc material 0))))) (hash-map) blocks))

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

(defn add-maps [m1 m2]
  (let [keys (set/union (set (keys m1)) (set (keys m2)))]
    (into {} (map (fn [k] {k (+ (get m1 k 0) (get m2 k 0))}) keys))))

(defn dfs [loc pred? func!]
  "Performs a depth first search of all blocks matching pred? and applies func. Returns map of found blocks."
  ;; TODO: return raw list of blocks so coy can use
  (if (pred? (wc/block loc))
    (loop [stack [loc]
           visited []
           found {}]
      (if (empty? stack)
        found
        (let* [loc (peek stack)
               neighbors (filter
                          (fn [n] (and (pred? (wc/block n))
                                       (not (.contains visited n))))
                          (get-neighbors loc))
               new-stack (into (pop stack) neighbors)
               block (wc/block loc)
               material (get block :material)]
          (func! block)
          (recur new-stack
                 (conj visited loc)
                 (assoc found material (inc (get found material 0)))))))
    {}))

(defn get-neighbors [loc]
  "Find all neighbors of a given block"
  (let [deltas [[1 0 0] [0 1 0] [0 0 1] [-1 0 0] [0 -1 0] [0 0 -1]]]
    (map (fn [delta]
           (into {} (map (fn [d [k v]] {k (+ v d)}) delta loc)))
         deltas)))

(defn fell-tree [player loc]
  "Use dfs to remove all logs of a tree and give to player."
  (letfn [(is-log? [block] (string/ends-with? (get block :material) "-log"))
          (remove-block! [block] (wc/set-block (wc/xyz block) :air))]
    (let* [wood (dfs loc is-log? remove-block!)]
      (doseq [[material qty] wood]
        (wc/add-inventory player material qty))
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

(def me (wc/player "maximus1233"))
(def pos {:x -7 :y 72 :z -62})
(def my-house (house 7 4 7 :dark-oak-planks))
(get-materials my-house)
(build me pos my-house)
(deconstruct me my-house)
(fell-tree me {:x 0 :y 74 :z -68})
(wc/undo!)
