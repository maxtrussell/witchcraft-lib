(ns net.maxtrussell.witchcraft
  (:require [lambdaisland.witchcraft :as wc]))

(defn get-inv-contents [player]
  (def contents (hash-map))
  (let [inv (remove nil? (get (wc/inventory player) :contents))]
    (doseq [item inv]
      (let [material (get item :material)]
            (def contents (assoc contents material (+ (get contents material 0) (get item :amount))))))
    contents))

(defn player-has-materials [player materials]
  (def remaining (get-inv-contents player))
  (doseq [kv materials]
    (let [material (first kv) qty (second kv)]
      (when (not (nil? material))
        (def remaining (assoc remaining material (- (get remaining material 0) qty))))))
  (reduce (fn [acc kv] (and acc (>= (second kv) 0))) true remaining))

(defn get-materials [materials]
  (def total (hash-map))
  (doseq [item materials]
    (def total (assoc total (nth item 3) (+ (get total (nth item 3) 0) 1))))
  total)

(defn remove-materials [player materials]
  (doseq [kv materials]
    (wc/remove-inventory player (first kv) (second kv))))

(defn build [player pos blocks]
  "Build a structure for a player, using blocks from their inventory."
  (let [materials (get-materials blocks)
        offset-blocks (map #(wc/add % pos) blocks)]
    (if (player-has-materials player materials)
      (do 
        (remove-materials player materials)
        (wc/set-blocks offset-blocks))
      "Insufficient materials")))

(defn deconstruct [player blocks]
  "Deconstruct the last structure and refund blocks to player."
  (let [materials (get-materials blocks)]
    ;; TODO: When coy has a cut function use this instead of undo
    (wc/undo!)
    (remove-materials player
                      (map (fn [[k v]] [k (* -1 v)]) materials))))

;; Building schematic
(defn house [dx dy dz material]
  (letfn [(is-wall [x dx] (or (= x 0) (= x (dec dx))))
          (is-roof [y dy] (= y (dec dy)))]
    (for [x (range dx)
          y (range dy)
          z (range dz)
          :when (or (is-wall x dx) (is-wall z dz) (is-roof y dy))]
      [x y z material])))

(def me (wc/player "maximus1233"))
(def pos [-10 72.0 -67])
(def my-house (house 7 4 7 :dark-oak-planks))
(get-materials my-house)
(build me pos my-house)
(deconstruct me my-house)
