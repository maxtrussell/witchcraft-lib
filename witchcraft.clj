(ns net.maxtrussell.witchcraft
  (:require [lambdaisland.witchcraft :as wc]
            [lambdaisland.witchcraft.events :as e]
            [clojure.set :as set]
            [clojure.string :as string]))

(def tool-durabilities {:gold 32 :wood 59 :stone 131 :iron 250 :diamond 1561 :netherite 2031})
(def seeds {:wheat-seeds :wheat, :carrot :carrot, :potato :potato})
(def crops #{:wheat :carrots :potatoes :beetroots})
(def crops-mature {:wheat 7 :carrots 7 :potatoes 7 :beetroots 3})

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

(defn dfs [loc pred? func! & {:keys [limit] :or {limit 100}}]
  "Performs a depth first search of all blocks matching pred? and applies func. Returns map of found blocks."
  ;; TODO: return raw list of blocks so coy can use
  (when (> limit 500)
    (throw (Exception. "Limit cannot exceed 500.")))

  (if (pred? (wc/block loc))
    (loop [stack [loc]
           visited []
           found {}]
      (if (or (empty? stack) (>= (count visited) limit))
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
  "Harvest entire tree or giant mushroom"
  (harvest player loc (fn [block]
                        (or (string/ends-with? (get block :material) "-log")
                            (= (get block :material) :mushroom-stem)
                            (string/ends-with? (get block :material) "mushroom-block")))))

(defn mine-vein [player loc]
  "Harvest entire ore vein"
  (harvest player loc (fn [block] (string/ends-with? (get block :material) "-ore"))))

(defn harvest-crops [player loc]
  "Harvest mature crops from farm"
  (letfn [(crop-age [block] (get (get block :block-data) :age))]
    (let [crops #{:wheat :carrots :potatoes :beetroots :melon :pumpkin}]
      (harvest player loc (fn [block] (and
                                       (crops (get block :material))
                                       (>=
                                        (crop-age block)
                                        (get crops-mature (get block :material)))))))))

;; TODO: Make harvest accept a list of blocks instead of calling DFS
;;       so it can be used by other funcs such as quarry.
(defn harvest [player loc pred?]
  "Use dfs to remove all blocks and give to player, damaging the equipped tool"
  ;; TODO: Set limit as max(tool-capacity 200)
  (let* [harvested (dfs loc pred? wc/break-naturally :limit 200)
         tool (wc/item-in-hand player)
         tool-use (reduce (fn [acc [_ qty]] (+ acc qty)) 0 harvested)]
    (.setDurability tool (+ (.getDurability tool) (* 1 tool-use)))
    harvested))

(defn get-drop [item]
  "Rolls for a drop for given item on the drop table. Defaults to 1"
  (into {}
        (map (fn [[material func]] {material (func)}) (get drop-table item {item (fn [] 1)}))))

(defn plant-crops [player loc]
  "Use dfs to plant seeds on all farmland"
  (letfn [(is-farm? [block] (= :farmland (get block :material)))
          (plant-seed! [block] (wc/set-block (wc/xyz (assoc block :y (inc (get block :y)))) :wheat))]
    (let [seed-type (wc/item-in-hand-type me)
          seed-qty (wc/item-in-hand-count me)]
      (when (nil? (get seeds seed-type))
        (throw (Exception. "Invalid seed type")))
      (let [used (dfs loc is-farm? plant-seed! :limit (min 100 seed-qty))]
        (doseq [[_ qty] used]
          (wc/remove-inventory me seed-type qty))
        used))))

(defn cube-walk [loc dx dy dz pred? func!]
  (let [blocks (for [x (range dx)
                     y (range dy)
                     z (range dz)
                     :when (pred? (wc/block (into [] (map + [x y z] (wc/xyz loc)))))]
                 [(wc/block (into [] (map + [x y z] (wc/xyz loc))))])]
    (doseq [block blocks] (func! block))
    blocks))

(defn scan [player loc dx dy dz]
  "Returns a vector of blocks in the scanned cube"
  (cube-walk player loc dx dy dz (fn [_] true) (fn [_] true)))

(defn quarry [player loc dx dy dz]
  "Harvest all blocks in a dx*dy*dz cube"
  (let [blocks (cube-walk loc dx dy dz (fn [_] true) (fn [_] true))]
    (harvest player blocks)))

;; Building schematic
(defn house [dx dy dz material]
  (letfn [(is-wall? [x dx] (or (= x 0) (= x (dec dx))))
          (is-roof? [y dy] (= y (dec dy)))]
    (for [x (range dx)
          y (range dy)
          z (range dz)
          :when (or (is-wall? x dx) (is-wall? z dz) (is-roof? y dy))]
      [x y z material])))

(defn inspect-block [player]
  (wc/send-message me (.toString (wc/block (wc/target-block player)))))

;; Bind functions to tools
(defn give-truncator [player]
  (let [axe (wc/item-stack :diamond-axe)]
    (wc/set-display-name axe "Truncator")
    (wc/set-lore axe [:italic "A magical dwarv-"])
    (wc/add-inventory player axe)))

(defn give-deveiner [player]
  (let [pick (wc/item-stack :diamond-pickaxe)]
    (wc/set-display-name pick "De-Veiner")
    (wc/set-lore pick [:italic "A pickaxe infused with witchery"])
    (wc/add-inventory player pick)))

(defn give-peeper [player]
  (let [spyglass (wc/item-stack :spyglass)]
    (wc/set-display-name spyglass "Peeper")
    (wc/set-lore spyglass [:italic "A spyglass infused with witchery"])
    (wc/add-inventory player spyglass)))

(defn register-tool-listener! []
  (e/listen!
   :player-interact
   ::peeper
   (fn [{:keys [clickedBlock player]}]
     (when (and clickedBlock (= "Peeper" (wc/display-name (wc/item-in-hand player))))
       (inspect-block player))))
  (e/listen!
   :block-break
   ::truncator
   (fn [{:keys [block player]}]
     (when (and block (= "Truncator" (wc/display-name (wc/item-in-hand player))))
       (fell-tree player (wc/block block))))))
  (e/listen!
   :block-break
   ::deveiner
   (fn [{:keys [block player]}]
     (when (and block (= "De-Veiner" (wc/display-name (wc/item-in-hand player))))
       (mine-vein player (wc/block block))))))
(register-tool-listener!)

(def me (wc/player "maximus1233"))
(def pos {:x -7 :y 72 :z -62})
(def my-house (house 7 4 7 :dark-oak-planks))
(get-materials my-house)
(build me pos my-house)
(deconstruct me my-house)
(fell-tree me (wc/target-block me))
(plant-crops me (wc/target-block me))
(harvest-crops me (wc/target-block me))
(mine-vein me {:x 0 :y 72 :z -68})
(scan me (wc/target-block me) 3 3 3)
(wc/clear-weather)

(wc/send-message me "foo")
(.broadcastMessage (wc/server) "foo")

(.getInventory (wc/get-target-block me))
(.getType (first (.getContents (wc/get-inventory (wc/get-target-block me)))))
(wc/get-inventory [-5 72 -63])
(wc/break-naturally (wc/target-block me))

(wc/fly! me)
