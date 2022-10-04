(ns net.maxtrussell.witchcraft
  (:require [lambdaisland.witchcraft :as wc]
            [clojure.set :as set]
            [clojure.string :as string]))

(def tool-durabilities {:gold 32 :wood 59 :stone 131 :iron 250 :diamond 1561 :netherite 2031})
(def seeds {:wheat-seeds :wheat, :carrot :carrot, :potato :potato})
(def crops #{:wheat :carrots :potatoes :beetroots})
(def crops-mature {:wheat 7 :carrots 7 :potatoes 7 :beetroots 3})
(def drop-table {:redstone-ore {:redstone #(roll 4 5)}
                 :lapis-ore {:lapis-lazuli #(roll 4 9)}
                 :copper-ore {:raw-copper #(roll 2 5)}
                 :wheat {:wheat #(roll 1 1) :wheat-seeds #(roll 0 3)}
                 :potatoes {:potato #(roll 1 5)
                            :poisonous-potato (fn [] (if (>= 98 (rand-int 100)) 1 0))}
                 ;; FIXME: Carrot isn't 100% accurate,
                 ;;        see: https://minecraft.fandom.com/wiki/Carrot#Breaking
                 :carrots {:carrot #(roll 2 5)}
                 :beetroots {:beetroot #(roll 1 1) :beetroot-seeds #(roll 1 4)}})

(defn roll [min max]
  "Generates an int from min to max, inclusive"
  (+ min (rand-int (- (inc max) min))))

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
  "Harvest entire tree"
  (harvest player loc (fn [block] (string/ends-with? (get block :material) "-log"))))

(defn mine-vein [player loc]
  "Harvest entire ore vein"
  (harvest player loc (fn [block] (string/ends-with? (get block :material) "-ore"))))

(defn harvest-crops [player loc]
  "Harvest mature crops from farm"
  (letfn [(crop-age [block] (get (get block :block-data) :age))]
    (let [crops #{:wheat :carrots :potatoes :beetroots}]
      (harvest player loc (fn [block] (and
                                       (crops (get block :material))
                                       (>=
                                        (crop-age block)
                                        (get crops-mature (get block :material)))))))))

(defn harvest [player loc pred?]
  "Use dfs to remove all blocks and give to player, damaging the equipped tool"
  (letfn [(remove-block! [block] (wc/set-block (wc/xyz block) :air))]
    ;; TODO: Set limit as max(tool-capacity 200)
    (let* [harvested (dfs loc pred? remove-block! :limit 200)
           tool (wc/item-in-hand player)
           tool-use (reduce (fn [acc [_ qty]] (+ acc qty)) 0 harvested)]
      ;; Do double damage to the tool for each block harvested
      (.setDurability tool (+ (.getDurability tool) (* 2 tool-use)))
      (doseq [[material qty] harvested]
        (let [yield (reduce add-maps (take qty (repeatedly #(get-drop material))))]
          (doseq [[k v] yield] (wc/add-inventory me k v))))
      harvested)))

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
(fell-tree me (wc/target-block me))
(plant-crops me {:x 2 :y 71 :z -67})
(harvest-crops me {:x 2 :y 72 :z -67})
(mine-vein me {:x 0 :y 72 :z -68})
(wc/clear-weather)

(.getInventory (wc/get-target-block me))
(.getType (first (.getContents (wc/get-inventory (wc/get-target-block me)))))
(wc/get-inventory [-5 72 -63])
