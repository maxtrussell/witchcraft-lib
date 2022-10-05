(ns coy.copy
    (:require [lambdaisland.witchcraft :as wc]
              [lambdaisland.witchcraft.events :as e]))

(defn apply-fun-to-cords [fun blockv cord]
  "apply fun to cord coordinate and blokv coordinate"
  (reduce #(assoc %1 %2 (fun (get blockv %2) (get cord %2))) blockv [0 1 2])
  )

(defn apply-set-blocks-to-cords [cords]
  "Add a block to cords and apply set-blocks to cords."
  (let [cords (into [](map #(conj % :diamond-block) cords))]
    (wc/set-blocks cords)))


(defn transform-cord-axis-lists [cord-axis-lists]
  "Return a list of coordinates in xyz format.
   Input is a list of values for each axis."
    (for [x (get cord-axis-lists 0)
          y (get cord-axis-lists 1)
          z (get cord-axis-lists 2)]
      [x y z]))


(defn get-min-max-cords-list [cords-list]
 "Get min and maximum of each axis in a set of coordinates.
  Returns (min max) for each axis."
  (def default-min-max [100000, -100000])
  (def output [default-min-max, default-min-max, default-min-max])

  (doseq [cord cords-list]
    (dotimes [axis (count output)]
      (let [cord-axis (get cord axis)
            axis-min (get (get output axis) 0)
            axis-max (get (get output axis) 1)]

        (if (< cord-axis axis-min)
          (def output (assoc output axis (assoc (get output axis) 0 cord-axis))))
        (if (> cord-axis axis-max)
          (def output (assoc output axis (assoc (get output axis) 1 cord-axis)))))))
  output)

(defn min-max-range [min-max-list]
  "Create a list using inclusive range given a list of minimums and maximus."
  (def output [])
  (doseq [min-max min-max-list]
    (let [min (get min-max 0)
          max (get min-max 1)]

    (def output (conj output (range min (+ max 1))))))
  output)

(defn copy [cords]
  "Copy blocks at cord"
  (swap! clipboard  (fn [_] (mapv #(apply-fun-to-cords - % (get @ls 0)) (map #(wc/blockv %) cords)))))

(defn create-copy-stick []
  (let [stick (wc/item-stack :stick)]
    (wc/set-display-name stick "Copy Stick")
    (wc/set-lore stick ["Use to copy blocks."])
    stick))

(defn give-copy-stick-to-player [player]
  (wc/add-inventory player (create-copy-stick)))

(defn listen-for-fun [fun target-player]
  "Set up listener and pass cords passed by player to fun"
  (e/listen!
   :player-interact
   ::copy-setup
   (fn [{:keys [clickedBlock player action]}]
     (when (and
            (= target-player player)
            (not (nil? clickedBlock))
            (= (wc/display-name (wc/item-in-hand player)) "Copy Stick"))
        (let [location (wc/xyz clickedBlock) ls-count (count @ls)]
        (when (< ls-count 2)
            (reset! ls (conj @ls location)) ;; add cords of player interacted block to ls
            (when (= (count @ls) 2) ;; on two blocks get all cords, pass to fun, reset ls
                (let [ cords
                (transform-cord-axis-lists
                    (min-max-range
                    (get-min-max-cords-list @ls)))] ;; cords is [[xyz]]
                (fun cords))
                (reset! ls [])
                (wc/unlisten!
                :player-interact
                ::copy-setup))))))))

(defn paste [target-player pred?]
  (e/listen!
   :player-interact
   ::paste-setup
   (fn [{:keys [clickedBlock player action]}]
     (when (and
            (= target-player player)
            (not (nil? clickedBlock))
            (= (wc/display-name (wc/item-in-hand player)) "Copy Stick")
            (not (= @clipboard [])))
        (let [blocks (map #(apply-fun-to-cords + % (wc/xyz clickedBlock)) (filter pred? @clipboard))]
          (wc/set-blocks blocks)
          (wc/unlisten! :player-interact ::paste-setup)
          )))))

(defn paste-without-air [player]
  (paste player (fn [block] (not (= (wc/material block) (wc/material :air))))))

(defn listen-for-copy [player]
  (def ls (atom []))
  (def clipboard (atom []))
  (listen-for-fun copy player))

;; Testing function
(comment
  (give-copy-stick-to-player (wc/player "coytorreblanca"))
  (listen-for-copy (wc/player "coytorreblanca"))
  (wc/unlisten! :player-interact ::copy-setup)
  (wc/unlisten! :player-interact ::paste-setup)
  (wc/undo!)
  (prn @clipboard)

  (paste-without-air (wc/player "coytorreblanca"))

  (wc/fly! (wc/player)))
