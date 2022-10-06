(ns coy.cronos-sword
  (:require [lambdaisland.witchcraft :as wc]
            [lambdaisland.witchcraft.events :as e]
            [lambdaisland.witchcraft.markup :as markup])
  (:import (org.bukkit.event.entity EntityDamageByEntityEvent)
           (org.bukkit.entity HumanEntity)
           (org.bukkit.material SpawnEgg)
           (org.bukkit.inventory ItemStack)))

(def cronos-sword-name
  (delay
    (wc/normalize-text
     (markup/fAnCy "Crono's Sword"
                   [:red]))))

(defn create-cronos-sword []
  (let [sword (wc/item-stack :netherite-sword)]
    (wc/set-display-name sword @cronos-sword-name)
    (wc/set-lore sword
                 [["Hack enemies to the past."]])
    sword))

(defn give-to-player [player]
  (wc/add-inventory player (create-cronos-sword)))

(defn register-listener! []
  (e/listen!
   :entity-death
   ::coy
   (fn [{:keys [entity]}]
    (let [damage-cause (.getLastDamageCause entity) entity-location (.getLocation entity)]
        (when (instance? EntityDamageByEntityEvent damage-cause)
            (let [damager (.getDamager damage-cause)]
            (when (and
                   (instance? HumanEntity damager)
                   (= (wc/display-name (wc/item-in-hand damager)) @cronos-sword-name))
            (.dropItem (wc/world "world") entity-location (.toItemStack (SpawnEgg. (.getType entity)) 1)))))))))

(give-to-player (wc/player))

(register-listener!)

(wc/player)
