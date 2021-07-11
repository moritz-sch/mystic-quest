;This program is free software: you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software Foundation, either version 3 of the License, or
;(at your option) any later version.

;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.

;You should have received a copy of the GNU General Public License
;along with this program.  If not, see <https://www.gnu.org/licenses/>.

(ns mystic-quest.data
  (:require [mystic-quest.arithmetic :refer [<hex]]))

(defn reverse-map
  "Exchanges the roles of keys and values."
  [to-reverse]
  (reduce (fn [xs x] (assoc xs (second x) (first x))) {} to-reverse))
  

(def enemy-names-map {"10" "Minidevil"
                      "11" "Rabite"
                      "12" "Goblin"
                      "13" "Myconid"
                      "14" "Grell"
                      "15" "Mudman"
                      "16" "Lizardman"
                      "17" "Death Flower"
                      "18" "Green Slime"
                      "19" "Skeleton"
                      "1A" "Roper"
                      "1B" "Land Leech"
                      "1C" "Zombie"
                      "1D" "Wererat"
                      "1E" "Pumpkin Bomb"
                      "1F" "Blood Owl"
                      "20" "Killer Bee"
                      "21" "Gas Cloud"
                      "22" "Orc"
                      "23" "Death Crab"
                      "24" "Tarantula"
                      "25" "Werewolf (Kett's)"
                      "26" "Mimic"
                      "27" "Ruster"
                      "28" "Porcupine"
                      "29" "Mandrake"
                      "2A" "Eye Spy"
                      "2B" "Werewolf"
                      "2C" "Ghost"
                      "2D" "Basilisk"
                      "2E" "Death Scorpion"
                      "2F" "Saurus"
                      "30" "Pakkun Lizard"
                      "31" "Mummy"
                      "32" "Cobra"
                      "33" "Shadow Zero"
                      "34" "Magician"
                      "35" "Red Wisp"
                      "36" "Gargoyle"
                      "37" "Ape"
                      "38" "Molebear"
                      "39" "Ogre"
                      "3A" "Banejako"
                      "3B" "Phantasm"
                      "3C" "Minotaur"
                      "3D" "Wizard"
                      "3E" "Darkstalker"
                      "3F" "Dark Lord"
                      "40" "Mega Xorn"
                      "41" "Dragonfly"
                      "42" "Bulette"
                      "43" "Snowman"
                      "44" "Saber Cat"
                      "45" "Walrus"
                      "46" "Duck Soldier"
                      "47" "Poto"
                      "48" "Air Element"
                      "49" "Beholder"
                      "4A" "Manta Ray"
                      "4B" "Griffin Hand"
                      "4C" "Tortoise Knight"
                      "4D" "Fire Moth"
                      "4E" "Earth Element"
                      "4F" "Denden"
                      "50" "Doppel Mirror"
                      "51" "Guardian"
                      "52" "Evil Sword"
                      "53" "Death Gauntlet"
                      "54" "Garasha"
                      "55" "Wonder"
                      "56" "Mammoth"
                      "57" "Ninja"
                      "58" "Julius"
                      "59" "Demon"
                      ;"5A" unused?
                      "5B" "Sahagin"
                      "5C" "Sea Dragon"
                      "5D" "Gall Fish"}) ;where is Amanda?

(def used-enemy-ids (sort <hex (keys enemy-names-map)))
(def enemy-names-map-r (reverse-map enemy-names-map))
(def unused-enemy-ids ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F" "5A" "5E" "5F" "60" "61" "62"])
(def enemy-ids (sort <hex (into used-enemy-ids unused-enemy-ids)))


(def enemy-names-map-drops {"10" "Minidevil"
                            "11" "Rabite"
                            "12" "Goblin"
                            "13" "Myconid"
                            "14" "Grell"
                            "15" "Mudman"
                            "16" "Lizardman"
                            "17" "Death Flower"
                            "18" "Green Slime"
                            "19" "Skeleton"
                            "1A" "Roper"
                            "1B" "Land Leech"
                            "1C" "Zombie"
                            "1D" "Wererat"
                            "1E" "Pumpkin Bomb"
                            "1F" "Blood Owl"
                            "20" "Killer Bee"
                            "21" "Gas Cloud"
                            "22" "Orc"
                            "23" "Death Crab"
                            "24" "Tarantula"
                            "29" "Mimic"
                            "2A" "Ruster"
                            "2B" "Porcupine"
                            "2C" "Mandrake"
                            "2D" "Eye Spy"
                            "2E" "Werewolf"
                            "2F" "Ghost"
                            "30" "Basilisk"
                            "31" "Death Scorpion"
                            "32" "Saurus"
                            "33" "Mummy"
                            "34" "Pakkun Lizard"
                            "35" "Cobra"
                            "36" "Shadow Zero"
                            "37" "Magician"
                            "38" "Red Wisp"
                            "39" "Gargoyle"
                            "3A" "Ape"
                            "3B" "Molebear"
                            "3C" "Ogre"
                            "3D" "Banejako"
                            "3E" "Phantasm"
                            "3F" "Minotaur"
                            "40" "Wizard"
                            "41" "Darkstalker"
                            "42" "Dark Lord"
                            "43" "Mega Xorn"
                            "44" "Dragonfly"
                            "45" "Bulette"
                            "46" "Snowman"
                            "47" "Saber Cat"
                            "48" "Walrus"
                            "49" "Duck Soldier"
                            "4A" "Poto"
                            "4B" "Air Element"
                            "4C" "Beholder"
                            "4D" "Manta Ray"
                            "4E" "Griffin Hand"
                            "4F" "Tortoise Knight"
                            "50" "Fire Moth"
                            "51" "Earth Element"
                            "52" "Denden"
                            "53" "Doppel Mirror"
                            "54" "Guardian"
                            "55" "Evil Sword"
                            "56" "Death Gauntlet"
                            "57" "Garasha"
                            "58" "Wonder"
                            "59" "Mammoth"
                            "5A" "Ninja"
                            "5B" "Julius"
                            "5C" "Demon"
                            "5E" "Sahagin"
                            "5F" "Sea Dragon"
                            "60" "Gall Fish"}) ;Amanda?
                            
(def enemy-names-map-drops-r (reverse-map enemy-names-map-drops))



(def boss-names-map {"0"  "Vampire"
                     "1"  "Hydra"
                     "2"  "Medusa"
                     "3"  "Megapede"
                     "4"  "Mind Flayer"
                     "5"  "Golem"
                     "6"  "Cyclops"
                     "7"  "Chimera"
                     "8"  "Kary"
                     "9"  "Kraken"
                     "A"  "Iflyte"
                     "B"  "Lich"
                     "C"  "Garuda"
                     "D"  "Dragon"
                     "E"  "Julius II"
                     "F"  "Dragon Zombie"
                     "10" "Jackal"
                     "11" "Julius III"
                     "12" "Metal Crab"
                     "13" "Mantis Ant"
                     "14" "Red Dragon"})

(def boss-ids (sort <hex (keys boss-names-map)))
(def boss-names-map-r (reverse-map boss-names-map))
(def boss-order-in-game ["10" "1" "0" "3" "2" "4" "12" "6" "5" "7" "8" "9" "A" "B" "13" "C" "D" "14" "F" "E" "11"])
(def boss-order-in-data-2 ["0" "1" "F" "2" "4" "5" "E" "6" "7" "8" "A" "C" "D" "B" "9" "3" "10" "11" "12" "13"])



(def attack-names-map {"0"  "Throwing Knife"    ;ALLE noch einmal ueberpruefen!
                       "1"  "Strange Voice"
                       "2"  "Needles"
                       "3"  "Harpoon"
                       "4"  "Mirror Image"
                       "5"  "Beam"
                       "6"  "Throwing Axe"
                       "7"  "Scissors"
                       "8"  "Ring Beam"
                       "9"  "Rock"
                       "A"  "Shuriken"
                       "B"  "Lightning Bolt"
                       "C"  "Fireball"
                       "D"  "Blizzard"
                       "E"  "Thunder"
                       "F"  "Poison Thread"
                       "10" "Spear"
                       "11" "Sword"
                       "12" "Rapier"
                       "13" "Scorpion Tail"
                       "14" "Flare (Dragon)"
                       "15" "Claw"
                       "16" "Flare (Julius)"
                       "17" "Mimic Attack"
                       "18" "Sword (Bogard)?"
                       "19" "Throwing Axe (Watts)?"
                       "1A" "Dagger (Amanda)?"
                       "1B" "Arrow (Lester)?"
                       "1C" "Laser (Marcie)?"
                       "1D" "Lightning Bolt (Red Mage)?"
                       "1E" "Bat"
                       "1F" "Flame (Hydra)"
                       "20" "Snake"
                       "21" "Wing"
                       "22" "Skull"
                       "23" "Flame (Chimera)"
                       "24" "Thunder (Julius II)"
                       "25" "Flare (Julius III)"
                       "26" "Flame (Mind Flayer)"
                       "27" "Flame (Dragon Zombie)"
                       "FF" "(none)"})

(def attack-ids (sort <hex (keys attack-names-map)))
(def attack-names-map-r (reverse-map attack-names-map))


;pointer to scripts -> items
(def drop-map-scripts {'(0    0)  "(none)"
                       '(255 84)  "Candy"
                       '(23  85)  "Ether, Wisdom"
                       '(58  85)  "Cure"
                       '(105 85)  "Pure"
                       '(150 85)  "X-Cure"
                       '(175 85)  "Ether"
                       '(222 85)  "Key"
                       '(245 85)  "Mattock"
                       '(15  86)  "Fang"
                       '(59  86)  "Candy, Gold"
                       '(117 86)  "Crystal"
                       '(166 86)  "X-Ether"
                       '(191 86)  "Unicorn"
                       '(217 86)  "Will"
                       '(255 86)  "Aegis Shield"
                       '(56  87)  "Elixir, Samurai Armor"
                       '(112 87)  "X-Ether, Samurai Helmet"
                       '(56  88)  "Thunder"
                       '(80  88)  "Pillow"
                       '(105 88)  "Ruby"
                       '(129 88)  "Opal"
                       '(152 88)  "Flame"
                       '(176 88)  "Blizzard"
                       '(202 88)  "Nectar"
                       '(227 88)  "Stamina"
                       '(39  90)  "Silence"})

;jumps -> items
(def drop-map-jumps {'(0   0)  "(none)"
                     '(56  4)  "Candy"
                     '(57  4)  "Ether, Wisdom"
                     '(58  4)  "Cure"
                     '(60  4)  "Pure"
                     '(62  4)  "X-Cure"
                     '(64  4)  "Ether"
                     '(66  4)  "Key"
                     '(67  4)  "Mattock"
                     '(68  4)  "Fang"
                     '(69  4)  "Candy, Gold"
                     '(71  4)  "Crystal"
                     '(73  4)  "X-Ether"
                     '(74  4)  "Unicorn"
                     '(75  4)  "Will"
                     '(77  4)  "Aegis Shield"
                     '(78  4)  "Elixir, Samurai Armor"
                     '(79  4)  "X-Ether, Samurai Helmet"
                     '(82  4)  "Thunder"
                     '(83  4)  "Pillow"
                     '(85  4)  "Ruby"
                     '(86  4)  "Opal"
                     '(87  4)  "Flame"
                     '(88  4)  "Blizzard"
                     '(89  4)  "Nectar"
                     '(90  4)  "Stamina"
                     '(103 4)  "Silence"})
                     
(def drop-map-jumps-r (reverse-map drop-map-jumps))
                     
               

(def weapon-groups-map {7 "Broad Sword, Blood Sword, Dragon Sword, Rusty Sword, Battle Axe, Zeus Axe, Sickle, Chain, Wind Spear"
                        6 "Silver Sword"
                        5 "Star (Flail), Mattock"
                        4 "Flame (Whip), Fire"
                        3 "Ice Sword"
                        2 "Thunder Spear, Lightning"
                        1 "Nuke, Excalibur"})

(def status-effects-map {7 "Poison"
                         6 "Dark"
                         5 "Stone"
                         4 "Moogle"})


(def spell-map {"0" "Cure"
                "1" "Heal"
                "2" "Mute"
                "3" "Sleep"
                "4" "Fire"
                "5" "Ice"
                "6" "Lightning"
                "7" "Nuke"})

(def spell-ids (sort <hex (keys spell-map)))
(def spell-map-r (reverse-map spell-map))


(def item-map {"0"  {:name "Cure" :type :item}
               "1"  {:name "X-Cure" :type :item}
               "2"  {:name "Ether" :type :item}
               "3"  {:name "X-Ether" :type :item}
               "4"  {:name "Elixir" :type :item}
               "5"  {:name "Pure" :type :item}
               "6"  {:name "Eyedrop" :type :item}
               "7"  {:name "Soft" :type :item}
               "8"  {:name "Moogle" :type :item}
               "9"  {:name "Unicorn" :type :item}
               "A"  {:name "Silence" :type :item}
               "B"  {:name "Pillow" :type :item}
               "C"  {:name "Unused" :type :unused}
               "D"  {:name "Unused" :type :unused}
               "E"  {:name "Flame" :type :item}
               "F"  {:name "Blaze" :type :item}
               "10" {:name "Blizzard" :type :item}
               "11" {:name "Frost" :type :item}
               "12" {:name "Lightning Bolt" :type :item}
               "13" {:name "Thunder" :type :item}
               "14" {:name "Candy" :type :item}
               "15" {:name "Unused" :type :unused}
               "16" {:name "Key" :type :item}
               "17" {:name "Bone Key" :type :item}
               "18" {:name "Bronze Key" :type :item}
               "19" {:name "Unused" :type :unused}
               "1A" {:name "Unused" :type :unused}
               "1B" {:name "Unused" :type :unused}
               "1C" {:name "Unused" :type :unused}
               "1D" {:name "Unused" :type :unused}
               "1E" {:name "Mirror" :type :item}
               "1F" {:name "Unused" :type :unused}
               "20" {:name "Unused" :type :unused}
               "21" {:name "Amanda" :type :item}
               "22" {:name "Unused" :type :unused}
               "23" {:name "Unused" :type :unused}
               "24" {:name "Oil" :type :item}
               "25" {:name "Unused" :type :unused}
               "26" {:name "Unused" :type :unused}
               "27" {:name "Unused" :type :unused}
               "28" {:name "Unused" :type :unused}
               "29" {:name "Crystal" :type :item}
               "2A" {:name "Unused" :type :unused}
               "2B" {:name "Nectar" :type :item}
               "2C" {:name "Stamina" :type :item}
               "2D" {:name "Wisdom" :type :item}
               "2E" {:name "Will" :type :item}
               "2F" {:name "Unused" :type :unused}
               "30" {:name "Unused" :type :unused}
               "31" {:name "Gold" :type :item}
               "32" {:name "Fang" :type :item}
               "33" {:name "Unused" :type :unused}
               "34" {:name "Unused" :type :unused}
               "35" {:name "Mattock" :type :item}
               "36" {:name "Ruby" :type :item}
               "37" {:name "Opal" :type :item}
               "38" {:name "Unused" :type :unused}
               "39" {:name "Broad Sword" :type :weapon}
               "3A" {:name "Battle Axe" :type :weapon}
               "3B" {:name "Sickle" :type :weapon}
               "3C" {:name "Chain" :type :weapon}
               "3D" {:name "Silver Sword" :type :weapon}
               "3E" {:name "Wind Spear" :type :weapon}
               "3F" {:name "Were Axe" :type :weapon}
               "40" {:name "Star (Flail)" :type :weapon}
               "41" {:name "Blood Sword" :type :weapon}
               "42" {:name "Dragon Sword" :type :weapon}
               "43" {:name "Flame Sword" :type :weapon}
               "44" {:name "Ice Sword" :type :weapon}
               "45" {:name "Zeus Axe" :type :weapon}
               "46" {:name "Rusty Sword" :type :weapon}
               "47" {:name "Thunder Spear" :type :weapon}
               "48" {:name "Excalibur" :type :weapon}
               "49" {:name "Bronze Armor" :type :armor}
               "4A" {:name "Iron Armor" :type :armor}
               "4B" {:name "Silver Armor" :type :armor}
               "4C" {:name "Gold Armor" :type :armor}
               "4D" {:name "Flame Armor" :type :armor}
               "4E" {:name "Ice Armor" :type :armor}
               "4F" {:name "Dragon Armor" :type :armor}
               "50" {:name "Samurai Armor" :type :armor}
               "51" {:name "Opal Armor" :type :armor}
               "52" {:name "Unused" :type :unused}
               "53" {:name "Unused" :type :unused}
               "54" {:name "Bronze Shield" :type :shield}
               "55" {:name "Iron Shield" :type :shield}
               "56" {:name "Silver Shield" :type :shield}
               "57" {:name "Gold Shield" :type :shield}
               "58" {:name "Flame Shield" :type :shield}
               "59" {:name "Dragon Shield" :type :shield}
               "5A" {:name "Aegis Shield" :type :shield}
               "5B" {:name "Opal Shield" :type :shield}
               "5C" {:name "Ice Shield" :type :shield}
               "5D" {:name "Unused" :type :unused}
               "5E" {:name "Unused" :type :unused}
               "5F" {:name "Bronze Helmet" :type :helmet}
               "60" {:name "Iron Helmet" :type :helmet}
               "61" {:name "Silver Helmet" :type :helmet}
               "62" {:name "Gold Helmet" :type :helmet}
               "63" {:name "Opal Helmet" :type :helmet}
               "64" {:name "Samurai Helmet" :type :helmet}})

(def item-ids (sort <hex (keys item-map)))
(def used-item-map (reduce (fn [xs [k v]] (assoc xs k v)) {} (filter (fn [item] (not= (:type (second item)) :unused)) item-map)))
(def used-item-ids (sort <hex (keys used-item-map)))
               
(def item-map-r (reduce (fn [xs x] (assoc xs (:name (second x)) (first x))) {} item-map))


(def shop-map {"0"  ["Topple Item Shop"]
               "1"  ["Shop on path to Kett's" "Wendel Item Shop 1"]
               "2"  ["Wendel Item Shop 2"]
               "3"  ["Menos Item Shop" "Jadd Item Shop" "Ish Item Shop" "Snowfields Item Shop" "Floatrocks Item Shop"]
               "4"  ["Topple Weapon Shop"]
               "5"  ["Unused"]
               "6"  ["Watts"]
               "7"  ["Airship Vendor"]
               "8"  ["Menos Weapon Shop"]
               "9"  ["Jadd Weapon Shop"]
               "A"  ["Mt. Rocks Area Weapon Shop"]
               "B"  ["Ish Weapon Shop"]
               "C"  ["Ammonite Coast Weapon Shop"]
               "D"  ["Floatrocks Weapon Shop"]
               "E"  ["Unused"]
               "F"  ["Mt. Rocks Area Magic Shop"]
               "10" ["Floatrocks Magic Shop" "Ish Magic Shop"]})

(def shop-ids (sort <hex (keys shop-map)))


(def levels (range 2 102))

;
;Demon 5C       X-Ether, Samurai Helmet     E12 (E810)     (79 4)      2206        46      (112 87)
;Ghost 2F       Ether, Wisdom               9DA (E3D8)     (57 4)      2162        2       (23 85)
;Ninja 5A       Elixir, Samurai Armor       DE2 (E7E0)     (78 4)      2204        44      (56 87)
;Orc 22         Candy, Gold                 8A2 (E2A0)     (69 4)      2186        26      (59 86)
;Poto 4A        X-Ether, Elixir             C62 (E660)     (73 4)      2194        34      (166 86)
;Porcupine 2B   Candy, Gold                 97A (E378)     (69 4)      2186        26      (59 86)
;Shadow Zero    Ether, Wisdom               A82 (E480)     (57 4)      2162        2       (23 85)
;Zombie         Ether, Wisdom               812 (E210)     (57 4)      2162        2       (23 85)


;probably wrong in wiki:
;Eye Spy 2A         Ether, Will? (wiki: Ether)
;Grell 14           none (w: Candy)
;Mudman 15          none (w: Candy)
;Myconid 13         none (w: Ether, Will)
;Shadow Zero 36     Ether, Wisdom (w: Ether)

