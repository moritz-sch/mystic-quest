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

(ns mystic-quest.core
  (:require [mystic-quest.data :refer :all]
            [mystic-quest.version :refer :all]
            [mystic-quest.arithmetic :refer :all]
            [seesaw.core :refer :all]
            [seesaw.chooser :refer :all]
            [seesaw.mig :refer :all]
            [clojure.java.io])
  (:gen-class))

;-------------------------------------------------------------------------------
; ATOMS
;-------------------------------------------------------------------------------

(defn create-atom-f
  [atom]
  `(def ~atom (atom nil)))
  
(defmacro initialize-atoms
  "Initializes atoms as nil."
  [atoms]
  `(do ~@(map create-atom-f atoms)))
  
(initialize-atoms [current-file rom-version rom-parts rom-values-enemy editor-values-enemy rom-values-enemy-drop editor-values-enemy-drop rom-values-boss editor-values-boss rom-values-attack editor-values-attack rom-values-spell editor-values-spell rom-values-item editor-values-item rom-values-shop editor-values-shop rom-values-experience editor-values-experience])

;-------------------------------------------------------------------------------

(defn multi-split-at-hex
  "Splits xs in a number of sequences similar to split-at only with potentially more than one splitting points, the splitting points are given in splits as hexadecimal numbers."
  [xs splits]
  (loop [[next-split & remaining-splits] (map hexadecimal-to-decimal splits)
         ys xs
         zs []]
    (if next-split
      (recur (map #(- % next-split) remaining-splits) (drop next-split ys) (conj zs (take next-split ys)))
      (conj zs ys))))

(defn get-n-elements-at
  "Retrieves number-of-bytes elements of a seq from beginning at address (given as a string representing a hexadecimal number), makes them positive."
  [xs address number-of-bytes]
  (->> xs
       (drop (hexadecimal-to-decimal address))
       (take number-of-bytes)
       (map make-pos)))

(defn convert-byte-to-bit-map
  "Takes an integer between 0 and 255 and returns it represented as a map with keys 0 to 7 with 0 representing the most significant bit."
  [byte]
  (loop [i          7
         power      1
         bit-map    {}]
    (if (neg? i)
      bit-map
      (recur (dec i) (* 2 power) (assoc bit-map i (not= 0 (bit-and byte power)))))))
    
(defn convert-bit-map-to-byte
  "Inverse of convert-byte-to-bit-map."
  [bitmap]
  (loop [i      7
         power  1
         byte   0]
    (if (neg? i)
      byte
      (let [s (if (get bitmap i) (+ byte power) byte)]
      (recur (dec i) (* 2 power) s)))))
      
;-------------------------------------------------------------------------------

;(def truncated-rom-scripts (drop 137089 @rom)) ; 137089 dec = 21781 hex 

;(defn get-jump-and-drop-script
;  "Returns a vector with the jump to the first pointer and the pointer of the drop script for the specified enemy. Used in the function create-drop-map"
;  [enemy-id]
;  (let [address-drop    (*hex enemy-id "18")
;        drop-jump-raw   (get-n-elements-at (@rom-parts "enemy-drop-data") address-drop 2)
;        drop-jump-dec   (* (+ (* 256 (second drop-jump-raw)) (first drop-jump-raw)) 2) ; now calculating with decimal numbers
;        drop-pointer    (- drop-jump-dec 2160)
;        drop-script     (if (neg? drop-pointer)
;                          (list 0 0)
;                          (get-n-elements-at truncated-rom-scripts (decimal-to-hexadecimal drop-pointer) 2))]
;    [drop-jump-raw drop-script]))

;this function was used to create drop-map-jumps
;(defn create-drop-map
;  "Creates a map with the jumps to the first pointer as keys and the corresponding items as values."
;  []
;  (let [enemy-ids (keys enemy-names-map)]
;    (reduce (fn [xs v] (apply assoc xs v)) {} (map (fn [id] (let [[jump drop-script] (get-jump-and-drop-script id)] [jump (drop-map-scripts drop-script)])) enemy-ids))))

;-------------------------------------------------------------------------------

(defn get-enemy-drop-id
  "Gives the ID of an enemy in drop data, input is the ID in enemy data."
  ([enemy-id]
    (enemy-names-map-drops-r (enemy-names-map enemy-id)))
  ([enemy-id enemy-name]
    (enemy-names-map-drops-r enemy-name)))

(defn extract-enemy-data-from-rom
  "Returns a map with data of the enemy with the given ID."
  [enemy-data enemy-drop-data enemy-id]
  (let [address-data    (*hex enemy-id "E")
        enemy-name      (enemy-names-map enemy-id)
        enemy-drop-id   (get-enemy-drop-id enemy-id enemy-name)
        address-drop    (*hex enemy-drop-id "18")
        data            (get-n-elements-at enemy-data address-data 14)
        drop-jump       (get-n-elements-at enemy-drop-data address-drop 2)]
    {:id enemy-id
     :name enemy-name
     :speed (first data)
     :hp (* 4 (second data))
     :b02 (nth data 2)
     :b03 (nth data 3)
     :immunities (convert-byte-to-bit-map (nth data 4))
     :b05 (nth data 5)
     :damage (nth data 6)
     :defense (nth data 7)
     :b08 (nth data 8)
     :attack (nth data 9)
     :b10 (nth data 10)
     :status-effects (convert-byte-to-bit-map (nth data 11))
     :exp (nth data 12)
     :gold (nth data 13)
     :drop drop-jump}))

(defn extract-enemy-drop-data-from-rom
  "Returns a map with drop data of the enemy with the given ID."
  [enemy-drop-data enemy-id]
  (let [address-drop   (*hex enemy-id "18")
        drop-jump      (get-n-elements-at enemy-drop-data address-drop 2)
        drop-rem       (get-n-elements-at enemy-drop-data (+hex address-drop "2") 22)]
    {:drop drop-jump
     :rem drop-rem}))
    
(defn extract-boss-data-from-rom
  "Returns a map with data of the boss with the given ID."
  [boss-data boss-data-2 boss-id]
  (let [address-data    (*hex boss-id "18")
        data            (get-n-elements-at boss-data address-data 24)
        address-data-2  (+ (nth data 16) (* 256 (nth data 17)) -18737)
        starts-data-2   '(0 64 120 176 208 272 320 408 464 512 584 640 704 704 776 832 912 1000 1048 1112 1168 1216)
        number-blocks-2 (/ (- (first (drop-while #(<= % address-data-2) starts-data-2)) address-data-2) 8)
        data-2          (get-n-elements-at boss-data-2 (decimal-to-hexadecimal address-data-2) (* 8 number-blocks-2))]
    {:id boss-id
     :name (boss-names-map boss-id)
     :speed (first data)
     :hp (* 16 (second data))
     :exp (nth data 2)
     :gold (nth data 3)
     :b04 (nth data 4)
     :attack (nth data 5)
     :b06 (nth data 6)
     :b07 (nth data 7)
     :b08 (nth data 8)
     :b09 (nth data 9)
     :b10 (nth data 10)
     :b11 (nth data 11)
     :b12 (nth data 12)
     :b13 (nth data 13)
     :b14 (nth data 14)
     :b15 (nth data 15)
     :pointer-low (nth data 16)
     :pointer-high (nth data 17)
     :b18 (nth data 18)
     :b19 (nth data 19)
     :b20 (nth data 20)
     :b21 (nth data 21)
     :b22 (nth data 22)
     :b23 (nth data 23)
     :number-blocks-in-data-2 number-blocks-2
     :blocks-in-data-2 (zipmap (range number-blocks-2) (map (fn [i] {
       :b00 (nth data-2 (* 8 i))
       :immunities (convert-byte-to-bit-map (nth data-2 (+ (* 8 i) 1)))
       :b02 (nth data-2 (+ (* 8 i) 2))
       :damage (nth data-2 (+ (* 8 i) 3))
       :defense (nth data-2 (+ (* 8 i) 4))
       :b05 (nth data-2 (+ (* 8 i) 5))
       :status-effects (convert-byte-to-bit-map (nth data-2 (+ (* 8 i) 6)))
       :b07 (nth data-2 (+ (* 8 i) 7))}) (range number-blocks-2)))}))

(defn extract-attack-data-from-rom
  "Returns a map with data of the attack with the given ID."
  [attack-data attack-id]
  (let [address-data    (*hex attack-id "10")
        data            (get-n-elements-at attack-data address-data 16)]
    {:id attack-id
     :name (attack-names-map attack-id)
     :b00 (nth data 0)
     :speed (nth data 1)
     :b02 (nth data 2)
     :b03 (nth data 3)
     :damage (nth data 4)
     :b05 (nth data 5)
     :b06 (nth data 6)
     :b07 (nth data 7)
     :b08 (nth data 8)
     :b09 (nth data 9)
     :b10 (nth data 10)
     :b11 (nth data 11)
     :b12 (nth data 12)
     :b13 (nth data 13)
     :b14 (nth data 14)
     :b15 (nth data 15)}))

(defn extract-shop-data-from-rom
  "Returns a map with the data of the shop with the given ID."
  [shop-data shop-id]
  (let [address-data    (*hex shop-id "10")
        data            (get-n-elements-at shop-data address-data 16)]
    {:id1 (nth data 0)
     :id2 (nth data 1)
     0 {:item (nth data 2) :used (nth data 3)}
     1 {:item (nth data 4) :used (nth data 5)}
     2 {:item (nth data 6) :used (nth data 7)}
     3 {:item (nth data 8) :used (nth data 9)}
     4 {:item (nth data 10) :used (nth data 11)}
     5 {:item (nth data 12) :used (nth data 13)}
     6 {:item (nth data 14) :used (nth data 15)}}))
     
(defn extract-spell-data-from-rom
  "Returns a map with the data of the spell with the given ID."
  [spell-data spell-id]
  (let [address-data    (*hex "10" spell-id)
        data            (get-n-elements-at spell-data address-data 16)]
    {:id (nth data 0)
     :name (spell-map spell-id)
     :b01 (nth data 1)
     :b02 (nth data 2)
     :b03 (nth data 3)
     :b04 (nth data 4)
     :b05 (nth data 5)
     :b06 (nth data 6)
     :b07 (nth data 7)
     :b08 (nth data 8)
     :mp-cost (- (nth data 9) 64)
     :b10 (nth data 10)
     :b11 (nth data 11)
     :b12 (nth data 12)
     :b13 (nth data 13)
     :b14 (nth data 14)
     :b15 (nth data 15)}))
     
(defn extract-item-data-from-rom
  "Returns a map with the data of the item with the given ID."
  [item-data item-id]
  (let [address-data    (*hex "10" item-id)
        data            (get-n-elements-at item-data address-data 16)]
    {:id item-id
     :name (get-in item-map [item-id :name])
     :type (get-in item-map [item-id :type])
     :b00 (first data)
     :b01 (second data)
     :b02 (nth data 2)
     :b03 (nth data 3)
     :b04 (nth data 4)
     :b05 (nth data 5)
     :b06 (nth data 6)
     :b07 (nth data 7)
     :b08 (nth data 8)
     :b09 (nth data 9)
     :b10 (nth data 10)
     :resistances (convert-byte-to-bit-map (nth data 11))     ;for shields
     ;see http://www6.plala.or.jp/fom/seiken/bogu.html for more information, A corresponds to bit 7, B to bit 6 etc, bit 0 is always set, I don't know what it does
     :b12 (nth data 12)
     :damage-defense (nth data 13)  ;damage for weapons, defense for armor and helmet
     :price-low (nth data 14)
     :price-high (nth data 15)}))

(defn extract-exp-data-from-rom
  "Returns a map with the numbers 2 to 102 as keys and the number of experience points required to reach the corresponding level as value, experience points are given as three tuples in the way the are stored in the ROM."
  [experience-data level]
  (let [address-data    (decimal-to-hexadecimal (* 3 (dec (dec level))))]
    (get-n-elements-at experience-data address-data 3)))

;-------------------------------------------------------------------------------

(defn ensure-number
  "Checks if n is a number, if not returns false, otherwise n converted to integer."
  [n]
  (try
    (int n)
    (catch Exception e false)))

(defn sanitize-hp
  "Takes an actual HP value input and returns the value to be put in the rom. Too small or large numbers are set to the minimum or maximum number of HP possible."
  [hp-value]
  (let [hp-value-mult-4 (- hp-value (rem hp-value 4))]
    (if (< hp-value-mult-4 4)
      4
      (min 1020 hp-value-mult-4))))
      
(defn fit-into-range
  "Fits the number i into the range (0..upper-bound)."
  [upper-bound i]
  (if (< i 0)
    0
    (min upper-bound i)))

(def fit-into-byte (partial fit-into-range 255))
    
(defn sanitize-hp-boss
  "Takes an actual HP value input for a boss and returns the value to be put in the rom. Too small or large numbers are set to the minimum or maximum number of HP possible."
  [hp-value]
  (let [hp-value-mult-16 (- hp-value (rem hp-value 16))]
    (if (< hp-value-mult-16 16)
      16
      (min 4080 hp-value-mult-16))))

;-------------------------------------------------------------------------------

(defn update-hp
  "Updates the HP value of an enemy in the provided atom."
  [atom enemy-id new-hp]
  (if-let [validated-new-hp (ensure-number new-hp)]
    (swap! atom (fn [current] (assoc-in current [enemy-id :hp] (sanitize-hp validated-new-hp))))))

(defn update-byte
  "Updates the value corresponding to the key k for the enemy given by enemy-id in the provided atom."
  [atom enemy-id k new-value]
  (if-let [validated-new-value (ensure-number new-value)]
    (swap! atom (fn [current] (assoc-in current [enemy-id k] (fit-into-byte validated-new-value))))))
    
(defn update-bit
  "Updates the specified bit of the value corresponding to the key k for the enemy given by enemy-id in the provided atom."
  [atom enemy-id k bit new-value]
   (swap! atom (fn [current] (assoc-in current [enemy-id k bit] new-value))))

(defn update-drop
  "Updates the drop of the value given by enemy-id in the provided atom."
  [atom enemy-id new-value]
  (swap! atom (fn [current] (assoc-in current [enemy-id :drop] new-value))))
  
(defn update-hp-boss
  "Updates the HP value of a boss in the provided atom."
  [atom boss-id new-hp]
  (if-let [validated-new-hp (ensure-number new-hp)]
    (swap! atom (fn [current] (assoc-in current [boss-id :hp] (sanitize-hp-boss validated-new-hp))))))
    
(defn update-byte-phase
  "Updates the value corresponding to the key k for the boss given by boss-id in the specified phase in the provided atom."
  [atom boss-id phase k new-value]
  (if-let [validated-new-value (ensure-number new-value)]
    (swap! atom (fn [current] (assoc-in current [boss-id :blocks-in-data-2 phase k] (fit-into-byte validated-new-value))))))
    
(defn update-bit-phase
  "Updates the specified bit of the value corresponding to the key k for the enemy given by enemy-id in the provided atom."
  [atom boss-id phase k bit new-value]
  (swap! atom (fn [current] (assoc-in current [boss-id :blocks-in-data-2 phase k bit] new-value))))
  
(defn update-shop-item
  "Updates the item in the specified shop and slot to the item given by new-value."
  [atom shop slot new-value]
  (if (= new-value "(none)")
    (swap! atom (fn [current] (assoc-in (assoc-in current [shop slot :item] 255) [shop slot :used] 0)))
    (swap! atom (fn [current] (assoc-in (assoc-in current [shop slot :item] (hexadecimal-to-decimal (item-map-r new-value))) [shop slot :used] 10)))))
    
(defn update-mp
  "Updates the MP cost of a spell in the provided atom."
  [atom spell-id new-value]
    (if-let [validated-new-value (ensure-number new-value)]
      (swap! atom (fn [current] (assoc-in current [spell-id :mp-cost] ((partial fit-into-range 31) validated-new-value))))))
    
(defn update-experience
  "Updates the experience points required for reaching level i."
  [atom i new-value]
  (let [l (fit-into-range 1048575 new-value)
        low (rem l 256)
        mid (quot (rem l 65536) 256)
        high (quot l 65536)]
    (swap! atom (fn [current] (assoc current i (list high mid low))))))

;-------------------------------------------------------------------------------
; PREPARING DATA FOR ROM INSERTION
;-------------------------------------------------------------------------------

(defn reassemble-data
  [f ids]
  (apply concat (map f ids)))

; Spell data

(defn reassemble-spell-data-a
  [spell-data id]
  (let [fs [:id :b01 :b02 :b03 :b04 :b05 :b06 :b07 :b08 #(+ (% :mp-cost) 64) :b10 :b11 :b12 :b13 :b14 :b15]]
    (map #(% (spell-data id)) fs)))
  
(defn reassemble-spell-data
  [spell-data]
  (reassemble-data (partial reassemble-spell-data-a spell-data) spell-ids))

; Item data

(defn reassemble-item-data-a
  [item-data id]
  (let [fs [:b00 :b01 :b02 :b03 :b04 :b05 :b06 :b07 :b08 :b09 :b10 (comp convert-bit-map-to-byte :resistances) :b12 :damage-defense :price-low :price-high]]
    (map #(% (item-data id)) fs)))
    
(defn reassemble-item-data
  [item-data]
  (reassemble-data (partial reassemble-item-data-a item-data) item-ids))
  
; Shop data

(defn reassemble-shop-data-a
  [shop-data id]
  (let [fs [:id1 :id2 #(get-in % [0 :item]) #(get-in % [0 :used]) #(get-in % [1 :item]) #(get-in % [1 :used]) #(get-in % [2 :item]) #(get-in % [2 :used]) #(get-in % [3 :item]) #(get-in % [3 :used]) #(get-in % [4 :item]) #(get-in % [4 :used]) #(get-in % [5 :item]) #(get-in % [5 :used]) #(get-in % [6 :item]) #(get-in % [6 :used])]]
    (map #(% (shop-data id)) fs)))
    
(defn reassemble-shop-data
  [shop-data]
  (reassemble-data (partial reassemble-shop-data-a shop-data) shop-ids))
  
; Enemy data

(defn reassemble-enemy-data-a
  [enemy-data id]
  (let [fs [:speed #(quot (% :hp) 4) :b02 :b03 (comp convert-bit-map-to-byte :immunities) :b05 :damage :defense :b08 :attack :b10 (comp convert-bit-map-to-byte :status-effects) :exp :gold]]
    (map #(% (enemy-data id)) fs)))

(defn reassemble-enemy-data
  [enemy-data]
  (reassemble-data (partial reassemble-enemy-data-a enemy-data) enemy-ids))
  
; Enemy drop data

(defn reassemble-enemy-drop-data-a
  [enemy-drop-data id]
  (let [drop-data (enemy-drop-data id)]
    (into (drop-data :rem) (reverse (drop-data :drop)))))
    
(defn reassemble-enemy-drop-data
  [enemy-drop-data]
  (reassemble-data (partial reassemble-enemy-drop-data-a enemy-drop-data) enemy-ids))

; Boss data 1

(defn reassemble-boss-data-1-a
  [boss-data id]
  (let [fs [:speed #(quot (% :hp) 16) :exp :gold :b04 :attack :b06 :b07 :b08 :b09 :b10 :b11 :b12 :b13 :b14 :b15 :pointer-low :pointer-high :b18 :b19 :b20 :b21 :b22 :b23]]
    (map #(% (boss-data id)) fs)))
    
(defn reassemble-boss-data-1
  [boss-data]
  (reassemble-data (partial reassemble-boss-data-1-a boss-data) boss-ids))

; Boss data 2 (phase data)

;these function are not called, they were used to get the order in boss data 2, stored in boss-order-in-data-2

(defn <addr
  [[low1 high1] [low2 high2]]
  (if (= high1 high2)
    (< low1 low2)
    (< high1 high2)))
    
(defn get-boss-with-pointer
  [pointer]
  (loop [id 0]
    (let [data (@editor-values-boss (decimal-to-hexadecimal id))
          pointer-boss [(data :pointer-low) (data :pointer-high)]]
      (if (= pointer pointer-boss)
        (data :id)
        (recur (inc id))))))

(defn get-boss-order-in-data-2
  []
  ;distinct is needed since Dragon and Red Dragon share the same phase data
  (map get-boss-with-pointer (distinct (sort <addr (map (fn [id] (let [data (@editor-values-boss id)] [(data :pointer-low) (data :pointer-high)] )) boss-ids)))))
  
;actual functions

(defn reassemble-boss-data-2-a
  [boss-data id]
  (let [data (boss-data id)
        number-blocks (data :number-blocks-in-data-2)
        fs [:b00 (comp convert-bit-map-to-byte :immunities) :b02 :damage :defense :b05 (comp convert-bit-map-to-byte :status-effects) :b07]
        get-ith-block (fn [i] (map #(% (get-in data [:blocks-in-data-2 i])) fs))]
   (reassemble-data get-ith-block (range number-blocks))))

(defn reassemble-boss-data-2
  [boss-data]
  (reassemble-data (partial reassemble-boss-data-2-a boss-data) boss-order-in-data-2))

; Experience data

(defn reassemble-experience-data
  [experience-data]
  (reassemble-data #(experience-data %) levels))
  
; Attack data

(defn reassemble-attack-data-a
  [attack-data id]
  (let [fs [:b00 :speed :b02 :b03 :damage :b05 :b06 :b07 :b08 :b09 :b10 :b11 :b12 :b13 :b14 :b15]]
    (map #(% (attack-data id)) fs)))
    
(defn reassemble-attack-data
  [attack-data]
  (reassemble-data (partial reassemble-attack-data-a attack-data) (butlast attack-ids)))
  
; putting everything together

(defn reassemble-rom
  ([rom-parts spell-data item-data shop-data enemy-data enemy-drop-data boss-data experience-data attack-data]
    (concat (rom-parts "rom-part-1") (reassemble-spell-data spell-data) (reassemble-item-data item-data) (rom-parts "rom-part-2") (reassemble-shop-data shop-data) (rom-parts "rom-part-3") (reassemble-enemy-data enemy-data) (rom-parts "rom-part-4") (reassemble-enemy-drop-data enemy-drop-data) (rom-parts "rom-part-5") (reassemble-boss-data-1 boss-data) (reassemble-boss-data-2 boss-data) (rom-parts "rom-part-6") (reassemble-experience-data experience-data) (rom-parts "rom-part-7") (reassemble-attack-data attack-data) (rom-parts "rom-part-8")))
  ([rom-parts]
    (reassemble-rom rom-parts @editor-values-spell @editor-values-item @editor-values-shop @editor-values-enemy @editor-values-enemy-drop @editor-values-boss @editor-values-experience @editor-values-attack)))
  
; testing the functions
  
(defn test-reassembly
  "Sanity check for the reassembly functions, test with unchanged atoms."
  []
  [(= (reassemble-spell-data @editor-values-spell) (map make-pos (@rom-parts "spell-data")))
   (= (reassemble-item-data @editor-values-item) (map make-pos (@rom-parts "item-data")))
   (= (reassemble-shop-data @editor-values-shop) (map make-pos (@rom-parts "shop-data")))
   (= (reassemble-enemy-data @editor-values-enemy) (map make-pos (@rom-parts "enemy-data")))
   (= (reassemble-enemy-drop-data @editor-values-enemy-drop) (map make-pos (@rom-parts "enemy-drop-data")))
   (= (reassemble-boss-data-1 @editor-values-boss) (map make-pos (@rom-parts "boss-data")))
   (= (reassemble-boss-data-2 @editor-values-boss) (map make-pos (@rom-parts "boss-data-2")))
   (= (reassemble-experience-data @editor-values-experience) (map make-pos (@rom-parts "experience-data")))
   (= (reassemble-attack-data @editor-values-attack) (map make-pos (@rom-parts "attack-data")))])
    
;-------------------------------------------------------------------------------
; GUI
;-------------------------------------------------------------------------------

; i have no idea about guis

(def selected-category (atom "Enemies"))
(def selected-enemy (atom "Rabite"))
(def selected-boss (atom "Vampire"))
(def selected-attack (atom "Throwing Knife"))
(def selected-spell (atom "Cure"))
(def selected-item (atom "Cure"))
(def selected-shop (atom "0"))

(def object-selections-map
  {'Enemies (map enemy-names-map used-enemy-ids)
   'Bosses (map boss-names-map boss-ids)
   'Attacks (map attack-names-map (butlast attack-ids))
   'Spells (map spell-map spell-ids)
   'Items (map #(get-in item-map [% :name]) used-item-ids)
   'Shops shop-ids
   'Experience '(---)})

;-------------------------------------------------------------------------------

(native!)

(def poto-icon (clojure.java.io/resource "poto.png"))

(declare open-file)
(defn open-file-a
  [e]
  (open-file))

(declare save-file)
(defn save-file-a
  [e]
  (save-file))

(declare save-to)
(defn save-to-a
  [e]
  (save-to)) 

(declare reset-all)
(defn reset-a
  [e]
  (reset-all))
  
(declare exit-program)
(defn exit-a
  [e]
  (exit-program))

(declare show-help)
(defn show-help-a
  [e]
  (show-help))

(declare show-about)
(defn show-about-a
  [e]
  (show-about))

(defn make-frame
  []
  (let [a-open (action :handler open-file-a :name "Open" :tip "Open a ROM file." :key "menu O")
        a-save (action :handler save-file-a :name "Save" :tip "Save the current file." :key "menu S")
        a-save-to (action :handler save-to-a :name "Save to" :tip "Save the current values to a different file.")
        a-reset (action :handler reset-a :name "Reset" :tip "Undo all changes.")
        a-exit (action :handler exit-a :name "Exit" :tip "Exit the editor.")
        a-info (action :handler show-help-a :name "Info")
        a-about (action :handler show-about-a :name "About")]
  (frame
    :title "Mystic Quest Editor"
    :width 640 :height 512
    :icon poto-icon
    :on-close :exit
    :menubar (menubar :items [(menu :text "File" :items [a-open a-save a-save-to a-reset a-exit])
                              (menu :text "Help" :items [a-info a-about])]))))
                              
(def whole-frame (make-frame))

(defn show-frame
  []
  (show! whole-frame))

;-------------------------------------------------------------------------------

(defn confirm-dialog
  [content f]
  (-> (dialog
    :type :warning
    :success-fn (fn [p] (f))
    :option-type :ok-cancel
    :content content
  ) pack! show!))

;-------------------------------------------------------------------------------

;ENEMIES

(def selection-box-enemy (selection! (listbox :model (object-selections-map 'Enemies)) @selected-enemy))

(defn make-radio-panel
  [partial-update-function preselection-function labels label-map]
  (into [] (map (fn [l] (radio :listen [:selection (fn [e] (partial-update-function l (selection e)))] :selected? (preselection-function l) :text (label-map l) ) ) labels)))

(declare reset-enemy)
(declare subframe-attack)
(declare subframe-content-attack)
(defn subframe-content-enemy
  [enemy]
  (let [enemy-id (enemy-names-map-r enemy)
        enemy-drop-id (get-enemy-drop-id enemy-id)
        enemy-data (@editor-values-enemy enemy-id)
        enemy-hp (enemy-data :hp)
        enemy-speed (enemy-data :speed)
        enemy-damage (enemy-data :damage)
        enemy-defense (enemy-data :defense)
        enemy-attack (decimal-to-hexadecimal (enemy-data :attack))
        enemy-immunities (enemy-data :immunities)
        enemy-status-effects (enemy-data :status-effects)
        enemy-experience (enemy-data :exp)
        enemy-gold (enemy-data :gold)
        enemy-drop (enemy-data :drop)]
    [[(str enemy " (ID " enemy-id ")") "wrap"]
     ["HP                                         "]
     [(spinner :listen [:selection (fn [e] (update-hp editor-values-enemy enemy-id (selection e)))] :size [80 :by 20] :model (spinner-model (long enemy-hp) :from 4 :to 1020 :by 4)) "wrap"]
     ["Speed"]
     [(spinner :listen [:selection (fn [e] (update-byte editor-values-enemy enemy-id :speed (selection e)))] :size [80 :by 20] :model (spinner-model (long enemy-speed) :from 0 :to 255)) "wrap"]
     ["Damage"]
     [(spinner :listen [:selection (fn [e] (update-byte editor-values-enemy enemy-id :damage (selection e)))] :size [80 :by 20] :model (spinner-model (long enemy-damage) :from 0 :to 255)) "wrap"]
     ["Defense"]
     [(spinner :listen [:selection (fn [e] (update-byte editor-values-enemy enemy-id :defense (selection e)))] :size [80 :by 20] :model (spinner-model (long enemy-defense) :from 0 :to 255)) "wrap"]
     ["Attack"]
     [(selection! (combobox :listen [:selection (fn [e] (do (update-byte editor-values-enemy enemy-id :attack (hexadecimal-to-decimal (attack-names-map-r (selection e)))) (config! subframe-attack :items (subframe-content-attack @selected-attack))))] :size [200 :by 20] :model (map #(attack-names-map %) attack-ids)) (attack-names-map enemy-attack)) "wrap"]
     ["Immunities" "wrap"]
     [(horizontal-panel :items (make-radio-panel (partial update-bit editor-values-enemy enemy-id :immunities) enemy-immunities (range 1 8) (zipmap (range 1 8) (map str (range 1 8))))) "wrap"]
     ["Status Effects" "wrap"]
     [(horizontal-panel :items (make-radio-panel (partial update-bit editor-values-enemy enemy-id :status-effects) enemy-status-effects [7 6 5 4] (zipmap [7 6 5 4] ["Poison" "Dark" "Stone" "Moogle"]))) "wrap"]
     ["Experience"]
     [(spinner :listen [:selection (fn [e] (update-byte editor-values-enemy enemy-id :exp (selection e)))] :size [80 :by 20] :model (spinner-model (long enemy-experience) :from 0 :to 255)) "wrap"]
     ["Gold"]
     [(spinner :listen [:selection (fn [e] (update-byte editor-values-enemy enemy-id :gold (selection e)))] :size [80 :by 20] :model (spinner-model (long enemy-gold) :from 0 :to 255)) "wrap"]
     ["Items dropped"]
     [(selection! (combobox :listen [:selection (fn [e] (do (update-drop editor-values-enemy enemy-id (drop-map-jumps-r (selection e))) (update-drop editor-values-enemy-drop enemy-drop-id (drop-map-jumps-r (selection e)))))] :size [200 :by 20] :model (vals drop-map-jumps)) (drop-map-jumps enemy-drop)) "wrap"]
     [(button :listen [:action (fn [e] (reset-enemy enemy enemy-id))] :text "Reset this entry")]]))

(defn draw-subframe-enemy
  [enemy]
  (mig-panel :items (subframe-content-enemy enemy)))

(def subframe-enemy (mig-panel :items []))

(listen selection-box-enemy :selection
  (fn [e]
    (when-let [s (selection e)]
      (reset! selected-enemy s)
      (config! subframe-enemy :items (subframe-content-enemy s)))))
      
(defn reset-enemy
  [enemy id]
  (swap! editor-values-enemy (fn [current] (assoc current id (@rom-values-enemy id))))
  (swap! editor-values-enemy-drop (fn [current] (assoc current (get-enemy-drop-id id) (@rom-values-enemy-drop id))))
  (config! subframe-enemy :items (subframe-content-enemy enemy)))


(defn enemies
  []
  (reset! selected-category "Enemies")
  (left-right-split (scrollable selection-box-enemy) (scrollable subframe-enemy) :divider-location 1/4))
    
;-------------------------------------------------------------------------------

;BOSSES

(def selection-box-boss (selection! (listbox :model (object-selections-map 'Bosses)) @selected-boss))

(defn subframe-content-boss-main
  [boss]
  (let [boss-id (boss-names-map-r boss)
        boss-data (@editor-values-boss boss-id)
        boss-hp (boss-data :hp)
        boss-speed (boss-data :speed)
        boss-attack (decimal-to-hexadecimal (boss-data :attack))
        boss-experience (boss-data :exp)
        boss-gold (boss-data :gold)]
    [[(str boss " (ID " boss-id ")") "wrap"]
     ["HP                                         "]
     [(spinner :listen [:selection (fn [e] (update-hp-boss editor-values-boss boss-id (selection e)))] :size [80 :by 20] :model (spinner-model (long boss-hp) :from 16 :to 4080 :by 16)) "wrap"]
     ["Speed"]
     [(spinner :listen [:selection (fn [e] (update-byte editor-values-boss boss-id :speed (selection e)))] :size [80 :by 20] :model (spinner-model (long boss-speed) :from 0 :to 255)) "wrap"]
     ["Attack"]
     [(selection! (combobox :listen [:selection (fn [e] (do (update-byte editor-values-boss boss-id :attack (hexadecimal-to-decimal (attack-names-map-r (selection e)))) (config! subframe-attack :items (subframe-content-attack @selected-attack))))] :size [200 :by 20] :model (map #(attack-names-map %) attack-ids)) (attack-names-map boss-attack)) "wrap"]
     ["Experience"]
     [(spinner :listen [:selection (fn [e] (update-byte editor-values-boss boss-id :exp (selection e)))] :size [80 :by 20] :model (spinner-model (long boss-experience) :from 0 :to 255)) "wrap"]
     ["Gold"]
     [(spinner :listen [:selection (fn [e] (update-byte editor-values-boss boss-id :gold (selection e)))] :size [80 :by 20] :model (spinner-model (long boss-gold) :from 0 :to 255)) "wrap"]]))
     
(defn subframe-content-boss-phase
  [boss i]
  (let [boss-id (boss-names-map-r boss)
        boss-data (@editor-values-boss boss-id)
        boss-phase-data (get-in boss-data [:blocks-in-data-2 i])
        boss-damage (boss-phase-data :damage)
        boss-defense (boss-phase-data :defense)
        boss-immunities (boss-phase-data :immunities)
        boss-status-effects (boss-phase-data :status-effects)]
    [["Damage"]
     [(spinner :listen [:selection (fn [e] (update-byte-phase editor-values-boss boss-id i :damage (selection e)))] :size [80 :by 20] :model (spinner-model (long boss-damage) :from 0 :to 255)) "wrap"]
     ["Defense"]
     [(spinner :listen [:selection (fn [e] (update-byte-phase editor-values-boss boss-id i :defense (selection e)))] :size [80 :by 20] :model (spinner-model (long boss-defense) :from 0 :to 255)) "wrap"]
     ["Immunities" "wrap"]
     [(horizontal-panel :items (make-radio-panel (partial update-bit-phase editor-values-boss boss-id i :immunities) boss-immunities (range 1 8) (zipmap (range 1 8) (map str (range 1 8))))) "wrap"]
     ["Status Effects" "wrap"]
     [(horizontal-panel :items (make-radio-panel (partial update-bit-phase editor-values-boss boss-id i :status-effects) boss-status-effects [7 6 5 4] (zipmap [7 6 5 4] ["Poison" "Dark" "Stone" "Moogle"]))) "wrap"]]))

(declare reset-boss)
(defn subframe-content-boss
  [boss]
  (let [boss-id (boss-names-map-r boss)
        boss-data (@editor-values-boss boss-id)
        number-of-phases (boss-data :number-blocks-in-data-2)
        content-main (subframe-content-boss-main boss)
        content-main-and-phase (reduce (fn [xs x] (conj xs (mig-panel :border (str "Phase " (first x)) :items (second x)))) [(mig-panel :items content-main)] (map (fn [i] [i (subframe-content-boss-phase boss i)]) (range number-of-phases)))
        ]
        (if (= boss "Red Dragon")
          [(mig-panel :items content-main)
           (mig-panel :items [["Phase data is shared with Dragon."]])]
        (conj content-main-and-phase (mig-panel :items [[(button :listen [:action (fn [e] (reset-boss boss boss-id))] :text "Reset this entry")]])))))
    
(defn draw-subframe-boss
  [boss]
  (vertical-panel :items (subframe-content-boss boss))  )

(def subframe-boss (vertical-panel :items []))

(listen selection-box-boss :selection
  (fn [e]
    (when-let [s (selection e)]
      (reset! selected-boss s)
      (config! subframe-boss :items (subframe-content-boss s)))))
      
(defn reset-boss
  [boss id]
  (swap! editor-values-boss (fn [current] (assoc current id (@rom-values-boss id))))
  (config! subframe-boss :items (subframe-content-boss boss)))

(defn bosses
  []
  (reset! selected-category "Bosses")
  (left-right-split (scrollable selection-box-boss) (scrollable subframe-boss) :divider-location 1/4))

;-------------------------------------------------------------------------------

;ATTACKS

(defn get-users-of-attack
  [id]
  (concat (filter (fn [data] (= id (decimal-to-hexadecimal (:attack data)))) (map (fn [id] (@editor-values-enemy id)) used-enemy-ids)) (filter (fn [data] (= id (decimal-to-hexadecimal (:attack data)))) (map (fn [id] (@editor-values-boss id)) boss-ids))))

(def selection-box-attack (selection! (listbox :model (object-selections-map 'Attacks)) @selected-attack))

(declare reset-attack)
(defn subframe-content-attack
  [attack]
  (let [attack-id (attack-names-map-r attack)
        attack-data (@editor-values-attack attack-id)
        speed (attack-data :speed)
        damage (attack-data :damage)
        users (let [us (map :name (get-users-of-attack attack-id))]
                (if (empty? us)
                  ["(none)"]
                  us))]
    [[(str attack " (ID " attack-id ")") "wrap"]
     ["Used by"]
     [(str "<html>" (apply str (interpose "<br>" users)) "</html>") "wrap"]
     ["Speed                                   "]
     [(spinner :listen [:selection (fn [e] (update-byte editor-values-attack attack-id :speed (selection e)))] :size [80 :by 20] :model (spinner-model (long speed) :from 0 :to 255)) "wrap"]
     ["Damage"]
     [(spinner :listen [:selection (fn [e] (update-byte editor-values-attack attack-id :damage (selection e)))] :size [80 :by 20] :model (spinner-model (long damage) :from 0 :to 255)) "wrap"]
     [(button :listen [:action (fn [e] (reset-attack attack attack-id))] :text "Reset this entry")]]))
        
(defn draw-subframe-attack
  [attack]
  (mig-panel :items (subframe-content-attack attack)))

(def subframe-attack (mig-panel :items []))

(listen selection-box-attack :selection
  (fn [e]
    (when-let [s (selection e)]
      (reset! selected-attack s)
      (config! subframe-attack :items (subframe-content-attack s)))))
      
(defn reset-attack
  [attack id]
  (swap! editor-values-attack (fn [current] (assoc current id (@rom-values-attack id))))
  (config! subframe-attack :items (subframe-content-attack attack)))

(defn attacks
  []
  (reset! selected-category "Attacks")
  (left-right-split (scrollable selection-box-attack) (scrollable subframe-attack) :divider-location 1/4))

;-------------------------------------------------------------------------------

;SPELLS

(def selection-box-spell (selection! (listbox :model (object-selections-map 'Spells)) @selected-spell))

(declare reset-spell)
(defn subframe-content-spell
  [spell]
  (let [spell-id (spell-map-r spell)
        spell-data (@editor-values-spell spell-id)
        mp-cost (spell-data :mp-cost)]
    [[(str spell " (ID " spell-id ")") "wrap"]
     ["MP Cost                            "]
     [(spinner :listen [:selection (fn [e] (update-mp editor-values-spell spell-id (selection e)))] :size [80 :by 20] :model (spinner-model (long mp-cost) :from 0 :to 31)) "wrap"]]))
        
(defn draw-subframe-spell
  [spell]
  (mig-panel :items (subframe-content-spell spell)))

(def subframe-spell (mig-panel :items []))

(listen selection-box-spell :selection
  (fn [e]
    (when-let [s (selection e)]
      (reset! selected-spell s)
      (config! subframe-spell :items (subframe-content-spell s)))))
      
(defn reset-spell
  [spell id]
  (swap! editor-values-spell (fn [current] (assoc current id (@rom-values-spell id))))
  (config! subframe-spell :items (subframe-content-spell spell)))

(defn spells
  []
  (reset! selected-category "Spells")
  (left-right-split (scrollable selection-box-spell) (scrollable subframe-spell) :divider-location 1/4))

;-------------------------------------------------------------------------------

;ITEMS

(def selection-box-item (selection! (listbox :model (object-selections-map 'Items)) @selected-item))

(declare reset-item)
(defn subframe-content-item
  [item]
  (let [item-id (item-map-r item)
        item-data (@editor-values-item item-id)
        item-type (get-in item-map [item-id :type])
        price (+ (* 256 (item-data :price-high)) (item-data :price-low))
        damage-defense (item-data :damage-defense)
        resistances (item-data :resistances)
        content-price [[(str item " (ID " item-id ")") "wrap"]
                   ["Price                                           "]
                   [(spinner :listen [:selection (fn [e] ((update-byte editor-values-item item-id :price-low (rem (selection e) 256))
                                                                     (update-byte editor-values-item item-id :price-high (quot (selection e) 256))))] 
                   :size [80 :by 20] :model (spinner-model (long price) :from 0 :to 65535)) "wrap"]]
        content-damage-defense-label (case item-type
                                       :weapon "Damage"
                                               "Defense")
        content-damage-defense (if (contains? #{:weapon :armor :helmet} item-type)
                                 [[content-damage-defense-label]
                                 [(spinner :listen [:selection (fn [e] (update-byte editor-values-item item-id :damage-defense (selection e)))] :size [80 :by 20] :model (spinner-model (long damage-defense) :from 0 :to 255)) "wrap"]]
                                 [])
        content-resistances (if (= item-type :shield)
                              [["Resistances"]
                              [(horizontal-panel :items (make-radio-panel (partial update-bit editor-values-item item-id :resistances) resistances [7 6 5 4 3 2 1] (zipmap [7 6 5 4 3 2 1] ["A" "B" "C" "D" "E" "F" "G"]))) "wrap"]]
                              [])]
    (reduce into content-price [content-damage-defense content-resistances [[(button :listen [:action (fn [e] (reset-item item item-id))] :text "Reset this entry")]]])))
        
(defn draw-subframe-item
  [item]
  (mig-panel :items (subframe-content-item item)))

(def subframe-item (mig-panel :items []))

(listen selection-box-item :selection
  (fn [e]
    (when-let [s (selection e)]
      (reset! selected-item s)
      (config! subframe-item :items (subframe-content-item s)))))
      
(defn reset-item
  [item id]
  (swap! editor-values-item (fn [current] (assoc current id (@rom-values-item id))))
  (config! subframe-item :items (subframe-content-item item)))

(defn items
  []
  (reset! selected-category "Items")
  (left-right-split (scrollable selection-box-item) (scrollable subframe-item) :divider-location 1/4))

;-------------------------------------------------------------------------------

;SHOPS

(def selection-box-shop (selection! (listbox :model (object-selections-map 'Shops)) @selected-shop))

(declare reset-shop)
(defn subframe-content-shop
  [shop]
  (let [shop-data (@editor-values-shop shop)
        sorted-items (conj (map (fn [id] (get-in used-item-map [id :name])) used-item-ids) "(none)")
        locations  (shop-map shop)
        content-main [[(str "Shop " shop) "wrap"]
                      ["Locations              "]
                      [(str "<html>" (apply str (interpose "<br>" locations)) "</html>") "wrap"]]
        get-item (fn [n] (if (= (get-in shop-data [n :used]) 10)
                           (get-in used-item-map [(decimal-to-hexadecimal (get-in shop-data [n :item])) :name])
                           "(none)"))
        item-slot-shop (fn [n] [[(str "Item " n)]
                                [(selection! (combobox :listen [:selection (fn [e] (update-shop-item editor-values-shop shop n (selection e)))] :size [200 :by 20] :model sorted-items) (get-item n)) "wrap"]])
        content-reset [[(button :listen [:action (fn [e] (reset-shop shop))] :text "Reset this entry")]]]
    (concat content-main (reduce concat (map (fn [n] (item-slot-shop n)) (range 7))) content-reset)))
        
(defn draw-subframe-shop
  [shop]
  (mig-panel :items (subframe-content-shop shop)))

(def subframe-shop (mig-panel :items []))

(listen selection-box-shop :selection
  (fn [e]
    (when-let [s (selection e)]
      (reset! selected-shop s)
      (config! subframe-shop :items (subframe-content-shop s)))))
      
(defn reset-shop
  [shop]
  (swap! editor-values-shop (fn [current] (assoc current shop (@rom-values-shop shop))))
  (config! subframe-shop :items (subframe-content-shop shop)))

(defn shops
  []
  (reset! selected-category "Shops")
  (left-right-split (scrollable selection-box-shop) (scrollable subframe-shop) :divider-location 1/4))

;-------------------------------------------------------------------------------
 
;EXPERIENCE

(defn row-for-level
  [i]
  (let [three-tuple (@editor-values-experience i)
        three-tuple-to-number (fn [[a b c]] (+ (* 65536 a) (* 256 b) c)) ]
  [[(str "Level " i)]
  [(spinner :listen [:selection (fn [e] (update-experience editor-values-experience i (selection e)))] :size [80 :by 20] :model (spinner-model (long (three-tuple-to-number three-tuple)) :from 0 :to 1048575)) "wrap"]]))

(declare reset-experience)
(defn subframe-experience-content
  []
  (let [content-headline [["Experience points required for level-ups.   " "wrap"]]
        content-reset [[(button :listen [:action (fn [e] (reset-experience))] :text "Reset all levels")]]]
    (concat content-headline (reduce concat (map (fn [n] (row-for-level n)) (range 2 102))) content-reset)))
        
(defn draw-subframe-experience
  []
  (mig-panel :items (subframe-experience-content)))

(def subframe-experience (mig-panel :items []))
      
(defn reset-experience
  []
  (reset! editor-values-experience @rom-values-experience)
  (config! subframe-experience :items (subframe-experience-content)))

(defn experience
  []
  (reset! selected-category "Experience")
  (scrollable subframe-experience))

;-------------------------------------------------------------------------------

(def tabs (tabbed-panel :tabs [{:title "Enemies" :content (enemies)}
                               {:title "Bosses" :content (bosses)}
                               {:title "Attacks" :content (attacks)}
                               {:title "Spells" :content (spells)}
                               {:title "Items" :content (items)}
                               {:title "Shops" :content (shops)}
                               {:title "Experience" :content (experience)}]))

;-------------------------------------------------------------------------------

(defn slurp-bytes
  "Reads a source byte-wise."
  [x]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (clojure.java.io/copy (clojure.java.io/input-stream x) out)
    (.toByteArray out)))

(defn update-rom-atoms
  "Updates ROM atoms after opening a new file."
  []
  (reset! rom-parts (zipmap rom-part-labels (multi-split-at-hex (slurp-bytes @current-file) (splits @rom-version))))
  (reset! rom-values-enemy (zipmap enemy-ids (map (partial extract-enemy-data-from-rom (@rom-parts "enemy-data") (@rom-parts "enemy-drop-data")) enemy-ids)))
  (reset! rom-values-enemy-drop (zipmap enemy-ids (map (partial extract-enemy-drop-data-from-rom (@rom-parts "enemy-drop-data")) enemy-ids)))
  (reset! rom-values-boss (zipmap boss-ids (map (partial extract-boss-data-from-rom (@rom-parts "boss-data") (@rom-parts "boss-data-2")) boss-ids)))
  (reset! rom-values-attack (zipmap (butlast attack-ids) (map (partial extract-attack-data-from-rom (@rom-parts "attack-data")) (butlast attack-ids))))
  (reset! rom-values-spell (zipmap spell-ids (map (partial extract-spell-data-from-rom (@rom-parts "spell-data")) spell-ids)))
  (reset! rom-values-item (zipmap item-ids (map (partial extract-item-data-from-rom (@rom-parts "item-data")) item-ids)))
  (reset! rom-values-shop (zipmap shop-ids (map (partial extract-shop-data-from-rom (@rom-parts "shop-data")) shop-ids)))
  (reset! rom-values-experience (zipmap levels (map (partial extract-exp-data-from-rom (@rom-parts "experience-data")) levels))))
  
(defn update-editor-atoms
  "Updates/resets editor atoms."
  []
  (reset! editor-values-enemy @rom-values-enemy)
  (reset! editor-values-enemy-drop @rom-values-enemy-drop)
  (reset! editor-values-boss @rom-values-boss)
  (reset! editor-values-attack @rom-values-attack)
  (reset! editor-values-spell @rom-values-spell)
  (reset! editor-values-item @rom-values-item)
  (reset! editor-values-shop @rom-values-shop)
  (reset! editor-values-experience @rom-values-experience))
  
(defn update-subframes
  "Updates subframes after opening a file."
  []
  (config! subframe-enemy :items (subframe-content-enemy @selected-enemy))
  (config! subframe-boss :items (subframe-content-boss @selected-boss))
  (config! subframe-attack :items (subframe-content-attack @selected-attack))
  (config! subframe-spell :items (subframe-content-spell @selected-spell))
  (config! subframe-item :items (subframe-content-item @selected-item))
  (config! subframe-shop :items (subframe-content-shop @selected-shop))
  (config! subframe-experience :items (subframe-experience-content)))

(declare open-file-updates)
(defn make-rom-version-dialog
  []
  (dialog
    :title "Select version"
    :content (mig-panel :items [["Select the ROM version" "wrap"] [(selection! (combobox :id :version :size [240 :by 20] :model rom-versions) (rom-versions-map-r @rom-version))]])
    :success-fn (fn [e] (do (reset! rom-version (rom-versions-map (selection (select (to-frame e) [:#version])))) (open-file-updates)))))
    
(defn show-rom-version-dialog
  []
  (-> (make-rom-version-dialog) pack! show!))

(declare update-rom-atoms-from-editor-atoms)
(defn open-file-updates
  []
  (update-rom-atoms)
  (update-editor-atoms)
  (update-subframes)
  (config! whole-frame :content tabs))
    
(defn open-file-aux
  []
  (when-let [file (choose-file)]
    (reset! current-file file)
    (reset! rom-version (get-rom-version (slurp-bytes file)))
    (show-rom-version-dialog)))
  
(defn open-file
  []
  (if (= @current-file nil)
    (open-file-aux)
    (confirm-dialog "All unsaved changes will be lost. Open a new file anyway?" (fn [] (open-file-aux)))))

;-------------------------------------------------------------------------------

(defn spit-bytes
  "Writes byte-wise to a file."
  [src file]
  (let [ba (into-array Byte/TYPE (map byte (map make-neg src)))]
  (with-open [w (clojure.java.io/output-stream file)]
    (.write w ba))))

(defn update-rom-atoms-from-editor-atoms
  []
  (reset! rom-values-enemy @editor-values-enemy)
  (reset! rom-values-enemy-drop @editor-values-enemy-drop)
  (reset! rom-values-boss @editor-values-boss)
  (reset! rom-values-attack @editor-values-attack)
  (reset! rom-values-spell @editor-values-spell)
  (reset! rom-values-item @editor-values-item)
  (reset! rom-values-shop @editor-values-shop)
  (reset! rom-values-experience @editor-values-experience))

(defn save-file-aux
  []
  (let [to-save (reassemble-rom @rom-parts)]
    (update-rom-atoms-from-editor-atoms)
    (spit-bytes to-save @current-file)
    (alert "File was saved successfully.")))

(defn save-file
  []
  (when (not= @current-file nil)
    (save-file-aux)))

;-------------------------------------------------------------------------------

;As it turns out the way I wrote the editor made transferring changes to different versions of the ROM not easy to implement, so here are the terrible functions that accomplish it

(defn extract-enemy-data-from-rom-and-atom
  "Returns a map with data of the enemy with the given ID. Editable data is taken from the enemy atom, anything else from the ROM to which the changes will be saved."
  [enemy-data enemy-drop-data enemy-id]
    (let [address-data    (*hex enemy-id "E")
          enemy-name      (enemy-names-map enemy-id)
          data            (get-n-elements-at enemy-data address-data 14)]
    {:id enemy-id
     :name enemy-name
     :speed (get-in @editor-values-enemy [enemy-id :speed])
     :hp (get-in @editor-values-enemy [enemy-id :hp])
     :b02 (nth data 2)
     :b03 (nth data 3)
     :immunities (get-in @editor-values-enemy [enemy-id :immunities])
     :b05 (nth data 5)
     :damage (get-in @editor-values-enemy [enemy-id :damage])
     :defense (get-in @editor-values-enemy [enemy-id :defense])
     :b08 (nth data 8)
     :attack (get-in @editor-values-enemy [enemy-id :attack])
     :b10 (nth data 10)
     :status-effects (get-in @editor-values-enemy [enemy-id :status-effects])
     :exp (get-in @editor-values-enemy [enemy-id :exp])
     :gold (get-in @editor-values-enemy [enemy-id :gold])
     :drop (get-in @editor-values-enemy [enemy-id :drop])}))
     
(defn extract-enemy-drop-data-from-rom-and-atom
  "Analogous to the enemy function."
  [enemy-drop-data enemy-id]
  (let [address-drop   (*hex enemy-id "18")
        drop-rem       (get-n-elements-at enemy-drop-data (+hex address-drop "2") 22)]
    {:drop (get-in @editor-values-enemy-drop [enemy-id :drop])
     :rem drop-rem}))
    
(defn extract-boss-data-from-rom-and-atom
  "Analogous to the enemy function."
  [boss-data boss-data-2 boss-id]
  (let [address-data    (*hex boss-id "18")
        data            (get-n-elements-at boss-data address-data 24)
        address-data-2  (+ (nth data 16) (* 256 (nth data 17)) -18737)
        starts-data-2   '(0 64 120 176 208 272 320 408 464 512 584 640 704 704 776 832 912 1000 1048 1112 1168 1216)
        number-blocks-2 (/ (- (first (drop-while #(<= % address-data-2) starts-data-2)) address-data-2) 8)
        data-2          (get-n-elements-at boss-data-2 (decimal-to-hexadecimal address-data-2) (* 8 number-blocks-2))]
    {:id boss-id
     :name (boss-names-map boss-id)
     :speed (get-in @editor-values-boss [boss-id :speed])
     :hp (get-in@editor-values-boss [boss-id :hp])
     :exp (get-in@editor-values-boss [boss-id :exp])
     :gold (get-in @editor-values-boss [boss-id :gold])
     :b04 (nth data 4)
     :attack (get-in @editor-values-boss [boss-id :attack])
     :b06 (nth data 6)
     :b07 (nth data 7)
     :b08 (nth data 8)
     :b09 (nth data 9)
     :b10 (nth data 10)
     :b11 (nth data 11)
     :b12 (nth data 12)
     :b13 (nth data 13)
     :b14 (nth data 14)
     :b15 (nth data 15)
     :pointer-low (nth data 16)
     :pointer-high (nth data 17)
     :b18 (nth data 18)
     :b19 (nth data 19)
     :b20 (nth data 20)
     :b21 (nth data 21)
     :b22 (nth data 22)
     :b23 (nth data 23)
     :number-blocks-in-data-2 number-blocks-2
     :blocks-in-data-2 (zipmap (range number-blocks-2) (map (fn [i] {
       :b00 (nth data-2 (* 8 i))
       :immunities (get-in @editor-values-boss [boss-id :blocks-in-data-2 i :immunities])
       :b02 (nth data-2 (+ (* 8 i) 2))
       :damage (get-in @editor-values-boss [boss-id :blocks-in-data-2 i :damage])
       :defense (get-in @editor-values-boss [boss-id :blocks-in-data-2 i :defense])
       :b05 (nth data-2 (+ (* 8 i) 5))
       :status-effects (get-in @editor-values-boss [boss-id :blocks-in-data-2 i :status-effects])
       :b07 (nth data-2 (+ (* 8 i) 7))}) (range number-blocks-2)))}))

(defn extract-attack-data-from-rom-and-atom
  "Analogous to the enemy function."
  [attack-data attack-id]
  (let [address-data    (*hex attack-id "10")
        data            (get-n-elements-at attack-data address-data 16)]
    {:id attack-id
     :name (attack-names-map attack-id)
     :b00 (nth data 0)
     :speed (get-in @editor-values-attack [attack-id :speed])
     :b02 (nth data 2)
     :b03 (nth data 3)
     :damage (get-in @editor-values-attack [attack-id :damage])
     :b05 (nth data 5)
     :b06 (nth data 6)
     :b07 (nth data 7)
     :b08 (nth data 8)
     :b09 (nth data 9)
     :b10 (nth data 10)
     :b11 (nth data 11)
     :b12 (nth data 12)
     :b13 (nth data 13)
     :b14 (nth data 14)
     :b15 (nth data 15)}))
     
(defn extract-spell-data-from-rom-and-atom
  "Analogous to the enemy function."
  [spell-data spell-id]
  (let [address-data    (*hex "10" spell-id)
        data            (get-n-elements-at spell-data address-data 16)]
    {:id (nth data 0)
     :name (spell-map spell-id)
     :b01 (nth data 1)
     :b02 (nth data 2)
     :b03 (nth data 3)
     :b04 (nth data 4)
     :b05 (nth data 5)
     :b06 (nth data 6)
     :b07 (nth data 7)
     :b08 (nth data 8)
     :mp-cost (get-in @editor-values-spell [spell-id :mp-cost])
     :b10 (nth data 10)
     :b11 (nth data 11)
     :b12 (nth data 12)
     :b13 (nth data 13)
     :b14 (nth data 14)
     :b15 (nth data 15)}))
     
(defn extract-item-data-from-rom-and-atom
  "Analogous to the enemy function."
  [item-data item-id]
  (let [address-data    (*hex "10" item-id)
        data            (get-n-elements-at item-data address-data 16)]
    {:id item-id
     :name (get-in item-map [item-id :name])
     :type (get-in item-map [item-id :type])
     :b00 (first data)
     :b01 (second data)
     :b02 (nth data 2)
     :b03 (nth data 3)
     :b04 (nth data 4)
     :b05 (nth data 5)
     :b06 (nth data 6)
     :b07 (nth data 7)
     :b08 (nth data 8)
     :b09 (nth data 9)
     :b10 (nth data 10)
     :resistances (get-in @editor-values-item [item-id :resistances])
     :b12 (nth data 12)
     :damage-defense (get-in @editor-values-item [item-id :damage-defense])
     :price-low (get-in @editor-values-item [item-id :price-low])
     :price-high (get-in @editor-values-item [item-id :price-high])}))


(defn make-rom-version-dialog-save-to
  [file rom]
  (dialog
    :title "Select version"
    :content (mig-panel :items [["Select the ROM version" "wrap"] [(selection! (combobox :id :version :size [240 :by 20] :model rom-versions) (rom-versions-map-r (get-rom-version rom)))]])
    :success-fn (fn [e] (rom-versions-map (selection (select (to-frame e) [:#version]))))))
    
(defn show-rom-version-dialog-save-to
  [file rom]
  (-> (make-rom-version-dialog-save-to file rom) pack! show!))
  
(defn choose-file-save-to
  []
  (when-let [file (choose-file)]
    (let [rom      (slurp-bytes file)
          version  (show-rom-version-dialog-save-to file rom)]
      [file (zipmap rom-part-labels (multi-split-at-hex rom (splits version)))])))

(defn save-to-aux
  []
  (when-let [[file parts]  (choose-file-save-to)]
    (let [spell-data       (future (zipmap spell-ids (map (partial extract-spell-data-from-rom-and-atom (parts "spell-data")) spell-ids)))
          item-data        (future (zipmap item-ids (map (partial extract-item-data-from-rom-and-atom (parts "item-data")) item-ids)))
          enemy-data       (future (zipmap enemy-ids (map (partial extract-enemy-data-from-rom-and-atom (parts "enemy-data") (parts "enemy-drop-data")) enemy-ids)))
          enemy-drop-data  (future (zipmap enemy-ids (map (partial extract-enemy-drop-data-from-rom-and-atom (parts "enemy-drop-data")) enemy-ids)))
          boss-data        (future (zipmap boss-ids (map (partial extract-boss-data-from-rom-and-atom (parts "boss-data") (parts "boss-data-2")) boss-ids)))
          attack-data      (future (zipmap (butlast attack-ids) (map (partial extract-attack-data-from-rom-and-atom (parts "attack-data")) (butlast attack-ids))))
          to-save       (reassemble-rom parts @spell-data @item-data @editor-values-shop @enemy-data @enemy-drop-data @boss-data @editor-values-experience @attack-data)]
    (spit-bytes to-save file)
    (alert "File was saved successfully."))))

(defn save-to
  []
  (when (not= @current-file nil)
    (save-to-aux)))

;-------------------------------------------------------------------------------

(defn reset-all-aux
  []
  (update-editor-atoms)
  (update-subframes))

(defn reset-all
  []
  (when (not= @current-file nil)
    (confirm-dialog "All unsaved changes will be lost. Continue anyway?" reset-all-aux)))

;-------------------------------------------------------------------------------

(defn exit-aux
  []
  (System/exit 0))

(defn exit-program
  []
  (if (= @current-file nil)
    (exit-aux)
    (confirm-dialog "All unsaved changes will be lost. Exit anyway?" exit-aux)))

;-------------------------------------------------------------------------------

(defn make-help
  []
  (let [help (try
               (slurp "help.html")
               (catch Exception e "Could not read from file help.html"))]
    (frame
      :title "Mystic Quest Editor Manual"
      :width 500 :height 500
      :icon poto-icon
      :content
      (scrollable
        (editor-pane
          :content-type "text/html"
          :editable? false
          :text help)))))

(def help-frame (make-help))

(defn show-help
  []
  (show! help-frame))
  
;-------------------------------------------------------------------------------

(defn make-license
  []
  (let [license-text (try
                       (slurp "License.txt")
                       (catch Exception e "Could not read from file License.txt"))]
    (frame
      :title "License"
      :width 550 :height 550
      :icon poto-icon
      :content
      (scrollable
        (editor-pane
          :content-type "text/plain"
          :editable? false
          :text license-text)))))

(def license-frame (make-license))
          
(defn show-license
  []
  (show! license-frame))
  
;-------------------------------------------------------------------------------

(defn make-about
  []
  (frame
    :title "About Mystic Quest Editor"
    :width 300 :height 220
    :icon poto-icon
    :content
    (border-panel
      :north
      (editor-pane
        :content-type "text/html"
        :editable? false
        :text "<html>
               <body style=\"margin-left:10px; margin-right:10px; text-align:center; \">
                 <h2>Mystic Quest Editor 1.0<h2>
                 <p>2021, by MS</p>
               </body>
               </html>")
      :center (flow-panel
        :items [(button :listen [:action (fn [e] (show-license))] :text "License")]))))
  
(defn show-about
  []
  (show! (make-about)))
  
;-------------------------------------------------------------------------------
; MAIN
;-------------------------------------------------------------------------------

(defn -main
  [& args]
  (show-frame))
