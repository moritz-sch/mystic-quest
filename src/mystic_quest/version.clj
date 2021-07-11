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

(ns mystic-quest.version)

;Mystic Quest (E)
;spell data 9DDA-9E59 (8 entries with 16 bytes each)
;item data 9E5A-A4A9 (100 entries with 16 bytes each)
;shop data A4EA-A5F9 (17 entries with 16 bytes each)
;enemy data D9FE-DF67 (99 entries with 14 bytes each)
;enemy drop data DF70-E8B7 (99 entries with 24 bytes each (only two of which determine which items are dropped)... although it seems to go on after that)
;boss data 10739-10930 (21 entries with 24 bytes each)
;boss data 2 10931-10df0 (20 (Dragon and Red Dragon share their data) entries with a variable number of bytes, grouped in blocks of 8)
;experience data 20DE5-20F10 (100 entries with 3 bytes each)
;attack data 24479-246f8 (40 entries with 16 bytes each)

;Differences in other versions:

;Mystic Quest (G) and Mystic Quest (F)
;spell data 9DE4-9E63
;item data 9E64-A4B3
;shop data A4F4-A603

;Final Fantasy Adventure (U)
;experience data 20DD9-20F04

;Seiken Densetsu (J)
;spell data 9F65-9FE4
;item data 9FE5-A634
;shop data A675-A784
;experience data 20DCD-20EF8


(def rom-versions ["Mystic Quest (E)" "Mystic Quest (G) / Mystic Quest (F)" "Final Fantasy Adventure (U)" "Seiken Densetsu (J)"])

(def rom-versions-map (zipmap rom-versions [:eu :de-fr :us :jp]))
(def rom-versions-map-r (zipmap [:eu :de-fr :us :jp] rom-versions))

(def splits {:eu    [        "9DDA"  ;spell data
                     "9E5A"  "A4AA"  ;item data
                     "A4EA"  "A5FA"  ;shop data
                     "D9FE"  "DF68"  ;enemy data
                     "DF70"  "E8B8"  ;enemy drop data
                     "10739" "10931" ;boss data
                             "10DF1" ;boss data 2
                     "20DE5" "20F11" ;experience data
                     "24479" "246F9" ;attack data
                    ]
             :de-fr [        "9DE4"  ;spell data
                     "9E64"  "A4B4"  ;item data
                     "A4F4"  "A604"  ;shop data
                     "D9FE"  "DF68"  ;enemy data
                     "DF70"  "E8B8"  ;enemy drop data
                     "10739" "10931" ;boss data
                             "10DF1" ;boss data 2
                     "20DE5" "20F11" ;experience data
                     "24479" "246F9" ;attack data
                    ]
             :us    [        "9DDA"  ;spell data
                     "9E5A"  "A4AA"  ;item data
                     "A4EA"  "A5FA"  ;shop data
                     "D9FE"  "DF68"  ;enemy data
                     "DF70"  "E8B8"  ;enemy drop data
                     "10739" "10931" ;boss data
                             "10DF1" ;boss data 2
                     "20DD9" "20F05" ;experience data
                     "24479" "246F9" ;attack data
                    ]
             :jp    [        "9F65"  ;spell data
                     "9FE5"  "A635"  ;item data
                     "A675"  "A785"  ;shop data
                     "D9FE"  "DF68"  ;enemy data
                     "DF70"  "E8B8"  ;enemy drop data
                     "10739" "10931" ;boss data
                             "10DF1" ;boss data 2
                     "20DCD" "20EF9" ;experience data
                     "24479" "246F9" ;attack data
                    ]})

(def rom-part-labels ["rom-part-1"
                      "spell-data"
                      "item-data"
                      "rom-part-2"
                      "shop-data"
                      "rom-part-3"
                      "enemy-data"
                      "rom-part-4"
                      "enemy-drop-data"
                      "rom-part-5"
                      "boss-data"
                      "boss-data-2"
                      "rom-part-6"
                      "experience-data"
                      "rom-part-7"
                      "attack-data"
                      "rom-part-8"])

(def rom-checksum-map {'(81 -2 38)     :eu
                       '(81 35 -122)   :de-fr  ;de
                       '(81 -14 30)    :de-fr  ;fr
                       '(-48 -39 -14)  :us
                       '(-47 -46 -70)  :jp})
                       
(defn get-rom-checksums
  "Takes a ROM and returns its checksums (at 014D-014F) to identify the version."
  [rom]
  (drop 333 (take 336 rom)))
  
(defn get-rom-version
  "Takes a ROM and tries to identify the version based on the checksums."
  [rom]
  (rom-checksum-map (get-rom-checksums rom)))
