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

(ns mystic-quest.arithmetic)

(defn to-p-adic
  "Returns the p-adic representation of a non-negative integer n."
  [n p]
  (loop [m n zs '()]
    (if (== m 0)
      (if (empty? zs)
        (list 0)
        zs)
      (let [r (rem m p)]
        (recur (/ (- m r) p) (conj zs r))))))

(defn to-p-adic-fixed-digits
  "Returns the last number-of-digits digits of the p-adic representation of a non-negative integer n."
  [n p number-of-digits]
  (loop [i number-of-digits zs '() m n]
    (if (== i 0)
      zs
      (let [r (rem m p)]
        (recur (dec i) (conj zs r) (/ (- m r) p))))))

(defn make-pos
  "Turns the value of a byte positive if needed (slurp reads the file as signed int)."
  [byte]
  (if (neg? byte) (+ 256 byte) byte))

(defn make-neg
  "Turns the value of a byte negative if needed."
  [byte]
  (if (> byte 127) (- byte 256) byte))

(defn hexadecimal-to-decimal
  "Converts a hexadecimal number, given as a string, to a decimal number."
  [n]
  (let [digit-map {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \A 10 \B 11 \C 12 \D 13 \E 14 \F 15}]
    (loop [[first-digit & remaining-digits] (reverse n) i 1 s 0]
      (if first-digit
        (recur remaining-digits (* 16 i) (+ s (* (digit-map first-digit) i)))
        s))))

(defn decimal-to-hexadecimal
  "Converts a decimal number to a hexadecimal number, represented by a string."
  [n]
  (let [digit-map (into (zipmap (range 0 10) (range 0 10)) {10 "A" 11 "B" 12 "C" 13 "D" 14 "E" 15 "F"})]
    (loop [m n hex ""]
      (if (== m 0)
        (if (= hex "")
          "0"
          hex)
        (let [r (rem m 16)]
          (recur (/ (- m r) 16) (str (digit-map r) hex)))))))

(defn +hex
  "Addition of hexadecimal numbers."
  [& summands]
    (decimal-to-hexadecimal (apply + (map hexadecimal-to-decimal summands))))

(defn *hex
  "Multiplication of hexadecimal numbers."
  [& factors]
    (decimal-to-hexadecimal (apply * (map hexadecimal-to-decimal factors))))
    
(defn <hex
  "Comparison of hexadecimal numbers."
  [a b]
    (< (hexadecimal-to-decimal a) (hexadecimal-to-decimal b)))
