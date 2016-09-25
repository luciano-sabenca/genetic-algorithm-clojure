(ns genetic_algorithm.bitarray
  [:require
    [clojure.pprint :refer :all]])


; Luciano P. Sabenca
;
; a bitvector implementation based on:
; https://github.com/fffej/clojure-snippets/blob/master/bitarray.clj

(defrecord bit-field [bits element-width array-data])

(defn flip-bit! [bitfield bit]
  (let [r (mod bit (get bitfield :element-width))
        n (int (/ bit (get bitfield :element-width)))
        x (aget (get bitfield :array-data) n)]

    (aset (get bitfield :array-data) n (bit-flip x r))
    bitfield
    )
  )


(defn bit-array
  [n]
  (bit-field. n 31 (int-array (inc (int (/ n 31))))))

(defn set-bit!
  [bitfield bit val]
  (let [r (mod bit (get bitfield :element-width))
        n (int (/ bit (get bitfield :element-width)))
        x (aget (get bitfield :array-data) n)
        aux (bit-shift-left 1 r)]
    (if (not (zero? val))
      (aset (get bitfield :array-data) n (bit-or x aux))
      (aset (get bitfield :array-data) n
            (if (= 0 (bit-and x aux))
              x
              (bit-xor x (bit-shift-left 1 r)))))
    bitfield))

(defn copy-of [bitfield]
  (bit-field. (get bitfield :bits) (get bitfield :element-width) (aclone (get bitfield :array-data))))

(defn get-bit
  [bitfield bit]
  (let [r (mod bit (get bitfield :element-width))
        x (aget (get bitfield :array-data) (int (/ bit (get bitfield :element-width))))]
    (if (= 0 (bit-and x (bit-shift-left 1 r))) 0 1)))



(defn debug-bit-array [bitfield]
  (apply cl-format nil "2r~6,'0',B" (get bitfield :array-data))
  )