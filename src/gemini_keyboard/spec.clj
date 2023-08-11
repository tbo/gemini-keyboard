(ns gemini-keyboard.spec
  (:require [scad-clj.scad :refer [write-scad]]
            [scad-clj.model :refer
             [fs! *fn* hull cube cylinder difference translate minkowski
              mirror rotate scale sphere union render]]))

(def dev false)
(def switch-min-width 14.00)
(def switch-max-width 15.5)
(def spacing 2.25)
(def thickness 4.3)
(def box-size (+ switch-min-width (* spacing 2) 0.05))
(fs! 120)
(def rows 5)
(def columns 7)
(def switch-count (-  (* rows columns) 4))

(defn fuzzy-cube [x y z offset]
  (hull
   (cube x y (- z offset))
   (cube (- x offset) (- y offset) z)))

(defn cart [& lists]
  (let [syms (for [_ lists] (gensym))]
    (eval `(for [~@(mapcat #(list %1 `'~%2) syms lists)]
             (list ~@syms)))))

(defn round-cube [x y z radius]
  (let [corner (binding [*fn* (if dev 1 32)] (sphere radius))
        dimensions (map #(- (/ % 2) radius) [x y z])]
    (hull
     (map
      #(translate (mapv * % dimensions) corner)
      (apply cart (repeat 3 [1 -1]))))))

(defn new-diode-holder [z]
  (union
   (difference
    (fuzzy-cube 6 8  z 0.5)
    (translate [0 -2 0.61] (cube 4.0 1.9 (- z 0.6)))
    (translate [0 -2 1.6] (cube 6.1 0.5 (- z 0.6))))))

(def shell-corner-offset (+ switch-max-width 5.5))

(defn cos [v] (Math/cos v))
(defn sin [v] (Math/sin v))

(defn get-row [index]
  (condp contains? index
    #{0 1 2 3 4 5} 0
    #{6 7 8 9 10 11} 1
    #{12 13 14 15 16 17 18} 2
    #{20 21 22 23 24 25} 3
    #{26 27 28 29 30 31 32} 4
    nil))

(defn get-column [index]
  (condp contains? index
    #{12 26} 0
    #{0 6 13 20 27} 1
    #{1 7 14 21 28} 2
    #{2 8 15 22 29} 3
    #{3 9 16 23 30 31 32} 4
    #{4 10 17 24 31} 5
    #{5 11 18 25} 6
    nil))

(def switch-shell
  (round-cube shell-corner-offset shell-corner-offset thickness 1.5))

(defn get-position [index]
  (let [x-offset [0 0 0 0 0 0 0.7]
        y-offset [-4 -1 1 4.5 2.0 -4 -5.5]
        z-base-offset -1
        column (get-column index)
        row (get-row index)]
    (if (or (nil? row) (nil? column))
      nil
      (case index
        12 [(- (* column box-size) 1) (+ (* row (- box-size 0.5) -1) (get y-offset column 0) 0.3) (get (get-position 6)
                                                                                                       2) 0 0 0]
        26 [(+ (* column box-size) 2.1) (+ (* row (- box-size 0.5) -1) (get y-offset column 0) 4.9)
            0 0 0 0]
        27 [(+ (* column box-size) (get x-offset column 0) 2.0)
            (+ (* row (- box-size 0.5) -1)
               (get y-offset column) 2.0)
            z-base-offset
            0
            0
            0]
        [(+ (* column box-size) (get x-offset column 0))
         (+ (* row (- box-size 0.5) -1)
            (get y-offset column 0))
         z-base-offset
         0
         0
         0]))))

(def controller-height (let [[_ _ z] (get-position 6)] z))

(def switch-positions (filter some? (map get-position (range switch-count))))

(defn get-switch-hull [form [x y z a b c]]
  (hull
   (translate [x y 0] (scale [1.1 1.1 1] (rotate [0 0 c] form)))
   (translate [x y z] (scale [1.1 1.1 1] (rotate [a b c] form)))))

(defn get-switch-hull-group [form] (union (map (partial get-switch-hull form) switch-positions)))

(def switch-hulls (render (get-switch-hull-group switch-shell)))

(def connector-end
  (scale [1 1 0.9] (union (cylinder [1.5 0] 1.9) (translate [0 0 -1] (cylinder 1.5 1)))))

(defn get-connector [start end]
  (translate [0 0 -1.3]
             (hull
              (translate start connector-end)
              (translate end connector-end))))

(def controller-holder-cutout
  (translate
   [(- box-size 19.4) -11 (+ controller-height 2.5)]
   (union
    (translate [-10 13.5 -6.5]
               (binding [*fn* (if dev 1 32)]
                 (union
                  (cylinder [1.6 1.55] 4)
                  (translate [0 0 -3.99] (cylinder 1.6 4)))))
    (translate [0 12.4 (- thickness 1.6 3.2)] (round-cube 8 12.4 thickness 0.5))
    (translate [0 23 (- thickness 1.6 2.8)] (round-cube 7 23 thickness 0.8))
    (translate [0 25.68 (- thickness 1.6 2.9 0.15)] (round-cube 11.5 18 8 0.5))
    (translate [0 -0.2 1.5] (round-cube 18.3 33.1 1.8 0.3))
    (translate [0 0.35 7.5] (cube 18.1 32.9 10.6))
    (translate [0 -0.25 -0.687] (difference (cube 16.2 30.5 100) (translate [1 13.5 -12.3] (cube 9 7 7)))))))

(def controller-holder
  (translate
   [(- box-size 19.4) -11 (/ controller-height 2)]
   (round-cube 23 37 (+ thickness controller-height) 1.5)))

(def mainboard-hull
  (union
   (hull
    switch-hulls
    controller-holder)
   ;(translate [75 -137 0] wrist-rest)
   ))

(def left [[1 1 1 1 1 1 1]
           [1.5 1 1 1 1 1]
           [1.75 1 1 1 1 1]
           [2.25 1 1 1 1 1]
           [1 1 1 1 1.5 1.5]])

(def right [[1 1 1 1 1 1 1]
            [1.5 1 1 1 1 1]
            [1.75 1 1 1 1 1]
            [2.25 1 1 1 1 1]
            [1 1 1 1 1.5 1.5]])

(defn socket-cube [x y z radius]
  (let [corner (binding [*fn* (if dev 1 32)] (cylinder radius z))
        dimensions (map #(- (/ % 2) radius) [x y])]
    (hull
     (map
      #(translate (mapv * % dimensions) corner)
      (apply cart (repeat 2 [1 -1]))))))

(def switch-space 19.6)

(defn get-positions [matrix]
  (apply concat
         (map-indexed
          (fn [index row]
            (map
             (fn [value] [(*  index switch-space) (* (second value) switch-space)])
             (reduce
              (fn [s v] (let [offset (+ (or (first (last s)) 0) v)] (conj s [offset (- offset (/ v 2))])))
              []
              row))) matrix)))

(def switch-socket
  (render
   (let [iw 13.70
         ew 14.64
         ww (+ iw 5.0)
         radius 0.5
         ty 2.3
         b 6.6
         by (- b ty)]
     (union
      (difference
       (union
        (translate [0 0 (/ ty 2)] (socket-cube ew ew ty radius))
        (translate [0 0 (/ (- by 2) -2.4)] (cube switch-space switch-space by)))
       (translate [0 0 (/ ty 2)] (socket-cube iw iw (+ ty 0.01) radius))
       (translate [(- (/ iw 2) 1.5) 0  (/ by -2)] (scale [1 1.8 1] (binding [*fn* (if dev 1 32)] (cylinder 2 by))))
       (translate [0 0 0.25] (difference (cube 10 (+ iw 0.5) 0.8) (cube 10 (- iw 0.5) 0.8)))
       (difference
        (hull
         (socket-cube (- iw 2) iw 0.01 radius)
         (translate [0 0 (+ (/ b -2)  0.1)] (socket-cube ww ww 0.01 radius)))
        (translate [-0.5 7.6 -1.65] (new-diode-holder 3.0))))))))

(defn get-switches [positions form] (map (fn [[y x]] (translate [y x 0] form)) positions))

(defn get-horizontal-connectors [form] (let [offset 5.5] (union (translate [offset 0 0] form) (translate [(* offset -1) 0 0] form))))

(defn get-keyboard [orientation]
  (let [keys (if (= orientation :left) left right)
        height (* switch-space (count keys))
        width (* switch-space (apply max (map #(reduce + %) keys)))
        controller-height 18
        positions (map #(vector (+ (/ (- height switch-space) -2) (first %)) (+ (/ width -2) (second %))) (get-positions keys))
        switches (get-switches positions switch-socket)
        spaces (get-switches positions (cube switch-space switch-space 30))]
    (difference
     (union
      (difference
       (translate [(/ controller-height 2) 0 (/ (- thickness 2) -2.4) -1] (cube (+  height controller-height) width thickness))
       spaces)
      switches)
     (get-horizontal-connectors (get-connector (nth positions 0) (nth positions 6)))
     (get-horizontal-connectors (get-connector (nth positions 7) (nth positions 12)))
     (get-horizontal-connectors (get-connector (nth positions 13) (nth positions 18)))
     (get-horizontal-connectors (get-connector (nth positions 19) (nth positions 24)))
     (get-horizontal-connectors (get-connector (nth positions 25) (nth positions 30)))
     (get-connector (nth positions 0) (nth positions 7))
     (get-connector (nth positions 7) (nth positions 13))
     (get-connector (nth positions 13) (nth positions 19))
     (get-connector (nth positions 19) (nth positions 25))
     (get-connector (nth positions 19) (nth positions 26))
     (get-connector (nth positions 2) (nth positions 8))
     (get-connector (nth positions 8) (nth positions 14))
     (get-connector (nth positions 14) (nth positions 20))
     (get-connector (nth positions 20) (nth positions 27))
     (get-connector (nth positions 3) (nth positions 9))
     (get-connector (nth positions 9) (nth positions 15))
     (get-connector (nth positions 15) (nth positions 21))
     (get-connector (nth positions 21) (nth positions 28))
     (get-connector (nth positions 4) (nth positions 10))
     (get-connector (nth positions 10) (nth positions 16))
     (get-connector (nth positions 16) (nth positions 22))
     (get-connector (nth positions 22) (nth positions 29))
     (get-connector (nth positions 5) (nth positions 11))
     (get-connector (nth positions 11) (nth positions 17))
     (get-connector (nth positions 17) (nth positions 23))
     (get-connector (nth positions 23) (nth positions 30))
     (get-connector (nth positions 6) (nth positions 12))
     (get-connector (nth positions 12) (nth positions 18))
     (get-connector (nth positions 18) (nth positions 24))
     (get-connector (nth positions 24) (nth positions 30))
     (translate [(+ (/ height 2) (/ controller-height 2) 0.4) -65.5  -2.8] (mirror [0 1 0] controller-holder-cutout)))))

(def tool
  (let
   [z 2.6]
    (union
     (translate [0.8 3.9 1] (binding [*fn* 64] (cylinder [0.8 0.6] 2.8)))
     (difference
      (fuzzy-cube 12 9.2 z 0.6)
      (translate [0 -0.9 0.6] (cube 1.9 4.0 (- z 0.6)))
      (translate [0 -0.4 0.8] (cube 0.5 20 (- z 0.6)))
      (translate [0 3.4 0.8] (fuzzy-cube 30 3 (- z 0.6) 0.6))))))

(defn get-case [orientation]
  (let [b 3
        c 2
        mainboard (scale [1 1 1.2] mainboard-hull)]
    (mirror
     [(if (= orientation :left) 1 0) 0 0]
     (difference
      (minkowski (translate [0 0 0] (cube b b c)) mainboard)
      (translate
       [0 0 (+ (/ c 2) 0.01)]
       (union
        (minkowski (cube 0.1 0.1 0.1) mainboard)
        controller-holder-cutout))))))

(defn write-models []
  (spit "case-right.scad" (write-scad (get-case :right)))
  (spit "case-left.scad" (write-scad (get-case :left)))
  (spit "gemini-right.scad" (write-scad (get-keyboard :right)))
  (spit "gemini-left.scad" (write-scad (get-keyboard :left)))
  (spit "demo-switch.scad" (write-scad switch-socket))
  (spit "tool.scad" (write-scad tool)))
