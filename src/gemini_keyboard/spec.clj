(ns gemini-keyboard.spec
  (:require [scad-clj.scad :refer [write-scad]]
            [scad-clj.model :refer
             [fs! fn! fa! *fn* hull circle cube cylinder difference extrude-rotate extrude-linear translate minkowski
              mirror rotate scale sphere union]]))

(def switch-min-width 13.89)
(def switch-max-width 15.5)
(def spacing 2.25)
(def thickness 5.5)
(def box-size (+ switch-min-width (* spacing 2) 0.05))
(fs! 120)
(def rows 5)
(def columns 7)
(def switch-count (-  (* rows columns) 4))
(def dev false)

(defn fuzzy-cube [x y z offset]
  (hull
   (cube x y (- z offset))
   (cube (- x offset) (- y offset) z)))

(def latch (hull (fuzzy-cube switch-max-width 4.9 0.8 0.7)))

(def diode-holder
  (let
   [z 2.8]
    (translate
     [0 0 (+ (/ thickness -2) (/ z 2))]
     (difference
      (fuzzy-cube 7 5.5 z 0.6)
      (translate [-0.4 0 0] (binding [*fn* (if dev 1 12)] (cylinder 1.2 (+ z 0.01))))
      (translate [1.6 0 0.61] (cube 1.9 4.0 (- z 0.6)))
      (translate [1.6 0 1.6] (cube 0.5 6.1 (- z 0.6)))))))

(def latch ())
(def diode-holder ())
(fs! 1)
(fn! 1)
(fa! 1)

(def shell-corner-offset (+ switch-max-width 5.5))

(defn cart [& lists]
  (let [syms (for [_ lists] (gensym))]
    (eval `(for [~@(mapcat #(list %1 `'~%2) syms lists)]
             (list ~@syms)))))

(defn round-cube [x y z radius]
  (let [corner (binding [*fn* (if dev 1 8)] (sphere radius))
        dimensions (map #(- (/ % 2) radius) [x y z])]
    (hull
     (map
      #(translate (mapv * % dimensions) corner)
      (apply cart (repeat 3 [1 -1]))))))

(defn cos [v] (Math/cos v))
(defn sin [v] (Math/sin v))
(defn abs [v] (Math/abs v))

(defn rotate-point [px py pz rx ry rz]
  (let [Axx (* (cos rz) (cos ry))
        Axy (- (* (cos rz) (sin ry) (sin rx)) (* (sin rz) (cos rx)))
        Axz (+ (* (cos rz) (sin ry) (cos rx)) (* (sin rz) (sin rx)))
        Ayx (* (sin rz) (cos ry))
        Ayy (+ (* (sin rz) (sin ry) (sin rx)) (* (cos rz) (cos rx)))
        Ayz (- (* (sin rz) (sin ry) (cos rx)) (* (cos rz) (sin rx)))
        Azx (- (sin ry))
        Azy (* (cos ry) (sin rx))
        Azz (* (cos ry) (cos rx))]
    [(+ (* Axx px) (* Axy py) (* Axz pz))
     (+ (* Ayx px) (* Ayy py) (* Ayz pz))
     (+ (* Azx px) (* Azy py) (* Azz pz))]))

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
    #{0 6 13 20} 1
    #{1 7 14 21 27 28} 2
    #{2 8 15 22 29} 3
    #{3 9 16 23 30 31 32} 4
    #{4 10 17 24 31} 5
    #{5 11 18 25} 6
    nil))

(def switch-shell
  (round-cube shell-corner-offset shell-corner-offset thickness 1.5))

(def home-row 2)

(def vertical-inclination 0.04)

(defn get-vertival-inclination [row] (* (- home-row row) vertical-inclination))

(defn get-horizontal-inclination [column]
  (condp contains? column
    #{6} 0
    #{2} 0.07
    #{1} 0.09
    0.06))

(defn get-inclination-height [angle] (* box-size 0.5 (sin (abs angle))))

(defn get-z-offset [row column]
  (let [current-vertical-offset (get-inclination-height (get-vertival-inclination row))
        previous-row (+ row (if (> row home-row) -1 1))
        previous-vertical-offset (if (= row home-row)
                                   0.0
                                   (+ (get-z-offset previous-row column) (get-inclination-height
                                                                          (get-vertival-inclination previous-row))))
        vertical-offset (+ current-vertical-offset previous-vertical-offset)
        current-horizontal-offset (get-inclination-height (get-horizontal-inclination column))
        previous-column (inc column)
        previous-horizontal-offset (if (= column (dec columns))
                                     0.0
                                     (+ (get-z-offset row previous-column) (get-inclination-height
                                                                            (get-horizontal-inclination previous-column))))
        horizontal-offset (+ current-horizontal-offset previous-horizontal-offset)]
    (max vertical-offset horizontal-offset)))

(defn get-position [index]
  (let [offset [-4 -1 1 4.5 3.5 -5 -5.5]
        z-offset-correction [0 0 0 -1.5 -0.5 0 0]
        column (get-column index)
        row (get-row index)]
    (if (or (nil? row) (nil? column))
      nil
      (case index
        12 [(- (* column box-size) 1) (+ (* row (- box-size 0.5) -1) (get offset column 0)) (get (get-position 6)
                                                                                                 2) 0.03 0 0 0.1]
        26 [(+ (* column box-size) 1.0) (+ (* row (- box-size 0.5) -1) (get offset column 0) 6.5)
            (get-inclination-height 0.1) 0 -0.1 0.19]
        [(* column box-size)
         (+ (* row (- box-size 0.5) -1)
            (get offset column 0))
         (+ (get-z-offset row column) (get z-offset-correction column))
         (get-vertival-inclination row)
         (get-horizontal-inclination column)
         0]))))

(def controller-height (let [[_ _ z] (get-position 6)] z))

(def switch-positions (filter some? (map get-position (range switch-count))))

(defn get-switch-group [form] (union (map #(translate (take 3 %) (rotate (drop 3 %) form)) switch-positions)))

(def switch-shells (get-switch-group switch-shell))

(def connector-end
  (scale [1 1 0.9] (union (cylinder [1.5 0] 1.9) (translate [0 0 -1] (cylinder 1.5 1)))))

(defn get-connector [start end]
  (hull
   (translate start connector-end)
   (translate end connector-end)))

(def horizontal-matches [[2 7] [3 6]])

(def vertical-connectors
  (loop
   [switches (sort #(compare (get-column %1) (get-column %2)) (range switch-count))
    result []]
    (if (> (count switches) 1)
      (if (= (get-column (first switches)) (get-column (second switches)))
        (recur (next switches) (conj result (vector [(first switches) 4] [(second switches) 1])))
        (recur (next switches) result))
      result)))

(def horizontal-connectors
  (filter #(not (some (fn [x] (= (first %) x)) [[26 2] [27 2]]))
          (map (fn [[index [from to]]] (vector [index from] [(inc index) to]))
               (cart
                (filter #(= (get-row %) (get-row (+ % 1))) (range (- switch-count 1)))
                horizontal-matches))))

(def connector-base-offset (+ (/ thickness -2) 2.2))

(defn get-juncture [[index connector-id]]
  (let [position (get-position index)]
    (if (nil? position) nil
        (let [[x y z rx ry rz] position
              base (/ switch-min-width 2)
              offsetsX [-6 0]
              offsetsY [-4.5 6]
              baseX (into [] (concat offsetsX (take 2 (repeat base)) (reverse offsetsX) (take 2 (repeat (* base -1)))))
              baseY (into [] (concat (take 2 (repeat base)) (reverse offsetsY) (take 2 (repeat (* base -1))) offsetsY))
              baseZ (+ z connector-base-offset)
              [a b c] (rotate-point (get baseX connector-id) (get baseY connector-id) baseZ rx ry rz)]
          [(+ a x) (+ b y) c]))))

(def get-connectors #(map (fn [[from to]] (get-connector (get-juncture from) (get-juncture to))) %))

(def controller-holder-cutout
  (translate
   [(- box-size 19.4) -11 controller-height]
   (union
    (translate [-10 13.5 -5.5]
               (rotate
                [0 (/ Math/PI 2) 0]
                (binding [*fn* (if dev 1 32)]
                  (union
                   (cylinder [1.6 1.53] 4)
                   (translate [0 0 -3.99] (cylinder 1.6 4))))))
    (translate [0 12.4 (- thickness 1.6 3.2)] (round-cube 8 12.4 thickness 0.5))
    (translate [0 23 (- thickness 1.6 2.8)] (round-cube 7 23 thickness 0.8))
    (translate [0 25.68 (- thickness 1.6 2.9 0.15)] (round-cube 11.5 18 8 0.5))
    (translate [0 -0.1 1.5] (round-cube 18.3 33.2 1.8 0.3))
    (translate [0 0.35 7.5] (cube 18.1 32.9 10.6))
    (translate [0 -0.25 -0.687] (cube 16.2 30.5 100)))))

(def controller-holder
  (translate
   [(- box-size 19.4) -11 (/ thickness 2)]
   (round-cube 23 37 (+ thickness controller-height) 1.3)))

(def controller-connector-z (- controller-height 4))

(def mainboard-hull
  (hull
   switch-shells
   controller-holder))

(defn get-keyboard [orientation]
  (let [cap-height 40
        cap-offset (+ (/ thickness 2) (/ cap-height 2) -1.01)
        switch-holder-cutout
        (union
         (difference
          (union
           (translate [0 0 cap-offset] (round-cube (+ box-size 0.6) (+ box-size 0.6) cap-height 1))
           (cube switch-min-width switch-min-width thickness)
           (hull
            (translate [0 0 0.3] (cube switch-min-width switch-min-width 0.001))
            (translate [0 0 (- (/ thickness -2) 10)] (cube (+ switch-min-width 6) (+ switch-min-width 6) 0.001))))

          (mirror [(if (= orientation :left) 1 0) 0 0] (translate [-5.5 0 0] diode-holder)))
         (translate [0 (- (/ switch-min-width 2) 3.4) (- (/ thickness 2) 1.8)] latch)
         (translate [0 (+ (/ switch-min-width -2) 3.4) (- (/ thickness 2) 1.8)] latch))]

    (mirror
     [(if (= orientation :left) 1 0) 0 0]
     (difference
      (union
       switch-shells
       mainboard-hull
       controller-holder)
      (union
       controller-holder-cutout
       (get-switch-group switch-holder-cutout)
       (get-connector (get-juncture [0 7]) [1.5 -3 controller-connector-z])
       (get-connector (get-juncture [0 6]) [1.5 -10  controller-connector-z])
       (get-connector (get-juncture [6 7]) [1.5 -17  controller-connector-z])
       (get-connector (get-juncture [6 6]) [1.5 -19  controller-connector-z])
       (get-connector (get-juncture [12 0]) [-6 -20  controller-connector-z])
       (get-connector (get-juncture [12 1]) [0 -20  controller-connector-z])
       (get-connector (get-juncture [28 7]) (get-juncture [26 3]))
       (get-connector (get-juncture [20 7]) (get-juncture [12 3]))
       (get-connector [4 -34 controller-connector-z] [5 -20 controller-connector-z])
       (get-connectors vertical-connectors)
       (get-connectors horizontal-connectors))))))

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
  (spit "hull.scad" (write-scad mainboard-hull))
  (spit "tool.scad" (write-scad tool))
  (spit "palm-rest.scad" (write-scad (binding [*fn* 15] (union (translate [10 0 0] (sphere 1.5)) (extrude-rotate {:angle 360} (translate [2 0 0] (circle 1.5))))))))
