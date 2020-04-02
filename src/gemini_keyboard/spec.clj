(ns gemini-keyboard.spec
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]))

(def switch-min-width 13.85)
(def switch-max-width 15.5)
(def spacing 2.25)
(def thickness 5.5)

(defn fuzzy-cube [x y z offset]
  (hull
   (cube x y (- z offset))
   (cube (- x offset) (- y offset) z)))

(def latch (hull (fuzzy-cube switch-max-width 4.9 0.8 0.7)))
(def boxSize (+ switch-min-width (* spacing 2) 0.05))
(fs! 120)
(def rows 5)
(def columns 7)
(def switch-count (-  (* rows columns) 2))

(def diode-holder
  (let
   [z 2.8]
    (translate
     [0 0 (+ (/ thickness -2) (/ z 2))]
     (difference
      (fuzzy-cube 7 5.5 z 0.6)
      (translate [-0.4 0 0] (binding [*fn* 12] (cylinder 1.2 (+ z 0.01))))
      (translate [1.6 0 0.61] (cube 1.9 4.0 (- z 0.6)))
      (translate [1.6 0 1.6] (cube 0.5 6.1 (- z 0.6)))))))

(def shell-corner-offset (+ switch-max-width 5.5))

(defn cart [& lists]
  (let [syms (for [_ lists] (gensym))]
    (eval `(for [~@(mapcat #(list %1 `'~%2) syms lists)]
             (list ~@syms)))))

(defn round-cube [x y z radius]
  (let [corner (binding [*fn* 8] (sphere radius))
        dimensions (map #(- (/ % 2) radius) [x y z])]
    (hull
     (map
      #(translate (mapv * % dimensions) corner)
      (apply cart (repeat 3 [1 -1]))))))

(defn cos [v] (Math/cos v))
(defn sin [v] (Math/sin v))

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
    #{19 20 21 22 23 24 25} 3
    4))

(defn get-column [index]
  (condp contains? index
    #{12 19} 0
    #{0 6 13 20} 1
    #{1 7 14 21} 2
    #{2 8 15 22 29 26 27 28} 3
    #{3 9 16 23 30 31 32} 4
    #{4 10 17 24 31} 5
    6))

(def switch-shell
  (round-cube shell-corner-offset shell-corner-offset thickness 1.5))

(defn get-position [index]
  (let [offset [-4 -1 1 4.5 3.5 -5 -5.5]
        column (get-column index)
        row (get-row index)]
    (case index
      12 [(- (* column boxSize) 1) (+ (* row (- boxSize 0.5) -1) (get offset column 0)) 0 0 0 0]
      19 [(- (* column boxSize) 0.8) (+ (* row (- boxSize 0.5) -1) (get offset column 0) -8) 0 0 0 0.19]
      [(* column boxSize) (+ (* row (- boxSize 0.5) -1) (get offset column 0)) 0 0 0 0])))

(def switch-positions (map get-position (range switch-count)))

(defn get-switch-group [form] (union (map #(translate (take 3 %) (rotate (drop 3 %) form)) switch-positions)))

(def switch-shells (get-switch-group switch-shell))

(def connector-end
  (union (cylinder [1.5 0] 1.9) (translate [0 0 -1] (cylinder 1.5 1))))

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
  (map (fn [[index [from to]]] (vector [index from] [(inc index) to]))
       (cart
        (filter #(= (get-row %) (get-row (+ % 1))) (range (- switch-count 1)))
        horizontal-matches)))

(def connector-base-offset (+ (/ thickness -2) 2.2))

(defn get-juncture [[index connector-id]]
  (let [[x y z rx ry rz] (get-position index)
        base (/ switch-min-width 2)
        offsetsX [-6 0]
        offsetsY [-4.5 6]
        baseX (into [] (concat offsetsX (take 2 (repeat base)) (reverse offsetsX) (take 2 (repeat (* base -1)))))
        baseY (into [] (concat (take 2 (repeat base)) (reverse offsetsY) (take 2 (repeat (* base -1))) offsetsY))
        baseZ connector-base-offset
        [a b c] (rotate-point (get baseX connector-id) (get baseY connector-id) baseZ rx ry rz)]
    [(+ a x) (+ b y) (- c z)]))

(def get-connectors #(map (fn [[from to]] (get-connector (get-juncture from) (get-juncture to))) %))

(def controller-holder-cutout
  (translate
   [(- boxSize 19.2) -11 0]
   (union
    (translate [-10 13 -0.6] (rotate [0 (/ Math/PI 2) 0] (binding [*fn* 32] (cylinder [1.9 1.6] 4))))
    (translate [0 23 (- thickness 1.6 2.9)] (round-cube 8 23 thickness 0.5))
    (translate [0 25.7 (- thickness 1.6 2.9 0.15)] (round-cube 10 13 8 0.5))
    (translate [0 0 1.5] (round-cube 18.3 33.3 1.8 0.3))
    (translate [0 0.35 3.0] (cube 18.1 32.9 1.6))
    (translate [0 -0.25 -1.2] (cube 16.2 30.5 (- thickness 1.57))))))

(def controller-holder
  (translate
   [(- boxSize 19.2) -11 0]
   (round-cube 23 37 thickness 1.3)))

(defn get-keyboard [orientation]
  (def switch-holder-cutout
    (union
     (difference
      (union
       (cube switch-min-width switch-min-width thickness)
       (hull
        (translate [0 0 0.3] (cube switch-min-width switch-min-width 0.001))
        (translate [0 0 (/ thickness -2)] (cube (+ switch-min-width 2.7) (+ switch-min-width 2.7) 0.001))))

      (mirror [(if (= orientation :left) 1 0) 0 0] (translate [-5.5 0 0] diode-holder)))
     (translate [0 (- (/ switch-min-width 2) 3.4) (- (/ thickness 2) 1.8)] latch)
     (translate [0 (+ (/ switch-min-width -2) 3.4) (- (/ thickness 2) 1.8)] latch)))

  (def switch-cutouts (get-switch-group switch-holder-cutout))

  (mirror
   [(if (= orientation :left) 1 0) 0 0]
   (difference
    (hull
     switch-shells
     controller-holder)
    (union
     controller-holder-cutout
     switch-cutouts
     (get-connector (get-juncture [0 7]) [1.5 -3 connector-base-offset])
     (get-connector (get-juncture [0 6]) [1.5 -10  connector-base-offset])
     (get-connector (get-juncture [6 7]) [1.5 -17  connector-base-offset])
     (get-connector (get-juncture [6 6]) [1.5 -19  connector-base-offset])
     (get-connector (get-juncture [12 0]) [-6 -20  connector-base-offset])
     (get-connector (get-juncture [12 1]) [0 -20  connector-base-offset])
     (get-connector [4 -34 connector-base-offset] [5 -20 connector-base-offset])
     (get-connectors vertical-connectors)
     (get-connectors horizontal-connectors)))))

(def tool
  (let
   [z 2.6]
    (union
     (translate [0.8 3.9 1] (binding [*fn* 15] (cylinder [0.8 0.6] 4)))
     (difference
      (fuzzy-cube 12 9.2 z 0.6)
      (translate [0 -0.9 0.6] (cube 1.9 4.0 (- z 0.6)))
      (translate [0 -0.4 0.8] (cube 0.5 20 (- z 0.6)))
      (translate [0 3.4 0.8] (fuzzy-cube 30 3 (- z 0.6) 0.6))))))

(spit "gemini-right.scad" (write-scad (get-keyboard :right)))
(spit "gemini-left.scad" (write-scad (get-keyboard :left)))
(spit "tool.scad" (write-scad tool))
(spit "palm-rest.scad" (write-scad (binding [*fn* 15] (union (translate [10 0 0] (sphere 1.5)) (extrude-rotate {:angle 360} (translate [2 0 0] (circle 1.5)))))))

