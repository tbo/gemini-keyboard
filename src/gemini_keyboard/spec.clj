(ns gemini-keyboard.spec
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]))

(def switch-min-width 13.75)
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
(def rows 3)
(def columns 3)
(def switch-count (- (* rows columns) 2))

;; (def diode-holder
;;   (let
;;    [x 1.5
;;     z 3.2]
;;     (translate [(+ (/ (+ switch-min-width 2.5) -2) (/ x 2)) 0 (+ (/ thickness -2) (/ z 2))]
;;                (difference
;;                 (cube x 5 z)
;;                 (translate [0 0 0.4] (cube (+ x 0.01) 3.7 3.2))
;;                 (translate [0.75 0 0.6] (cube 2 8 2.7))))))

(def diode-holder
  (let
   [x 1.5
    z 2.8]
    (translate
     [-5.5 0 (+ (/ thickness -2) (/ z 2))]
     (difference
      (fuzzy-cube 7 5.5 z 0.6)
      (translate [-0.4 0 0] (binding [*fn* 12] (cylinder 1.2 (+ z 0.01))))
      (translate [1.6 0 0.61] (cube 1.9 4.0 (- z 0.6)))
      (translate [1.6 0 1.6] (cube 0.5 6.1 (- z 0.6)))))))

(def shell-corner-offset (+ switch-max-width 5.5))
(def switch-holder-cutout
  (union
   (difference
    (union
     (cube switch-min-width switch-min-width thickness)
     (hull
      (translate [0 0 0.3] (cube switch-min-width switch-min-width 0.0001))
      (translate [0 0 (/ thickness -2)] (cube (+ switch-min-width 2.7) (+ switch-min-width 2.7) 0.0001))))

    (translate [0 0 0] diode-holder))
   (translate [0 (- (/ switch-min-width 2) 3.4) (- (/ thickness 2) 1.8)] latch)
   (translate [0 (+ (/ switch-min-width -2) 3.4) (- (/ thickness 2) 1.8)] latch)))

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

(def switch-shell
  (round-cube shell-corner-offset shell-corner-offset thickness 1.5))

(defn get-position [index]
  (let [offset [0 2 1 -2]
        column (mod index columns)
        row (Math/floor (/ index columns))]
    [(* column boxSize) (+ (* row (- boxSize 0.5) -1) (get offset column) (if (= index 6) -1.5 0)) 0  0 0 (if (= index 6) 0.15 0)]))

(def switch-positions (map get-position (range switch-count)))

(defn get-switch-group [form] (union (map #(translate (take 3 %) (rotate (drop 3 %) form)) switch-positions)))

(def switch-cutouts (get-switch-group switch-holder-cutout))
(def switch-shells (get-switch-group switch-shell))

(def connector-end
  (union (cylinder [1.2 0] 1.7) (translate [0 0 -1] (cylinder 1.2 0.9))))

(defn get-connector [start end]
  (hull
   (translate start connector-end)
   (translate end connector-end)))

(def vertical-matches [[4 1]])
(def horizontal-matches [[2 7] [3 6]])

(def vertical-connectors
  (map
   (fn [[index [from to]]] [[index from] [(+ index columns) to]])
   (cart (range (- switch-count columns)) vertical-matches)))

(def horizontal-connectors
  (map (fn [[index [from to]]] (vector [index from] [(inc index) to]))
       (cart
        (filter #(pos? (mod  (+ % 1) columns)) (range (- switch-count 1)))
        horizontal-matches)))

(defn get-juncture [[index connector-id]]
  (let [[x y z rx ry rz] (get-position index)
        base (/ switch-min-width 2)
        offsetsX [-6 0]
        offsetsY [-4.5 6]
        baseX (into [] (concat offsetsX (take 2 (repeat base)) (reverse offsetsX) (take 2 (repeat (* base -1)))))
        baseY (into [] (concat (take 2 (repeat base)) (reverse offsetsY) (take 2 (repeat (* base -1))) offsetsY))
        baseZ (+ (/ thickness -2) 2.2)
        [a b c] (rotate-point (get baseX connector-id) (get baseY connector-id) baseZ rx ry rz)]
    [(+ a x) (+ b y) (- c z)]))

(def get-connectors #(map (fn [[from to]] (get-connector (get-juncture from) (get-juncture to))) %))

(def controller-holder
  (translate
   [-20.7 -9 0]
   (difference
    (round-cube 23 37 thickness 1.3)
    (translate [0 1.3 1.5] (fuzzy-cube 18.5 35.0 1.8 1.6))
    (translate [0 3.5 2.25] (cube 16.9 37.8 1.6))
    (translate [5.5 -0.25 -1.2] (cube 6.2 30.5 (- thickness 1.87)))
    (translate [-5.5 -0.25 -1.2] (cube 6.2 30.5 (- thickness 1.87)))
    (translate [0 -0.25 -1.2] (cube 7 22 (- thickness 1.87))))))

(def keyboard
  (difference
   (union switch-shells controller-holder)
   (get-connector (get-juncture [columns 6]) [-15 -21 0])
   (get-connector (get-juncture [columns 7]) [-15 -12 0])
   (get-connector (get-juncture [0 7]) [-15 4 0])
   (get-connector (get-juncture [0 6]) [-15 -6 0])
   (get-connectors vertical-connectors)
   (get-connectors horizontal-connectors)
   switch-cutouts))

(def tool
  (let
   [z 2.0]
    (union
     (translate [0.8 3.9 1] (binding [*fn* 15] (cylinder 0.8 4)))
     (difference
      (fuzzy-cube 12 10 z 0.6)
      (translate [0 -0.5 0.6] (cube 1.9 4.0 (- z 0.6)))
      (translate [0 0 0.8] (cube 0.5 20 (- z 0.6)))
      (translate [0 3.8 1.0] (fuzzy-cube 30 3 2 0.6))))))

(spit "gemini.scad" (write-scad keyboard))
(spit "tool.scad" (write-scad tool))
