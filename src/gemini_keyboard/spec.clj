(ns gemini-keyboard.spec
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]))

(def switch-min-width 13.9)
(def switch-max-width 15.5)
(def spacing 2.25)
(def thickness 5.1)
(def latch (cube switch-max-width 4.9 0.7))
(def boxSize (+ switch-min-width (* spacing 2)))
(fs! 120)
(def rows 2)
(def columns 3)
(def switch-count (* rows columns))

(def shell-corner-offset (+ switch-max-width 2.5))
(def switch-holder-cutout
  (union
   (cube switch-min-width switch-min-width thickness)
   (hull
    (->> (cube switch-min-width switch-min-width 2.5)
         (translate [0 0 -1]))
    (->> (cube (+ switch-min-width 2.5) (+ switch-min-width 2.5) 3.5)
         (translate [0 0 -3])))
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

(def switch-shell
  (round-cube shell-corner-offset shell-corner-offset thickness 1))

(defn get-position [index]
  (let [offset [0 2 1 -2]
        column (mod index columns)
        row (Math/floor (/ index columns))]
    [(* column boxSize) (+ (* row (- boxSize 0.5)) (get offset column)) 0]))

(def switch-positions (map get-position (range switch-count)))

(def switch-cutouts
  (union (map #(translate % switch-holder-cutout) switch-positions)))

(def switch-shells
  (union (map #(translate % switch-shell) switch-positions)))

(def connector-end
  (union (cylinder [1 0] 1.8) (translate [0 0 -0.5] (cylinder 1 1.2))))

(defn get-connector [start end]
  (hull
   (translate start connector-end)
   (translate end connector-end)))

(def vertical-matches [[14 0] [13 1] [12 2] [11 3] [10 4]])
(def horizontal-matches [[5 19] [6 18] [7 17] [8 16] [9 15]])

(def vertical-connectors
  (map
   (fn [[index [from to]]] [[index from] [(+ index columns) to]])
   (cart (range (- switch-count columns)) vertical-matches)))

(def horizontal-connectors
  (map (fn [[index [from to]]] (vector [index from] [(inc index) to]))
       (cart
        (filter #(pos? (mod  (+ % 1) columns)) (range switch-count))
        horizontal-matches)))

(defn get-juncture [[index connector-id]]
  (let [[x y z] (get-position index)
        base (/ switch-min-width 2)
        offsets [-5.9 -3 0 3 5.9]
        baseX (into [] (concat offsets (take 5 (repeat base)) (reverse offsets) (take 5 (repeat (* base -1)))))
        baseY (into [] (concat (take 5 (repeat base)) (reverse offsets) (take 5 (repeat (* base -1))) offsets))
        baseZ (+ (/ thickness -2) 1.5)]
    [(+ (get baseX connector-id) x) (+ (get baseY connector-id) y) (- baseZ z)]))

(def get-connectors #(map (fn [[from to]] (get-connector (get-juncture from) (get-juncture to))) %))

(defn fuzzy-cube [x y z offset]
  (hull
   (cube x y (- z offset))
   (cube (- x offset) (- y offset) z)))

(def controller-holder
  (translate
   [-20 2.8 (/ thickness -2)]
   (union
    (translate [0 2.7 1.5] (fuzzy-cube 18.9 34 1.8 1.6))
    (translate [0 3.1 2.25] (round-cube 17.9 33.2 1.6 1))
    (translate [5.5 1.75 0] (cube 7 30.5 3))
    (translate [-5.5 1.75 0] (cube 7 30.5 3))
    (translate [0 1.75 0] (cube 7 26 3)))))

(def keyboard
  (union
   controller-holder
   (difference
    (hull switch-shells)
    (get-connectors vertical-connectors)
    (get-connectors horizontal-connectors)
    switch-cutouts)))

(spit "example.scad" (write-scad keyboard))
