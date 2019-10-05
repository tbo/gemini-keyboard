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
(def shell-corner-radius 1)
(def shell-corner (binding [*fn* 5] (sphere shell-corner-radius)))
(def shell-corner-offset (- (+ (/ switch-min-width 2) 2.5) shell-corner-radius))
(def switch-holder-cutout
  (union
   (cube switch-min-width switch-min-width thickness)
   (hull
    (->> (cube switch-min-width switch-min-width 2.5)
         (translate [0, 0, -1]))
    (->> (cube (+ switch-min-width 2.5) (+ switch-min-width 2.5) 3.5)
         (translate [0, 0, -3])))
   (translate [0 (- (/ switch-min-width 2) 3.4) (- (/ thickness 2) 1.8)] latch)

   (translate [0 (+ (/ switch-min-width -2) 3.4) (- (/ thickness 2) 1.8)] latch)))

(def shell-top (- (/ thickness 2) shell-corner-radius))
(def shell-bottom (+ (/ thickness -2) shell-corner-radius))

(def switch-shell
  (union
   (translate [shell-corner-offset shell-corner-offset shell-top] shell-corner)
   (translate [(* shell-corner-offset -1) shell-corner-offset shell-top] shell-corner)
   (translate [shell-corner-offset (* shell-corner-offset -1) shell-top] shell-corner)
   (translate [(* shell-corner-offset -1) (* shell-corner-offset -1) shell-top] shell-corner)
   (translate [shell-corner-offset shell-corner-offset shell-bottom] shell-corner)
   (translate [(* shell-corner-offset -1) shell-corner-offset shell-bottom] shell-corner)
   (translate [shell-corner-offset (* shell-corner-offset -1) shell-bottom] shell-corner)
   (translate [(* shell-corner-offset -1) (* shell-corner-offset -1) shell-bottom] shell-corner)))

(defn get-position [index]
  (let [offset [0 2 1 -2]
        column (mod index columns)
        row (Math/floor (/ index columns))]
    [(* column boxSize) (+ (* row (- boxSize 0.5)) (get offset column))]))

(def switch-positions (map get-position (range switch-count)))

(def switch-cutouts
  (union (map #(translate % switch-holder-cutout) switch-positions)))

(def switch-shells
  (union (map #(translate % switch-shell) switch-positions)))

(def connector-end
  (scale [1 1 1.2] (rotate [0 (/ Math/PI 2) 0] (binding [*fn* 5] (sphere 1)))))

(def horizontal-matches [[5, 19], [6, 18], [7, 17], [8, 16], [9, 15]])

(defn cart [& lists]
  (let [syms (for [_ lists] (gensym))]
    (eval `(for [~@(mapcat #(list %1 `'~%2) syms lists)]
             (list ~@syms)))))

(defn get-connector [start end]
  (hull
   (translate start connector-end)
   (translate end connector-end)))

(def horizontal-connectors (cart (range switch-count) horizontal-matches))
(println horizontal-connectors)

(def keyboard
  (union
   (get-connector [0 0 0] [0 50 0])
   (difference
    (hull switch-shells)
    switch-cutouts)))
(spit "example.scad" (write-scad keyboard))
