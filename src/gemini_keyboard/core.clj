(ns gemini-keyboard.core
  (:use [scad-clj.scad])
  (:use [scad-clj.model]))

;; make primitives
(def primitives
  (union
    (->> (rotate (* Math/PI (/ 0 4)) [0 0 1])
         (->> (cube 5 5 5)
              (rotate (/ Math/PI 4) [1 1 1])
              (translate [150 0 0]))
         )
    (->> (rotate (* Math/PI (/ 1 4)) [0 0 1])
         (->> (cube 10 10 10)
              (rotate (/ Math/PI 4) [1 1 1])
              (translate [150 0 0]))
         )
    (->> (rotate (* Math/PI (/ 2 4)) [0 0 1])
         (->> (cube 20 20 20)
              (rotate (/ Math/PI 4) [1 1 1])
              (translate [150 0 0]))
         )
    (->> (rotate (* Math/PI (/ 3 4)) [0 0 1])
         (->> (cube 30 30 30)
              (rotate (/ Math/PI 4) [1 1 1])
              (translate [150 0 0]))
         )
    (->> (rotate (* Math/PI (/ 4 4)) [0 0 1])
         (->> (cube 40 40 40)
              (rotate (/ Math/PI 4) [1 1 1])
              (translate [150 0 0]))
         )
    )
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (spit "example.scad" (write-scad primitives)))
