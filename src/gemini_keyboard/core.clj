(ns gemini-keyboard.core
  (:require [gemini-keyboard.spec :refer [write-models]]))

(defn -main
  "Generates keyboard models"
  []
  (write-models)
  (printf "Generated keyboard models"))
