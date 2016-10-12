(ns hearts-lecture.utils)

(defn spy [& args]
  (println (apply pr-str args))
  (last args))
