(ns trivial.core6)

(.log js/console (->> (map inc (range 10)) (filter even?) (partition 2) (drop 1) (mapcat identity) into-array)})
