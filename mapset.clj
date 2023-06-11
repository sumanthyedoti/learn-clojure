;; a map that returns result in set instead of list
(defn mapset [f data]
    (reduce
     #(conj %1 (f %2))
     #{}
     data))
