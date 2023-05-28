(defn mapset [f data]
    (reduce
     #(conj %1 (f %2))
     #{}
     data))
