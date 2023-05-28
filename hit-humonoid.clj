((defn -main
   ""
   []
   ))

(def asym-body-parts [{:name "head" :size 3}
                      {:name "eye" :size 1 :sym true}
                      {:name "ear" :size 1 :sym true}
                      {:name "mouth" :size 1}
                      {:name "nose" :size 1}
                      {:name "neck" :size 2}
                      {:name "shoulder" :size 3 :sym true}
                      {:name "upper-arm" :size 3 :sym true}
                      {:name "chest" :size 10 }
                      {:name "back" :size 10}
                      {:name "forearm" :size 3 :sym true}
                      {:name "abdomen" :size 6}
                      {:name "hand" :size 2 :sym true}
                      {:name "knee" :size 2 :sym true}
                      {:name "thigh" :size 4 :sym true}
                      {:name "lower-leg" :size 3 :sym true}
                      {:name "achilles" :size 1 :sym true}
                      {:name "foot" :size 2 :sym true}])

(defn symmetrize-body-part [{:keys [name size sym] :as part} ]
  (if sym
    [{:name (str "left-" name)
      :size size}
     {:name (str "right-" name)
      :size size}]
    [part]))

(defn symmetrize-body-parts [asym-body-parts]
  (reduce (fn [sym-body-parts part]
            (into sym-body-parts (symmetrize-body-part part))) [] asym-body-parts))

(defn hit-humanoid [asym-body-parts]
  (let [sym-body-parts (symmetrize-body-parts asym-body-parts)
        target (rand (reduce
                      (fn [total-size part] (+ total-size (:size part)))
                      0 sym-body-parts))]
    (loop [accumulated-size 0
           [part & remaining] sym-body-parts]
      (if
          (> accumulated-size target) part
          (recur (+ accumulated-size (:size part)) remaining)))))
