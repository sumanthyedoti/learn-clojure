;; comp
(defn my-comp
      [& fns]
      (fn [& args]
       (if (nil? (second fns))
         (apply (first fns) args)
         ((first fns) (apply (apply my-comp (rest fns)) args)))))
((my-comp inc *) 2 3 4) ; 25
((comp inc *) 2 3 4) ; 25
