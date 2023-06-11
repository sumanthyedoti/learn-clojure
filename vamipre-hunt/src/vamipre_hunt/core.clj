(ns vamipre-hunt.core)
(def filename "suspects.csv")

(defn str->int
  [str]
  (Integer. str))

(defn parse
  "Covert csv into rows of columns"
  [string]
  (map #(clojure.string/split % #",") (clojure.string/split string #"\n")))

(def row-keys [:name :glitter-index])
(def conversions {:name identity :glitter-index str->int})

(defn convert
  [key value]
  ((get conversions key) value))

(defn mapify
  "return maps for the rows"
  [rows]
  (map (fn [unmapped-row]
          (reduce (fn [row-map [k v]]
                    (assoc row-map k (convert k v)))
                  {}
                  (map vector row-keys unmapped-row)))
            rows))

(defn glitter-filter [min-glitter vampires]
  (filter #(>= (:glitter-index %) min-glitter) vampires))

(def suspects (mapify (parse (slurp filename))))

(defn validate [suspects keys] (reduce #(and %1 (%2 suspects)) true keys))

(defn add-suspect [suspect]
  (if (validate suspect row-keys) (conj suspects suspect) suspects))

(defn cvs-stringify-suspects [suspects]
  (map (fn [suspect]
         (let [unmapped-row (reduce (fn [vals [_ v]] (conj vals v)) [] suspect)]
           (clojure.string/join #"," unmapped-row)))
       suspects))

(glitter-filter 3 suspects)
