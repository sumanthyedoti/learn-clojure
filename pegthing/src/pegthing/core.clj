(ns pegthing.core
  (:gen-class))

(declare successful-move prompt-move game-over query-rows)

(defn tri*
  "Generate lazy sequence of triangluar numbers"
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
        (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(defn triangular?
  "Is the number triangular? e.g. 1 3 6"
  [n] (= n (last (take-while #(>= n %) (tri*)))))

(defn row-tri
  "nth (row number) triangular number"
  [n]
  (last (take n (tri*))))

(defn row-num
  "Returns row number that the position belongs to"
  [pos]
  (inc (count (take-while #(> pos %) (tri*)))))

(defn connect
  "Form a mutual connection between two positions"
  [board size pos neighbor dest]
  (if (every? #(<= % (row-tri size)) [pos neighbor dest])
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbor))
            board
            [[pos dest] [dest pos]])
    board))

(defn connect-right
  [board size pos]
  (let [neighbor (inc pos)
        dest (inc neighbor)]
    (if-not (or (triangular? neighbor) (triangular? pos))
      (connect board size pos neighbor dest)
      board)))

(defn connect-down-left
  [board size pos]
  (let [row (row-num pos)
        neighbor (+ row pos)
        dest (+ 1 row neighbor)]
    (connect board size pos neighbor dest)))

(defn connect-down-right
  [board size pos]
  (let [row (row-num pos)
        neighbor (+ 1 row pos)
        dest (+ 2 row neighbor)]
    (connect board size pos neighbor dest)))

(defn add-pos
  "Add peg to the position and create connections"
  [board size pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board, connecting-fn]
              (connecting-fn new-board size pos))
            pegged-board
            [connect-right connect-down-left connect-down-right]) ))

(defn new-board
  "Create a new board with the given number number rows, SIZE"
  [size]
  (let [initial-board {:rows size}
        max-pos (row-tri size)]
    (reduce (fn [board pos] (add-pos board size pos))
            initial-board
            (range 1 (inc max-pos)))))

(defn pegged? [board pos] (get-in board [pos :pegged]))
(defn remove-peg [board pos] (assoc-in board [pos :pegged] false))
(defn place-peg [board pos] (assoc-in board [pos :pegged] true))

(defn valid-moves
  "Return valid moves for the give position"
  [board pos]
  (if (get-in board [pos :pegged])
    (into {}
        (filter (fn [[dest jumped]]
                  (and (not (pegged? board dest)) (pegged? board jumped)))
                (get-in board [pos :connections])))'
    {}))

(defn move-peg
  "Take peg out of pos1 and place it in pos2"
  [board pos dest]
      (place-peg (remove-peg board pos) dest))

(defn make-move
  "Move peg from p1 to p2, removing jumped peg"
  [board p1 p2]
  (if-let [jumped (get (valid-moves board p1) p2)]
    (move-peg (remove-peg board jumped) p1 p2)
    nil))

(defn can-move?
  "Can any pegged positions move?"
  [board]
  (let [positions (map #(first %) board)]
    (some #(not-empty (valid-moves board %)) positions)))

;;;;;
;; Render board textually and print it
;;;;;
(def alpha-start 97)
(def alpha-end 122)
(def letters (map (comp str char) (range alpha-start (inc alpha-end))))
(def pos-chars 3)

(def ansi-styles
  {:red   "[31m"
   :green "[32m"
   :blue  "[34m"
   :reset "[0m"})

(defn ansi
  "Produce a string which will apply an ansi style"
  [style]
  (str \u001b (style ansi-styles)))

(defn colorize
  "Apply ansi color to text"
  [text color]
  (str (ansi color) text (ansi :reset)))

(defn row-positions
  "Return all positions in the given row"
  [n]
  (range (inc (or (row-tri (dec n)) 0))
         (inc (row-tri n))))

(defn render-pos
  [board pos]
  (str (nth letters (dec pos))
       (if (get-in board [pos :pegged])
         (colorize "0" :blue)
         (colorize "-" :red))))


(defn row-padding
  "Spaces at the beginning of a row"
  [row-num rows]
  (let [pad-length (/ (* (- rows row-num) pos-chars) 2)]
    (apply str (take pad-length (repeat " ")))))

(defn render-row
  [board row-num]
  (str (row-padding row-num (:rows board))
       (clojure.string/join " " (map
                            (partial render-pos board)
                            (row-positions row-num)))))

(defn print-board
  [board]
  (doseq [row-num (range 1 (inc (:rows board)))]
    (println (render-row board row-num))))

(defn letter->pos
  "convert position letter to number"
  [letter]
  (inc (- (int (first letter)) alpha-start)))

(defn get-input
  "Clean the user input"
  ([] (get-input nil))
  ([default]
   (let [input (clojure.string/trim (read-line))]
     (if (empty? input)
       default
       (clojure.string/lower-case input)))))

(defn characters-as-strings
  "Given a string, return a collection consisting of each individual
  character"
  [string]
  (re-seq #"[a-zA-Z]" string))

(defn user-entered-valid-move
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board)))

(defn user-entered-invalid-move
  [board]
  (println "\n !! Invalid move !!\n")
  (prompt-move board))

(defn prompt-move
  [board]
  (print-board board)
  (println "Enter two letters of positions to move peg")
  (let [input (map letter->pos (characters-as-strings (get-input)))
        p1 (first input)
        p2 (second input)]
    (if-let [new-board (make-move board p1 p2)]
      (user-entered-valid-move new-board)
      (user-entered-invalid-move board))))

(defn prompt-empty-peg
  [board]
  (println "Here is your board:")
  (print-board board)
  (println "Remove which peg [e]")
  (prompt-move (remove-peg board (letter->pos (get-input "e")))))

(defn prompt-rows
  []
  (println "How many rows? [5]")
  (let [rows (Integer. (get-input 5))
        board (new-board rows)
        max-board-position (- alpha-end alpha-start)]
    (if (> (row-tri rows) max-board-position)
      (do
        (println "Board size is too big. Max size is " (dec (row-num max-board-position)))
        (prompt-rows))
     (prompt-empty-peg board))))

(defn game-over
  [board]
  (let [remaining-pegs (count (filter :pegged (vals board)))]
    (println "\nGame over! You had " remaining-pegs " pegs left:")
    (print-board board))
  (println "\nPlay again? y/n [y]")
  (let [input (get-input "y")]
    (if (= input "y")
      (prompt-rows)
      (do
        (println "Bye!")
        (System/exit 0)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (prompt-rows))
