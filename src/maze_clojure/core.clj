(ns maze-clojure.core
  (:gen-class))
(declare create-maze)
(def size 10)

(defn create-rooms []
 (vec
  (for [row (range size)]
   (vec
    (for [col (range size)]
      {:row row, :col col, :visited? false,
       :bottom? true, :right? true :start? false, :end? false})))))


(defn possible-neighbors [rooms row col]
 (vec
  (filter
    (fn [room]
      (and room (= false (:visited? room))))
    [(get-in rooms [(dec row) col])
     (get-in rooms [(inc row) col])
     (get-in rooms [row (dec col)])
     (get-in rooms [row (inc col)])])))

(defn random-neighbor [rooms row col]
  (let [neighbors (possible-neighbors rooms row col)]
    (if (pos? (count neighbors))
      (rand-nth neighbors)
      nil)))
(defn tear-down-wall [rooms old-row old-col new-row new-col]
  (cond
    ; going up
    (< new-row old-row)
    (assoc-in rooms [new-row new-col :bottom?] false)
    ; going down
    (> new-row old-row)
    (assoc-in rooms [old-row old-col :bottom?] false)
    ; going left
    (< new-col old-col)
    (assoc-in rooms [new-row new-col :right?] false)
    ; going right
    (> new-col old-col)
    (assoc-in rooms [old-row old-col :right?] false)))

(defn endpoint-reached [rooms]
  (>(count
      (set
        (for [row rooms
              room row]
          (:end? room)))) 1))

;(defn create-maze-loop [rooms old-row old-col new-row new-col]
;  (let [new-rooms (tear-down-wall rooms old-row old-col new-row new-col)
;        new-rooms (create-maze new-rooms new-row new-col)
;    (if (= rooms new-rooms)
;      rooms
;     (create-maze-loop new-rooms old-row old-col new-row new-col)))

(defn has-no-end? [rooms]
  (= 1 (count (set (map :end? (flatten rooms))))))


(defn create-maze [rooms row col first?] 
  (let [rooms       (assoc-in rooms [row col :visited?] true) 
        next-room   (random-neighbor rooms row col)    
        rooms       (if (and (nil? next-room) (not (endpoint-reached rooms)))
                      (assoc-in rooms [row col :end?] true)
                      rooms)
        rooms (if first? (assoc-in rooms [row col :start?] true)   
                rooms)]
    (if next-room      
      (loop [old-rooms (tear-down-wall rooms row col (:row next-room) (:col next-room))]
        (let [new-rooms (create-maze old-rooms (:row next-room) (:col next-room) false)]
          (if (= old-rooms new-rooms)
            old-rooms            
            (recur new-rooms))))
      rooms)))


(defn -main []
  (let [rooms (create-rooms)
        ;rooms (assoc-in rooms [0 0 :start?] true)
        rooms (create-maze rooms 0 0 true)]
    (doseq [_ rooms]
      (print " _"))
    (println)
    (doseq [row rooms]
      (print "|")
      (doseq [room row]
        (cond
          (:start? room)
          (print "o")
          (:end? room)
          (print "x")
          :else        
          (if (:bottom? room)
            (print "_")
            (print " ")))
        (if (:right? room)
          (print "|")
          (print " ")))
      (println))))
