(import '(javax.swing JFrame))
(import '(java.awt Canvas))
(import '(javax.swing.event MouseInputAdapter))

(defn get-point [e]
    [(quot (.getX e) 10) (quot (.getY e) 10)])

(defn canvas [xs max-h max-w]
	(let [f (JFrame.)
                c (proxy [Canvas] []
			(update [g]
				(doto g (.clearRect 0 0 max-h max-w))
				(doseq [x @xs] (. g fillRect (* 10 (first x)) (* 10 (last x)) 10 10))))]
        (. f setSize max-h max-w)
	(. f add c)
	(. f setVisible true)
        (. c addMouseListener (proxy [MouseInputAdapter] []
                                  (mouseClicked [e]
                                                (reset! xs (conj @xs (get-point e))))))
                                  
	c))

(defn same-set [xs ys]
    (and
          (= 0 (count (clojure.set/difference xs ys)))
          (= 0 (count (clojure.set/difference ys xs)))))


;; Looping 'n' times with state:
(defn game-loop [sleep-time]
    (fn [xs f n canvas]
        (let [old-xs @xs]
          (.repaint canvas)
          (reset! xs (f @xs))
          (Thread/sleep sleep-time)
          (when (not (same-set @xs old-xs))
                  (recur xs f (- n 1) canvas)))))

;; Getting into Rules
(defn in-range [p]
	(let [x (first p) y (last p)]
		(and (>= x 0) (>= y 0) (< x 1000) (< x 1000))))

 (defn neighbors [p]
	(let [x (first p) x_1 (- x 1) x1 (+ x 1)
		y (last p) y_1 (- y 1) y1 (+ y 1)]
	(set (filter in-range 
		#{[x_1 y_1] [x_1 y] [x_1 y1] [x y_1] [x y1] [x1 y_1] [x1 y] [x1 y1]}))))


(defn should-live [p world]
	(let [n (count (clojure.set/intersection world (neighbors p)))]
	(or (= 3 n) (= 2 n))))

(defn should-regen [p world]
	(let [n (count (clojure.set/intersection world (neighbors p)))]
	(= 3 n)))

(defn reanimate-candidates [xs]
	(let [ns (set (mapcat neighbors xs))]
		(reduce disj ns xs)))

(defn regens [world]
	(let [candidates (reanimate-candidates world)]
	(filter #(should-regen %1 world) candidates)))

(defn keep-alive [world]
	(filter #(should-live %1 world) world))

(defn next-gen [world]
	(clojure.set/union (set (keep-alive world))
		(set (regens world))))

(let [xs (atom #{[0 2] [1 3] [2 3] [2 2] [2 1]})
		c (canvas xs 1000 1000)]
	((game-loop 150) xs next-gen 20 c))
