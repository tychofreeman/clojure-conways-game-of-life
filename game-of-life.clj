(import '(javax.swing JFrame))
(import '(java.awt Canvas))
(import '(javax.swing.event MouseInputAdapter))
(import '(java.awt.event KeyAdapter))
(require '[clojure.set :as set])

; In order to reduce the flickering (which is annoying), I think I'll have to replace the Canvas with a Panel with lots of boxes which can be toggled black/white.
; Also, a glider should be able to go off the edge, which it can't yet.

(defn get-point [e]
    [(quot (.getX e) 10) (quot (.getY e) 10)])

(defn toggle-point [xs e]
    (let [p (get-point e)]
      (if (contains? xs p)
        (disj xs p)
        (conj xs p))))


; @clear-xs is an optimization which may not be necessary...

(defn canvas [xs clear-xs max-h max-w paused]
	(let [f (JFrame.)
                c (proxy [Canvas] []
			(paint [g]
				;(doto g (.clearRect 0 0 max-h max-w))
				(doseq [x @clear-xs] (. g clearRect (* 10 (first x)) (* 10 (last x)) 10 10))
				(doseq [x @xs] (. g fillRect (* 10 (first x)) (* 10 (last x)) 10 10))))]
        (. f setSize max-h max-w)
	(. f add c)
	(. f setVisible true)
        (. c addMouseListener (proxy [MouseInputAdapter] []
                                  (mouseClicked [e]
                                                (reset! xs (toggle-point @xs e)))))
        (. c addKeyListener (proxy [KeyAdapter] []
                                  (keyPressed [e]
                                                (reset! paused (not @paused)))))
	c))

(defn same-set [xs ys]
    (and
          (= 0 (count (set/difference xs ys)))
          (= 0 (count (set/difference ys xs)))))


;; Looping 'n' times with state:
(defn game-loop [sleep-time paused clear-xs]
    (fn [xs f n canvas]
        (let [old-xs @xs]
          (Thread/sleep sleep-time)
          (.repaint canvas)
          (when (not @paused)
            (reset! xs (f @xs)))
          (reset! clear-xs old-xs)
          (when (or @paused (not (same-set @xs old-xs)))
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
	(let [n (count (set/intersection world (neighbors p)))]
	(or (= 3 n) (= 2 n))))

(defn should-regen [p world]
	(let [n (count (set/intersection world (neighbors p)))]
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
	(set/union (set (keep-alive world))
		(set (regens world))))

(let [xs (atom #{[0 2] [1 3] [2 3] [2 2] [2 1]})
                paused (atom false)
                clear-xs (atom #{})
		c (canvas xs clear-xs 1000 1000 paused)]
	((game-loop 75 paused clear-xs) xs next-gen 20 c))
