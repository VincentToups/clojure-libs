(ns toups.twodarray (:use toups.list toups.misc toups.structstar))
(in-ns 'toups.twodarray)

(defstruct* twodarray (:shape [0 0]) (:data []))
  
(defn init-array [n & [val]]
  (let [val (if val val 0)]
	(loop [array [] i 0]
	  (if (< i n) (recur (conj array val) (+ i 1)) array))))


; 0  5/2
; 1  3/2
; 2  1/2
; 3 -1/2
; 4 -3/2
; 5 -5/2

; 0 -2
; 1 -1
; 2  0
; 3  1
; 4  2

(defn index->coordinate [n i]
  (cond (number? n)
		(cond (even? n)
			  (+ (/ 1 2) (- i (int (/ n 2))))
			  (odd? n)
			  (- i (int (/ n 2))))
		(vector? n)
		[(index->coordinate (first n) (first i))
		 (- (index->coordinate (frest n) (frest i)))]))

(defn coordinate->index [n c]
  (cond (number? n)
		(cond 
		 (odd? n) (+ c (int (/ n 2)))
		 (even? n) (+ (- c (/ 1 2)) (int (/ n 2))))
		(vector? n)
		[(coordinate->index (first n) (first c))
		 (coordinate->index (frest n) (- (frest c)))]))

(defn rotate-coordinate [v theta]
  (let [[x y] v
		s (Math/sin theta)
		c (Math/cos theta)]
	[ (- (* c x) (* s y)) (+ (* s x) (* c y)) ]))

(defn rotate-size [sz]
  (reverse sz))

(defn rotate-index [sz ind theta]
  (map int (coordinate->index sz (rotate-coordinate (index->coordinate sz ind) theta))))

(defn rotate-matrix [m r]
  (let [shape (:shape m)
		[w h] shape
		om (make-twodarray :shape (rotate-size shape)
						   :data (init-array (* w h)))]
	(loop [i 0 om om]
	  (if (< i h)
		(loop [j 0 om om]
		  (if (< j w)
			(loop (+ j 1)
			  (assoc om :data (assoc (index om (rotate-index shape [i j])) (get 
	
(defn cs [n] 
  (cond (even? n)
		(map #(+ (/ 1 2) (- % (int (/ n 2)))) (vec (reverse (range 0 n))))
		(odd? n)
		(map #(- % (int (/ n 2))) (vec (reverse (range 0 n))))))

;(defn ra [sz lam]
;  (make-twodarray :shape sz :data (init-array (prod sz))))


(defn zeros [shape]
  (make-twodarray 
   :shape shape
   :data (init-array (foldl * 1 shape))))

(defn index [array i j]
  (+ i (* (first (:shape array)) j)))

(defn print-twodarray [array]
  (loop [i 0]
	(if (< i (first (:shape array)))
	  (do 
		(loop [j 0]
		  (if (< j (frest (:shape array)))
			(do
			  (printf "%d " (get (:data array) (index array i j)))
			  (recur (+ j 1)))
			()))
		(do 
		  (newline)
		  (recur (+ i 1))))
	  ())))

(defn set2d [array i j val]
  (assoc array :data (assoc (:data array) (index array i j) val)))

(defn get2d [array i j val]
  (get (:data array) (index array i j)))

(defn slice2d [array is js]
  (make-twodarray :shape [(count is) (count js)]
				  :data (foldl 
						 (fn [it ac] (conj ac it))
						 []
						 (map (fn [ix] (apply get2d (cons array ix))) (square-zip is js)))))

(defn map2d [array thunk]
  (assoc array :data
  (foldl (fn [it ac] (assoc ac (apply index (cons array it)) (thunk (get ac (apply index (cons array it))))))
		 (:data array)
		 (unique (square-zip (range 0 (first (:shape array)))
					 (range 0 (frest (:shape array))))))))



	  
