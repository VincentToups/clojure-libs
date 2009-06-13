(ns toups.mat (:use toups.misc toups.list toups.structstar))
(in-ns 'toups.mat)

(defstruct* mat
  (:sz [0 0])
  (:data []))

(defmacro ij [& body] `(fn [~'i ~'j] ~@body))

(defn ->lin [h w i j]
  (+ i (* j h)))

(defn nvec [n]
  (loop [i 0 v []]
	(if (< i n) (recur (+ i 1) (conj v 0))
		v)))

(defn mat->raw [{data :data}]
  data)

(defn mat ([h w initer]
  (make-mat {:data (loop [data [] j 0]
	(if (< j w)
	  (recur 
		(loop [data data i 0]
		  (if (< i h)
			(recur (conj data (initer i j)) (+ i 1))
			data))
		(+ j 1))
	  data))
			:sz [h w]}))
  ([h w]
	 (mat h w (fn [i j] 0))))

(defn prn-mat [{sz :sz data :data :as mat}]
  (let [[h w] sz]
	(loop [i 0]
	  (if (< i h)
		(do 
		  (loop [j 0]
			(if (< j w)
			  (do
				(print "\t\t" (data (->lin h w i j)))
				(recur (+ j 1)))
			  true))
		  (print "\n")
		  (recur (+ i 1)))
		true))))

(defn mat-get 
  ([{sz :sz data :data :as mat} i j]
	 (let [[h w] sz]
	   (data (->lin h w i j))))
  ([{sz :sz data :data :as mat} [i j]]
	 (mat-get mat i j)))

(defn mat-set
  ([{sz :sz data :data :as mat} i j v]
	 (let [[h w] sz]
	   (assoc mat :data (assoc data (->lin h w i j) v))))
  ([{sz :sz data :data :as mat} [i j] v]
	 (mat-set mat i j v)))

(defn get-or [v k o]
  (if (< k (count v)) (v k) o))

(defn nest-get [vov i j o]
  (get-or (vov i) j o))

(defn mat-lit [vov]
  (let [h (count vov)
		w (apply max
				 (map count vov))]
	(mat h w
	   (fn [i j]
		 (nest-get vov i j 0)))))

(defn sub-mat-or [{ [h w] :sz :as m} ii jj la]
  (let [ii (vec ii)
		jj (vec jj)]
	(mat (count ii) (count jj)
		 (fn [i j]
		   (let [ei (ii i)
				 ej (jj j)]
			 (if (and (>= ei 0)
					  (< ei h)
					  (>= ej 0)
					  (< ej w))
			   (mat-get m ei ej)
			   (la ei ej)))))))
			 
  
(defn sub-mat [m ii jj]
  (let [ii (vec ii)
		jj (vec jj)]
	(mat (count ii) (count jj)
		 (fn [i j] (mat-get m (ii i) (jj j))))))

(defn map-mat [f & ms]
  (let [[h w] (:sz (first ms))]
	(mat h w
		 (fn [i j]
		   (apply f
				  (map #(mat-get % i j) ms))))))

(defn map-mat-ij [f & ms]
  (let [[h w] (:sz (first ms))]
	(mat h w
		 (fn [i j]
		   (apply f i j
				  (map #(mat-get % i j) ms))))))

(defn for-each-mat [f & ms]
  (let [[h w] (:sz (first ms))]
	(loop-if 
		[j 0]
		($ j < w)
		nil
	  (loop-if
		  [i 0]
		  ($ i < h)
		  nil
		(apply f (map #(mat-get % i j) ms))
		(recur (+ i 1)))
	  (recur (+ j 1)))))

(defn for-each-mat-ij [f & ms]
  (let [[h w] (:sz (first ms))]
	(loop-if 
		[j 0]
		($ j < w)
		nil
	  (loop-if
		  [i 0]
		  ($ i < h)
		  nil
		(apply f i j (map #(mat-get % i j) ms))
		(recur (+ i 1)))
	  (recur (+ j 1)))))

(defn transpose [{sz :sz :as m}]
  (mat (second sz) (first sz)
	   (fn [i j]
		 (mat-get m j i))))

(defn revi [n i]
  (- (- n 1) i))

(defn fliplr [{sz :sz :as m}]
  (mat (first sz) (second sz)
	   (fn [i j] (mat-get m i (revi (second sz) j)))))

(defn flipud [{sz :sz :as m}]
  (mat (first sz) (second sz)
	   (fn [i j] 
		 (mat-get m (revi (first sz) i) j))))

(defn rem-neg [n b]
  (if (> n 0) (rem n b)
	  (let [shift (* (Math/ceil (/ (Math/abs n) b)) b)]
		(rem (+ n shift) b))))

(defn rot90 [m k]
  (let [r (rem-neg k 4)]
   (cond
	 (= r 0) m
	 (= r 1) (flipud (transpose m))
	 (= r 2) (flipud (fliplr m))
	 (= r 3) (transpose (flipud m)))))
		
	 
