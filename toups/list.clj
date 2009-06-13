(ns toups.list)
(in-ns 'toups.list)

(defn foldl [f init sq]
  (cond (empty? sq) init
		:else (recur f (f (first sq) init) (rest sq))))
(defn foldr [f init sq]
  (cond (empty? sq) init
		:else (f (first sq) (foldr f init (rest sq)))))

(defn seq-able? [s]
  (try (seq s) (catch Exception e false)))

(defn first-or [lst & [alt]]
	(cond (seq-able? lst) (first (seq lst))
		  :else alt))
(defn second-or [lst & [alt]]
  (cond (and (seq-able? lst) (>= (count lst) 2)) (second (seq lst))
		:else alt))

(defn bunch [sq by]
  (let [n (count sq)]
	(loop [out '() in sq]
	  (cond (empty? in) (reverse out)
			:else 
			(recur 
			 (cons (take by in) out)
			 (drop by in))))))

(defn flatten [sq]
  (reverse (foldl
   (fn [it ac]
	 (cond 
	  (seq? it)
	  (foldl cons ac (flatten it)) ;; NOT tail recursive - bear this in mind.
	  :else
	  (cons it ac))) '() sq)))

(defn flatten-n [sq n]
  (cond (= n 0) sq
		:else
		(reverse (foldl 
		 (fn [it ac]
		   (cond (seq? it)
				 (foldl cons ac (flatten-n it (- n 1)))
				 :else (cons it ac)))
		 ()
		 sq))))

(defn zip [& lsts]
  (apply map (cons list lsts)))

(defn square-zip [sa sb]
  (for [x sa y sb] [x y]))

(defn square-zip-no-diag [sa sb]
  (for [i (range (count sa)) j (range (count sb)) :when (not (= i j))] [ (nth sa i) (nth sb j)]))

(defn square-zip-triu [sa sb]
  (for [i (range (count sa)) j (range (count sb)) :when (> j i)] 
	[ (nth sa i) (nth sb j)]))

(defn member-if [f [hd & tl :as sq]]
  (cond 
   (empty? sq) false
   (f hd) sq
   :else (recur f tl)))

(defn any [pred sq]
  (if (member-if pred sq) true false))

(defn unique [sq]
  (keys (foldl 
		 (fn [it ac]
		   (if (nil? (get ac it))
			 (assoc ac it true)
			 ac))
		 {}
		 sq)))

