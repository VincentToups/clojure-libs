(ns toups.functional)

(defn drop-in [sentinal sq subs]
  (loop [out '() 
		 rst-sq sq
		 rst-subs subs]
	(cond (empty? rst-sq) (reverse out)
		  :else
		  (let [head (first rst-sq)]
			(if (= head sentinal)
			  (recur (cons (first rst-subs) out)
					 (rest rst-sq)
					 (rest rst-subs))
			  (recur (cons (first rst-sq) out)
					 (rest rst-sq)
					 rst-subs))))))

(defn plx [sent f & args]
  (if (not (keyword? sent))
	(apply plx (cons :_ (cons sent (cons f args))))
	(fn [& reduced-args]
	  (apply f (drop-in sent args reduced-args)))))
			
