(ns toups.misc (:use toups.list))
(in-ns 'toups.misc)

(defn do1 [& rest]
  (first rest))

(defn != [a b] (not (= a b)))

(defn rfirst [sq] (rest (first sq)))
(defn rrest [sq] (rest (rest sq)))
(defn frest [sq] (first (rest sq)))

(defmacro loop-if [bindings cond ret & body]
  `(loop ~bindings
	 (if ~cond (do ~@body) ~ret)))

(defmacro $ [ first f & rest ] `(~f ~first ~@rest))

(defn pairs-to-map [& pairs]
  (apply array-map pairs))


(defn foreach [f & cols]
  (loop [args (apply zip cols)]
	(if (not (empty? args)) (do (apply f (first args)) (recur (rest args))) nil)))

(defn std-in []
  (new java.io.BufferedReader (new java.io.InputStreamReader (. System in))))

;; (defn read-line
;;   ([] (recur (std-in)))
;;   ([buf-read] (. buf-read readLine)))

(defn open-read [filename]
  (new java.io.BufferedReader (new java.io.FileReader (new java.io.File filename))))

(defn deref-or [ref & [else]]
  (try
   (deref ref)
   (catch Exception e
	 else)))

(defn get-runtime []
  (. java.lang.Runtime (getRuntime)))

(defn buffered-reader [s]
  (new java.io.BufferedReader (new java.io.InputStreamReader s)))

(defn sub-proc-decompose [p]
  {:process p
   :in (. p (getInputStream))
   :out (. p (getOutputStream))
   :err (. p (getErrorStream))})

(defn sub-process 
  ([cmd env-vars]
	 (sub-proc-decompose (. (get-runtime) (exec cmd (to-array env-vars)))))
  ([cmd]
	 (sub-proc-decompose (. (get-runtime) (exec cmd (make-array java.lang.String 0))))))

(defn case-has-else-clause? [pairs]
  (let [r (member-if #(= (first %) :else) pairs)]
	(if r
	  (first (rfirst r))
	  nil)))
  
(defmacro case [expr & pairs]
  (let [pairs (bunch (seq pairs) 2)
		else-val (case-has-else-clause? pairs)
		val-sym (gensym)]
	`(let [~val-sym ~expr]
	   (cond 
	   ~@(flatten-n (map 
					 (fn [pair]
					   (list `(= ~val-sym ~(first pair))
							 (second pair))
					   )
						 (filter #(not= (first %) :else) pairs)) 1)
	   :else ~else-val))))
	     
(defn lorem 
  ([n]
	 (. (new de.svenjacobs.loremipsum.LoremIpsum) (getWords n)))
  ([n m]
	 (. (new de.svenjacobs.loremipsum.LoremIpsum) (getWords n m))))
 
(def *random* (new java.util.Random))
(defn random
  ([] (. *random* (nextDouble)))
  ([n] (. *random* (nextInt n)))
  ([n m] (+ n (. *random* (nextInt (- m n))))))
(defn seed
  ([] (. *random* (setSeed (. (. java.util.Calendar (getInstance)) (getTimeInMillis)))))
  ([n] (. *random* (setSeed n))))

(defn round [number]
  (if (= (class number) (class 10.0))
	(. number longValue)
	number))

(defn print-return [x]
  (prn x)
  x)

(defn sym->kw [sym]
  (keyword (format "%s" sym)))

(defn cwd [& [dir]]
  (if dir (do (. System (setProperty "user.dir" dir))
			  (cwd))
	  (. System (getProperty "user.dir"))))
	  

(defn sym-cat [& strs]
  (if (not (keyword? (first strs)))
	(let [sep "-"
		  rev (reverse strs)]
	  (symbol (apply str (reverse (cons (first rev) (map (fn [x] (str x sep)) (rest rev)))))))
	(let [sep (second strs)
		  rev (reverse (rrest strs))]
	  (symbol (apply str (reverse (cons (first rev) (map (fn [x] (str x sep)) (rest rev)))))))))

(defn md5-bytes [text]
  (let [digest (. java.security.MessageDigest (getInstance "MD5"))]
	(. digest (update (. text (getBytes))))
	(. digest (digest))))

(defn md5 [text & [salt]]
  (let [salt (if salt salt "")]
	(. (foldl
		(fn [it ac]
		  (. ac (append (. Integer (toHexString (bit-and 0xFF it)))))
		  ac)
		(new java.lang.StringBuffer)
		(seq (md5-bytes (str text salt))))
	   (toString))))

(defn as-int [n]
  (let [rounded (. java.lang.Math (round n))]
	(if (= n rounded) rounded n)))

(defn string->number 
  ([str]
	 (as-int (. java.lang.Double (parseDouble str))))
  ([str otherwise]
	 (try
	  (string->number str)
	  (catch Exception e
		otherwise))))

(defn string< [s1 s2]
  (neg? (. s1 (compareTo s2))))

(def str->num string->number)

(defn error
  "wraps throw for convenient error generation."
  [s]
  (throw (new Exception s)))

(defn cumsum [sq]
  (rest 
   (reverse 
	(reduce
	 (fn [[prev & rest :as ac] it]
	   (cons (+ it prev) ac))
	 (list 0)
	 sq))))
	 

(defn terms->partitions [terms]
  (let [total (reduce + 0 terms)]
	(vec (cumsum (cons 0 (map #(/ % total) terms))))))
	
(defn construct-cond-parts [draw-s 
							partitions-s
							todos]
  (let [n (count todos)]
	(loop-if [i 0 
			  cond-parts '()
			  left-todos todos]
		(< i n) 
		cond-parts
	  (recur 
	   (+ i 1)
	   (cons 
		`(and (>= ~draw-s (~partitions-s ~i))
			  (< ~draw-s (~partitions-s ~(+ i 1))))
		(cons (first left-todos) cond-parts))
	   (rest left-todos)))))


(defmacro weighted-randomly
  "perform one of many actions randomly, weighted by values."
  [& pairs]
  (if (!= (mod (count pairs) 2) 0) (error "randomly needs an even number of forms.")
	  (do 
		(let [pairs (bunch pairs 2)
			  weight-terms (map first pairs)
			  todos (map second pairs)
			  n (count weight-terms)
			  partitions-s (gensym 'partitions)
			  draw-s (gensym 'draw)
			  cond-parts (construct-cond-parts 
						  draw-s 
						  partitions-s 
						  todos)]
		  `(let [~partitions-s (terms->partitions [~@weight-terms])
				 ~draw-s (random)]
			 (cond ~@cond-parts))))))

(defmacro randomly
  "perform one of many actions, purely randomly"
  [& actions]
  (let [draw-s (gensym 'draw)]
	`(let [~draw-s (random ~(count actions))]
	   (cond 
		 ~@(:terms 
			(foldl 
			 (fn [it {i :i terms :terms :as ac}]
			   (assoc ac 
				 :i (+ i 1)
				 :terms (conj terms `(= ~draw-s ~i) it)))
			 {:i 0 :terms []}
			 actions))))))
				  


(comment
  (loop-if [i 0
			counts [0 0 0 0]]
	  (< i 1000)
	  counts
	(recur
	 (+ i 1)
	 (randomly 
	     1 (assoc counts 0 (+ 1 (counts 0)))
		 1 (assoc counts 1 (+ 1 (counts 1)))
		 1 (assoc counts 2 (+ 1 (counts 2)))
		 1 (assoc counts 3 (+ 1 (counts 3))))))

)





				 

