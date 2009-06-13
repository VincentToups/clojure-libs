(ns toups.structstar (:use toups.misc toups.list toups.functional))
(in-ns 'toups.structstar)

(defn get-arg-kws [lst]
  (map (fn [x] (first-or x x)) lst))

(defn get-arg-vals [lst]
  (map second-or lst))

(defn default-merge [de ne] (merge-with (fn [de ne] ne) de ne))
(defn merge-left [de & args]
  (foldl 
   (fn [it ac] 
	 (default-merge ac it))
   de args))

(let [*class-lists* (ref (hash-map))
	  *default-values* (ref (hash-map))]
  (defn get-class-lists [] @*class-lists*)
  (defn add-to-class-list [name sq]
	(dosync (ref-set *class-lists* (assoc @*class-lists* name sq))))
  (defn get-class-list [name]
	(dissoc @*class-lists* name))
  (defn get-defaults []
	@*default-values*)
  
  (defn add-default [name & args]
	(if (= 1 (count args)) 
	  (dosync (ref-set *default-values* (assoc @*default-values* name (first args))))
	  (dosync (ref-set *default-values* (assoc @*default-values* name (apply hash-map args))))))
  (defn remove-default [name]
	(dosync (ref-set *default-values* (dissoc @*default-values* name)))))

(defn construct-defstruct* [names params]
  (if (symbol? names) 
	(construct-defstruct* (list names) params)
	(let [arg-kws (get-arg-kws params)
		  arg-vals (get-arg-vals params)
		  hm-args (flatten-n (zip arg-kws arg-vals) 1)]
	`(do
	   (add-default '~(first names) (hash-map ~@hm-args :*struct*-name* '~(first names)))
	   (add-to-class-list '~(first names) '~(rest names))
	   (defn ~(sym-cat 'make (first names)) [& args#]
		 (let [all-defaults# (map (plx get (get-defaults) :_) '(~@(reverse names)))]
		   (apply merge-left (reverse (cons (apply hash-map args#) (reverse all-defaults#))))))))))
		 
(defmacro defstruct* [names & more]
  (construct-defstruct* names more))

