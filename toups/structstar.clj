(ns toups.structstar (:use toups.misc toups.list))
(in-ns 'toups.structstar)

(defn names-of [parts]
  (map (fn [part]
		 (if (coll? part)
		   (first part)
		   part))
	   parts))

(defn init-vals-of [parts]
  (map (fn [part]
		 (if (coll? part)
		   (second part)
		   'nil))
	   parts))

(defn name-of [name]
  (if (coll? name)
	(first name)
	name))

(defn parents-of [name]
  (if (coll? name)
	(rest name)
	()))

(def *struct-info* (ref {}))

(defn add-struct-info [name parents defaults]
  (dosync (ref-set *struct-info* (assoc @*struct-info* name {:parents parents :defaults defaults}))))

(defn produce-default-value 
  ([name element]
	 (let [search-list (cons name (:parents (@*struct-info* name)))]
	   (loop [left search-list]
		 (if (empty? left) nil
			 (do 
			   (let [current (first left)
					 left (rest left)
					 purported-value (element (:defaults (@*struct-info* current)))]
				 (if purported-value (purported-value)
					 (recur left))))))))
  ([name element mp]
	 (binding [*struct-info* mp]
	   (produce-default-value name element))))

(defn kw-or-error [x]
  (if (keyword? x) x (throw (new Exception "Expected a keyword but got something else"))))

(defn parts-to-parts-map [parts]
  (loop [[part & left :as parts] parts
		 mp {}]
	(if (empty? parts) mp
		(if (coll? part)
		  (recur left (assoc mp (kw-or-error (first part)) `(fn [] ~(second part))))
		  (recur left (assoc mp (kw-or-error part) nil))))))

(defn list-if-not-list [x]
  (if (coll? x) x (list x)))

(defn parents-of [names]
  (if (coll? names) (rest names)
	  ()))

(defn all-records-of [name]
  (let [search-list (cons name (:parents (@*struct-info* name)))]
	(loop [[item & rest :as left] search-list
		   kw []]
	  (if (empty? left) kw
		  (recur rest (foldl #(conj %2 %1) kw (keys (:defaults (@*struct-info* item)))))))))

(defn make 
  ([name args]
	 (loop [[key & rst :as keys] (all-records-of name)
			obj args]
	   (if (empty? keys) (assoc obj :object-type (keyword (str name)))
		   (recur (rest keys)
				  (if (key obj) obj
					  (assoc obj key (produce-default-value name key)))))))
  ([name]
	 (make name {})))
			
(defmacro defstruct* [name-or-names & parts]
  `(do 
	 (add-struct-info '~(name-of name-or-names) '~(parents-of name-or-names) ~(parts-to-parts-map parts))
	 (defn ~(sym-cat 'make (name-of name-or-names)) 
	   ([mp#]
		  (make '~(name-of name-or-names) mp#))
	   ([] 
		  (make '~(name-of name-or-names) {})))))
