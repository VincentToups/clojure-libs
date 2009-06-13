(ns toups.smartsort (:use toups.misc toups.functional toups.list))
(in-ns 'toups.smartsort)

(defn digit? [c]
  (some #(= c %) (seq "0123456789")))

(defn not-digit? [c]
  (not (digit? c)))

(defn same-class? [c1 c2]
  (or
	(and (not-digit? c1)
		 (not-digit? c2))
	(and (digit? c1)
		 (digit? c2))))

(defn not-same-class? [c1 c2]
  (not (same-class? c1 c2)))

(defn file-name-parser [next-character 
						[total-ac 
						 element-ac]]
  (cond 
   (nil? (last element-ac)) ;; first call
   (do
	 (list 
	  total-ac ;; should be nil anyway
	  (conj element-ac next-character)))

   (same-class? next-character (last element-ac))
   (do 
	 (list
	  total-ac
	  (conj element-ac next-character)))

   (not-same-class? next-character (last element-ac))
   (do 
	 (list
	  (conj total-ac element-ac)
	  [next-character]))))

(defn parse-number-or-leave [s]
  (str->num s s))

(defn unparse-file [file]
  (foldl
   (fn [it ac]

	 (str ac))
   ""
   file))

(defn parse-file-name [name]
  (let 
	  [[total rem] (foldl
					file-name-parser (list [] []) (seq name))]
	(map (comp parse-number-or-leave #(apply str %)) (conj total rem))))

(defn string-part [pfn]
  (foldl #(str %2 %1) "" (filter string? pfn)))

(defn number-part [pfn]
  (filter number? pfn))

(defn both-strings? [p1 p2]
  (and (string? p1) (string? p2)))

(defn both-numbers? [p1 p2]
  (and (number? p1) (number? p2)))

(defn string-and-number? [p1 p2]
  (and (string? p1) (number? p2)))

(defn number-and-string? [p1 p2]
  (and (string? p2) (number? p1)))

;; (defn numeric-file-< [f1 f2]
;;   (cond 
;;    (nil? f1) true
;;    (nil? f2) false
;;    (not= (first f1) (first f2))
;;    (let [p1 (first f1)
;; 		 p2 (first f2)]
;; 	 (prn "compare")
;; 	 (cond 
;; 	  (both-strings? p1 p2)
;; 	  (string< p1 p2)
;; 	  (both-numbers? p1 p2)
;; 	  (< p1 p2)
;; 	  (string-and-number? p1 p2)
;; 	  (string< p1 (str p2))
;; 	  (number-and-string? p1 p2)
;; 	  (string< (str p1) p2)))
;;    :else
;;    (do (prn "recur")
;; 			(recur (rest f1) (rest f2)))))

(defn number-comp [n1 n2]
  (cond 
   (< n1 n2) -1
   (> n1 n2)  1
   (= n1 n2) 0))

(defn str-comp [s1 s2]
  (. s1 (compareTo s2)))

(defn num&str? [p1 p2]
  (and (number? p1) (string? p2)))

(defn str&num? [p1 p2]
  (and (string? p1) (number? p2)))

(defn numeric-file-comp-inside [ [p1 & rest1 :as f1] [p2 & rest2 :as f2] ]
  (cond
   (= f1 f2) 0
   (nil? p1) -1
   (nil? p2) 1
   (= p1 p2) (recur rest1 rest2)
   (both-numbers? p1 p2) (do (number-comp p1 p2))
   (both-strings? p1 p2) (str-comp p1 p2)
   (num&str? p1 p2) (str-comp (str p1) p2)
   (str&num? p1 p2) (str&num? p1 (str p2))
   :else
   (recur rest1 rest2)))
  
(defn numeric-file-comp [filename-1 filename-2]
  (numeric-file-comp-inside (parse-file-name filename-1) (parse-file-name filename-2)))

(defn group-by-string-part [files]
  (let [files (map parse-file-name files)]
	(foldl
	 (fn [it ac]
	   (assoc ac (string-part it) (cons it (ac (string-part it))) ))
	 {}
	 files)))

(defn sort-files [files]
  (map identity (sort (comparator numeric-file-comp) files)))

(let [s1 "abc10" 
	  s2 "abc1"]
  (prn s1)
  (prn s2)
  (prn (numeric-file-comp s1 s2)))

(def r (let [files (list
			 "catapult-1"
			 "catapult-10"
			 "catapult-13"
			 "file-1"
			 "file-2"
			 "file-3"
			 "file-4"
			 "file-5"
			 "file-6"
			 "file-7"
			 "file-8"
			 "file-9"
			 "file-10"
			 "file-11"
			 "file-12"
			 "file-13-1"
			 "file-13-2"
			 "file-13-10"
			 "file-14"
			 "file-15"
			 "file-16"
			 "file-17"
			 "file-18"
			 "file-19"
			 "file-20"
			 "file-21"
			 "file-22"
			 "file-23"
			 "file-24"
			 "file-25"
			 "file-26"
			 "file-27"
			 "file-28"
			 "file-29"
			 "file-30")]
		 (sort-files files)))

;  (sort (comparator numeric-file-comp) files))
