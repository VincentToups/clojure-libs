(ns toups.combinators)
(in-ns 'toups.combinators)

(defn cleave
  "applies n lambdas to the final arguement, return a list of results."
  [& args]
  (cond 
	(and (= 2 (count args))
		 (list? (first args)))
	(do 
	  (let [arg (second args)]
		(map #(% arg) (first args))))
	:else
	(do
	  (let [fs (reverse (rest (reverse args)))
			arg (first (reverse args))]
		(map #(% arg) fs)))))
