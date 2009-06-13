(clojure/ns koppa-web 
  (:use 
   koppa
   compojure.html 
   compojure.http 
   compojure.jetty 
   toups.structstar 
   toups.misc
   toups.list))

(in-ns 'koppa-web)

(defn seq->html-list
  ([sq] (seq->html-list sq {:type :ul}))
  ([sq {type :type :or {:type :ul}}]
	 (html
	  (foldl
	   (fn [it ac]
		 (conj ac [:li it]))
	   [type]
	   sq))))

(defn how->kw [s]
  (case s
		"Buy" :buying
		"Sell" :selling))

(defn render-koppa
  ([] (render-koppa {}))
  ([params]
	 (html
	  (form-to [POST "/koppa"]
		[:p (label :buy-or-sell "Buy/Sell:")
		 (drop-down :buy-or-sell ["Buy" "Sell"])]
		[:p (label :jar-cost "Jar Cost:")
		 (text-field :jar-cost (get params :jar-cost "0"))]
		[:p (label :jar-slots "Jar Slots:")
		 (text-field :jar-slots (get params :jar-slots "0"))]
		[:p (label :staff-cost "Staff Cost:")
		 (text-field :staff-cost (get params :staff-cost "0"))]
		[:p (submit-button "Submit")])
	  (if (not (empty? params))
		(try
		 (let [jar-cost (str->num (:jar-cost params))
			   jar-slots (str->num (:jar-slots params))
			   staff-cost (str->num (:staff-cost params))]
		   (html
			[:p [:em "Possible Jars"]]
			[:p (seq->html-list (map :name (find-consistent-jars jar-cost jar-slots (how->kw (:buy-or-sell params)))))]
			[:p [:em "Possible Staffs"]]
			[:p (seq->html-list (map :name (find-consistent-staffs staff-cost (how->kw (:buy-or-sell params)))))]))
		 (catch Exception e (html [:p "I didn't understand some of your inputs."])))))))
		   
		   

(defservlet koppa-servlet
  (GET "/*"
	(or (serve-file (route :*))
		:next))
  (GET "/koppa"
	(render-koppa))
  (POST "/koppa"
	(render-koppa params)))

(defserver server {:port 8080} "/*" koppa-servlet)
(start server)
	
