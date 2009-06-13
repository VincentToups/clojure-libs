(clojure/ns koppa (:use toups.misc toups.list toups.functional))
(in-ns 'koppa)

(defn staff! [name desc base-buy buy-per base-sell sell-per]
  (hash-map
   :name name 
   :desc desc 
   :base-buy base-buy 
   :buy-per buy-per 
   :base-sell base-sell 
   :sell-per sell-per))

(defn jar! [name desc base-buy buy-per base-sell sell-per]
  (hash-map
   :name name 
   :desc desc 
   :base-buy base-buy 
   :buy-per buy-per 
   :base-sell base-sell 
   :sell-per sell-per))

(def *staffs* (list 
				(staff! "Accelerating Staff"
						"Causes target to move faster."
						700
						70
						210
						21)
				(staff! "Bufu's Staff"
						"Changes a monster into meat. Will turn Shiren into a riceball."
						3000
						300
						900
						90)
				(staff! "Doppelganger Staff"
						"Changes target into a fake Shiren and confuses them for a certain period of time. Monsters will attack the fake Shiren instead of the real one (you). If a monster kills the fake Shiren, they will level up."
						1500
						150
						450
						45)
				(staff! "Knockback Staff"
						"Pushes target back 10
spaces and causes 5 damage."
						500
						50
						150
						15)
				(staff! "Lightning Staff"
						"Sends a bolt in the direction your facing, bolt does 20
damage."
						2000
						200
						600
						60)
				(staff! "Pain Sharing Staff"
						"After being hit by this staff, the target will take damage equal to whatever damage it does to any other target."
						1200
						120
						360
						36)
				(staff! "Sealing Staff"
						"Seals away special abilities of your target."
						1500
						150
						450
						45)
				(staff! "Skull Staff"
						"Random effect on the target (Sleep, confusion, warp). Occasional drop from Skull Mage family."
						300
						30
						90
						9)
				(staff! "Staff of Happiness"
						"Increases the target's level. Will give Shiren 500 EXP."
						500
						50
						150
						15)
				(staff! "Staff of Misfortune"
						"Decreases the target's level."
						500
						50
						150
						15)
				(staff! "Staff of Paralysis"
						"Paralyzes the target."
						1500
						150
						450
						45)
				(staff! "Staff of Postpone"
						"Sends the target to the stairs while paralyzed."
						500
						50
						150
						15)
				(staff! "Staff of Recovery"
						"Restores the target's HP. (Does 50
damage to ghost-type monsters.)"
						1200
						120
						360
						36)
				(staff! "Staff of Sloth"
						"Causes target to move slower."
						700
						70
						210
						21)
				(staff! "Staff of Stability"
						"Keeps you from falling over if in your inventory. No effect when swung."
						1200
						120
						360
						36)
				(staff! "Switching Staff"
						"Switches your position with your target."
						700
						70
						210
						21)))

(def *jars* (list
			  (jar! "Bottomless Jar"
					"Items put inside will disappear forever. Breaking this pot or using an Extraction Scroll on it will cause Pitfall Traps to appear."
					2500
					250
					750
					75)
			  (jar! "Chiropractic Jar"
					"Pressing this jar will recover all your HP and some status effects."
					1500
					150
					450
					45)
			  (jar! "Gaibara's Jar"
					"An expensive Jar."
					15000
					1500
					4500
					450)
			  (jar! "Jar of Change"
					"Items placed inside will change into something else. Will not work on unpurchased items."
					1200
					120
					360
					36)
			  (jar! "Jar of Hiding"
					"You can hide in this jar or you can throw it at monsters to trap it inside."
					1200
					120
					360
					36)
			  (jar! "Jar of Holding"
					"Can freely insert and remove from this Jar."
					1200
					120
					360
					36)
			  (jar! "Jar of Identity"
					"Any unidentified items put inside will become identified."
					3000
					300
					900
					90)
			  (jar! "Melding Jar"
					"Putting in items of the same type will cause them to be combined. For weapons and shields, the first item becomes the base the second item will combine with the base. For staffs, two of the same will combine the number of uses."
					5000
					500
					1500
					150)
			  (jar! "Monster Jar"
					"Pressing this jar will cause monsters to surround you. When thrown, monsters will be confused."
					1500
					150
					450
					45)
			  (jar! "Pointless Jar"
					"Has no special properties."
					1200
					120
					360
					36)
			  (jar! "Storehouse Jar"
					"Items put in this jar will disappear and be sent to the warehouse in Canyon Hamlet."
					2500
					250
					750
					75)
			  (jar! "Strengthening Jar"
					"Weapons, shields, staffs or gitans inside this jar will increase in level as you enter further floors."
					10000
					1000
					3000
					300)
			  (jar! "Weakening Jar"
					"Weapons, shields, staffs or gitans inside this jar will decrease in level as you enter further floors."
					10000
					1000
					3000
					300)
			  (jar! "Unbreakable Jar"
					"Will bounce off a wall if you throw it then disappear."
					3000
					300
					900
					90)
			  (jar! "Venting Jar"
					"Throwing this jar will cause it to explode upon impact. Any items inside will be lost."
					10000
					1000
					3000
					300)
			  (jar! "Walrus Jar"
					"A Thiefwalrus comes out in the direction your facing and will steal any items, except for other jars. Breaking this jar will release Thiefwalrus(es) in a Confused state."
					1500
					150
					450
					45)))

(defn jar-by-name [name]
  (let [res
		(first (member-if (fn [it] (= name (:name it))) *jars*))]
	(if res res (throw (new java.lang.Error (format "Could not find jar : %s" name))))))

(defn staff-by-name [name]
  (let [res
		(first (member-if (fn [it] (= name (:name it))) *staffs*))]
	(if res res (throw (new java.lang.Error (format "Could not find staff : %s" name))))))


(defn jar-cost-buying [name charges]
  (let [jar (jar-by-name name)]
	(+ (:base-buy jar) (* (:buy-per jar) charges))))

(defn consistent-jar-buy? [jar price spots]
  (let [base (:base-buy jar)
		per (:buy-per jar)]
	(= (- price (* spots per)) base)))

(defn find-consistent-jars-buy [price spots]
  (filter (plx consistent-jar-buy? :_ price spots) *jars*))

(defn jar-cost-selling [name charges]
  (let [jar (jar-by-name name)]
	(+ (:base-sell jar) (* (:sell-per jar) charges))))

(defn consistent-jar-sell? [jar price spots]
  (let [base (:base-sell jar)
		per (:sell-per jar)]
	(= (- price (* spots per)) base)))

(defn find-consistent-jars-sell [price spots]
  (filter (plx consistent-jar-sell? :_ price spots) *jars*))


;;;

(defn staff-cost-buying [name charges]
  (let [staff (staff-by-name name)]
	(+ (:base-buy staff) (* (:buy-per staff) charges))))
 
(defn consistent-staff-buy? [staff price spots]
  (let [base (:base-buy staff)
		per (:buy-per staff)]
	(= (- price (* spots per)) base)))

(defn consistent-staff-buy-in-range? [staff price range]
  (foldl
   (fn [it ac]
	 (or (consistent-staff-buy? staff price it) ac))
   false
   range))

(defn find-consistent-staffs-buy [price & [rng]]
  (let [rng (if rng rng [3 6])]	
	(let [rng (apply range (seq rng))]
	  (filter (plx consistent-staff-buy-in-range? :_ price rng) *staffs*))))

(defn staff-cost-selling [name charges]
  (let [staff (staff-by-name name)]
	(+ (:base-sell staff) (* (:sell-per staff) charges))))

(defn consistent-staff-sell? [staff price spots]
  (let [base (:base-sell staff)
		per (:sell-per staff)]
	(= (- price (* spots per)) base)))

(defn consistent-staff-sell-in-range? [staff price range]
  (foldl
   (fn [it ac]
	 (or (consistent-staff-sell? staff price it) ac))
   false
   range))

(defn find-consistent-staffs-sell [price & [rng]]
  (let [rng (if rng rng [3 6])]	
	(let [rng (apply range (seq rng))]
	  (filter (plx consistent-staff-sell-in-range? :_ price rng) *staffs*))))

(defn find-consistent-jars 
  ([price slots] (find-consistent-jars price slots :buying))
  ([price slots how]
	 (let [how (if (map? how) (:how how) how)]
	   (case how
			 :buying (find-consistent-jars-buy price slots)
			 :selling (find-consistent-jars-sell price slots)
			 :else (list "Unknown condition")))))

(defn find-consistent-staffs
  ([price] (find-consistent-staffs price {:rng [3 6] :how :buying}))
  ([price kw]
	 (let [how (if (map? kw) (:how kw :buying) kw)
		   rng (if (map? kw) (:rng kw [3 6]) [3 6])]
	   (prn how)
	   (prn rng)
	   (case how
			 :buying (find-consistent-staffs-buy price rng)
			 :selling (find-consistent-staffs-sell price rng)
			 :else (list "Unknown condition")))))
