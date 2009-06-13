(require 'cl)

(defun bang (sym)
  (intern (format "%s!" sym)))
(defun* s-cat (sym1 sym2 &key no-dash)
  (if (not no-dash)
	  (intern (format "%s-%s" sym1 sym2))
	(intern (format "%s%s" sym1 sym2))))
(defun ques (sym)
  (intern (format "%s?" sym)))

(defmacro defstruquine (name &rest slots)
  (let* ((n-fields (length slots))
		 (i 1)
		 (out `(progn
				 (defvar ,(s-cat (s-cat '* name :no-dash t) 'slots*) ',slots)
				 (defun ,(bang name) ,slots
				   (list ',(bang name) ,@slots)) 
				 (defun ,(ques name) (item)
				   (eq (car item) ',(bang name))))))
	(loop for slot in slots do
		  (setf out 
				(append out
						(list `(defun ,(s-cat name slot) (item) (elt item ,i)))))
		  (setf i (+ i 1)))
	(append out (list nil))))

(defun* struquine-slot-index (slots slot &optional (i 0))
  (cond 
   ((not slots) nil)
   ((eq slot (car slots)) i)
   (t
	(struquine-slot-index (cdr slots) slot (+ i 1)))))


(defstruquine staff name desc base-buy buy-per base-sell sell-per)
(defstruquine jar name desc base-buy buy-per base-sell sell-per)

(defvar *staffs* (list 
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

(defvar *jars* (list
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

(defun jar-by-name (name)
  (let ((res (car (member-if (lambda (it) (string= name (jar-name it))) *jars*))))
	(if res res (error (format "Could not find jar : %s" name)))))

(defun staff-by-name (name)
  (let ((res (car (member-if (lambda (it) (string= name (staff-name it))) *staffs*))))
	(if res res (error (format "Could not find jar : %s" name)))))

(defun jar-cost-buying (jar-name charges)
  (+ (jar-base-buy (jar-by-name jar-name)) (* (jar-buy-per (jar-by-name jar-name)) charges)))

(defun consistent-jar-buy? (jar price spots)
  (let ((base (jar-base-buy jar))
		(per (jar-buy-per jar)))
	(= (- price (* spots per)) base)))

(defun find-consistent-jars-buy (price spots)
  (filter (lambda (jar) (consistent-jar-buy? jar price spots))
		  *jars*))

(defun jar-cost-selling (jar-name charges)
  (+ (jar-base-sell (jar-by-name jar-name)) (* (jar-sell-per (jar-by-name jar-name)) charges)))

(defun consistent-jar-sell? (jar price spots)
  (let ((base (jar-base-sell jar))
		(per (jar-sell-per jar)))
	(= (- price (* spots per)) base)))

(defun find-consistent-jars-sell (price spots)
  (filter (lambda (jar) (consistent-jar-sell? jar price spots))
		  *jars*))

(defun staff-cost-buying (staff-name charges)
  (+ (staff-base-buy (staff-by-name staff-name)) (* (staff-buy-per (staff-by-name staff-name)) charges)))

(defun consistent-staff-buy? (staff price charges)
  (let ((base (staff-base-buy staff))
		(per (staff-buy-per staff)))
	(= (- price (* charges per)) base)))

(defun consistent-staff-buy-in-range? (staff price range)
  (foldl 
   (lambda (it ac)
	 (or (consistent-staff-buy? staff price it) ac))
   nil
   range))

(defun* find-consistent-staffs-buy (price &optional (range (list 3 6)))
  (let ((range (loop for i from (car range) to (cadr range) collect i)))
	(filter (lambda (staff)
			  (consistent-staff-buy-in-range? staff price range))
			*staffs*)))

(defun staff-cost-selling (staff-name charges)
  (+ (staff-base-sell (staff-by-name staff-name)) (* (staff-sell-per (staff-by-name staff-name)) charges)))

(defun consistent-staff-sell? (staff price charges)
  (let ((base (staff-base-sell staff))
		(per (staff-sell-per staff)))
	(= (- price (* charges per)) base)))

(defun consistent-staff-sell-in-range? (staff price range)
  (foldl 
   (lambda (it ac)
	 (or (consistent-staff-sell? staff price it) ac))
   nil
   range))

(defun* find-consistent-staffs-sell (price &optional (range (list 3 6)))
  (let ((range (loop for i from (car range) to (cadr range) collect i)))
	(filter (lambda (staff)
			  (consistent-staff-sell-in-range? staff price range))
			*staffs*)))

(find-consistent-staffs-buy 1800 (list 1 6))
			
(find-consistent-jars-buy 7500 5)

(defun print-consistent-jars-buy (cost-spots)
  (interactive "XEnter '(cost spots):")
  (let ((cost (car cost-spots))
		(spots (cadr cost-spots)))
  (loop for jar in (find-consistent-jars-buy cost spots) do
		(insert (format "%s\n" (jar-name jar))))))

(defun print-consistent-jars-sell (cost-spots)
  (interactive "XEnter '(cost spots)")
  (let ((cost (car cost-spots))
		(spots (cadr cost-spots)))
  (loop for jar in (find-consistent-jars-sell cost spots) do
		(insert (format "%s\n" (jar-name jar))))))

(defun print-consistent-staffs-buy (cost)
  (interactive "XEnter cost:")
  (loop for staff in (find-consistent-staffs-buy cost) do
		(insert (format "%s\n" (staff-name staff)))))
												 
(defun print-consistent-jars-buy (cost)
  (interactive "XEnter cost:")
  (loop for jar in (find-consistent-jars-buy cost) do
		(insert (format "%s\n" (jar-name jar)))))
												 
(provide 'shiren)
		    
;;; Leaning Jar = Holes
