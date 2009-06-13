(ns regsim (:use 
			toups.misc 
			toups.list 
			toups.gaming 
			toups.drawing
			toups.colors
			toups.structstar))
(in-ns 'regsim)

(def π Math/PI)

(def π2 (* 2 Math/PI))

(let [id (ref 0)]
  (defn reset-ids []
	(dosync (ref-set id 0)))
  (defstruct* person
	(:id (do1 
		  @id
		  (dosync (ref-set id (+ 1 @id)))))
	(:state :lost)
	(:dir (* π2 (random)))
	(:pos [(random) (random)])
	(:vel [0 0])))

(def *people* (ref (list)))

(def g (start-game))
(def screen (:screen g))
(def w (:w screen))
(def h (:h screen))

(let [{screen :screen} g
	  {w :w h :h} screen]
  (translate screen (/ w 2.0) (/ h 2.0))
  (scale screen (/ w 2.0)  (- (/ h 2.0)))

(reset-transform screen)
(use 'toups.drawing)

(comment
  (defn test-box [{s :screen :as g}]
	(rect s [0.0 0.0  1.0 0.5] green))
  (test-box g)
  (add-loop-fun g test-box 500)
  (game-going? g)

(with-color (:g2d (:screen g))
  (translate (:screen g) 0 0))

(clear-game g)

(defn lost->
  "updates a lost person's state"
  [person persons]
  (randomly
   10 (lost->inspired person persons)
   10 (lost->leader) 
   40 (lost->follower person person)
   40 person
  

(defn update-person
  "Takes a person from one state to another."
  [person persons]
  (case (:state person)
		:lost (lost-> person persons)
		:leading (leading-> person persons)
		:following (following-> person persons)
		:inspired (inspired-> person persons)))


		
		
