(ns toups.gaming 
  (:use toups.drawing toups.misc toups.structstar toups.list))
(in-ns 'toups.gaming)

(def *mouse-1* java.awt.event.MouseEvent/BUTTON1)
(def *mouse-2* java.awt.event.MouseEvent/BUTTON2)
(def *mouse-3* java.awt.event.MouseEvent/BUTTON3)

(defn clear-game [game]
  (clear (:screen game)))

(defn draw-game [game]
  (show (:screen game)))

(defn sleep-game [game]
  (Thread/sleep @(:sleep-time game)))

(defn exit-game [game]
  (dosync (ref-set (:done game) true)))

(defn game-done? [game]
  @(:done game))

(defstruct* game (:screen (new-screen 640 480))
  (:thread (ref nil))
  (:key-state (ref {}))
  (:mouse-state (ref {:x 0 :y 0 :buttons {*mouse-1* false 
										  *mouse-2* false 
										  *mouse-3* false}}))
  (:sleep-time (ref 5))
  (:init-hook (ref (sorted-map)))
  (:loop-hook (ref (sorted-map
			   100 clear-game
			   10000 draw-game
			   11000 sleep-game)))
  (:exit-hook (ref (sorted-map 100 exit-game)))
  (:done (ref false)))

(defn add-loop-fun [game f rank]
  (dosync (ref-set (:loop-hook game) (assoc @(:loop-hook game) rank f))))

(defn add-init-fun [game f rank]
  (dosync (ref-set (:init-hook game) (assoc @(:init-hook game) rank f))))

(defn add-exit-fun [game f rank]
  (dosync (ref-set (:exit-hook game) (assoc @(:exit-hook game) rank f))))

(defn rm-loop-fun [game rank]
  (dosync (ref-set (:loop-hook game) (dissoc @(:loop-hook game) rank))))

(defn rm-init-fun [game rank]
  (dosync (ref-set (:init-hook game) (dissoc @(:init-hook game) rank))))

(defn rm-exit-fun [game rank]
  (dosync (ref-set (:exit-hook game) (dissoc @(:exit-hook game) rank))))

(defmacro defn-loop [game rank & body]
  `(add-loop-fun ~game ~rank (fn [~(symbol "game")] ~@body)))

(defmacro defn-init [game rank & body]
  `(add-init-fun ~game ~rank (fn [~(symbol "game")] ~@body)))

(defmacro defn-exit [game rank & body]
  `(add-exit-fun ~game ~rank (fn [~(symbol "game")] ~@body)))

(defn execute-init [game]
  (let [hook @(:init-hook game)]
	(loop [[key & rst :as lft] (keys hook)]
	  (if (empty? lft) nil
		  (do 
			(try 
			 ((hook key) game)
			 (catch Exception e
			   (prn "Error in function at: " key " (" (hook key) ")")
			   (prn "Removing from init")
			   (dosync (ref-set (:init-hook game) (dissoc @(:init-hook game) key)))))
			(recur rst))))))

(defn execute-loop [game]
  (let [hook @(:loop-hook game)]
	(loop [[key & rst :as lft] (keys hook)]
	  (if (empty? lft) nil
		  (do 
			(try 
			 ((hook key) game)
			 (catch Exception e
			   (prn "Error in function at: " key " (" (hook key) ")")
			   (prn "Removing from loop")
			   (dosync (ref-set (:loop-hook game) (dissoc @(:loop-hook game) key)))
			   ))
			(recur rst))))))

(defn execute-exit [game]
  (let [hook @(:exit-hook game)]
	(loop [[key & rst :as lft] (keys hook)]
	  (if (empty? lft) nil
		  (do 
			(try 
			 ((hook key) game)
			 (catch Exception e
			   (prn "Error in function at: " key " (" (hook key) ")")
			   (prn "Removing from exit")
			   (dosync (ref-set (:exit-hook game) (dissoc @(:exit-hook game) key)))))
			(recur rst))))))

(defn start-game 
  ([game]
	 (execute-init game)
	 (. (:frame (:screen game))
		addWindowListener
		(proxy [java.awt.event.WindowListener] []
		  (windowClosing [evt] (dosync (ref-set (:done game) true)))))
	 (. (:canvas (:screen game))
		addKeyListener
		(proxy [java.awt.event.KeyListener] []
		  (keyReleased [evt]
					   (dosync (ref-set (:key-state game) (dissoc @(:key-state game) (. evt getKeyChar)))))
		  (keyPressed [evt]
					  (dosync (ref-set (:key-state game) (assoc @(:key-state game) (. evt getKeyChar) (System/nanoTime)))))))
	 (. (:canvas (:screen game))
		addMouseListener
		(proxy [java.awt.event.MouseListener] []
		  (mousePressed [evt]
						(let [{mouse-state :mouse-state} game
							  {buttons :buttons} @mouse-state
							  button (. evt getButton)]
						  (dosync (ref-set mouse-state
										   (assoc @mouse-state 
											 :x (. evt getX)
											 :y (. evt getY))))
						  (dosync (ref-set mouse-state
										   (assoc @mouse-state
											 :buttons 
											 (assoc buttons button true))))))
		  (mouseReleased [evt]
						 (let [{mouse-state :mouse-state} game
							   {buttons :buttons} @mouse-state
							   button (. evt getButton)]
						   (dosync (ref-set mouse-state
											(assoc @mouse-state 
											  :x (. evt getX)
											  :y (. evt getY))))
						   (dosync (ref-set mouse-state
											(assoc @mouse-state
											  :buttons 
											  (assoc buttons button false))))))))
	 (. (:canvas (:screen game))
		addMouseMotionListener
		(proxy [java.awt.event.MouseMotionListener] []
		  (mouseDragged [evt]
						(let [{mouse-state :mouse-state} game]
						  (dosync (ref-set mouse-state
										   (assoc @mouse-state 
											 :x (. evt getX)
											 :y (. evt getY))))))
		  (mouseMoved [evt]
					  (let [{mouse-state :mouse-state} game]
						(dosync (ref-set mouse-state
										 (assoc @mouse-state 
										   :x (. evt getX)
										   :y (. evt getY))))))))
	 (let [thread (Thread. (fn [] (loop [done @(:done game)]
									(if (not done) 
									  (do (execute-loop game)
										  (recur @(:done game)))
									  nil))))]
	   (dosync (ref-set (:thread game) thread))
	   (.start thread))
	 game)
  ([]
	 (start-game (make-game))))

(defn clear-key-listeners [game]
  (let [cvs (:canvas (:screen game))]
	(loop [[c r :as listeners] 
		   (seq (. cvs getKeyListeners))]
	  (if (empty? listeners) nil
		  (do 
			(. cvs removeKeyListener c)
			(recur r))))))

(defn game-going? [game]
  (. @(:thread game) isAlive))
