(ns toups.drawing (:use toups.list toups.misc toups.structstar toups.colors))
(in-ns 'toups.drawing)

(defstruct* screen :w :h :canvas :frame :buf-strat :g2d)

(defn new-screen [w h]
  (let [canvas (new java.awt.Canvas)
		frame (new javax.swing.JFrame
				(.. java.awt.GraphicsEnvironment (getLocalGraphicsEnvironment)
					(getDefaultScreenDevice) (getDefaultConfiguration)))]
	;(. frame setDefaultCloseOperation javax.swing.WindowConstants/EXIT_ON_CLOSE)
	(. (. frame getContentPane) add canvas)
	(. frame setSize w h)
	(. canvas setSize w h)
	;(. frame pack)
	(. frame show)
	(. canvas createBufferStrategy 2)
	(make-screen {:w w :h h
				 :canvas canvas 
				 :frame frame
				 :g2d (. (. canvas getBufferStrategy) getDrawGraphics)
				 :buf-strat (. canvas getBufferStrategy)})))
	
(defn show [screen]
  (. (:buf-strat screen) show))

(defmacro with-color [g color & body]
  `(let [color# (. ~g getColor)]
	 (. ~g setColor ~color)
	 (try ~@body
		  (catch Exception e#
			(do (. ~g setColor color#)
				(throw e#))))
	 (. ~g setColor color#)))

(defmacro save-excursion [screen & body]
  `(let [screen# ~screen
		 transform# (. (:g2d screen#) getTransform)]
	 (try ~@body
		  (catch Exception e#
			(do 
			  (. (:g2d screen#) setTransform transform#)
			  (throw e#))))
	 (. (:g2d screen#) setTransform transform#)))

(defn clear [screen]
  (let [w (:w screen)
		h (:h screen)
		g (:g2d screen)]
	(save-excursion screen
	  (. g setTransform (new java.awt.geom.AffineTransform))
	  (with-color g black
		(. g fillRect 0 0 w h)))))

(defn rect [{g :g2d} [x y w h] clr]
  (with-color g clr
	(. g fillRect x y w h)))

(defn string 
  ([{g :g2d} txt i j]
	 (. g drawString txt i j))
  ([{g :g2d} txt [i j]]
	 (. g drawString txt i j)))

(defn vect->poly [vs]
  (let [p (new java.awt.Polygon)]
	(map (fn [v] (. p addPoint (first v) (second v))) vs)
	p))
	
(defn poly
  ([{g :g2d} shape clr]
	 (if (= (class shape) java.awt.Polygon)
	   (with-color g clr 
	     (. g fill shape))
	   (with-color g clr
		 (. g fill (vect->poly shape))))))

(def images (ref {}))
(defn clear-images []
  (dosync (ref-set images {})))
(defn load-image [nm]
  (let [v (@images nm)]
	(if v v
		(let [img (. javax.imageio.ImageIO read (new java.io.File nm))]
		  (dosync (ref-set images (assoc @images nm img)))
		  img))))
			  
(defn blit [{g :g2d} img [x y]]
  (. g drawImage img nil x y))

(defn translate 
  ([{g :g2d} x y]
	 (. g translate x y))
  ([{g :g2d} [x y]]
	 (. g translate x y)))

(defn rotate
  ([{g :g2d} theta x y]
	 (. g rotate theta x y))
  ([{g :g2d} theta [x y]]
	 (. g rotate theta x y))
  ([{g :g2d} theta]
	 (. g rotate theta)))

;; (try (seq s) (catch Exception e false))


(defn scale 
  ([{g :g2d} fx fy]
	 (. g scale fx fy))
  ([{g :g2d} arg]
	 (if (vector? arg)
	   (. g scale (first arg) (frest arg))
	   (. g scale arg arg))))

(defn shear 
  ([{g :g2d} fx fy]
	 (. g shear fx fy))
  ([{g :g2d} [fx fy]]
	 (. g shear fx fy)))

;;; (defn reset-transform [{g :g2d :as screen}]
  
  

;;; Testing

(comment 

(use 'toups.drawing)

(def stop (ref false))

(let [petite (load-image "/home/toups/test.png")]
  (defn draw-fun [screen]
	(save-excursion screen
	  (translate screen (random 600) (random 400))
	  (scale screen (* 10 (random)))
	  (rotate screen (* 2 3.14159 (random)))
	  (blit screen petite [0 0]))))
			
(defn testblit [w h]
  (let [screen (new-screen w h)
		img (load-image "/home/toups/test.png")]
	(. (:frame screen) addWindowListener
	   (proxy [java.awt.event.WindowListener] []
		 (windowClosing [evt] (dosync (ref-set stop true)))))
	(loop []
	  (draw-fun screen)
	  (show screen)
	  (if (not @stop)
		(recur)
		nil)))))
  
