(ns toups.nbacktris 
  (:use 
   toups.gaming 
   toups.drawing 
   toups.colors 
   toups.mat 
   toups.list
   toups.misc))
(in-ns 'toups.nbacktris)

(def pi Math/PI)
(def pi2 (/ pi 2))


(def block-size 16)

(def f false)
(def t true)

(let [shape
	  (mat-lit 
			[[  f  f  f  f  f ]
			 [  f  f  f  f  f ]
			 [  f \i \i \i \i ]
			 [  f  f  f  f  f ]
			 [  f  f  f  f  f ]])
	  r1 (rot90 shape -1)
	  r2 (rot90 shape -2)
	  r3 (rot90 shape -3)]
  (defn i-block [r]
	(let [r (mod r 4)]
	  (cond
		(= r 0) shape
		(= r 1) r1
		(= r 2) r2 
		(= r 3) r3))))

(let [shape
	  (mat-lit
	   [[  f  f  f  f  f ]
		[  f  f  f  f  f ]
		[  f  f \o \o  f ]
		[  f  f \o \o  f ]
		[  f  f  f  f  f ]])] 
  (defn o-block [n]
	   shape))

(let [shape
	  (mat-lit
	  [[  f  f  f  f  f ]
	   [  f  f \t  f  f ]
	   [  f \t \t \t  f ]
	   [  f  f  f  f  f ]
	   [  f  f  f  f  f ]])
	  r1 (rot90 shape -1)
	  r2 (rot90 shape -2)
	  r3 (rot90 shape -3)]
  
  (defn t-block [n]
	(let [r (mod n 4)]
	  (cond
		(= r 0) shape
		(= r 1) r1
		(= r 2) r2
		(= r 3) r3))))

(let [shape
	 (mat-lit
	  [[  f  f  f  f  f ]
	   [  f  f  f \n  f ]
	   [  f  f \n \n  f ]
	   [  f  f \n  f  f ]
	   [  f  f  f  f  f ]])
	  r1 (rot90 shape -1)
	  r2 (rot90 shape -2)
	  r3 (rot90 shape -3)]
(defn n-block [n]
	 (let [r (mod n 4)]
	   (cond
		 (= r 0) shape
		 (= r 1) r1
		 (= r 2) r2
		 (= r 3) r3))))

(let [shape
	 (mat-lit
	  [[  f  f  f  f  f ]
	   [  f \z  f  f  f ]
	   [  f \z \z  f  f ]
	   [  f  f \z  f  f ]
	   [  f  f  f  f  f ]])
	  r1 (rot90 shape -1)
	  r2 (rot90 shape -2)
	  r3 (rot90 shape -3)]
(defn z-block [n]
	 (let [r (mod n 4)]
	   (cond
		 (= r 0) shape
		 (= r 1) r1
		 (= r 2) r2
		 (= r 3) r3))))

(let [shape
	 (mat-lit
	  [[  f  f  f  f  f ]
	   [  f  f \l  f  f ]
	   [  f  f \l  f  f ]
	   [  f \l \l  f  f ]
	   [  f  f  f  f  f ]])
	  r1 (rot90 shape -1)
	  r2 (rot90 shape -2)
	  r3 (rot90 shape -3)]
(defn l-block [n]
	 (let [r (mod n 4)]
	   (cond
		 (= r 0) shape
		 (= r 1) r1
		 (= r 2) r2
		 (= r 3) r3))))

(let [shape
	 (mat-lit
	  [[  f  f  f  f  f ]
	   [  f  f \j  f  f ]
	   [  f  f \j  f  f ]
	   [  f  f \j \j  f ]
	   [  f  f  f  f  f ]])
	  r1 (rot90 shape -1)
	  r2 (rot90 shape -2)
	  r3 (rot90 shape -3)]
(defn j-block [n]
	 (let [r (mod n 4)]
	   (cond
		 (= r 0) shape
		 (= r 1) r1
		 (= r 2) r2
		 (= r 3) r3))))

(defn print-block [{[ h w :as sz] :sz :as b}]
  (loop-if [i 0]
	  (< i h)
	  false
	(loop-if [j 0] 
		(< j w)
		false
	  (if (mat-get b i j)
		(print \*)
		(print " "))
	  (recur (+ 1 j)))
	(print "\n")
	(recur (+ i 1))))

(defn block-on-board? [brd-h brd-w
					   blk-h blk-w
					   i j]
  (and (> 0 i)
	   (> 0 j)
	   (< brd-h (+ i blk-h))
	   (< brd-w (+ j blk-w))))

(defn el->n [e]
  (if e 1 0))

(defn block-overlap? [b1 b2]
  (any (fn [x] x) (mat->raw (map-mat
   (fn [e1 e2]
	 (> (+ (el->n e1)
		(el->n e2)) 1))
   b1 b2))))

(defn block-place? [{[brd-h brd-w :as brd-sz] :sz :as brd} 
					{[blk-h blk-w :as blk-sz] :sz :as blk} 
					[i j :as pos]]
  (and
   (block-on-board? brd-h brd-w
					blk-h blk-w
					i j))
   (let [sub (sub-mat-or brd
					  (range i (+ i blk-h))
					  (range j (+ j blk-w))
					  (ij t))]
	 (not (block-overlap? sub blk))))
   
(defn block-blit [{[brd-h brd-w :as brd-sz] :sz :as brd} 
				  {[blk-h blk-w :as blk-sz] :sz :as blk} 
				  [i j :as pos]]
  (loop-if [ii 0
			brd brd] 
		   (< ii blk-h)
		   brd
		   (recur (+ ii 1) 
				  (loop-if [jj 0
							brd brd]
						   (< jj blk-w)
						   brd
						   (recur (+ jj 1) (mat-set brd
													(+ ii i)
													(+ jj j)
													(mat-get blk ii jj)))))))

(defn block->colors [b]
  (case b
		\g [(color 70 101 106)
			(color 120 134 136)]
		\i [(color 0xA6 0x31 0x00)
			(color 0xFF 0xF3 0x73)]
		\o [(color 0x34 0x05 0x70)
			(color 0x60 0xD6 0xA7)]
		\t [(color 0x00 0x71 0x43)
			(color 0xFF 0xEF 0x40)]
		\n [(color 0x50 0x29 0x82)
			(color 0xBF 0xB3 0x30)]
		\z [(color 0x21 0x82 0x5B)
			(color 0x9A 0x6A 0xD6)]
		\l [(color 0xFF 0x79 0x40)
			(color 0xFF 0x9D 0x73)]
		\j [(color 0x50 0x29 0x82)
			(color 0xFF 0xE9 0x00)]))

(defn draw-block 
  ([screen in out]
	 (let [b2 (/ block-size 2)
		   ib (- block-size 2)
		   ib2 (/ ib 2)]
	   (rect screen [(- b2) (- b2) block-size block-size] out)
	   (rect screen [(- ib2) (- ib2) ib ib] in)))
  ([screen [in out]]
	 (draw-block screen in out)))

(defn translate-block [screen j i]
  (translate screen (* i block-size) (* j block-size)))

(defn draw-block-at [screen [in out] [nx ny]]
  (save-excursion screen
	(translate-block screen nx ny)
	(draw-block screen in out)))

(def *bx* 240)
(def *by* 32)
(def *bw* 10)
(def *bh* 20)

(def *board* (ref (mat *bh* *bw* (ij f))))
(def *current-tetromino* (ref nil))

(defn draw-border [screen]
  (save-excursion screen
	(translate screen *bx* *by*)
	(translate-block screen -1 -1)
	(save-excursion screen
		(loop-if [i 0] (< i (+ *bh* 2)) nil
		  (draw-block screen (block->colors \g))
		  (draw-block-at screen (block->colors \g) [0 (+ *bw* 1)])
		  (translate-block screen 1 0)
		  (recur (+ i 1))))
	(save-excursion screen
		(loop-if [j 0] (< j (+ *bw* 2)) nil
		  (draw-block screen (block->colors \g))
		  (draw-block-at screen (block->colors \g) [(+ *bh* 1) 0])
		  (translate-block screen 0 1)
		  (recur (+ j 1))))))

(defn draw-board [screen]
  (save-excursion screen
	(translate screen *bx* *by*)
	(for-each-mat-ij 
	 (fn [i j v] 
	   (draw-block-at 
		screen 
		(block->colors v)
		[i j]))
	 @*board*)))

(defn tetromino-> [[b-id r]]
  (case b-id
		\i (i-block r)
		\o (o-block r)
		\t (t-block r)
		\n (n-block r)
		\z (z-block r)
		\l (l-block r)
		\j (j-block r)))

(defn random-tetromino []
  (randomly 
   [\i 0]
   [\o 0]
   [\t 0]
   [\n 0]
   [\z 0]
   [\l 0]
   [\j 0]))

(defn draw-current-tetromino [screen]
  (if *current-tetromino*
	(let [{i :i 
		   j :j
		   r :r 
		   b :b} *current-tetromino*]
	(save-excursion screen 
		(translate-block screen i j)
		(for-each-mat-ij 
		 (fn [i j v]
		   (draw-block-at screen (block->colors v) [i j]))
		 (tetromino-> b r))))))

(def g (start-game))
(draw-border (:screen g))
(draw-board (:screen g))
(defn main [{s :screen :as g}]
  (draw-border s)
  (draw-board s)
  (draw-current-block s)
  (dosync (ref-set *board* (random-board *bh* *bw*))))
			   
(main g)
(add-loop-fun g main 500)
(draw-board {:screen g})
(rm-loop-fun g 500)
(game-going? g)

(def *blocks* [\i \o \t \n \z \l \j])
(defn rand-block []
  (*blocks* (random 7)))
		
(defn random-board [h w]
  (mat h w
	   (ij (rand-block))))

(let [start-x 120
	  start-y 20]
  (defn draw-board [screen board]
	(save-excursion screen
      (translate screen start-x start-y)
	  (map-mat-ij (fn [i j b] 
					(when b
					  (save-excursion screen
									  (translate-block screen j i)
									  (draw-block screen (block->colors b)))))
				  board))))

(def *board* (ref (mat 10 8 (ij f))))
(defn main [game]
  (let [screen (:screen game)]
	(draw-board screen @*board*)))





(add-loop-fun g main 500)

(def *f* (ref 0))
(defn test-fr [{s :screen :as g}]
  (dosync (ref-set *f* (+ @*f* 1)))
  (string s (str @*f*) 10 10))

(add-loop-fun g test-fr 501)

((fn [{s :screen :as g}]
				  (dosync (ref-set *f* (+ @*f* 1)))
				  (string s (str @*f*) 10 10)) g)

(test-fr g)
(:loop-hook g)

(loop [i 0]
  (if (< i 100000)
	(do (let [nb (random-board 10 10)]
		  (dosync (ref-set *board* nb)))
		(recur (+ i 1)))
	false))

(comment (draw-i (:screen g) pi2)

		 (game-going? g)
		 (. (:g2d (:screen g)) setTransform (new java.awt.geom.AffineTransform))

		 (main g))


