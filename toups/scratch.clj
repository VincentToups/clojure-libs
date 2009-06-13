(in-ns 'user)

(use 'toups.drawing)
(use 'toups.colors)

(defn mff [game]
  (let [ms @(:mouse-state game)
		{x :x y :y b :buttons} ms]
	(save-excursion (:screen game)
	  (translate (:screen game) x y)
	  (if (b *mouse-1*)
		(rotate (:screen game) (/ Math/PI 4)))
	  (rect (:screen game)
			[0 0 10 10] ivory-1))))
	
(mff g)
(add-loop-fun g mff 500)
(game-going? g)

(. (:g2d (:screen g)) setTransform (new java.awt.geom.AffineTransform))
