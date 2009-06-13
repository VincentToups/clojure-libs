(clojure/ns koppa-gui
    (:use 
   koppa-base
   compojure.html 
   compojure.http 
   compojure.jetty 
   toups.structstar 
   toups.misc
   toups.list))

(in-ns 'koppa-gui)

(import '(javax.swing JFrame JLabel JTextField JButton JTextArea)
		'(java.awt.event ActionListener)
		'(java.awt GridLayout))

(defn parseDoubleOr [s & [v]]
  (try
   (. Double (parseDouble s))
   (catch Exception e
	(if v v 0))))

(defn text-field->number [tf]
  (parseDoubleOr (. tf (getText))))

(defn ui []
  (let [frame (new JFrame "Koppa v 0.01")
		jar-cost (new JTextField)
		jar-cost-label (new JLabel "Jar Cost")
		jar-slots (new JTextField)
		jar-slots-label (new JLabel "Jar Slots")
		staff-cost (new JTextField)
		staff-cost-label (new JLabel "Staff Cost")
		cogitate-button (new JButton "Cogitate")
		output-text (new JTextArea)
		output-panel (new java.awt.Panel)
		input-panel (new java.awt.Panel)]
	(. cogitate-button
	   (addActionListener 
		(proxy [ActionListener] []
                (actionPerformed [evt]
                    (let [jc (. Double (parseDouble (. jar-cost (getText))))
						  js (. Double (parseDouble (. jar-slots (getText))))
						  sc (. Double (parseDouble (. staff-cost (getText))))
						  names (map :name (find-consistent-jars-buy jc js))
						  staff-names (map :name (find-consistent-staffs-buy sc))]
					  (. output-text (setText "Possible Jars: {"))
					  (loop [[name & rest] names]
						(. output-text (append (str name ", ")))
						(if (not (empty? rest))
								 (recur rest)))
					  (. output-text (append "}\n"))
					  (. output-text (append "Possible Staves: {"))
					   (loop [[name & rest] staff-names]
 						(. output-text (append (str name ", ")))
 						(if (not (empty? rest))
 								 (recur rest)))
 					  (. output-text (append "}\n"))
                      ;(. output-text
                      ;   (setText (str (+ 32 (* 1.8 jc)) " Fahrenheit : " js " " sc)))

					  )))))
	(doto frame 
	  (setLayout (new java.awt.GridLayout 2 1 2 2))
	  (add input-panel)
	  (add output-panel))
	(doto input-panel
	  (setLayout (new java.awt.GridLayout 4 2 2 2))
	  (add jar-cost)
	  (add jar-cost-label)
	  (add jar-slots)
	  (add jar-slots-label)
	  (add staff-cost)
	  (add staff-cost-label)
	  (add cogitate-button))
	(doto output-text
	  (setColumns 35)
	  (setLineWrap true)
	  (setRows 5)
	  (setWrapStyleWord true)
	  (setEditable false))
	(doto output-panel 
	  (add output-text))
	(doto frame
	  (setSize 400 400)
	  (setVisible true))
	(hash-map :frame frame :jar-cost jar-cost :jar-slots jar-slots :staff-cost staff-cost)))
			
