

(defun leaves (x)
  (let ( acc )
    (println [ "walking leaves of node: %s" % (repr x) ] )
    (labels ( (walk (node)
		    (println [ "Current node: %s" % (getLocalName node) ] )
		    (cond
		     ( ( isLeaf node )
		       (setq acc (Cons node acc ) ) )
		     ( true 
		       (foreach c (children node) 
				(println [ "Looking at node: %s" % [c getLocalName ] ] )
				(walk c) ) ) ) ) )
      ( walk x ) 
      (println [ "Returning result: %s" % (repr acc ) ] )
      acc ) ) )



(global a (QDomNode :fromFile "test.xml") )
(global b (leaves a) )
(println ( format "Leaves: %s" (repr b ) ) )
