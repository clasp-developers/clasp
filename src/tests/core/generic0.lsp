
(defgeneric ptype (x))
(defmethod ptype ((x integer)) (print "integer"))
(defmethod ptype ((x float)) (print "float"))
(defmethod ptype ((x symbol)) (print "symbol"))

