(print "Loading core database")
(defvar *dbFileName* ())

(when (> (length *ARGS*) 1)
  (setq *dbFileName* (@ *ARGS* 1)))

(if *dbFileName*
    (database *dbFileName*)
  (standardDatabase "core"))


(defun identifyPlugs (top)
  (let ( inPlug outPlugs )
    (foreach p (plugsWithMatesAsCons top)
	     (if ( isA p InPlug )
		 ( setq inPlug p)
	       (setq outPlugs (Cons p outPlugs))))
    (list inPlug outPlugs)))

(defun sortedList ( unsorted )
  (let ((sorted (sort unsorted (lambda (x y) (< (getName x) (getName y))))))
    sorted))

(defvar *constitutions* (ObjectDictionary))
(let* ((db *DATABASE*)
       (sortedConstitutions (sortedList (constitutionsAsCons db))))
   (foreach c sortedConstitutions
	   (print (% "Constitution[%s]" (getName c)))
	   (foreach s (stereoisomersAsCons c)
		    (print (% "    Stereoisomer[%s] pdb[%s]" (getName s) (getPdb s))))
	   (let ((sortedTopologies (sortedList (topologiesAsCons c))))
	   (foreach t sortedTopologies
		    (print (% "      Topology[%s]" (getName t)))
		    (let* ((identifiedPlugs (identifyPlugs t))
			   (inPlug (car identifiedPlugs))
			   (outPlugs (cadr identifiedPlugs)))
		      (when inPlug
			(prin1 (% " %s" (getName inPlug))))
		      (foreach p outPlugs
			       (prin1 (% " %s" (getName p)))))
		    ))))
