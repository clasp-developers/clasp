;;
;; Convert dunbrak rotamer libraries to cando format
(defun convert-dunbrack-rotamer-library-to-compressed-cxml (&key inPath outPath)
  "Convert a Dunbrack rotamer library file into a compressed Cando CXML file"
  (let ((fin (open inPath :direction :input :compression :gzip)))
    (let ((done false)
	  (all (ObjList))
	  (totalProb 0.0)
	  prevPhi prevPsi
	  rotamerSet
	  (rotamerLibrary (kin:BackboneDependentRotamerLibrary
			   :phiStart -180 :phiCount 37 :phiStep 10
			   :psiStart -180 :psiCount 37 :psiStep 10))
	  )
      (while (not done)
	(multiple-value-bind ( line hitEof ) (read-line fin false)
	  (when line
	    (unless (or (== (left line 1) "#") (== line ""))
	      (let* ((parts (split-at-white-space line))
		     (phi (parse-integer (nth 1 parts)))
		     (psi (parse-integer (nth 2 parts)))
		     (count (parse-integer (nth 3 parts)))
		     (r1 (parse-integer (nth 4 parts)))
		     (r2 (parse-integer (nth 5 parts)))
		     (r3 (parse-integer (nth 6 parts)))
		     (r4 (parse-integer (nth 7 parts)))
		     (prob (parse-real (nth 8 parts)))
		     (x1 (parse-real (nth 9 parts)))
		     (x2 (parse-real (nth 10 parts)))
		     (x3 (parse-real (nth 11 parts)))
		     (x4 (parse-real (nth 12 parts)))
		     (s1 (parse-real (nth 13 parts)))
		     (s2 (parse-real (nth 14 parts)))
		     (s3 (parse-real (nth 15 parts)))
		     (s4 (parse-real (nth 16 parts))))
		(when (or (!= prevPsi psi) (!= prevPhi phi))
		  (bformat t "%s\n" line)
		  (setq rotamerSet (kin:BackboneDependentRotamerSet :phi phi :psi psi))
		  (kin:backbone-dependent-rotamer-library-add-rotamer-set rotamerLibrary rotamerSet))
		(setq prevPsi psi)
		(setq prevPhi phi)
		(multiple-value-bind
		      (dihList sigList indList)
		    (cond
		      ((== r2 0) (values (list x1) (list s1) (list r1)))
		      ((== r3 0) (values (list x1 x2) (list s1 s2) (list r1 r2)))
		      ((== r4 0) (values (list x1 x2 x3) (list s1 s2 s3) (list r1 r2 r3)))
		      (t (values (list x1 x2 x3 x4) (list s1 s2 s3 s4) (list r1 r2 r3 r4))))
		  (let ((rot (kin:Rotamer :dihedrals dihList
					  :sigmas sigList
					  :indices indList
					  :count count
					  :probability prob)))
		    (kin:rotamer-set-add-rotamer rotamerSet rot)
		    )
		  )
		(setq totalProb (+ prob totalProb))
		))
	    )
	  (when (not line)
	    (setq done t))))
      (let ((fout (open outPath :direction :output :compression :gzip)))
	(serialize-xml fout rotamerLibrary))
      )
    )
  )



(defparameter *inPath* (Path (cadr *ARGS*)))
(defparameter *outPath* (Path (caddr *ARGS*)))

(bformat t "Converting [%s] to [%s]" (asString *inPath*) (asString *outPath*))

(convert-dunbrack-rotamer-library-to-compressed-cxml :inPath *inPath* :outPath *outPath*)

