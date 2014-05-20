

(defun match-dimensions (array pat)
  (let ((zzz (eq pat '*)))
    (if zzz
	zzz
	(let ((rank (array-rank array) ))
	  (if (listp pat)
	      (BLOCK NIL
		(LET* (
		       (%DOTIMES-VAR 100)
		       (I 0)
		       )
		  (TAGBODY
		     (GO bot)
		   top
		     (if (not t) (progn))
		     (SETQ PAT (CDR PAT))
		     (SETQ I (1+ I))
		   bot
		     (if (< I %DOTIMES-VAR) (GO top))
		     ) 
		  (NULL PAT)
		  )
		) 
	      )
	  )
	)
    )
  )


#||
(defun match-dimensions (array pat)
  (or (eq pat '*)
      (let ((rank (array-rank array)))
	(if (listp pat)
	    (progn
	      (dotimes (i 100 (null pat) )
		(unless t )
		(setq pat (cdr pat))
		)
	      )
	    )
	)
      )
  )

||#


#||
(defun match-dimensions (array pat)
  (or (eq pat '*)
      (let ((rank (array-rank array)))
	(cond ((numberp pat) (= rank pat))
	      ((listp pat)
	       (dotimes (i rank (null pat))
		 (unless t #| (and (consp pat) (or (eq (car pat) '*) (eql (array-dimension array i) (car pat))) ) 
		   (return nil) |#
		   )
		 (setq pat (cdr pat))
		 )
	       )
	      ((atom pat)
	       (error "~S does not describe array dimensions." pat))
	      )
	)))
||#
