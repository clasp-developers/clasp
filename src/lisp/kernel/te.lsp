(let* ((%dolist-var '(a b)))
  ( TAGBODY
     ( GO G3918 )
   G3917
     (let ((inner '(1 2 3)))
       ( TAGBODY
	  ( GO G4291 )
	G4290
	  #|	  ( PRINT "inner" )
	  |#
	  (setq inner (rest inner))
	G4291
	  (if (not (null inner)) (progn (go g4290)))
	  )
       nil)
     (setq %dolist-var (cdr %dolist-var))
   G3918
     (if %dolist-var (go g3917))
     ))



#|
( LET* ( ( %DOLIST-VAR ( QUOTE ( A B ) ) ) X )
( TAGBODY
( GO G3918 )
G3917
( SETQ X ( FIRST %DOLIST-VAR ) )
( LET ( ( L ( QUOTE ( 1 2 3 ) ) ) )
( TAGBODY
( GO G4291 )
G4290
( PRINT L )
( SETQ L ( REST L ) )
G4291
( IF ( NOT ( NULL L ) )
( PROGN ( GO G4290 ) ) ) )
nil )
( SETQ %DOLIST-VAR ( CDR %DOLIST-VAR ) )
G3918
( if %DOLIST-VAR
( GO G3917 ) )
)
nil )
|#
#|
( BLOCK nil
( LET* ( ( %DOLIST-VAR ( QUOTE ( A B ) ) ) X )
( DECLARE )
( TAGBODY
( GO G3918 )
G3917
( SETQ X ( FIRST %DOLIST-VAR ) )
( BLOCK nil
( LET ( ( L ( QUOTE ( 1 2 3 ) ) ) )
( DECLARE )
( TAGBODY
( GO G4291 )
G4290
( PRINT L )
( SETQ L ( REST L ) )
G4291
( IF ( NOT ( NULL L ) )
( PROGN ( GO G4290 ) ) ) )
nil ) )
( SETQ %DOLIST-VAR ( CDR %DOLIST-VAR ) )
G3918
( if %DOLIST-VAR
( GO G3917 ) )
)
nil )
)
|#


#|
;; fails				; ;
(dolist (x '(a b))
(do ((l '(1 2 3) (rest l)))
((null l))
(print l)))
|#

#| ;; works
(let ((lambda-list '(x &optional y &key z)))
(dolist (x '(&optional &key))
))
|#

#| ;; works
(let ((lambda-list '(x &optional y &key z)))
(do ((l lambda-list (rest l)))
((null l))
(print l)))
|#


;; broken
#|
(let ((lambda-list '(x &optional y &key z)))
(dolist (x '(&optional &key))
(do ((l lambda-list (rest l)))
((null l))
(print l))))
|#


#|
(let ((lambda-list '(x &optional y &key z)))
(dolist (x '(&optional &key))
(do ((l (rest lambda-list) (rest l)))
((null l))
(let ((variable (first l)))
(print variable)))))
|#


#|
(let ((lambda-list '(x &optional y &key z)))
(dolist (x '(&optional &key))
(do ((l (rest (member x lambda-list)) (rest l)))
((null l))
(let ((variable (first l)))
(print variable)))))
|#
