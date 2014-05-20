
;;
;; Test macro
;; test /form/ => t
;;
;; form - a form that is evaluated - if it returns true then continue
;;                                   otherwise a message is displayed
(defparameter *all-tests-passed* t)

(defmacro test (exp)
  (let ((%result (gensym))
	(%saved-exp (gensym)))
    `(let ((,%result ,exp)
	   (,%saved-exp ',exp))
       (if ,%result
	   t
	   (progn
	     (bformat t "FAILED exp = %s\n" ,%saved-exp)
	     (setq *all-tests-passed* nil)
	     )
	   ))))


(defmacro test-equal (x y)
  (let ((%rx (gensym))
	(%ry (gensym))
	(%exp-rx (gensym))
	(%exp-ry (gensym)))
    `(let ((,%rx ,x)
	   (,%ry ,y)
	   (,%exp-rx ',x)
	   (,%exp-ry ',y))
       (if (equal ,%rx ,%ry)
	   (progn
	     (bformat t "SUCCESS (equal %s %s)\n" ,%rx ,%ry)
	     t
	     )
	   (progn
	     (bformat t "FAILED (equal %s %s)\n" ,%exp-rx ,%exp-ry)
	     (bformat t "    ---> (not (equal %s %s))\n" ,%rx ,%ry )
	     (setq *all-tests-passed* nil)
	     )
	   )
       )
    ))





(load "test_specials.lsp")
(load "test_builtins.lsp")
(load "destructuring.lsp")
(load "test_base.lsp")
(load "test_evalmacros.lsp")
(load "test_flet.lsp")


(if *all-tests-passed*
    (print "!\b!\nALL tests passed")
    (print "Some tests failed")
)
