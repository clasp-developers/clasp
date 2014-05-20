
;;(init-load "loop.lsp")

(trace loop-do-for
       loop-translate-1
       loop-translate
       loop-hack-iteration
       loop-do-for
       loop-make-variable)

(loop for i in '(1 2 3) do (print i))



#|  SBCL version

(load "lowp.lsp")

(lowp for i in '(1 2 3) do (print i))


(trace sb-loop::loop-hack-iteration
       sb-loop::loop-do-for
       sb-loop::loop-translate
       sb-loop::loop-pop-source)

(apropos "loop-source-code")
(apropos "loop-hack")
(apropos "loop-make-variable")
(apropos "loop-pop-source")

(defvar shadow-me)
(setq shadow-me 10)

(defun call-sub-sub ()
  (print (list " sub-sub shadow-me = " shadow-me)))


(defun call-sub ()
  (print (list " sub shadow-me = " shadow-me))
  (let ((shadow-me "inner"))
    (print (list "  inside sub shadow-me = " shadow-me))
    (call-sub-sub))
  (print (list "2 sub shadow-me = " shadow-me))
)

(defun test-shadow (shadow-me)
;;  (declare (special shadow-me))
  (print (list "test-shadow shadow-me =  " shadow-me))
  (call-sub))

shadow-me

(test-shadow 9999)








 (defparameter *p* 1)
;; =>  *P*
 *p*
;; =>  1
 (constantp '*p*)
;;=>  false
 (setq *p* 2)
;;=>  2
 (defparameter *p* 3)
;;=>  *P*
 *p* 
;;=>  3

 (defvar *v* 1)
;; =>  *V*
 (setq *v* 2)

 (defun foo (*p*)
   (print (list "   (specialp *p*) = " (specialp *p*)))
   (print (list "foo passed *p* = " *p*))
   (bar)
   (let ((*p* 'p) (*v* 'v))
     (bar)))


;;=>  FOO
 (defun bar ()
   (print (list "   (specialp *p*) = " (specialp *p*)))
   (print (list "*p* = " *p* "  *v* = " *v*)))
;;=>  BAR
 (foo 999)
;; =>  (P V)
 (bar)
|#

