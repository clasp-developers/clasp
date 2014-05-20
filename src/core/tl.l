
(defun pp (h) (print h))

(defvar *a* '(pp z))












(defvar *b* `( ,(car *a*) ,@(cdr *a*)))










(defvar *c* `` *a* ( ,(car *a*) ,@(cdr *a*) ) )

(print (% "*a* = %s" (repr *a*)))

(print (% "*b* = %s" (repr *b*)))
(print (% "*c* = %s" (repr *c*)))
