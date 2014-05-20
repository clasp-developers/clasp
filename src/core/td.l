
(defvar *x* 10)

(defun ppx ()
  (print *x*)
)

(defun px ()
  (ppx))

(px)

(let ((*x* 20))
  (px) )
(px)
