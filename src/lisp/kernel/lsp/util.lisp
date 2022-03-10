
(in-package :core)

;;
;; TODO: Rewrite this in C++ when you get the chance - a lot of stuff depends on it
;;
(defun nconc (&rest lists)
  "Concatenate LISTS by changing them."
  (setq lists (do ((p lists (cdr p)))
		  ((or (car p) (null p)) p)))
  (do* ((top (car lists))
	(splice top)
	(here (cdr lists) (cdr here)))
      ((null here) top)
    (rplacd (last splice) (car here))
    (if (car here)
      (setq splice (car here)))))

(defun tailp (object list)
  "Return true if OBJECT is the same as some tail of LIST, otherwise false."
  (if (null list)
      (null object)
    (do ((list list (cdr list)))
	((atom (cdr list)) (or (eql object list) (eql object (cdr list))))
      (if (eql object list)
	  (return t)))))

(defun ldiff (list object)
  "Return a copy of LIST before the part which is the same as OBJECT."
  (unless (eql list object)
    (do* ((result (list (car list)))
	  (splice result)
	  (list (cdr list) (cdr list)))
	((atom list) (if (eql list object) (rplacd splice nil)) result)
      (if (eql list object)
	  (return result)
	(setq splice (cdr (rplacd splice (list (car list)))))))))

;; stuff



