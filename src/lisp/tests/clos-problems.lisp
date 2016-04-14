;;; The following code will trigger a problem in the new CLOS code
;;;
;;; Needs both :around and :before methods to trigger
;;;
;;; The problem is in standard-compute-effective-method at the bottom
;;;
#|      (if around
	  (let ((main (if (or before after)
			  (list
			   (standard-main-effective-method before primary after))
			  primary)))
	    (setf around (nreverse around))
	    (combine-method-functions (first around)
				      (nconc (rest around) main)))
	  (if (or before after)
	      (standard-main-effective-method before primary after)
	      (combine-method-functions (first primary) (rest primary)))))))
|#

(fmakunbound 'foo)
(defgeneric foo (x &rest keys &key verbose))
(defmethod foo :around (x &rest keys &key verbose)
  (format t "In foo :around  keys: ~a~%" keys)
  (call-next-method))
(defmethod foo (x &rest keys &key verbose)
  (format t "In foo (eql 'foo) keys: ~a~%" keys))
(trace clos::standard-compute-effective-method)
(trace clos::standard-main-effective-method)
;; this should work
(foo 'bar :verbose t)
(defmethod foo :before (x &rest keys &key verbose)
  (format t "In foo :before  keys: ~a~%" keys))
;; This will crash
(foo 'bar :verbose t)




;;; Another problem
;;; call-next-method with arguments doesn't propagate the arguments
(fmakunbound 'bar)
(defclass ca () ())
(defclass cb (ca) ())
(defclass cc (cb) ())
(defgeneric bar (x y))
(defmethod bar ((x ca) y) (format t "bar (ca) y --> ~a~%" y))
(defmethod bar ((x cb) y) (format t "bar (cb) y --> ~a~%" y) (call-next-method))
(defmethod bar ((x cc) y) (format t "bar (cc) y --> ~a~%" y) (call-next-method))
(defmethod bar :around ((x cc) y) (format t "bar around y --> ~a~%" y) (call-next-method x 999))
(defparameter *a* (make-instance 'cc))
(bar *a* 2)
;;clasp output
bar around y --> 2
bar (cc) y --> 999
bar (cb) y --> 2
bar (ca) y --> 2
NIL
;;sbcl output
* (bar *a* 2)
bar around y --> 2
bar (cc) y --> 999
bar (cb) y --> 999
bar (ca) y --> 999
NIL
* 
