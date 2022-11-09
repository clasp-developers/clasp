(in-package "SI")

(defun search-print-circle (object)
  (multiple-value-bind
        (code present-p)
      (gethash object *circle-stack*)
    (if (not (fixnump *circle-counter*))
        (cond ((not present-p)
	       ;; Was not found before
	       (setf (gethash object *circle-stack*) nil)
	       0)
	      ((null code)
	       ;; Second reference
	       (setf (gethash object *circle-stack*) t)
	       1)
	      (t
	       ;; Further references
	       2))
	(cond ((or (not present-p) (null code))
	       ;; Is not referenced or was not found before
	       0)
	      ((eql code t)
	       ;; Reference twice but had no code yet
               (incf *circle-counter*)
	       (setf (gethash object *circle-stack*)
		     *circle-counter*)
	       (- *circle-counter*))
	      (t code)))))
	       
(defun write-object-with-circle (object stream function)
  (if (and *print-circle*
           (not (null object))
           (not (fixnump object))
           (not (characterp object))
           (or (not (symbolp object)) (null (symbol-package object))))
            ;;; *print-circle* and an object that might have a circle
      (if (null *circle-counter*)
          (let* ((hash (make-hash-table :test 'eq
                                        :size 1024))
                 (*circle-counter* t)
                 (*circle-stack* hash))
            (write-object-with-circle object (make-broadcast-stream) function)
            (setf *circle-counter* 0)
            (write-object-with-circle object stream function)
            (clrhash hash)
            object)
          (let ((code (search-print-circle object)))
            (cond ((not (fixnump *circle-counter*))
                   ;; We are only inspecting the object to be printed.
                   ;; Only print X if it was not referenced before
                   (if (not (zerop code))
                       object
                       (funcall function object stream)))
                  ((zerop code)
                   ;; Object is not referenced twice
                   (funcall function object stream))
                  ((minusp code)
                   ;; Object is referenced twice. We print its definition 
                   (write-char #\# stream)
                   (let ((*print-radix* nil)
                         (*print-base* 10))
                     (write-ugly-object (- code) stream))
                   (write-char #\= stream)
                   (funcall function object stream))
                  (t
                   ;; Second reference to the object
                   (write-char #\# stream)
                   (let ((*print-radix* nil)
                         (*print-base* 10))
                     (write-ugly-object code stream))
                   (write-char #\# stream)
                   object))))
      ;;; live is good, print simple
      (funcall function object stream)))
