
(defmacro time-run (op &optional (n 7))
  (let ((start (gensym))
        (diff (gensym)))
    `(progn
       (let ((,start (get-internal-real-time)))
         (funcall ,op (expt 10 ,n))
         (let ((,diff (float (/ (- (get-internal-real-time) ,start) internal-time-units-per-second))))
           (format t "~6,4f 10^~d ~30a~%" ,diff ,n ,op))))))


;;;; Run tests

(defun do-nothing (n) (dotimes (i n)))
(time-run 'do-nothing)

(defun do-5+6 (n) (dotimes (i n) (+ 5 6)))
(time-run 'do-5+6 6)

(defparameter *d* (make-list 1000))
(defun do-cdr (n) (dotimes (i n) (cdr *d*)))
(time-run 'do-cdr)

(defun do-car (n) (dotimes (i n) (cdr *d*)))
(time-run 'do-car)

(defun do-nthcdr5 (n) (dotimes (i n) (nthcdr 5 *d*)))
(time-run 'do-nthcdr5)

(defparameter *ht-eql* (make-hash-table :test 'eql))
(dolist (l *d*) (setf (gethash l *ht-eql*) l))
(defparameter *ht-eq* (make-hash-table :test 'eq))
(dolist (l *d*) (setf (gethash l *ht-eq*) l))
(defparameter *ht-equal* (make-hash-table :test 'equal))
(dolist (l *d*) (setf (gethash l *ht-equal*) l))
;; Make the list circular
(rplacd (last *d*) *d*)

(defun do-notinline-gethash-eq (n) (declare (notinline gethash))  (dotimes (i n) (gethash *d* *ht-eq*) (setf *d* (cdr *d*))))
(time-run 'do-notinline-gethash-eq)

#+clasp
(progn
  (declaim (inline gethash))
  (core::generate-direct-call-defun (core:magic-intern "cl__gethash") (key hash-table &optional default-value) "wrapped_cl__gethash_T_spT_spT_sp" )
  (declaim (notinline gethash)))

(defun do-inline-gethash-eq (n) (declare (inline gethash))  (dotimes (i n) (gethash *d* *ht-eq*) (setf *d* (cdr *d*))))
(time-run 'do-inline-gethash-eq)

#+clasp
(progn
  (defun do-notinline-gethash3-eq (n) (declare (notinline gethash))  (dotimes (i n) (core:gethash3 *d* *ht-eq* nil) (setf *d* (cdr *d*))))
  (time-run 'do-notinline-gethash3-eq))


(defun do-notinline-gethash-eql (n) (declare (notinline gethash))  (dotimes (i n) (gethash *d* *ht-eql*) (setf *d* (cdr *d*))))
(time-run 'do-notinline-gethash-eql)

(defun do-notinline-gethash-equal (n) (declare (notinline gethash))  (dotimes (i n) (gethash *d* *ht-equal*) (setf *d* (cdr *d*))))
(time-run 'do-notinline-gethash-equal)

(defun do-sxhash (n) (dotimes (i n) (sxhash 1234)))
(time-run 'do-sxhash 6)

(defun foo (x y z) (declare (ignore x y z)) nil)
(defun call-foo (n) (dotimes (i n) (foo 1 2 3)))
(time-run 'call-foo)

(defparameter *dump* nil)
(defun do-alloc10 (n) (dotimes (i n) (setq *dump* (make-list 10))))
(time-run 'do-alloc10 6)

(defparameter *dump* nil)
(defun do-alloc1000 (n) (dotimes (i n) (setq *dump* (make-list 1000))))
(time-run 'do-alloc1000 4)

(defun do-alloc1000000 (n) (dotimes (i n) (setq *dump* (make-list 1000000))))
(time-run 'do-alloc1000000 1)

(defun do-alloc10000000 (n) (dotimes (i n) (setq *dump* (make-list 10000000))))
(time-run 'do-alloc10000000 0)


