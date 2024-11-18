(in-package #:clasp-tests)

;;; BTB compilation is a little different from regular Lisp compilation.
;;; We want it to work on any function at all, including closures and
;;; stuff, and when it does it should allow seamless replacement.

;;; Can we compile closures at all?
(test btb.closure-1
      (let ((c (funcall (cmp:bytecompile
                         '(lambda (x) (lambda () x)))
                        119)))
        (multiple-value-bind (cc warningsp failurep)
            (compile nil c)
          (values (funcall c) (funcall cc) warningsp failurep)))
      (119 119 nil nil))

;;; Do compiled closures keep the same cell?
(test btb.closure-2
      (multiple-value-bind (read write)
          (funcall (cmp:bytecompile
                    '(lambda (x)
                      (values (lambda () x)
                       (lambda (y) (setq x y)))))
                   237)
        (multiple-value-bind (cread rwarnings rfailure)
            (compile nil read)
          (multiple-value-bind (cwrite wwarnings wfailure)
              (compile nil write)
            (values (funcall read)
                    (funcall write 18)
                    (funcall cread)
                    (funcall cwrite 33)
                    (funcall read)
                    rwarnings rfailure wwarnings wfailure))))
      (237 18 18 33 33 nil nil nil nil))

;;; Do compiled closures order multiple variables correctly?
;;; As of this writing clasp-cleavir orders closure variables
;;; nondeterministically, so we do this a few times to be sure.
(test btb.closure-3
      (let ((c (funcall (cmp:bytecompile '(lambda (x y)
                                           (lambda () (list x y))))
                        10 382)))
        (loop repeat 7
              collect (multiple-value-bind (f warningsp failurep)
                          (compile nil c)
                        (if (or warningsp failurep)
                            (return (values warningsp failurep))
                            (funcall f)))))
      (((10 382) (10 382) (10 382) (10 382) (10 382) (10 382) (10 382))))

;;; Can we handle variables that optimization deletes?
(test btb.closure-4
      (let ((c (funcall (cmp:bytecompile '(lambda (x y)
                                           ;; Here we rely on
                                           ;; bytecomp not optimizing.
                                           (lambda () (if t x y))))
                        7 19)))
        (multiple-value-bind (f warningp failurep)
            (compile nil c)
          (values (funcall f) warningp failurep)))
      (7 nil nil))

;;; Does LOAD-TIME-VALUE with a normal object work OK?
(test btb.ltv-1
      (multiple-value-bind (f warningsp failurep)
          (compile nil (cmp:bytecompile
                        '(lambda () (load-time-value (+ 189 911)))))
        (values (funcall f) warningsp failurep))
      (1100 nil nil))
(test btb.ltv-1-readonly
      (multiple-value-bind (f warningsp failurep)
          (compile nil (cmp:bytecompile
                        '(lambda () (load-time-value (+ 189 911) t))))
        (values (funcall f) warningsp failurep))
      (1100 nil nil))

;;; An object being unserializable shouldn't matter
(defclass undumpable () ())

(test btb.ltv-2
      (multiple-value-bind (f warningsp failurep)
          (compile nil (cmp:bytecompile
                        '(lambda () (load-time-value
                                     (make-instance 'undumpable)))))
        (values (class-name (class-of (funcall f)))
                warningsp failurep))
      (undumpable nil nil))
(test btb.ltv-2-readonly
      (multiple-value-bind (f warningsp failurep)
          (compile nil (cmp:bytecompile
                        '(lambda () (load-time-value
                                     (make-instance 'undumpable) t))))
        (values (class-name (class-of (funcall f)))
                warningsp failurep))
      (undumpable nil nil))

;;; Compiled LTV gets the updated LTV, not whatever original value
(test btb.ltv-3
      (let ((c (cmp:bytecompile
                '(lambda () (incf (car (load-time-value (list 0))))))))
        (funcall c) (funcall c)
        (multiple-value-bind (f warningsp failurep) (compile nil c)
          (values (funcall f) warningsp failurep)))
      (3 nil nil))

;;; And keeps updating
(test btb.ltv-4
      (let* ((c (cmp:bytecompile
                 '(lambda () (incf (car (load-time-value (list 0)))))))
             (cc (compile nil c)))
        (funcall cc) (funcall cc)
        (funcall cc))
      (3))

;;; And updates are to the original object.
(test btb.ltv-5
      (let ((c (cmp:bytecompile
                '(lambda () (incf (car (load-time-value (list 0))))))))
        (multiple-value-bind (cc warningsp failurep) (compile nil c)
          (funcall cc) (funcall cc)
          (values (funcall c) warningsp failurep)))
      (3 nil nil))

;;; meister ran into this in some complex cando code
;;; errored out with #<VARIABLE R> fell through ETYPECASE expression.
;;; Wanted one of LINEAR-DATUM (CONS LINEAR-DATUM)
(test btb.misc-1
      (let ((f (cmp:bytecompile
                '(lambda ()
                  (let ((ef (let (r)
                              (flet ((f (a) (eql r a))) #'f))))
                    ef)))))
        (multiple-value-bind (cc warningsp failurep) (compile nil f)
          (declare (ignore cc))
          (values warningsp failurep)))
      (nil nil))
