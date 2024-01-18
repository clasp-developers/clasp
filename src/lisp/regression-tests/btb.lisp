(in-package #:clasp-tests)

;;; BTB compilation is a little different from regular Lisp compilation.
;;; We want it to work on any function at all, including closures and
;;; stuff, and when it does it should allow seamless replacement.

;;; Can we compile closures at all?
(test btb.closure-1
      (let ((c (funcall (cmp:bytecompile
                         '(lambda (x) (lambda () x)))
                        119)))
        (values (funcall c)
                (funcall (compile nil c))))
      (119 119))

;;; Do compiled closures keep the same cell?
(test btb.closure-2
      (multiple-value-bind (read write)
          (funcall (cmp:bytecompile
                    '(lambda (x)
                      (values (lambda () x)
                       (lambda (y) (setq x y)))))
                   237)
        (values (funcall read)
                (funcall write 18)
                (funcall (compile nil read))
                (funcall (compile nil write) 33)
                (funcall read)))
      (237 18 18 33 33))

;;; Do compiled closures order multiple variables correctly?
;;; As of this writing clasp-cleavir orders closure variables
;;; nondeterministically, so we do this a few times to be sure.
(test btb.closure-3
      (let ((c (funcall (cmp:bytecompile '(lambda (x y)
                                           (lambda () (list x y))))
                        10 382)))
        (loop repeat 7 collect (funcall (compile nil c))))
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
      (funcall
       (compile nil (cmp:bytecompile
                     '(lambda () (load-time-value (+ 189 911))))))
      (1100))
(test btb.ltv-1-readonly
      (funcall
       (compile nil (cmp:bytecompile
                     '(lambda () (load-time-value (+ 189 911) t)))))
      (1100))

;;; An object being unserializable shouldn't matter
(defclass undumpable () ())

(test btb.ltv-2
      (class-name
       (class-of
        (funcall
         (compile nil (cmp:bytecompile
                       '(lambda () (load-time-value
                                    (make-instance 'undumpable))))))))
      (undumpable))
(test btb.ltv-2-readonly
      (class-name
       (class-of
        (funcall
         (compile nil (cmp:bytecompile
                       '(lambda () (load-time-value
                                    (make-instance 'undumpable) t)))))))
      (undumpable))

;;; Compiled LTV gets the updated LTV, not whatever original value
(test btb.ltv-3
      (let ((c (cmp:bytecompile
                '(lambda () (incf (car (load-time-value (list 0))))))))
        (funcall c) (funcall c)
        (funcall (compile nil c)))
      (3))

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
      (let* ((c (cmp:bytecompile
                 '(lambda () (incf (car (load-time-value (list 0)))))))
             (cc (compile nil c)))
        (funcall cc) (funcall cc)
        (funcall c))
      (3))
