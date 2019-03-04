(in-package #:cmp)

(core:bclasp-define-compiler-macro
 find-class (&whole form class &optional (errorp t) env)
 (format *debug-io* "In find-class with class ~a errorp ~a~%" class errorp)
 (let ((class-holder-gs (gensym)))
   (if (and (constantp class env) (constantp errorp env))
       (let ((class (ext:constant-form-value class env)))
         (if (eq (ext:constant-form-value errorp) nil)
             (progn
               (format *debug-io* "Case 1 - in find-class with class ~a errorp ~a~%" class errorp)
               `(let ((,class-holder-gs (load-time-value (core:find-class-holder ',class))))
                  (format t "In find-class for ~a~%" ',class)
                  (if (ext:class-unboundp ,class-holder-gs)
                      nil
                      (ext:class-get ,class-holder-gs))))
             (progn
               (format *debug-io* "Case 2 - in find-class with class ~a errorp ~a~%" class errorp)
               `(let ((,class-holder-gs (load-time-value (core:find-class-holder ',class))))
                (format t "In find-class for ~a~%" ',class)
                (if (ext:class-unboundp ,class-holder-gs)
                    (error 'ext:undefined-class :name ',class)
                    (ext:class-get ,class-holder-gs))))))
       (progn
         (format *debug-io* "Fallback - in find-class with class ~a errorp ~a~%" class errorp)
         form))))


;;; generic functions are usually hard to work with at compile time,
;;; but we can at least take care of the simple method on SYMBOL,
;;; which cannot be customized.
(core:bclasp-define-compiler-macro make-instance (&whole form class &rest initargs &environment env)
  (when (constantp class env)
    (let ((class (ext:constant-form-value class env)))
      (when (symbolp class)
        (return-from make-instance
          `(make-instance (find-class ',class) ,@initargs)))))
  form)

