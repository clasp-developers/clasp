(in-package #:core)

;;; based on the stupidly named export.lisp in clasp sources

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-while-until (test body jmp-op)
    (let ((label (gensym))
          (exit (gensym)))
      `(TAGBODY
          (GO ,exit)
          ,label
          ,@body
          ,exit
          (,jmp-op ,test (GO ,label))))))

(defmacro while (test &body body) (expand-while-until test body 'when))
(defmacro until (test &body body) (expand-while-until test body 'unless))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun filter-dolist-declarations (declarations)
    (let ((a nil))
      (mapc #'(lambda (clause)
                (when (not (and (consp clause)
                             (or (eq (car clause) 'type)
                               (eq (car clause) 'ignore))))
                  (setq a (cons clause a))))
            declarations)
      (nreverse a))))

(defmacro dolist ((var list-form &optional result-form) &body body)
  (multiple-value-bind (declarations body)
      (process-declarations body nil)
    `(block nil
       (let ((%dolist-var ,list-form))
         (while %dolist-var
           (let ((,var (first %dolist-var)))
             (declare ,@declarations)
             (tagbody
                ,@body
                (setq %dolist-var (cdr %dolist-var)))))
         ,(when result-form
            `(let ((,var nil))
               (declare (ignorable ,var)
                        ,@(filter-dolist-declarations declarations))
               ,result-form))))))

(defmacro dotimes ((var count-form &optional result-form) &body body)
  (multiple-value-bind (declarations body)
      (process-declarations body nil)
    (when (and (integerp count-form) (>= count-form 0))
      (setq declarations
            (cons `(type (integer 0 ,count-form) ,var) declarations)))
    `(block nil
       (let ((%dotimes-var ,count-form)
             (,var 0))
         (declare ,@declarations)
         (while (< ,var %dotimes-var)
           ,@body
           (setq ,var (1+ ,var)))
         ,result-form))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-do/do* (op vars test result body)
    (multiple-value-bind (let psetq)
        (ecase op
          ((do) (values 'let 'psetq))
          ((do*) (values 'let* 'setq)))
      (multiple-value-bind (declarations body)
          (process-declarations body nil)
        (multiple-value-bind (inits set)
            (loop for var in vars
                  if (symbolp var)
                    collect `(,var nil) into inits
                  else if (and (consp var) (null (cdr var)))
                         collect `(,(first var) nil) into inits
                  else if (and (consp var) (consp (cdr var)) (null (cddr var)))
                         collect `(,(first var) ,(second var)) into inits
                  else if (and (consp var) (consp (cdr var)) (consp (cddr var))
                            (null (cdddr var)))
                         collect `(,(first var) ,(second var)) into inits
                         and nconc (list (first var) (third var)) into set
                  else do (simple-program-error "Invalid ~s var clause: ~s"
                                                op var)
                  finally (return (values inits set)))
          `(block nil
             (,let (,@inits)
               (declare ,@declarations)
               (until ,test ,@body (,psetq ,@set))
               ,@(or result '(nil)))))))))

(defmacro do ((&rest vars) (test &rest result) &body body)
  (expand-do/do* 'do vars test result body))
(defmacro do* ((&rest vars) (test &rest result) &body body)
  (expand-do/do* 'do* vars test result body))
