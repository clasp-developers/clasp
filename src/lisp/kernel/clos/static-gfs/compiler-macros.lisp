(in-package #:static-gfs)

;;; Kind of KLUDGEy: During startup we do a fair bit of make-instance-ing,
;;; and it's nice to not compile a bunch of constructors then.
;;; So we track what constructors we need and dump them all way later.
(defvar *constructors-during-build*)
;;; This is based on the build procedure. We load everything and then compile.
;;; The load is serial, so that's when we should be saving everything.
;;; but only for cclasp, which means while bclasp is loading cclasp.
#+bclasp
(eval-when (:load-toplevel)
  (setf *constructors-during-build* nil))

(defmacro precompile-build-constructors ()
  (let ((specs *constructors-during-build*))
    `(progn
       (eval-when (:compile-toplevel)
         ;; stop saving now
         (makunbound '*constructors-during-build*))
       (eval-when (:load-toplevel)
         ,@(loop for (classn . keys) in specs
                 collect `(precompile-constructor ,classn ,keys))))))

;; Returns values:
;; INITARGS, a list of initargs.
;; GENSYMS, a corresponding list of gensyms.
;; BINDINGS, a list of variable bindings.
;; VALIDP: if false, the above are invalid and give up.
;; The intent is that the initial call is equivalent to
;; (let (,@bindings) (fun ,initarg0 ,gensym0 ,initarg1 ,gensym1 ...))
;; where bindings has the original forms, and evaluated in the correct order.
(defun extract (initargs env)
  (if (core:proper-list-p initargs)
      (let ((len (list-length initargs)))
        (if (and len (evenp len))
            ;; list is valid
            (loop for (key form) on initargs by #'cddr
                  if (constantp key env)
                    collect (let ((keyc (ext:constant-form-value key env)))
                              (unless (symbolp keyc)
                                (return (values nil nil nil nil)))
                              (list keyc
                                    (gensym (symbol-name keyc))
                                    form))
                      into x
                  else return (values nil nil nil nil)
                  finally (return
                            (values
                             (mapcar #'first x)
                             (mapcar #'second x)
                             (mapcar #'cdr x)
                             t)))
            ;; odd list length
            (values nil nil nil nil)))
      ;; circular or dotted list
      (values nil nil nil nil)))

(define-compiler-macro make-instance
    (&whole form class-designatorf &rest initargs &environment env)
  (let ((class-designator
          (and (constantp class-designatorf env)
               (ext:constant-form-value class-designatorf env))))
    (multiple-value-bind (keys syms bindings validp)
        (extract initargs env)
      (if validp
          (if class-designator
              ;; Class is constant: get cell at load time
              (let ((cellg (gensym "CONSTRUCTOR-CELL")))
                (when (boundp '*constructors-during-build*)
                  (pushnew (cons class-designator keys)
                           *constructors-during-build*
                           :test #'equal))
                `(let ((,cellg
                         (load-time-value
                          (ensure-constructor-cell ',class-designator ',keys)))
                       ,@bindings)
                   (funcall ,cellg ,@syms)))
              ;; Non-constant class: Get cell at runtime
              `(let (,@bindings)
                 (funcall
                  (ensure-constructor-cell ,class-designatorf ',keys)
                  ,@syms)))
          ;; Non-constant initargs, but maybe we can do just a bit.
          (if (and class-designator (symbolp class-designator))
              `(locally (declare (notinline make-instance))
                 (make-instance (find-class ',class-designator) ,@initargs))
              form)))))
