(in-package #:static-gfs)

(defun symbol< (symbol1 symbol2)
  (or (string< (symbol-name symbol1) (symbol-name symbol2))
      (string< (package-name (symbol-package symbol1))
               (package-name (symbol-package symbol2)))))

;; Returns values:
;; INITARGS, a sorted list of initargs.
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
                              (list (ext:constant-form-value key env)
                                    (gensym (symbol-name key))
                                    form))
                      into x
                  else return (values nil nil nil nil)
                  finally (return
                            (let ((sorted
                                    ;; Stable sort because if the same
                                    ;; keyword is specified twice, we
                                    ;; want the value forms to be in
                                    ;; the right order for evaluation.
                                    (stable-sort (copy-list x) #'symbol<
                                                 :key #'first)))
                              (values
                               (mapcar #'first sorted)
                               (mapcar #'second sorted)
                               (mapcar #'cdr x)
                               t))))
            ;; odd list length
            (values nil nil nil nil)))
      ;; circular or dotted list
      (values nil nil nil nil)))

(defun create-instance (class &rest initargs) (apply #'make-instance class initargs))

(define-compiler-macro make-instance
    (&whole form class-designatorf &rest initargs &environment env)
  (let ((class-designator
          (and (constantp class-designatorf env)
               (ext:constant-form-value class-designatorf env))))
    (unless class-designator
      (return-from make-instance form))
    ;; We don't handle the constant class case correctly, since
    ;; we don't need to worry about redefining the name there...
    ;; So we skip it. It's rare anyway.
    (unless (symbolp class-designator)
      (return-from make-instance form))
    (multiple-value-bind (keys syms bindings validp)
        (extract initargs env)
      (if validp
          ;; keeping binding in case we do handle literal classes later.
          (let ((classn class-designator)
                (cellg (gensym "CONSTRUCTOR-CELL")))
            `(let ((,cellg
                     (cell-function
                      (load-time-value
                       (ensure-constructor-cell ',classn ',keys))))
                   ,@bindings)
               (funcall ,cellg ,@syms)))
          ;; We have non constant arguments, but we can still do just a bit.
          ;; (Compiler macro expansion won't recurse, as (find-class ...) is
          ;;  not a constant.)
          `(make-instance (find-class ',class-designator) ,@initargs)))))
