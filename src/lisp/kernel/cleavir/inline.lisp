

(in-package :clasp-cleavir)
(export '(*function-inline-asts*))

(defvar *function-inline-asts* (make-hash-table :test #'equal))

(defun proclaim-hook (decl)
  (let ((head (car decl)))
    (cond
      ((eq head 'cl:ftype)
       (format t "*** Do something with proclaim ftype ~s~%" decl))
      ;; Add other clauses here
      (t (warn "Add support for proclaim ~s~%" decl)))))

(defun global-function-inline-ast (name)
  (gethash name *function-inline-asts*))

(defun do-inline-hook (name function)
  (when (core:declared-global-inline-p name)
    (let ((ast (cleavir-generate-ast:generate-ast 
                function
                clasp-cleavir:*clasp-env* 
                clasp-cleavir:*clasp-system*)))
      (setf (gethash name clasp-cleavir:*function-inline-asts*) ast))))
  
;; Generate an AST and save it for inlining if the
;; function is proclaimed as inline
(defun defun-inline-hook (name function)
  (let ((ast-gs (gensym "AST")))
    (when (core:declared-global-inline-p name)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (when core:*do-inline-hook*
           (funcall core:*do-inline-hook* (QUOTE ,name) (QUOTE ,function)))))))
    
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq core:*defun-inline-hook* #'defun-inline-hook)
  (setq core:*do-inline-hook* #'do-inline-hook)
  (setq core:*proclaim-hook* #'proclaim-hook))


;;; Stubs to keep the already compiled code working
(clasp-cleavir:cleavir-compile
 'cleavir-primop:consp
 '(lambda (x) (if (cleavir-primop:consp x) t nil )))
  
(clasp-cleavir:cleavir-compile
 'cleavir-primop:car
 '(lambda (x)
   (if (cleavir-primop:consp x)
       (cleavir-primop:car x)
       (if (null x)
           nil
           (error "Cannot get car of non list: ~s" x)))))
  
(clasp-cleavir:cleavir-compile
 'cleavir-primop:cdr
 '(lambda (x)
   (if (cleavir-primop:consp x)
       (cleavir-primop:cdr x)
       (if (null x)
           nil
           (error "Cannot get cdr of non list: ~s" x)))))
  
(clasp-cleavir:cleavir-compile
 'cleavir-primop:rplaca
 '(lambda (p v)
   (if (cleavir-primop:consp p)
       (progn
         (cleavir-primop:rplaca p v)
         p)
       (error "Cannot rplaca non-cons ~s" p))))

(clasp-cleavir:cleavir-compile
 'cleavir-primop:rplacd
 '(lambda (p v)
   (if (cleavir-primop:consp p)
       (progn
         (cleavir-primop:rplacd p v)
         p)
       (error "Cannot rplacd non-cons ~s" p))))



(progn
  (declaim (inline cl:consp))
  (defun cl:consp (x)
    (if (cleavir-primop:consp x) t nil)))

(progn
  (declaim (inline cl:car))
  (defun cl:car (x)
    (if (consp x)
        (cleavir-primop:car x)
        (if (null x)
            nil
            (error "Cannot get car of non-list ~s" x)))))

(progn
  (declaim (inline cl:cdr))
  (defun cl:cdr (x)
    (if (consp x)
        (cleavir-primop:cdr x)
        (if (null x)
            nil
            (error "Cannot get cdr of non-list ~s" x)))))

(progn
  (declaim (inline cl:rplaca))
  (defun cl:rplaca (p v)
    (if (consp p)
        (progn
          (cleavir-primop:rplaca p v)
          p)
        (error "Cannot rplaca non-cons ~s" p))))

(progn
  (declaim (inline cl:rplacd))
  (defun cl:rplacd (p v)
    (if (consp p)
        (progn
          (cleavir-primop:rplacd p v)
          p)
        (error "Cannot rplacd non-cons ~s" p))))


(defpackage "PRIMOP"
  (:export #:convert-to-bignum
           #:inlined-two-arg-+
           #:inlined-two-arg--
           #:inlined-two-arg-<
           #:inlined-two-arg-<=
           #:inlined-two-arg-=
           #:inlined-two-arg->
           #:inlined-two-arg->=
           ))

(progn
  (in-package :primop)
  (defun convert-to-bignum (z)
    (if (> z 0)
        (- z (expt 2 63))
        (+ z (expt 2 63))))
  (defmacro def-inline-arithmetic (inlined-name cleavir-primop generic-name)
    (let ((x (gensym))
          (y (gensym))
          (z (gensym)))
      `(progn
         (declaim (inline ,inlined-name))
         (defun ,inlined-name (,x ,y)
           (block nil
             (tagbody
                (if (cleavir-primop:typeq ,x fixnum)
                    (if (cleavir-primop:typeq ,y fixnum)
                        (cleavir-primop:let-uninitialized (,z)
                                                          (if (,cleavir-primop ,x ,y ,z)
                                                              (return ,z)
                                                              (return (core:convert-overflow-result-to-bignum ,z))))
                        (go generic))
                    (go generic))
                ;; ... Other tests
              generic
                (return (,generic-name ,x ,y))))))))
  (def-inline-arithmetic primop:inlined-two-arg-+ cleavir-primop:fixnum-+ core:two-arg-+)
  (def-inline-arithmetic primop:inlined-two-arg-- cleavir-primop:fixnum-- core:two-arg--)
  ;;; Need * / and other primops
  (defmacro def-inline-comparison (inlined-name cleavir-primop generic-name)
    (let ((x (gensym))
          (y (gensym)))
      `(progn
         (declaim (inline ,inlined-name))
         (defun ,inlined-name (,x ,y)
           (block nil
             (tagbody
                (if (cleavir-primop:typeq ,x fixnum)
                    (if (cleavir-primop:typeq ,y fixnum)
                        (if (,cleavir-primop ,x ,y)
                            (return t)
                            (return nil)))
                    (go generic))
                ;; ... Other tests
              generic
                (return (,generic-name ,x ,y))))))))
  (def-inline-comparison primop:inlined-two-arg-<  cleavir-primop:fixnum-<  core:two-arg-<)
  (def-inline-comparison primop:inlined-two-arg-<= cleavir-primop:fixnum-<= core:two-arg-<=)
  (def-inline-comparison primop:inlined-two-arg-=  cleavir-primop:fixnum-=  core:two-arg-=)
  (def-inline-comparison primop:inlined-two-arg->  cleavir-primop:fixnum->  core:two-arg->)
  (def-inline-comparison primop:inlined-two-arg->= cleavir-primop:fixnum->= core:two-arg->=))

(progn
  (define-compiler-macro + (&rest numbers)
    (core::expand-associative '+ 'primop:inlined-two-arg-+ numbers 0))
  (define-compiler-macro - (&rest numbers)
    (core::expand-associative '- 'primop:inlined-two-arg-- numbers 0))
  (define-compiler-macro < (&rest numbers)
    (core::expand-compare 'primop:inlined-two-arg-< numbers))
  (define-compiler-macro <= (&rest numbers)
    (core::expand-compare 'primop:inlined-two-arg-<= numbers))
  (define-compiler-macro = (&rest numbers)
    (core::expand-compare 'primop:inlined-two-arg-= numbers))
  (define-compiler-macro > (&rest numbers)
    (core::expand-compare 'primop:inlined-two-arg-> numbers))
  (define-compiler-macro >= (&rest numbers)
    (core::expand-compare 'primop:inlined-two-arg->= numbers))
  (define-compiler-macro 1+ (x)
    `(primop:inlined-two-arg-+ ,x 1))
  (define-compiler-macro 1- (x)
    `(primop:inlined-two-arg-- ,x 1)))
