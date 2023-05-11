;;(in-package :core)
(eval-when (eval compile load) (core:select-package :core))


(if (boundp 'lambda-list-keywords)
    nil ; don't redefine
    (defconstant lambda-list-keywords
      '(&ALLOW-OTHER-KEYS
        &AUX &BODY &ENVIRONMENT &KEY
        &OPTIONAL &REST
        &VA-REST
        &WHOLE) ))


;; Temporary check-type - everything is true
(funcall #'(setf macro-function)
         #'(lambda (whole env) (declare (ignore whole env)) t)
         'check-type)

(defun 1- (num) (- num 1))
(defun 1+ (num) (+ num 1))


(defun constantly (object)
  #'(lambda (&rest arguments) (declare (ignore arguments)) object))


(defun simple-program-error (e1 &rest args)
  (apply 'error e1 args))

(defun simple-reader-error (stream e1 &rest args)
  (declare (ignore stream))
  (apply 'error e1 args))


(funcall #'(setf macro-function)
         #'(lambda (whole env)
             (declare (ignore env))
             (let ((val (cadr whole)))
               `(return-from nil ,val)))
         'return)

#| --- loose this - its in evalmacros where ecl had it |#
#+clasp-min
(funcall #'(setf macro-function)
         #'(lambda (whole env)
                     (declare (ignore env))
                     "Syntax: (psetq {var form}*)
Similar to SETQ, but evaluates all FORMs first, and then assigns each value to
the corresponding VAR.  Returns NIL."
                     (let ((l (cdr whole))
                           (forms nil)
                           (bindings nil))
                       (block nil
                         (tagbody
                          top
                            (if (endp l) (return (list* 'LET* (nreverse bindings) (nreverse (cons nil forms)))))
                            (let ((sym (gensym)))
                              (push (list sym (cadr l)) bindings)
                              (push (list 'setq (car l) sym) forms))
                            (setq l (cddr l))
                            (go top)))))
         'psetq)



(funcall #'(setf macro-function)
         #'(lambda (def env)
             (declare (ignore env))
             `(car (the cons ,(cadr def))))
         'cons-car)
(funcall #'(setf macro-function)
         #'(lambda (def env)
             (declare (ignore env))
             `(cdr (the cons ,(cadr def))))
         'cons-cdr)


;;
;;   "Return true if OBJECT is the same as some tail of LIST, otherwise false."
;;
(defun tailp (object list)
  (if (null list)
      (null object)
    (do ((list list (cdr list)))
        ((atom (cdr list)) (or (eql object list) (eql object (cdr list))))
      (if (eql object list)
          (return t)))))
;;
;;   "Return a copy of LIST before the part which is the same as OBJECT."
;;

;;; Definition from CLHS 14.2.30 (LDIFF, TAILP)
(defun ldiff (list object)
  (unless (listp list)
    (error 'simple-type-error
           :format-control "Not a proper list or a dotted list.; ~s."
           :format-arguments (list list)
           :datum list
           :expected-type 'list))
  (do ((list list (cdr list))
       (r '() (cons (car list) r)))
      ((atom list)
       (if (eql list object) (nreverse r) (nreconc r list)))
    (when (eql object list)
      (return (nreverse r)))))


;; in-package macro is re-defined in evalmacros.lisp
(funcall #'(setf macro-function)
         #'(lambda (whole env)
             (declare (ignore env))
             `(eval-when (:compile-toplevel :load-toplevel :execute)
                (si::select-package ,(string (cadr whole)))
                *package*))
         'in-package)

(funcall #'(setf macro-function)
         #'(lambda (w e)
             (declare (ignore e))
             (let ((key (cadr w))
                           (element (caddr w)))
               `(if ,key
                    (funcall ,key ,element)
                    ,element)))
         'apply-key)

;;   "Add ITEM to LIST unless it is already a member."
(defun adjoin (item list &key key (test #'eql) test-not)
  (when test-not
    (setq test (complement test-not)))
  (if (member (apply-key key item) list :key key :test test)
      list
    (cons item list)))




;;
;; This is defined in ecl>>lsp>>load.d
;; It keeps track of the current source location
;; I should use it.
(defparameter *source-location* '("-no-file-" 0 . 0))


;; Required by REGISTER-GLOBAL in cmp/cmpvar.lisp
(funcall #'(setf macro-function)
         #'(lambda (w e)
             (declare (ignore e))
             (let ((item (cadr w))
                   (place (caddr w)))
               `(setq ,place (adjoin ,item ,place))))
         'pushnew)


(defun hash-table-iterator (hash-table)
  (let ((pairs (core:hash-table-pairs hash-table))
        (hash-index 0))
    (function (lambda ()
      (if (>= hash-index (length pairs))
          nil
          (let* ((key (elt pairs hash-index))
                 (val (elt pairs (incf hash-index))))
            (incf hash-index)
            (values t key val)))))))

;   "Substitute data of ALIST for subtrees matching keys of ALIST."
(defun sublis (alist tree &key key (test #'eql) test-not)
  (when test-not
    (setq test (complement test-not)))
  (labels ((sub (subtree)
                (let ((assoc (assoc (apply-key key subtree) alist :test test)))
                  (cond
                   (assoc (cdr assoc))
                   ((atom subtree) subtree)
                   (t (let ((car (sub (car subtree)))
                            (cdr (sub (cdr subtree))))
                        (if (and (eq car (car subtree)) (eq cdr (cdr subtree)))
                            subtree
                          (cons car cdr))))))))
    (sub tree)))
;   "Substitute data of ALIST for subtrees matching keys of ALIST destructively."
(defun nsublis (alist tree &key key (test #'eql) test-not)
  (when test-not
    (setq test (complement test-not)))
  (labels ((sub (subtree)
                (let ((assoc (assoc (apply-key key subtree) alist :test test)))
                  (cond
                   (assoc (cdr assoc))
                   ((atom subtree) subtree)
                   (t
                    (rplaca subtree (sub (car subtree)))
                    (rplacd subtree (sub (cdr subtree)))
                    subtree)))))
    (sub tree)))


(defun invoke-unix-debugger ()
  (gdb "invoking unix debugger"))


(defun signal-type-error (datum expected-type)
  (error 'type-error :datum datum :expected-type expected-type))

#+(or)
(defun inform (fmt &rest args)
  (apply #'fmt t fmt args))

(in-package :cl)

(defun warn (x &rest args)
  (core:fmt t "WARN: {} {}%N" x args))


(defun class-name (x)
  (core:name-of-class x))

(defun invoke-debugger (cond)
  (core:invoke-internal-debugger cond))

(export 'class-name)
