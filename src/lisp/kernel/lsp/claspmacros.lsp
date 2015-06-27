
(in-package :ffi)

#+clasp
(defmacro c-inline (fn-name (&rest values) (&rest c-types) return-type C-code &key one-liner side-effects)
  `(,fn-name ,@values))
(export 'c-inline)


(in-package :ext)
(defmacro ext::special-var (name)
  `(ext::special-var ,name))

(defmacro ext::lexical-var (name depth index)
  `(ext::lexical-var ,name ,depth ,index))



;;
;; Some helper macros for working with iterators
;;
;;

(in-package :ext)

(defmacro do-c++-iterator ((i iterator) &rest body)
  (let ((begin-gs (gensym))
        (end-gs (gensym))
        (cur (gensym)))
    `(multiple-value-bind (,begin-gs ,end-gs)
         ,iterator
       (do* ((,cur ,begin-gs (sys:iterator-step ,cur))
             (,i (sys:iterator-unsafe-element ,cur) (sys:iterator-unsafe-element ,cur)))
            ((eql ,cur ,end-gs))
         ,@body
         )
       )))


(defmacro map-c++-iterator (code iterator)
  (let ((val (gensym)))
    `(progn
       (do-c++-iterator (,val ,iterator) (funcall ,code ,val))
       nil)))

(export '(do-c++-iterator map-c++-iterator))


(in-package :cl)

(defmacro unwind-protect (protected-form &rest cleanup-forms)
  `(core:funwind-protect (lambda () ,protected-form) (lambda () ,@cleanup-forms)))

#+(or)
(defmacro catch (tag &rest forms)
  `(core:catch-function ,tag (lambda () (declare (core:lambda-name catch-lambda)) ,@forms)))

#+(or)
(defmacro throw (tag result-form) `(core:throw-function ,tag ,result-form))

#+(or)
(defmacro progv (symbols values &rest forms)
  `(core:progv-function ,symbols ,values #'(lambda () ,@forms)))



(in-package :core)
#+clasp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSE-MACRO is needed by sp_macrolet and the compiler
;;;
(defun parse-macro (name vl body &optional env)
  (multiple-value-bind (lblock ppn doc)
      (si::expand-defmacro name vl body)
    lblock))

(export 'parse-macro)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; core:debug-message is a macro to mimic the core:debug-message special operator
;;;
(defmacro debug-message (msg) nil)
(export 'debug-message)


(progn
  ;; proper-list-p code from Robert Strandh's Cleavir code
  (defun proper-list-p (object)
    (cond  ((null object) t)
           ((atom object) nil)
           (t (let ((slow object)
                    (fast (cdr object)))
                (declare (type cons slow))
                (tagbody
                 again
                   (unless (consp fast)
                     (return-from proper-list-p
                       (if (null fast) t nil)))
                   (when (eq fast slow)
                     (return-from proper-list-p nil))
                   (setq fast (cdr fast))
                   (unless (consp fast)
                     (return-from proper-list-p
                       (if (null fast) t nil)))
                   (setq fast (cdr fast))
                   (setq slow (cdr slow))
                   (go again))))))

  (defun do+ (numbers)
    (if (consp numbers)
        (if (= (length numbers) 1)
            (car numbers)
            `(core:binary-+ ,(car numbers) ,(do+ (cdr numbers))))
        (if (null numbers)
            0
            (error "The + operator can not be part of a form that is a dotted list."))))

  (define-compiler-macro + (&rest numbers)
    (if (null numbers)
        0
        (if (proper-list-p numbers)
            (do+ numbers)
            (error "The + operator requires a proper list not ~s" numbers))))

  (defun do- (minuend subtrahends)
    (if (consp subtrahends)
        `(core:binary-- ,minuend ,(do+ subtrahends))
        (if (null subtrahends)
            `(core:negate ,minuend)
            (error "The - operator can not be part of a form that is a dotted list."))))

  (define-compiler-macro - (minuend &rest subtrahends)
    (if (proper-list-p subtrahends)
        (do- minuend subtrahends)
        (error "The - operator requires a proper list")))

  (defun do-< (numbers)
    (if (consp numbers)
        (if (= (length numbers) 1)
            t
            (if (= (length numbers) 2)
                `(core:binary-< ,(car numbers) ,(cadr numbers))
                `(if (core:binary-< ,(car numbers) ,(cadr numbers))
                     ,(do-< (cdr numbers))
                     nil)))))

  (defun do-<= (numbers)
    (if (consp numbers)
        (if (= (length numbers) 1)
            t
            (if (= (length numbers) 2)
                `(core:binary-<= ,(car numbers) ,(cadr numbers))
                `(if (core:binary-<= ,(car numbers) ,(cadr numbers))
                     ,(do-<= (cdr numbers))
                     nil)))))

  (defun do-> (numbers)
    (if (consp numbers)
        (if (= (length numbers) 1)
            t
            (if (= (length numbers) 2)
                `(core:binary-> ,(car numbers) ,(cadr numbers))
                `(if (core:binary-> ,(car numbers) ,(cadr numbers))
                     ,(do-> (cdr numbers))
                     nil)))))

  (defun do->= (numbers)
    (if (consp numbers)
        (if (= (length numbers) 1)
            t
            (if (= (length numbers) 2)
                `(core:binary->= ,(car numbers) ,(cadr numbers))
                `(if (core:binary->= ,(car numbers) ,(cadr numbers))
                     ,(do->= (cdr numbers))
                     nil)))))

  (defun do-= (numbers)
    (if (consp numbers)
        (if (= (length numbers) 1)
            t
            (if (= (length numbers) 2)
                `(core:binary-= ,(car numbers) ,(cadr numbers))
                `(if (core:binary-= ,(car numbers) ,(cadr numbers))
                     ,(do-= (cdr numbers))
                     nil)))))

  (define-compiler-macro < (&rest numbers)
    (if (null numbers)
        (error "The < operator requires a list")
        (if (proper-list-p numbers)
            (do-< numbers)
            (error "The < operator requires a proper list not ~s" numbers))))

  (define-compiler-macro <= (&rest numbers)
    (if (null numbers)
        (error "The <= operator requires a list")
        (if (proper-list-p numbers)
            (do-<= numbers)
            (error "The <= operator requires a proper list not ~s" numbers))))

  (define-compiler-macro > (&rest numbers)
    (if (null numbers)
        (error "The > operator requires a list")
        (if (proper-list-p numbers)
            (do-> numbers)
            (error "The > operator requires a proper list not ~s" numbers))))

  (define-compiler-macro >= (&rest numbers)
    (if (null numbers)
        (error "The >= operator requires a list")
        (if (proper-list-p numbers)
            (do->= numbers)
            (error "The >= operator requires a proper list not ~s" numbers))))

  (define-compiler-macro = (&rest numbers)
    (if (null numbers)
        (error "The = operator requires a list")
        (if (proper-list-p numbers)
            (do-= numbers)
            (error "The = operator requires a proper list not ~s" numbers))))

  (define-compiler-macro 1+ (x)
    `(core:binary-+ ,x 1))

  (define-compiler-macro 1- (x)
    `(core:binary-- ,x 1)))

