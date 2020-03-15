(in-package #:clasp-tests)

#|
Functions, Macros, and Special Forms:

documentation (x function) (doc-type (eql 't))
documentation (x function) (doc-type (eql 'function))
documentation (x list) (doc-type (eql 'function))
documentation (x list) (doc-type (eql 'compiler-macro))
documentation (x symbol) (doc-type (eql 'function))
documentation (x symbol) (doc-type (eql 'compiler-macro))
documentation (x symbol) (doc-type (eql 'setf))
(setf documentation) new value (x function) (doc-type (eql 't))
(setf documentation) new value (x function) (doc-type (eql 'function))
(setf documentation) new value (x list) (doc-type (eql 'function))
(setf documentation) new value (x list) (doc-type (eql 'compiler-macro))
(setf documentation) new value (x symbol) (doc-type (eql 'function))
(setf documentation) new value (x symbol) (doc-type (eql 'compiler-macro))
(setf documentation) new value (x symbol) (doc-type (eql 'setf))
Method Combinations:

documentation (x method-combination) (doc-type (eql 't))
documentation (x method-combination) (doc-type (eql 'method-combination))
documentation (x symbol) (doc-type (eql 'method-combination))
(setf documentation) new value (x method-combination) (doc-type (eql 't))
(setf documentation) new value (x method-combination) (doc-type (eql 'method-combination))
(setf documentation) new value (x symbol) (doc-type (eql 'method-combination))
Methods:

documentation (x standard-method) (doc-type (eql 't))
(setf documentation) new value (x standard-method) (doc-type (eql 't))
tes:

documentation (x package) (doc-type (eql 't))
(setf documentation) new value (x package) (doc-type (eql 't))
Types, Classes, and Structure Names:

documentation (x standard-class) (doc-type (eql 't))
documentation (x standard-class) (doc-type (eql 'type))
documentation (x standard-class) (doc-type (eql 't))
documentation (x standard-class) (doc-type (eql 'type))
documentation (x symbol) (doc-type (eql 'type))
documentation (x symbol) (doc-type (eql 'structure))
(setf documentation) new value (x standard-class) (doc-type (eql 't))
(setf documentation) new value (x standard-class) (doc-type (eql 'type))
(setf documentation) new value (x standard-class) (doc-type (eql 't))
(setf documentation) new value (x standard-class) (doc-type (eql 'type))
(setf documentation) new value (x symbol) (doc-type (eql 'type))
(setf documentation) new value (x symbol) (doc-type (eql 'structure))
Variables:

documentation (x symbol) (doc-type (eql 'variable))
(setf documentation) new value (x symbol) (doc-type (eql 'variable))
|#

(defun foo-test-describe (a b c)
  "to test describe"
  (list a b c))

(defun test-documentation-with-args (object &optional (doc-category 'function))
  (let ((doc (documentation object doc-category)))
    (if doc
        (let ((describe-string 
               (with-output-to-string (*standard-output*)
                 (describe object)))
              (args (core:function-lambda-list object)))
          (and (search doc describe-string)
               (search (write-to-string args :escape t :readably t) describe-string)))
        (with-output-to-string (*standard-output*)
          (describe object)))))

(defun test-documentation-without-args (object &optional (doc-category 'function))
  (let ((doc (documentation object doc-category)))
    (if doc
         (let ((describe-string 
                (with-output-to-string (*standard-output*)
                  (describe object))))
           (search doc describe-string))
         (with-output-to-string (*standard-output*)
                  (describe object)))))

(test describe-function-documentation (test-documentation-with-args 'foo-test-describe))

(test describe-function-documentation-a (test-documentation-with-args 'core:function-lambda-list))

(test describe-function-documentation-macro (test-documentation-with-args 'cl:defconstant))

(defun stupid-function (a) a)

(define-compiler-macro stupid-function (&whole form arg)
  "This is an empty compiler macro"
  `(list ,arg))

(test describe-compiler-macro (test-documentation-without-args 'stupid-function 'compiler-macro))

(test describe-special-from (test-documentation-with-args 'progn))

(defconstant a-silly-constant 23 "and the constant doc")

(test describe-constant (test-documentation-without-args 'a-silly-constant 'variable))

(defparameter *a-silly-var* 23 "and the special doc")

(test describe-special (test-documentation-without-args '*a-silly-var* 'variable))

(deftype a-silly-type ()
  "Documentation of a type"
  'fixnum)

(test describe-type (test-documentation-without-args 'a-silly-type 'type))

(defstruct silly-struct-normal "Documentation for a normal struct" a (b 23) (c "nada" :type string)(d 25 :read-only t))
(defstruct (silly-struct-vector (:type vector)) "Documentation for a vector struct" a b c)
(defstruct (silly-struct-list (:type vector)) "Documentation for a list struct" a b c)

(test describe-struct-a (test-documentation-without-args 'silly-struct-normal 'structure))

(test describe-struct-b (test-documentation-without-args 'silly-struct-vector 'structure))

(test describe-struct-c (test-documentation-without-args  'silly-struct-list 'structure))

(defun (setf nada) (new old)
  "asdjhaskdhj"
  (list new old))

(test simple-setf (documentation '(setf nada) 'function))

(defun setf-access-fn (value) "a fn doc" value)
(defun setf-update-fn (old new)(list old new))
(defsetf setf-access-fn setf-update-fn "A short detsetf documentation")

(test describe-setf-short (test-documentation-without-args 'setf-access-fn 'setf))

(defun setf-access-fn-for-long-setf (value) "a fn doc" value)

(defsetf setf-access-fn-for-long-setf (sequence start &optional end) (new-sequence)
  "A long form of setf"
   `(progn (replace ,sequence ,new-sequence
                    :start1 ,start :end1 ,end)
           ,new-sequence))

(test describe-setf-long (test-documentation-without-args 'setf-access-fn-for-long-setf 'setf))

(defclass describe-class ()
  ((foo :initform 23 :initarg :foo :accessor describe-class))
  (:documentation "A doc for a class"))

(test describe-class (test-documentation-without-args (find-class 'describe-class) t))

(test describe-instance
      (let* ((object (make-instance 'describe-class)))
        (with-output-to-string (*standard-output*)
          (describe object))))

(defpackage nonsense-for-describe
  (:use :cl)
  (:documentation "A package doc")
  (:nicknames :a-package-nickname)
  (:shadow #:car)
  (:local-nicknames (:mine :cl-user)))

(defpackage nonsense-for-describe-uses
  (:use :cl :nonsense-for-describe))

(test describe-package (test-documentation-without-args (find-package :nonsense-for-describe) t))

(defgeneric blah-blah (a b c)
  (:documentation "A generic function documentation")
  (:method ((a describe-class) (b string) c)
    "A method documentation"
    (list a b c)))

(test describe-generic-function (test-documentation-without-args #'blah-blah  'function))

(test describe-method-via-generic-function
      (test-documentation-without-args
       (find-method #'blah-blah nil (list (find-class 'describe-class)(find-class 'string)(find-class t)))
       'function))

#|
(define-method-combination test1 :identity-with-one-argument t) 

(describe (CLOS::SEARCH-METHOD-COMBINATION 'test1))
|#

(test describe-alltogether
      (with-output-to-string (*standard-output*)
        (let ((objects-to-describe
               (list
                'foo-test-describe
                'core:function-lambda-list
                'cl:defconstant
                'stupid-function
                'progn
                'a-silly-constant
                '*a-silly-var*
                'a-silly-type
                'silly-struct-normal 
                'silly-struct-vector 
                'silly-struct-list
                '(setf nada)
                'setf-access-fn
                'setf-access-fn-for-long-setf
                (find-class 'describe-class)
                (make-instance 'describe-class)
                (find-package :nonsense-for-describe)
                #'blah-blah
                (find-method #'blah-blah nil (list (find-class 'describe-class)(find-class 'string)(find-class t)))
                #\newline
                (code-char 256)
                "234987234iuziuz"
                most-positive-fixnum
                (1+ most-positive-fixnum)
                pi
                (/ 3 23)
                (complex .4 .5)
                (vector 1 2 3)
                (make-array (list 2 2) :initial-contents '((1 2)(3 4)))
                (make-hash-table))))
          (dolist (object objects-to-describe)
            (describe object)))))

