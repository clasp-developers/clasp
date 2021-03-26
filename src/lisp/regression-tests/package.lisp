(in-package #:clasp-tests)

;; Test for #433: unexport not accepting designators
(let* ((package-name (symbol-name (gensym)))
       (package (make-package package-name)))
  (export (list (intern "FOO" package) (intern "BAR" package)) package)
  (test unexport-designators
        (unexport (list (find-symbol "FOO" package) (find-symbol "BAR" package))
                  package-name))
  (delete-package package))

;; Test for #417: characters not working as package designators
(let ((package (make-package "Z")))
  (test package-designator-character
        (gentemp "Y" #\Z)) ; signaled an error before fix
  (delete-package package))

;; don't allow to unintern t, nil or any symbol of :cl, :keyword and :core
(test unintern-1
      (null (unintern t)))

(test unintern-2
      (null (unintern nil)))

(test unintern-3
      (null (unintern 'defun)))

(test unintern-4
      (null (unintern 'every (find-package :cl))))

(test unintern-5
      (null (unintern :test)))

(test unintern-6
      (null (unintern 'core:simple-program-error)))

(let ((pkg (make-package "KARSTEN" :nicknames (list "CARLES" "CARLITO"))))
  (delete-package "KARSTEN")
  (test nicknames-1 (null (package-nicknames pkg))))

(test nicknames-2 (prog1
                      (packagep (make-package "KARSTEN-NEW" :nicknames (list "CARLES" "CARLITO")))
                    (delete-package "KARSTEN-NEW")))

;;; used to unintern the symbols in shadowing-import-from when delete-package
(test shadowing-import-1
      (let ((package (defpackage "FOO" (:use)(:shadowing-import-from "CL" "T"))))
        (delete-package package)
        (not (null (symbol-package 'cl:t)))))

(let () (make-package "KARSTEN-NEW" :nicknames (list "pepito"))
  (test-expect-error
   nicknames-3
   (let () (make-package "KARSTEN-OLD" :nicknames (list "pepito")))
   :type core:simple-package-error)
  (when (find-package "KARSTEN-NEW")
    (delete-package (find-package "KARSTEN-NEW")))
  (when (find-package "KARSTEN-OLD")
    (delete-package (find-package "KARSTEN-OLD"))))

(let ()
  (make-package "KARSTEN-NEW")
  (test-expect-error
   packages-1
   (let ()
     (make-package "KARSTEN-NEW"))
   :type core:simple-package-error)
  (when (find-package "KARSTEN-NEW")
    (delete-package (find-package "KARSTEN-NEW"))))

(test-expect-error defpackage-1
                   (eval '(defpackage :test (:size 10)(:size 10)))
                   :type program-error)

(test-expect-error defpackage-2
                   (eval '(defpackage :test1 (:documentation "foo")(:documentation "bar")))
                   :type program-error)

(test issue-699
      (every #'stringp (package-nicknames (find-package :ast-tooling))))

(test issue-699-generic
      (every #'(lambda(package)
                 (cond ((and (every #'stringp (package-nicknames package))
                             (every #'packagep (package-use-list package))
                             (every #'packagep (package-used-by-list package))
                             (stringp (package-name package))))
                       (t (format t "Error in package-tests for ~s" package)
                          nil)))
             (list-all-packages)))

(defpackage :%test% (:use))
(defpackage :%test-foo% (:use))
(defpackage :%test-bar% (:use)(:nicknames :a-nickname))

(test add-nickname-happy-path
      (let ((package (find-package :%test%)))
        (and (null (package-nicknames package))
             (let ()
               (ext:package-add-nickname package :future-common-lisp)
               (and (package-nicknames package)
                    (find-package :future-common-lisp))))))

(test-expect-error add-nickname-inexisting-package
    (let ((package-desig :inexistant-package))
      (ext:package-add-nickname package-desig :a-nickname))
    :type package-error)

(test-expect-error add-nickname-already-in-use
    (let ((package-desig :%test%))
      (ext:package-add-nickname package-desig :a-nickname))
    :type package-error)

(test-expect-error add-nickname-twice
    (let ((package-desig :%test%))
      (ext:package-add-nickname package-desig :a-new-nickname)
      (ext:package-add-nickname package-desig :a-new-nickname))
    :type package-error)

(test remove-nickname-happy-path
      (let ((package (find-package :%test-foo%)))
        (and (null (package-nicknames package))
             (let ()
               (ext:package-add-nickname package :very-future-common-lisp)
               (and (package-nicknames package)
                    (find-package :very-future-common-lisp)))
             (let ()
               (ext:package-remove-nickname package :very-future-common-lisp)
               (null (package-nicknames package))
               (null (find-package :very-future-common-lisp))))))

(test remove-nickname-inexistant-nickname
      (null (ext:package-remove-nickname (find-package :%test-foo%) :inexistant-nickname)))

(test remove-nickname-of-other-package
      (null (ext:package-remove-nickname (find-package :%test-foo%) :a-nickname)))

(test-expect-error remove-nickname-inexisting-package-existing-nickname
    (let ((package-desig :inexistant-package))
      (and (null (find-package package-desig))
           (ext:package-remove-nickname package-desig :a-nickname)))
    :type package-error)

(test-expect-error remove-nickname-inexisting-package-inexisting-nickname
    (let ((package-desig :inexistant-package))
      (and (null (find-package package-desig))
           (ext:package-remove-nickname package-desig :inexistant-nickname)))
                   :type package-error)

(defpackage #:foo-bar-1
  (:use)
  (:export #:test))

(defpackage #:foo-bar-2
  (:use)
  (:export #:test))

(defpackage #:foo-bar-3
  (:use))

(test
 import-same-symbol-twice
 (let ()                   
   (import 'foo-bar-1:test (find-package :foo-bar-3))
   (import 'foo-bar-1:test (find-package :foo-bar-3))))

(test-expect-error
 import-different-symbol-same-name-twice
 (let ()                   
   (import 'foo-bar-1:test (find-package :foo-bar-3))
   (import 'foo-bar-2:test (find-package :foo-bar-3)))
 :type package-error)

(test-expect-error
 export-not-accessable-symbol
 (let ((sym (gensym)))
   (export sym (find-package :core)))
 :type package-error)

(test
 export-not-accessable-symbol-continue
 (let ((sym (gensym)))
   (handler-bind ((error #'(lambda(c)
                             (declare (ignore c))
                             (invoke-restart 'continue))))
     (export sym (find-package :core)))
   (multiple-value-bind
         (symbol status)
       (find-symbol (symbol-name sym) (find-package :core))
     (and symbol (eq :external status)))))

(defpackage #:foo-bar-4
  (:use))

(defpackage #:foo-bar-5
  (:use :foo-bar-4)
  (:intern #:foo-bar-6))

(test-expect-error
 export-name_conflict_in_other_package
 (let ((symbol '#:foo-bar-6))
   (import symbol :foo-bar-4)
   (export (find-symbol (symbol-name symbol) (find-package :foo-bar-4)) :foo-bar-4))
 :type package-error)

;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_import.htm
;;; If any symbol to be imported has no home package (i.e., (symbol-package symbol) => nil),
;;; import sets the home package of the symbol to package.

(test import-uinterned-symbol-set-home-package
      (eq (find-package :cl-user)
          (let ((symbol (make-symbol (symbol-name :foo))))
            (import symbol (find-package :cl-user))
            (symbol-package symbol))))

(test-expect-error
 import-uinterned-symbol-set-home-package-twice
 (let ()
   (let ((symbol (make-symbol (symbol-name :bar))))
     (import symbol (find-package :cl-user)))
   (let ((symbol (make-symbol (symbol-name :bar))))
     (import symbol (find-package :cl-user))))
 :type package-error)

;;; Exceptional Situations: http://www.lispworks.com/documentation/HyperSpec/Body/f_uninte.htm
;;; Giving a shadowing symbol to unintern can uncover a name conflict that had previously
;;; been resolved by the shadowing. If package A uses packages B and C, A contains a shadowing
;;; symbol x, and B and C each contain external symbols named x, then removing the shadowing
;;; symbol x from A will reveal a name conflict between b:x and c:x if those two symbols are distinct
;;; In this case unintern will signal an error.

;;; The issue is also mentionned in the source code
(test ansi-tests-UNINTERN.8
 (let ()
  (when (find-package "H")
    (delete-package "H"))
  (when (find-package "G1")
    (delete-package "G1"))
  (when (find-package "G2")
    (delete-package "G2"))
  (LET* ((PG1 (MAKE-PACKAGE "G1" :USE NIL))
         (PG2 (MAKE-PACKAGE "G2" :USE NIL))
         (PH (MAKE-PACKAGE "H" :USE (LIST PG1 PG2))))
    (SHADOW "FOO" PH)
    (LET ((GSYM1 (INTERN "FOO" PG1))
          (GSYM2 (INTERN "FOO" PG2)))
      (EXPORT GSYM1 PG1)
      (EXPORT GSYM2 PG2)
      (MULTIPLE-VALUE-BIND
            (SYM1 ACCESS1)
          (FIND-SYMBOL "FOO" PH)
        (and
         (EQUAL (LIST SYM1)
                (PACKAGE-SHADOWING-SYMBOLS PH))
         (NOT (EQ SYM1 GSYM1))
         (NOT (EQ SYM1 GSYM2))
         (EQ (SYMBOL-PACKAGE SYM1) PH)
         (EQ ACCESS1 :INTERNAL)
         (EQUAL (SYMBOL-NAME SYM1) "FOO")
         ;;; this returns nil in clasp, t.m. the error in not signaled
         (HANDLER-CASE
             (PROGN (UNINTERN SYM1 PH) NIL)
           (ERROR (C)
             (FORMAT T
                     "Properly threw an error: ~S~%"
                     C)
             T))))))))

;;; From various ansi-tests, modified
(defun safely-delete-package (pkg)
  (when (find-package pkg)
    (delete-package pkg)))

(defun set-up-packages ()
  (safely-delete-package "B")
  (safely-delete-package "A")
  (safely-delete-package "Q")
  (safely-delete-package "Z")
  (defpackage "A"
    (:use)
    (:nicknames "Q")
    (:export "FOO"))
  (defpackage "B"
    (:use "A")
    (:export "BAR")))

;;; clhs make-package
;;; A correctable error is signaled if the package-name or any of the nicknames
;;; is already the name or nickname of an existing package.
(test continuable-error-for-make-package-existing-package
      (numberp
       (let ((continue-restart-found nil))
         (set-up-packages)
         (handler-case
             (handler-bind
                 ((package-error #'(lambda(condition)
                                     (setq continue-restart-found
                                           (position 'continue (compute-restarts condition)
                                                     :key #'restart-name :test #'eq)))))
               (make-package "A"))
           (package-error () nil))
         continue-restart-found)))

(test continuable-error-for-make-package-existing-package-in-nicknames
      (numberp
       (let ((continue-restart-found nil))
         (set-up-packages)
         (handler-case
           (handler-bind
               ((package-error #'(lambda(condition)
                                   (setq continue-restart-found
                                         (position 'continue (compute-restarts condition)
                                                   :key #'restart-name :test #'eq)))))
             (make-package "Z" :nicknames (list "A")))
           (package-error () nil))
         continue-restart-found)))

(test-expect-error
 delete-package-in-use
      (progn
        (set-up-packages)
        ;;; to delete package-a should cerror, if package B is still alive
        ;;; test here just that it errors, not that is calls cerror
        (safely-delete-package "A"))
 :type package-error)

;;; Test that we get an cerror
(test continuable-error-for-delete-package-use-package
      (numberp
       (let ((continue-restart-found nil))
         (set-up-packages)
         (handler-case
           (handler-bind
               ((package-error #'(lambda(condition)
                                   (setq continue-restart-found
                                         (position 'continue (compute-restarts condition)
                                                   :key #'restart-name :test #'eq)))))
             (safely-delete-package "A"))
           (package-error () nil))
         continue-restart-found)))

;;; From ansi, fails in clasp with llvm-sys:function, see issue 1142
;;; Direct test (loop for s being the symbols of "LLVM-SYS" when (string= "FUNCTION" (symbol-name s)) collect s)
(test find-all-symbols.1
      (null
       (let ((all-packages (list-all-packages)))
         (loop for
               package
               in
               all-packages
               append
                  (let ((failures nil))
                    (do-symbols (sym package failures)
                      (when (eql (symbol-package sym) package)
                        (let* ((name (symbol-name sym))
                               (similar (find-all-symbols name))
                               (similar2 (find-all-symbols sym)))
                          (unless
                              (and (member sym similar)
                                   (subsetp similar similar2)
                                   (subsetp similar2 similar)
                                   (loop for
                                         sym2
                                         in
                                         similar
                                         always
                                            (string= name (symbol-name sym2))))
                            (push sym failures))))))))))

;;; from discussion in https://github.com/clasp-developers/clasp/pull/1152
(test delete-package-use-packages-correctly
      (let ((par0 (make-package "PAR0"))
            (par1 (make-package "PAR1"))
            (par2 (make-package "PAR2"))
            (chil (make-package "CHIL")))
        (use-package (list par0 par1 par2) chil)
        (delete-package chil)
        (delete-package par0)
        (delete-package par1)
        (delete-package par2)
        t))
                   
        
      
      


