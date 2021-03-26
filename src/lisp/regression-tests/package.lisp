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

(test-expect-error unintern-4 (unintern 'cl:standard "CL")
                   :type package-error)

;; Make sure that the symbol was not in fact uninterned.
(test unintern-5 (find-symbol "STANDARD" "CL"))

(test unintern-6 (null (unintern :test)))

(test-expect-error unintern-7 (unintern 'core:simple-program-error "CORE")
                   :type package-error)

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
   :type package-error)
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
   :type package-error)
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
 export-not-accessible-symbol
 (let ((sym (gensym)))
   (export sym (find-package :core)))
 :type package-error)

(test
 export-not-accessible-symbol-continue
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
;;; CLHS 11.1.1.2.3 says the opposite - that IMPORT does not change the home
;;; package - but setting the home package seems more useful, so we do that.

(test import-uninterned-symbol-set-home-package
      (eq (find-package :cl-user)
          (let ((symbol (make-symbol (symbol-name :foo))))
            (import symbol (find-package :cl-user))
            (symbol-package symbol))))

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
                   
        
      
      


;;; Name conflict and resolution tests

;;; IMPORT
(let* ((nc0 (make-package "NC0")) (nc1 (make-package "NC1"))
       (sname "TEST") (rname "TSET")
       (s0 (intern sname nc0)) (s1 (intern sname nc1))
       (r0 (intern rname nc0)) (r1 (intern rname nc1)))
  (test import-conflict-0
        (handler-case
            (progn (import s1 nc0) nil)
          (ext:name-conflict (c)
            (let ((candidates (ext:name-conflict-candidates c)))
              (or (equal candidates (list s0 s1))
                  (equal candidates (list s1 s0)))))))
  ;; Test that RESOLVE-CONFLICT only allows the conflicting symbols
  ;; (and also, transfers control)
  (test-expect-error import-conflict-1
                     (block nil
                       (handler-bind
                           ((ext:name-conflict
                              (lambda (c)
                                (invoke-restart
                                 (find-restart 'ext:resolve-conflict c) r0)
                                (return nil))))
                         (progn (import s1 nc0) nil))))
  ;; Resolving a conflict in favor of the new symbol
  (test import-conflict-2
        (handler-bind
            ((ext:name-conflict
               (lambda (c)
                 (invoke-restart (find-restart 'ext:resolve-conflict c) s1))))
          (progn (import s1 nc0)
                 ;; Check that s0 has been uninterned and replaced by s1
                 (and (null (symbol-package s0))
                      (equal (multiple-value-list (find-symbol sname nc0))
                             (list s1 :internal))))))
  ;; Resolving a conflict in favor of the old symbol
  (test import-conflict-3
        (handler-bind
            ((ext:name-conflict
               (lambda (c)
                 (invoke-restart (find-restart 'ext:resolve-conflict c) r0))))
          (progn (import r1 nc0)
                 (equal (multiple-value-list (find-symbol rname nc0))
                        (list r0 :internal)))))
  (delete-package nc0) (delete-package nc1))

;;; USE-PACKAGE: Conflicts between a present symbol and a newly inherited symbol
(let* ((nc0 (make-package "NC0")) (nc1 (make-package "NC1"))
       (aname "A") (a0 (intern aname nc0)) (a1 (intern aname nc1))
       (bname "B") (b0 (intern bname nc0)) (b1 (intern bname nc1)))
  (export (list a1 b1) nc1)
  ;; Exactly two conflicts are signaled and resolvable
  (test use-package-conflict-0
        (let ((a-resolved nil) (b-resolved nil))
          (block nil
            (handler-bind
                ((ext:name-conflict
                   (lambda (c)
                     (let ((candidates (ext:name-conflict-candidates c)))
                       (cond ((member a1 candidates)
                              (if (and (not a-resolved)
                                       (or (equal candidates (list a0 a1))
                                           (equal candidates (list a1 a0))))
                                  (setf a-resolved t)
                                  (return nil))
                              (invoke-restart 'ext:resolve-conflict a0))
                             ((member b1 candidates)
                              (if (and (not b-resolved)
                                       (or (equal candidates (list b0 b1))
                                           (equal candidates (list b1 b0))))
                                  (setf b-resolved t)
                                  (return nil))
                              (invoke-restart 'ext:resolve-conflict b1))
                             (t (return nil)))))))
              (use-package nc1 nc0))
            (and a-resolved b-resolved))))
  ;; a0, the old present symbol, was chosen: a0 shadows.
  (test use-package-conflict-1
        (and (equal (multiple-value-list (find-symbol aname nc0))
                    (list a0 :internal))
             (member a0 (package-shadowing-symbols nc0))))
  ;; b1, the new symbol, was chosen: b0 is uninterned.
  (test use-package-conflict-2
        (and (equal (multiple-value-list (find-symbol bname nc0))
                    (list b1 :inherited))
             (null (symbol-package b0))))
  (delete-package nc0) (delete-package nc1))

;;; USE-PACKAGE: Conflicts between two inherited symbols
;;; These are separate in part because as far as I can tell, SBCL currently gets
;;; this slightly wrong by interning a new symbol in the child package, instead
;;; of shadowing-import-ing one of the existing ones.
(let* ((par0 (make-package "PAR0")) (par1 (make-package "PAR1"))
       (aname "A") (a0 (intern aname par0)) (a1 (intern aname par1))
       (bname "B") (b0 (intern bname par0)) (b1 (intern bname par1))
       (chil (make-package "CHIL" :use (list par0))))
  (export (list a0 b0) par0) (export (list a1 b1) par1)
  ;; Exactly two conflicts are signaled and resolvable
  (test use-package-conflict-3
        (let ((a-resolved nil) (b-resolved nil))
          (block nil
            (handler-bind
                ((ext:name-conflict
                   (lambda (c)
                     (let ((candidates (ext:name-conflict-candidates c)))
                       (cond ((member a1 candidates)
                              (if (and (not a-resolved)
                                       (or (equal candidates (list a0 a1))
                                           (equal candidates (list a1 a0))))
                                  (setf a-resolved t)
                                  (return nil))
                              (invoke-restart 'ext:resolve-conflict a0))
                             ((member b1 candidates)
                              (if (and (not b-resolved)
                                       (or (equal candidates (list b0 b1))
                                           (equal candidates (list b1 b0))))
                                  (setf b-resolved t)
                                  (return nil))
                              (invoke-restart 'ext:resolve-conflict b1))
                             (t (return nil)))))))
              (use-package par1 chil))
            (and a-resolved b-resolved))))
  ;; a0 from par0 was chosen by shadowing-import-ing it.
  (test use-package-conflict-4
        (and (equal (multiple-value-list (find-symbol aname chil))
                    (list a0 :internal))
             (member a0 (package-shadowing-symbols chil))))
  ;; b1 from par1 was chosen by shadowing-import-ing it.
  (test use-package-conflict-5
        (and (equal (multiple-value-list (find-symbol bname chil))
                    (list b1 :internal))
             (member b1 (package-shadowing-symbols chil))))
  (delete-package chil) (delete-package par0) (delete-package par1))

(let* ((par0 (make-package "PAR0")) (par1 (make-package "PAR1"))
       (par2 (make-package "PAR2")) (chil (make-package "CHIL"))
       (sname "TEST") (s0 (intern sname par0)) (s1 (intern sname par1))
       (s2 (intern sname par2)) (sc (intern sname chil)))
  (shadow (list sname) chil)
  (export s0 par0) (export s1 par1) (export s2 par2)
  (use-package (list par0 par1 par2) chil)
  (test unintern-conflict-0
        (handler-case (unintern sc chil)
          (ext:name-conflict (c)
            (let ((candidates (ext:name-conflict-candidates c)))
              (and (= (length candidates) 3)
                   (member s0 candidates :test #'eq)
                   (member s1 candidates :test #'eq)
                   (member s2 candidates :test #'eq))))))
  ;; Conflict can be resolved
  (test unintern-conflict-1
        (handler-bind
            ((ext:name-conflict
               (lambda (c)
                 (declare (ignore c))
                 (invoke-restart 'ext:resolve-conflict s2))))
          (progn (unintern sc chil) t)))
  ;; sc is actually uninterned
  (test unintern-conflict-2 (null (symbol-package sc)))
  ;; s2 was shadowing-import-ed
  (test unintern-conflict-3
        (and (equal (multiple-value-list (find-symbol sname chil))
                    (list s2 :internal))
             (member s2 (package-shadowing-symbols chil))))
  (delete-package chil)
  (delete-package par0) (delete-package par1) (delete-package par2))
