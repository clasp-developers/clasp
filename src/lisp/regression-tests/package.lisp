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

(let ((pkg (make-package "KARSTEN-NEW" :nicknames (list "pepito"))))
  (test-expect-error
   nicknames-3
   (let ((p2 (make-package "KARSTEN-OLD" :nicknames (list "pepito")))))
   :type core:simple-package-error)
  (when (find-package "KARSTEN-NEW")
    (delete-package (find-package "KARSTEN-NEW")))
  (when (find-package "KARSTEN-OLD")
    (delete-package (find-package "KARSTEN-OLD"))))

(let ((pkg (make-package "KARSTEN-NEW")))
  (test-expect-error
   packages-1
   (let ((p2 (make-package "KARSTEN-NEW"))))
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

