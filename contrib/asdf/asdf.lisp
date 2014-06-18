;;; -*- mode: Common-Lisp; Base: 10 ; Syntax: ANSI-Common-Lisp ; buffer-read-only: t; -*-
;;; This is ASDF 3.1.2: Another System Definition Facility.
;;;
;;; Feedback, bug reports, and patches are all welcome:
;;; please mail to <asdf-devel@common-lisp.net>.
;;; Note first that the canonical source for ASDF is presently
;;; <URL:http://common-lisp.net/project/asdf/>.
;;;
;;; If you obtained this copy from anywhere else, and you experience
;;; trouble using it, or find bugs, you may want to check at the
;;; location above for a more recent version (and for documentation
;;; and test files, if your copy came without them) before reporting
;;; bugs.  There are usually two "supported" revisions - the git master
;;; branch is the latest development version, whereas the git release
;;; branch may be slightly older but is considered `stable'

;;; -- LICENSE START
;;; (This is the MIT / X Consortium license as taken from
;;;  http://www.opensource.org/licenses/mit-license.html on or about
;;;  Monday; July 13, 2009)
;;;
;;; Copyright (c) 2001-2014 Daniel Barlow and contributors
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; -- LICENSE END

;;; The problem with writing a defsystem replacement is bootstrapping:
;;; we can't use defsystem to compile it.  Hence, all in one file.

#+xcvb (module ())

(in-package :cl-user)

#+cmu
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf ext:*gc-verbose* nil))

;;; pre 1.3.0 ABCL versions do not support the bundle-op on Mac OS X
#+abcl
(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (and (member :darwin *features*)
               (second (third (sys::arglist 'directory))))
    (push :abcl-bundle-op-supported *features*)))

;; Punt on hard package upgrade: from ASDF1 always, and even from ASDF2 on most implementations.
(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (member :asdf3 *features*)
    (let* ((existing-version
             (when (find-package :asdf)
               (or (symbol-value (find-symbol (string :*asdf-version*) :asdf))
                   (let ((ver (symbol-value (find-symbol (string :*asdf-revision*) :asdf))))
                     (etypecase ver
                       (string ver)
                       (cons (format nil "~{~D~^.~}" ver))
                       (null "1.0"))))))
           (first-dot (when existing-version (position #\. existing-version)))
           (second-dot (when first-dot (position #\. existing-version :start (1+ first-dot))))
           (existing-major-minor (subseq existing-version 0 second-dot))
           (existing-version-number (and existing-version (read-from-string existing-major-minor)))
           (away (format nil "~A-~A" :asdf existing-version)))
      (when (and existing-version
                 (< existing-version-number
                    #+(or allegro clisp lispworks sbcl) 2.0
                    #-(or allegro clisp lispworks sbcl) 2.27))
        (rename-package :asdf away)
        (when *load-verbose*
          (format t "~&; Renamed old ~A package away to ~A~%" :asdf away))))))
;;;; ---------------------------------------------------------------------------
;;;; Handle ASDF package upgrade, including implementation-dependent magic.
;;
;; See https://bugs.launchpad.net/asdf/+bug/485687
;;

(defpackage :uiop/package
  ;; CAUTION: we must handle the first few packages specially for hot-upgrade.
  ;; This package definition MUST NOT change unless its name too changes;
  ;; if/when it changes, don't forget to add new functions missing from below.
  ;; Until then, uiop/package is frozen to forever
  ;; import and export the same exact symbols as for ASDF 2.27.
  ;; Any other symbol must be import-from'ed and re-export'ed in a different package.
  (:use :common-lisp)
  (:export
   #:find-package* #:find-symbol* #:symbol-call
   #:intern* #:export* #:import* #:shadowing-import* #:shadow* #:make-symbol* #:unintern*
   #:symbol-shadowing-p #:home-package-p
   #:symbol-package-name #:standard-common-lisp-symbol-p
   #:reify-package #:unreify-package #:reify-symbol #:unreify-symbol
   #:nuke-symbol-in-package #:nuke-symbol #:rehome-symbol
   #:ensure-package-unused #:delete-package*
   #:package-names #:packages-from-names #:fresh-package-name #:rename-package-away
   #:package-definition-form #:parse-define-package-form
   #:ensure-package #:define-package))

(in-package :uiop/package)

;;;; General purpose package utilities

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun find-package* (package-designator &optional (error t))
    (let ((package (find-package package-designator)))
      (cond
        (package package)
        (error (error "No package named ~S" (string package-designator)))
        (t nil))))
  (defun find-symbol* (name package-designator &optional (error t))
    "Find a symbol in a package of given string'ified NAME;
unless CL:FIND-SYMBOL, work well with 'modern' case sensitive syntax
by letting you supply a symbol or keyword for the name;
also works well when the package is not present.
If optional ERROR argument is NIL, return NIL instead of an error
when the symbol is not found."
    (block nil
      (let ((package (find-package* package-designator error)))
        (when package ;; package error handled by find-package* already
          (multiple-value-bind (symbol status) (find-symbol (string name) package)
            (cond
              (status (return (values symbol status)))
              (error (error "There is no symbol ~S in package ~S" name (package-name package))))))
        (values nil nil))))
  (defun symbol-call (package name &rest args)
    "Call a function associated with symbol of given name in given package,
with given ARGS. Useful when the call is read before the package is loaded,
or when loading the package is optional."
    (apply (find-symbol* name package) args))
  (defun intern* (name package-designator &optional (error t))
    (intern (string name) (find-package* package-designator error)))
  (defun export* (name package-designator)
    (let* ((package (find-package* package-designator))
           (symbol (intern* name package)))
      (export (or symbol (list symbol)) package)))
  (defun import* (symbol package-designator)
    (import (or symbol (list symbol)) (find-package* package-designator)))
  (defun shadowing-import* (symbol package-designator)
    (shadowing-import (or symbol (list symbol)) (find-package* package-designator)))
  (defun shadow* (name package-designator)
    (shadow (string name) (find-package* package-designator)))
  (defun make-symbol* (name)
    (etypecase name
      (string (make-symbol name))
      (symbol (copy-symbol name))))
  (defun unintern* (name package-designator &optional (error t))
    (block nil
      (let ((package (find-package* package-designator error)))
        (when package
          (multiple-value-bind (symbol status) (find-symbol* name package error)
            (cond
              (status (unintern symbol package)
                      (return (values symbol status)))
              (error (error "symbol ~A not present in package ~A"
                            (string symbol) (package-name package))))))
        (values nil nil))))
  (defun symbol-shadowing-p (symbol package)
    (and (member symbol (package-shadowing-symbols package)) t))
  (defun home-package-p (symbol package)
    (and package (let ((sp (symbol-package symbol)))
                   (and sp (let ((pp (find-package* package)))
                             (and pp (eq sp pp))))))))


(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun symbol-package-name (symbol)
    (let ((package (symbol-package symbol)))
      (and package (package-name package))))
  (defun standard-common-lisp-symbol-p (symbol)
    (multiple-value-bind (sym status) (find-symbol* symbol :common-lisp nil)
      (and (eq sym symbol) (eq status :external))))
  (defun reify-package (package &optional package-context)
    (if (eq package package-context) t
        (etypecase package
          (null nil)
          ((eql (find-package :cl)) :cl)
          (package (package-name package)))))
  (defun unreify-package (package &optional package-context)
    (etypecase package
      (null nil)
      ((eql t) package-context)
      ((or symbol string) (find-package package))))
  (defun reify-symbol (symbol &optional package-context)
    (etypecase symbol
      ((or keyword (satisfies standard-common-lisp-symbol-p)) symbol)
      (symbol (vector (symbol-name symbol)
                      (reify-package (symbol-package symbol) package-context)))))
  (defun unreify-symbol (symbol &optional package-context)
    (etypecase symbol
      (symbol symbol)
      ((simple-vector 2)
       (let* ((symbol-name (svref symbol 0))
              (package-foo (svref symbol 1))
              (package (unreify-package package-foo package-context)))
         (if package (intern* symbol-name package)
             (make-symbol* symbol-name)))))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defvar *all-package-happiness* '())
  (defvar *all-package-fishiness* (list t))
  (defun record-fishy (info)
    ;;(format t "~&FISHY: ~S~%" info)
    (push info *all-package-fishiness*))
  (defmacro when-package-fishiness (&body body)
    `(when *all-package-fishiness* ,@body))
  (defmacro note-package-fishiness (&rest info)
    `(when-package-fishiness (record-fishy (list ,@info)))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  #+(or clisp clozure)
  (defun get-setf-function-symbol (symbol)
    #+clisp (let ((sym (get symbol 'system::setf-function)))
              (if sym (values sym :setf-function)
                  (let ((sym (get symbol 'system::setf-expander)))
                    (if sym (values sym :setf-expander)
                        (values nil nil)))))
    #+clozure (gethash symbol ccl::%setf-function-names%))
  #+(or clisp clozure)
  (defun set-setf-function-symbol (new-setf-symbol symbol &optional kind)
    #+clisp (assert (member kind '(:setf-function :setf-expander)))
    #+clozure (assert (eq kind t))
    #+clisp
    (cond
      ((null new-setf-symbol)
       (remprop symbol 'system::setf-function)
       (remprop symbol 'system::setf-expander))
      ((eq kind :setf-function)
       (setf (get symbol 'system::setf-function) new-setf-symbol))
      ((eq kind :setf-expander)
       (setf (get symbol 'system::setf-expander) new-setf-symbol))
      (t (error "invalid kind of setf-function ~S for ~S to be set to ~S"
                kind symbol new-setf-symbol)))
    #+clozure
    (progn
      (gethash symbol ccl::%setf-function-names%) new-setf-symbol
      (gethash new-setf-symbol ccl::%setf-function-name-inverses%) symbol))
  #+(or clisp clozure)
  (defun create-setf-function-symbol (symbol)
    #+clisp (system::setf-symbol symbol)
    #+clozure (ccl::construct-setf-function-name symbol))
  (defun set-dummy-symbol (symbol reason other-symbol)
    (setf (get symbol 'dummy-symbol) (cons reason other-symbol)))
  (defun make-dummy-symbol (symbol)
    (let ((dummy (copy-symbol symbol)))
      (set-dummy-symbol dummy 'replacing symbol)
      (set-dummy-symbol symbol 'replaced-by dummy)
      dummy))
  (defun dummy-symbol (symbol)
    (get symbol 'dummy-symbol))
  (defun get-dummy-symbol (symbol)
    (let ((existing (dummy-symbol symbol)))
      (if existing (values (cdr existing) (car existing))
          (make-dummy-symbol symbol))))
  (defun nuke-symbol-in-package (symbol package-designator)
    (let ((package (find-package* package-designator))
          (name (symbol-name symbol)))
      (multiple-value-bind (sym stat) (find-symbol name package)
        (when (and (member stat '(:internal :external)) (eq symbol sym))
          (if (symbol-shadowing-p symbol package)
              (shadowing-import* (get-dummy-symbol symbol) package)
              (unintern* symbol package))))))
  (defun nuke-symbol (symbol &optional (packages (list-all-packages)))
    #+(or clisp clozure)
    (multiple-value-bind (setf-symbol kind)
        (get-setf-function-symbol symbol)
      (when kind (nuke-symbol setf-symbol)))
    (loop :for p :in packages :do (nuke-symbol-in-package symbol p)))
  (defun rehome-symbol (symbol package-designator)
    "Changes the home package of a symbol, also leaving it present in its old home if any"
    (let* ((name (symbol-name symbol))
           (package (find-package* package-designator))
           (old-package (symbol-package symbol))
           (old-status (and old-package (nth-value 1 (find-symbol name old-package))))
           (shadowing (and old-package (symbol-shadowing-p symbol old-package) (make-symbol name))))
      (multiple-value-bind (overwritten-symbol overwritten-symbol-status) (find-symbol name package)
        (unless (eq package old-package)
          (let ((overwritten-symbol-shadowing-p
                  (and overwritten-symbol-status
                       (symbol-shadowing-p overwritten-symbol package))))
            (note-package-fishiness
             :rehome-symbol name
             (when old-package (package-name old-package)) old-status (and shadowing t)
             (package-name package) overwritten-symbol-status overwritten-symbol-shadowing-p)
            (when old-package
              (if shadowing
                  (shadowing-import* shadowing old-package))
              (unintern* symbol old-package))
            (cond
              (overwritten-symbol-shadowing-p
               (shadowing-import* symbol package))
              (t
               (when overwritten-symbol-status
                 (unintern* overwritten-symbol package))
               (import* symbol package)))
            (if shadowing
                (shadowing-import* symbol old-package)
                (import* symbol old-package))
            #+(or clisp clozure)
            (multiple-value-bind (setf-symbol kind)
                (get-setf-function-symbol symbol)
              (when kind
                (let* ((setf-function (fdefinition setf-symbol))
                       (new-setf-symbol (create-setf-function-symbol symbol)))
                  (note-package-fishiness
                   :setf-function
                   name (package-name package)
                   (symbol-name setf-symbol) (symbol-package-name setf-symbol)
                   (symbol-name new-setf-symbol) (symbol-package-name new-setf-symbol))
                  (when (symbol-package setf-symbol)
                    (unintern* setf-symbol (symbol-package setf-symbol)))
                  (setf (fdefinition new-setf-symbol) setf-function)
                  (set-setf-function-symbol new-setf-symbol symbol kind))))
            #+(or clisp clozure)
            (multiple-value-bind (overwritten-setf foundp)
                (get-setf-function-symbol overwritten-symbol)
              (when foundp
                (unintern overwritten-setf)))
            (when (eq old-status :external)
              (export* symbol old-package))
            (when (eq overwritten-symbol-status :external)
              (export* symbol package))))
        (values overwritten-symbol overwritten-symbol-status))))
  (defun ensure-package-unused (package)
    (loop :for p :in (package-used-by-list package) :do
      (unuse-package package p)))
  (defun delete-package* (package &key nuke)
    (let ((p (find-package package)))
      (when p
        (when nuke (do-symbols (s p) (when (home-package-p s p) (nuke-symbol s))))
        (ensure-package-unused p)
        (delete-package package))))
  (defun package-names (package)
    (cons (package-name package) (package-nicknames package)))
  (defun packages-from-names (names)
    (remove-duplicates (remove nil (mapcar #'find-package names)) :from-end t))
  (defun fresh-package-name (&key (prefix :%TO-BE-DELETED)
                               separator
                               (index (random most-positive-fixnum)))
    (loop :for i :from index
          :for n = (format nil "~A~@[~A~D~]" prefix (and (plusp i) (or separator "")) i)
          :thereis (and (not (find-package n)) n)))
  (defun rename-package-away (p &rest keys &key prefix &allow-other-keys)
    (let ((new-name
            (apply 'fresh-package-name
                   :prefix (or prefix (format nil "__~A__" (package-name p))) keys)))
      (record-fishy (list :rename-away (package-names p) new-name))
      (rename-package p new-name))))


;;; Communicable representation of symbol and package information

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun package-definition-form (package-designator
                                  &key (nicknamesp t) (usep t)
                                    (shadowp t) (shadowing-import-p t)
                                    (exportp t) (importp t) internp (error t))
    (let* ((package (or (find-package* package-designator error)
                        (return-from package-definition-form nil)))
           (name (package-name package))
           (nicknames (package-nicknames package))
           (use (mapcar #'package-name (package-use-list package)))
           (shadow ())
           (shadowing-import (make-hash-table :test 'equal))
           (import (make-hash-table :test 'equal))
           (export ())
           (intern ()))
      (when package
        (loop :for sym :being :the :symbols :in package
              :for status = (nth-value 1 (find-symbol* sym package)) :do
                (ecase status
                  ((nil :inherited))
                  ((:internal :external)
                   (let* ((name (symbol-name sym))
                          (external (eq status :external))
                          (home (symbol-package sym))
                          (home-name (package-name home))
                          (imported (not (eq home package)))
                          (shadowing (symbol-shadowing-p sym package)))
                     (cond
                       ((and shadowing imported)
                        (push name (gethash home-name shadowing-import)))
                       (shadowing
                        (push name shadow))
                       (imported
                        (push name (gethash home-name import))))
                     (cond
                       (external
                        (push name export))
                       (imported)
                       (t (push name intern)))))))
        (labels ((sort-names (names)
                   (sort names #'string<))
                 (table-keys (table)
                   (loop :for k :being :the :hash-keys :of table :collect k))
                 (when-relevant (key value)
                   (when value (list (cons key value))))
                 (import-options (key table)
                   (loop :for i :in (sort-names (table-keys table))
                         :collect `(,key ,i ,@(sort-names (gethash i table))))))
          `(defpackage ,name
             ,@(when-relevant :nicknames (and nicknamesp (sort-names nicknames)))
             (:use ,@(and usep (sort-names use)))
             ,@(when-relevant :shadow (and shadowp (sort-names shadow)))
             ,@(import-options :shadowing-import-from (and shadowing-import-p shadowing-import))
             ,@(import-options :import-from (and importp import))
             ,@(when-relevant :export (and exportp (sort-names export)))
             ,@(when-relevant :intern (and internp (sort-names intern)))))))))


;;; ensure-package, define-package
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun ensure-shadowing-import (name to-package from-package shadowed imported)
    (check-type name string)
    (check-type to-package package)
    (check-type from-package package)
    (check-type shadowed hash-table)
    (check-type imported hash-table)
    (let ((import-me (find-symbol* name from-package)))
      (multiple-value-bind (existing status) (find-symbol name to-package)
        (cond
          ((gethash name shadowed)
           (unless (eq import-me existing)
             (error "Conflicting shadowings for ~A" name)))
          (t
           (setf (gethash name shadowed) t)
           (setf (gethash name imported) t)
           (unless (or (null status)
                       (and (member status '(:internal :external))
                            (eq existing import-me)
                            (symbol-shadowing-p existing to-package)))
             (note-package-fishiness
              :shadowing-import name
              (package-name from-package)
              (or (home-package-p import-me from-package) (symbol-package-name import-me))
              (package-name to-package) status
              (and status (or (home-package-p existing to-package) (symbol-package-name existing)))))
           (shadowing-import* import-me to-package))))))
  (defun ensure-imported (import-me into-package &optional from-package)
    (check-type import-me symbol)
    (check-type into-package package)
    (check-type from-package (or null package))
    (let ((name (symbol-name import-me)))
      (multiple-value-bind (existing status) (find-symbol name into-package)
        (cond
          ((not status)
           (import* import-me into-package))
          ((eq import-me existing))
          (t
           (let ((shadowing-p (symbol-shadowing-p existing into-package)))
             (note-package-fishiness
              :ensure-imported name
              (and from-package (package-name from-package))
              (or (home-package-p import-me from-package) (symbol-package-name import-me))
              (package-name into-package)
              status
              (and status (or (home-package-p existing into-package) (symbol-package-name existing)))
              shadowing-p)
             (cond
               ((or shadowing-p (eq status :inherited))
                (shadowing-import* import-me into-package))
               (t
                (unintern* existing into-package)
                (import* import-me into-package))))))))
    (values))
  (defun ensure-import (name to-package from-package shadowed imported)
    (check-type name string)
    (check-type to-package package)
    (check-type from-package package)
    (check-type shadowed hash-table)
    (check-type imported hash-table)
    (multiple-value-bind (import-me import-status) (find-symbol name from-package)
      (when (null import-status)
        (note-package-fishiness
         :import-uninterned name (package-name from-package) (package-name to-package))
        (setf import-me (intern* name from-package)))
      (multiple-value-bind (existing status) (find-symbol name to-package)
        (cond
          ((and imported (gethash name imported))
           (unless (and status (eq import-me existing))
             (error "Can't import ~S from both ~S and ~S"
                    name (package-name (symbol-package existing)) (package-name from-package))))
          ((gethash name shadowed)
           (error "Can't both shadow ~S and import it from ~S" name (package-name from-package)))
          (t
           (setf (gethash name imported) t))))
      (ensure-imported import-me to-package from-package)))
  (defun ensure-inherited (name symbol to-package from-package mixp shadowed imported inherited)
    (check-type name string)
    (check-type symbol symbol)
    (check-type to-package package)
    (check-type from-package package)
    (check-type mixp (member nil t)) ; no cl:boolean on Genera
    (check-type shadowed hash-table)
    (check-type imported hash-table)
    (check-type inherited hash-table)
    (multiple-value-bind (existing status) (find-symbol name to-package)
      (let* ((sp (symbol-package symbol))
             (in (gethash name inherited))
             (xp (and status (symbol-package existing))))
        (when (null sp)
          (note-package-fishiness
           :import-uninterned name
           (package-name from-package) (package-name to-package) mixp)
          (import* symbol from-package)
          (setf sp (package-name from-package)))
        (cond
          ((gethash name shadowed))
          (in
           (unless (equal sp (first in))
             (if mixp
                 (ensure-shadowing-import name to-package (second in) shadowed imported)
                 (error "Can't inherit ~S from ~S, it is inherited from ~S"
                        name (package-name sp) (package-name (first in))))))
          ((gethash name imported)
           (unless (eq symbol existing)
             (error "Can't inherit ~S from ~S, it is imported from ~S"
                    name (package-name sp) (package-name xp))))
          (t
           (setf (gethash name inherited) (list sp from-package))
           (when (and status (not (eq sp xp)))
             (let ((shadowing (symbol-shadowing-p existing to-package)))
               (note-package-fishiness
                :inherited name
                (package-name from-package)
                (or (home-package-p symbol from-package) (symbol-package-name symbol))
                (package-name to-package)
                (or (home-package-p existing to-package) (symbol-package-name existing)))
               (if shadowing (ensure-shadowing-import name to-package from-package shadowed imported)
                   (unintern* existing to-package)))))))))
  (defun ensure-mix (name symbol to-package from-package shadowed imported inherited)
    (check-type name string)
    (check-type symbol symbol)
    (check-type to-package package)
    (check-type from-package package)
    (check-type shadowed hash-table)
    (check-type imported hash-table)
    (check-type inherited hash-table)
    (unless (gethash name shadowed)
      (multiple-value-bind (existing status) (find-symbol name to-package)
        (let* ((sp (symbol-package symbol))
               (im (gethash name imported))
               (in (gethash name inherited)))
          (cond
            ((or (null status)
                 (and status (eq symbol existing))
                 (and in (eq sp (first in))))
             (ensure-inherited name symbol to-package from-package t shadowed imported inherited))
            (in
             (remhash name inherited)
             (ensure-shadowing-import name to-package (second in) shadowed imported))
            (im
             (error "Symbol ~S import from ~S~:[~; actually ~:[uninterned~;~:*from ~S~]~] conflicts with existing symbol in ~S~:[~; actually ~:[uninterned~;from ~:*~S~]~]"
                    name (package-name from-package)
                    (home-package-p symbol from-package) (symbol-package-name symbol)
                    (package-name to-package)
                    (home-package-p existing to-package) (symbol-package-name existing)))
            (t
             (ensure-inherited name symbol to-package from-package t shadowed imported inherited)))))))

  (defun recycle-symbol (name recycle exported)
    ;; Takes a symbol NAME (a string), a list of package designators for RECYCLE
    ;; packages, and a hash-table of names (strings) of symbols scheduled to be
    ;; EXPORTED from the package being defined. It returns two values, the
    ;; symbol found (if any, or else NIL), and a boolean flag indicating whether
    ;; a symbol was found. The caller (DEFINE-PACKAGE) will then do the
    ;; re-homing of the symbol, etc.
    (check-type name string)
    (check-type recycle list)
    (check-type exported hash-table)
    (when (gethash name exported) ;; don't bother recycling private symbols
      (let (recycled foundp)
        (dolist (r recycle (values recycled foundp))
          (multiple-value-bind (symbol status) (find-symbol name r)
            (when (and status (home-package-p symbol r))
              (cond
                (foundp
                 ;; (nuke-symbol symbol)) -- even simple variable names like O or C will do that.
                 (note-package-fishiness :recycled-duplicate name (package-name foundp) (package-name r)))
                (t
                 (setf recycled symbol foundp r)))))))))
  (defun symbol-recycled-p (sym recycle)
    (check-type sym symbol)
    (check-type recycle list)
    (and (member (symbol-package sym) recycle) t))
  (defun ensure-symbol (name package intern recycle shadowed imported inherited exported)
    (check-type name string)
    (check-type package package)
    (check-type intern (member nil t)) ; no cl:boolean on Genera
    (check-type shadowed hash-table)
    (check-type imported hash-table)
    (check-type inherited hash-table)
    (unless (or (gethash name shadowed)
                (gethash name imported)
                (gethash name inherited))
      (multiple-value-bind (existing status)
          (find-symbol name package)
        (multiple-value-bind (recycled previous) (recycle-symbol name recycle exported)
          (cond
            ((and status (eq existing recycled) (eq previous package)))
            (previous
             (rehome-symbol recycled package))
            ((and status (eq package (symbol-package existing))))
            (t
             (when status
               (note-package-fishiness
                :ensure-symbol name
                (reify-package (symbol-package existing) package)
                status intern)
               (unintern existing))
             (when intern
               (intern* name package))))))))
  (declaim (ftype (function (t t t &optional t) t) ensure-exported))
  (defun ensure-exported-to-user (name symbol to-package &optional recycle)
    (check-type name string)
    (check-type symbol symbol)
    (check-type to-package package)
    (check-type recycle list)
    (assert (equal name (symbol-name symbol)))
    (multiple-value-bind (existing status) (find-symbol name to-package)
      (unless (and status (eq symbol existing))
        (let ((accessible
                (or (null status)
                    (let ((shadowing (symbol-shadowing-p existing to-package))
                          (recycled (symbol-recycled-p existing recycle)))
                      (unless (and shadowing (not recycled))
                        (note-package-fishiness
                         :ensure-export name (symbol-package-name symbol)
                         (package-name to-package)
                         (or (home-package-p existing to-package) (symbol-package-name existing))
                         status shadowing)
                        (if (or (eq status :inherited) shadowing)
                            (shadowing-import* symbol to-package)
                            (unintern existing to-package))
                        t)))))
          (when (and accessible (eq status :external))
            (ensure-exported name symbol to-package recycle))))))
  (defun ensure-exported (name symbol from-package &optional recycle)
    (dolist (to-package (package-used-by-list from-package))
      (ensure-exported-to-user name symbol to-package recycle))
    (unless (eq from-package (symbol-package symbol))
      (ensure-imported symbol from-package))
    (export* name from-package))
  (defun ensure-export (name from-package &optional recycle)
    (multiple-value-bind (symbol status) (find-symbol* name from-package)
      (unless (eq status :external)
        (ensure-exported name symbol from-package recycle))))
  (defun ensure-package (name &key
                                nicknames documentation use
                                shadow shadowing-import-from
                                import-from export intern
                                recycle mix reexport
                                unintern)
    #+genera (declare (ignore documentation))
    (let* ((package-name (string name))
           (nicknames (mapcar #'string nicknames))
           (names (cons package-name nicknames))
           (previous (packages-from-names names))
           (discarded (cdr previous))
           (to-delete ())
           (package (or (first previous) (make-package package-name :nicknames nicknames)))
           (recycle (packages-from-names recycle))
           (use (mapcar 'find-package* use))
           (mix (mapcar 'find-package* mix))
           (reexport (mapcar 'find-package* reexport))
           (shadow (mapcar 'string shadow))
           (export (mapcar 'string export))
           (intern (mapcar 'string intern))
           (unintern (mapcar 'string unintern))
           (shadowed (make-hash-table :test 'equal)) ; string to bool
           (imported (make-hash-table :test 'equal)) ; string to bool
           (exported (make-hash-table :test 'equal)) ; string to bool
           ;; string to list home package and use package:
           (inherited (make-hash-table :test 'equal)))
      (when-package-fishiness (record-fishy package-name))
      #-genera
      (when documentation (setf (documentation package t) documentation))
      (loop :for p :in (set-difference (package-use-list package) (append mix use))
            :do (note-package-fishiness :over-use name (package-names p))
                (unuse-package p package))
      (loop :for p :in discarded
            :for n = (remove-if #'(lambda (x) (member x names :test 'equal))
                                (package-names p))
            :do (note-package-fishiness :nickname name (package-names p))
                (cond (n (rename-package p (first n) (rest n)))
                      (t (rename-package-away p)
                         (push p to-delete))))
      (rename-package package package-name nicknames)
      (dolist (name unintern)
        (multiple-value-bind (existing status) (find-symbol name package)
          (when status
            (unless (eq status :inherited)
              (note-package-fishiness
               :unintern (package-name package) name (symbol-package-name existing) status)
              (unintern* name package nil)))))
      (dolist (name export)
        (setf (gethash name exported) t))
      (dolist (p reexport)
        (do-external-symbols (sym p)
          (setf (gethash (string sym) exported) t)))
      (do-external-symbols (sym package)
        (let ((name (symbol-name sym)))
          (unless (gethash name exported)
            (note-package-fishiness
             :over-export (package-name package) name
             (or (home-package-p sym package) (symbol-package-name sym)))
            (unexport sym package))))
      (dolist (name shadow)
        (setf (gethash name shadowed) t)
        (multiple-value-bind (existing status) (find-symbol name package)
          (multiple-value-bind (recycled previous) (recycle-symbol name recycle exported)
            (let ((shadowing (and status (symbol-shadowing-p existing package))))
              (cond
                ((eq previous package))
                (previous
                 (rehome-symbol recycled package))
                ((or (member status '(nil :inherited))
                     (home-package-p existing package)))
                (t
                 (let ((dummy (make-symbol name)))
                   (note-package-fishiness
                    :shadow-imported (package-name package) name
                    (symbol-package-name existing) status shadowing)
                   (shadowing-import* dummy package)
                   (import* dummy package)))))))
        (shadow* name package))
      (loop :for (p . syms) :in shadowing-import-from
            :for pp = (find-package* p) :do
              (dolist (sym syms) (ensure-shadowing-import (string sym) package pp shadowed imported)))
      (loop :for p :in mix
            :for pp = (find-package* p) :do
              (do-external-symbols (sym pp) (ensure-mix (symbol-name sym) sym package pp shadowed imported inherited)))
      (loop :for (p . syms) :in import-from
            :for pp = (find-package p) :do
              (dolist (sym syms) (ensure-import (symbol-name sym) package pp shadowed imported)))
      (dolist (p (append use mix))
        (do-external-symbols (sym p) (ensure-inherited (string sym) sym package p nil shadowed imported inherited))
        (use-package p package))
      (loop :for name :being :the :hash-keys :of exported :do
        (ensure-symbol name package t recycle shadowed imported inherited exported)
        (ensure-export name package recycle))
      (dolist (name intern)
        (ensure-symbol name package t recycle shadowed imported inherited exported))
      (do-symbols (sym package)
        (ensure-symbol (symbol-name sym) package nil recycle shadowed imported inherited exported))
      (map () 'delete-package* to-delete)
      package)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun parse-define-package-form (package clauses)
    (loop
      :with use-p = nil :with recycle-p = nil
      :with documentation = nil
      :for (kw . args) :in clauses
      :when (eq kw :nicknames) :append args :into nicknames :else
      :when (eq kw :documentation)
        :do (cond
              (documentation (error "define-package: can't define documentation twice"))
              ((or (atom args) (cdr args)) (error "define-package: bad documentation"))
              (t (setf documentation (car args)))) :else
      :when (eq kw :use) :append args :into use :and :do (setf use-p t) :else
      :when (eq kw :shadow) :append args :into shadow :else
      :when (eq kw :shadowing-import-from) :collect args :into shadowing-import-from :else
      :when (eq kw :import-from) :collect args :into import-from :else
      :when (eq kw :export) :append args :into export :else
      :when (eq kw :intern) :append args :into intern :else
      :when (eq kw :recycle) :append args :into recycle :and :do (setf recycle-p t) :else
      :when (eq kw :mix) :append args :into mix :else
      :when (eq kw :reexport) :append args :into reexport :else
      :when (eq kw :use-reexport) :append args :into use :and :append args :into reexport
        :and :do (setf use-p t) :else
      :when (eq kw :mix-reexport) :append args :into mix :and :append args :into reexport
        :and :do (setf use-p t) :else
      :when (eq kw :unintern) :append args :into unintern :else
        :do (error "unrecognized define-package keyword ~S" kw)
      :finally (return `(,package
                         :nicknames ,nicknames :documentation ,documentation
                         :use ,(if use-p use '(:common-lisp))
                         :shadow ,shadow :shadowing-import-from ,shadowing-import-from
                         :import-from ,import-from :export ,export :intern ,intern
                         :recycle ,(if recycle-p recycle (cons package nicknames))
                         :mix ,mix :reexport ,reexport :unintern ,unintern)))))

(defmacro define-package (package &rest clauses)
  "DEFINE-PACKAGE takes a PACKAGE and a number of CLAUSES, of the form
\(KEYWORD . ARGS\).
DEFINE-PACKAGE supports the following keywords:
USE, SHADOW, SHADOWING-IMPORT-FROM, IMPORT-FROM, EXPORT, INTERN -- as per CL:DEFPACKAGE.
RECYCLE -- Recycle the package's exported symbols from the specified packages,
in order.  For every symbol scheduled to be exported by the DEFINE-PACKAGE,
either through an :EXPORT option or a :REEXPORT option, if the symbol exists in
one of the :RECYCLE packages, the first such symbol is re-homed to the package
being defined.
For the sake of idempotence, it is important that the package being defined
should appear in first position if it already exists, and even if it doesn't,
ahead of any package that is not going to be deleted afterwards and never
created again. In short, except for special cases, always make it the first
package on the list if the list is not empty.
MIX -- Takes a list of package designators.  MIX behaves like
\(:USE PKG1 PKG2 ... PKGn\) but additionally uses :SHADOWING-IMPORT-FROM to
resolve conflicts in favor of the first found symbol.  It may still yield
an error if there is a conflict with an explicitly :IMPORT-FROM symbol.
REEXPORT -- Takes a list of package designators.  For each package, p, in the list,
export symbols with the same name as those exported from p.  Note that in the case
of shadowing, etc. the symbols with the same name may not be the same symbols.
UNINTERN -- Remove symbols here from PACKAGE."
  (let ((ensure-form
          `(apply 'ensure-package ',(parse-define-package-form package clauses))))
    `(progn
       #+(or ecl gcl mkcl) (defpackage ,package (:use))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ,ensure-form))))

;;;; Final tricks to keep various implementations happy.
;; We want most such tricks in common-lisp.lisp,
;; but these need to be done before the define-package form there,
;; that we nevertheless want to be the very first form.
(eval-when (:load-toplevel :compile-toplevel :execute)
  #+allegro ;; We need to disable autoloading BEFORE any mention of package ASDF.
  (setf excl::*autoload-package-name-alist*
        (remove "asdf" excl::*autoload-package-name-alist*
                :test 'equalp :key 'car)))

;; Compatibility with whoever calls asdf/package
(define-package :asdf/package (:use :cl :uiop/package) (:reexport :uiop/package))
;;;; -------------------------------------------------------------------------
;;;; Handle compatibility with multiple implementations.
;;; This file is for papering over the deficiencies and peculiarities
;;; of various Common Lisp implementations.
;;; For implementation-specific access to the system, see os.lisp instead.
;;; A few functions are defined here, but actually exported from utility;
;;; from this package only common-lisp symbols are exported.

(uiop/package:define-package :uiop/common-lisp
  (:nicknames :uoip/cl :asdf/common-lisp :asdf/cl)
  (:use #-genera :common-lisp #+genera :future-common-lisp :uiop/package)
  (:reexport :common-lisp)
  (:recycle :uiop/common-lisp :uoip/cl :asdf/common-lisp :asdf/cl :asdf)
  #+allegro (:intern #:*acl-warn-save*)
  #+cormanlisp (:shadow #:user-homedir-pathname)
  #+cormanlisp
  (:export
   #:logical-pathname #:translate-logical-pathname
   #:make-broadcast-stream #:file-namestring)
  #+genera (:shadowing-import-from :scl #:boolean)
  #+genera (:export #:boolean #:ensure-directories-exist)
  #+mcl (:shadow #:user-homedir-pathname))
(in-package :uiop/common-lisp)

#-(or abcl allegro clisp clozure cmu cormanlisp ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
(error "ASDF is not supported on your implementation. Please help us port it.")

;; (declaim (optimize (speed 1) (debug 3) (safety 3))) ; DON'T: trust implementation defaults.


;;;; Early meta-level tweaks

#+(or abcl allegro clisp cmu ecl mkcl clozure lispworks mkcl sbcl scl)
(eval-when (:load-toplevel :compile-toplevel :execute)
  ;; Check for unicode at runtime, so that a hypothetical FASL compiled with unicode
  ;; but loaded in a non-unicode setting (e.g. on Allegro) won't tell a lie.
  (when (and #+allegro (member :ics *features*)
             #+(or clisp cmu ecl mkcl) (member :unicode *features*)
             #+sbcl (member :sb-unicode *features*))
    (pushnew :asdf-unicode *features*)))

#+allegro
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *acl-warn-save*
    (when (boundp 'excl:*warn-on-nested-reader-conditionals*)
      excl:*warn-on-nested-reader-conditionals*))
  (when (boundp 'excl:*warn-on-nested-reader-conditionals*)
    (setf excl:*warn-on-nested-reader-conditionals* nil))
  (setf *print-readably* nil))

#+clozure (in-package :ccl)
#+(and clozure windows-target) ;; See http://trac.clozure.com/ccl/ticket/1117
(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (fboundp 'external-process-wait)
    (in-development-mode
     (defun external-process-wait (proc)
       (when (and (external-process-pid proc) (eq (external-process-%status proc) :running))
         (with-interrupts-enabled
             (wait-on-semaphore (external-process-completed proc))))
       (values (external-process-%exit-code proc)
               (external-process-%status proc))))))
#+clozure (in-package :uiop/common-lisp)


#+cormanlisp
(eval-when (:load-toplevel :compile-toplevel :execute)
  (deftype logical-pathname () nil)
  (defun make-broadcast-stream () *error-output*)
  (defun translate-logical-pathname (x) x)
  (defun user-homedir-pathname (&optional host)
    (declare (ignore host))
    (parse-namestring (format nil "~A\\" (cl:user-homedir-pathname))))
  (defun file-namestring (p)
    (setf p (pathname p))
    (format nil "~@[~A~]~@[.~A~]" (pathname-name p) (pathname-type p))))

#+ecl
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf *load-verbose* nil)
  (defun use-ecl-byte-compiler-p () (and (member :ecl-bytecmp *features*) t))
  (unless (use-ecl-byte-compiler-p) (require :cmp)))

#+gcl
(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (member :ansi-cl *features*)
    (error "ASDF only supports GCL in ANSI mode. Aborting.~%"))
  (setf compiler::*compiler-default-type* (pathname "")
        compiler::*lsp-ext* "")
  #.(let ((code ;; Only support very recent GCL 2.7.0 from November 2013 or later.
            (cond
              #+gcl
              ((or (< system::*gcl-major-version* 2)
                   (and (= system::*gcl-major-version* 2)
                        (< system::*gcl-minor-version* 7)))
               '(error "GCL 2.7 or later required to use ASDF")))))
      (eval code)
      code))

#+genera
(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (fboundp 'ensure-directories-exist)
    (defun ensure-directories-exist (path)
      (fs:create-directories-recursively (pathname path)))))

#.(or #+mcl ;; the #$ doesn't work on other lisps, even protected by #+mcl, so we use this trick
      (read-from-string
       "(eval-when (:load-toplevel :compile-toplevel :execute)
          (ccl:define-entry-point (_getenv \"getenv\") ((name :string)) :string)
          (ccl:define-entry-point (_system \"system\") ((name :string)) :int)
          ;; Note: ASDF may expect user-homedir-pathname to provide
          ;; the pathname of the current user's home directory, whereas
          ;; MCL by default provides the directory from which MCL was started.
          ;; See http://code.google.com/p/mcl/wiki/Portability
          (defun user-homedir-pathname ()
            (ccl::findfolder #$kuserdomain #$kCurrentUserFolderType))
          (defun probe-posix (posix-namestring)
            \"If a file exists for the posix namestring, return the pathname\"
            (ccl::with-cstrs ((cpath posix-namestring))
              (ccl::rlet ((is-dir :boolean)
                          (fsref :fsref))
                (when (eq #$noerr (#_fspathmakeref cpath fsref is-dir))
                  (ccl::%path-from-fsref fsref is-dir))))))"))

#+mkcl
(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :cmp)
  (setq clos::*redefine-class-in-place* t)) ;; Make sure we have strict ANSI class redefinition semantics


;;;; Looping
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro loop* (&rest rest)
    #-genera `(loop ,@rest)
    #+genera `(lisp:loop ,@rest))) ;; In genera, CL:LOOP can't destructure, so we use LOOP*. Sigh.


;;;; compatfmt: avoid fancy format directives when unsupported
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun frob-substrings (string substrings &optional frob)
    "for each substring in SUBSTRINGS, find occurrences of it within STRING
that don't use parts of matched occurrences of previous strings, and
FROB them, that is to say, remove them if FROB is NIL,
replace by FROB if FROB is a STRING, or if FROB is a FUNCTION,
call FROB with the match and a function that emits a string in the output.
Return a string made of the parts not omitted or emitted by FROB."
    (declare (optimize (speed 0) (safety #-gcl 3 #+gcl 0) (debug 3)))
    (let ((length (length string)) (stream nil))
      (labels ((emit-string (x &optional (start 0) (end (length x)))
                 (when (< start end)
                   (unless stream (setf stream (make-string-output-stream)))
                   (write-string x stream :start start :end end)))
               (emit-substring (start end)
                 (when (and (zerop start) (= end length))
                   (return-from frob-substrings string))
                 (emit-string string start end))
               (recurse (substrings start end)
                 (cond
                   ((>= start end))
                   ((null substrings) (emit-substring start end))
                   (t (let* ((sub-spec (first substrings))
                             (sub (if (consp sub-spec) (car sub-spec) sub-spec))
                             (fun (if (consp sub-spec) (cdr sub-spec) frob))
                             (found (search sub string :start2 start :end2 end))
                             (more (rest substrings)))
                        (cond
                          (found
                           (recurse more start found)
                           (etypecase fun
                             (null)
                             (string (emit-string fun))
                             (function (funcall fun sub #'emit-string)))
                           (recurse substrings (+ found (length sub)) end))
                          (t
                           (recurse more start end))))))))
        (recurse substrings 0 length))
      (if stream (get-output-stream-string stream) "")))

  (defmacro compatfmt (format)
    #+(or gcl genera)
    (frob-substrings format `("~3i~_" #+genera ,@'("~@<" "~@;" "~@:>" "~:>")))
    #-(or gcl genera) format))
;;;; -------------------------------------------------------------------------
;;;; General Purpose Utilities for ASDF

(uiop/package:define-package :uiop/utility
  (:nicknames :asdf/utility)
  (:recycle :uiop/utility :asdf/utility :asdf)
  (:use :uiop/common-lisp :uiop/package)
  ;; import and reexport a few things defined in :uiop/common-lisp
  (:import-from :uiop/common-lisp #:compatfmt #:loop* #:frob-substrings
   #+ecl #:use-ecl-byte-compiler-p #+mcl #:probe-posix)
  (:export #:compatfmt #:loop* #:frob-substrings #:compatfmt
   #+ecl #:use-ecl-byte-compiler-p #+mcl #:probe-posix)
  (:export
   ;; magic helper to define debugging functions:
   #:uiop-debug #:load-uiop-debug-utility #:*uiop-debug-utility*
   #:with-upgradability ;; (un)defining functions in an upgrade-friendly way
   #:undefine-function #:undefine-functions #:defun* #:defgeneric*
   #:nest #:if-let ;; basic flow control
   #:while-collecting #:appendf #:length=n-p #:ensure-list ;; lists
   #:remove-plist-keys #:remove-plist-key ;; plists
   #:emptyp ;; sequences
   #:+non-base-chars-exist-p+ ;; characters
   #:+max-character-type-index+ #:character-type-index #:+character-types+
   #:base-string-p #:strings-common-element-type #:reduce/strcat #:strcat ;; strings
   #:first-char #:last-char #:split-string #:stripln #:+cr+ #:+lf+ #:+crlf+
   #:string-prefix-p #:string-enclosed-p #:string-suffix-p
   #:coerce-class ;; CLOS
   #:stamp< #:stamps< #:stamp*< #:stamp<= ;; stamps
   #:earlier-stamp #:stamps-earliest #:earliest-stamp
   #:later-stamp #:stamps-latest #:latest-stamp #:latest-stamp-f
   #:list-to-hash-set #:ensure-gethash ;; hash-table
   #:ensure-function #:access-at #:access-at-count ;; functions
   #:call-function #:call-functions #:register-hook-function
   #:match-condition-p #:match-any-condition-p ;; conditions
   #:call-with-muffled-conditions #:with-muffled-conditions
   #:lexicographic< #:lexicographic<=
   #:parse-version #:unparse-version #:version< #:version<= #:version-compatible-p)) ;; version
(in-package :uiop/utility)

;;;; Defining functions in a way compatible with hot-upgrade:
;; DEFUN* and DEFGENERIC* use FMAKUNBOUND to delete any previous fdefinition,
;; thus replacing the function without warning or error
;; even if the signature and/or generic-ness of the function has changed.
;; For a generic function, this invalidates any previous DEFMETHOD.
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun undefine-function (function-spec)
    (cond
      ((symbolp function-spec)
       ;; undefining the previous function is the portable way
       ;; of overriding any incompatible previous gf,
       ;; but CLISP needs extra help with getting rid of previous methods.
       #+clisp
       (let ((f (and (fboundp function-spec) (fdefinition function-spec))))
         (when (typep f 'clos:standard-generic-function)
           (loop :for m :in (clos:generic-function-methods f)
                 :do (remove-method f m))))
       (fmakunbound function-spec))
      ((and (consp function-spec) (eq (car function-spec) 'setf)
            (consp (cdr function-spec)) (null (cddr function-spec)))
       (fmakunbound function-spec))
      (t (error "bad function spec ~S" function-spec))))
  (defun undefine-functions (function-spec-list)
    (map () 'undefine-function function-spec-list))
  (macrolet
      ((defdef (def* def)
         `(defmacro ,def* (name formals &rest rest)
            (destructuring-bind (name &key (supersede t))
                (if (or (atom name) (eq (car name) 'setf))
                    (list name :supersede nil)
                    name)
              (declare (ignorable supersede))
              `(progn
                 ;; We usually try to do it only for the functions that need it,
                 ;; which happens in asdf/upgrade - however, for ECL, we need this hammer.
                 ,@(when (or supersede #+ecl t)
                     `((undefine-function ',name)))
                 ,@(when (and #+ecl (symbolp name)) ; fails for setf functions on ecl
                     `((declaim (notinline ,name))))
                 (,',def ,name ,formals ,@rest))))))
    (defdef defgeneric* defgeneric)
    (defdef defun* defun))
  (defmacro with-upgradability ((&optional) &body body)
    "Evaluate BODY at compile- load- and run- times, with DEFUN and DEFGENERIC modified
to also declare the functions NOTINLINE and to accept a wrapping the function name
specification into a list with keyword argument SUPERSEDE (which defaults to T if the name
is not wrapped, and NIL if it is wrapped). If SUPERSEDE is true, call UNDEFINE-FUNCTION
to supersede any previous definition."
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(loop :for form :in body :collect
               (if (consp form)
                   (destructuring-bind (car . cdr) form
                     (case car
                       ((defun) `(defun* ,@cdr))
                       ((defgeneric) `(defgeneric* ,@cdr))
                       (otherwise form)))
                   form)))))

;;; Magic debugging help. See contrib/debug.lisp
(with-upgradability ()
  (defvar *uiop-debug-utility*
    '(or (ignore-errors
          (symbol-call :asdf :system-relative-pathname :uiop "contrib/debug.lisp"))
      (symbol-call :uiop/pathname :subpathname (user-homedir-pathname) "cl/asdf/uiop/contrib/debug.lisp"))
    "form that evaluates to the pathname to your favorite debugging utilities")

  (defmacro uiop-debug (&rest keys)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (load-uiop-debug-utility ,@keys)))

  (defun load-uiop-debug-utility (&key package utility-file)
    (let* ((*package* (if package (find-package package) *package*))
           (keyword (read-from-string
                     (format nil ":DBG-~:@(~A~)" (package-name *package*)))))
      (unless (member keyword *features*)
        (let* ((utility-file (or utility-file *uiop-debug-utility*))
               (file (ignore-errors (probe-file (eval utility-file)))))
          (if file (load file)
              (error "Failed to locate debug utility file: ~S" utility-file)))))))

;;; Flow control
(with-upgradability ()
  (defmacro nest (&rest things)
    "Macro to do keep code nesting and indentation under control." ;; Thanks to mbaringer
    (reduce #'(lambda (outer inner) `(,@outer ,inner))
            things :from-end t))

  (defmacro if-let (bindings &body (then-form &optional else-form)) ;; from alexandria
    ;; bindings can be (var form) or ((var1 form1) ...)
    (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                             (list bindings)
                             bindings))
           (variables (mapcar #'car binding-list)))
      `(let ,binding-list
         (if (and ,@variables)
             ,then-form
             ,else-form)))))

;;; List manipulation
(with-upgradability ()
  (defmacro while-collecting ((&rest collectors) &body body)
    "COLLECTORS should be a list of names for collections.  A collector
defines a function that, when applied to an argument inside BODY, will
add its argument to the corresponding collection.  Returns multiple values,
a list for each collection, in order.
   E.g.,
\(while-collecting \(foo bar\)
           \(dolist \(x '\(\(a 1\) \(b 2\) \(c 3\)\)\)
             \(foo \(first x\)\)
             \(bar \(second x\)\)\)\)
Returns two values: \(A B C\) and \(1 2 3\)."
    (let ((vars (mapcar #'(lambda (x) (gensym (symbol-name x))) collectors))
          (initial-values (mapcar (constantly nil) collectors)))
      `(let ,(mapcar #'list vars initial-values)
         (flet ,(mapcar #'(lambda (c v) `(,c (x) (push x ,v) (values))) collectors vars)
           ,@body
           (values ,@(mapcar #'(lambda (v) `(reverse ,v)) vars))))))

  (define-modify-macro appendf (&rest args)
    append "Append onto list") ;; only to be used on short lists.

  (defun length=n-p (x n) ;is it that (= (length x) n) ?
    (check-type n (integer 0 *))
    (loop
      :for l = x :then (cdr l)
      :for i :downfrom n :do
        (cond
          ((zerop i) (return (null l)))
          ((not (consp l)) (return nil)))))

  (defun ensure-list (x)
    (if (listp x) x (list x))))


;;; remove a key from a plist, i.e. for keyword argument cleanup
(with-upgradability ()
  (defun remove-plist-key (key plist)
    "Remove a single key from a plist"
    (loop* :for (k v) :on plist :by #'cddr
           :unless (eq k key)
           :append (list k v)))

  (defun remove-plist-keys (keys plist)
    "Remove a list of keys from a plist"
    (loop* :for (k v) :on plist :by #'cddr
           :unless (member k keys)
           :append (list k v))))


;;; Sequences
(with-upgradability ()
  (defun emptyp (x)
    "Predicate that is true for an empty sequence"
    (or (null x) (and (vectorp x) (zerop (length x))))))


;;; Characters
(with-upgradability () ;; base-char != character on ECL, LW, SBCL, Genera. LW also has SIMPLE-CHAR.
  (defconstant +non-base-chars-exist-p+ (not (subtypep 'character 'base-char)))
  #-scl ;; In SCL, all characters seem to be 16-bit base-char, but this flag gets set somehow???
  (when +non-base-chars-exist-p+ (pushnew :non-base-chars-exist-p *features*)))

(with-upgradability ()
  (defparameter +character-types+ ;; assuming a simple hierarchy
    #(#+non-base-chars-exist-p base-char #+lispworks lw:simple-char character))
  (defparameter +max-character-type-index+ (1- (length +character-types+))))

(with-upgradability ()
  (defun character-type-index (x)
    (declare (ignorable x))
    #.(case +max-character-type-index+
        (0 0)
        (1 '(etypecase x
             (character (if (typep x 'base-char) 0 1))
             (symbol (if (subtypep x 'base-char) 0 1))))
        (otherwise
         '(or (position-if (etypecase x
                             (character  #'(lambda (type) (typep x type)))
                             (symbol #'(lambda (type) (subtypep x type))))
               +character-types+)
           (error "Not a character or character type: ~S" x))))))


;;; Strings
(with-upgradability ()
  (defun base-string-p (string)
    "Does the STRING only contain BASE-CHARs?"
    (declare (ignorable string))
    (and #+non-base-chars-exist-p (eq 'base-char (array-element-type string))))

  (defun strings-common-element-type (strings)
    "What least subtype of CHARACTER can contain all the elements of all the STRINGS?"
    (declare (ignorable strings))
    #.(if +non-base-chars-exist-p+
          `(aref +character-types+
            (loop :with index = 0 :for s :in strings :do
              (cond
                ((= index ,+max-character-type-index+) (return index))
                ((emptyp s)) ;; NIL or empty string
                ((characterp s) (setf index (max index (character-type-index s))))
                ((stringp s) (unless (>= index (character-type-index (array-element-type s)))
                               (setf index (reduce 'max s :key #'character-type-index
                                                          :initial-value index))))
                (t (error "Invalid string designator ~S for ~S" s 'strings-common-element-type)))
                  :finally (return index)))
          ''character))

  (defun reduce/strcat (strings &key key start end)
    "Reduce a list as if by STRCAT, accepting KEY START and END keywords like REDUCE.
NIL is interpreted as an empty string. A character is interpreted as a string of length one."
    (when (or start end) (setf strings (subseq strings start end)))
    (when key (setf strings (mapcar key strings)))
    (loop :with output = (make-string (loop :for s :in strings
                                            :sum (if (characterp s) 1 (length s)))
                                      :element-type (strings-common-element-type strings))
          :with pos = 0
          :for input :in strings
          :do (etypecase input
                (null)
                (character (setf (char output pos) input) (incf pos))
                (string (replace output input :start1 pos) (incf pos (length input))))
          :finally (return output)))

  (defun strcat (&rest strings)
    "Concatenate strings.
NIL is interpreted as an empty string, a character as a string of length one."
    (reduce/strcat strings))

  (defun first-char (s)
    "Return the first character of a non-empty string S, or NIL"
    (and (stringp s) (plusp (length s)) (char s 0)))

  (defun last-char (s)
    "Return the last character of a non-empty string S, or NIL"
    (and (stringp s) (plusp (length s)) (char s (1- (length s)))))

  (defun split-string (string &key max (separator '(#\Space #\Tab)))
    "Split STRING into a list of components separated by
any of the characters in the sequence SEPARATOR.
If MAX is specified, then no more than max(1,MAX) components will be returned,
starting the separation from the end, e.g. when called with arguments
 \"a.b.c.d.e\" :max 3 :separator \".\" it will return (\"a.b.c\" \"d\" \"e\")."
    (block ()
      (let ((list nil) (words 0) (end (length string)))
        (when (zerop end) (return nil))
        (flet ((separatorp (char) (find char separator))
               (done () (return (cons (subseq string 0 end) list))))
          (loop
            :for start = (if (and max (>= words (1- max)))
                             (done)
                             (position-if #'separatorp string :end end :from-end t))
            :do (when (null start) (done))
                (push (subseq string (1+ start) end) list)
                (incf words)
                (setf end start))))))

  (defun string-prefix-p (prefix string)
    "Does STRING begin with PREFIX?"
    (let* ((x (string prefix))
           (y (string string))
           (lx (length x))
           (ly (length y)))
      (and (<= lx ly) (string= x y :end2 lx))))

  (defun string-suffix-p (string suffix)
    "Does STRING end with SUFFIX?"
    (let* ((x (string string))
           (y (string suffix))
           (lx (length x))
           (ly (length y)))
      (and (<= ly lx) (string= x y :start1 (- lx ly)))))

  (defun string-enclosed-p (prefix string suffix)
    "Does STRING begin with PREFIX and end with SUFFIX?"
    (and (string-prefix-p prefix string)
         (string-suffix-p string suffix))))

  (defvar +cr+ (coerce #(#\Return) 'string))
  (defvar +lf+ (coerce #(#\Linefeed) 'string))
  (defvar +crlf+ (coerce #(#\Return #\Linefeed) 'string))

  (defun stripln (x)
    "Strip a string X from any ending CR, LF or CRLF.
Return two values, the stripped string and the ending that was stripped,
or the original value and NIL if no stripping took place.
Since our STRCAT accepts NIL as empty string designator,
the two results passed to STRCAT always reconstitute the original string"
    (check-type x string)
    (block nil
      (flet ((c (end) (when (string-suffix-p x end)
                        (return (values (subseq x 0 (- (length x) (length end))) end)))))
        (when x (c +crlf+) (c +lf+) (c +cr+) (values x nil)))))


;;; stamps: a REAL or a boolean where NIL=-infinity, T=+infinity
(eval-when (#-lispworks :compile-toplevel :load-toplevel :execute)
  (deftype stamp () '(or real boolean)))
(with-upgradability ()
  (defun stamp< (x y)
    (etypecase x
      (null (and y t))
      ((eql t) nil)
      (real (etypecase y
              (null nil)
              ((eql t) t)
              (real (< x y))))))
  (defun stamps< (list) (loop :for y :in list :for x = nil :then y :always (stamp< x y)))
  (defun stamp*< (&rest list) (stamps< list))
  (defun stamp<= (x y) (not (stamp< y x)))
  (defun earlier-stamp (x y) (if (stamp< x y) x y))
  (defun stamps-earliest (list) (reduce 'earlier-stamp list :initial-value t))
  (defun earliest-stamp (&rest list) (stamps-earliest list))
  (defun later-stamp (x y) (if (stamp< x y) y x))
  (defun stamps-latest (list) (reduce 'later-stamp list :initial-value nil))
  (defun latest-stamp (&rest list) (stamps-latest list))
  (define-modify-macro latest-stamp-f (&rest stamps) latest-stamp))


;;; Function designators
(with-upgradability ()
  (defun ensure-function (fun &key (package :cl))
    "Coerce the object FUN into a function.

If FUN is a FUNCTION, return it.
If the FUN is a non-sequence literal constant, return constantly that,
i.e. for a boolean keyword character number or pathname.
Otherwise if FUN is a non-literally constant symbol, return its FDEFINITION.
If FUN is a CONS, return the function that applies its CAR
to the appended list of the rest of its CDR and the arguments,
unless the CAR is LAMBDA, in which case the expression is evaluated.
If FUN is a string, READ a form from it in the specified PACKAGE (default: CL)
and EVAL that in a (FUNCTION ...) context."
    (etypecase fun
      (function fun)
      ((or boolean keyword character number pathname) (constantly fun))
      (hash-table (lambda (x) (gethash x fun)))
      (symbol (fdefinition fun))
      (cons (if (eq 'lambda (car fun))
                (eval fun)
                #'(lambda (&rest args) (apply (car fun) (append (cdr fun) args)))))
      (string (eval `(function ,(with-standard-io-syntax
                                  (let ((*package* (find-package package)))
                                    (read-from-string fun))))))))

  (defun access-at (object at)
    "Given an OBJECT and an AT specifier, list of successive accessors,
call each accessor on the result of the previous calls.
An accessor may be an integer, meaning a call to ELT,
a keyword, meaning a call to GETF,
NIL, meaning identity,
a function or other symbol, meaning itself,
or a list of a function designator and arguments, interpreted as per ENSURE-FUNCTION.
As a degenerate case, the AT specifier may be an atom of a single such accessor
instead of a list."
    (flet ((access (object accessor)
             (etypecase accessor
               (function (funcall accessor object))
               (integer (elt object accessor))
               (keyword (getf object accessor))
               (null object)
               (symbol (funcall accessor object))
               (cons (funcall (ensure-function accessor) object)))))
      (if (listp at)
          (dolist (accessor at object)
            (setf object (access object accessor)))
          (access object at))))

  (defun access-at-count (at)
    "From an AT specification, extract a COUNT of maximum number
of sub-objects to read as per ACCESS-AT"
    (cond
      ((integerp at)
       (1+ at))
      ((and (consp at) (integerp (first at)))
       (1+ (first at)))))

  (defun call-function (function-spec &rest arguments)
    "Call the function designated by FUNCTION-SPEC as per ENSURE-FUNCTION,
with the given ARGUMENTS"
    (apply (ensure-function function-spec) arguments))

  (defun call-functions (function-specs)
    "For each function in the list FUNCTION-SPECS, in order, call the function as per CALL-FUNCTION"
    (map () 'call-function function-specs))

  (defun register-hook-function (variable hook &optional call-now-p)
    "Push the HOOK function (a designator as per ENSURE-FUNCTION) onto the hook VARIABLE.
When CALL-NOW-P is true, also call the function immediately."
    (pushnew hook (symbol-value variable) :test 'equal)
    (when call-now-p (call-function hook))))


;;; CLOS
(with-upgradability ()
  (defun coerce-class (class &key (package :cl) (super t) (error 'error))
    "Coerce CLASS to a class that is subclass of SUPER if specified,
or invoke ERROR handler as per CALL-FUNCTION.

A keyword designates the name a symbol, which when found in either PACKAGE, designates a class.
-- for backward compatibility, *PACKAGE* is also accepted for now, but this may go in the future.
A string is read as a symbol while in PACKAGE, the symbol designates a class.

A class object designates itself.
NIL designates itself (no class).
A symbol otherwise designates a class by name."
    (let* ((normalized
             (typecase class
              (keyword (or (find-symbol* class package nil)
                           (find-symbol* class *package* nil)))
              (string (symbol-call :uiop :safe-read-from-string class :package package))
              (t class)))
           (found
             (etypecase normalized
               ((or standard-class built-in-class) normalized)
               ((or null keyword) nil)
               (symbol (find-class normalized nil nil)))))
      (or (and found
               (or (eq super t) (#-cormanlisp subtypep #+cormanlisp cl::subclassp found super))
               found)
          (call-function error "Can't coerce ~S to a ~@[class~;subclass of ~:*~S]" class super)))))


;;; Hash-tables
(with-upgradability ()
  (defun ensure-gethash (key table default)
    "Lookup the TABLE for a KEY as by GETHASH, but if not present,
call the (possibly constant) function designated by DEFAULT as per CALL-FUNCTION,
set the corresponding entry to the result in the table.
Return two values: the entry after its optional computation, and whether it was found"
    (multiple-value-bind (value foundp) (gethash key table)
      (values
       (if foundp
           value
           (setf (gethash key table) (call-function default)))
       foundp)))

  (defun list-to-hash-set (list &aux (h (make-hash-table :test 'equal)))
    "Convert a LIST into hash-table that has the same elements when viewed as a set,
up to the given equality TEST"
    (dolist (x list h) (setf (gethash x h) t))))


;;; Version handling
(with-upgradability ()
  (defun unparse-version (version-list)
    (format nil "~{~D~^.~}" version-list))

  (defun parse-version (version-string &optional on-error)
    "Parse a VERSION-STRING as a series of natural integers separated by dots.
Return a (non-null) list of integers if the string is valid;
otherwise return NIL.

When invalid, ON-ERROR is called as per CALL-FUNCTION before to return NIL,
with format arguments explaining why the version is invalid.
ON-ERROR is also called if the version is not canonical
in that it doesn't print back to itself, but the list is returned anyway."
    (block nil
      (unless (stringp version-string)
        (call-function on-error "~S: ~S is not a string" 'parse-version version-string)
        (return))
      (unless (loop :for prev = nil :then c :for c :across version-string
                    :always (or (digit-char-p c)
                                (and (eql c #\.) prev (not (eql prev #\.))))
                    :finally (return (and c (digit-char-p c))))
        (call-function on-error "~S: ~S doesn't follow asdf version numbering convention"
                       'parse-version version-string)
        (return))
      (let* ((version-list
               (mapcar #'parse-integer (split-string version-string :separator ".")))
             (normalized-version (unparse-version version-list)))
        (unless (equal version-string normalized-version)
          (call-function on-error "~S: ~S contains leading zeros" 'parse-version version-string))
        version-list)))

  (defun lexicographic< (< x y)
    (cond ((null y) nil)
          ((null x) t)
          ((funcall < (car x) (car y)) t)
          ((funcall < (car y) (car x)) nil)
          (t (lexicographic< < (cdr x) (cdr y)))))

  (defun lexicographic<= (< x y)
    (not (lexicographic< < y x)))

  (defun version< (version1 version2)
    (let ((v1 (parse-version version1 nil))
          (v2 (parse-version version2 nil)))
      (lexicographic< '< v1 v2)))

  (defun version<= (version1 version2)
    (not (version< version2 version1)))

  (defun version-compatible-p (provided-version required-version)
    "Is the provided version a compatible substitution for the required-version?
If major versions differ, it's not compatible.
If they are equal, then any later version is compatible,
with later being determined by a lexicographical comparison of minor numbers."
    (let ((x (parse-version provided-version nil))
          (y (parse-version required-version nil)))
      (and x y (= (car x) (car y)) (lexicographic<= '< (cdr y) (cdr x))))))


;;; Condition control

(with-upgradability ()
  (defparameter +simple-condition-format-control-slot+
    #+abcl 'system::format-control
    #+allegro 'excl::format-control
    #+clisp 'system::$format-control
    #+clozure 'ccl::format-control
    #+(or cmu scl) 'conditions::format-control
    #+(or ecl mkcl) 'si::format-control
    #+(or gcl lispworks) 'conditions::format-string
    #+sbcl 'sb-kernel:format-control
    #-(or abcl allegro clisp clozure cmu ecl gcl lispworks mkcl sbcl scl) nil
    "Name of the slot for FORMAT-CONTROL in simple-condition")

  (defun match-condition-p (x condition)
    "Compare received CONDITION to some pattern X:
a symbol naming a condition class,
a simple vector of length 2, arguments to find-symbol* with result as above,
or a string describing the format-control of a simple-condition."
    (etypecase x
      (symbol (typep condition x))
      ((simple-vector 2)
       (ignore-errors (typep condition (find-symbol* (svref x 0) (svref x 1) nil))))
      (function (funcall x condition))
      (string (and (typep condition 'simple-condition)
                   ;; On SBCL, it's always set and the check triggers a warning
                   #+(or allegro clozure cmu lispworks scl)
                   (slot-boundp condition +simple-condition-format-control-slot+)
                   (ignore-errors (equal (simple-condition-format-control condition) x))))))

  (defun match-any-condition-p (condition conditions)
    "match CONDITION against any of the patterns of CONDITIONS supplied"
    (loop :for x :in conditions :thereis (match-condition-p x condition)))

  (defun call-with-muffled-conditions (thunk conditions)
    "calls the THUNK in a context where the CONDITIONS are muffled"
    (handler-bind ((t #'(lambda (c) (when (match-any-condition-p c conditions)
                                      (muffle-warning c)))))
      (funcall thunk)))

  (defmacro with-muffled-conditions ((conditions) &body body)
    "Shorthand syntax for CALL-WITH-MUFFLED-CONDITIONS"
    `(call-with-muffled-conditions #'(lambda () ,@body) ,conditions)))

;;;; ---------------------------------------------------------------------------
;;;; Access to the Operating System

(uiop/package:define-package :uiop/os
  (:nicknames :asdf/os)
  (:recycle :uiop/os :asdf/os :asdf)
  (:use :uiop/common-lisp :uiop/package :uiop/utility)
  (:export
   #:featurep #:os-unix-p #:os-macosx-p #:os-windows-p #:os-genera-p #:detect-os ;; features
   #:getenv #:getenvp ;; environment variables
   #:implementation-identifier ;; implementation identifier
   #:implementation-type #:*implementation-type*
   #:operating-system #:architecture #:lisp-version-string
   #:hostname #:getcwd #:chdir
   ;; Windows shortcut support
   #:read-null-terminated-string #:read-little-endian
   #:parse-file-location-info #:parse-windows-shortcut))
(in-package :uiop/os)

;;; Features
(with-upgradability ()
  (defun featurep (x &optional (*features* *features*))
    "Checks whether a feature expression X is true with respect to the *FEATURES* set,
as per the CLHS standard for #+ and #-. Beware that just like the CLHS,
we assume symbols from the KEYWORD package are used, but that unless you're using #+/#-
your reader will not have magically used the KEYWORD package, so you need specify
keywords explicitly."
    (cond
      ((atom x) (and (member x *features*) t))
      ((eq :not (car x)) (assert (null (cddr x))) (not (featurep (cadr x))))
      ((eq :or (car x)) (some #'featurep (cdr x)))
      ((eq :and (car x)) (every #'featurep (cdr x)))
      (t (error "Malformed feature specification ~S" x))))

  (defun os-unix-p ()
    "Is the underlying operating system some Unix variant?"
    (or #+abcl (featurep :unix)
        #+(and (not abcl) (or unix cygwin darwin)) t))

  (defun os-macosx-p ()
    "Is the underlying operating system MacOS X?"
    ;; OS-MACOSX is not mutually exclusive with OS-UNIX,
    ;; in fact the former implies the latter.
    (or
     #+allegro (featurep :macosx)
     #+clisp (featurep :macos)
     (featurep :darwin)))

  (defun os-windows-p ()
    "Is the underlying operating system Microsoft Windows?"
    (or #+abcl (featurep :windows)
        #+(and (not (or abcl unix cygwin darwin)) (or win32 windows mswindows mingw32 mingw64)) t))

  (defun os-genera-p ()
    "Is the underlying operating system Genera (running on a Symbolics Lisp Machine)?"
    (or #+genera t))

  (defun os-oldmac-p ()
    "Is the underlying operating system an (emulated?) MacOS 9 or earlier?"
    (or #+mcl t))

  (defun detect-os ()
    "Detects the current operating system. Only needs be run at compile-time,
except on ABCL where it might change between FASL compilation and runtime."
    (loop* :with o
           :for (feature . detect) :in '((:os-unix . os-unix-p) (:os-macosx . os-macosx-p)
                                         (:os-windows . os-windows-p)
                                         (:genera . os-genera-p) (:os-oldmac . os-oldmac-p))
           :when (and (or (not o) (eq feature :os-macosx)) (funcall detect))
           :do (setf o feature) (pushnew feature *features*)
           :else :do (setf *features* (remove feature *features*))
           :finally
           (return (or o (error "Congratulations for trying ASDF on an operating system~%~
that is neither Unix, nor Windows, nor Genera, nor even old MacOS.~%Now you port it.")))))

  (detect-os))

;;;; Environment variables: getting them, and parsing them.

(with-upgradability ()
  (defun getenv (x)
    "Query the environment, as in C getenv.
Beware: may return empty string if a variable is present but empty;
use getenvp to return NIL in such a case."
    (declare (ignorable x))
    #+(or abcl clisp ecl xcl) (ext:getenv x)
    #+allegro (sys:getenv x)
    #+clozure (ccl:getenv x)
    #+(or cmu scl) (cdr (assoc x ext:*environment-list* :test #'string=))
    #+cormanlisp
    (let* ((buffer (ct:malloc 1))
           (cname (ct:lisp-string-to-c-string x))
           (needed-size (win:getenvironmentvariable cname buffer 0))
           (buffer1 (ct:malloc (1+ needed-size))))
      (prog1 (if (zerop (win:getenvironmentvariable cname buffer1 needed-size))
                 nil
                 (ct:c-string-to-lisp-string buffer1))
        (ct:free buffer)
        (ct:free buffer1)))
    #+gcl (system:getenv x)
    #+genera nil
    #+lispworks (lispworks:environment-variable x)
    #+mcl (ccl:with-cstrs ((name x))
            (let ((value (_getenv name)))
              (unless (ccl:%null-ptr-p value)
                (ccl:%get-cstring value))))
    #+mkcl (#.(or (find-symbol* 'getenv :si nil) (find-symbol* 'getenv :mk-ext nil)) x)
    #+sbcl (sb-ext:posix-getenv x)
    #-(or abcl allegro clisp clozure cmu cormanlisp ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
    (error "~S is not supported on your implementation" 'getenv))

  (defun getenvp (x)
    "Predicate that is true if the named variable is present in the libc environment,
then returning the non-empty string value of the variable"
    (let ((g (getenv x))) (and (not (emptyp g)) g))))


;;;; implementation-identifier
;;
;; produce a string to identify current implementation.
;; Initially stolen from SLIME's SWANK, completely rewritten since.
;; We're back to runtime checking, for the sake of e.g. ABCL.

(with-upgradability ()
  (defun first-feature (feature-sets)
    "A helper for various feature detection functions"
    (dolist (x feature-sets)
      (multiple-value-bind (short long feature-expr)
          (if (consp x)
              (values (first x) (second x) (cons :or (rest x)))
              (values x x x))
        (when (featurep feature-expr)
          (return (values short long))))))

  (defun implementation-type ()
    "The type of Lisp implementation used, as a short UIOP-standardized keyword"
    (first-feature
     '(:abcl (:acl :allegro) (:ccl :clozure) :clisp (:corman :cormanlisp)
       (:cmu :cmucl :cmu) :ecl :gcl
       (:lwpe :lispworks-personal-edition) (:lw :lispworks)
       :mcl :mkcl :sbcl :scl (:smbx :symbolics) :xcl)))

  (defvar *implementation-type* (implementation-type)
    "The type of Lisp implementation used, as a short UIOP-standardized keyword")

  (defun operating-system ()
    "The operating system of the current host"
    (first-feature
     '(:cygwin (:win :windows :mswindows :win32 :mingw32) ;; try cygwin first!
       (:linux :linux :linux-target) ;; for GCL at least, must appear before :bsd
       (:macosx :macosx :darwin :darwin-target :apple) ; also before :bsd
       (:solaris :solaris :sunos) (:bsd :bsd :freebsd :netbsd :openbsd) :unix
       :genera)))

  (defun architecture ()
    "The CPU architecture of the current host"
    (first-feature
     '((:x64 :x86-64 :x86_64 :x8664-target :amd64 (:and :word-size=64 :pc386))
       (:x86 :x86 :i386 :i486 :i586 :i686 :pentium3 :pentium4 :pc386 :iapx386 :x8632-target)
       (:ppc64 :ppc64 :ppc64-target) (:ppc32 :ppc32 :ppc32-target :ppc :powerpc)
       :hppa64 :hppa :sparc64 (:sparc32 :sparc32 :sparc)
       :mipsel :mipseb :mips :alpha (:arm :arm :arm-target) :imach
       ;; Java comes last: if someone uses C via CFFI or otherwise JNA or JNI,
       ;; we may have to segregate the code still by architecture.
       (:java :java :java-1.4 :java-1.5 :java-1.6 :java-1.7))))

  #+clozure
  (defun ccl-fasl-version ()
    ;; the fasl version is target-dependent from CCL 1.8 on.
    (or (let ((s 'ccl::target-fasl-version))
          (and (fboundp s) (funcall s)))
        (and (boundp 'ccl::fasl-version)
             (symbol-value 'ccl::fasl-version))
        (error "Can't determine fasl version.")))

  (defun lisp-version-string ()
    "return a string that identifies the current Lisp implementation version"
    (let ((s (lisp-implementation-version)))
      (car ; as opposed to OR, this idiom prevents some unreachable code warning
       (list
        #+allegro
        (format nil "~A~@[~A~]~@[~A~]~@[~A~]"
                excl::*common-lisp-version-number*
                ;; M means "modern", as opposed to ANSI-compatible mode (which I consider default)
                (and (eq excl:*current-case-mode* :case-sensitive-lower) "M")
                ;; Note if not using International ACL
                ;; see http://www.franz.com/support/documentation/8.1/doc/operators/excl/ics-target-case.htm
                (excl:ics-target-case (:-ics "8"))
                (and (member :smp *features*) "S"))
        #+armedbear (format nil "~a-fasl~a" s system::*fasl-version*)
        #+clisp
        (subseq s 0 (position #\space s)) ; strip build information (date, etc.)
        #+clozure
        (format nil "~d.~d-f~d" ; shorten for windows
                ccl::*openmcl-major-version*
                ccl::*openmcl-minor-version*
                (logand (ccl-fasl-version) #xFF))
        #+cmu (substitute #\- #\/ s)
        #+scl (format nil "~A~A" s
                      ;; ANSI upper case vs lower case.
                      (ecase ext:*case-mode* (:upper "") (:lower "l")))
        #+ecl (format nil "~A~@[-~A~]" s
                      (let ((vcs-id (ext:lisp-implementation-vcs-id)))
                        (subseq vcs-id 0 (min (length vcs-id) 8))))
        #+gcl (subseq s (1+ (position #\space s)))
        #+genera
        (multiple-value-bind (major minor) (sct:get-system-version "System")
          (format nil "~D.~D" major minor))
        #+mcl (subseq s 8) ; strip the leading "Version "
        s))))

  (defun implementation-identifier ()
    "Return a string that identifies the ABI of the current implementation,
suitable for use as a directory name to segregate Lisp FASLs, C dynamic libraries, etc."
    (substitute-if
     #\_ #'(lambda (x) (find x " /:;&^\\|?<>(){}[]$#`'\""))
     (format nil "~(~a~@{~@[-~a~]~}~)"
             (or (implementation-type) (lisp-implementation-type))
             (or (lisp-version-string) (lisp-implementation-version))
             (or (operating-system) (software-type))
             (or (architecture) (machine-type))))))


;;;; Other system information

(with-upgradability ()
  (defun hostname ()
    "return the hostname of the current host"
    ;; Note: untested on RMCL
    #+(or abcl clozure cmu ecl genera lispworks mcl mkcl sbcl scl xcl) (machine-instance)
    #+cormanlisp "localhost" ;; is there a better way? Does it matter?
    #+allegro (symbol-call :excl.osi :gethostname)
    #+clisp (first (split-string (machine-instance) :separator " "))
    #+gcl (system:gethostname)))


;;; Current directory
(with-upgradability ()

  #+cmu
  (defun parse-unix-namestring* (unix-namestring)
    "variant of LISP::PARSE-UNIX-NAMESTRING that returns a pathname object"
    (multiple-value-bind (host device directory name type version)
        (lisp::parse-unix-namestring unix-namestring 0 (length unix-namestring))
      (make-pathname :host (or host lisp::*unix-host*) :device device
                     :directory directory :name name :type type :version version)))

  (defun getcwd ()
    "Get the current working directory as per POSIX getcwd(3), as a pathname object"
    (or #+abcl (truename (symbol-call :asdf/filesystem :parse-native-namestring
                          (java:jstatic "getProperty" "java.lang.System" "user.dir")
                          :ensure-directory t))
        #+allegro (excl::current-directory)
        #+clisp (ext:default-directory)
        #+clozure (ccl:current-directory)
        #+(or cmu scl) (#+cmu parse-unix-namestring* #+scl lisp::parse-unix-namestring
                        (strcat (nth-value 1 (unix:unix-current-directory)) "/"))
        #+cormanlisp (pathname (pl::get-current-directory)) ;; Q: what type does it return?
        #+ecl (ext:getcwd)
        #+gcl (let ((*default-pathname-defaults* #p"")) (truename #p""))
        #+genera *default-pathname-defaults* ;; on a Lisp OS, it *is* canonical!
        #+lispworks (system:current-directory)
        #+mkcl (mk-ext:getcwd)
        #+sbcl (sb-ext:parse-native-namestring (sb-unix:posix-getcwd/))
        #+xcl (extensions:current-directory)
        (error "getcwd not supported on your implementation")))

  (defun chdir (x)
    "Change current directory, as per POSIX chdir(2), to a given pathname object"
    (if-let (x (pathname x))
      (or #+abcl (java:jstatic "setProperty" "java.lang.System" "user.dir" (namestring x))
          #+allegro (excl:chdir x)
          #+clisp (ext:cd x)
          #+clozure (setf (ccl:current-directory) x)
          #+(or cmu scl) (unix:unix-chdir (ext:unix-namestring x))
          #+cormanlisp (unless (zerop (win32::_chdir (namestring x)))
                         (error "Could not set current directory to ~A" x))
          #+ecl (ext:chdir x)
          #+genera (setf *default-pathname-defaults* x)
          #+lispworks (hcl:change-directory x)
          #+mkcl (mk-ext:chdir x)
          #+sbcl (progn (require :sb-posix) (symbol-call :sb-posix :chdir (sb-ext:native-namestring x)))
          (error "chdir not supported on your implementation")))))


;;;; -----------------------------------------------------------------
;;;; Windows shortcut support.  Based on:
;;;;
;;;; Jesse Hager: The Windows Shortcut File Format.
;;;; http://www.wotsit.org/list.asp?fc=13

#-(or clisp genera) ; CLISP doesn't need it, and READ-SEQUENCE annoys old Genera that doesn't need it
(with-upgradability ()
  (defparameter *link-initial-dword* 76)
  (defparameter *link-guid* #(1 20 2 0 0 0 0 0 192 0 0 0 0 0 0 70))

  (defun read-null-terminated-string (s)
    "Read a null-terminated string from an octet stream S"
    ;; note: doesn't play well with UNICODE
    (with-output-to-string (out)
      (loop :for code = (read-byte s)
            :until (zerop code)
            :do (write-char (code-char code) out))))

  (defun read-little-endian (s &optional (bytes 4))
    "Read a number in little-endian format from an byte (octet) stream S,
the number having BYTES octets (defaulting to 4)."
    (loop :for i :from 0 :below bytes
          :sum (ash (read-byte s) (* 8 i))))

  (defun parse-file-location-info (s)
    "helper to parse-windows-shortcut"
    (let ((start (file-position s))
          (total-length (read-little-endian s))
          (end-of-header (read-little-endian s))
          (fli-flags (read-little-endian s))
          (local-volume-offset (read-little-endian s))
          (local-offset (read-little-endian s))
          (network-volume-offset (read-little-endian s))
          (remaining-offset (read-little-endian s)))
      (declare (ignore total-length end-of-header local-volume-offset))
      (unless (zerop fli-flags)
        (cond
          ((logbitp 0 fli-flags)
           (file-position s (+ start local-offset)))
          ((logbitp 1 fli-flags)
           (file-position s (+ start
                               network-volume-offset
                               #x14))))
        (strcat (read-null-terminated-string s)
                (progn
                  (file-position s (+ start remaining-offset))
                  (read-null-terminated-string s))))))

  (defun parse-windows-shortcut (pathname)
    "From a .lnk windows shortcut, extract the pathname linked to"
    ;; NB: doesn't do much checking & doesn't look like it will work well with UNICODE.
    (with-open-file (s pathname :element-type '(unsigned-byte 8))
      (handler-case
          (when (and (= (read-little-endian s) *link-initial-dword*)
                     (let ((header (make-array (length *link-guid*))))
                       (read-sequence header s)
                       (equalp header *link-guid*)))
            (let ((flags (read-little-endian s)))
              (file-position s 76)        ;skip rest of header
              (when (logbitp 0 flags)
                ;; skip shell item id list
                (let ((length (read-little-endian s 2)))
                  (file-position s (+ length (file-position s)))))
              (cond
                ((logbitp 1 flags)
                 (parse-file-location-info s))
                (t
                 (when (logbitp 2 flags)
                   ;; skip description string
                   (let ((length (read-little-endian s 2)))
                     (file-position s (+ length (file-position s)))))
                 (when (logbitp 3 flags)
                   ;; finally, our pathname
                   (let* ((length (read-little-endian s 2))
                          (buffer (make-array length)))
                     (read-sequence buffer s)
                     (map 'string #'code-char buffer)))))))
        (end-of-file (c)
          (declare (ignore c))
          nil)))))


;;;; -------------------------------------------------------------------------
;;;; Portability layer around Common Lisp pathnames
;; This layer allows for portable manipulation of pathname objects themselves,
;; which all is necessary prior to any access the filesystem or environment.

(uiop/package:define-package :uiop/pathname
  (:nicknames :asdf/pathname)
  (:recycle :uiop/pathname :asdf/pathname :asdf)
  (:use :uiop/common-lisp :uiop/package :uiop/utility :uiop/os)
  (:export
   ;; Making and merging pathnames, portably
   #:normalize-pathname-directory-component #:denormalize-pathname-directory-component
   #:merge-pathname-directory-components #:*unspecific-pathname-type* #:make-pathname*
   #:make-pathname-component-logical #:make-pathname-logical
   #:merge-pathnames*
   #:nil-pathname #:*nil-pathname* #:with-pathname-defaults
   ;; Predicates
   #:pathname-equal #:logical-pathname-p #:physical-pathname-p #:physicalize-pathname
   #:absolute-pathname-p #:relative-pathname-p #:hidden-pathname-p #:file-pathname-p
   ;; Directories
   #:pathname-directory-pathname #:pathname-parent-directory-pathname
   #:directory-pathname-p #:ensure-directory-pathname
   ;; Parsing filenames
   #:component-name-to-pathname-components
   #:split-name-type #:parse-unix-namestring #:unix-namestring
   #:split-unix-namestring-directory-components
   ;; Absolute and relative pathnames
   #:subpathname #:subpathname*
   #:ensure-absolute-pathname
   #:pathname-root #:pathname-host-pathname
   #:subpathp #:enough-pathname #:with-enough-pathname #:call-with-enough-pathname
   ;; Checking constraints
   #:ensure-pathname ;; implemented in filesystem.lisp to accommodate for existence constraints
   ;; Wildcard pathnames
   #:*wild* #:*wild-file* #:*wild-directory* #:*wild-inferiors* #:*wild-path* #:wilden
   ;; Translate a pathname
   #:relativize-directory-component #:relativize-pathname-directory
   #:directory-separator-for-host #:directorize-pathname-host-device
   #:translate-pathname*
   #:*output-translation-function*))
(in-package :uiop/pathname)

;;; Normalizing pathnames across implementations

(with-upgradability ()
  (defun normalize-pathname-directory-component (directory)
    "Convert the DIRECTORY component from a format usable by the underlying
implementation's MAKE-PATHNAME and other primitives to a CLHS-standard format
that is a list and not a string."
    (cond
      #-(or cmu sbcl scl) ;; these implementations already normalize directory components.
      ((stringp directory) `(:absolute ,directory))
      ((or (null directory)
           (and (consp directory) (member (first directory) '(:absolute :relative))))
       directory)
      #+gcl
      ((consp directory)
       (cons :relative directory))
      (t
       (error (compatfmt "~@<Unrecognized pathname directory component ~S~@:>") directory))))

  (defun denormalize-pathname-directory-component (directory-component)
    "Convert the DIRECTORY-COMPONENT from a CLHS-standard format to a format usable
by the underlying implementation's MAKE-PATHNAME and other primitives"
    directory-component)

  (defun merge-pathname-directory-components (specified defaults)
    "Helper for MERGE-PATHNAMES* that handles directory components"
    (let ((directory (normalize-pathname-directory-component specified)))
      (ecase (first directory)
        ((nil) defaults)
        (:absolute specified)
        (:relative
         (let ((defdir (normalize-pathname-directory-component defaults))
               (reldir (cdr directory)))
           (cond
             ((null defdir)
              directory)
             ((not (eq :back (first reldir)))
              (append defdir reldir))
             (t
              (loop :with defabs = (first defdir)
                    :with defrev = (reverse (rest defdir))
                    :while (and (eq :back (car reldir))
                                (or (and (eq :absolute defabs) (null defrev))
                                    (stringp (car defrev))))
                    :do (pop reldir) (pop defrev)
                    :finally (return (cons defabs (append (reverse defrev) reldir)))))))))))

  ;; Giving :unspecific as :type argument to make-pathname is not portable.
  ;; See CLHS make-pathname and 19.2.2.2.3.
  ;; This will be :unspecific if supported, or NIL if not.
  (defparameter *unspecific-pathname-type*
    #+(or abcl allegro clozure cmu genera lispworks sbcl scl) :unspecific
    #+(or clisp ecl mkcl gcl xcl #|These haven't been tested:|# cormanlisp mcl) nil
    "Unspecific type component to use with the underlying implementation's MAKE-PATHNAME")

  (defun make-pathname* (&rest keys &key (directory nil)
                                      host (device () #+allegro devicep) name type version defaults
                                      #+scl &allow-other-keys)
    "Takes arguments like CL:MAKE-PATHNAME in the CLHS, and
   tries hard to make a pathname that will actually behave as documented,
   despite the peculiarities of each implementation"
    ;; TODO: reimplement defaulting for MCL, whereby an explicit NIL should override the defaults.
    (declare (ignorable host device directory name type version defaults))
    (apply 'make-pathname
           (append
            #+allegro (when (and devicep (null device)) `(:device :unspecific))
            keys)))

  (defun make-pathname-component-logical (x)
    "Make a pathname component suitable for use in a logical-pathname"
    (typecase x
      ((eql :unspecific) nil)
      #+clisp (string (string-upcase x))
      #+clisp (cons (mapcar 'make-pathname-component-logical x))
      (t x)))

  (defun make-pathname-logical (pathname host)
    "Take a PATHNAME's directory, name, type and version components,
and make a new pathname with corresponding components and specified logical HOST"
    (make-pathname*
     :host host
     :directory (make-pathname-component-logical (pathname-directory pathname))
     :name (make-pathname-component-logical (pathname-name pathname))
     :type (make-pathname-component-logical (pathname-type pathname))
     :version (make-pathname-component-logical (pathname-version pathname))))

  (defun merge-pathnames* (specified &optional (defaults *default-pathname-defaults*))
    "MERGE-PATHNAMES* is like MERGE-PATHNAMES except that
if the SPECIFIED pathname does not have an absolute directory,
then the HOST and DEVICE both come from the DEFAULTS, whereas
if the SPECIFIED pathname does have an absolute directory,
then the HOST and DEVICE both come from the SPECIFIED pathname.
This is what users want on a modern Unix or Windows operating system,
unlike the MERGE-PATHNAMES behavior.
Also, if either argument is NIL, then the other argument is returned unmodified;
this is unlike MERGE-PATHNAMES which always merges with a pathname,
by default *DEFAULT-PATHNAME-DEFAULTS*, which cannot be NIL."
    (when (null specified) (return-from merge-pathnames* defaults))
    (when (null defaults) (return-from merge-pathnames* specified))
    #+scl
    (ext:resolve-pathname specified defaults)
    #-scl
    (let* ((specified (pathname specified))
           (defaults (pathname defaults))
           (directory (normalize-pathname-directory-component (pathname-directory specified)))
           (name (or (pathname-name specified) (pathname-name defaults)))
           (type (or (pathname-type specified) (pathname-type defaults)))
           (version (or (pathname-version specified) (pathname-version defaults))))
      (labels ((unspecific-handler (p)
                 (if (typep p 'logical-pathname) #'make-pathname-component-logical #'identity)))
        (multiple-value-bind (host device directory unspecific-handler)
            (ecase (first directory)
              ((:absolute)
               (values (pathname-host specified)
                       (pathname-device specified)
                       directory
                       (unspecific-handler specified)))
              ((nil :relative)
               (values (pathname-host defaults)
                       (pathname-device defaults)
                       (merge-pathname-directory-components directory (pathname-directory defaults))
                       (unspecific-handler defaults))))
          (make-pathname* :host host :device device :directory directory
                          :name (funcall unspecific-handler name)
                          :type (funcall unspecific-handler type)
                          :version (funcall unspecific-handler version))))))

  (defun logical-pathname-p (x)
    "is X a logical-pathname?"
    (typep x 'logical-pathname))

  (defun physical-pathname-p (x)
    "is X a pathname that is not a logical-pathname?"
    (and (pathnamep x) (not (logical-pathname-p x))))

  (defun physicalize-pathname (x)
    "if X is a logical pathname, use translate-logical-pathname on it."
    ;; Ought to be the same as translate-logical-pathname, except the latter borks on CLISP
    (let ((p (when x (pathname x))))
      (if (logical-pathname-p p) (translate-logical-pathname p) p)))

  (defun nil-pathname (&optional (defaults *default-pathname-defaults*))
    "A pathname that is as neutral as possible for use as defaults
when merging, making or parsing pathnames"
    ;; 19.2.2.2.1 says a NIL host can mean a default host;
    ;; see also "valid physical pathname host" in the CLHS glossary, that suggests
    ;; strings and lists of strings or :unspecific
    ;; But CMUCL decides to die on NIL.
    ;; MCL has issues with make-pathname, nil and defaulting
    (declare (ignorable defaults))
    #.`(make-pathname* :directory nil :name nil :type nil :version nil
                       :device (or #+(and mkcl unix) :unspecific)
                       :host (or #+cmu lisp::*unix-host* #+(and mkcl unix) "localhost")
                       #+scl ,@'(:scheme nil :scheme-specific-part nil
                                 :username nil :password nil :parameters nil :query nil :fragment nil)
                       ;; the default shouldn't matter, but we really want something physical
                       #-mcl ,@'(:defaults defaults)))

  (defvar *nil-pathname* (nil-pathname (physicalize-pathname (user-homedir-pathname)))
    "A pathname that is as neutral as possible for use as defaults
when merging, making or parsing pathnames")

  (defmacro with-pathname-defaults ((&optional defaults) &body body)
    "Execute BODY in a context where the *DEFAULT-PATHNAME-DEFAULTS* are as neutral as possible
when merging, making or parsing pathnames"
    `(let ((*default-pathname-defaults* ,(or defaults '*nil-pathname*))) ,@body)))


;;; Some pathname predicates
(with-upgradability ()
  (defun pathname-equal (p1 p2)
    "Are the two pathnames P1 and P2 reasonably equal in the paths they denote?"
    (when (stringp p1) (setf p1 (pathname p1)))
    (when (stringp p2) (setf p2 (pathname p2)))
    (flet ((normalize-component (x)
             (unless (member x '(nil :unspecific :newest (:relative)) :test 'equal)
               x)))
      (macrolet ((=? (&rest accessors)
                   (flet ((frob (x)
                            (reduce 'list (cons 'normalize-component accessors)
                                    :initial-value x :from-end t)))
                     `(equal ,(frob 'p1) ,(frob 'p2)))))
        (or (and (null p1) (null p2))
            (and (pathnamep p1) (pathnamep p2)
                 (and (=? pathname-host)
                      #-(and mkcl unix) (=? pathname-device)
                      (=? normalize-pathname-directory-component pathname-directory)
                      (=? pathname-name)
                      (=? pathname-type)
                      #-mkcl (=? pathname-version)))))))

  (defun absolute-pathname-p (pathspec)
    "If PATHSPEC is a pathname or namestring object that parses as a pathname
possessing an :ABSOLUTE directory component, return the (parsed) pathname.
Otherwise return NIL"
    (and pathspec
         (typep pathspec '(or null pathname string))
         (let ((pathname (pathname pathspec)))
           (and (eq :absolute (car (normalize-pathname-directory-component
                                    (pathname-directory pathname))))
                pathname))))

  (defun relative-pathname-p (pathspec)
    "If PATHSPEC is a pathname or namestring object that parses as a pathname
possessing a :RELATIVE or NIL directory component, return the (parsed) pathname.
Otherwise return NIL"
    (and pathspec
         (typep pathspec '(or null pathname string))
         (let* ((pathname (pathname pathspec))
                (directory (normalize-pathname-directory-component
                            (pathname-directory pathname))))
           (when (or (null directory) (eq :relative (car directory)))
             pathname))))

  (defun hidden-pathname-p (pathname)
    "Return a boolean that is true if the pathname is hidden as per Unix style,
i.e. its name starts with a dot."
    (and pathname (equal (first-char (pathname-name pathname)) #\.)))

  (defun file-pathname-p (pathname)
    "Does PATHNAME represent a file, i.e. has a non-null NAME component?

Accepts NIL, a string (converted through PARSE-NAMESTRING) or a PATHNAME.

Note that this does _not_ check to see that PATHNAME points to an
actually-existing file.

Returns the (parsed) PATHNAME when true"
    (when pathname
      (let* ((pathname (pathname pathname))
             (name (pathname-name pathname)))
        (when (not (member name '(nil :unspecific "") :test 'equal))
          pathname)))))


;;; Directory pathnames
(with-upgradability ()
  (defun pathname-directory-pathname (pathname)
    "Returns a new pathname with same HOST, DEVICE, DIRECTORY as PATHNAME,
and NIL NAME, TYPE and VERSION components"
    (when pathname
      (make-pathname :name nil :type nil :version nil :defaults pathname)))

  (defun pathname-parent-directory-pathname (pathname)
    "Returns a new pathname that corresponds to the parent of the current pathname's directory,
i.e. removing one level of depth in the DIRECTORY component. e.g. if pathname is
Unix pathname /foo/bar/baz/file.type then return /foo/bar/"
    (when pathname
      (make-pathname* :name nil :type nil :version nil
                      :directory (merge-pathname-directory-components
                                  '(:relative :back) (pathname-directory pathname))
                      :defaults pathname)))

  (defun directory-pathname-p (pathname)
    "Does PATHNAME represent a directory?

A directory-pathname is a pathname _without_ a filename. The three
ways that the filename components can be missing are for it to be NIL,
:UNSPECIFIC or the empty string.

Note that this does _not_ check to see that PATHNAME points to an
actually-existing directory."
    (when pathname
      ;; I tried using Allegro's excl:file-directory-p, but this cannot be done,
      ;; because it rejects apparently legal pathnames as
      ;; ill-formed. [2014/02/10:rpg]
      (let ((pathname (pathname pathname)))
        (flet ((check-one (x)
                 (member x '(nil :unspecific) :test 'equal)))
          (and (not (wild-pathname-p pathname))
               (check-one (pathname-name pathname))
               (check-one (pathname-type pathname))
               t)))))

  (defun ensure-directory-pathname (pathspec &optional (on-error 'error))
    "Converts the non-wild pathname designator PATHSPEC to directory form."
    (cond
      ((stringp pathspec)
       (ensure-directory-pathname (pathname pathspec)))
      ((not (pathnamep pathspec))
       (call-function on-error (compatfmt "~@<Invalid pathname designator ~S~@:>") pathspec))
      ((wild-pathname-p pathspec)
       (call-function on-error (compatfmt "~@<Can't reliably convert wild pathname ~3i~_~S~@:>") pathspec))
      ((directory-pathname-p pathspec)
       pathspec)
      (t
       (make-pathname* :directory (append (or (normalize-pathname-directory-component
                                               (pathname-directory pathspec))
                                              (list :relative))
                                          (list (file-namestring pathspec)))
                       :name nil :type nil :version nil :defaults pathspec)))))


;;; Parsing filenames
(with-upgradability ()
  (defun split-unix-namestring-directory-components
      (unix-namestring &key ensure-directory dot-dot)
    "Splits the path string UNIX-NAMESTRING, returning four values:
A flag that is either :absolute or :relative, indicating
   how the rest of the values are to be interpreted.
A directory path --- a list of strings and keywords, suitable for
   use with MAKE-PATHNAME when prepended with the flag value.
   Directory components with an empty name or the name . are removed.
   Any directory named .. is read as DOT-DOT, or :BACK if it's NIL (not :UP).
A last-component, either a file-namestring including type extension,
   or NIL in the case of a directory pathname.
A flag that is true iff the unix-style-pathname was just
   a file-namestring without / path specification.
ENSURE-DIRECTORY forces the namestring to be interpreted as a directory pathname:
the third return value will be NIL, and final component of the namestring
will be treated as part of the directory path.

An empty string is thus read as meaning a pathname object with all fields nil.

Note that : characters will NOT be interpreted as host specification.
Absolute pathnames are only appropriate on Unix-style systems.

The intention of this function is to support structured component names,
e.g., \(:file \"foo/bar\"\), which will be unpacked to relative pathnames."
    (check-type unix-namestring string)
    (check-type dot-dot (member nil :back :up))
    (if (and (not (find #\/ unix-namestring)) (not ensure-directory)
             (plusp (length unix-namestring)))
        (values :relative () unix-namestring t)
        (let* ((components (split-string unix-namestring :separator "/"))
               (last-comp (car (last components))))
          (multiple-value-bind (relative components)
              (if (equal (first components) "")
                  (if (equal (first-char unix-namestring) #\/)
                      (values :absolute (cdr components))
                      (values :relative nil))
                  (values :relative components))
            (setf components (remove-if #'(lambda (x) (member x '("" ".") :test #'equal))
                                        components))
            (setf components (substitute (or dot-dot :back) ".." components :test #'equal))
            (cond
              ((equal last-comp "")
               (values relative components nil nil)) ; "" already removed from components
              (ensure-directory
               (values relative components nil nil))
              (t
               (values relative (butlast components) last-comp nil)))))))

  (defun split-name-type (filename)
    "Split a filename into two values NAME and TYPE that are returned.
We assume filename has no directory component.
The last . if any separates name and type from from type,
except that if there is only one . and it is in first position,
the whole filename is the NAME with an empty type.
NAME is always a string.
For an empty type, *UNSPECIFIC-PATHNAME-TYPE* is returned."
    (check-type filename string)
    (assert (plusp (length filename)))
    (destructuring-bind (name &optional (type *unspecific-pathname-type*))
        (split-string filename :max 2 :separator ".")
      (if (equal name "")
          (values filename *unspecific-pathname-type*)
          (values name type))))

  (defun parse-unix-namestring (name &rest keys &key type defaults dot-dot ensure-directory
                                &allow-other-keys)
    "Coerce NAME into a PATHNAME using standard Unix syntax.

Unix syntax is used whether or not the underlying system is Unix;
on such non-Unix systems it is only usable but for relative pathnames;
but especially to manipulate relative pathnames portably, it is of crucial
to possess a portable pathname syntax independent of the underlying OS.
This is what PARSE-UNIX-NAMESTRING provides, and why we use it in ASDF.

When given a PATHNAME object, just return it untouched.
When given NIL, just return NIL.
When given a non-null SYMBOL, first downcase its name and treat it as a string.
When given a STRING, portably decompose it into a pathname as below.

#\\/ separates directory components.

The last #\\/-separated substring is interpreted as follows:
1- If TYPE is :DIRECTORY or ENSURE-DIRECTORY is true,
 the string is made the last directory component, and NAME and TYPE are NIL.
 if the string is empty, it's the empty pathname with all slots NIL.
2- If TYPE is NIL, the substring is a file-namestring, and its NAME and TYPE
 are separated by SPLIT-NAME-TYPE.
3- If TYPE is a string, it is the given TYPE, and the whole string is the NAME.

Directory components with an empty name the name . are removed.
Any directory named .. is read as DOT-DOT,
which must be one of :BACK or :UP and defaults to :BACK.

HOST, DEVICE and VERSION components are taken from DEFAULTS,
which itself defaults to *NIL-PATHNAME*, also used if DEFAULTS is NIL.
No host or device can be specified in the string itself,
which makes it unsuitable for absolute pathnames outside Unix.

For relative pathnames, these components (and hence the defaults) won't matter
if you use MERGE-PATHNAMES* but will matter if you use MERGE-PATHNAMES,
which is an important reason to always use MERGE-PATHNAMES*.

Arbitrary keys are accepted, and the parse result is passed to ENSURE-PATHNAME
with those keys, removing TYPE DEFAULTS and DOT-DOT.
When you're manipulating pathnames that are supposed to make sense portably
even though the OS may not be Unixish, we recommend you use :WANT-RELATIVE T
to throw an error if the pathname is absolute"
    (block nil
      (check-type type (or null string (eql :directory)))
      (when ensure-directory
        (setf type :directory))
      (etypecase name
        ((or null pathname) (return name))
        (symbol
         (setf name (string-downcase name)))
        (string))
      (multiple-value-bind (relative path filename file-only)
          (split-unix-namestring-directory-components
           name :dot-dot dot-dot :ensure-directory (eq type :directory))
        (multiple-value-bind (name type)
            (cond
              ((or (eq type :directory) (null filename))
               (values nil nil))
              (type
               (values filename type))
              (t
               (split-name-type filename)))
          (apply 'ensure-pathname
                 (make-pathname*
                  :directory (unless file-only (cons relative path))
                  :name name :type type
                  :defaults (or #-mcl defaults *nil-pathname*))
                 (remove-plist-keys '(:type :dot-dot :defaults) keys))))))

  (defun unix-namestring (pathname)
    "Given a non-wild PATHNAME, return a Unix-style namestring for it.
If the PATHNAME is NIL or a STRING, return it unchanged.

This only considers the DIRECTORY, NAME and TYPE components of the pathname.
This is a portable solution for representing relative pathnames,
But unless you are running on a Unix system, it is not a general solution
to representing native pathnames.

An error is signaled if the argument is not NULL, a STRING or a PATHNAME,
or if it is a PATHNAME but some of its components are not recognized."
    (etypecase pathname
      ((or null string) pathname)
      (pathname
       (with-output-to-string (s)
         (flet ((err () #+lispworks (describe pathname) (error "Not a valid unix-namestring ~S" pathname)))
           (let* ((dir (normalize-pathname-directory-component (pathname-directory pathname)))
                  (name (pathname-name pathname))
                  (name (and (not (eq name :unspecific)) name))
                  (type (pathname-type pathname))
                  (type (and (not (eq type :unspecific)) type)))
             (cond
               ((member dir '(nil :unspecific)))
               ((eq dir '(:relative)) (princ "./" s))
               ((consp dir)
                (destructuring-bind (relabs &rest dirs) dir
                  (or (member relabs '(:relative :absolute)) (err))
                  (when (eq relabs :absolute) (princ #\/ s))
                  (loop :for x :in dirs :do
                    (cond
                      ((member x '(:back :up)) (princ "../" s))
                      ((equal x "") (err))
                      ;;((member x '("." "..") :test 'equal) (err))
                      ((stringp x) (format s "~A/" x))
                      (t (err))))))
               (t (err)))
             (cond
               (name
                (unless (and (stringp name) (or (null type) (stringp type))) (err))
                (format s "~A~@[.~A~]" name type))
               (t
                (or (null type) (err)))))))))))

;;; Absolute and relative pathnames
(with-upgradability ()
  (defun subpathname (pathname subpath &key type)
    "This function takes a PATHNAME and a SUBPATH and a TYPE.
If SUBPATH is already a PATHNAME object (not namestring),
and is an absolute pathname at that, it is returned unchanged;
otherwise, SUBPATH is turned into a relative pathname with given TYPE
as per PARSE-UNIX-NAMESTRING with :WANT-RELATIVE T :TYPE TYPE,
then it is merged with the PATHNAME-DIRECTORY-PATHNAME of PATHNAME."
    (or (and (pathnamep subpath) (absolute-pathname-p subpath))
        (merge-pathnames* (parse-unix-namestring subpath :type type :want-relative t)
                          (pathname-directory-pathname pathname))))

  (defun subpathname* (pathname subpath &key type)
    "returns NIL if the base pathname is NIL, otherwise like SUBPATHNAME."
    (and pathname
         (subpathname (ensure-directory-pathname pathname) subpath :type type)))

  (defun pathname-root (pathname)
    "return the root directory for the host and device of given PATHNAME"
    (make-pathname* :directory '(:absolute)
                    :name nil :type nil :version nil
                    :defaults pathname ;; host device, and on scl, *some*
                    ;; scheme-specific parts: port username password, not others:
                    . #.(or #+scl '(:parameters nil :query nil :fragment nil))))

  (defun pathname-host-pathname (pathname)
    "return a pathname with the same host as given PATHNAME, and all other fields NIL"
    (make-pathname* :directory nil
                    :name nil :type nil :version nil :device nil
                    :defaults pathname ;; host device, and on scl, *some*
                    ;; scheme-specific parts: port username password, not others:
                    . #.(or #+scl '(:parameters nil :query nil :fragment nil))))

  (defun ensure-absolute-pathname (path &optional defaults (on-error 'error))
    "Given a pathname designator PATH, return an absolute pathname as specified by PATH
considering the DEFAULTS, or, if not possible, use CALL-FUNCTION on the specified ON-ERROR behavior,
with a format control-string and other arguments as arguments"
    (cond
      ((absolute-pathname-p path))
      ((stringp path) (ensure-absolute-pathname (pathname path) defaults on-error))
      ((not (pathnamep path)) (call-function on-error "not a valid pathname designator ~S" path))
      ((let ((default-pathname (if (pathnamep defaults) defaults (call-function defaults))))
         (or (if (absolute-pathname-p default-pathname)
                 (absolute-pathname-p (merge-pathnames* path default-pathname))
                 (call-function on-error "Default pathname ~S is not an absolute pathname"
                                default-pathname))
             (call-function on-error "Failed to merge ~S with ~S into an absolute pathname"
                            path default-pathname))))
      (t (call-function on-error
                        "Cannot ensure ~S is evaluated as an absolute pathname with defaults ~S"
                        path defaults))))

  (defun subpathp (maybe-subpath base-pathname)
    "if MAYBE-SUBPATH is a pathname that is under BASE-PATHNAME, return a pathname object that
when used with MERGE-PATHNAMES* with defaults BASE-PATHNAME, returns MAYBE-SUBPATH."
    (and (pathnamep maybe-subpath) (pathnamep base-pathname)
         (absolute-pathname-p maybe-subpath) (absolute-pathname-p base-pathname)
         (directory-pathname-p base-pathname) (not (wild-pathname-p base-pathname))
         (pathname-equal (pathname-root maybe-subpath) (pathname-root base-pathname))
         (with-pathname-defaults ()
           (let ((enough (enough-namestring maybe-subpath base-pathname)))
             (and (relative-pathname-p enough) (pathname enough))))))

  (defun enough-pathname (maybe-subpath base-pathname)
    "if MAYBE-SUBPATH is a pathname that is under BASE-PATHNAME, return a pathname object that
when used with MERGE-PATHNAMES* with defaults BASE-PATHNAME, returns MAYBE-SUBPATH."
    (let ((sub (when maybe-subpath (pathname maybe-subpath)))
	  (base (when base-pathname (ensure-absolute-pathname (pathname base-pathname)))))
      (or (and base (subpathp sub base)) sub)))

  (defun call-with-enough-pathname (maybe-subpath defaults-pathname thunk)
    "In a context where *DEFAULT-PATHNAME-DEFAULTS* is bound to DEFAULTS-PATHNAME (if not null,
or else to its current value), call THUNK with ENOUGH-PATHNAME for MAYBE-SUBPATH
given DEFAULTS-PATHNAME as a base pathname."
    (let ((enough (enough-pathname maybe-subpath defaults-pathname))
          (*default-pathname-defaults* (or defaults-pathname *default-pathname-defaults*)))
      (funcall thunk enough)))

  (defmacro with-enough-pathname ((pathname-var &key (pathname pathname-var)
                                                  (defaults *default-pathname-defaults*))
                                  &body body)
    "Shorthand syntax for CALL-WITH-ENOUGH-PATHNAME"
    `(call-with-enough-pathname ,pathname ,defaults #'(lambda (,pathname-var) ,@body))))


;;; Wildcard pathnames
(with-upgradability ()
  (defparameter *wild* (or #+cormanlisp "*" :wild)
    "Wild component for use with MAKE-PATHNAME")
  (defparameter *wild-directory-component* (or :wild)
    "Wild directory component for use with MAKE-PATHNAME")
  (defparameter *wild-inferiors-component* (or :wild-inferiors)
    "Wild-inferiors directory component for use with MAKE-PATHNAME")
  (defparameter *wild-file*
    (make-pathname :directory nil :name *wild* :type *wild*
                   :version (or #-(or allegro abcl xcl) *wild*))
    "A pathname object with wildcards for matching any file in a given directory")
  (defparameter *wild-directory*
    (make-pathname* :directory `(:relative ,*wild-directory-component*)
                    :name nil :type nil :version nil)
    "A pathname object with wildcards for matching any subdirectory")
  (defparameter *wild-inferiors*
    (make-pathname* :directory `(:relative ,*wild-inferiors-component*)
                    :name nil :type nil :version nil)
    "A pathname object with wildcards for matching any recursive subdirectory")
  (defparameter *wild-path*
    (merge-pathnames* *wild-file* *wild-inferiors*)
    "A pathname object with wildcards for matching any file in any recursive subdirectory")

  (defun wilden (path)
    "From a pathname, return a wildcard pathname matching any file in any subdirectory of given pathname's directory"
    (merge-pathnames* *wild-path* path)))


;;; Translate a pathname
(with-upgradability ()
  (defun relativize-directory-component (directory-component)
    "Given the DIRECTORY-COMPONENT of a pathname, return an otherwise similar relative directory component"
    (let ((directory (normalize-pathname-directory-component directory-component)))
      (cond
        ((stringp directory)
         (list :relative directory))
        ((eq (car directory) :absolute)
         (cons :relative (cdr directory)))
        (t
         directory))))

  (defun relativize-pathname-directory (pathspec)
    "Given a PATHNAME, return a relative pathname with otherwise the same components"
    (let ((p (pathname pathspec)))
      (make-pathname*
       :directory (relativize-directory-component (pathname-directory p))
       :defaults p)))

  (defun directory-separator-for-host (&optional (pathname *default-pathname-defaults*))
    "Given a PATHNAME, return the character used to delimit directory names on this host and device."
    (let ((foo (make-pathname* :directory '(:absolute "FOO") :defaults pathname)))
      (last-char (namestring foo))))

  #-scl
  (defun directorize-pathname-host-device (pathname)
    "Given a PATHNAME, return a pathname that has representations of its HOST and DEVICE components
added to its DIRECTORY component. This is useful for output translations."
    #+(or unix abcl)
    (when (and #+abcl (os-unix-p) (physical-pathname-p pathname))
      (return-from directorize-pathname-host-device pathname))
    (let* ((root (pathname-root pathname))
           (wild-root (wilden root))
           (absolute-pathname (merge-pathnames* pathname root))
           (separator (directory-separator-for-host root))
           (root-namestring (namestring root))
           (root-string
             (substitute-if #\/
                            #'(lambda (x) (or (eql x #\:)
                                              (eql x separator)))
                            root-namestring)))
      (multiple-value-bind (relative path filename)
          (split-unix-namestring-directory-components root-string :ensure-directory t)
        (declare (ignore relative filename))
        (let ((new-base
                (make-pathname* :defaults root :directory `(:absolute ,@path))))
          (translate-pathname absolute-pathname wild-root (wilden new-base))))))

  #+scl
  (defun directorize-pathname-host-device (pathname)
    (let ((scheme (ext:pathname-scheme pathname))
          (host (pathname-host pathname))
          (port (ext:pathname-port pathname))
          (directory (pathname-directory pathname)))
      (flet ((specificp (x) (and x (not (eq x :unspecific)))))
        (if (or (specificp port)
                (and (specificp host) (plusp (length host)))
                (specificp scheme))
            (let ((prefix ""))
              (when (specificp port)
                (setf prefix (format nil ":~D" port)))
              (when (and (specificp host) (plusp (length host)))
                (setf prefix (strcat host prefix)))
              (setf prefix (strcat ":" prefix))
              (when (specificp scheme)
                (setf prefix (strcat scheme prefix)))
              (assert (and directory (eq (first directory) :absolute)))
              (make-pathname* :directory `(:absolute ,prefix ,@(rest directory))
                              :defaults pathname)))
        pathname)))

  (defun* (translate-pathname*) (path absolute-source destination &optional root source)
    "A wrapper around TRANSLATE-PATHNAME to be used by the ASDF output-translations facility.
PATH is the pathname to be translated.
ABSOLUTE-SOURCE is an absolute pathname to use as source for translate-pathname,
DESTINATION is either a function, to be called with PATH and ABSOLUTE-SOURCE,
or a relative pathname, to be merged with ROOT and used as destination for translate-pathname
or an absolute pathname, to be used as destination for translate-pathname.
In that last case, if ROOT is non-NIL, PATH is first transformated by DIRECTORIZE-PATHNAME-HOST-DEVICE."
    (declare (ignore source))
    (cond
      ((functionp destination)
       (funcall destination path absolute-source))
      ((eq destination t)
       path)
      ((not (pathnamep destination))
       (error "Invalid destination"))
      ((not (absolute-pathname-p destination))
       (translate-pathname path absolute-source (merge-pathnames* destination root)))
      (root
       (translate-pathname (directorize-pathname-host-device path) absolute-source destination))
      (t
       (translate-pathname path absolute-source destination))))

  (defvar *output-translation-function* 'identity
    "Hook for output translations.

This function needs to be idempotent, so that actions can work
whether their inputs were translated or not,
which they will be if we are composing operations. e.g. if some
create-lisp-op creates a lisp file from some higher-level input,
you need to still be able to use compile-op on that lisp file."))

;;;; -------------------------------------------------------------------------
;;;; Portability layer around Common Lisp filesystem access

(uiop/package:define-package :uiop/filesystem
  (:nicknames :asdf/filesystem)
  (:recycle :uiop/filesystem :asdf/pathname :asdf)
  (:use :uiop/common-lisp :uiop/package :uiop/utility :uiop/os :uiop/pathname)
  (:export
   ;; Native namestrings
   #:native-namestring #:parse-native-namestring
   ;; Probing the filesystem
   #:truename* #:safe-file-write-date #:probe-file* #:directory-exists-p #:file-exists-p
   #:directory* #:filter-logical-directory-results #:directory-files #:subdirectories
   #:collect-sub*directories
   ;; Resolving symlinks somewhat
   #:truenamize #:resolve-symlinks #:*resolve-symlinks* #:resolve-symlinks*
   ;; merging with cwd
   #:get-pathname-defaults #:call-with-current-directory #:with-current-directory
   ;; Environment pathnames
   #:inter-directory-separator #:split-native-pathnames-string
   #:getenv-pathname #:getenv-pathnames
   #:getenv-absolute-directory #:getenv-absolute-directories
   #:lisp-implementation-directory #:lisp-implementation-pathname-p
   ;; Simple filesystem operations
   #:ensure-all-directories-exist
   #:rename-file-overwriting-target
   #:delete-file-if-exists #:delete-empty-directory #:delete-directory-tree))
(in-package :uiop/filesystem)

;;; Native namestrings, as seen by the operating system calls rather than Lisp
(with-upgradability ()
  (defun native-namestring (x)
    "From a non-wildcard CL pathname, a return namestring suitable for passing to the operating system"
    (when x
      (let ((p (pathname x)))
        #+clozure (with-pathname-defaults () (ccl:native-translated-namestring p)) ; see ccl bug 978
        #+(or cmu scl) (ext:unix-namestring p nil)
        #+sbcl (sb-ext:native-namestring p)
        #-(or clozure cmu sbcl scl)
        (if (os-unix-p) (unix-namestring p)
            (namestring p)))))

  (defun parse-native-namestring (string &rest constraints &key ensure-directory &allow-other-keys)
    "From a native namestring suitable for use by the operating system, return
a CL pathname satisfying all the specified constraints as per ENSURE-PATHNAME"
    (check-type string (or string null))
    (let* ((pathname
             (when string
               (with-pathname-defaults ()
                 #+clozure (ccl:native-to-pathname string)
                 #+sbcl (sb-ext:parse-native-namestring string)
                 #-(or clozure sbcl)
                 (if (os-unix-p)
                     (parse-unix-namestring string :ensure-directory ensure-directory)
                     (parse-namestring string)))))
           (pathname
             (if ensure-directory
                 (and pathname (ensure-directory-pathname pathname))
                 pathname)))
      (apply 'ensure-pathname pathname constraints))))


;;; Probing the filesystem
(with-upgradability ()
  (defun truename* (p)
    "Nicer variant of TRUENAME that plays well with NIL and avoids logical pathname contexts"
    ;; avoids both logical-pathname merging and physical resolution issues
    (and p (handler-case (with-pathname-defaults () (truename p)) (file-error () nil))))

  (defun safe-file-write-date (pathname)
    "Safe variant of FILE-WRITE-DATE that may return NIL rather than raise an error."
    ;; If FILE-WRITE-DATE returns NIL, it's possible that
    ;; the user or some other agent has deleted an input file.
    ;; Also, generated files will not exist at the time planning is done
    ;; and calls compute-action-stamp which calls safe-file-write-date.
    ;; So it is very possible that we can't get a valid file-write-date,
    ;; and we can survive and we will continue the planning
    ;; as if the file were very old.
    ;; (or should we treat the case in a different, special way?)
    (and pathname
         (handler-case (file-write-date (physicalize-pathname pathname))
           (file-error () nil))))

  (defun probe-file* (p &key truename)
    "when given a pathname P (designated by a string as per PARSE-NAMESTRING),
probes the filesystem for a file or directory with given pathname.
If it exists, return its truename is ENSURE-PATHNAME is true,
or the original (parsed) pathname if it is false (the default)."
    (with-pathname-defaults () ;; avoids logical-pathname issues on some implementations
      (etypecase p
        (null nil)
        (string (probe-file* (parse-namestring p) :truename truename))
        (pathname
         (and (not (wild-pathname-p p))
              (handler-case
                  (or
                   #+allegro
                   (probe-file p :follow-symlinks truename)
                   #+gcl
                   (if truename
                       (truename* p)
                       (let ((kind (car (si::stat p))))
                         (when (eq kind :link)
                           (setf kind (ignore-errors (car (si::stat (truename* p))))))
                         (ecase kind
                           ((nil) nil)
                           ((:file :link)
                            (cond
                              ((file-pathname-p p) p)
                              ((directory-pathname-p p)
                               (subpathname p (car (last (pathname-directory p)))))))
                           (:directory (ensure-directory-pathname p)))))
                   #+clisp
                   #.(flet ((probe (probe)
                              `(let ((foundtrue ,probe))
                                 (cond
                                   (truename foundtrue)
                                   (foundtrue p)))))
                       (let* ((fs (or #-os-windows (find-symbol* '#:file-stat :posix nil)))
                              (pp (find-symbol* '#:probe-pathname :ext nil))
                              (resolve (if pp
                                           `(ignore-errors (,pp p))
                                           '(or (truename* p)
                                             (truename* (ignore-errors (ensure-directory-pathname p)))))))
                         (if fs
                             `(if truename
                                  ,resolve
                                  (and (ignore-errors (,fs p)) p))
                             (probe resolve))))
                   #-(or allegro clisp gcl)
                   (if truename
                       (probe-file p)
                       (ignore-errors
                        (let ((pp (physicalize-pathname p)))
                          (and
                           #+(or cmu scl) (unix:unix-stat (ext:unix-namestring pp))
                           #+(and lispworks unix) (system:get-file-stat pp)
                           #+sbcl (sb-unix:unix-stat (sb-ext:native-namestring pp))
                           #-(or cmu (and lispworks unix) sbcl scl) (file-write-date pp)
                           p)))))
                (file-error () nil)))))))

  (defun directory-exists-p (x)
    "Is X the name of a directory that exists on the filesystem?"
    #+allegro
    (excl:probe-directory x)
    #+clisp
    (handler-case (ext:probe-directory x)
           (sys::simple-file-error ()
             nil))
    #-(or allegro clisp)
    (let ((p (probe-file* x :truename t)))
      (and (directory-pathname-p p) p)))

  (defun file-exists-p (x)
    "Is X the name of a file that exists on the filesystem?"
    (let ((p (probe-file* x :truename t)))
      (and (file-pathname-p p) p)))

  (defun directory* (pathname-spec &rest keys &key &allow-other-keys)
    "Return a list of the entries in a directory by calling DIRECTORY.
Try to override the defaults to not resolving symlinks, if implementation allows."
    (apply 'directory pathname-spec
           (append keys '#.(or #+allegro '(:directories-are-files nil :follow-symbolic-links nil)
                               #+(or clozure digitool) '(:follow-links nil)
                               #+clisp '(:circle t :if-does-not-exist :ignore)
                               #+(or cmu scl) '(:follow-links nil :truenamep nil)
                               #+lispworks '(:link-transparency nil)
                               #+sbcl (when (find-symbol* :resolve-symlinks '#:sb-impl nil)
                                        '(:resolve-symlinks nil))))))

  (defun filter-logical-directory-results (directory entries merger)
    "Given ENTRIES in a DIRECTORY, remove if the directory is logical
the entries which are physical yet when transformed by MERGER have a different TRUENAME.
This function is used as a helper to DIRECTORY-FILES to avoid invalid entries when using logical-pathnames."
    (remove-duplicates ;; on CLISP, querying ~/ will return duplicates
     (if (logical-pathname-p directory)
         ;; Try hard to not resolve logical-pathname into physical pathnames;
         ;; otherwise logical-pathname users/lovers will be disappointed.
         ;; If directory* could use some implementation-dependent magic,
         ;; we will have logical pathnames already; otherwise,
         ;; we only keep pathnames for which specifying the name and
         ;; translating the LPN commute.
         (loop :for f :in entries
               :for p = (or (and (logical-pathname-p f) f)
                            (let* ((u (ignore-errors (call-function merger f))))
                              ;; The first u avoids a cumbersome (truename u) error.
                              ;; At this point f should already be a truename,
                              ;; but isn't quite in CLISP, for it doesn't have :version :newest
                              (and u (equal (truename* u) (truename* f)) u)))
               :when p :collect p)
         entries)
     :test 'pathname-equal))


  (defun directory-files (directory &optional (pattern *wild-file*))
    "Return a list of the files in a directory according to the PATTERN.
Subdirectories should NOT be returned.
  PATTERN defaults to a pattern carefully chosen based on the implementation;
override the default at your own risk.
  DIRECTORY-FILES tries NOT to resolve symlinks if the implementation
permits this."
    (let ((dir (pathname directory)))
      (when (logical-pathname-p dir)
        ;; Because of the filtering we do below,
        ;; logical pathnames have restrictions on wild patterns.
        ;; Not that the results are very portable when you use these patterns on physical pathnames.
        (when (wild-pathname-p dir)
          (error "Invalid wild pattern in logical directory ~S" directory))
        (unless (member (pathname-directory pattern) '(() (:relative)) :test 'equal)
          (error "Invalid file pattern ~S for logical directory ~S" pattern directory))
        (setf pattern (make-pathname-logical pattern (pathname-host dir))))
      (let* ((pat (merge-pathnames* pattern dir))
             (entries (append (ignore-errors (directory* pat))
                              #+(or clisp gcl)
                              (when (equal :wild (pathname-type pattern))
                                (ignore-errors (directory* (make-pathname :type nil :defaults pat)))))))
        (remove-if 'directory-pathname-p
                   (filter-logical-directory-results
                    directory entries
                    #'(lambda (f)
                        (make-pathname :defaults dir
                                       :name (make-pathname-component-logical (pathname-name f))
                                       :type (make-pathname-component-logical (pathname-type f))
                                       :version (make-pathname-component-logical (pathname-version f)))))))))

  (defun subdirectories (directory)
    "Given a DIRECTORY pathname designator, return a list of the subdirectories under it."
    (let* ((directory (ensure-directory-pathname directory))
           #-(or abcl cormanlisp genera xcl)
           (wild (merge-pathnames*
                  #-(or abcl allegro cmu lispworks sbcl scl xcl)
                  *wild-directory*
                  #+(or abcl allegro cmu lispworks sbcl scl xcl) "*.*"
                  directory))
           (dirs
             #-(or abcl cormanlisp genera xcl)
             (ignore-errors
              (directory* wild . #.(or #+clozure '(:directories t :files nil)
                                       #+mcl '(:directories t))))
             #+(or abcl xcl) (system:list-directory directory)
             #+cormanlisp (cl::directory-subdirs directory)
             #+genera (fs:directory-list directory))
           #+(or abcl allegro cmu genera lispworks sbcl scl xcl)
           (dirs (loop :for x :in dirs
                       :for d = #+(or abcl xcl) (extensions:probe-directory x)
                       #+allegro (excl:probe-directory x)
                       #+(or cmu sbcl scl) (directory-pathname-p x)
                       #+genera (getf (cdr x) :directory)
                       #+lispworks (lw:file-directory-p x)
                       :when d :collect #+(or abcl allegro xcl) d
                         #+genera (ensure-directory-pathname (first x))
                       #+(or cmu lispworks sbcl scl) x)))
      (filter-logical-directory-results
       directory dirs
       (let ((prefix (or (normalize-pathname-directory-component (pathname-directory directory))
                         '(:absolute)))) ; because allegro returns NIL for #p"FOO:"
         #'(lambda (d)
             (let ((dir (normalize-pathname-directory-component (pathname-directory d))))
               (and (consp dir) (consp (cdr dir))
                    (make-pathname
                     :defaults directory :name nil :type nil :version nil
                     :directory (append prefix (make-pathname-component-logical (last dir)))))))))))

  (defun collect-sub*directories (directory collectp recursep collector)
    "Given a DIRECTORY, call-function the COLLECTOR function designator
on the directory if COLLECTP returns true when CALL-FUNCTION'ed with the directory,
and recurse each of its subdirectories on which the RECURSEP returns true when CALL-FUNCTION'ed with them."
    (when (call-function collectp directory)
      (call-function collector directory))
    (dolist (subdir (subdirectories directory))
      (when (call-function recursep subdir)
        (collect-sub*directories subdir collectp recursep collector)))))

;;; Resolving symlinks somewhat
(with-upgradability ()
  (defun truenamize (pathname)
    "Resolve as much of a pathname as possible"
    (block nil
      (when (typep pathname '(or null logical-pathname)) (return pathname))
      (let ((p pathname))
        (unless (absolute-pathname-p p)
          (setf p (or (absolute-pathname-p (ensure-absolute-pathname p 'get-pathname-defaults nil))
                      (return p))))
        (when (logical-pathname-p p) (return p))
        (let ((found (probe-file* p :truename t)))
          (when found (return found)))
        (let* ((directory (normalize-pathname-directory-component (pathname-directory p)))
               (up-components (reverse (rest directory)))
               (down-components ()))
          (assert (eq :absolute (first directory)))
          (loop :while up-components :do
            (if-let (parent
                     (ignore-errors
                      (probe-file* (make-pathname* :directory `(:absolute ,@(reverse up-components))
                                                   :name nil :type nil :version nil :defaults p))))
              (if-let (simplified
                       (ignore-errors
                        (merge-pathnames*
                         (make-pathname* :directory `(:relative ,@down-components)
                                         :defaults p)
                         (ensure-directory-pathname parent))))
                (return simplified)))
            (push (pop up-components) down-components)
            :finally (return p))))))

  (defun resolve-symlinks (path)
    "Do a best effort at resolving symlinks in PATH, returning a partially or totally resolved PATH."
    #-allegro (truenamize path)
    #+allegro
    (if (physical-pathname-p path)
        (or (ignore-errors (excl:pathname-resolve-symbolic-links path)) path)
        path))

  (defvar *resolve-symlinks* t
    "Determine whether or not ASDF resolves symlinks when defining systems.
Defaults to T.")

  (defun resolve-symlinks* (path)
    "RESOLVE-SYMLINKS in PATH iff *RESOLVE-SYMLINKS* is T (the default)."
    (if *resolve-symlinks*
        (and path (resolve-symlinks path))
        path)))


;;; Check pathname constraints
(with-upgradability ()
  (defun ensure-pathname
      (pathname &key
                  on-error
                  defaults type dot-dot namestring
                  want-pathname
                  want-logical want-physical ensure-physical
                  want-relative want-absolute ensure-absolute ensure-subpath
                  want-non-wild want-wild wilden
                  want-file want-directory ensure-directory
                  want-existing ensure-directories-exist
                  truename resolve-symlinks truenamize
       &aux (p pathname)) ;; mutable working copy, preserve original
    "Coerces its argument into a PATHNAME,
optionally doing some transformations and checking specified constraints.

If the argument is NIL, then NIL is returned unless the WANT-PATHNAME constraint is specified.

If the argument is a STRING, it is first converted to a pathname via
PARSE-UNIX-NAMESTRING, PARSE-NAMESTRING or PARSE-NATIVE-NAMESTRING respectively
depending on the NAMESTRING argument being :UNIX, :LISP or :NATIVE respectively,
or else by using CALL-FUNCTION on the NAMESTRING argument;
if :UNIX is specified (or NIL, the default, which specifies the same thing),
then PARSE-UNIX-NAMESTRING it is called with the keywords
DEFAULTS TYPE DOT-DOT ENSURE-DIRECTORY WANT-RELATIVE, and
the result is optionally merged into the DEFAULTS if ENSURE-ABSOLUTE is true.

The pathname passed or resulting from parsing the string
is then subjected to all the checks and transformations below are run.

Each non-nil constraint argument can be one of the symbols T, ERROR, CERROR or IGNORE.
The boolean T is an alias for ERROR.
ERROR means that an error will be raised if the constraint is not satisfied.
CERROR means that an continuable error will be raised if the constraint is not satisfied.
IGNORE means just return NIL instead of the pathname.

The ON-ERROR argument, if not NIL, is a function designator (as per CALL-FUNCTION)
that will be called with the the following arguments:
a generic format string for ensure pathname, the pathname,
the keyword argument corresponding to the failed check or transformation,
a format string for the reason ENSURE-PATHNAME failed,
and a list with arguments to that format string.
If ON-ERROR is NIL, ERROR is used instead, which does the right thing.
You could also pass (CERROR \"CONTINUE DESPITE FAILED CHECK\").

The transformations and constraint checks are done in this order,
which is also the order in the lambda-list:

WANT-PATHNAME checks that pathname (after parsing if needed) is not null.
Otherwise, if the pathname is NIL, ensure-pathname returns NIL.
WANT-LOGICAL checks that pathname is a LOGICAL-PATHNAME
WANT-PHYSICAL checks that pathname is not a LOGICAL-PATHNAME
ENSURE-PHYSICAL ensures that pathname is physical via TRANSLATE-LOGICAL-PATHNAME
WANT-RELATIVE checks that pathname has a relative directory component
WANT-ABSOLUTE checks that pathname does have an absolute directory component
ENSURE-ABSOLUTE merges with the DEFAULTS, then checks again
that the result absolute is an absolute pathname indeed.
ENSURE-SUBPATH checks that the pathname is a subpath of the DEFAULTS.
WANT-FILE checks that pathname has a non-nil FILE component
WANT-DIRECTORY checks that pathname has nil FILE and TYPE components
ENSURE-DIRECTORY uses ENSURE-DIRECTORY-PATHNAME to interpret
any file and type components as being actually a last directory component.
WANT-NON-WILD checks that pathname is not a wild pathname
WANT-WILD checks that pathname is a wild pathname
WILDEN merges the pathname with **/*.*.* if it is not wild
WANT-EXISTING checks that a file (or directory) exists with that pathname.
ENSURE-DIRECTORIES-EXIST creates any parent directory with ENSURE-DIRECTORIES-EXIST.
TRUENAME replaces the pathname by its truename, or errors if not possible.
RESOLVE-SYMLINKS replaces the pathname by a variant with symlinks resolved by RESOLVE-SYMLINKS.
TRUENAMIZE uses TRUENAMIZE to resolve as many symlinks as possible."
    (block nil
      (flet ((report-error (keyword description &rest arguments)
               (call-function (or on-error 'error)
                              "Invalid pathname ~S: ~*~?"
                              pathname keyword description arguments)))
        (macrolet ((err (constraint &rest arguments)
                     `(report-error ',(intern* constraint :keyword) ,@arguments))
                   (check (constraint condition &rest arguments)
                     `(when ,constraint
                        (unless ,condition (err ,constraint ,@arguments))))
                   (transform (transform condition expr)
                     `(when ,transform
                        (,@(if condition `(when ,condition) '(progn))
                         (setf p ,expr)))))
          (etypecase p
            ((or null pathname))
            (string
             (setf p (case namestring
                       ((:unix nil)
                        (parse-unix-namestring
                         p :defaults defaults :type type :dot-dot dot-dot
                           :ensure-directory ensure-directory :want-relative want-relative))
                       ((:native)
                        (parse-native-namestring p))
                       ((:lisp)
                        (parse-namestring p))
                       (t
                        (call-function namestring p))))))
          (etypecase p
            (pathname)
            (null
             (check want-pathname (pathnamep p) "Expected a pathname, not NIL")
             (return nil)))
          (check want-logical (logical-pathname-p p) "Expected a logical pathname")
          (check want-physical (physical-pathname-p p) "Expected a physical pathname")
          (transform ensure-physical () (physicalize-pathname p))
          (check ensure-physical (physical-pathname-p p) "Could not translate to a physical pathname")
          (check want-relative (relative-pathname-p p) "Expected a relative pathname")
          (check want-absolute (absolute-pathname-p p) "Expected an absolute pathname")
          (transform ensure-absolute (not (absolute-pathname-p p))
                     (ensure-absolute-pathname p defaults (list #'report-error :ensure-absolute "~@?")))
          (check ensure-absolute (absolute-pathname-p p)
                 "Could not make into an absolute pathname even after merging with ~S" defaults)
          (check ensure-subpath (absolute-pathname-p defaults)
                 "cannot be checked to be a subpath of non-absolute pathname ~S" defaults)
          (check ensure-subpath (subpathp p defaults) "is not a sub pathname of ~S" defaults)
          (check want-file (file-pathname-p p) "Expected a file pathname")
          (check want-directory (directory-pathname-p p) "Expected a directory pathname")
          (transform ensure-directory (not (directory-pathname-p p)) (ensure-directory-pathname p))
          (check want-non-wild (not (wild-pathname-p p)) "Expected a non-wildcard pathname")
          (check want-wild (wild-pathname-p p) "Expected a wildcard pathname")
          (transform wilden (not (wild-pathname-p p)) (wilden p))
          (when want-existing
            (let ((existing (probe-file* p :truename truename)))
              (if existing
                  (when truename
                    (return existing))
                  (err want-existing "Expected an existing pathname"))))
          (when ensure-directories-exist (ensure-directories-exist p))
          (when truename
            (let ((truename (truename* p)))
              (if truename
                  (return truename)
                  (err truename "Can't get a truename for pathname"))))
          (transform resolve-symlinks () (resolve-symlinks p))
          (transform truenamize () (truenamize p))
          p)))))


;;; Pathname defaults
(with-upgradability ()
  (defun get-pathname-defaults (&optional (defaults *default-pathname-defaults*))
    "Find the actual DEFAULTS to use for pathnames, including
resolving them with respect to GETCWD if the DEFAULTS were relative"
    (or (absolute-pathname-p defaults)
        (merge-pathnames* defaults (getcwd))))

  (defun call-with-current-directory (dir thunk)
    "call the THUNK in a context where the current directory was changed to DIR, if not NIL.
Note that this operation is usually NOT thread-safe."
    (if dir
        (let* ((dir (resolve-symlinks* (get-pathname-defaults (pathname-directory-pathname dir))))
               (cwd (getcwd))
               (*default-pathname-defaults* dir))
          (chdir dir)
          (unwind-protect
               (funcall thunk)
            (chdir cwd)))
        (funcall thunk)))

  (defmacro with-current-directory ((&optional dir) &body body)
    "Call BODY while the POSIX current working directory is set to DIR"
    `(call-with-current-directory ,dir #'(lambda () ,@body))))


;;; Environment pathnames
(with-upgradability ()
  (defun inter-directory-separator ()
    "What character does the current OS conventionally uses to separate directories?"
    (if (os-unix-p) #\: #\;))

  (defun split-native-pathnames-string (string &rest constraints &key &allow-other-keys)
    "Given a string of pathnames specified in native OS syntax, separate them in a list,
check constraints and normalize each one as per ENSURE-PATHNAME."
    (loop :for namestring :in (split-string string :separator (string (inter-directory-separator)))
          :collect (apply 'parse-native-namestring namestring constraints)))

  (defun getenv-pathname (x &rest constraints &key ensure-directory want-directory on-error &allow-other-keys)
    "Extract a pathname from a user-configured environment variable, as per native OS,
check constraints and normalize as per ENSURE-PATHNAME."
    ;; For backward compatibility with ASDF 2, want-directory implies ensure-directory
    (apply 'parse-native-namestring (getenvp x)
           :ensure-directory (or ensure-directory want-directory)
           :on-error (or on-error
                         `(error "In (~S ~S), invalid pathname ~*~S: ~*~?" getenv-pathname ,x))
           constraints))
  (defun getenv-pathnames (x &rest constraints &key on-error &allow-other-keys)
    "Extract a list of pathname from a user-configured environment variable, as per native OS,
check constraints and normalize each one as per ENSURE-PATHNAME."
    (apply 'split-native-pathnames-string (getenvp x)
           :on-error (or on-error
                         `(error "In (~S ~S), invalid pathname ~*~S: ~*~?" getenv-pathnames ,x))
           constraints))
  (defun getenv-absolute-directory (x)
    "Extract an absolute directory pathname from a user-configured environment variable,
as per native OS"
    (getenv-pathname x :want-absolute t :ensure-directory t))
  (defun getenv-absolute-directories (x)
    "Extract a list of absolute directories from a user-configured environment variable,
as per native OS"
    (getenv-pathnames x :want-absolute t :ensure-directory t))

  (defun lisp-implementation-directory (&key truename)
    "Where are the system files of the current installation of the CL implementation?"
    (declare (ignorable truename))
    #+(or clozure ecl gcl mkcl sbcl)
    (let ((dir
            (ignore-errors
             #+clozure #p"ccl:"
             #+(or ecl mkcl) #p"SYS:"
             #+gcl system::*system-directory*
             #+sbcl (if-let (it (find-symbol* :sbcl-homedir-pathname :sb-int nil))
                      (funcall it)
                      (getenv-pathname "SBCL_HOME" :ensure-directory t)))))
      (if (and dir truename)
          (truename* dir)
          dir)))

  (defun lisp-implementation-pathname-p (pathname)
    "Is the PATHNAME under the current installation of the CL implementation?"
    ;; Other builtin systems are those under the implementation directory
    (and (when pathname
           (if-let (impdir (lisp-implementation-directory))
             (or (subpathp pathname impdir)
                 (when *resolve-symlinks*
                   (if-let (truename (truename* pathname))
                     (if-let (trueimpdir (truename* impdir))
                       (subpathp truename trueimpdir)))))))
         t)))


;;; Simple filesystem operations
(with-upgradability ()
  (defun ensure-all-directories-exist (pathnames)
    "Ensure that for every pathname in PATHNAMES, we ensure its directories exist"
    (dolist (pathname pathnames)
      (when pathname
        (ensure-directories-exist (physicalize-pathname pathname)))))

  (defun rename-file-overwriting-target (source target)
    "Rename a file, overwriting any previous file with the TARGET name,
in an atomic way if the implementation allows."
    #+clisp ;; in recent enough versions of CLISP, :if-exists :overwrite would make it atomic
    (progn (funcall 'require "syscalls")
           (symbol-call :posix :copy-file source target :method :rename))
    #-clisp
    (rename-file source target
                 #+(or clozure ecl) :if-exists #+clozure :rename-and-delete #+ecl t))

  (defun delete-file-if-exists (x)
    "Delete a file X if it already exists"
    (when x (handler-case (delete-file x) (file-error () nil))))

  (defun delete-empty-directory (directory-pathname)
    "Delete an empty directory"
    #+(or abcl digitool gcl) (delete-file directory-pathname)
    #+allegro (excl:delete-directory directory-pathname)
    #+clisp (ext:delete-directory directory-pathname)
    #+clozure (ccl::delete-empty-directory directory-pathname)
    #+(or cmu scl) (multiple-value-bind (ok errno)
                       (unix:unix-rmdir (native-namestring directory-pathname))
                     (unless ok
                       #+cmu (error "Error number ~A when trying to delete directory ~A"
                                    errno directory-pathname)
                       #+scl (error "~@<Error deleting ~S: ~A~@:>"
                                    directory-pathname (unix:get-unix-error-msg errno))))
    #+cormanlisp (win32:delete-directory directory-pathname)
    #+ecl (si:rmdir directory-pathname)
    #+lispworks (lw:delete-directory directory-pathname)
    #+mkcl (mkcl:rmdir directory-pathname)
    #+sbcl #.(if-let (dd (find-symbol* :delete-directory :sb-ext nil))
               `(,dd directory-pathname) ;; requires SBCL 1.0.44 or later
               `(progn (require :sb-posix) (symbol-call :sb-posix :rmdir directory-pathname)))
    #+xcl (symbol-call :uiop :run-program `("rmdir" ,(native-namestring directory-pathname)))
    #-(or abcl allegro clisp clozure cmu cormanlisp digitool ecl gcl lispworks mkcl sbcl scl xcl)
    (error "~S not implemented on ~S" 'delete-empty-directory (implementation-type))) ; genera

  (defun delete-directory-tree (directory-pathname &key (validate nil validatep) (if-does-not-exist :error))
    "Delete a directory including all its recursive contents, aka rm -rf.

To reduce the risk of infortunate mistakes, DIRECTORY-PATHNAME must be
a physical non-wildcard directory pathname (not namestring).

If the directory does not exist, the IF-DOES-NOT-EXIST argument specifies what happens:
if it is :ERROR (the default), an error is signaled, whereas if it is :IGNORE, nothing is done.

Furthermore, before any deletion is attempted, the DIRECTORY-PATHNAME must pass
the validation function designated (as per ENSURE-FUNCTION) by the VALIDATE keyword argument
which in practice is thus compulsory, and validates by returning a non-NIL result.
If you're suicidal or extremely confident, just use :VALIDATE T."
    (check-type if-does-not-exist (member :error :ignore))
    (cond
      ((not (and (pathnamep directory-pathname) (directory-pathname-p directory-pathname)
                 (physical-pathname-p directory-pathname) (not (wild-pathname-p directory-pathname))))
       (error "~S was asked to delete ~S but it is not a physical non-wildcard directory pathname"
              'delete-filesystem-tree directory-pathname))
      ((not validatep)
       (error "~S was asked to delete ~S but was not provided a validation predicate"
              'delete-filesystem-tree directory-pathname))
      ((not (call-function validate directory-pathname))
       (error "~S was asked to delete ~S but it is not valid ~@[according to ~S~]"
              'delete-filesystem-tree directory-pathname validate))
      ((not (directory-exists-p directory-pathname))
       (ecase if-does-not-exist
         (:error
          (error "~S was asked to delete ~S but the directory does not exist"
              'delete-filesystem-tree directory-pathname))
         (:ignore nil)))
      #-(or allegro cmu clozure sbcl scl)
      ((os-unix-p) ;; On Unix, don't recursively walk the directory and delete everything in Lisp,
       ;; except on implementations where we can prevent DIRECTORY from following symlinks;
       ;; instead spawn a standard external program to do the dirty work.
       (symbol-call :uiop :run-program `("rm" "-rf" ,(native-namestring directory-pathname))))
      (t
       ;; On supported implementation, call supported system functions
       #+allegro (symbol-call :excl.osi :delete-directory-and-files
                              directory-pathname :if-does-not-exist if-does-not-exist)
       #+clozure (ccl:delete-directory directory-pathname)
       #+genera (error "~S not implemented on ~S" 'delete-directory-tree (implementation-type))
       #+sbcl #.(if-let (dd (find-symbol* :delete-directory :sb-ext nil))
                  `(,dd directory-pathname :recursive t) ;; requires SBCL 1.0.44 or later
                  '(error "~S requires SBCL 1.0.44 or later" 'delete-directory-tree))
       ;; Outside Unix or on CMUCL and SCL that can avoid following symlinks,
       ;; do things the hard way.
       #-(or allegro clozure genera sbcl)
       (let ((sub*directories
               (while-collecting (c)
                 (collect-sub*directories directory-pathname t t #'c))))
             (dolist (d (nreverse sub*directories))
               (map () 'delete-file (directory-files d))
               (delete-empty-directory d)))))))

;;;; ---------------------------------------------------------------------------
;;;; Utilities related to streams

(uiop/package:define-package :uiop/stream
  (:nicknames :asdf/stream)
  (:recycle :uiop/stream :asdf/stream :asdf)
  (:use :uiop/common-lisp :uiop/package :uiop/utility :uiop/os :uiop/pathname :uiop/filesystem)
  (:export
   #:*default-stream-element-type*
   #:*stdin* #:setup-stdin #:*stdout* #:setup-stdout #:*stderr* #:setup-stderr
   #:detect-encoding #:*encoding-detection-hook* #:always-default-encoding
   #:encoding-external-format #:*encoding-external-format-hook* #:default-encoding-external-format
   #:*default-encoding* #:*utf-8-external-format*
   #:with-safe-io-syntax #:call-with-safe-io-syntax #:safe-read-from-string
   #:with-output #:output-string #:with-input
   #:with-input-file #:call-with-input-file #:with-output-file #:call-with-output-file
   #:null-device-pathname #:call-with-null-input #:with-null-input
   #:call-with-null-output #:with-null-output
   #:finish-outputs #:format! #:safe-format!
   #:copy-stream-to-stream #:concatenate-files #:copy-file
   #:slurp-stream-string #:slurp-stream-lines #:slurp-stream-line
   #:slurp-stream-forms #:slurp-stream-form
   #:read-file-string #:read-file-line #:read-file-lines #:safe-read-file-line
   #:read-file-forms #:read-file-form #:safe-read-file-form
   #:eval-input #:eval-thunk #:standard-eval-thunk
   #:println #:writeln
   ;; Temporary files
   #:*temporary-directory* #:temporary-directory #:default-temporary-directory
   #:setup-temporary-directory
   #:call-with-temporary-file #:with-temporary-file
   #:add-pathname-suffix #:tmpize-pathname
   #:call-with-staging-pathname #:with-staging-pathname))
(in-package :uiop/stream)

(with-upgradability ()
  (defvar *default-stream-element-type*
    (or #+(or abcl cmu cormanlisp scl xcl) 'character
        #+lispworks 'lw:simple-char
        :default)
    "default element-type for open (depends on the current CL implementation)")

  (defvar *stdin* *standard-input*
    "the original standard input stream at startup")

  (defun setup-stdin ()
    (setf *stdin*
          #.(or #+clozure 'ccl::*stdin*
                #+(or cmu scl) 'system:*stdin*
                #+ecl 'ext::+process-standard-input+
                #+sbcl 'sb-sys:*stdin*
                '*standard-input*)))

  (defvar *stdout* *standard-output*
    "the original standard output stream at startup")

  (defun setup-stdout ()
    (setf *stdout*
          #.(or #+clozure 'ccl::*stdout*
                #+(or cmu scl) 'system:*stdout*
                #+ecl 'ext::+process-standard-output+
                #+sbcl 'sb-sys:*stdout*
                '*standard-output*)))

  (defvar *stderr* *error-output*
    "the original error output stream at startup")

  (defun setup-stderr ()
    (setf *stderr*
          #.(or #+allegro 'excl::*stderr*
                #+clozure 'ccl::*stderr*
                #+(or cmu scl) 'system:*stderr*
                #+ecl 'ext::+process-error-output+
                #+sbcl 'sb-sys:*stderr*
                '*error-output*)))

  ;; Run them now. In image.lisp, we'll register them to be run at image restart.
  (setup-stdin) (setup-stdout) (setup-stderr))


;;; Encodings (mostly hooks only; full support requires asdf-encodings)
(with-upgradability ()
  (defparameter *default-encoding*
    ;; preserve explicit user changes to something other than the legacy default :default
    (or (if-let (previous (and (boundp '*default-encoding*) (symbol-value '*default-encoding*)))
          (unless (eq previous :default) previous))
        :utf-8)
    "Default encoding for source files.
The default value :utf-8 is the portable thing.
The legacy behavior was :default.
If you (asdf:load-system :asdf-encodings) then
you will have autodetection via *encoding-detection-hook* below,
reading emacs-style -*- coding: utf-8 -*- specifications,
and falling back to utf-8 or latin1 if nothing is specified.")

  (defparameter *utf-8-external-format*
    (if (featurep :asdf-unicode)
        (or #+clisp charset:utf-8 :utf-8)
        :default)
    "Default :external-format argument to pass to CL:OPEN and also
CL:LOAD or CL:COMPILE-FILE to best process a UTF-8 encoded file.
On modern implementations, this will decode UTF-8 code points as CL characters.
On legacy implementations, it may fall back on some 8-bit encoding,
with non-ASCII code points being read as several CL characters;
hopefully, if done consistently, that won't affect program behavior too much.")

  (defun always-default-encoding (pathname)
    "Trivial function to use as *encoding-detection-hook*,
always 'detects' the *default-encoding*"
    (declare (ignore pathname))
    *default-encoding*)

  (defvar *encoding-detection-hook* #'always-default-encoding
    "Hook for an extension to define a function to automatically detect a file's encoding")

  (defun detect-encoding (pathname)
    "Detects the encoding of a specified file, going through user-configurable hooks"
    (if (and pathname (not (directory-pathname-p pathname)) (probe-file* pathname))
        (funcall *encoding-detection-hook* pathname)
        *default-encoding*))

  (defun default-encoding-external-format (encoding)
    "Default, ignorant, function to transform a character ENCODING as a
portable keyword to an implementation-dependent EXTERNAL-FORMAT specification.
Load system ASDF-ENCODINGS to hook in a better one."
    (case encoding
      (:default :default) ;; for backward-compatibility only. Explicit usage discouraged.
      (:utf-8 *utf-8-external-format*)
      (otherwise
       (cerror "Continue using :external-format :default" (compatfmt "~@<Your ASDF component is using encoding ~S but it isn't recognized. Your system should :defsystem-depends-on (:asdf-encodings).~:>") encoding)
       :default)))

  (defvar *encoding-external-format-hook*
    #'default-encoding-external-format
    "Hook for an extension (e.g. ASDF-ENCODINGS) to define a better mapping
from non-default encodings to and implementation-defined external-format's")

  (defun encoding-external-format (encoding)
    "Transform a portable ENCODING keyword to an implementation-dependent EXTERNAL-FORMAT,
going through all the proper hooks."
    (funcall *encoding-external-format-hook* (or encoding *default-encoding*))))


;;; Safe syntax
(with-upgradability ()
  (defvar *standard-readtable* (with-standard-io-syntax *readtable*)
    "The standard readtable, implementing the syntax specified by the CLHS.
It must never be modified, though only good implementations will even enforce that.")

  (defmacro with-safe-io-syntax ((&key (package :cl)) &body body)
    "Establish safe CL reader options around the evaluation of BODY"
    `(call-with-safe-io-syntax #'(lambda () (let ((*package* (find-package ,package))) ,@body))))

  (defun call-with-safe-io-syntax (thunk &key (package :cl))
    (with-standard-io-syntax
      (let ((*package* (find-package package))
            (*read-default-float-format* 'double-float)
            (*print-readably* nil)
            (*read-eval* nil))
        (funcall thunk))))

  (defun safe-read-from-string (string &key (package :cl) (eof-error-p t) eof-value (start 0) end preserve-whitespace)
    "Read from STRING using a safe syntax, as per WITH-SAFE-IO-SYNTAX"
    (with-safe-io-syntax (:package package)
      (read-from-string string eof-error-p eof-value :start start :end end :preserve-whitespace preserve-whitespace))))

;;; Output helpers
(with-upgradability ()
  (defun call-with-output-file (pathname thunk
                                &key
                                  (element-type *default-stream-element-type*)
                                  (external-format *utf-8-external-format*)
                                  (if-exists :error)
                                  (if-does-not-exist :create))
    "Open FILE for input with given recognizes options, call THUNK with the resulting stream.
Other keys are accepted but discarded."
    (with-open-file (s pathname :direction :output
                                :element-type element-type
                                :external-format external-format
                                :if-exists if-exists
                                :if-does-not-exist if-does-not-exist)
      (funcall thunk s)))

  (defmacro with-output-file ((var pathname &rest keys
                               &key element-type external-format if-exists if-does-not-exist)
                              &body body)
    (declare (ignore element-type external-format if-exists if-does-not-exist))
    `(call-with-output-file ,pathname #'(lambda (,var) ,@body) ,@keys))

  (defun call-with-output (output function &key keys)
    "Calls FUNCTION with an actual stream argument,
behaving like FORMAT with respect to how stream designators are interpreted:
If OUTPUT is a STREAM, use it as the stream.
If OUTPUT is NIL, use a STRING-OUTPUT-STREAM as the stream, and return the resulting string.
If OUTPUT is T, use *STANDARD-OUTPUT* as the stream.
If OUTPUT is a STRING with a fill-pointer, use it as a string-output-stream.
If OUTPUT is a PATHNAME, open the file and write to it, passing KEYS to WITH-OUTPUT-FILE
-- this latter as an extension since ASDF 3.1.
Otherwise, signal an error."
    (etypecase output
      (null
       (with-output-to-string (stream) (funcall function stream)))
      ((eql t)
       (funcall function *standard-output*))
      (stream
       (funcall function output))
      (string
       (assert (fill-pointer output))
       (with-output-to-string (stream output) (funcall function stream)))
      (pathname
       (apply 'call-with-output-file output function keys))))

  (defmacro with-output ((output-var &optional (value output-var)) &body body)
    "Bind OUTPUT-VAR to an output stream, coercing VALUE (default: previous binding of OUTPUT-VAR)
as per FORMAT, and evaluate BODY within the scope of this binding."
    `(call-with-output ,value #'(lambda (,output-var) ,@body)))

  (defun output-string (string &optional output)
    "If the desired OUTPUT is not NIL, print the string to the output; otherwise return the string"
    (if output
        (with-output (output) (princ string output))
        string)))


;;; Input helpers
(with-upgradability ()
  (defun call-with-input-file (pathname thunk
                               &key
                                 (element-type *default-stream-element-type*)
                                 (external-format *utf-8-external-format*)
                                 (if-does-not-exist :error))
    "Open FILE for input with given recognizes options, call THUNK with the resulting stream.
Other keys are accepted but discarded."
    (with-open-file (s pathname :direction :input
                                :element-type element-type
                                :external-format external-format
                                :if-does-not-exist if-does-not-exist)
      (funcall thunk s)))

  (defmacro with-input-file ((var pathname &rest keys
                              &key element-type external-format if-does-not-exist)
                             &body body)
    (declare (ignore element-type external-format if-does-not-exist))
    `(call-with-input-file ,pathname #'(lambda (,var) ,@body) ,@keys))

  (defun call-with-input (input function &key keys)
    "Calls FUNCTION with an actual stream argument, interpreting
stream designators like READ, but also coercing strings to STRING-INPUT-STREAM,
and PATHNAME to FILE-STREAM.
If INPUT is a STREAM, use it as the stream.
If INPUT is NIL, use a *STANDARD-INPUT* as the stream.
If INPUT is T, use *TERMINAL-IO* as the stream.
If INPUT is a STRING, use it as a string-input-stream.
If INPUT is a PATHNAME, open it, passing KEYS to WITH-INPUT-FILE
-- the latter is an extension since ASDF 3.1.
Otherwise, signal an error."
    (etypecase input
      (null (funcall function *standard-input*))
      ((eql t) (funcall function *terminal-io*))
      (stream (funcall function input))
      (string (with-input-from-string (stream input) (funcall function stream)))
      (pathname (apply 'call-with-input-file input function keys))))

  (defmacro with-input ((input-var &optional (value input-var)) &body body)
    "Bind INPUT-VAR to an input stream, coercing VALUE (default: previous binding of INPUT-VAR)
as per CALL-WITH-INPUT, and evaluate BODY within the scope of this binding."
    `(call-with-input ,value #'(lambda (,input-var) ,@body))))


;;; Null device
(with-upgradability ()
  (defun null-device-pathname ()
    "Pathname to a bit bucket device that discards any information written to it
and always returns EOF when read from"
    (cond
      ((os-unix-p) #p"/dev/null")
      ((os-windows-p) #p"NUL") ;; Q: how many Lisps accept the #p"NUL:" syntax?
      (t (error "No /dev/null on your OS"))))
  (defun call-with-null-input (fun &rest keys &key element-type external-format if-does-not-exist)
    "Call FUN with an input stream from the null device; pass keyword arguments to OPEN."
    (declare (ignore element-type external-format if-does-not-exist))
    (apply 'call-with-input-file (null-device-pathname) fun keys))
  (defmacro with-null-input ((var &rest keys
                              &key element-type external-format if-does-not-exist)
                             &body body)
    (declare (ignore element-type external-format if-does-not-exist))
    "Evaluate BODY in a context when VAR is bound to an input stream accessing the null device.
Pass keyword arguments to OPEN."
    `(call-with-null-input #'(lambda (,var) ,@body) ,@keys))
  (defun call-with-null-output (fun
                                &key (element-type *default-stream-element-type*)
                                  (external-format *utf-8-external-format*)
                                  (if-exists :overwrite)
                                  (if-does-not-exist :error))
    "Call FUN with an output stream to the null device; pass keyword arguments to OPEN."
    (call-with-output-file
     (null-device-pathname) fun
     :element-type element-type :external-format external-format
     :if-exists if-exists :if-does-not-exist if-does-not-exist))
  (defmacro with-null-output ((var &rest keys
                              &key element-type external-format if-does-not-exist if-exists)
                              &body body)
    "Evaluate BODY in a context when VAR is bound to an output stream accessing the null device.
Pass keyword arguments to OPEN."
    (declare (ignore element-type external-format if-exists if-does-not-exist))
    `(call-with-null-output #'(lambda (,var) ,@body) ,@keys)))

;;; Ensure output buffers are flushed
(with-upgradability ()
  (defun finish-outputs (&rest streams)
    "Finish output on the main output streams as well as any specified one.
Useful for portably flushing I/O before user input or program exit."
    ;; CCL notably buffers its stream output by default.
    (dolist (s (append streams
                       (list *stdout* *stderr* *error-output* *standard-output* *trace-output*
                             *debug-io* *terminal-io* *query-io*)))
      (ignore-errors (finish-output s)))
    (values))

  (defun format! (stream format &rest args)
    "Just like format, but call finish-outputs before and after the output."
    (finish-outputs stream)
    (apply 'format stream format args)
    (finish-outputs stream))

  (defun safe-format! (stream format &rest args)
    "Variant of FORMAT that is safe against both
dangerous syntax configuration and errors while printing."
    (with-safe-io-syntax ()
      (ignore-errors (apply 'format! stream format args))
      (finish-outputs stream)))) ; just in case format failed


;;; Simple Whole-Stream processing
(with-upgradability ()
  (defun copy-stream-to-stream (input output &key element-type buffer-size linewise prefix)
    "Copy the contents of the INPUT stream into the OUTPUT stream.
If LINEWISE is true, then read and copy the stream line by line, with an optional PREFIX.
Otherwise, using WRITE-SEQUENCE using a buffer of size BUFFER-SIZE."
    (with-open-stream (input input)
      (if linewise
          (loop* :for (line eof) = (multiple-value-list (read-line input nil nil))
                 :while line :do
                 (when prefix (princ prefix output))
                 (princ line output)
                 (unless eof (terpri output))
                 (finish-output output)
                 (when eof (return)))
          (loop
            :with buffer-size = (or buffer-size 8192)
            :for buffer = (make-array (list buffer-size) :element-type (or element-type 'character))
            :for end = (read-sequence buffer input)
            :until (zerop end)
            :do (write-sequence buffer output :end end)
                (when (< end buffer-size) (return))))))

  (defun concatenate-files (inputs output)
    "create a new OUTPUT file the contents of which a the concatenate of the INPUTS files."
    (with-open-file (o output :element-type '(unsigned-byte 8)
                              :direction :output :if-exists :rename-and-delete)
      (dolist (input inputs)
        (with-open-file (i input :element-type '(unsigned-byte 8)
                                 :direction :input :if-does-not-exist :error)
          (copy-stream-to-stream i o :element-type '(unsigned-byte 8))))))

  (defun copy-file (input output)
    "Copy contents of the INPUT file to the OUTPUT file"
    ;; Not available on LW personal edition or LW 6.0 on Mac: (lispworks:copy-file i f)
    (concatenate-files (list input) output))

  (defun slurp-stream-string (input &key (element-type 'character) stripped)
    "Read the contents of the INPUT stream as a string"
    (let ((string
            (with-open-stream (input input)
              (with-output-to-string (output)
                (copy-stream-to-stream input output :element-type element-type)))))
      (if stripped (stripln string) string)))

  (defun slurp-stream-lines (input &key count)
    "Read the contents of the INPUT stream as a list of lines, return those lines.

Note: relies on the Lisp's READ-LINE, but additionally removes any remaining CR
from the line-ending if the file or stream had CR+LF but Lisp only removed LF.

Read no more than COUNT lines."
    (check-type count (or null integer))
    (with-open-stream (input input)
      (loop :for n :from 0
            :for l = (and (or (not count) (< n count))
                          (read-line input nil nil))
            ;; stripln: to remove CR when the OS sends CRLF and Lisp only remove LF
            :while l :collect (stripln l))))

  (defun slurp-stream-line (input &key (at 0))
    "Read the contents of the INPUT stream as a list of lines,
then return the ACCESS-AT of that list of lines using the AT specifier.
PATH defaults to 0, i.e. return the first line.
PATH is typically an integer, or a list of an integer and a function.
If PATH is NIL, it will return all the lines in the file.

The stream will not be read beyond the Nth lines,
where N is the index specified by path
if path is either an integer or a list that starts with an integer."
    (access-at (slurp-stream-lines input :count (access-at-count at)) at))

  (defun slurp-stream-forms (input &key count)
    "Read the contents of the INPUT stream as a list of forms,
and return those forms.

If COUNT is null, read to the end of the stream;
if COUNT is an integer, stop after COUNT forms were read.

BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
    (check-type count (or null integer))
    (loop :with eof = '#:eof
          :for n :from 0
          :for form = (if (and count (>= n count))
                          eof
                          (read-preserving-whitespace input nil eof))
          :until (eq form eof) :collect form))

  (defun slurp-stream-form (input &key (at 0))
    "Read the contents of the INPUT stream as a list of forms,
then return the ACCESS-AT of these forms following the AT.
AT defaults to 0, i.e. return the first form.
AT is typically a list of integers.
If AT is NIL, it will return all the forms in the file.

The stream will not be read beyond the Nth form,
where N is the index specified by path,
if path is either an integer or a list that starts with an integer.

BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
    (access-at (slurp-stream-forms input :count (access-at-count at)) at))

  (defun read-file-string (file &rest keys)
    "Open FILE with option KEYS, read its contents as a string"
    (apply 'call-with-input-file file 'slurp-stream-string keys))

  (defun read-file-lines (file &rest keys)
    "Open FILE with option KEYS, read its contents as a list of lines
BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
    (apply 'call-with-input-file file 'slurp-stream-lines keys))

  (defun read-file-line (file &rest keys &key (at 0) &allow-other-keys)
    "Open input FILE with option KEYS (except AT),
and read its contents as per SLURP-STREAM-LINE with given AT specifier.
BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
    (apply 'call-with-input-file file
           #'(lambda (input) (slurp-stream-line input :at at))
           (remove-plist-key :at keys)))

  (defun read-file-forms (file &rest keys &key count &allow-other-keys)
    "Open input FILE with option KEYS (except COUNT),
and read its contents as per SLURP-STREAM-FORMS with given COUNT.
BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
    (apply 'call-with-input-file file
           #'(lambda (input) (slurp-stream-forms input :count count))
           (remove-plist-key :count keys)))

  (defun read-file-form (file &rest keys &key (at 0) &allow-other-keys)
    "Open input FILE with option KEYS (except AT),
and read its contents as per SLURP-STREAM-FORM with given AT specifier.
BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
    (apply 'call-with-input-file file
           #'(lambda (input) (slurp-stream-form input :at at))
           (remove-plist-key :at keys)))

  (defun safe-read-file-line (pathname &rest keys &key (package :cl) &allow-other-keys)
    "Reads the specified line from the top of a file using a safe standardized syntax.
Extracts the line using READ-FILE-LINE,
within an WITH-SAFE-IO-SYNTAX using the specified PACKAGE."
    (with-safe-io-syntax (:package package)
      (apply 'read-file-line pathname (remove-plist-key :package keys))))

  (defun safe-read-file-form (pathname &rest keys &key (package :cl) &allow-other-keys)
    "Reads the specified form from the top of a file using a safe standardized syntax.
Extracts the form using READ-FILE-FORM,
within an WITH-SAFE-IO-SYNTAX using the specified PACKAGE."
    (with-safe-io-syntax (:package package)
      (apply 'read-file-form pathname (remove-plist-key :package keys))))

  (defun eval-input (input)
    "Portably read and evaluate forms from INPUT, return the last values."
    (with-input (input)
      (loop :with results :with eof ='#:eof
            :for form = (read input nil eof)
            :until (eq form eof)
            :do (setf results (multiple-value-list (eval form)))
            :finally (return (apply 'values results)))))

  (defun eval-thunk (thunk)
    "Evaluate a THUNK of code:
If a function, FUNCALL it without arguments.
If a constant literal and not a sequence, return it.
If a cons or a symbol, EVAL it.
If a string, repeatedly read and evaluate from it, returning the last values."
    (etypecase thunk
      ((or boolean keyword number character pathname) thunk)
      ((or cons symbol) (eval thunk))
      (function (funcall thunk))
      (string (eval-input thunk))))

  (defun standard-eval-thunk (thunk &key (package :cl))
    "Like EVAL-THUNK, but in a more standardized evaluation context."
    ;; Note: it's "standard-" not "safe-", because evaluation is never safe.
    (when thunk
      (with-safe-io-syntax (:package package)
        (let ((*read-eval* t))
          (eval-thunk thunk))))))

(with-upgradability ()
  (defun println (x &optional (stream *standard-output*))
    "Variant of PRINC that also calls TERPRI afterwards"
    (princ x stream) (terpri stream) (finish-output stream) (values))

  (defun writeln (x &rest keys &key (stream *standard-output*) &allow-other-keys)
    "Variant of WRITE that also calls TERPRI afterwards"
    (apply 'write x keys) (terpri stream) (finish-output stream) (values)))


;;; Using temporary files
(with-upgradability ()
  (defun default-temporary-directory ()
    "Return a default directory to use for temporary files"
    (or
     (when (os-unix-p)
       (or (getenv-pathname "TMPDIR" :ensure-directory t)
           (parse-native-namestring "/tmp/")))
     (when (os-windows-p)
       (getenv-pathname "TEMP" :ensure-directory t))
     (subpathname (user-homedir-pathname) "tmp/")))

  (defvar *temporary-directory* nil "User-configurable location for temporary files")

  (defun temporary-directory ()
    "Return a directory to use for temporary files"
    (or *temporary-directory* (default-temporary-directory)))

  (defun setup-temporary-directory ()
    "Configure a default temporary directory to use."
    (setf *temporary-directory* (default-temporary-directory))
    #+gcl (setf system::*tmp-dir* *temporary-directory*))

  (defun call-with-temporary-file
      (thunk &key
               (want-stream-p t) (want-pathname-p t) (direction :io) keep after
               directory (type "tmp" typep) prefix (suffix (when typep "-tmp"))
               (element-type *default-stream-element-type*)
               (external-format *utf-8-external-format*))
    "Call a THUNK with stream and/or pathname arguments identifying a temporary file.

The temporary file's pathname will be based on concatenating
PREFIX (defaults to \"uiop\"), a random alphanumeric string,
and optional SUFFIX (defaults to \"-tmp\" if a type was provided)
and TYPE (defaults to \"tmp\", using a dot as separator if not NIL),
within DIRECTORY (defaulting to the TEMPORARY-DIRECTORY) if the PREFIX isn't absolute.

The file will be open with specified DIRECTION (defaults to :IO),
ELEMENT-TYPE (defaults to *DEFAULT-STREAM-ELEMENT-TYPE*) and
EXTERNAL-FORMAT (defaults to *UTF-8-EXTERNAL-FORMAT*).
If WANT-STREAM-P is true (the defaults to T), then THUNK will then be CALL-FUNCTION'ed
with the stream and the pathname (if WANT-PATHNAME-P is true, defaults to T),
and stream with be closed after the THUNK exits (either normally or abnormally).
If WANT-STREAM-P is false, then WANT-PATHAME-P must be true, and then
THUNK is only CALL-FUNCTION'ed after the stream is closed, with the pathname as argument.
Upon exit of THUNK, the AFTER thunk if defined is CALL-FUNCTION'ed with the pathname as argument.
If AFTER is defined, its results are returned, otherwise, the results of THUNK are returned.
Finally, the file will be deleted, unless the KEEP argument when CALL-FUNCTION'ed returns true."
    #+xcl (declare (ignorable typep))
    (check-type direction (member :output :io))
    (assert (or want-stream-p want-pathname-p))
    (loop
      :with prefix = (native-namestring
                      (ensure-absolute-pathname
                       (or prefix "tmp")
                       (or (ensure-pathname directory :namestring :native :ensure-directory t)
                           #'temporary-directory)))
      :with results = ()
      :for counter :from (random (expt 36 #-gcl 8 #+gcl 5))
      :for pathname = (parse-native-namestring
                       (format nil "~A~36R~@[~A~]~@[.~A~]" prefix counter suffix type))
      :for okp = nil :do
        ;; TODO: on Unix, do something about umask
        ;; TODO: on Unix, audit the code so we make sure it uses O_CREAT|O_EXCL
        ;; TODO: on Unix, use CFFI and mkstemp --
        ;; except UIOP is precisely meant to not depend on CFFI or on anything! Grrrr.
        ;; Can we at least design some hook?
        (unwind-protect
             (progn
               (with-open-file (stream pathname
                                       :direction direction
                                       :element-type element-type
                                       :external-format external-format
                                       :if-exists nil :if-does-not-exist :create)
                 (when stream
                   (setf okp pathname)
                   (when want-stream-p
                     (setf results
                           (multiple-value-list
                            (if want-pathname-p
                                (funcall thunk stream pathname)
                                (funcall thunk stream)))))))
               (when okp
                 (unless want-stream-p
                   (setf results (multiple-value-list (call-function thunk pathname))))
                 (when after
                   (setf results (multiple-value-list (call-function after pathname))))
                 (return (apply 'values results))))
          (when (and okp (not (call-function keep)))
            (ignore-errors (delete-file-if-exists okp))))))

  (defmacro with-temporary-file ((&key (stream (gensym "STREAM") streamp)
                                    (pathname (gensym "PATHNAME") pathnamep)
                                    directory prefix suffix type
                                    keep direction element-type external-format)
                                 &body body)
    "Evaluate BODY where the symbols specified by keyword arguments
STREAM and PATHNAME (if respectively specified) are bound corresponding
to a newly created temporary file ready for I/O, as per CALL-WITH-TEMPORARY-FILE.
At least one of STREAM or PATHNAME must be specified.
If the STREAM is not specified, it will be closed before the BODY is evaluated.
If STREAM is specified, then the :CLOSE-STREAM label if it appears in the BODY,
separates forms run before and after the stream is closed.
The values of the last form of the BODY (not counting the separating :CLOSE-STREAM) are returned.
Upon success, the KEEP form is evaluated and the file is is deleted unless it evaluates to TRUE."
    (check-type stream symbol)
    (check-type pathname symbol)
    (assert (or streamp pathnamep))
    (let* ((afterp (position :close-stream body))
           (before (if afterp (subseq body 0 afterp) body))
           (after (when afterp (subseq body (1+ afterp))))
           (beforef (gensym "BEFORE"))
           (afterf (gensym "AFTER")))
      `(flet (,@(when before
                  `((,beforef (,@(when streamp `(,stream)) ,@(when pathnamep `(,pathname))) ,@before)))
              ,@(when after
                  (assert pathnamep)
                  `((,afterf (,pathname) ,@after))))
         #-gcl (declare (dynamic-extent ,@(when before `(#',beforef)) ,@(when after `(#',afterf))))
         (call-with-temporary-file
          ,(when before `#',beforef)
          :want-stream-p ,streamp
          :want-pathname-p ,pathnamep
          ,@(when direction `(:direction ,direction))
          ,@(when directory `(:directory ,directory))
          ,@(when prefix `(:prefix ,prefix))
          ,@(when suffix `(:suffix ,suffix))
          ,@(when type `(:type ,type))
          ,@(when keep `(:keep ,keep))
          ,@(when after `(:after #',afterf))
          ,@(when element-type `(:element-type ,element-type))
          ,@(when external-format `(:external-format ,external-format))))))

  (defun get-temporary-file (&key directory prefix suffix type)
    (with-temporary-file (:pathname pn :keep t
                          :directory directory :prefix prefix :suffix suffix :type type)
      pn))

  ;; Temporary pathnames in simple cases where no contention is assumed
  (defun add-pathname-suffix (pathname suffix &rest keys)
    "Add a SUFFIX to the name of a PATHNAME, return a new pathname.
Further KEYS can be passed to MAKE-PATHNAME."
    (apply 'make-pathname :name (strcat (pathname-name pathname) suffix)
                          :defaults pathname keys))

  (defun tmpize-pathname (x)
    "Return a new pathname modified from X by adding a trivial deterministic suffix"
    (add-pathname-suffix x "-TMP"))

  (defun call-with-staging-pathname (pathname fun)
    "Calls FUN with a staging pathname, and atomically
renames the staging pathname to the PATHNAME in the end.
NB: this protects only against failure of the program, not against concurrent attempts.
For the latter case, we ought pick a random suffix and atomically open it."
    (let* ((pathname (pathname pathname))
           (staging (tmpize-pathname pathname)))
      (unwind-protect
           (multiple-value-prog1
               (funcall fun staging)
             (rename-file-overwriting-target staging pathname))
        (delete-file-if-exists staging))))

  (defmacro with-staging-pathname ((pathname-var &optional (pathname-value pathname-var)) &body body)
    "Trivial syntax wrapper for CALL-WITH-STAGING-PATHNAME"
    `(call-with-staging-pathname ,pathname-value #'(lambda (,pathname-var) ,@body))))

;;;; -------------------------------------------------------------------------
;;;; Starting, Stopping, Dumping a Lisp image

(uiop/package:define-package :uiop/image
  (:nicknames :asdf/image)
  (:recycle :uiop/image :asdf/image :xcvb-driver)
  (:use :uiop/common-lisp :uiop/package :uiop/utility :uiop/pathname :uiop/stream :uiop/os)
  (:export
   #:*image-dumped-p* #:raw-command-line-arguments #:*command-line-arguments*
   #:command-line-arguments #:raw-command-line-arguments #:setup-command-line-arguments #:argv0
   #:*lisp-interaction*
   #:*fatal-conditions* #:fatal-condition-p #:handle-fatal-condition
   #:call-with-fatal-condition-handler #:with-fatal-condition-handler
   #:*image-restore-hook* #:*image-prelude* #:*image-entry-point*
   #:*image-postlude* #:*image-dump-hook*
   #:quit #:die #:raw-print-backtrace #:print-backtrace #:print-condition-backtrace
   #:shell-boolean-exit
   #:register-image-restore-hook #:register-image-dump-hook
   #:call-image-restore-hook #:call-image-dump-hook
   #:restore-image #:dump-image #:create-image
))
(in-package :uiop/image)

(with-upgradability ()
  (defvar *lisp-interaction* t
    "Is this an interactive Lisp environment, or is it batch processing?")

  (defvar *command-line-arguments* nil
    "Command-line arguments")

  (defvar *image-dumped-p* nil ; may matter as to how to get to command-line-arguments
    "Is this a dumped image? As a standalone executable?")

  (defvar *image-restore-hook* nil
    "Functions to call (in reverse order) when the image is restored")

  (defvar *image-restored-p* nil
    "Has the image been restored? A boolean, or :in-progress while restoring, :in-regress while dumping")

  (defvar *image-prelude* nil
    "a form to evaluate, or string containing forms to read and evaluate
when the image is restarted, but before the entry point is called.")

  (defvar *image-entry-point* nil
    "a function with which to restart the dumped image when execution is restored from it.")

  (defvar *image-postlude* nil
    "a form to evaluate, or string containing forms to read and evaluate
before the image dump hooks are called and before the image is dumped.")

  (defvar *image-dump-hook* nil
    "Functions to call (in order) when before an image is dumped")

  (defvar *fatal-conditions* '(error)
    "conditions that cause the Lisp image to enter the debugger if interactive,
or to die if not interactive"))


;;; Exiting properly or im-
(with-upgradability ()
  (defun quit (&optional (code 0) (finish-output t))
    "Quits from the Lisp world, with the given exit status if provided.
This is designed to abstract away the implementation specific quit forms."
    (when finish-output ;; essential, for ClozureCL, and for standard compliance.
      (finish-outputs))
    #+(or abcl xcl) (ext:quit :status code)
    #+allegro (excl:exit code :quiet t)
    #+clisp (ext:quit code)
    #+clozure (ccl:quit code)
    #+cormanlisp (win32:exitprocess code)
    #+(or cmu scl) (unix:unix-exit code)
    #+ecl (si:quit code)
    #+gcl (system:quit code)
    #+genera (error "You probably don't want to Halt the Machine. (code: ~S)" code)
    #+lispworks (lispworks:quit :status code :confirm nil :return nil :ignore-errors-p t)
    #+mcl (progn code (ccl:quit)) ;; or should we use FFI to call libc's exit(3) ?
    #+mkcl (mk-ext:quit :exit-code code)
    #+sbcl #.(let ((exit (find-symbol* :exit :sb-ext nil))
                   (quit (find-symbol* :quit :sb-ext nil)))
               (cond
                 (exit `(,exit :code code :abort (not finish-output)))
                 (quit `(,quit :unix-status code :recklessly-p (not finish-output)))))
    #-(or abcl allegro clisp clozure cmu ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
    (error "~S called with exit code ~S but there's no quitting on this implementation" 'quit code))

  (defun die (code format &rest arguments)
    "Die in error with some error message"
    (with-safe-io-syntax ()
      (ignore-errors
       (format! *stderr* "~&~?~&" format arguments)))
    (quit code))

  (defun raw-print-backtrace (&key (stream *debug-io*) count condition)
    "Print a backtrace, directly accessing the implementation"
    (declare (ignorable stream count condition))
    #+abcl
    (loop :for i :from 0
	  :for frame :in (sys:backtrace (or count most-positive-fixnum)) :do
	    (safe-format! stream "~&~D: ~A~%" i frame))
    #+allegro
    (let ((*terminal-io* stream)
          (*standard-output* stream)
          (tpl:*zoom-print-circle* *print-circle*)
          (tpl:*zoom-print-level* *print-level*)
          (tpl:*zoom-print-length* *print-length*))
      (tpl:do-command "zoom"
        :from-read-eval-print-loop nil
        :count (or count t)
        :all t))
    #+clisp
    (system::print-backtrace :out stream :limit count)
    #+(or clozure mcl)
    (let ((*debug-io* stream))
      #+clozure (ccl:print-call-history :count count :start-frame-number 1)
      #+mcl (ccl:print-call-history :detailed-p nil)
      (finish-output stream))
    #+(or cmu scl)
    (let ((debug:*debug-print-level* *print-level*)
          (debug:*debug-print-length* *print-length*))
      (debug:backtrace (or count most-positive-fixnum) stream))
    #+(or ecl mkcl)
    (let* ((top (si:ihs-top))
	   (repeats (if count (min top count) top))
	   (backtrace (loop :for ihs :from 0 :below top
                            :collect (list (si::ihs-fun ihs)
                                           (si::ihs-env ihs)))))
      (loop :for i :from 0 :below repeats
	    :for frame :in (nreverse backtrace) :do
	      (safe-format! stream "~&~D: ~S~%" i frame)))
    #+gcl
    (let ((*debug-io* stream))
      (ignore-errors
       (with-safe-io-syntax ()
	 (if condition
	     (conditions::condition-backtrace condition)
	     (system::simple-backtrace)))))
    #+lispworks
    (let ((dbg::*debugger-stack*
            (dbg::grab-stack nil :how-many (or count most-positive-fixnum)))
          (*debug-io* stream)
          (dbg:*debug-print-level* *print-level*)
          (dbg:*debug-print-length* *print-length*))
      (dbg:bug-backtrace nil))
    #+sbcl
    (sb-debug:backtrace
     #.(if (find-symbol* "*VERBOSITY*" "SB-DEBUG" nil) :stream '(or count most-positive-fixnum))
     stream)
    #+xcl
    (loop :for i :from 0 :below (or count most-positive-fixnum)
	  :for frame :in (extensions:backtrace-as-list) :do
	    (safe-format! stream "~&~D: ~S~%" i frame)))

  (defun print-backtrace (&rest keys &key stream count condition)
    "Print a backtrace"
    (declare (ignore stream count condition))
    (with-safe-io-syntax (:package :cl)
      (let ((*print-readably* nil)
            (*print-circle* t)
            (*print-miser-width* 75)
            (*print-length* nil)
            (*print-level* nil)
            (*print-pretty* t))
        (ignore-errors (apply 'raw-print-backtrace keys)))))

  (defun print-condition-backtrace (condition &key (stream *stderr*) count)
    "Print a condition after a backtrace triggered by that condition"
    ;; We print the condition *after* the backtrace,
    ;; for the sake of who sees the backtrace at a terminal.
    ;; It is up to the caller to print the condition *before*, with some context.
    (print-backtrace :stream stream :count count :condition condition)
    (when condition
      (safe-format! stream "~&Above backtrace due to this condition:~%~A~&"
                    condition)))

  (defun fatal-condition-p (condition)
    "Is the CONDITION fatal? It is if it matches any in *FATAL-CONDITIONS*"
    (match-any-condition-p condition *fatal-conditions*))

  (defun handle-fatal-condition (condition)
    "Handle a fatal CONDITION:
depending on whether *LISP-INTERACTION* is set, enter debugger or die"
    (cond
      (*lisp-interaction*
       (invoke-debugger condition))
      (t
       (safe-format! *stderr* "~&Fatal condition:~%~A~%" condition)
       (print-condition-backtrace condition :stream *stderr*)
       (die 99 "~A" condition))))

  (defun call-with-fatal-condition-handler (thunk)
    "Call THUNK in a context where fatal conditions are appropriately handled"
    (handler-bind (((satisfies fatal-condition-p) #'handle-fatal-condition))
      (funcall thunk)))

  (defmacro with-fatal-condition-handler ((&optional) &body body)
    "Execute BODY in a context where fatal conditions are appropriately handled"
    `(call-with-fatal-condition-handler #'(lambda () ,@body)))

  (defun shell-boolean-exit (x)
    "Quit with a return code that is 0 iff argument X is true"
    (quit (if x 0 1))))


;;; Using image hooks
(with-upgradability ()
  (defun register-image-restore-hook (hook &optional (call-now-p t))
    "Regiter a hook function to be run when restoring a dumped image"
    (register-hook-function '*image-restore-hook* hook call-now-p))

  (defun register-image-dump-hook (hook &optional (call-now-p nil))
    "Register a the hook function to be run before to dump an image"
    (register-hook-function '*image-dump-hook* hook call-now-p))

  (defun call-image-restore-hook ()
    "Call the hook functions registered to be run when restoring a dumped image"
    (call-functions (reverse *image-restore-hook*)))

  (defun call-image-dump-hook ()
    "Call the hook functions registered to be run before to dump an image"
    (call-functions *image-dump-hook*)))


;;; Proper command-line arguments
(with-upgradability ()
  (defun raw-command-line-arguments ()
    "Find what the actual command line for this process was."
    #+abcl ext:*command-line-argument-list* ; Use 1.0.0 or later!
    #+allegro (sys:command-line-arguments) ; default: :application t
    #+clisp (coerce (ext:argv) 'list)
    #+clozure (ccl::command-line-arguments)
    #+(or cmu scl) extensions:*command-line-strings*
    #+ecl (loop :for i :from 0 :below (si:argc) :collect (si:argv i))
    #+gcl si:*command-args*
    #+(or genera mcl) nil
    #+lispworks sys:*line-arguments-list*
    #+mkcl (loop :for i :from 0 :below (mkcl:argc) :collect (mkcl:argv i))
    #+sbcl sb-ext:*posix-argv*
    #+xcl system:*argv*
    #-(or abcl allegro clisp clozure cmu ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
    (error "raw-command-line-arguments not implemented yet"))

  (defun command-line-arguments (&optional (arguments (raw-command-line-arguments)))
    "Extract user arguments from command-line invocation of current process.
Assume the calling conventions of a generated script that uses --
if we are not called from a directly executable image."
    (block nil
      #+abcl (return arguments)
      ;; SBCL and Allegro already separate user arguments from implementation arguments.
      #-(or sbcl allegro)
      (unless (eq *image-dumped-p* :executable)
	;; LispWorks command-line processing isn't transparent to the user
	;; unless you create a standalone executable; in that case,
	;; we rely on cl-launch or some other script to set the arguments for us.
	#+lispworks (return *command-line-arguments*)
	;; On other implementations, on non-standalone executables,
	;; we trust cl-launch or whichever script starts the program
	;; to use -- as a delimiter between implementation arguments and user arguments.
	#-lispworks (setf arguments (member "--" arguments :test 'string-equal)))
      (rest arguments)))

  (defun argv0 ()
    "On supported implementations (most that matter), or when invoked by a proper wrapper script,
return a string that for the name with which the program was invoked, i.e. argv[0] in C.
Otherwise, return NIL."
    (cond
      ((eq *image-dumped-p* :executable) ; yes, this ARGV0 is our argv0 !
       ;; NB: not currently available on ABCL, Corman, Genera, MCL
       (or #+(or allegro clisp clozure cmu gcl lispworks sbcl scl xcl)
           (first (raw-command-line-arguments))
           #+ecl (si:argv 0) #+mkcl (mkcl:argv 0)))
      (t ;; argv[0] is the name of the interpreter.
       ;; The wrapper script can export __CL_ARGV0. cl-launch does as of 4.0.1.8.
       (getenvp "__CL_ARGV0"))))

  (defun setup-command-line-arguments ()
    (setf *command-line-arguments* (command-line-arguments)))

  (defun restore-image (&key
                          (lisp-interaction *lisp-interaction*)
                          (restore-hook *image-restore-hook*)
                          (prelude *image-prelude*)
                          (entry-point *image-entry-point*)
                          (if-already-restored '(cerror "RUN RESTORE-IMAGE ANYWAY")))
    "From a freshly restarted Lisp image, restore the saved Lisp environment
by setting appropriate variables, running various hooks, and calling any specified entry point.

If the image has already been restored or is already being restored, as per *IMAGE-RESTORED-P*,
call the IF-ALREADY-RESTORED error handler (by default, a continuable error), and do return
immediately to the surrounding restore process if allowed to continue.

Then, comes the restore process itself:
First, call each function in the RESTORE-HOOK,
in the order they were registered with REGISTER-RESTORE-HOOK.
Second, evaluate the prelude, which is often Lisp text that is read,
as per EVAL-INPUT.
Third, call the ENTRY-POINT function, if any is specified, with no argument.

The restore process happens in a WITH-FATAL-CONDITION-HANDLER, so that if LISP-INTERACTION is NIL,
any unhandled error leads to a backtrace and an exit with an error status.
If LISP-INTERACTION is NIL, the process also exits when no error occurs:
if neither restart nor entry function is provided, the program will exit with status 0 (success);
if a function was provided, the program will exit after the function returns (if it returns),
with status 0 if and only if the primary return value of result is generalized boolean true,
and with status 1 if this value is NIL.

If LISP-INTERACTION is true, unhandled errors will take you to the debugger, and the result
of the function will be returned rather than interpreted as a boolean designating an exit code."
    (when *image-restored-p*
      (if if-already-restored
          (call-function if-already-restored "Image already ~:[being ~;~]restored"
                         (eq *image-restored-p* t))
          (return-from restore-image)))
    (with-fatal-condition-handler ()
      (setf *lisp-interaction* lisp-interaction)
      (setf *image-restore-hook* restore-hook)
      (setf *image-prelude* prelude)
      (setf *image-restored-p* :in-progress)
      (call-image-restore-hook)
      (standard-eval-thunk prelude)
      (setf *image-restored-p* t)
      (let ((results (multiple-value-list
                      (if entry-point
                          (call-function entry-point)
                          t))))
        (if lisp-interaction
            (apply 'values results)
            (shell-boolean-exit (first results)))))))


;;; Dumping an image

(with-upgradability ()
  (defun dump-image (filename &key output-name executable
                                (postlude *image-postlude*)
                                (dump-hook *image-dump-hook*)
                                #+clozure prepend-symbols #+clozure (purify t)
                                #+sbcl compression
                                #+(and sbcl windows) application-type)
    "Dump an image of the current Lisp environment at pathname FILENAME, with various options.

First, finalize the image, by evaluating the POSTLUDE as per EVAL-INPUT, then calling each of
 the functions in DUMP-HOOK, in reverse order of registration by REGISTER-DUMP-HOOK.

If EXECUTABLE is true, create an standalone executable program that calls RESTORE-IMAGE on startup.

Pass various implementation-defined options, such as PREPEND-SYMBOLS and PURITY on CCL,
or COMPRESSION on SBCL, and APPLICATION-TYPE on SBCL/Windows."
    ;; Note: at least SBCL saves only global values of variables in the heap image,
    ;; so make sure things you want to dump are NOT just local bindings shadowing the global values.
    (declare (ignorable filename output-name executable))
    (setf *image-dumped-p* (if executable :executable t))
    (setf *image-restored-p* :in-regress)
    (setf *image-postlude* postlude)
    (standard-eval-thunk *image-postlude*)
    (setf *image-dump-hook* dump-hook)
    (call-image-dump-hook)
    (setf *image-restored-p* nil)
    #-(or clisp clozure cmu lispworks sbcl scl)
    (when executable
      (error "Dumping an executable is not supported on this implementation! Aborting."))
    #+allegro
    (progn
      (sys:resize-areas :global-gc t :pack-heap t :sift-old-areas t :tenure t) ; :new 5000000
      (excl:dumplisp :name filename :suppress-allegro-cl-banner t))
    #+clisp
    (apply #'ext:saveinitmem filename
           :quiet t
           :start-package *package*
           :keep-global-handlers nil
           :executable (if executable 0 t) ;--- requires clisp 2.48 or later, still catches --clisp-x
           (when executable
             (list
              ;; :parse-options nil ;--- requires a non-standard patch to clisp.
              :norc t :script nil :init-function #'restore-image)))
    #+clozure
    (flet ((dump (prepend-kernel)
             (ccl:save-application filename :prepend-kernel prepend-kernel :purify purify
                                            :toplevel-function (when executable #'restore-image))))
      ;;(setf ccl::*application* (make-instance 'ccl::lisp-development-system))
      (if prepend-symbols
          (with-temporary-file (:prefix "ccl-symbols-" :direction :output :pathname path)
            (require 'elf)
            (funcall (fdefinition 'ccl::write-elf-symbols-to-file) path)
            (dump path))
          (dump t)))
    #+(or cmu scl)
    (progn
      (ext:gc :full t)
      (setf ext:*batch-mode* nil)
      (setf ext::*gc-run-time* 0)
      (apply 'ext:save-lisp filename
             #+cmu :executable #+cmu t
             (when executable '(:init-function restore-image :process-command-line nil))))
    #+gcl
    (progn
      (si::set-hole-size 500) (si::gbc nil) (si::sgc-on t)
      (si::save-system filename))
    #+lispworks
    (if executable
        (lispworks:deliver 'restore-image filename 0 :interface nil)
        (hcl:save-image filename :environment nil))
    #+sbcl
    (progn
      ;;(sb-pcl::precompile-random-code-segments) ;--- it is ugly slow at compile-time (!) when the initial core is a big CLOS program. If you want it, do it yourself
      (setf sb-ext::*gc-run-time* 0)
      (apply 'sb-ext:save-lisp-and-die filename
             :executable t ;--- always include the runtime that goes with the core
             (append
              (when compression (list :compression compression))
              ;;--- only save runtime-options for standalone executables
              (when executable (list :toplevel #'restore-image :save-runtime-options t))
              #+(and sbcl windows) ;; passing :application-type :gui will disable the console window.
              ;; the default is :console - only works with SBCL 1.1.15 or later.
              (when application-type (list :application-type application-type)))))
    #-(or allegro clisp clozure cmu gcl lispworks sbcl scl)
    (error "Can't ~S ~S: UIOP doesn't support image dumping with ~A.~%"
           'dump-image filename (nth-value 1 (implementation-type))))

  (defun create-image (destination lisp-object-files
                       &key kind output-name prologue-code epilogue-code extra-object-files
                         (prelude () preludep) (postlude () postludep)
                         (entry-point () entry-point-p) build-args no-uiop)
    (declare (ignorable destination lisp-object-files extra-object-files kind output-name
                        prologue-code epilogue-code prelude preludep postlude postludep
                        entry-point entry-point-p build-args no-uiop))
    "On ECL, create an executable at pathname DESTINATION from the specified OBJECT-FILES and options"
    ;; Is it meaningful to run these in the current environment?
    ;; only if we also track the object files that constitute the "current" image,
    ;; and otherwise simulate dump-image, including quitting at the end.
    #-(or ecl mkcl) (error "~S not implemented for your implementation (yet)" 'create-image)
    #+(or ecl mkcl)
    (let ((epilogue-code
            (if no-uiop
                epilogue-code
                (let ((forms
                        (append
                         (when epilogue-code `(,epilogue-code))
                         (when postludep `((setf *image-postlude* ',postlude)))
                         (when preludep `((setf *image-prelude* ',prelude)))
                         (when entry-point-p `((setf *image-entry-point* ',entry-point)))
                         (case kind
                           ((:image)
                            (setf kind :program) ;; to ECL, it's just another program.
                            `((setf *image-dumped-p* t)
                              (si::top-level #+ecl t) (quit)))
                           ((:program)
                            `((setf *image-dumped-p* :executable)
                              (shell-boolean-exit
                               (restore-image))))))))
                  (when forms `(progn ,@forms))))))
      #+ecl (check-type kind (member :dll :lib :static-library :program :object :fasl))
      (apply #+ecl 'c::builder #+ecl kind
             #+mkcl (ecase kind
                      ((:dll) 'compiler::build-shared-library)
                      ((:lib :static-library) 'compiler::build-static-library)
                      ((:fasl) 'compiler::build-bundle)
                      ((:program) 'compiler::build-program))
             (pathname destination)
             #+ecl :lisp-files #+mkcl :lisp-object-files (append lisp-object-files #+ecl extra-object-files)
             #+ecl :init-name #+ecl (c::compute-init-name (or output-name destination) :kind kind)
             (append
              (when prologue-code `(:prologue-code ,prologue-code))
              (when epilogue-code `(:epilogue-code ,epilogue-code))
              #+mkcl (when extra-object-files `(:object-files ,extra-object-files))
              build-args)))))


;;; Some universal image restore hooks
(with-upgradability ()
  (map () 'register-image-restore-hook
       '(setup-stdin setup-stdout setup-stderr
         setup-command-line-arguments setup-temporary-directory
         #+abcl detect-os)))
;;;; -------------------------------------------------------------------------
;;;; run-program initially from xcvb-driver.

(uiop/package:define-package :uiop/run-program
  (:nicknames :asdf/run-program)
  (:recycle :uiop/run-program :asdf/run-program :xcvb-driver)
  (:use :uiop/common-lisp :uiop/package :uiop/utility
   :uiop/pathname :uiop/os :uiop/filesystem :uiop/stream)
  (:export
   ;;; Escaping the command invocation madness
   #:easy-sh-character-p #:escape-sh-token #:escape-sh-command
   #:escape-windows-token #:escape-windows-command
   #:escape-token #:escape-command

   ;;; run-program
   #:slurp-input-stream #:vomit-output-stream
   #:run-program
   #:subprocess-error
   #:subprocess-error-code #:subprocess-error-command #:subprocess-error-process
   ))
(in-package :uiop/run-program)

;;;; ----- Escaping strings for the shell -----

(with-upgradability ()
  (defun requires-escaping-p (token &key good-chars bad-chars)
    "Does this token require escaping, given the specification of
either good chars that don't need escaping or bad chars that do need escaping,
as either a recognizing function or a sequence of characters."
    (some
     (cond
       ((and good-chars bad-chars)
        (error "only one of good-chars and bad-chars can be provided"))
       ((functionp good-chars)
        (complement good-chars))
       ((functionp bad-chars)
        bad-chars)
       ((and good-chars (typep good-chars 'sequence))
        #'(lambda (c) (not (find c good-chars))))
       ((and bad-chars (typep bad-chars 'sequence))
        #'(lambda (c) (find c bad-chars)))
       (t (error "requires-escaping-p: no good-char criterion")))
     token))

  (defun escape-token (token &key stream quote good-chars bad-chars escaper)
    "Call the ESCAPER function on TOKEN string if it needs escaping as per
REQUIRES-ESCAPING-P using GOOD-CHARS and BAD-CHARS, otherwise output TOKEN,
using STREAM as output (or returning result as a string if NIL)"
    (if (requires-escaping-p token :good-chars good-chars :bad-chars bad-chars)
        (with-output (stream)
          (apply escaper token stream (when quote `(:quote ,quote))))
        (output-string token stream)))

  (defun escape-windows-token-within-double-quotes (x &optional s)
    "Escape a string token X within double-quotes
for use within a MS Windows command-line, outputing to S."
    (labels ((issue (c) (princ c s))
             (issue-backslash (n) (loop :repeat n :do (issue #\\))))
      (loop
        :initially (issue #\") :finally (issue #\")
        :with l = (length x) :with i = 0
        :for i+1 = (1+ i) :while (< i l) :do
          (case (char x i)
            ((#\") (issue-backslash 1) (issue #\") (setf i i+1))
            ((#\\)
             (let* ((j (and (< i+1 l) (position-if-not
                                       #'(lambda (c) (eql c #\\)) x :start i+1)))
                    (n (- (or j l) i)))
               (cond
                 ((null j)
                  (issue-backslash (* 2 n)) (setf i l))
                 ((and (< j l) (eql (char x j) #\"))
                  (issue-backslash (1+ (* 2 n))) (issue #\") (setf i (1+ j)))
                 (t
                  (issue-backslash n) (setf i j)))))
            (otherwise
             (issue (char x i)) (setf i i+1))))))

  (defun escape-windows-token (token &optional s)
    "Escape a string TOKEN within double-quotes if needed
for use within a MS Windows command-line, outputing to S."
    (escape-token token :stream s :bad-chars #(#\space #\tab #\") :quote nil
                        :escaper 'escape-windows-token-within-double-quotes))

  (defun escape-sh-token-within-double-quotes (x s &key (quote t))
    "Escape a string TOKEN within double-quotes
for use within a POSIX Bourne shell, outputing to S;
omit the outer double-quotes if key argument :QUOTE is NIL"
    (when quote (princ #\" s))
    (loop :for c :across x :do
      (when (find c "$`\\\"") (princ #\\ s))
      (princ c s))
    (when quote (princ #\" s)))

  (defun easy-sh-character-p (x)
    "Is X an \"easy\" character that does not require quoting by the shell?"
    (or (alphanumericp x) (find x "+-_.,%@:/")))

  (defun escape-sh-token (token &optional s)
    "Escape a string TOKEN within double-quotes if needed
for use within a POSIX Bourne shell, outputing to S."
    (escape-token token :stream s :quote #\" :good-chars #'easy-sh-character-p
                        :escaper 'escape-sh-token-within-double-quotes))

  (defun escape-shell-token (token &optional s)
    "Escape a token for the current operating system shell"
    (cond
      ((os-unix-p) (escape-sh-token token s))
      ((os-windows-p) (escape-windows-token token s))))

  (defun escape-command (command &optional s
                                  (escaper 'escape-shell-token))
    "Given a COMMAND as a list of tokens, return a string of the
spaced, escaped tokens, using ESCAPER to escape."
    (etypecase command
      (string (output-string command s))
      (list (with-output (s)
              (loop :for first = t :then nil :for token :in command :do
                (unless first (princ #\space s))
                (funcall escaper token s))))))

  (defun escape-windows-command (command &optional s)
    "Escape a list of command-line arguments into a string suitable for parsing
by CommandLineToArgv in MS Windows"
    ;; http://msdn.microsoft.com/en-us/library/bb776391(v=vs.85).aspx
    ;; http://msdn.microsoft.com/en-us/library/17w5ykft(v=vs.85).aspx
    (escape-command command s 'escape-windows-token))

  (defun escape-sh-command (command &optional s)
    "Escape a list of command-line arguments into a string suitable for parsing
by /bin/sh in POSIX"
    (escape-command command s 'escape-sh-token))

  (defun escape-shell-command (command &optional stream)
    "Escape a command for the current operating system's shell"
    (escape-command command stream 'escape-shell-token)))


;;;; Slurping a stream, typically the output of another program
(with-upgradability ()
  (defun call-stream-processor (fun processor stream)
    "Given FUN (typically SLURP-INPUT-STREAM or VOMIT-OUTPUT-STREAM,
a PROCESSOR specification which is either an atom or a list specifying
a processor an keyword arguments, call the specified processor with
the given STREAM as input"
    (if (consp processor)
        (apply fun (first processor) stream (rest processor))
        (funcall fun processor stream)))

  (defgeneric slurp-input-stream (processor input-stream &key)
    (:documentation
     "SLURP-INPUT-STREAM is a generic function with two positional arguments
PROCESSOR and INPUT-STREAM and additional keyword arguments, that consumes (slurps)
the contents of the INPUT-STREAM and processes them according to a method
specified by PROCESSOR.

Built-in methods include the following:
* if PROCESSOR is a function, it is called with the INPUT-STREAM as its argument
* if PROCESSOR is a list, its first element should be a function.  It will be applied to a cons of the
  INPUT-STREAM and the rest of the list.  That is (x . y) will be treated as
    \(APPLY x <stream> y\)
* if PROCESSOR is an output-stream, the contents of INPUT-STREAM is copied to the output-stream,
  per copy-stream-to-stream, with appropriate keyword arguments.
* if PROCESSOR is the symbol CL:STRING or the keyword :STRING, then the contents of INPUT-STREAM
  are returned as a string, as per SLURP-STREAM-STRING.
* if PROCESSOR is the keyword :LINES then the INPUT-STREAM will be handled by SLURP-STREAM-LINES.
* if PROCESSOR is the keyword :LINE then the INPUT-STREAM will be handled by SLURP-STREAM-LINE.
* if PROCESSOR is the keyword :FORMS then the INPUT-STREAM will be handled by SLURP-STREAM-FORMS.
* if PROCESSOR is the keyword :FORM then the INPUT-STREAM will be handled by SLURP-STREAM-FORM.
* if PROCESSOR is T, it is treated the same as *standard-output*. If it is NIL, NIL is returned.

Programmers are encouraged to define their own methods for this generic function."))

  #-genera
  (defmethod slurp-input-stream ((function function) input-stream &key)
    (funcall function input-stream))

  (defmethod slurp-input-stream ((list cons) input-stream &key)
    (apply (first list) input-stream (rest list)))

  #-genera
  (defmethod slurp-input-stream ((output-stream stream) input-stream
                                 &key linewise prefix (element-type 'character) buffer-size)
    (copy-stream-to-stream
     input-stream output-stream
     :linewise linewise :prefix prefix :element-type element-type :buffer-size buffer-size))

  (defmethod slurp-input-stream ((x (eql 'string)) stream &key stripped)
    (slurp-stream-string stream :stripped stripped))

  (defmethod slurp-input-stream ((x (eql :string)) stream &key stripped)
    (slurp-stream-string stream :stripped stripped))

  (defmethod slurp-input-stream ((x (eql :lines)) stream &key count)
    (slurp-stream-lines stream :count count))

  (defmethod slurp-input-stream ((x (eql :line)) stream &key (at 0))
    (slurp-stream-line stream :at at))

  (defmethod slurp-input-stream ((x (eql :forms)) stream &key count)
    (slurp-stream-forms stream :count count))

  (defmethod slurp-input-stream ((x (eql :form)) stream &key (at 0))
    (slurp-stream-form stream :at at))

  (defmethod slurp-input-stream ((x (eql t)) stream &rest keys &key &allow-other-keys)
    (apply 'slurp-input-stream *standard-output* stream keys))

  (defmethod slurp-input-stream ((x null) (stream t) &key)
    nil)

  (defmethod slurp-input-stream ((pathname pathname) input
                                 &key
                                   (element-type *default-stream-element-type*)
                                   (external-format *utf-8-external-format*)
                                   (if-exists :rename-and-delete)
                                   (if-does-not-exist :create)
                                   buffer-size
                                   linewise)
    (with-output-file (output pathname
                              :element-type element-type
                              :external-format external-format
                              :if-exists if-exists
                              :if-does-not-exist if-does-not-exist)
      (copy-stream-to-stream
       input output
       :element-type element-type :buffer-size buffer-size :linewise linewise)))

  (defmethod slurp-input-stream (x stream
                                 &key linewise prefix (element-type 'character) buffer-size)
    (declare (ignorable stream linewise prefix element-type buffer-size))
    (cond
      #+genera
      ((functionp x) (funcall x stream))
      #+genera
      ((output-stream-p x)
       (copy-stream-to-stream
        stream x
        :linewise linewise :prefix prefix :element-type element-type :buffer-size buffer-size))
      (t
       (error "Invalid ~S destination ~S" 'slurp-input-stream x)))))


(with-upgradability ()
  (defgeneric vomit-output-stream (processor output-stream &key)
    (:documentation
     "VOMIT-OUTPUT-STREAM is a generic function with two positional arguments
PROCESSOR and OUTPUT-STREAM and additional keyword arguments, that produces (vomits)
some content onto the OUTPUT-STREAM, according to a method specified by PROCESSOR.

Built-in methods include the following:
* if PROCESSOR is a function, it is called with the OUTPUT-STREAM as its argument
* if PROCESSOR is a list, its first element should be a function.
  It will be applied to a cons of the OUTPUT-STREAM and the rest of the list.
  That is (x . y) will be treated as \(APPLY x <stream> y\)
* if PROCESSOR is an input-stream, its contents will be copied the OUTPUT-STREAM,
  per copy-stream-to-stream, with appropriate keyword arguments.
* if PROCESSOR is a string, its contents will be printed to the OUTPUT-STREAM.
* if PROCESSOR is T, it is treated the same as *standard-input*. If it is NIL, nothing is done.

Programmers are encouraged to define their own methods for this generic function."))

  #-genera
  (defmethod vomit-output-stream ((function function) output-stream &key)
    (funcall function output-stream))

  (defmethod vomit-output-stream ((list cons) output-stream &key)
    (apply (first list) output-stream (rest list)))

  #-genera
  (defmethod vomit-output-stream ((input-stream stream) output-stream
                                 &key linewise prefix (element-type 'character) buffer-size)
    (copy-stream-to-stream
     input-stream output-stream
     :linewise linewise :prefix prefix :element-type element-type :buffer-size buffer-size))

  (defmethod vomit-output-stream ((x string) stream &key fresh-line terpri)
    (princ x stream)
    (when fresh-line (fresh-line stream))
    (when terpri (terpri stream))
    (values))

  (defmethod vomit-output-stream ((x (eql t)) stream &rest keys &key &allow-other-keys)
    (apply 'vomit-output-stream *standard-input* stream keys))

  (defmethod vomit-output-stream ((x null) (stream t) &key)
    (values))

  (defmethod vomit-output-stream ((pathname pathname) input
                                 &key
                                   (element-type *default-stream-element-type*)
                                   (external-format *utf-8-external-format*)
                                   (if-exists :rename-and-delete)
                                   (if-does-not-exist :create)
                                   buffer-size
                                   linewise)
    (with-output-file (output pathname
                              :element-type element-type
                              :external-format external-format
                              :if-exists if-exists
                              :if-does-not-exist if-does-not-exist)
      (copy-stream-to-stream
       input output
       :element-type element-type :buffer-size buffer-size :linewise linewise)))

  (defmethod vomit-output-stream (x stream
                                 &key linewise prefix (element-type 'character) buffer-size)
    (declare (ignorable stream linewise prefix element-type buffer-size))
    (cond
      #+genera
      ((functionp x) (funcall x stream))
      #+genera
      ((input-stream-p x)
       (copy-stream-to-stream
        x stream
        :linewise linewise :prefix prefix :element-type element-type :buffer-size buffer-size))
      (t
       (error "Invalid ~S source ~S" 'vomit-output-stream x)))))


;;;; ----- Running an external program -----
;;; Simple variant of run-program with no input, and capturing output
;;; On some implementations, may output to a temporary file...
(with-upgradability ()
  (define-condition subprocess-error (error)
    ((code :initform nil :initarg :code :reader subprocess-error-code)
     (command :initform nil :initarg :command :reader subprocess-error-command)
     (process :initform nil :initarg :process :reader subprocess-error-process))
    (:report (lambda (condition stream)
               (format stream "Subprocess~@[ ~S~]~@[ run with command ~S~] exited with error~@[ code ~D~]"
                       (subprocess-error-process condition)
                       (subprocess-error-command condition)
                       (subprocess-error-code condition)))))

  ;;; Internal helpers for run-program
  (defun %normalize-command (command)
    "Given a COMMAND as a list or string, transform it in a format suitable
for the implementation's underlying run-program function"
    (etypecase command
      #+os-unix (string `("/bin/sh" "-c" ,command))
      #+os-unix (list command)
      #+os-windows
      (string
       #+mkcl (list "cmd" '#:/c command)
       ;; NB: We do NOT add cmd /c here. You might want to.
       #+(or allegro clisp) command
       ;; On ClozureCL for Windows, we assume you are using
       ;; r15398 or later in 1.9 or later,
       ;; so that bug 858 is fixed http://trac.clozure.com/ccl/ticket/858
       #+clozure (cons "cmd" (strcat "/c " command))
       ;; NB: On other Windows implementations, this is utterly bogus
       ;; except in the most trivial cases where no quoting is needed.
       ;; Use at your own risk.
       #-(or allegro clisp clozure mkcl) (list "cmd" "/c" command))
      #+os-windows
      (list
       #+allegro (escape-windows-command command)
       #-allegro command)))

  (defun %active-io-specifier-p (specifier)
    "Determines whether a run-program I/O specifier requires Lisp-side processing
via SLURP-INPUT-STREAM or VOMIT-OUTPUT-STREAM (return T),
or whether it's already taken care of by the implementation's underlying run-program."
    (not (typep specifier '(or null string pathname (member :interactive :output)
                            #+(or cmu (and sbcl os-unix) scl) (or stream (eql t))
                            #+lispworks file-stream)))) ;; not a type!? comm:socket-stream

  (defun %normalize-io-specifier (specifier &optional role)
    "Normalizes a portable I/O specifier for %RUN-PROGRAM into an implementation-dependent
argument to pass to the internal RUN-PROGRAM"
    (declare (ignorable role))
    (etypecase specifier
      (null (or #+(or allegro lispworks) (null-device-pathname)))
      (string (parse-native-namestring specifier))
      (pathname specifier)
      (stream specifier)
      ((eql :stream) :stream)
      ((eql :interactive)
       #+allegro nil
       #+clisp :terminal
       #+(or clozure cmu ecl mkcl sbcl scl) t)
      #+(or allegro clozure cmu ecl lispworks mkcl sbcl scl)
      ((eql :output)
       (if (eq role :error-output)
           :output
           (error "Wrong specifier ~S for role ~S" specifier role)))))

  (defun %interactivep (input output error-output)
    (member :interactive (list input output error-output)))

  #+clisp
  (defun clisp-exit-code (raw-exit-code)
    (typecase raw-exit-code
      (null 0) ; no error
      (integer raw-exit-code) ; negative: signal
      (t -1)))

  (defun %run-program (command
                       &rest keys
                       &key input (if-input-does-not-exist :error)
                         output (if-output-exists :overwrite)
                         error-output (if-error-output-exists :overwrite)
                         directory wait
                         #+allegro separate-streams
                         &allow-other-keys)
    "A portable abstraction of a low-level call to the implementation's run-program or equivalent.
It spawns a subprocess that runs the specified COMMAND (a list of program and arguments).
INPUT, OUTPUT and ERROR-OUTPUT specify a portable IO specifer,
to be normalized by %NORMALIZE-IO-SPECIFIER.
It returns a process-info plist with possible keys:
     PROCESS, EXIT-CODE, INPUT-STREAM, OUTPUT-STREAM, BIDIR-STREAM, ERROR-STREAM."
    ;; NB: these implementations have unix vs windows set at compile-time.
    (declare (ignorable directory if-input-does-not-exist if-output-exists if-error-output-exists))
    (assert (not (and wait (member :stream (list input output error-output)))))
    #-(or allegro clisp clozure cmu (and lispworks os-unix) mkcl sbcl scl)
    (progn command keys directory
           (error "run-program not available"))
    #+(or allegro clisp clozure cmu (and lispworks os-unix) mkcl sbcl scl)
    (let* ((%command (%normalize-command command))
           (%input (%normalize-io-specifier input :input))
           (%output (%normalize-io-specifier output :output))
           (%error-output (%normalize-io-specifier error-output :error-output))
           #+(and allegro os-windows) (interactive (%interactivep input output error-output))
           (process*
             #+allegro
             (multiple-value-list
              (apply
               'excl:run-shell-command
               #+os-unix (coerce (cons (first %command) %command) 'vector)
               #+os-windows %command
               :input %input
               :output %output
               :error-output %error-output
               :directory directory :wait wait
               #+os-windows :show-window #+os-windows (if interactive nil :hide)
               :allow-other-keys t keys))
             #-allegro
             (with-current-directory (#-(or sbcl mkcl) directory)
               #+clisp
               (flet ((run (f x &rest args)
                        (multiple-value-list
                         (apply f x :input %input :output %output
                                    :allow-other-keys t `(,@args ,@keys)))))
                 (assert (eq %error-output :terminal))
                 ;;; since we now always return a code, we can't use this code path, anyway!
                 (etypecase %command
                   #+os-windows (string (run 'ext:run-shell-command %command))
                   (list (run 'ext:run-program (car %command)
                              :arguments (cdr %command)))))
               #+(or clozure cmu ecl mkcl sbcl scl)
               (#-(or ecl mkcl) progn #+(or ecl mkcl) multiple-value-list
                (apply
                 '#+(or cmu ecl scl) ext:run-program
                 #+clozure ccl:run-program #+sbcl sb-ext:run-program #+mkcl mk-ext:run-program
                 (car %command) (cdr %command)
                 :input %input
                 :output %output
                 :error %error-output
                 :wait wait
                 :allow-other-keys t
                 (append
                  #+(or clozure cmu mkcl sbcl scl)
                  `(:if-input-does-not-exist ,if-input-does-not-exist
                    :if-output-exists ,if-output-exists
                    :if-error-exists ,if-error-output-exists)
                  #+sbcl `(:search t
                           :if-output-does-not-exist :create
                           :if-error-does-not-exist :create)
                  #-sbcl keys #+sbcl (if directory keys (remove-plist-key :directory keys)))))
               #+(and lispworks os-unix) ;; note: only used on Unix in non-interactive case
               (multiple-value-list
                (apply
                 'system:run-shell-command
                 (cons "/usr/bin/env" %command) ; lispworks wants a full path.
                 :input %input :if-input-does-not-exist if-input-does-not-exist
                 :output %output :if-output-exists if-output-exists
                 :error-output %error-output :if-error-output-exists if-error-output-exists
                 :wait wait :save-exit-status t :allow-other-keys t keys))))
           (process-info-r ()))
      (flet ((prop (key value) (push key process-info-r) (push value process-info-r)))
        #+allegro
        (cond
          (wait (prop :exit-code (first process*)))
          (separate-streams
           (destructuring-bind (in out err pid) process*
             (prop :process pid)
             (when (eq input :stream) (prop :input-stream in))
             (when (eq output :stream) (prop :output-stream out))
             (when (eq error-output :stream) (prop :error-stream err))))
          (t
           (prop :process (third process*))
           (let ((x (first process*)))
             (ecase (+ (if (eq input :stream) 1 0) (if (eq output :stream) 2 0))
               (0)
               (1 (prop :input-stream x))
               (2 (prop :output-stream x))
               (3 (prop :bidir-stream x))))
           (when (eq error-output :stream)
             (prop :error-stream (second process*)))))
        #+clisp
        (cond
          (wait (prop :exit-code (clisp-exit-code (first process*))))
          (t
           (ecase (+ (if (eq input :stream) 1 0) (if (eq output :stream) 2 0))
             (0)
             (1 (prop :input-stream (first process*)))
             (2 (prop :output-stream (first process*)))
             (3 (prop :bidir-stream (pop process*))
                (prop :input-stream (pop process*))
                (prop :output-stream (pop process*))))))
        #+(or clozure cmu sbcl scl)
        (progn
          (prop :process process*)
          (when (eq input :stream)
            (prop :input-stream
                  #+clozure (ccl:external-process-input-stream process*)
                  #+(or cmu scl) (ext:process-input process*)
                  #+sbcl (sb-ext:process-input process*)))
          (when (eq output :stream)
            (prop :output-stream
                  #+clozure (ccl:external-process-output-stream process*)
                  #+(or cmu scl) (ext:process-output process*)
                  #+sbcl (sb-ext:process-output process*)))
          (when (eq error-output :stream)
            (prop :error-output-stream
                  #+clozure (ccl:external-process-error-stream process*)
                  #+(or cmu scl) (ext:process-error process*)
                  #+sbcl (sb-ext:process-error process*))))
        #+(or ecl mkcl)
        (destructuring-bind #+ecl (stream code process) #+mkcl (stream process code) process*
          (let ((mode (+ (if (eq input :stream) 1 0) (if (eq output :stream) 2 0))))
            (cond
              ((zerop mode))
              ((null process*) (prop :exit-code -1))
              (t (prop (case mode (1 :input-stream) (2 :output-stream) (3 :bidir-stream)) stream))))
          (when code (prop :exit-code code))
          (when process (prop :process process)))
        #+lispworks
        (if wait
            (prop :exit-code (first process*))
            (let ((mode (+ (if (eq input :stream) 1 0) (if (eq output :stream) 2 0))))
              (if (zerop mode)
                  (prop :process (first process*))
                  (destructuring-bind (x err pid) process*
                    (prop :process pid)
                    (prop (ecase mode (1 :input-stream) (2 :output-stream) (3 :bidir-stream)) x)
                    (when (eq error-output :stream) (prop :error-stream err))))))
        (nreverse process-info-r))))

  (defun %process-info-pid (process-info)
    (let ((process (getf process-info :process)))
      (declare (ignorable process))
      #+(or allegro lispworks) process
      #+clozure (ccl::external-process-pid process)
      #+ecl (si:external-process-pid process)
      #+(or cmu scl) (ext:process-pid process)
      #+mkcl (mkcl:process-id process)
      #+sbcl (sb-ext:process-pid process)
      #-(or allegro cmu mkcl sbcl scl) (error "~S not implemented" '%process-info-pid)))

  (defun %wait-process-result (process-info)
    (or (getf process-info :exit-code)
        (let ((process (getf process-info :process)))
          (when process
            ;; 1- wait
            #+clozure (ccl::external-process-wait process)
            #+(or cmu scl) (ext:process-wait process)
            #+(and ecl os-unix) (ext:external-process-wait process)
            #+sbcl (sb-ext:process-wait process)
            ;; 2- extract result
            #+allegro (sys:reap-os-subprocess :pid process :wait t)
            #+clozure (nth-value 1 (ccl:external-process-status process))
            #+(or cmu scl) (ext:process-exit-code process)
            #+ecl (nth-value 1 (ext:external-process-status process))
            #+lispworks
            (if-let ((stream (or (getf process-info :input-stream)
                                 (getf process-info :output-stream)
                                 (getf process-info :bidir-stream)
                                 (getf process-info :error-stream))))
              (system:pipe-exit-status stream :wait t)
              (if-let ((f (find-symbol* :pid-exit-status :system nil)))
                (funcall f process :wait t)))
            #+sbcl (sb-ext:process-exit-code process)
            #+mkcl (mkcl:join-process process)))))

  (defun %check-result (exit-code &key command process ignore-error-status)
    (unless ignore-error-status
      (unless (eql exit-code 0)
        (cerror "IGNORE-ERROR-STATUS"
                'subprocess-error :command command :code exit-code :process process)))
    exit-code)

  (defun %call-with-program-io (gf tval stream-easy-p fun direction spec activep returner
                                &key element-type external-format &allow-other-keys)
    ;; handle redirection for run-program and system
    ;; SPEC is the specification for the subprocess's input or output or error-output
    ;; TVAL is the value used if the spec is T
    ;; GF is the generic function to call to handle arbitrary values of SPEC
    ;; STREAM-EASY-P is T if we're going to use a RUN-PROGRAM that copies streams in the background
    ;; (it's only meaningful on CMUCL, SBCL, SCL that actually do it)
    ;; DIRECTION is :INPUT, :OUTPUT or :ERROR-OUTPUT for the direction of this io argument
    ;; FUN is a function of the new reduced spec and an activity function to call with a stream
    ;; when the subprocess is active and communicating through that stream.
    ;; ACTIVEP is a boolean true if we will get to run code while the process is running
    ;; ELEMENT-TYPE and EXTERNAL-FORMAT control what kind of temporary file we may open.
    ;; RETURNER is a function called with the value of the activity.
    ;; --- TODO (fare@tunes.org): handle if-output-exists and such when doing it the hard way.
    (declare (ignorable stream-easy-p))
    (let* ((actual-spec (if (eq spec t) tval spec))
           (activity-spec (if (eq actual-spec :output)
                              (ecase direction
                                ((:input :output)
                                 (error "~S not allowed as a ~S ~S spec"
                                        :output 'run-program direction))
                                ((:error-output)
                                 nil))
                              actual-spec)))
      (labels ((activity (stream)
                 (call-function returner (call-stream-processor gf activity-spec stream)))
               (easy-case ()
                 (funcall fun actual-spec nil))
               (hard-case ()
                 (if activep
                     (funcall fun :stream #'activity)
                     (with-temporary-file (:pathname tmp)
                       (ecase direction
                         (:input
                          (with-output-file (s tmp :if-exists :overwrite
                                               :external-format external-format
                                               :element-type element-type)
                            (activity s))
                          (funcall fun tmp nil))
                         ((:output :error-output)
                          (multiple-value-prog1 (funcall fun tmp nil)
                            (with-input-file (s tmp
                                               :external-format external-format
                                               :element-type element-type)
                              (activity s)))))))))
        (typecase activity-spec
          ((or null string pathname (eql :interactive))
           (easy-case))
          #+(or cmu (and sbcl os-unix) scl) ;; streams are only easy on implementations that try very hard
          (stream
           (if stream-easy-p (easy-case) (hard-case)))
          (t
           (hard-case))))))

  (defmacro place-setter (place)
    (when place
      (let ((value (gensym)))
        `#'(lambda (,value) (setf ,place ,value)))))

  (defmacro with-program-input (((reduced-input-var
                                  &optional (input-activity-var (gensym) iavp))
                                 input-form &key setf stream-easy-p active keys) &body body)
    `(apply '%call-with-program-io 'vomit-output-stream *standard-input* ,stream-easy-p
            #'(lambda (,reduced-input-var ,input-activity-var)
                ,@(unless iavp `((declare (ignore ,input-activity-var))))
                ,@body)
            :input ,input-form ,active (place-setter ,setf) ,keys))

  (defmacro with-program-output (((reduced-output-var
                                  &optional (output-activity-var (gensym) oavp))
                                  output-form &key setf stream-easy-p active keys) &body body)
    `(apply '%call-with-program-io 'slurp-input-stream *standard-output* ,stream-easy-p
            #'(lambda (,reduced-output-var ,output-activity-var)
                ,@(unless oavp `((declare (ignore ,output-activity-var))))
                ,@body)
            :output ,output-form ,active (place-setter ,setf) ,keys))

  (defmacro with-program-error-output (((reduced-error-output-var
                                         &optional (error-output-activity-var (gensym) eoavp))
                                        error-output-form &key setf stream-easy-p active keys)
                                       &body body)
    `(apply '%call-with-program-io 'slurp-input-stream *error-output* ,stream-easy-p
            #'(lambda (,reduced-error-output-var ,error-output-activity-var)
                ,@(unless eoavp `((declare (ignore ,error-output-activity-var))))
                ,@body)
            :error-output ,error-output-form ,active (place-setter ,setf) ,keys))

  (defun %use-run-program (command &rest keys
                           &key input output error-output ignore-error-status &allow-other-keys)
    ;; helper for RUN-PROGRAM when using %run-program
    #+(or abcl cormanlisp gcl (and lispworks os-windows) mcl xcl)
    (progn
      command keys input output error-output ignore-error-status ;; ignore
      (error "Not implemented on this platform"))
    (assert (not (member :stream (list input output error-output))))
    (let* ((active-input-p (%active-io-specifier-p input))
           (active-output-p (%active-io-specifier-p output))
           (active-error-output-p (%active-io-specifier-p error-output))
           (activity
             (cond
               (active-output-p :output)
               (active-input-p :input)
               (active-error-output-p :error-output)
               (t nil)))
           (wait (not activity))
           output-result error-output-result exit-code)
      (with-program-output ((reduced-output output-activity)
                            output :keys keys :setf output-result
                            :stream-easy-p t :active (eq activity :output))
        (with-program-error-output ((reduced-error-output error-output-activity)
                                    error-output :keys keys :setf error-output-result
                                    :stream-easy-p t :active (eq activity :error-output))
          (with-program-input ((reduced-input input-activity)
                               input :keys keys
                               :stream-easy-p t :active (eq activity :input))
            (let ((process-info
                    (apply '%run-program command
                           :wait wait :input reduced-input :output reduced-output
                           :error-output (if (eq error-output :output) :output reduced-error-output)
                           keys)))
              (labels ((get-stream (stream-name &optional fallbackp)
                         (or (getf process-info stream-name)
                             (when fallbackp
                               (getf process-info :bidir-stream))))
                       (run-activity (activity stream-name &optional fallbackp)
                         (if-let (stream (get-stream stream-name fallbackp))
                           (funcall activity stream)
                           (error 'subprocess-error
                                  :code `(:missing ,stream-name)
                                  :command command :process process-info))))
                (unwind-protect
                     (ecase activity
                       ((nil))
                       (:input (run-activity input-activity :input-stream t))
                       (:output (run-activity output-activity :output-stream t))
                       (:error-output (run-activity error-output-activity :error-output-stream)))
                  (loop :for (() val) :on process-info :by #'cddr
                        :when (streamp val) :do (ignore-errors (close val)))
                  (setf exit-code
                        (%check-result (%wait-process-result process-info)
                                       :command command :process process-info
                                       :ignore-error-status ignore-error-status))))))))
      (values output-result error-output-result exit-code)))

  (defun %normalize-system-command (command) ;; helper for %USE-SYSTEM
    (etypecase command
      (string command)
      (list (escape-shell-command
             (if (os-unix-p) (cons "exec" command) command)))))

  (defun %redirected-system-command (command in out err directory) ;; helper for %USE-SYSTEM
    (flet ((redirect (spec operator)
             (let ((pathname
                     (typecase spec
                       (null (null-device-pathname))
                       (string (parse-native-namestring spec))
                       (pathname spec)
                       ((eql :output)
                        (assert (equal operator " 2>"))
                        (return-from redirect '(" 2>&1"))))))
               (when pathname
                 (list operator " "
                       (escape-shell-token (native-namestring pathname)))))))
      (multiple-value-bind (before after)
          (let ((normalized (%normalize-system-command command)))
            (if (os-unix-p)
                (values '("exec") (list " ; " normalized))
                (values (list normalized) ())))
        (reduce/strcat
         (append
          before (redirect in " <") (redirect out " >") (redirect err " 2>")
          (when (and directory (os-unix-p)) ;; NB: unless on Unix, %system uses with-current-directory
            `(" ; cd " ,(escape-shell-token (native-namestring directory))))
          after)))))

  (defun %system (command &rest keys
                  &key input output error-output directory &allow-other-keys)
    "A portable abstraction of a low-level call to libc's system()."
    (declare (ignorable input output error-output directory keys))
    #+(or allegro clozure cmu (and lispworks os-unix) sbcl scl)
    (%wait-process-result
     (apply '%run-program (%normalize-system-command command) :wait t keys))
    #+(or abcl cormanlisp clisp ecl gcl (and lispworks os-windows) mkcl xcl)
    (let ((%command (%redirected-system-command command input output error-output directory)))
      #+(and lispworks os-windows)
      (system:call-system %command :current-directory directory :wait t)
      #+clisp
      (%wait-process-result
       (apply '%run-program %command :wait t
              :input :interactive :output :interactive :error-output :interactive keys))
      #-(or clisp (and lispworks os-windows))
      (with-current-directory ((unless (os-unix-p) directory))
        #+abcl (ext:run-shell-command %command)
        #+cormanlisp (win32:system %command)
        #+ecl (let ((*standard-input* *stdin*)
                    (*standard-output* *stdout*)
                    (*error-output* *stderr*))
                (ext:system %command))
        #+gcl (system:system %command)
        #+mcl (ccl::with-cstrs ((%%command %command)) (_system %%command))
        #+mkcl (mkcl:system %command)
        #+xcl (system:%run-shell-command %command))))

  (defun %use-system (command &rest keys
                      &key input output error-output ignore-error-status &allow-other-keys)
    ;; helper for RUN-PROGRAM when using %system
    (let (output-result error-output-result exit-code)
      (with-program-output ((reduced-output)
                            output :keys keys :setf output-result)
        (with-program-error-output ((reduced-error-output)
                                    error-output :keys keys :setf error-output-result)
          (with-program-input ((reduced-input) input :keys keys)
            (setf exit-code
                  (%check-result (apply '%system command
                                        :input reduced-input :output reduced-output
                                        :error-output reduced-error-output keys)
                                 :command command
                                 :ignore-error-status ignore-error-status)))))
      (values output-result error-output-result exit-code)))

  (defun run-program (command &rest keys
                       &key ignore-error-status force-shell
                         (input nil inputp) (if-input-does-not-exist :error)
                         output (if-output-exists :overwrite)
                         (error-output nil error-output-p) (if-error-output-exists :overwrite)
                         (element-type #-clozure *default-stream-element-type* #+clozure 'character)
                         (external-format *utf-8-external-format*)
                      &allow-other-keys)
    "Run program specified by COMMAND,
either a list of strings specifying a program and list of arguments,
or a string specifying a shell command (/bin/sh on Unix, CMD.EXE on Windows).

Always call a shell (rather than directly execute the command when possible)
if FORCE-SHELL is specified.

Signal a continuable SUBPROCESS-ERROR if the process wasn't successful (exit-code 0),
unless IGNORE-ERROR-STATUS is specified.

If OUTPUT is a pathname, a string designating a pathname, or NIL designating the null device,
the file at that path is used as output.
If it's :INTERACTIVE, output is inherited from the current process;
beware that this may be different from your *STANDARD-OUTPUT*,
and under SLIME will be on your *inferior-lisp* buffer.
If it's T, output goes to your current *STANDARD-OUTPUT* stream.
Otherwise, OUTPUT should be a value that is a suitable first argument to
SLURP-INPUT-STREAM (qv.), or a list of such a value and keyword arguments.
In this case, RUN-PROGRAM will create a temporary stream for the program output;
the program output, in that stream, will be processed by a call to SLURP-INPUT-STREAM,
using OUTPUT as the first argument (or the first element of OUTPUT, and the rest as keywords).
The primary value resulting from that call (or NIL if no call was needed)
will be the first value returned by RUN-PROGRAM.
E.g., using :OUTPUT :STRING will have it return the entire output stream as a string.
And using :OUTPUT '(:STRING :STRIPPED T) will have it return the same string
stripped of any ending newline.

ERROR-OUTPUT is similar to OUTPUT, except that the resulting value is returned
as the second value of RUN-PROGRAM. T designates the *ERROR-OUTPUT*.
Also :OUTPUT means redirecting the error output to the output stream,
in which case NIL is returned.

INPUT is similar to OUTPUT, except that VOMIT-OUTPUT-STREAM is used,
no value is returned, and T designates the *STANDARD-INPUT*.

Use ELEMENT-TYPE and EXTERNAL-FORMAT are passed on
to your Lisp implementation, when applicable, for creation of the output stream.

One and only one of the stream slurping or vomiting may or may not happen
in parallel in parallel with the subprocess,
depending on options and implementation,
and with priority being given to output processing.
Other streams are completely produced or consumed
before or after the subprocess is spawned, using temporary files.

RUN-PROGRAM returns 3 values:
0- the result of the OUTPUT slurping if any, or NIL
1- the result of the ERROR-OUTPUT slurping if any, or NIL
2- either 0 if the subprocess exited with success status,
or an indication of failure via the EXIT-CODE of the process"
    (declare (ignorable ignore-error-status))
    #-(or abcl allegro clisp clozure cmu cormanlisp ecl gcl lispworks mcl mkcl sbcl scl xcl)
    (error "RUN-PROGRAM not implemented for this Lisp")
    (flet ((default (x xp output) (cond (xp x) ((eq output :interactive) :interactive))))
      (apply (if (or force-shell
                     #+(or clisp ecl) (or (not ignore-error-status) t)
                     #+clisp (eq error-output :interactive)
                     #+(or abcl clisp) (eq :error-output :output)
                     #+(and lispworks os-unix) (%interactivep input output error-output)
                     #+(or abcl cormanlisp gcl (and lispworks os-windows) mcl xcl) t)
                 '%use-system '%use-run-program)
             command
             :input (default input inputp output)
             :error-output (default error-output error-output-p output)
             :if-input-does-not-exist if-input-does-not-exist
             :if-output-exists if-output-exists
             :if-error-output-exists if-error-output-exists
             :element-type element-type :external-format external-format
           keys))))
;;;; -------------------------------------------------------------------------
;;;; Support to build (compile and load) Lisp files

(uiop/package:define-package :uiop/lisp-build
  (:nicknames :asdf/lisp-build)
  (:recycle :uiop/lisp-build :asdf/lisp-build :asdf)
  (:use :uiop/common-lisp :uiop/package :uiop/utility
   :uiop/os :uiop/pathname :uiop/filesystem :uiop/stream :uiop/image)
  (:export
   ;; Variables
   #:*compile-file-warnings-behaviour* #:*compile-file-failure-behaviour*
   #:*output-translation-function*
   #:*optimization-settings* #:*previous-optimization-settings*
   #:*base-build-directory*
   #:compile-condition #:compile-file-error #:compile-warned-error #:compile-failed-error
   #:compile-warned-warning #:compile-failed-warning
   #:check-lisp-compile-results #:check-lisp-compile-warnings
   #:*uninteresting-conditions* #:*usual-uninteresting-conditions*
   #:*uninteresting-compiler-conditions* #:*uninteresting-loader-conditions*
   ;; Types
   #+sbcl #:sb-grovel-unknown-constant-condition
   ;; Functions & Macros
   #:get-optimization-settings #:proclaim-optimization-settings #:with-optimization-settings
   #:call-with-muffled-compiler-conditions #:with-muffled-compiler-conditions
   #:call-with-muffled-loader-conditions #:with-muffled-loader-conditions
   #:reify-simple-sexp #:unreify-simple-sexp
   #:reify-deferred-warnings #:unreify-deferred-warnings
   #:reset-deferred-warnings #:save-deferred-warnings #:check-deferred-warnings
   #:with-saved-deferred-warnings #:warnings-file-p #:warnings-file-type #:*warnings-file-type*
   #:enable-deferred-warnings-check #:disable-deferred-warnings-check
   #:current-lisp-file-pathname #:load-pathname
   #:lispize-pathname #:compile-file-type #:call-around-hook
   #:compile-file* #:compile-file-pathname* #:*compile-check*
   #:load* #:load-from-string #:combine-fasls)
  (:intern #:defaults #:failure-p #:warnings-p #:s #:y #:body))
(in-package :uiop/lisp-build)

(with-upgradability ()
  (defvar *compile-file-warnings-behaviour*
    (or #+clisp :ignore :warn)
    "How should ASDF react if it encounters a warning when compiling a file?
Valid values are :error, :warn, and :ignore.")

  (defvar *compile-file-failure-behaviour*
    (or #+(or mkcl sbcl) :error #+clisp :ignore :warn)
    "How should ASDF react if it encounters a failure (per the ANSI spec of COMPILE-FILE)
when compiling a file, which includes any non-style-warning warning.
Valid values are :error, :warn, and :ignore.
Note that ASDF ALWAYS raises an error if it fails to create an output file when compiling.")

  (defvar *base-build-directory* nil
    "When set to a non-null value, it should be an absolute directory pathname,
which will serve as the *DEFAULT-PATHNAME-DEFAULTS* around a COMPILE-FILE,
what more while the input-file is shortened if possible to ENOUGH-PATHNAME relative to it.
This can help you produce more deterministic output for FASLs."))

;;; Optimization settings
(with-upgradability ()
  (defvar *optimization-settings* nil
    "Optimization settings to be used by PROCLAIM-OPTIMIZATION-SETTINGS")
  (defvar *previous-optimization-settings* nil
    "Optimization settings saved by PROCLAIM-OPTIMIZATION-SETTINGS")
  (defparameter +optimization-variables+
    ;; TODO: allegro genera corman mcl
    (or #+(or abcl xcl) '(system::*speed* system::*space* system::*safety* system::*debug*)
        #+clisp '() ;; system::*optimize* is a constant hash-table! (with non-constant contents)
        #+clozure '(ccl::*nx-speed* ccl::*nx-space* ccl::*nx-safety*
                    ccl::*nx-debug* ccl::*nx-cspeed*)
        #+(or cmu scl) '(c::*default-cookie*)
        #+ecl (unless (use-ecl-byte-compiler-p) '(c::*speed* c::*space* c::*safety* c::*debug*))
        #+gcl '(compiler::*speed* compiler::*space* compiler::*compiler-new-safety* compiler::*debug*)
        #+lispworks '(compiler::*optimization-level*)
        #+mkcl '(si::*speed* si::*space* si::*safety* si::*debug*)
        #+sbcl '(sb-c::*policy*)))
  (defun get-optimization-settings ()
    "Get current compiler optimization settings, ready to PROCLAIM again"
    #-(or abcl allegro clisp clozure cmu ecl lispworks mkcl sbcl scl xcl)
    (warn "~S does not support ~S. Please help me fix that."
          'get-optimization-settings (implementation-type))
    #+(or abcl allegro clisp clozure cmu ecl lispworks mkcl sbcl scl xcl)
    (let ((settings '(speed space safety debug compilation-speed #+(or cmu scl) c::brevity)))
      #.`(loop #+(or allegro clozure)
               ,@'(:with info = #+allegro (sys:declaration-information 'optimize)
                   #+clozure (ccl:declaration-information 'optimize nil))
               :for x :in settings
               ,@(or #+(or abcl ecl gcl mkcl xcl) '(:for v :in +optimization-variables+))
               :for y = (or #+(or allegro clozure) (second (assoc x info)) ; normalize order
                            #+clisp (gethash x system::*optimize* 1)
                            #+(or abcl ecl mkcl xcl) (symbol-value v)
                            #+(or cmu scl) (slot-value c::*default-cookie*
                                                       (case x (compilation-speed 'c::cspeed)
                                                             (otherwise x)))
                            #+lispworks (slot-value compiler::*optimization-level* x)
                            #+sbcl (cdr (assoc x sb-c::*policy*)))
               :when y :collect (list x y))))
  (defun proclaim-optimization-settings ()
    "Proclaim the optimization settings in *OPTIMIZATION-SETTINGS*"
    (proclaim `(optimize ,@*optimization-settings*))
    (let ((settings (get-optimization-settings)))
      (unless (equal *previous-optimization-settings* settings)
        (setf *previous-optimization-settings* settings))))
  (defmacro with-optimization-settings ((&optional (settings *optimization-settings*)) &body body)
    #+(or allegro clisp)
    (let ((previous-settings (gensym "PREVIOUS-SETTINGS")))
      `(let ((,previous-settings (get-optimization-settings)))
         ,@(when settings `((proclaim `(optimize ,@,settings))))
         (unwind-protect (progn ,@body)
           (proclaim `(optimize ,@,previous-settings)))))
    #-(or allegro clisp)
    `(let ,(loop :for v :in +optimization-variables+ :collect `(,v ,v))
       ,@(when settings `((proclaim `(optimize ,@,settings))))
       ,@body)))


;;; Condition control
(with-upgradability ()
  #+sbcl
  (progn
    (defun sb-grovel-unknown-constant-condition-p (c)
      "Detect SB-GROVEL unknown-constant conditions on older versions of SBCL"
      (and (typep c 'sb-int:simple-style-warning)
           (string-enclosed-p
            "Couldn't grovel for "
            (simple-condition-format-control c)
            " (unknown to the C compiler).")))
    (deftype sb-grovel-unknown-constant-condition ()
      '(and style-warning (satisfies sb-grovel-unknown-constant-condition-p))))

  (defvar *usual-uninteresting-conditions*
    (append
     ;;#+clozure '(ccl:compiler-warning)
     #+cmu '("Deleting unreachable code.")
     #+lispworks '("~S being redefined in ~A (previously in ~A)."
                   "~S defined more than once in ~A.") ;; lispworks gets confused by eval-when.
     #+sbcl
     '(sb-c::simple-compiler-note
       "&OPTIONAL and &KEY found in the same lambda list: ~S"
       #+sb-eval sb-kernel:lexical-environment-too-complex
       sb-kernel:undefined-alien-style-warning
       sb-grovel-unknown-constant-condition ; defined above.
       sb-ext:implicit-generic-function-warning ;; Controversial.
       sb-int:package-at-variance
       sb-kernel:uninteresting-redefinition
       ;; BEWARE: the below four are controversial to include here.
       sb-kernel:redefinition-with-defun
       sb-kernel:redefinition-with-defgeneric
       sb-kernel:redefinition-with-defmethod
       sb-kernel::redefinition-with-defmacro) ; not exported by old SBCLs
     '("No generic function ~S present when encountering macroexpansion of defmethod. Assuming it will be an instance of standard-generic-function.")) ;; from closer2mop
    "A suggested value to which to set or bind *uninteresting-conditions*.")

  (defvar *uninteresting-conditions* '()
    "Conditions that may be skipped while compiling or loading Lisp code.")
  (defvar *uninteresting-compiler-conditions* '()
    "Additional conditions that may be skipped while compiling Lisp code.")
  (defvar *uninteresting-loader-conditions*
    (append
     '("Overwriting already existing readtable ~S." ;; from named-readtables
       #(#:finalizers-off-warning :asdf-finalizers)) ;; from asdf-finalizers
     #+clisp '(clos::simple-gf-replacing-method-warning))
    "Additional conditions that may be skipped while loading Lisp code."))

;;;; ----- Filtering conditions while building -----
(with-upgradability ()
  (defun call-with-muffled-compiler-conditions (thunk)
    "Call given THUNK in a context where uninteresting conditions and compiler conditions are muffled"
    (call-with-muffled-conditions
     thunk (append *uninteresting-conditions* *uninteresting-compiler-conditions*)))
  (defmacro with-muffled-compiler-conditions ((&optional) &body body)
    "Trivial syntax for CALL-WITH-MUFFLED-COMPILER-CONDITIONS"
    `(call-with-muffled-compiler-conditions #'(lambda () ,@body)))
  (defun call-with-muffled-loader-conditions (thunk)
    "Call given THUNK in a context where uninteresting conditions and loader conditions are muffled"
    (call-with-muffled-conditions
     thunk (append *uninteresting-conditions* *uninteresting-loader-conditions*)))
  (defmacro with-muffled-loader-conditions ((&optional) &body body)
    "Trivial syntax for CALL-WITH-MUFFLED-LOADER-CONDITIONS"
    `(call-with-muffled-loader-conditions #'(lambda () ,@body))))


;;;; Handle warnings and failures
(with-upgradability ()
  (define-condition compile-condition (condition)
    ((context-format
      :initform nil :reader compile-condition-context-format :initarg :context-format)
     (context-arguments
      :initform nil :reader compile-condition-context-arguments :initarg :context-arguments)
     (description
      :initform nil :reader compile-condition-description :initarg :description))
    (:report (lambda (c s)
               (format s (compatfmt "~@<~A~@[ while ~?~]~@:>")
                       (or (compile-condition-description c) (type-of c))
                       (compile-condition-context-format c)
                       (compile-condition-context-arguments c)))))
  (define-condition compile-file-error (compile-condition error) ())
  (define-condition compile-warned-warning (compile-condition warning) ())
  (define-condition compile-warned-error (compile-condition error) ())
  (define-condition compile-failed-warning (compile-condition warning) ())
  (define-condition compile-failed-error (compile-condition error) ())

  (defun check-lisp-compile-warnings (warnings-p failure-p
                                                  &optional context-format context-arguments)
    "Given the warnings or failures as resulted from COMPILE-FILE or checking deferred warnings,
raise an error or warning as appropriate"
    (when failure-p
      (case *compile-file-failure-behaviour*
        (:warn (warn 'compile-failed-warning
                     :description "Lisp compilation failed"
                     :context-format context-format
                     :context-arguments context-arguments))
        (:error (error 'compile-failed-error
                       :description "Lisp compilation failed"
                       :context-format context-format
                       :context-arguments context-arguments))
        (:ignore nil)))
    (when warnings-p
      (case *compile-file-warnings-behaviour*
        (:warn (warn 'compile-warned-warning
                     :description "Lisp compilation had style-warnings"
                     :context-format context-format
                     :context-arguments context-arguments))
        (:error (error 'compile-warned-error
                       :description "Lisp compilation had style-warnings"
                       :context-format context-format
                       :context-arguments context-arguments))
        (:ignore nil))))

  (defun check-lisp-compile-results (output warnings-p failure-p
                                             &optional context-format context-arguments)
    "Given the results of COMPILE-FILE, raise an error or warning as appropriate"
    (unless output
      (error 'compile-file-error :context-format context-format :context-arguments context-arguments))
    (check-lisp-compile-warnings warnings-p failure-p context-format context-arguments)))


;;;; Deferred-warnings treatment, originally implemented by Douglas Katzman.
;;;
;;; To support an implementation, three functions must be implemented:
;;; reify-deferred-warnings unreify-deferred-warnings reset-deferred-warnings
;;; See their respective docstrings.
(with-upgradability ()
  (defun reify-simple-sexp (sexp)
    "Given a simple SEXP, return a representation of it as a portable SEXP.
Simple means made of symbols, numbers, characters, simple-strings, pathnames, cons cells."
    (etypecase sexp
      (symbol (reify-symbol sexp))
      ((or number character simple-string pathname) sexp)
      (cons (cons (reify-simple-sexp (car sexp)) (reify-simple-sexp (cdr sexp))))
      (simple-vector (vector (mapcar 'reify-simple-sexp (coerce sexp 'list))))))

  (defun unreify-simple-sexp (sexp)
    "Given the portable output of REIFY-SIMPLE-SEXP, return the simple SEXP it represents"
    (etypecase sexp
      ((or symbol number character simple-string pathname) sexp)
      (cons (cons (unreify-simple-sexp (car sexp)) (unreify-simple-sexp (cdr sexp))))
      ((simple-vector 2) (unreify-symbol sexp))
      ((simple-vector 1) (coerce (mapcar 'unreify-simple-sexp (aref sexp 0)) 'vector))))

  #+clozure
  (progn
    (defun reify-source-note (source-note)
      (when source-note
        (with-accessors ((source ccl::source-note-source) (filename ccl:source-note-filename)
                         (start-pos ccl:source-note-start-pos) (end-pos ccl:source-note-end-pos)) source-note
          (declare (ignorable source))
          (list :filename filename :start-pos start-pos :end-pos end-pos
                #|:source (reify-source-note source)|#))))
    (defun unreify-source-note (source-note)
      (when source-note
        (destructuring-bind (&key filename start-pos end-pos source) source-note
          (ccl::make-source-note :filename filename :start-pos start-pos :end-pos end-pos
                                 :source (unreify-source-note source)))))
    (defun unsymbolify-function-name (name)
      (if-let (setfed (gethash name ccl::%setf-function-name-inverses%))
        `(setf ,setfed)
        name))
    (defun symbolify-function-name (name)
      (if (and (consp name) (eq (first name) 'setf))
          (let ((setfed (second name)))
            (gethash setfed ccl::%setf-function-names%))
          name))
    (defun reify-function-name (function-name)
      (let ((name (or (first function-name) ;; defun: extract the name
                      (let ((sec (second function-name)))
                        (or (and (atom sec) sec) ; scoped method: drop scope
                            (first sec)))))) ; method: keep gf name, drop method specializers
        (list name)))
    (defun unreify-function-name (function-name)
      function-name)
    (defun nullify-non-literals (sexp)
      (typecase sexp
        ((or number character simple-string symbol pathname) sexp)
        (cons (cons (nullify-non-literals (car sexp))
                    (nullify-non-literals (cdr sexp))))
        (t nil)))
    (defun reify-deferred-warning (deferred-warning)
      (with-accessors ((warning-type ccl::compiler-warning-warning-type)
                       (args ccl::compiler-warning-args)
                       (source-note ccl:compiler-warning-source-note)
                       (function-name ccl:compiler-warning-function-name)) deferred-warning
        (list :warning-type warning-type :function-name (reify-function-name function-name)
              :source-note (reify-source-note source-note)
              :args (destructuring-bind (fun &rest more)
                        args
                      (cons (unsymbolify-function-name fun)
                            (nullify-non-literals more))))))
    (defun unreify-deferred-warning (reified-deferred-warning)
      (destructuring-bind (&key warning-type function-name source-note args)
          reified-deferred-warning
        (make-condition (or (cdr (ccl::assq warning-type ccl::*compiler-whining-conditions*))
                            'ccl::compiler-warning)
                        :function-name (unreify-function-name function-name)
                        :source-note (unreify-source-note source-note)
                        :warning-type warning-type
                        :args (destructuring-bind (fun . more) args
                                (cons (symbolify-function-name fun) more))))))
  #+(or cmu scl)
  (defun reify-undefined-warning (warning)
    ;; Extracting undefined-warnings from the compilation-unit
    ;; To be passed through the above reify/unreify link, it must be a "simple-sexp"
    (list*
     (c::undefined-warning-kind warning)
     (c::undefined-warning-name warning)
     (c::undefined-warning-count warning)
     (mapcar
      #'(lambda (frob)
          ;; the lexenv slot can be ignored for reporting purposes
          `(:enclosing-source ,(c::compiler-error-context-enclosing-source frob)
            :source ,(c::compiler-error-context-source frob)
            :original-source ,(c::compiler-error-context-original-source frob)
            :context ,(c::compiler-error-context-context frob)
            :file-name ,(c::compiler-error-context-file-name frob) ; a pathname
            :file-position ,(c::compiler-error-context-file-position frob) ; an integer
            :original-source-path ,(c::compiler-error-context-original-source-path frob)))
      (c::undefined-warning-warnings warning))))

  #+sbcl
  (defun reify-undefined-warning (warning)
    ;; Extracting undefined-warnings from the compilation-unit
    ;; To be passed through the above reify/unreify link, it must be a "simple-sexp"
    (list*
     (sb-c::undefined-warning-kind warning)
     (sb-c::undefined-warning-name warning)
     (sb-c::undefined-warning-count warning)
     (mapcar
      #'(lambda (frob)
          ;; the lexenv slot can be ignored for reporting purposes
          `(:enclosing-source ,(sb-c::compiler-error-context-enclosing-source frob)
            :source ,(sb-c::compiler-error-context-source frob)
            :original-source ,(sb-c::compiler-error-context-original-source frob)
            :context ,(sb-c::compiler-error-context-context frob)
            :file-name ,(sb-c::compiler-error-context-file-name frob) ; a pathname
            :file-position ,(sb-c::compiler-error-context-file-position frob) ; an integer
            :original-source-path ,(sb-c::compiler-error-context-original-source-path frob)))
      (sb-c::undefined-warning-warnings warning))))

  (defun reify-deferred-warnings ()
    "return a portable S-expression, portably readable and writeable in any Common Lisp implementation
using READ within a WITH-SAFE-IO-SYNTAX, that represents the warnings currently deferred by
WITH-COMPILATION-UNIT. One of three functions required for deferred-warnings support in ASDF."
    #+allegro
    (list :functions-defined excl::.functions-defined.
          :functions-called excl::.functions-called.)
    #+clozure
    (mapcar 'reify-deferred-warning
            (if-let (dw ccl::*outstanding-deferred-warnings*)
              (let ((mdw (ccl::ensure-merged-deferred-warnings dw)))
                (ccl::deferred-warnings.warnings mdw))))
    #+(or cmu scl)
    (when lisp::*in-compilation-unit*
      ;; Try to send nothing through the pipe if nothing needs to be accumulated
      `(,@(when c::*undefined-warnings*
            `((c::*undefined-warnings*
               ,@(mapcar #'reify-undefined-warning c::*undefined-warnings*))))
        ,@(loop :for what :in '(c::*compiler-error-count*
                                c::*compiler-warning-count*
                                c::*compiler-note-count*)
                :for value = (symbol-value what)
                :when (plusp value)
                  :collect `(,what . ,value))))
    #+sbcl
    (when sb-c::*in-compilation-unit*
      ;; Try to send nothing through the pipe if nothing needs to be accumulated
      `(,@(when sb-c::*undefined-warnings*
            `((sb-c::*undefined-warnings*
               ,@(mapcar #'reify-undefined-warning sb-c::*undefined-warnings*))))
        ,@(loop :for what :in '(sb-c::*aborted-compilation-unit-count*
                                sb-c::*compiler-error-count*
                                sb-c::*compiler-warning-count*
                                sb-c::*compiler-style-warning-count*
                                sb-c::*compiler-note-count*)
                :for value = (symbol-value what)
                :when (plusp value)
                  :collect `(,what . ,value)))))

  (defun unreify-deferred-warnings (reified-deferred-warnings)
    "given a S-expression created by REIFY-DEFERRED-WARNINGS, reinstantiate the corresponding
deferred warnings as to be handled at the end of the current WITH-COMPILATION-UNIT.
Handle any warning that has been resolved already,
such as an undefined function that has been defined since.
One of three functions required for deferred-warnings support in ASDF."
    (declare (ignorable reified-deferred-warnings))
    #+allegro
    (destructuring-bind (&key functions-defined functions-called)
        reified-deferred-warnings
      (setf excl::.functions-defined.
            (append functions-defined excl::.functions-defined.)
            excl::.functions-called.
            (append functions-called excl::.functions-called.)))
    #+clozure
    (let ((dw (or ccl::*outstanding-deferred-warnings*
                  (setf ccl::*outstanding-deferred-warnings* (ccl::%defer-warnings t)))))
      (appendf (ccl::deferred-warnings.warnings dw)
               (mapcar 'unreify-deferred-warning reified-deferred-warnings)))
    #+(or cmu scl)
    (dolist (item reified-deferred-warnings)
      ;; Each item is (symbol . adjustment) where the adjustment depends on the symbol.
      ;; For *undefined-warnings*, the adjustment is a list of initargs.
      ;; For everything else, it's an integer.
      (destructuring-bind (symbol . adjustment) item
        (case symbol
          ((c::*undefined-warnings*)
           (setf c::*undefined-warnings*
                 (nconc (mapcan
                         #'(lambda (stuff)
                             (destructuring-bind (kind name count . rest) stuff
                               (unless (case kind (:function (fboundp name)))
                                 (list
                                  (c::make-undefined-warning
                                   :name name
                                   :kind kind
                                   :count count
                                   :warnings
                                   (mapcar #'(lambda (x)
                                               (apply #'c::make-compiler-error-context x))
                                           rest))))))
                         adjustment)
                        c::*undefined-warnings*)))
          (otherwise
           (set symbol (+ (symbol-value symbol) adjustment))))))
    #+sbcl
    (dolist (item reified-deferred-warnings)
      ;; Each item is (symbol . adjustment) where the adjustment depends on the symbol.
      ;; For *undefined-warnings*, the adjustment is a list of initargs.
      ;; For everything else, it's an integer.
      (destructuring-bind (symbol . adjustment) item
        (case symbol
          ((sb-c::*undefined-warnings*)
           (setf sb-c::*undefined-warnings*
                 (nconc (mapcan
                         #'(lambda (stuff)
                             (destructuring-bind (kind name count . rest) stuff
                               (unless (case kind (:function (fboundp name)))
                                 (list
                                  (sb-c::make-undefined-warning
                                   :name name
                                   :kind kind
                                   :count count
                                   :warnings
                                   (mapcar #'(lambda (x)
                                               (apply #'sb-c::make-compiler-error-context x))
                                           rest))))))
                         adjustment)
                        sb-c::*undefined-warnings*)))
          (otherwise
           (set symbol (+ (symbol-value symbol) adjustment)))))))

  (defun reset-deferred-warnings ()
    "Reset the set of deferred warnings to be handled at the end of the current WITH-COMPILATION-UNIT.
One of three functions required for deferred-warnings support in ASDF."
    #+allegro
    (setf excl::.functions-defined. nil
          excl::.functions-called. nil)
    #+clozure
    (if-let (dw ccl::*outstanding-deferred-warnings*)
      (let ((mdw (ccl::ensure-merged-deferred-warnings dw)))
        (setf (ccl::deferred-warnings.warnings mdw) nil)))
    #+(or cmu scl)
    (when lisp::*in-compilation-unit*
      (setf c::*undefined-warnings* nil
            c::*compiler-error-count* 0
            c::*compiler-warning-count* 0
            c::*compiler-note-count* 0))
    #+sbcl
    (when sb-c::*in-compilation-unit*
      (setf sb-c::*undefined-warnings* nil
            sb-c::*aborted-compilation-unit-count* 0
            sb-c::*compiler-error-count* 0
            sb-c::*compiler-warning-count* 0
            sb-c::*compiler-style-warning-count* 0
            sb-c::*compiler-note-count* 0)))

  (defun save-deferred-warnings (warnings-file)
    "Save forward reference conditions so they may be issued at a latter time,
possibly in a different process."
    (with-open-file (s warnings-file :direction :output :if-exists :supersede
                       :element-type *default-stream-element-type*
                       :external-format *utf-8-external-format*)
      (with-safe-io-syntax ()
        (write (reify-deferred-warnings) :stream s :pretty t :readably t)
        (terpri s))))

  (defun warnings-file-type (&optional implementation-type)
    "The pathname type for warnings files on given IMPLEMENTATION-TYPE,
where NIL designates the current one"
    (case (or implementation-type *implementation-type*)
      ((:acl :allegro) "allegro-warnings")
      ;;((:clisp) "clisp-warnings")
      ((:cmu :cmucl) "cmucl-warnings")
      ((:sbcl) "sbcl-warnings")
      ((:clozure :ccl) "ccl-warnings")
      ((:scl) "scl-warnings")))

  (defvar *warnings-file-type* nil
    "Pathname type for warnings files, or NIL if disabled")

  (defun enable-deferred-warnings-check ()
    "Enable the saving of deferred warnings"
    (setf *warnings-file-type* (warnings-file-type)))

  (defun disable-deferred-warnings-check ()
    "Disable the saving of deferred warnings"
    (setf *warnings-file-type* nil))

  (defun warnings-file-p (file &optional implementation-type)
    "Is FILE a saved warnings file for the given IMPLEMENTATION-TYPE?
If that given type is NIL, use the currently configured *WARNINGS-FILE-TYPE* instead."
    (if-let (type (if implementation-type
                      (warnings-file-type implementation-type)
                      *warnings-file-type*))
      (equal (pathname-type file) type)))

  (defun check-deferred-warnings (files &optional context-format context-arguments)
    "Given a list of FILES containing deferred warnings saved by CALL-WITH-SAVED-DEFERRED-WARNINGS,
re-intern and raise any warnings that are still meaningful."
    (let ((file-errors nil)
          (failure-p nil)
          (warnings-p nil))
      (handler-bind
          ((warning #'(lambda (c)
                        (setf warnings-p t)
                        (unless (typep c 'style-warning)
                          (setf failure-p t)))))
        (with-compilation-unit (:override t)
          (reset-deferred-warnings)
          (dolist (file files)
            (unreify-deferred-warnings
             (handler-case (safe-read-file-form file)
               (error (c)
                 ;;(delete-file-if-exists file) ;; deleting forces rebuild but prevents debugging
                 (push c file-errors)
                 nil))))))
      (dolist (error file-errors) (error error))
      (check-lisp-compile-warnings
       (or failure-p warnings-p) failure-p context-format context-arguments)))

  #|
  Mini-guide to adding support for deferred warnings on an implementation.

  First, look at what such a warning looks like:

  (describe
  (handler-case
  (and (eval '(lambda () (some-undefined-function))) nil)
  (t (c) c)))

  Then you can grep for the condition type in your compiler sources
  and see how to catch those that have been deferred,
  and/or read, clear and restore the deferred list.

  Also look at
  (macroexpand-1 '(with-compilation-unit () foo))
  |#

  (defun call-with-saved-deferred-warnings (thunk warnings-file &key source-namestring)
    "If WARNINGS-FILE is not nil, record the deferred-warnings around a call to THUNK
and save those warnings to the given file for latter use,
possibly in a different process. Otherwise just call THUNK."
    (declare (ignorable source-namestring))
    (if warnings-file
        (with-compilation-unit (:override t #+sbcl :source-namestring #+sbcl source-namestring)
          (unwind-protect
               (let (#+sbcl (sb-c::*undefined-warnings* nil))
                 (multiple-value-prog1
                     (funcall thunk)
                   (save-deferred-warnings warnings-file)))
            (reset-deferred-warnings)))
        (funcall thunk)))

  (defmacro with-saved-deferred-warnings ((warnings-file &key source-namestring) &body body)
    "Trivial syntax for CALL-WITH-SAVED-DEFERRED-WARNINGS"
    `(call-with-saved-deferred-warnings
      #'(lambda () ,@body) ,warnings-file :source-namestring ,source-namestring)))


;;; from ASDF
(with-upgradability ()
  (defun current-lisp-file-pathname ()
    "Portably return the PATHNAME of the current Lisp source file being compiled or loaded"
    (or *compile-file-pathname* *load-pathname*))

  (defun load-pathname ()
    "Portably return the LOAD-PATHNAME of the current source file or fasl"
    *load-pathname*) ;; magic no longer needed for GCL.

  (defun lispize-pathname (input-file)
    "From a INPUT-FILE pathname, return a corresponding .lisp source pathname"
    (make-pathname :type "lisp" :defaults input-file))

  (defun compile-file-type (&rest keys)
    "pathname TYPE for lisp FASt Loading files"
    (declare (ignorable keys))
    #-(or ecl mkcl) (load-time-value (pathname-type (compile-file-pathname "foo.lisp")))
    #+(or ecl mkcl) (pathname-type (apply 'compile-file-pathname "foo" keys)))

  (defun call-around-hook (hook function)
    "Call a HOOK around the execution of FUNCTION"
    (call-function (or hook 'funcall) function))

  (defun compile-file-pathname* (input-file &rest keys &key output-file &allow-other-keys)
    "Variant of COMPILE-FILE-PATHNAME that works well with COMPILE-FILE*"
    (let* ((keys
             (remove-plist-keys `(#+(or (and allegro (not (version>= 8 2)))) :external-format
                                    ,@(unless output-file '(:output-file))) keys)))
      (if (absolute-pathname-p output-file)
          ;; what cfp should be doing, w/ mp* instead of mp
          (let* ((type (pathname-type (apply 'compile-file-type keys)))
                 (defaults (make-pathname
                            :type type :defaults (merge-pathnames* input-file))))
            (merge-pathnames* output-file defaults))
          (funcall *output-translation-function*
                   (apply 'compile-file-pathname input-file keys)))))

  (defvar *compile-check* nil
    "A hook for user-defined compile-time invariants")

  (defun* (compile-file*) (input-file &rest keys
                                      &key (compile-check *compile-check*) output-file warnings-file
                                      #+clisp lib-file #+(or ecl mkcl) object-file #+sbcl emit-cfasl
                                      &allow-other-keys)
    "This function provides a portable wrapper around COMPILE-FILE.
It ensures that the OUTPUT-FILE value is only returned and
the file only actually created if the compilation was successful,
even though your implementation may not do that, and including
an optional call to an user-provided consistency check function COMPILE-CHECK;
it will call this function if not NIL at the end of the compilation
with the arguments sent to COMPILE-FILE*, except with :OUTPUT-FILE TMP-FILE
where TMP-FILE is the name of a temporary output-file.
It also checks two flags (with legacy british spelling from ASDF1),
*COMPILE-FILE-FAILURE-BEHAVIOUR* and *COMPILE-FILE-WARNINGS-BEHAVIOUR*
with appropriate implementation-dependent defaults,
and if a failure (respectively warnings) are reported by COMPILE-FILE
with consider it an error unless the respective behaviour flag
is one of :SUCCESS :WARN :IGNORE.
If WARNINGS-FILE is defined, deferred warnings are saved to that file.
On ECL or MKCL, it creates both the linkable object and loadable fasl files.
On implementations that erroneously do not recognize standard keyword arguments,
it will filter them appropriately."
    #+ecl (when (and object-file (equal (compile-file-type) (pathname object-file)))
            (format t "Whoa, some funky ASDF upgrade switched ~S calling convention for ~S and ~S~%"
                    'compile-file* output-file object-file)
            (rotatef output-file object-file))
    (let* ((keywords (remove-plist-keys
                      `(:output-file :compile-check :warnings-file
                                     #+clisp :lib-file #+(or ecl mkcl) :object-file) keys))
           (output-file
             (or output-file
                 (apply 'compile-file-pathname* input-file :output-file output-file keywords)))
           #+ecl
           (object-file
             (unless (use-ecl-byte-compiler-p)
               (or object-file
                   (compile-file-pathname output-file :type :object))))
           #+mkcl
           (object-file
             (or object-file
                 (compile-file-pathname output-file :fasl-p nil)))
           (tmp-file (tmpize-pathname output-file))
           #+sbcl
           (cfasl-file (etypecase emit-cfasl
                         (null nil)
                         ((eql t) (make-pathname :type "cfasl" :defaults output-file))
                         (string (parse-namestring emit-cfasl))
                         (pathname emit-cfasl)))
           #+sbcl
           (tmp-cfasl (when cfasl-file (make-pathname :type "cfasl" :defaults tmp-file)))
           #+clisp
           (tmp-lib (make-pathname :type "lib" :defaults tmp-file)))
      (multiple-value-bind (output-truename warnings-p failure-p)
          (with-enough-pathname (input-file :defaults *base-build-directory*)
            (with-saved-deferred-warnings (warnings-file :source-namestring (namestring input-file))
              (with-muffled-compiler-conditions ()
                (or #-(or ecl mkcl)
                    (apply 'compile-file input-file :output-file tmp-file
                           #+sbcl (if emit-cfasl (list* :emit-cfasl tmp-cfasl keywords) keywords)
                           #-sbcl keywords)
                    #+ecl (apply 'compile-file input-file :output-file
                                 (if object-file
                                     (list* object-file :system-p t keywords)
                                     (list* tmp-file keywords)))
                    #+mkcl (apply 'compile-file input-file
                                  :output-file object-file :fasl-p nil keywords)))))
        (cond
          ((and output-truename
                (flet ((check-flag (flag behaviour)
                         (or (not flag) (member behaviour '(:success :warn :ignore)))))
                  (and (check-flag failure-p *compile-file-failure-behaviour*)
                       (check-flag warnings-p *compile-file-warnings-behaviour*)))
                (progn
                  #+(or ecl mkcl)
                  (when (and #+ecl object-file)
                    (setf output-truename
                          (compiler::build-fasl
                           tmp-file #+ecl :lisp-files #+mkcl :lisp-object-files
                                    (list object-file))))
                  (or (not compile-check)
                      (apply compile-check input-file :output-file tmp-file keywords))))
           (delete-file-if-exists output-file)
           (when output-truename
             #+clisp (when lib-file (rename-file-overwriting-target tmp-lib lib-file))
             #+sbcl (when cfasl-file (rename-file-overwriting-target tmp-cfasl cfasl-file))
             (rename-file-overwriting-target output-truename output-file)
             (setf output-truename (truename output-file)))
           #+clisp (delete-file-if-exists tmp-lib))
          (t ;; error or failed check
           (delete-file-if-exists output-truename)
           #+clisp (delete-file-if-exists tmp-lib)
           #+sbcl (delete-file-if-exists tmp-cfasl)
           (setf output-truename nil)))
        (values output-truename warnings-p failure-p))))

  (defun load* (x &rest keys &key &allow-other-keys)
    "Portable wrapper around LOAD that properly handles loading from a stream."
    (with-muffled-loader-conditions ()
      (etypecase x
        ((or pathname string #-(or allegro clozure genera) stream #+clozure file-stream)
         (apply 'load x keys))
        ;; Genera can't load from a string-input-stream
        ;; ClozureCL 1.6 can only load from file input stream
        ;; Allegro 5, I don't remember but it must have been broken when I tested.
        #+(or allegro clozure genera)
        (stream ;; make do this way
         (let ((*package* *package*)
               (*readtable* *readtable*)
               (*load-pathname* nil)
               (*load-truename* nil))
           (eval-input x))))))

  (defun load-from-string (string)
    "Portably read and evaluate forms from a STRING."
    (with-input-from-string (s string) (load* s))))

;;; Links FASLs together
(with-upgradability ()
  (defun combine-fasls (inputs output)
    "Combine a list of FASLs INPUTS into a single FASL OUTPUT"
    #-(or abcl allegro clisp clozure cmu lispworks sbcl scl xcl)
    (error "~A does not support ~S~%inputs ~S~%output  ~S"
           (implementation-type) 'combine-fasls inputs output)
    #+abcl (funcall 'sys::concatenate-fasls inputs output) ; requires ABCL 1.2.0
    #+(or allegro clisp cmu sbcl scl xcl) (concatenate-files inputs output)
    #+clozure (ccl:fasl-concatenate output inputs :if-exists :supersede)
    #+lispworks
    (let (fasls)
      (unwind-protect
           (progn
             (loop :for i :in inputs
                   :for n :from 1
                   :for f = (add-pathname-suffix
                             output (format nil "-FASL~D" n))
                   :do (copy-file i f)
                       (push f fasls))
             (ignore-errors (lispworks:delete-system :fasls-to-concatenate))
             (eval `(scm:defsystem :fasls-to-concatenate
                      (:default-pathname ,(pathname-directory-pathname output))
                      :members
                      ,(loop :for f :in (reverse fasls)
                             :collect `(,(namestring f) :load-only t))))
             (scm:concatenate-system output :fasls-to-concatenate))
        (loop :for f :in fasls :do (ignore-errors (delete-file f)))
        (ignore-errors (lispworks:delete-system :fasls-to-concatenate))))))

;;;; ---------------------------------------------------------------------------
;;;; Generic support for configuration files

(uiop/package:define-package :uiop/configuration
  (:nicknames :asdf/configuration)
  (:recycle :uiop/configuration :asdf/configuration :asdf)
  (:use :uiop/common-lisp :uiop/utility
   :uiop/os :uiop/pathname :uiop/filesystem :uiop/stream :uiop/image :uiop/lisp-build)
  (:export
   #:get-folder-path
   #:user-configuration-directories #:system-configuration-directories
   #:in-first-directory
   #:in-user-configuration-directory #:in-system-configuration-directory
   #:validate-configuration-form #:validate-configuration-file #:validate-configuration-directory
   #:configuration-inheritance-directive-p
   #:report-invalid-form #:invalid-configuration #:*ignored-configuration-form* #:*user-cache*
   #:*clear-configuration-hook* #:clear-configuration #:register-clear-configuration-hook
   #:resolve-location #:location-designator-p #:location-function-p #:*here-directory*
   #:resolve-relative-location #:resolve-absolute-location #:upgrade-configuration))
(in-package :uiop/configuration)

(with-upgradability ()
  (define-condition invalid-configuration ()
    ((form :reader condition-form :initarg :form)
     (location :reader condition-location :initarg :location)
     (format :reader condition-format :initarg :format)
     (arguments :reader condition-arguments :initarg :arguments :initform nil))
    (:report (lambda (c s)
               (format s (compatfmt "~@<~? (will be skipped)~@:>")
                       (condition-format c)
                       (list* (condition-form c) (condition-location c)
                              (condition-arguments c))))))

  (defun get-folder-path (folder)
    "Semi-portable implementation of a subset of LispWorks' sys:get-folder-path,
this function tries to locate the Windows FOLDER for one of
:LOCAL-APPDATA, :APPDATA or :COMMON-APPDATA."
    (or #+(and lispworks mswindows) (sys:get-folder-path folder)
        ;; read-windows-registry HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders\AppData
        (ecase folder
          (:local-appdata (getenv-absolute-directory "LOCALAPPDATA"))
          (:appdata (getenv-absolute-directory "APPDATA"))
          (:common-appdata (or (getenv-absolute-directory "ALLUSERSAPPDATA")
                               (subpathname* (getenv-absolute-directory "ALLUSERSPROFILE") "Application Data/"))))))

  (defun user-configuration-directories ()
    "Determine user configuration directories"
    (let ((dirs
            `(,@(when (os-unix-p)
                  (cons
                   (subpathname* (getenv-absolute-directory "XDG_CONFIG_HOME") "common-lisp/")
                   (loop :for dir :in (getenv-absolute-directories "XDG_CONFIG_DIRS")
                         :collect (subpathname* dir "common-lisp/"))))
              ,@(when (os-windows-p)
                  `(,(subpathname* (get-folder-path :local-appdata) "common-lisp/config/")
                    ,(subpathname* (get-folder-path :appdata) "common-lisp/config/")))
              ,(subpathname (user-homedir-pathname) ".config/common-lisp/"))))
      (remove-duplicates (remove-if-not #'absolute-pathname-p dirs)
                         :from-end t :test 'equal)))

  (defun system-configuration-directories ()
    "Determine system user configuration directories"
    (cond
      ((os-unix-p) '(#p"/etc/common-lisp/"))
      ((os-windows-p)
       (if-let (it (subpathname* (get-folder-path :common-appdata) "common-lisp/config/"))
         (list it)))))

  (defun in-first-directory (dirs x &key (direction :input))
    "Determine system user configuration directories"
    (loop :with fun = (ecase direction
                        ((nil :input :probe) 'probe-file*)
                        ((:output :io) 'identity))
          :for dir :in dirs
          :thereis (and dir (funcall fun (subpathname (ensure-directory-pathname dir) x)))))

  (defun in-user-configuration-directory (x &key (direction :input))
    "return pathname under user configuration directory, subpathname X"
    (in-first-directory (user-configuration-directories) x :direction direction))
  (defun in-system-configuration-directory (x &key (direction :input))
    "return pathname under system configuration directory, subpathname X"
    (in-first-directory (system-configuration-directories) x :direction direction))

  (defun configuration-inheritance-directive-p (x)
    "Is X a configuration inheritance directive?"
    (let ((kw '(:inherit-configuration :ignore-inherited-configuration)))
      (or (member x kw)
          (and (length=n-p x 1) (member (car x) kw)))))

  (defun report-invalid-form (reporter &rest args)
    "Report an invalid form according to REPORTER and various ARGS"
    (etypecase reporter
      (null
       (apply 'error 'invalid-configuration args))
      (function
       (apply reporter args))
      ((or symbol string)
       (apply 'error reporter args))
      (cons
       (apply 'apply (append reporter args)))))

  (defvar *ignored-configuration-form* nil
    "Have configuration forms been ignored while parsing the configuration?")

  (defun validate-configuration-form (form tag directive-validator
                                            &key location invalid-form-reporter)
    "Validate a configuration FORM"
    (unless (and (consp form) (eq (car form) tag))
      (setf *ignored-configuration-form* t)
      (report-invalid-form invalid-form-reporter :form form :location location)
      (return-from validate-configuration-form nil))
    (loop :with inherit = 0 :with ignore-invalid-p = nil :with x = (list tag)
          :for directive :in (cdr form)
          :when (cond
                  ((configuration-inheritance-directive-p directive)
                   (incf inherit) t)
                  ((eq directive :ignore-invalid-entries)
                   (setf ignore-invalid-p t) t)
                  ((funcall directive-validator directive)
                   t)
                  (ignore-invalid-p
                   nil)
                  (t
                   (setf *ignored-configuration-form* t)
                   (report-invalid-form invalid-form-reporter :form directive :location location)
                   nil))
            :do (push directive x)
          :finally
             (unless (= inherit 1)
               (report-invalid-form invalid-form-reporter
                                    :form form :location location
                                    ;; we throw away the form and location arguments, hence the ~2*
                                    ;; this is necessary because of the report in INVALID-CONFIGURATION
                                    :format (compatfmt "~@<Invalid source registry ~S~@[ in ~S~]. ~
                                                        One and only one of ~S or ~S is required.~@:>")
                                    :arguments '(:inherit-configuration :ignore-inherited-configuration)))
             (return (nreverse x))))

  (defun validate-configuration-file (file validator &key description)
    "Validate a configuration file for conformance of its form with the validator function"
    (let ((forms (read-file-forms file)))
      (unless (length=n-p forms 1)
        (error (compatfmt "~@<One and only one form allowed for ~A. Got: ~3i~_~S~@:>~%")
               description forms))
      (funcall validator (car forms) :location file)))

  (defun validate-configuration-directory (directory tag validator &key invalid-form-reporter)
    "Map the VALIDATOR across the .conf files in DIRECTORY, the TAG will
be applied to the results to yield a configuration form.  Current
values of TAG include :source-registry and :output-translations."
    (let ((files (sort (ignore-errors
                        (remove-if
                         'hidden-pathname-p
                         (directory* (make-pathname :name *wild* :type "conf" :defaults directory))))
                       #'string< :key #'namestring)))
      `(,tag
        ,@(loop :for file :in files :append
                                    (loop :with ignore-invalid-p = nil
                                          :for form :in (read-file-forms file)
                                          :when (eq form :ignore-invalid-entries)
                                            :do (setf ignore-invalid-p t)
                                          :else
                                            :when (funcall validator form)
                                              :collect form
                                          :else
                                            :when ignore-invalid-p
                                              :do (setf *ignored-configuration-form* t)
                                          :else
                                            :do (report-invalid-form invalid-form-reporter :form form :location file)))
        :inherit-configuration)))

  (defun resolve-relative-location (x &key ensure-directory wilden)
    "Given a designator X for an relative location, resolve it to a pathname"
    (ensure-pathname
     (etypecase x
       (pathname x)
       (string (parse-unix-namestring
                x :ensure-directory ensure-directory))
       (cons
        (if (null (cdr x))
            (resolve-relative-location
             (car x) :ensure-directory ensure-directory :wilden wilden)
            (let* ((car (resolve-relative-location
                         (car x) :ensure-directory t :wilden nil)))
              (merge-pathnames*
               (resolve-relative-location
                (cdr x) :ensure-directory ensure-directory :wilden wilden)
               car))))
       ((eql :*/) *wild-directory*)
       ((eql :**/) *wild-inferiors*)
       ((eql :*.*.*) *wild-file*)
       ((eql :implementation)
        (parse-unix-namestring
         (implementation-identifier) :ensure-directory t))
       ((eql :implementation-type)
        (parse-unix-namestring
         (string-downcase (implementation-type)) :ensure-directory t))
       ((eql :hostname)
        (parse-unix-namestring (hostname) :ensure-directory t)))
     :wilden (and wilden (not (pathnamep x)) (not (member x '(:*/ :**/ :*.*.*))))
     :want-relative t))

  (defvar *here-directory* nil
    "This special variable is bound to the currect directory during calls to
PROCESS-SOURCE-REGISTRY in order that we be able to interpret the :here
directive.")

  (defvar *user-cache* nil
    "A specification as per RESOLVE-LOCATION of where the user keeps his FASL cache")

  (defun compute-user-cache ()
    "Compute the location of the default user-cache for translate-output objects"
    (setf *user-cache*
          (flet ((try (x &rest sub) (and x `(,x ,@sub))))
            (or
             (try (getenv-absolute-directory "XDG_CACHE_HOME") "common-lisp" :implementation)
             (when (os-windows-p)
               (try (or (get-folder-path :local-appdata)
                        (get-folder-path :appdata))
                    "common-lisp" "cache" :implementation))
             '(:home ".cache" "common-lisp" :implementation)))))
  (register-image-restore-hook 'compute-user-cache)

  (defun resolve-absolute-location (x &key ensure-directory wilden)
    "Given a designator X for an absolute location, resolve it to a pathname"
    (ensure-pathname
     (etypecase x
       (pathname x)
       (string
        (let ((p #-mcl (parse-namestring x)
                 #+mcl (probe-posix x)))
          #+mcl (unless p (error "POSIX pathname ~S does not exist" x))
          (if ensure-directory (ensure-directory-pathname p) p)))
       (cons
        (return-from resolve-absolute-location
          (if (null (cdr x))
              (resolve-absolute-location
               (car x) :ensure-directory ensure-directory :wilden wilden)
              (merge-pathnames*
               (resolve-relative-location
                (cdr x) :ensure-directory ensure-directory :wilden wilden)
               (resolve-absolute-location
                (car x) :ensure-directory t :wilden nil)))))
       ((eql :root)
        ;; special magic! we return a relative pathname,
        ;; but what it means to the output-translations is
        ;; "relative to the root of the source pathname's host and device".
        (return-from resolve-absolute-location
          (let ((p (make-pathname* :directory '(:relative))))
            (if wilden (wilden p) p))))
       ((eql :home) (user-homedir-pathname))
       ((eql :here) (resolve-absolute-location
                     (or *here-directory* (pathname-directory-pathname (load-pathname)))
                     :ensure-directory t :wilden nil))
       ((eql :user-cache) (resolve-absolute-location
                           *user-cache* :ensure-directory t :wilden nil)))
     :wilden (and wilden (not (pathnamep x)))
     :resolve-symlinks *resolve-symlinks*
     :want-absolute t))

  ;; Try to override declaration in previous versions of ASDF.
  (declaim (ftype (function (t &key (:directory boolean) (:wilden boolean)
                               (:ensure-directory boolean)) t) resolve-location))

  (defun* (resolve-location) (x &key ensure-directory wilden directory)
    "Resolve location designator X into a PATHNAME"
    ;; :directory backward compatibility, until 2014-01-16: accept directory as well as ensure-directory
    (loop* :with dirp = (or directory ensure-directory)
           :with (first . rest) = (if (atom x) (list x) x)
           :with path = (resolve-absolute-location
                         first :ensure-directory (and (or dirp rest) t)
                               :wilden (and wilden (null rest)))
           :for (element . morep) :on rest
           :for dir = (and (or morep dirp) t)
           :for wild = (and wilden (not morep))
           :for sub = (merge-pathnames*
                       (resolve-relative-location
                        element :ensure-directory dir :wilden wild)
                       path)
           :do (setf path (if (absolute-pathname-p sub) (resolve-symlinks* sub) sub))
           :finally (return path)))

  (defun location-designator-p (x)
    "Is X a designator for a location?"
    (flet ((absolute-component-p (c)
             (typep c '(or string pathname
                        (member :root :home :here :user-cache))))
           (relative-component-p (c)
             (typep c '(or string pathname
                        (member :*/ :**/ :*.*.* :implementation :implementation-type)))))
      (or (typep x 'boolean)
          (absolute-component-p x)
          (and (consp x) (absolute-component-p (first x)) (every #'relative-component-p (rest x))))))

  (defun location-function-p (x)
    "Is X the specification of a location function?"
    (and
     (length=n-p x 2)
     (eq (car x) :function)))

  (defvar *clear-configuration-hook* '())

  (defun register-clear-configuration-hook (hook-function &optional call-now-p)
    "Register a function to be called when clearing configuration"
    (register-hook-function '*clear-configuration-hook* hook-function call-now-p))

  (defun clear-configuration ()
    "Call the functions in *CLEAR-CONFIGURATION-HOOK*"
    (call-functions *clear-configuration-hook*))

  (register-image-dump-hook 'clear-configuration)

  (defun upgrade-configuration ()
    "If a previous version of ASDF failed to read some configuration, try again now."
    (when *ignored-configuration-form*
      (clear-configuration)
      (setf *ignored-configuration-form* nil))))


;;;; -------------------------------------------------------------------------
;;; Hacks for backward-compatibility of the driver

(uiop/package:define-package :uiop/backward-driver
  (:nicknames :asdf/backward-driver)
  (:recycle :uiop/backward-driver :asdf/backward-driver :asdf)
  (:use :uiop/common-lisp :uiop/package :uiop/utility
   :uiop/pathname :uiop/stream :uiop/os :uiop/image
   :uiop/run-program :uiop/lisp-build
   :uiop/configuration)
  (:export
   #:coerce-pathname #:component-name-to-pathname-components
   #+(or ecl mkcl) #:compile-file-keeping-object
   ))
(in-package :uiop/backward-driver)

;;;; Backward compatibility with various pathname functions.

(with-upgradability ()
  (defun coerce-pathname (name &key type defaults)
    ;; For backward-compatibility only, for people using internals
    ;; Reported users in quicklisp: hu.dwim.asdf, asdf-utils, xcvb
    ;; Will be removed after 2014-01-16.
    ;;(warn "Please don't use ASDF::COERCE-PATHNAME. Use ASDF/PATHNAME:PARSE-UNIX-NAMESTRING.")
    (parse-unix-namestring name :type type :defaults defaults))

  (defun component-name-to-pathname-components (unix-style-namestring
                                                 &key force-directory force-relative)
    ;; Will be removed after 2014-01-16.
    ;; (warn "Please don't use ASDF::COMPONENT-NAME-TO-PATHNAME-COMPONENTS, use SPLIT-UNIX-NAMESTRING-DIRECTORY-COMPONENTS")
    (multiple-value-bind (relabs path filename file-only)
        (split-unix-namestring-directory-components
         unix-style-namestring :ensure-directory force-directory)
      (declare (ignore file-only))
      (when (and force-relative (not (eq relabs :relative)))
        (error (compatfmt "~@<Absolute pathname designator not allowed: ~3i~_~S~@:>")
               unix-style-namestring))
      (values relabs path filename)))

  #+(or ecl mkcl)
  (defun compile-file-keeping-object (&rest args) (apply #'compile-file* args)))
;;;; ---------------------------------------------------------------------------
;;;; Re-export all the functionality in UIOP

(uiop/package:define-package :uiop/driver
  (:nicknames :uiop :asdf/driver :asdf-driver :asdf-utils)
  (:use :uiop/common-lisp)
   ;; NB: not reexporting uiop/common-lisp
   ;; which include all of CL with compatibility modifications on select platforms,
   ;; that could cause potential conflicts for packages that would :use (cl uiop)
   ;; or :use (closer-common-lisp uiop), etc.
  (:use-reexport
   :uiop/package :uiop/utility
   :uiop/os :uiop/pathname :uiop/stream :uiop/filesystem :uiop/image
   :uiop/run-program :uiop/lisp-build
   :uiop/configuration :uiop/backward-driver))

#+mkcl (provide :uiop)
;;;; -------------------------------------------------------------------------
;;;; Handle upgrade as forward- and backward-compatibly as possible
;; See https://bugs.launchpad.net/asdf/+bug/485687

(uiop/package:define-package :asdf/upgrade
  (:recycle :asdf/upgrade :asdf)
  (:use :uiop/common-lisp :uiop)
  (:export
   #:asdf-version #:*previous-asdf-versions* #:*asdf-version*
   #:asdf-message #:*verbose-out*
   #:upgrading-p #:when-upgrading #:upgrade-asdf #:asdf-upgrade-error #:defparameter*
   #:*post-upgrade-cleanup-hook* #:*post-upgrade-restart-hook* #:cleanup-upgraded-asdf
   ;; There will be no symbol left behind!
   #:intern*)
  (:import-from :uiop/package #:intern* #:find-symbol*))
(in-package :asdf/upgrade)

;;; Special magic to detect if this is an upgrade

(with-upgradability ()
  (defun asdf-version ()
    "Exported interface to the version of ASDF currently installed. A string.
You can compare this string with e.g.: (ASDF:VERSION-SATISFIES (ASDF:ASDF-VERSION) \"3.4.5.67\")."
    (when (find-package :asdf)
      (or (symbol-value (find-symbol (string :*asdf-version*) :asdf))
          (let* ((revsym (find-symbol (string :*asdf-revision*) :asdf))
                 (rev (and revsym (boundp revsym) (symbol-value revsym))))
            (etypecase rev
              (string rev)
              (cons (format nil "~{~D~^.~}" rev))
              (null "1.0"))))))
  ;; Important: define *p-a-v* /before/ *a-v* so that it initializes correctly.
  (defvar *previous-asdf-versions* (if-let (previous (asdf-version)) (list previous)))
  (defvar *asdf-version* nil)
  ;; We need to clear systems from versions yet older than the below:
  (defparameter *oldest-forward-compatible-asdf-version* "2.33") ;; 2.32.13 renames a slot in component.
  (defvar *verbose-out* nil)
  (defun asdf-message (format-string &rest format-args)
    (when *verbose-out* (apply 'format *verbose-out* format-string format-args)))
  (defvar *post-upgrade-cleanup-hook* ())
  (defvar *post-upgrade-restart-hook* ())
  (defun upgrading-p (&optional (oldest-compatible-version *oldest-forward-compatible-asdf-version*))
    (and *previous-asdf-versions*
         (version< (first *previous-asdf-versions*) oldest-compatible-version)))
  (defmacro defparameter* (var value &optional docstring (version *oldest-forward-compatible-asdf-version*))
    (let* ((name (string-trim "*" var))
           (valfun (intern (format nil "%~A-~A-~A" :compute name :value))))
      `(progn
         (defun ,valfun () ,value)
         (defvar ,var (,valfun) ,@(ensure-list docstring))
         (when (upgrading-p ,version)
           (setf ,var (,valfun))))))
  (defmacro when-upgrading ((&key (version *oldest-forward-compatible-asdf-version*)
                               (upgrading-p `(upgrading-p ,version)) when) &body body)
    "A wrapper macro for code that should only be run when upgrading a
previously-loaded version of ASDF."
    `(with-upgradability ()
       (when (and ,upgrading-p ,@(when when `(,when)))
         (handler-bind ((style-warning #'muffle-warning))
           (eval '(progn ,@body))))))
  (let* (;; For bug reporting sanity, please always bump this version when you modify this file.
         ;; Please also modify asdf.asd to reflect this change. make bump-version v=3.4.5.67.8
         ;; can help you do these changes in synch (look at the source for documentation).
         ;; Relying on its automation, the version is now redundantly present on top of asdf.lisp.
         ;; "3.4" would be the general branch for major version 3, minor version 4.
         ;; "3.4.5" would be an official release in the 3.4 branch.
         ;; "3.4.5.67" would be a development version in the official branch, on top of 3.4.5.
         ;; "3.4.5.0.8" would be your eighth local modification of official release 3.4.5
         ;; "3.4.5.67.8" would be your eighth local modification of development version 3.4.5.67
         (asdf-version "3.1.2")
         (existing-version (asdf-version)))
    (setf *asdf-version* asdf-version)
    (when (and existing-version (not (equal asdf-version existing-version)))
      (push existing-version *previous-asdf-versions*)
      (when (or *verbose-out* *load-verbose*)
        (format (or *verbose-out* *trace-output*)
                (compatfmt "~&~@<; ~@;Upgrading ASDF ~@[from version ~A ~]to version ~A~@:>~%")
                existing-version asdf-version)))))

(when-upgrading ()
  (let ((redefined-functions ;; gf signature and/or semantics changed incompatibly. Oops.
	  ;; NB: it's too late to do anything about functions in UIOP!
	  ;; If you introduce some critically incompatibility there, you must change name.
          '(#:component-relative-pathname #:component-parent-pathname ;; component
            #:source-file-type
            #:find-system #:system-source-file #:system-relative-pathname ;; system
	    #:find-component ;; find-component
	    #:explain #:perform #:perform-with-restarts #:input-files #:output-files ;; action
	    #:component-depends-on #:operation-done-p #:component-depends-on
	    #:traverse ;; backward-interface
            #:map-direct-dependencies #:reduce-direct-dependencies #:direct-dependencies ;; plan
	    #:operate  ;; operate
	    #:parse-component-form ;; defsystem
	    #:apply-output-translations ;; output-translations
	    #:process-output-translations-directive
	    #:inherit-source-registry #:process-source-registry ;; source-registry
	    #:process-source-registry-directive
	    #:trivial-system-p)) ;; bundle
	(redefined-classes
          ;; redefining the classes causes interim circularities
	  ;; with the old ASDF during upgrade, and many implementations bork
          '((#:compile-concatenated-source-op (#:operation) ()))))
    (loop :for name :in redefined-functions
          :for sym = (find-symbol* name :asdf nil) :do
            (when sym
              ;; On CLISP we seem to be unable to fmakunbound and define a function in the same fasl. Sigh.
              #-clisp (fmakunbound sym)))
    (labels ((asym (x) (multiple-value-bind (s p) (if (consp x) (values (car x) (cadr x)) (values x :asdf))
			 (find-symbol* s p nil)))
	     (asyms (l) (mapcar #'asym l)))
      (loop* :for (name superclasses slots) :in redefined-classes
	     :for sym = (find-symbol* name :asdf nil)
	     :when (and sym (find-class sym))
	     :do (eval `(defclass ,sym ,(asyms superclasses) ,(asyms slots)))))))


;;; Self-upgrade functions

(with-upgradability ()
  (defun asdf-upgrade-error ()
    ;; Important notice for whom it concerns. The crux of the matter is that
    ;; TRAVERSE can be completely refactored, and so after the find-system returns, it's too late.
    (error "When a system transitively depends on ASDF, it must :defsystem-depends-on (:asdf)~%~
          Otherwise, when you upgrade from ASDF 2, you must do it before you operate on any system.~%"))

  (defun cleanup-upgraded-asdf (&optional (old-version (first *previous-asdf-versions*)))
    (let ((new-version (asdf-version)))
      (unless (equal old-version new-version)
        (push new-version *previous-asdf-versions*)
        (when old-version
          (if (version<= new-version old-version)
              (error (compatfmt "~&~@<; ~@;Downgraded ASDF from version ~A to version ~A~@:>~%")
                     old-version new-version)
              (asdf-message (compatfmt "~&~@<; ~@;Upgraded ASDF from version ~A to version ~A~@:>~%")
                            old-version new-version))
          ;; In case the previous version was too old to be forward-compatible, clear systems.
          ;; TODO: if needed, we may have to define a separate hook to run
          ;; in case of forward-compatible upgrade.
          ;; Or to move the tests forward-compatibility test inside each hook function?
          (unless (version<= *oldest-forward-compatible-asdf-version* old-version)
            (call-functions (reverse *post-upgrade-cleanup-hook*)))
          t))))

  (defun upgrade-asdf ()
    "Try to upgrade of ASDF. If a different version was used, return T.
   We need do that before we operate on anything that may possibly depend on ASDF."
    (let ((*load-print* nil)
          (*compile-print* nil))
      (handler-bind (((or style-warning) #'muffle-warning))
        (symbol-call :asdf :load-system :asdf :verbose nil))))

  (register-hook-function '*post-upgrade-cleanup-hook* 'upgrade-configuration))

;;;; -------------------------------------------------------------------------
;;;; Components

(uiop/package:define-package :asdf/component
  (:recycle :asdf/component :asdf/defsystem :asdf/find-system :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade)
  (:export
   #:component #:component-find-path
   #:component-name #:component-pathname #:component-relative-pathname
   #:component-parent #:component-system #:component-parent-pathname
   #:child-component #:parent-component #:module
   #:file-component
   #:source-file #:c-source-file #:java-source-file
   #:static-file #:doc-file #:html-file
   #:file-type
   #:source-file-type #:source-file-explicit-type ;; backward-compatibility
   #:component-in-order-to #:component-sideway-dependencies
   #:component-if-feature #:around-compile-hook
   #:component-description #:component-long-description
   #:component-version #:version-satisfies
   #:component-inline-methods ;; backward-compatibility only. DO NOT USE!
   #:component-operation-times ;; For internal use only.
   ;; portable ASDF encoding and implementation-specific external-format
   #:component-external-format #:component-encoding
   #:component-children-by-name #:component-children #:compute-children-by-name
   #:component-build-operation
   #:module-default-component-class
   #:module-components ;; backward-compatibility. DO NOT USE.
   #:sub-components

   ;; conditions
   #:system-definition-error ;; top level, moved here because this is the earliest place for it.
   #:duplicate-names

   ;; Internals we'd like to share with the ASDF package, especially for upgrade purposes
   #:name #:version #:description #:long-description #:author #:maintainer #:licence
   #:components-by-name #:components #:children #:children-by-name
   #:default-component-class #:source-file
   #:defsystem-depends-on ; This symbol retained for backward compatibility.
   #:sideway-dependencies #:if-feature #:in-order-to #:inline-methods
   #:relative-pathname #:absolute-pathname #:operation-times #:around-compile
   #:%encoding #:properties #:component-properties #:parent))
(in-package :asdf/component)

(with-upgradability ()
  (defgeneric component-name (component)
    (:documentation "Name of the COMPONENT, unique relative to its parent"))
  (defgeneric component-system (component)
    (:documentation "Find the top-level system containing COMPONENT"))
  (defgeneric component-pathname (component)
    (:documentation "Extracts the pathname applicable for a particular component."))
  (defgeneric (component-relative-pathname) (component)
    (:documentation "Returns a pathname for the component argument intended to be
interpreted relative to the pathname of that component's parent.
Despite the function's name, the return value may be an absolute
pathname, because an absolute pathname may be interpreted relative to
another pathname in a degenerate way."))
  (defgeneric component-external-format (component))
  (defgeneric component-encoding (component))
  (defgeneric version-satisfies (component version))
  (defgeneric component-version (component))
  (defgeneric (setf component-version) (new-version component))
  (defgeneric component-parent (component))
  (defmethod component-parent ((component null)) nil)

  ;; Backward compatible way of computing the FILE-TYPE of a component.
  ;; TODO: find users, have them stop using that, remove it for ASDF4.
  (defgeneric (source-file-type) (component system))

  (define-condition system-definition-error (error) ()
    ;; [this use of :report should be redundant, but unfortunately it's not.
    ;; cmucl's lisp::output-instance prefers the kernel:slot-class-print-function
    ;; over print-object; this is always conditions::%print-condition for
    ;; condition objects, which in turn does inheritance of :report options at
    ;; run-time.  fortunately, inheritance means we only need this kludge here in
    ;; order to fix all conditions that build on it.  -- rgr, 28-Jul-02.]
    #+cmu (:report print-object))

  (define-condition duplicate-names (system-definition-error)
    ((name :initarg :name :reader duplicate-names-name))
    (:report (lambda (c s)
               (format s (compatfmt "~@<Error while defining system: multiple components are given same name ~S~@:>")
                       (duplicate-names-name c))))))


(with-upgradability ()
  (defclass component ()
    ((name :accessor component-name :initarg :name :type string :documentation
           "Component name: designator for a string composed of portable pathname characters")
     ;; We might want to constrain version with
     ;; :type (and string (satisfies parse-version))
     ;; but we cannot until we fix all systems that don't use it correctly!
     (version :accessor component-version :initarg :version :initform nil)
     (description :accessor component-description :initarg :description :initform nil)
     (long-description :accessor component-long-description :initarg :long-description :initform nil)
     (sideway-dependencies :accessor component-sideway-dependencies :initform nil)
     (if-feature :accessor component-if-feature :initform nil :initarg :if-feature)
     ;; In the ASDF object model, dependencies exist between *actions*,
     ;; where an action is a pair of an operation and a component.
     ;; Dependencies are represented as alists of operations
     ;; to a list where each entry is a pair of an operation and a list of component specifiers.
     ;; Up until ASDF 2.26.9, there used to be two kinds of dependencies:
     ;; in-order-to and do-first, each stored in its own slot. Now there is only in-order-to.
     ;; in-order-to used to represent things that modify the filesystem (such as compiling a fasl)
     ;; and do-first things that modify the current image (such as loading a fasl).
     ;; These are now unified because we now correctly propagate timestamps between dependencies.
     ;; Happily, no one seems to have used do-first too much (especially since until ASDF 2.017,
     ;; anything you specified was overridden by ASDF itself anyway), but the name in-order-to remains.
     ;; The names are bad, but they have been the official API since Dan Barlow's ASDF 1.52!
     ;; LispWorks's defsystem has caused-by and requires for in-order-to and do-first respectively.
     ;; Maybe rename the slots in ASDF? But that's not very backward-compatible.
     ;; See our ASDF 2 paper for more complete explanations.
     (in-order-to :initform nil :initarg :in-order-to
                  :accessor component-in-order-to)
     ;; methods defined using the "inline" style inside a defsystem form:
     ;; need to store them somewhere so we can delete them when the system
     ;; is re-evaluated.
     (inline-methods :accessor component-inline-methods :initform nil) ;; OBSOLETE! DELETE THIS IF NO ONE USES.
     ;; ASDF4: rename it from relative-pathname to specified-pathname. It need not be relative.
     ;; There is no initform and no direct accessor for this specified pathname,
     ;; so we only access the information through appropriate methods, after it has been processed.
     ;; Unhappily, some braindead systems directly access the slot. Make them stop before ASDF4.
     (relative-pathname :initarg :pathname)
     ;; The absolute-pathname is computed based on relative-pathname and parent pathname.
     ;; The slot is but a cache used by component-pathname.
     (absolute-pathname)
     (operation-times :initform (make-hash-table)
                      :accessor component-operation-times)
     (around-compile :initarg :around-compile)
     ;; Properties are for backward-compatibility with ASDF2 only. DO NOT USE!
     (properties :accessor component-properties :initarg :properties
                 :initform nil)
     (%encoding :accessor %component-encoding :initform nil :initarg :encoding)
     ;; For backward-compatibility, this slot is part of component rather than of child-component. ASDF4: stop it.
     (parent :initarg :parent :initform nil :reader component-parent)
     (build-operation
      :initarg :build-operation :initform nil :reader component-build-operation)))

  (defun component-find-path (component)
    "Return a path from a root system to the COMPONENT.
The return value is a list of component NAMES; a list of strings."
    (check-type component (or null component))
    (reverse
     (loop :for c = component :then (component-parent c)
           :while c :collect (component-name c))))

  (defmethod print-object ((c component) stream)
    (print-unreadable-object (c stream :type t :identity nil)
      (format stream "~{~S~^ ~}" (component-find-path c))))

  (defmethod component-system ((component component))
    (if-let (system (component-parent component))
      (component-system system)
      component)))


;;;; Component hierarchy within a system
;; The tree typically but not necessarily follows the filesystem hierarchy.
(with-upgradability ()
  (defclass child-component (component) ()
    (:documentation "A CHILD-COMPONENT is a component that may be part of
a PARENT-COMPONENT."))

  (defclass file-component (child-component)
    ((type :accessor file-type :initarg :type))) ; no default
  (defclass source-file (file-component)
    ((type :accessor source-file-explicit-type ;; backward-compatibility
           :initform nil))) ;; NB: many systems have come to rely on this default.
  (defclass c-source-file (source-file)
    ((type :initform "c")))
  (defclass java-source-file (source-file)
    ((type :initform "java")))
  (defclass static-file (source-file)
    ((type :initform nil)))
  (defclass doc-file (static-file) ())
  (defclass html-file (doc-file)
    ((type :initform "html")))

  (defclass parent-component (component)
    ((children
      :initform nil
      :initarg :components
      :reader module-components ; backward-compatibility
      :accessor component-children)
     (children-by-name
      :reader module-components-by-name ; backward-compatibility
      :accessor component-children-by-name)
     (default-component-class
      :initform nil
      :initarg :default-component-class
      :accessor module-default-component-class))
  (:documentation "A PARENT-COMPONENT is a component that may have
children.")))

(with-upgradability ()
  (defun compute-children-by-name (parent &key only-if-needed-p)
    (unless (and only-if-needed-p (slot-boundp parent 'children-by-name))
      (let ((hash (make-hash-table :test 'equal)))
        (setf (component-children-by-name parent) hash)
        (loop :for c :in (component-children parent)
              :for name = (component-name c)
              :for previous = (gethash name hash)
              :do (when previous (error 'duplicate-names :name name))
                  (setf (gethash name hash) c))
        hash))))

(with-upgradability ()
  (defclass module (child-component parent-component)
    (#+clisp (components)))) ;; backward compatibility during upgrade only


;;;; component pathnames
(with-upgradability ()
  (defgeneric* (component-parent-pathname) (component))
  (defmethod component-parent-pathname (component)
    (component-pathname (component-parent component)))

  (defmethod component-pathname ((component component))
    (if (slot-boundp component 'absolute-pathname)
        (slot-value component 'absolute-pathname)
        (let ((pathname
                (merge-pathnames*
                 (component-relative-pathname component)
                 (pathname-directory-pathname (component-parent-pathname component)))))
          (unless (or (null pathname) (absolute-pathname-p pathname))
            (error (compatfmt "~@<Invalid relative pathname ~S for component ~S~@:>")
                   pathname (component-find-path component)))
          (setf (slot-value component 'absolute-pathname) pathname)
          pathname)))

  (defmethod component-relative-pathname ((component component))
    ;; SOURCE-FILE-TYPE below is strictly for backward-compatibility with ASDF1.
    ;; We ought to be able to extract this from the component alone with COMPONENT-TYPE.
    ;; TODO: track who uses it, and have them not use it anymore;
    ;; maybe issue a WARNING (then eventually CERROR) if the two methods diverge?
    (parse-unix-namestring
     (or (and (slot-boundp component 'relative-pathname)
              (slot-value component 'relative-pathname))
         (component-name component))
     :want-relative t
     :type (source-file-type component (component-system component))
     :defaults (component-parent-pathname component)))

  (defmethod source-file-type ((component parent-component) (system parent-component))
    :directory)

  (defmethod source-file-type ((component file-component) (system parent-component))
    (file-type component)))


;;;; Encodings
(with-upgradability ()
  (defmethod component-encoding ((c component))
    (or (loop :for x = c :then (component-parent x)
              :while x :thereis (%component-encoding x))
        (detect-encoding (component-pathname c))))

  (defmethod component-external-format ((c component))
    (encoding-external-format (component-encoding c))))


;;;; around-compile-hook
(with-upgradability ()
  (defgeneric around-compile-hook (component))
  (defmethod around-compile-hook ((c component))
    (cond
      ((slot-boundp c 'around-compile)
       (slot-value c 'around-compile))
      ((component-parent c)
       (around-compile-hook (component-parent c))))))


;;;; version-satisfies
(with-upgradability ()
  ;; short-circuit testing of null version specifications.
  ;; this is an all-pass, without warning
  (defmethod version-satisfies :around ((c t) (version null))
    t)
  (defmethod version-satisfies ((c component) version)
    (unless (and version (slot-boundp c 'version) (component-version c))
      (when version
        (warn "Requested version ~S but ~S has no version" version c))
      (return-from version-satisfies nil))
    (version-satisfies (component-version c) version))

  (defmethod version-satisfies ((cver string) version)
    (version<= version cver)))


;;; all sub-components (of a given type)
(with-upgradability ()
  (defun sub-components (component &key (type t))
    (while-collecting (c)
      (labels ((recurse (x)
                 (when (if-let (it (component-if-feature x)) (featurep it) t)
                   (when (typep x type)
                     (c x))
                   (when (typep x 'parent-component)
                     (map () #'recurse (component-children x))))))
        (recurse component)))))

;;;; -------------------------------------------------------------------------
;;;; Systems

(uiop/package:define-package :asdf/system
  (:recycle :asdf :asdf/system)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/component)
  (:export
   #:system #:proto-system
   #:system-source-file #:system-source-directory #:system-relative-pathname
   #:reset-system
   #:system-description #:system-long-description
   #:system-author #:system-maintainer #:system-licence #:system-license
   #:system-defsystem-depends-on #:system-depends-on #:system-weakly-depends-on
   #:component-build-pathname #:build-pathname
   #:component-entry-point #:entry-point
   #:homepage #:system-homepage
   #:bug-tracker #:system-bug-tracker
   #:mailto #:system-mailto
   #:long-name #:system-long-name
   #:source-control #:system-source-control
   #:find-system #:builtin-system-p)) ;; forward-reference, defined in find-system
(in-package :asdf/system)

(with-upgradability ()
  (defgeneric* (find-system) (system &optional error-p))
  (defgeneric* (system-source-file :supersede #-clisp t #+clisp nil) (system)
    (:documentation "Return the source file in which system is defined."))
  (defgeneric component-build-pathname (component))

  (defgeneric component-entry-point (component))
  (defmethod component-entry-point ((c component))
    nil))


;;;; The system class

(with-upgradability ()
  (defclass proto-system () ; slots to keep when resetting a system
    ;; To preserve identity for all objects, we'd need keep the components slots
    ;; but also to modify parse-component-form to reset the recycled objects.
    ((name) (source-file) #|(children) (children-by-names)|#))

  (defclass system (module proto-system)
    ;; Backward-compatibility: inherit from module. ASDF4: only inherit from parent-component.
    (;; {,long-}description is now inherited from component, but we add the legacy accessors
     (description :accessor system-description)
     (long-description :accessor system-long-description)
     (author :accessor system-author :initarg :author :initform nil)
     (maintainer :accessor system-maintainer :initarg :maintainer :initform nil)
     (licence :accessor system-licence :initarg :licence
              :accessor system-license :initarg :license :initform nil)
     (homepage :accessor system-homepage :initarg :homepage :initform nil)
     (bug-tracker :accessor system-bug-tracker :initarg :bug-tracker :initform nil)
     (mailto :accessor system-mailto :initarg :mailto :initform nil)
     (long-name :accessor system-long-name :initarg :long-name :initform nil)
     ;; Conventions for this slot aren't clear yet as of ASDF 2.27, but whenever they are, they will be enforced.
     ;; I'm introducing the slot before the conventions are set for maximum compatibility.
     (source-control :accessor system-source-control :initarg :source-control :initform nil)
     (builtin-system-p :accessor builtin-system-p :initform nil :initarg :builtin-system-p)
     (build-pathname
      :initform nil :initarg :build-pathname :accessor component-build-pathname)
     (entry-point
      :initform nil :initarg :entry-point :accessor component-entry-point)
     (source-file :initform nil :initarg :source-file :accessor system-source-file)
     (defsystem-depends-on :reader system-defsystem-depends-on :initarg :defsystem-depends-on
                           :initform nil)
     ;; these two are specially set in parse-component-form, so have no :INITARGs.
     (depends-on :reader system-depends-on :initform nil)
     (weakly-depends-on :reader system-weakly-depends-on :initform nil)))

  (defun reset-system (system &rest keys &key &allow-other-keys)
    (change-class (change-class system 'proto-system) 'system)
    (apply 'reinitialize-instance system keys)))


;;;; Pathnames

(with-upgradability ()
  (defmethod system-source-file ((system-name string))
    (system-source-file (find-system system-name)))
  (defmethod system-source-file ((system-name symbol))
    (system-source-file (find-system system-name)))

  (defun system-source-directory (system-designator)
    "Return a pathname object corresponding to the directory
in which the system specification (.asd file) is located."
    (pathname-directory-pathname (system-source-file system-designator)))

  (defun (system-relative-pathname) (system name &key type)
    (subpathname (system-source-directory system) name :type type))

  (defmethod component-pathname ((system system))
    (let ((pathname (or (call-next-method) (system-source-directory system))))
      (unless (and (slot-boundp system 'relative-pathname) ;; backward-compatibility with ASDF1-age
                   (slot-value system 'relative-pathname)) ;; systems that directly access this slot.
        (setf (slot-value system 'relative-pathname) pathname))
      pathname))

  (defmethod component-relative-pathname ((system system))
    (parse-unix-namestring
     (and (slot-boundp system 'relative-pathname)
          (slot-value system 'relative-pathname))
     :want-relative t
     :type :directory
     :ensure-absolute t
     :defaults (system-source-directory system)))

  (defmethod component-parent-pathname ((system system))
    (system-source-directory system))

  (defmethod component-build-pathname ((c component))
    nil))

;;;; -------------------------------------------------------------------------
;;;; Stamp cache

(uiop/package:define-package :asdf/cache
  (:use :uiop/common-lisp :uiop :asdf/upgrade)
  (:export #:get-file-stamp #:compute-file-stamp #:register-file-stamp
           #:set-asdf-cache-entry #:unset-asdf-cache-entry #:consult-asdf-cache
	   #:do-asdf-cache #:normalize-namestring
           #:call-with-asdf-cache #:with-asdf-cache #:*asdf-cache*))
(in-package :asdf/cache)

;;; This stamp cache is useful for:
;; * consistency of stamps used within a single run
;; * fewer accesses to the filesystem
;; * the ability to test with fake timestamps, without touching files

(with-upgradability ()
  (defvar *asdf-cache* nil)

  (defun set-asdf-cache-entry (key value-list)
    (apply 'values
           (if *asdf-cache*
               (setf (gethash key *asdf-cache*) value-list)
               value-list)))

  (defun unset-asdf-cache-entry (key)
    (when *asdf-cache*
      (remhash key *asdf-cache*)))

  (defun consult-asdf-cache (key &optional thunk)
    (if *asdf-cache*
        (multiple-value-bind (results foundp) (gethash key *asdf-cache*)
          (if foundp
              (apply 'values results)
              (set-asdf-cache-entry key (multiple-value-list (call-function thunk)))))
        (call-function thunk)))

  (defmacro do-asdf-cache (key &body body)
    `(consult-asdf-cache ,key #'(lambda () ,@body)))

  (defun call-with-asdf-cache (thunk &key override key)
    (let ((fun (if key #'(lambda () (consult-asdf-cache key thunk)) thunk)))
      (if (and *asdf-cache* (not override))
          (funcall fun)
          (let ((*asdf-cache* (make-hash-table :test 'equal)))
            (funcall fun)))))

  (defmacro with-asdf-cache ((&key key override) &body body)
    `(call-with-asdf-cache #'(lambda () ,@body) :override ,override :key ,key))

  (defun normalize-namestring (pathname)
    (let ((resolved (resolve-symlinks*
                     (ensure-absolute-pathname
                      (physicalize-pathname pathname)
                      'get-pathname-defaults))))
      (with-pathname-defaults () (namestring resolved))))

  (defun compute-file-stamp (normalized-namestring)
    (with-pathname-defaults ()
      (safe-file-write-date normalized-namestring)))

  (defun register-file-stamp (file &optional (stamp nil stampp))
    (let* ((namestring (normalize-namestring file))
           (stamp (if stampp stamp (compute-file-stamp namestring))))
      (set-asdf-cache-entry `(get-file-stamp ,namestring) (list stamp))))

  (defun get-file-stamp (file)
    (when file
      (let ((namestring (normalize-namestring file)))
        (do-asdf-cache `(get-file-stamp ,namestring) (compute-file-stamp namestring))))))

;;;; -------------------------------------------------------------------------
;;;; Finding systems

(uiop/package:define-package :asdf/find-system
  (:recycle :asdf/find-system :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
    :asdf/cache :asdf/component :asdf/system)
  (:export
   #:remove-entry-from-registry #:coerce-entry-to-directory
   #:coerce-name #:primary-system-name #:coerce-filename
   #:find-system #:locate-system #:load-asd
   #:system-registered-p #:register-system #:registered-systems #:clear-system #:map-systems
   #:missing-component #:missing-requires #:missing-parent
   #:formatted-system-definition-error #:format-control #:format-arguments #:sysdef-error
   #:load-system-definition-error #:error-name #:error-pathname #:error-condition
   #:*system-definition-search-functions* #:search-for-system-definition
   #:*central-registry* #:probe-asd #:sysdef-central-registry-search
   #:find-system-if-being-defined
   #:contrib-sysdef-search #:sysdef-find-asdf ;; backward compatibility symbols, functions removed
   #:sysdef-preloaded-system-search #:register-preloaded-system #:*preloaded-systems*
   #:clear-defined-system #:clear-defined-systems #:*defined-systems*
   #:*immutable-systems*
   ;; defined in source-registry, but specially mentioned here:
   #:initialize-source-registry #:sysdef-source-registry-search))
(in-package :asdf/find-system)

(with-upgradability ()
  (declaim (ftype (function (&optional t) t) initialize-source-registry)) ; forward reference

  (define-condition missing-component (system-definition-error)
    ((requires :initform "(unnamed)" :reader missing-requires :initarg :requires)
     (parent :initform nil :reader missing-parent :initarg :parent)))

  (define-condition formatted-system-definition-error (system-definition-error)
    ((format-control :initarg :format-control :reader format-control)
     (format-arguments :initarg :format-arguments :reader format-arguments))
    (:report (lambda (c s)
               (apply 'format s (format-control c) (format-arguments c)))))

  (define-condition load-system-definition-error (system-definition-error)
    ((name :initarg :name :reader error-name)
     (pathname :initarg :pathname :reader error-pathname)
     (condition :initarg :condition :reader error-condition))
    (:report (lambda (c s)
               (format s (compatfmt "~@<Error while trying to load definition for system ~A from pathname ~A: ~3i~_~A~@:>")
                       (error-name c) (error-pathname c) (error-condition c)))))

  (defun sysdef-error (format &rest arguments)
    (error 'formatted-system-definition-error :format-control
           format :format-arguments arguments))

  (defun coerce-name (name)
    (typecase name
      (component (component-name name))
      (symbol (string-downcase (symbol-name name)))
      (string name)
      (t (sysdef-error (compatfmt "~@<Invalid component designator: ~3i~_~A~@:>") name))))

  (defun primary-system-name (name)
    ;; When a system name has slashes, the file with defsystem is named by
    ;; the first of the slash-separated components.
    (first (split-string (coerce-name name) :separator "/")))

  (defun coerce-filename (name)
    (frob-substrings (coerce-name name) '("/" ":" "\\") "--"))

  (defvar *defined-systems* (make-hash-table :test 'equal)
    "This is a hash table whose keys are strings, being the
names of the systems, and whose values are pairs, the first
element of which is a universal-time indicating when the
system definition was last updated, and the second element
of which is a system object.")

  (defun system-registered-p (name)
    (gethash (coerce-name name) *defined-systems*))

  (defun registered-systems ()
    (loop :for registered :being :the :hash-values :of *defined-systems*
          :collect (coerce-name (cdr registered))))

  (defun register-system (system)
    (check-type system system)
    (let ((name (component-name system)))
      (check-type name string)
      (asdf-message (compatfmt "~&~@<; ~@;Registering ~3i~_~A~@:>~%") system)
      (unless (eq system (cdr (gethash name *defined-systems*)))
        (setf (gethash name *defined-systems*)
              (cons (if-let (file (ignore-errors (system-source-file system)))
                      (get-file-stamp file))
                    system)))))

  (defun clear-defined-system (system)
    (let ((name (coerce-name system)))
      (remhash name *defined-systems*)
      (unset-asdf-cache-entry `(locate-system ,name))
      (unset-asdf-cache-entry `(find-system ,name))
      nil))

  (defun clear-defined-systems ()
    ;; Invalidate all systems but ASDF itself, if registered.
    (loop :for name :being :the :hash-keys :of *defined-systems*
	  :unless (equal name "asdf")
	    :do (clear-defined-system name)))

  (register-hook-function '*post-upgrade-cleanup-hook* 'clear-defined-systems nil)

  (defun clear-system (name)
    "Clear the entry for a system in the database of systems previously loaded.
Note that this does NOT in any way cause the code of the system to be unloaded."
    ;; There is no "unload" operation in Common Lisp, and
    ;; a general such operation cannot be portably written,
    ;; considering how much CL relies on side-effects to global data structures.
    (remhash (coerce-name name) *defined-systems*))

  (defun map-systems (fn)
    "Apply FN to each defined system.

FN should be a function of one argument. It will be
called with an object of type asdf:system."
    (loop :for registered :being :the :hash-values :of *defined-systems*
          :do (funcall fn (cdr registered)))))

;;; for the sake of keeping things reasonably neat, we adopt a
;;; convention that functions in this list are prefixed SYSDEF-
(with-upgradability ()
  (defvar *system-definition-search-functions* '())

  (defun cleanup-system-definition-search-functions ()
    (setf *system-definition-search-functions*
          (append
           ;; Remove known-incompatible sysdef functions from old versions of asdf.
           (remove-if #'(lambda (x) (member x '(contrib-sysdef-search sysdef-find-asdf sysdef-preloaded-system-search)))
                      *system-definition-search-functions*)
           ;; Tuck our defaults at the end of the list if they were absent.
           ;; This is imperfect, in case they were removed on purpose,
           ;; but then it will be the responsibility of whoever does that
           ;; to upgrade asdf before he does such a thing rather than after.
           (remove-if #'(lambda (x) (member x *system-definition-search-functions*))
                      '(sysdef-central-registry-search
                        sysdef-source-registry-search)))))
  (cleanup-system-definition-search-functions)

  (defun search-for-system-definition (system)
    (let ((name (coerce-name system)))
      (flet ((try (f) (if-let ((x (funcall f name))) (return-from search-for-system-definition x))))
        (try 'find-system-if-being-defined)
        (try 'sysdef-immutable-system-search)
        (map () #'try *system-definition-search-functions*)
        (try 'sysdef-preloaded-system-search))))

  (defvar *central-registry* nil
    "A list of 'system directory designators' ASDF uses to find systems.

A 'system directory designator' is a pathname or an expression
which evaluates to a pathname. For example:

    (setf asdf:*central-registry*
          (list '*default-pathname-defaults*
                #p\"/home/me/cl/systems/\"
                #p\"/usr/share/common-lisp/systems/\"))

This is for backward compatibility.
Going forward, we recommend new users should be using the source-registry.
")

  (defun probe-asd (name defaults &key truename)
    (block nil
      (when (directory-pathname-p defaults)
        (if-let (file (probe-file*
                       (ensure-absolute-pathname
                        (parse-unix-namestring name :type "asd")
                        #'(lambda () (ensure-absolute-pathname defaults 'get-pathname-defaults nil))
                        nil)
                       :truename truename))
          (return file))
        #-(or clisp genera) ; clisp doesn't need it, plain genera doesn't have read-sequence(!)
        (when (and (os-windows-p) (physical-pathname-p defaults))
          (let ((shortcut
                  (make-pathname
                   :defaults defaults :case :local
                   :name (strcat name ".asd")
                   :type "lnk")))
            (when (probe-file* shortcut)
              (ensure-pathname (parse-windows-shortcut shortcut) :namestring :native)))))))

  (defun sysdef-central-registry-search (system)
    (let ((name (primary-system-name system))
          (to-remove nil)
          (to-replace nil))
      (block nil
        (unwind-protect
             (dolist (dir *central-registry*)
               (let ((defaults (eval dir))
                     directorized)
                 (when defaults
                   (cond ((directory-pathname-p defaults)
                          (let* ((file (probe-asd name defaults :truename *resolve-symlinks*)))
                            (when file
                              (return file))))
                         (t
                          (restart-case
                              (let* ((*print-circle* nil)
                                     (message
                                       (format nil
                                               (compatfmt "~@<While searching for system ~S: ~3i~_~S evaluated to ~S which is not an absolute directory.~@:>")
                                               system dir defaults)))
                                (error message))
                            (remove-entry-from-registry ()
                              :report "Remove entry from *central-registry* and continue"
                              (push dir to-remove))
                            (coerce-entry-to-directory ()
                              :test (lambda (c) (declare (ignore c))
                                      (and (not (directory-pathname-p defaults))
                                           (directory-pathname-p
                                            (setf directorized
                                                  (ensure-directory-pathname defaults)))))
                              :report (lambda (s)
                                        (format s (compatfmt "~@<Coerce entry to ~a, replace ~a and continue.~@:>")
                                                directorized dir))
                              (push (cons dir directorized) to-replace))))))))
          ;; cleanup
          (dolist (dir to-remove)
            (setf *central-registry* (remove dir *central-registry*)))
          (dolist (pair to-replace)
            (let* ((current (car pair))
                   (new (cdr pair))
                   (position (position current *central-registry*)))
              (setf *central-registry*
                    (append (subseq *central-registry* 0 position)
                            (list new)
                            (subseq *central-registry* (1+ position))))))))))

  (defvar *preloaded-systems* (make-hash-table :test 'equal))

  (defun make-preloaded-system (name keys)
    (apply 'make-instance (getf keys :class 'system)
           :name name :source-file (getf keys :source-file)
           (remove-plist-keys '(:class :name :source-file) keys)))

  (defun sysdef-preloaded-system-search (requested)
    (let ((name (coerce-name requested)))
      (multiple-value-bind (keys foundp) (gethash name *preloaded-systems*)
        (when foundp
          (make-preloaded-system name keys)))))

  (defun register-preloaded-system (system-name &rest keys)
    (setf (gethash (coerce-name system-name) *preloaded-systems*) keys))

  (dolist (s '("asdf" "uiop" "asdf-driver" "asdf-defsystem" "asdf-package-system"))
    ;; don't bother with these, no one relies on them: "asdf-utils" "asdf-bundle"
    (register-preloaded-system s :version *asdf-version*))

  (defmethod find-system ((name null) &optional (error-p t))
    (when error-p
      (sysdef-error (compatfmt "~@<NIL is not a valid system name~@:>"))))

  (defmethod find-system (name &optional (error-p t))
    (find-system (coerce-name name) error-p))

  (defun find-system-if-being-defined (name)
    ;; notable side effect: mark the system as being defined, to avoid infinite loops
    (first (gethash `(find-system ,(coerce-name name)) *asdf-cache*)))

  (defun load-asd (pathname &key name (external-format (encoding-external-format (detect-encoding pathname))) &aux (readtable *readtable*) (print-pprint-dispatch *print-pprint-dispatch*))
    ;; Tries to load system definition with canonical NAME from PATHNAME.
    (with-asdf-cache ()
      (with-standard-io-syntax
        (let ((*package* (find-package :asdf-user))
              ;; Note that our backward-compatible *readtable* is
              ;; a global readtable that gets globally side-effected. Ouch.
              ;; Same for the *print-pprint-dispatch* table.
              ;; We should do something about that for ASDF3 if possible, or else ASDF4.
              (*readtable* readtable)
              (*print-pprint-dispatch* print-pprint-dispatch)
              (*print-readably* nil)
              (*default-pathname-defaults*
                ;; resolve logical-pathnames so they won't wreak havoc in parsing namestrings.
                (pathname-directory-pathname (physicalize-pathname pathname))))
          (handler-bind
              ((error #'(lambda (condition)
                          (error 'load-system-definition-error
                                 :name name :pathname pathname
                                 :condition condition))))
            (asdf-message (compatfmt "~&~@<; ~@;Loading system definition~@[ for ~A~] from ~A~@:>~%")
                          name pathname)
            (load* pathname :external-format external-format))))))

  (defvar *old-asdf-systems* (make-hash-table :test 'equal))

  (defun check-not-old-asdf-system (name pathname)
    (or (not (equal name "asdf"))
        (null pathname)
        (let* ((version-pathname (subpathname pathname "version.lisp-expr"))
               (version (and (probe-file* version-pathname :truename nil)
                             (read-file-form version-pathname)))
               (old-version (asdf-version)))
          (cond
            ((version< old-version version) t) ;; newer version: good!
            ((equal old-version version) nil) ;; same version: don't load, but don't warn
            (t ;; old version: bad
             (ensure-gethash
              (list (namestring pathname) version) *old-asdf-systems*
              #'(lambda ()
                 (let ((old-pathname
                         (if-let (pair (system-registered-p "asdf"))
                           (system-source-file (cdr pair)))))
                   (warn "~@<~
        You are using ASDF version ~A ~:[(probably from (require \"asdf\") ~
        or loaded by quicklisp)~;from ~:*~S~] and have an older version of ASDF ~
        ~:[(and older than 2.27 at that)~;~:*~A~] registered at ~S. ~
        Having an ASDF installed and registered is the normal way of configuring ASDF to upgrade itself, ~
        and having an old version registered is a configuration error. ~
        ASDF will ignore this configured system rather than downgrade itself. ~
        In the future, you may want to either: ~
        (a) upgrade this configured ASDF to a newer version, ~
        (b) install a newer ASDF and register it in front of the former in your configuration, or ~
        (c) uninstall or unregister this and any other old version of ASDF from your configuration. ~
        Note that the older ASDF might be registered implicitly through configuration inherited ~
        from your system installation, in which case you might have to specify ~
        :ignore-inherited-configuration in your in your ~~/.config/common-lisp/source-registry.conf ~
        or other source-registry configuration file, environment variable or lisp parameter. ~
        Indeed, a likely offender is an obsolete version of the cl-asdf debian or ubuntu package, ~
        that you might want to upgrade (if a recent enough version is available) ~
        or else remove altogether (since most implementations ship with a recent asdf); ~
        if you lack the system administration rights to upgrade or remove this package, ~
        then you might indeed want to either install and register a more recent version, ~
        or use :ignore-inherited-configuration to avoid registering the old one. ~
        Please consult ASDF documentation and/or experts.~@:>~%"
                         old-version old-pathname version pathname))))
             nil))))) ;; only issue the warning the first time, but always return nil

  (defvar *immutable-systems* nil
    "An hash-set (equal hash-table mapping keys to T) of systems that are immutable,
i.e. already loaded in memory and not to be refreshed from the filesystem.
They will be treated specially by find-system, and passed as :force-not argument to make-plan.

If you deliver an image with many systems precompiled, *and* do not want to check the filesystem
for them every time a user loads an extension, what more risk a problematic upgrade or catastrophic
downgrade, before you dump an image, use:
   (setf asdf::*immutable-systems* (uiop:list-to-hash-set (asdf:already-loaded-systems)))")

  (defun sysdef-immutable-system-search (requested)
    (let ((name (coerce-name requested)))
      (when (and *immutable-systems* (gethash name *immutable-systems*))
        (or (cdr (system-registered-p requested))
            (error 'formatted-system-definition-error
                   :format-control "Requested system ~A is in the *immutable-systems* set, ~
but not loaded in memory"
                   :format-arguments (list name))))))

  (defun locate-system (name)
    "Given a system NAME designator, try to locate where to load the system from.
Returns five values: FOUNDP FOUND-SYSTEM PATHNAME PREVIOUS PREVIOUS-TIME
FOUNDP is true when a system was found,
either a new unregistered one or a previously registered one.
FOUND-SYSTEM when not null is a SYSTEM object that may be REGISTER-SYSTEM'ed as is
PATHNAME when not null is a path from where to load the system,
either associated with FOUND-SYSTEM, or with the PREVIOUS system.
PREVIOUS when not null is a previously loaded SYSTEM object of same name.
PREVIOUS-TIME when not null is the time at which the PREVIOUS system was loaded."
    (with-asdf-cache (:key `(locate-system ,name))
      (let* ((name (coerce-name name))
             (in-memory (system-registered-p name)) ; load from disk if absent or newer on disk
             (previous (cdr in-memory))
             (previous (and (typep previous 'system) previous))
             (previous-time (car in-memory))
             (found (search-for-system-definition name))
             (found-system (and (typep found 'system) found))
             (pathname (ensure-pathname
                        (or (and (typep found '(or pathname string)) (pathname found))
                            (and found-system (system-source-file found-system))
                            (and previous (system-source-file previous)))
                        :want-absolute t :resolve-symlinks *resolve-symlinks*))
             (foundp (and (or found-system pathname previous) t)))
        (check-type found (or null pathname system))
        (unless (check-not-old-asdf-system name pathname)
          (cond
            (previous (setf found nil pathname nil))
            (t
             (setf found (sysdef-preloaded-system-search "asdf"))
             (assert (typep found 'system))
             (setf found-system found pathname nil))))
        (values foundp found-system pathname previous previous-time))))

  (defmethod find-system ((name string) &optional (error-p t))
    (with-asdf-cache (:key `(find-system ,name))
      (let ((primary-name (primary-system-name name)))
        (unless (equal name primary-name)
          (find-system primary-name nil)))
      (loop
        (restart-case
            (multiple-value-bind (foundp found-system pathname previous previous-time)
                (locate-system name)
              (when (and found-system (eq found-system previous)
                         (or (first (gethash `(find-system ,name) *asdf-cache*))
                             (and *immutable-systems* (gethash name *immutable-systems*))))
                (return found-system))
              (assert (eq foundp (and (or found-system pathname previous) t)))
              (let ((previous-pathname (and previous (system-source-file previous)))
                    (system (or previous found-system)))
                (when (and found-system (not previous))
                  (register-system found-system))
                (when (and system pathname)
                  (setf (system-source-file system) pathname))
                (when (and pathname
                           (let ((stamp (get-file-stamp pathname)))
                             (and stamp
                                  (not (and previous
                                            (or (pathname-equal pathname previous-pathname)
                                                (and pathname previous-pathname
                                                     (pathname-equal
                                                      (physicalize-pathname pathname)
                                                      (physicalize-pathname previous-pathname))))
                                            (stamp<= stamp previous-time))))))
                  ;; only load when it's a pathname that is different or has newer content, and not an old asdf
                  (load-asd pathname :name name)))
              (let ((in-memory (system-registered-p name))) ; try again after loading from disk if needed
                (return
                  (cond
                    (in-memory
                     (when pathname
                       (setf (car in-memory) (get-file-stamp pathname)))
                     (cdr in-memory))
                    (error-p
                     (error 'missing-component :requires name))))))
          (reinitialize-source-registry-and-retry ()
            :report (lambda (s)
                      (format s (compatfmt "~@<Retry finding system ~A after reinitializing the source-registry.~@:>") name))
	    (unset-asdf-cache-entry `(locate-system ,name))
            (initialize-source-registry)))))))

;;;; -------------------------------------------------------------------------
;;;; Finding components

(uiop/package:define-package :asdf/find-component
  (:recycle :asdf/find-component :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/cache
   :asdf/component :asdf/system :asdf/find-system)
  (:export
   #:find-component
   #:resolve-dependency-name #:resolve-dependency-spec
   #:resolve-dependency-combination
   ;; Conditions
   #:missing-component #:missing-component-of-version #:retry
   #:missing-dependency #:missing-dependency-of-version
   #:missing-requires #:missing-parent
   #:missing-required-by #:missing-version))
(in-package :asdf/find-component)

;;;; Missing component conditions

(with-upgradability ()
  (define-condition missing-component-of-version (missing-component)
    ((version :initform nil :reader missing-version :initarg :version)))

  (define-condition missing-dependency (missing-component)
    ((required-by :initarg :required-by :reader missing-required-by)))

  (defmethod print-object ((c missing-dependency) s)
    (format s (compatfmt "~@<~A, required by ~A~@:>")
            (call-next-method c nil) (missing-required-by c)))

  (define-condition missing-dependency-of-version (missing-dependency
                                                   missing-component-of-version)
    ())

  (defmethod print-object ((c missing-component) s)
    (format s (compatfmt "~@<Component ~S not found~@[ in ~A~]~@:>")
            (missing-requires c)
            (when (missing-parent c)
              (coerce-name (missing-parent c)))))

  (defmethod print-object ((c missing-component-of-version) s)
    (format s (compatfmt "~@<Component ~S does not match version ~A~@[ in ~A~]~@:>")
            (missing-requires c)
            (missing-version c)
            (when (missing-parent c)
              (coerce-name (missing-parent c))))))


;;;; Finding components

(with-upgradability ()
  (defgeneric* (find-component) (base path)
    (:documentation "Find a component by resolving the PATH starting from BASE parent"))
  (defgeneric resolve-dependency-combination (component combinator arguments))

  (defmethod find-component ((base string) path)
    (let ((s (find-system base nil)))
      (and s (find-component s path))))

  (defmethod find-component ((base symbol) path)
    (cond
      (base (find-component (coerce-name base) path))
      (path (find-component path nil))
      (t    nil)))

  (defmethod find-component ((base cons) path)
    (find-component (car base) (cons (cdr base) path)))

  (defmethod find-component ((parent parent-component) (name string))
    (compute-children-by-name parent :only-if-needed-p t) ;; SBCL may miss the u-i-f-r-c method!!!
    (values (gethash name (component-children-by-name parent))))

  (defmethod find-component (base (name symbol))
    (if name
        (find-component base (coerce-name name))
        base))

  (defmethod find-component ((c component) (name cons))
    (find-component (find-component c (car name)) (cdr name)))

  (defmethod find-component ((base t) (actual component))
    actual)

  (defun resolve-dependency-name (component name &optional version)
    (loop
      (restart-case
          (return
            (let ((comp (find-component (component-parent component) name)))
              (unless comp
                (error 'missing-dependency
                       :required-by component
                       :requires name))
              (when version
                (unless (version-satisfies comp version)
                  (error 'missing-dependency-of-version
                         :required-by component
                         :version version
                         :requires name)))
              comp))
        (retry ()
          :report (lambda (s)
                    (format s (compatfmt "~@<Retry loading ~3i~_~A.~@:>") name))
          :test
          (lambda (c)
            (or (null c)
                (and (typep c 'missing-dependency)
                     (eq (missing-required-by c) component)
                     (equal (missing-requires c) name))))
	  (unless (component-parent component)
	    (let ((name (coerce-name name)))
	      (unset-asdf-cache-entry `(find-system ,name))
	      (unset-asdf-cache-entry `(locate-system ,name))))))))


  (defun resolve-dependency-spec (component dep-spec)
    (let ((component (find-component () component)))
      (if (atom dep-spec)
          (resolve-dependency-name component dep-spec)
          (resolve-dependency-combination component (car dep-spec) (cdr dep-spec)))))

  (defmethod resolve-dependency-combination (component combinator arguments)
    (error (compatfmt "~@<Bad dependency ~S for ~S~@:>")
           (cons combinator arguments) component))

  (defmethod resolve-dependency-combination (component (combinator (eql :feature)) arguments)
    (when (featurep (first arguments))
      (resolve-dependency-spec component (second arguments))))

  (defmethod resolve-dependency-combination (component (combinator (eql :version)) arguments)
    (resolve-dependency-name component (first arguments) (second arguments)))) ;; See lp#527788

;;;; -------------------------------------------------------------------------
;;;; Operations

(uiop/package:define-package :asdf/operation
  (:recycle :asdf/operation :asdf/action :asdf) ;; asdf/action for FEATURE pre 2.31.5.
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/find-system)
  (:export
   #:operation
   #:operation-original-initargs #:original-initargs ;; backward-compatibility only. DO NOT USE.
   #:*operations* #:make-operation #:find-operation
   #:feature)) ;; TODO: stop exporting the deprecated FEATURE feature.
(in-package :asdf/operation)

;;; Operation Classes

(when-upgrading (:when (find-class 'operation nil))
  ;; override any obsolete shared-initialize method when upgrading from ASDF2.
  (defmethod shared-initialize :after ((o operation) (slot-names t) &key)
    (values)))

(with-upgradability ()
  (defclass operation ()
    ((original-initargs ;; for backward-compat -- used by GBBopen and swank (via operation-forced)
      :initform nil :initarg :original-initargs :accessor operation-original-initargs)))

  ;; Cache a copy of the INITARGS in the ORIGINAL-INITARGS slot, if that slot is not
  ;; already bound.
  (defmethod initialize-instance :after ((o operation) &rest initargs
                                         &key force force-not system verbose &allow-other-keys)
    (declare (ignore force force-not system verbose))
    (unless (slot-boundp o 'original-initargs)
      (setf (operation-original-initargs o) initargs)))

  (defmethod print-object ((o operation) stream)
    (print-unreadable-object (o stream :type t :identity nil)
      (ignore-errors
       (format stream "~{~S~^ ~}" (operation-original-initargs o))))))

;;; make-operation, find-operation

(with-upgradability ()
  (defparameter* *operations* (make-hash-table :test 'equal))

  (defun make-operation (operation-class &rest initargs)
    (let ((class (coerce-class operation-class
                               :package :asdf/interface :super 'operation :error 'sysdef-error)))
      (ensure-gethash (cons class initargs) *operations*
                      (list* 'make-instance class initargs))))

  (defgeneric find-operation (context spec)
    (:documentation "Find an operation by resolving the SPEC in the CONTEXT"))
  (defmethod find-operation ((context t) (spec operation))
    spec)
  (defmethod find-operation (context (spec symbol))
    (when spec ;; NIL designates itself, i.e. absence of operation
      (apply 'make-operation spec (operation-original-initargs context))))
  (defmethod find-operation (context (spec string))
    (apply 'make-operation spec (operation-original-initargs context)))
  (defmethod operation-original-initargs ((context symbol))
    (declare (ignorable context))
    nil))

;;;; -------------------------------------------------------------------------
;;;; Actions

(uiop/package:define-package :asdf/action
  (:nicknames :asdf-action)
  (:recycle :asdf/action :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
   :asdf/component :asdf/system #:asdf/cache :asdf/find-system :asdf/find-component :asdf/operation)
  (:export
   #:action #:define-convenience-action-methods
   #:explain #:action-description
   #:downward-operation #:upward-operation #:sideway-operation #:selfward-operation #:non-propagating-operation
   #:component-depends-on
   #:input-files #:output-files #:output-file #:operation-done-p
   #:action-status #:action-stamp #:action-done-p
   #:component-operation-time #:mark-operation-done #:compute-action-stamp
   #:perform #:perform-with-restarts #:retry #:accept
   #:traverse-actions #:traverse-sub-actions #:required-components ;; in plan
   #:action-path #:find-action #:stamp #:done-p
   #:operation-definition-warning #:operation-definition-error ;; condition
   ))
(in-package :asdf/action)

(eval-when (#-lispworks :compile-toplevel :load-toplevel :execute) ;; LispWorks issues spurious warning
  (deftype action () '(cons operation component)) ;; a step to be performed while building

  (deftype operation-designator ()
    ;; an operation designates itself,
    ;; nil designates a context-dependent current operation, and
    ;; class-name or class designates an instance of the designated class.
    '(or operation null symbol class)))

(with-upgradability ()
  (defgeneric traverse-actions (actions &key &allow-other-keys))
  (defgeneric traverse-sub-actions (operation component &key &allow-other-keys))
  (defgeneric required-components (component &key &allow-other-keys)))

;;;; Reified representation for storage or debugging. Note: dropping original-initargs
(with-upgradability ()
  (defun action-path (action)
    (destructuring-bind (o . c) action (cons (type-of o) (component-find-path c))))
  (defun find-action (path)
    (destructuring-bind (o . c) path (cons (make-operation o) (find-component () c)))))


;;;; Convenience methods
(with-upgradability ()
  (defmacro define-convenience-action-methods
      (function formals &key if-no-operation if-no-component operation-initargs)
    (let* ((rest (gensym "REST"))
           (found (gensym "FOUND"))
           (keyp (equal (last formals) '(&key)))
           (formals-no-key (if keyp (butlast formals) formals))
           (len (length formals-no-key))
           (operation 'operation)
           (component 'component)
           (opix (position operation formals))
           (coix (position component formals))
           (prefix (subseq formals 0 opix))
           (suffix (subseq formals (1+ coix) len))
           (more-args (when keyp `(&rest ,rest &key &allow-other-keys))))
      (assert (and (integerp opix) (integerp coix) (= coix (1+ opix))))
      (flet ((next-method (o c)
               (if keyp
                   `(apply ',function ,@prefix ,o ,c ,@suffix ,rest)
                   `(,function ,@prefix ,o ,c ,@suffix))))
        `(progn
           (defmethod ,function (,@prefix (,operation string) ,component ,@suffix ,@more-args)
             (let ((,component (find-component () ,component))) ;; do it first, for defsystem-depends-on
               ,(next-method `(safe-read-from-string ,operation :package :asdf/interface) component)))
           (defmethod ,function (,@prefix (,operation symbol) ,component ,@suffix ,@more-args)
             (if ,operation
                 ,(next-method
                   (if operation-initargs ;backward-compatibility with ASDF1's operate. Yuck.
                       `(apply 'make-operation ,operation :original-initargs ,rest ,rest)
                       `(make-operation ,operation))
                   `(or (find-component () ,component) ,if-no-component))
                 ,if-no-operation))
           (defmethod ,function (,@prefix (,operation operation) ,component ,@suffix ,@more-args)
             (if (typep ,component 'component)
                 (error "No defined method for ~S on ~/asdf-action:format-action/"
                        ',function (cons ,operation ,component))
                 (if-let (,found (find-component () ,component))
                    ,(next-method operation found)
                    ,if-no-component))))))))


;;;; self-description
(with-upgradability ()
  (defgeneric action-description (operation component)
    (:documentation "returns a phrase that describes performing this operation
on this component, e.g. \"loading /a/b/c\".
You can put together sentences using this phrase."))
  (defmethod action-description (operation component)
    (format nil (compatfmt "~@<~A on ~A~@:>")
            (type-of operation) component))
  (defgeneric* (explain) (operation component))
  (defmethod explain ((o operation) (c component))
    (asdf-message (compatfmt "~&~@<; ~@;~A~:>~%") (action-description o c)))
  (define-convenience-action-methods explain (operation component))

  (defun format-action (stream action &optional colon-p at-sign-p)
    (assert (null colon-p)) (assert (null at-sign-p))
    (destructuring-bind (operation . component) action
      (princ (action-description operation component) stream))))


;;;; Dependencies
(with-upgradability ()
  (defgeneric* (component-depends-on) (operation component) ;; ASDF4: rename to component-dependencies
    (:documentation
     "Returns a list of dependencies needed by the component to perform
    the operation.  A dependency has one of the following forms:

      (<operation> <component>*), where <operation> is an operation designator
        with respect to FIND-OPERATION in the context of the OPERATION argument,
        and each <component> is a component designator with respect to
        FIND-COMPONENT in the context of the COMPONENT argument,
        and means that the component depends on
        <operation> having been performed on each <component>;

        [Note: an <operation> is an operation designator -- it can be either an
        operation name or an operation object.  Similarly, a <component> may be
        a component name or a component object.  Also note that, the degenerate
        case of (<operation>) is a no-op.]

    Methods specialized on subclasses of existing component types
    should usually append the results of CALL-NEXT-METHOD to the list."))
  (define-convenience-action-methods component-depends-on (operation component))

  (defmethod component-depends-on :around ((o operation) (c component))
    (do-asdf-cache `(component-depends-on ,o ,c)
      (call-next-method))))


;;;; upward-operation, downward-operation, sideway-operation, selfward-operation
;; These together handle actions that propagate along the component hierarchy or operation universe.
(with-upgradability ()
  (defclass downward-operation (operation)
    ((downward-operation
      :initform nil :reader downward-operation
      :type operation-designator :allocation :class))
    (:documentation "A DOWNWARD-OPERATION's dependencies propagate down the component hierarchy.
I.e., if O is a DOWNWARD-OPERATION and its DOWNWARD-OPERATION slot designates operation D, then
the action (O . M) of O on module M will depends on each of (D . C) for each child C of module M.
The default value for slot DOWNWARD-OPERATION is NIL, which designates the operation O itself.
E.g. in order for a MODULE to be loaded with LOAD-OP (resp. compiled with COMPILE-OP), all the
children of the MODULE must have been loaded with LOAD-OP (resp. compiled with COMPILE-OP."))
  (defun downward-operation-depends-on (o c)
    `((,(or (downward-operation o) o) ,@(component-children c))))
  (defmethod component-depends-on ((o downward-operation) (c parent-component))
    `(,@(downward-operation-depends-on o c) ,@(call-next-method)))

  (defclass upward-operation (operation)
    ((upward-operation
      :initform nil :reader upward-operation
      :type operation-designator :allocation :class))
    (:documentation "An UPWARD-OPERATION has dependencies that propagate up the component hierarchy.
I.e., if O is an instance of UPWARD-OPERATION, and its UPWARD-OPERATION slot designates operation U,
then the action (O . C) of O on a component C that has the parent P will depends on (U . P).
The default value for slot UPWARD-OPERATION is NIL, which designates the operation O itself.
E.g. in order for a COMPONENT to be prepared for loading or compiling with PREPARE-OP, its PARENT
must first be prepared for loading or compiling with PREPARE-OP."))
  ;; For backward-compatibility reasons, a system inherits from module and is a child-component
  ;; so we must guard against this case. ASDF4: remove that.
  (defun upward-operation-depends-on (o c)
    (if-let (p (component-parent c)) `((,(or (upward-operation o) o) ,p))))
  (defmethod component-depends-on ((o upward-operation) (c child-component))
    `(,@(upward-operation-depends-on o c) ,@(call-next-method)))

  (defclass sideway-operation (operation)
    ((sideway-operation
      :initform nil :reader sideway-operation
      :type operation-designator :allocation :class))
    (:documentation "A SIDEWAY-OPERATION has dependencies that propagate \"sideway\" to siblings
that a component depends on. I.e. if O is a SIDEWAY-OPERATION, and its SIDEWAY-OPERATION slot
designates operation S (where NIL designates O itself), then the action (O . C) of O on component C
depends on each of (S . D) where D is a declared dependency of C.
E.g. in order for a COMPONENT to be prepared for loading or compiling with PREPARE-OP,
each of its declared dependencies must first be loaded as by LOAD-OP."))
  (defun sideway-operation-depends-on (o c)
    `((,(or (sideway-operation o) o) ,@(component-sideway-dependencies c))))
  (defmethod component-depends-on ((o sideway-operation) (c component))
    `(,@(sideway-operation-depends-on o c) ,@(call-next-method)))

  (defclass selfward-operation (operation)
    ((selfward-operation
      ;; NB: no :initform -- if an operation depends on others, it must explicitly specify which
      :type (or operation-designator list) :reader selfward-operation :allocation :class))
    (:documentation "A SELFWARD-OPERATION depends on another operation on the same component.
I.e., if O is a SELFWARD-OPERATION, and its SELFWARD-OPERATION designates a list of operations L,
then the action (O . C) of O on component C depends on each (S . C) for S in L.
E.g. before a component may be loaded by LOAD-OP, it must have been compiled by COMPILE-OP.
A operation-designator designates a singleton list of the designated operation;
a list of operation-designators designates the list of designated operations;
NIL is not a valid operation designator in that context.  Note that any dependency
ordering between the operations in a list of SELFWARD-OPERATION should be specified separately
in the respective operation's COMPONENT-DEPENDS-ON methods so that they be scheduled properly."))
  (defun selfward-operation-depends-on (o c)
    (loop :for op :in (ensure-list (selfward-operation o)) :collect `(,op ,c)))
  (defmethod component-depends-on ((o selfward-operation) (c component))
    `(,@(selfward-operation-depends-on o c) ,@(call-next-method)))

  (defclass non-propagating-operation (operation)
    ()
    (:documentation "A NON-PROPAGATING-OPERATION is an operation that propagates
no dependencies whatsoever.  It is supplied in order that the programmer be able
to specify that s/he is intentionally specifying an operation which invokes no
dependencies.")))


;;;---------------------------------------------------------------------------
;;; Help programmers catch obsolete OPERATION subclasses
;;;---------------------------------------------------------------------------
(with-upgradability ()
  (define-condition operation-definition-warning (simple-warning)
    ()
    (:documentation "Warning condition related to definition of obsolete OPERATION objects."))

  (define-condition operation-definition-error (simple-error)
    ()
    (:documentation "Error condition related to definition of incorrect OPERATION objects."))

  (defmethod initialize-instance :before ((o operation) &key)
    (unless (typep o '(or downward-operation upward-operation sideway-operation
                          selfward-operation non-propagating-operation))
      (warn 'operation-definition-warning
            :format-control
            "No dependency propagating scheme specified for operation class ~S.
The class needs to be updated for ASDF 3.1 and specify appropriate propagation mixins."
            :format-arguments (list (type-of o)))))

  (defmethod initialize-instance :before ((o non-propagating-operation) &key)
    (when (typep o '(or downward-operation upward-operation sideway-operation selfward-operation))
      (error 'operation-definition-error
             :format-control
             "Inconsistent class: ~S
  NON-PROPAGATING-OPERATION is incompatible with propagating operation classes as superclasses."
             :format-arguments
             (list (type-of o)))))

  (defmethod component-depends-on ((o operation) (c component))
    `(;; Normal behavior, to allow user-specified in-order-to dependencies
      ,@(cdr (assoc (type-of o) (component-in-order-to c)))
      ;; For backward-compatibility with ASDF2, any operation that doesn't specify propagation
      ;; or non-propagation through an appropriate mixin will be downward and sideway.
      ,@(unless (typep o '(or downward-operation upward-operation sideway-operation
                              selfward-operation non-propagating-operation))
          `(,@(sideway-operation-depends-on o c)
            ,@(when (typep c 'parent-component) (downward-operation-depends-on o c))))))

  (defmethod downward-operation ((o operation)) nil)
  (defmethod sideway-operation ((o operation)) nil))


;;;---------------------------------------------------------------------------
;;; End of OPERATION class checking
;;;---------------------------------------------------------------------------


;;;; Inputs, Outputs, and invisible dependencies
(with-upgradability ()
  (defgeneric* (output-files) (operation component))
  (defgeneric* (input-files) (operation component))
  (defgeneric* (operation-done-p) (operation component)
    (:documentation "Returns a boolean, which is NIL if the action is forced to be performed again"))
  (define-convenience-action-methods output-files (operation component))
  (define-convenience-action-methods input-files (operation component))
  (define-convenience-action-methods operation-done-p (operation component))

  (defmethod operation-done-p ((o operation) (c component))
    t)

  (defmethod output-files :around (operation component)
    "Translate output files, unless asked not to. Memoize the result."
    operation component ;; hush genera, not convinced by declare ignorable(!)
    (do-asdf-cache `(output-files ,operation ,component)
      (values
       (multiple-value-bind (pathnames fixedp) (call-next-method)
         ;; 1- Make sure we have absolute pathnames
         (let* ((directory (pathname-directory-pathname
                            (component-pathname (find-component () component))))
                (absolute-pathnames
                  (loop
                    :for pathname :in pathnames
                    :collect (ensure-absolute-pathname pathname directory))))
           ;; 2- Translate those pathnames as required
           (if fixedp
               absolute-pathnames
               (mapcar *output-translation-function* absolute-pathnames))))
       t)))
  (defmethod output-files ((o operation) (c component))
    nil)
  (defun output-file (operation component)
    "The unique output file of performing OPERATION on COMPONENT"
    (let ((files (output-files operation component)))
      (assert (length=n-p files 1))
      (first files)))

  (defmethod input-files :around (operation component)
    "memoize input files."
    (do-asdf-cache `(input-files ,operation ,component)
      (call-next-method)))

  (defmethod input-files ((o operation) (c component))
    nil)

  (defmethod input-files ((o selfward-operation) (c component))
    `(,@(or (loop :for dep-o :in (ensure-list (selfward-operation o))
                  :append (or (output-files dep-o c) (input-files dep-o c)))
            (if-let ((pathname (component-pathname c)))
              (and (file-pathname-p pathname) (list pathname))))
      ,@(call-next-method))))


;;;; Done performing
(with-upgradability ()
  (defgeneric component-operation-time (operation component)) ;; ASDF4: hide it behind plan-action-stamp
  (define-convenience-action-methods component-operation-time (operation component))

  (defgeneric mark-operation-done (operation component)) ;; ASDF4: hide it behind (setf plan-action-stamp)
  (defgeneric compute-action-stamp (plan operation component &key just-done)
    (:documentation "Has this action been successfully done already,
and at what known timestamp has it been done at or will it be done at?
Takes two keywords JUST-DONE and PLAN:
JUST-DONE is a boolean that is true if the action was just successfully performed,
at which point we want compute the actual stamp and warn if files are missing;
otherwise we are making plans, anticipating the effects of the action.
PLAN is a plan object modelling future effects of actions,
or NIL to denote what actually happened.
Returns two values:
* a STAMP saying when it was done or will be done,
  or T if the action has involves files that need to be recomputed.
* a boolean DONE-P that indicates whether the action has actually been done,
  and both its output-files and its in-image side-effects are up to date."))

  (defclass action-status ()
    ((stamp
      :initarg :stamp :reader action-stamp
      :documentation "STAMP associated with the ACTION if it has been completed already
in some previous image, or T if it needs to be done.")
     (done-p
      :initarg :done-p :reader action-done-p
      :documentation "a boolean, true iff the action was already done (before any planned action)."))
    (:documentation "Status of an action"))

  (defmethod print-object ((status action-status) stream)
    (print-unreadable-object (status stream :type t)
      (with-slots (stamp done-p) status
        (format stream "~@{~S~^ ~}" :stamp stamp :done-p done-p))))

  (defmethod component-operation-time ((o operation) (c component))
    (gethash (type-of o) (component-operation-times c)))

  (defmethod mark-operation-done ((o operation) (c component))
    (setf (gethash (type-of o) (component-operation-times c))
          (compute-action-stamp nil o c :just-done t))))


;;;; Perform
(with-upgradability ()
  (defgeneric* (perform-with-restarts) (operation component))
  (defgeneric* (perform) (operation component))
  (define-convenience-action-methods perform (operation component))

  (defmethod perform :before ((o operation) (c component))
    (ensure-all-directories-exist (output-files o c)))
  (defmethod perform :after ((o operation) (c component))
    (mark-operation-done o c))
  (defmethod perform ((o operation) (c parent-component))
    nil)
  (defmethod perform ((o operation) (c source-file))
    ;; For backward compatibility, don't error on operations that don't specify propagation.
    (when (typep o '(or downward-operation upward-operation sideway-operation
                     selfward-operation non-propagating-operation))
      (sysdef-error
       (compatfmt "~@<Required method ~S not implemented for ~/asdf-action:format-action/~@:>")
       'perform (cons o c))))

  (defmethod perform-with-restarts (operation component)
    ;; TOO verbose, especially as the default. Add your own :before method
    ;; to perform-with-restart or perform if you want that:
    #|(explain operation component)|#
    (perform operation component))
  (defmethod perform-with-restarts :around (operation component)
    (loop
      (restart-case
          (return (call-next-method))
        (retry ()
          :report
          (lambda (s)
            (format s (compatfmt "~@<Retry ~A.~@:>")
                    (action-description operation component))))
        (accept ()
          :report
          (lambda (s)
            (format s (compatfmt "~@<Continue, treating ~A as having been successful.~@:>")
                    (action-description operation component)))
          (mark-operation-done operation component)
          (return))))))
;;;; -------------------------------------------------------------------------
;;;; Actions to build Common Lisp software

(uiop/package:define-package :asdf/lisp-action
  (:recycle :asdf/lisp-action :asdf)
  (:intern #:proclamations #:flags)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/cache
   :asdf/component :asdf/system :asdf/find-component :asdf/find-system
   :asdf/operation :asdf/action)
  (:export
   #:try-recompiling
   #:cl-source-file #:cl-source-file.cl #:cl-source-file.lsp
   #:basic-load-op #:basic-compile-op #:compile-op-flags #:compile-op-proclamations
   #:load-op #:prepare-op #:compile-op #:test-op #:load-source-op #:prepare-source-op
   #:call-with-around-compile-hook
   #:perform-lisp-compilation #:perform-lisp-load-fasl #:perform-lisp-load-source
   #:lisp-compilation-output-files #:flags))
(in-package :asdf/lisp-action)


;;;; Component classes
(with-upgradability ()
  (defclass cl-source-file (source-file)
    ((type :initform "lisp")))
  (defclass cl-source-file.cl (cl-source-file)
    ((type :initform "cl")))
  (defclass cl-source-file.lsp (cl-source-file)
    ((type :initform "lsp"))))


;;;; Operation classes
(with-upgradability ()
  (defclass basic-load-op (operation) ())
  (defclass basic-compile-op (operation)
    ((proclamations :initarg :proclamations :accessor compile-op-proclamations :initform nil)
     (flags :initarg :flags :accessor compile-op-flags :initform nil))))

;;; Our default operations: loading into the current lisp image
(with-upgradability ()
  (defclass prepare-op (upward-operation sideway-operation)
    ((sideway-operation :initform 'load-op :allocation :class))
    (:documentation "Load dependencies necessary for COMPILE-OP or LOAD-OP of a given COMPONENT."))
  (defclass load-op (basic-load-op downward-operation selfward-operation)
    ;; NB: even though compile-op depends on prepare-op it is not needed-in-image-p,
    ;; so we need to directly depend on prepare-op for its side-effects in the current image.
    ((selfward-operation :initform '(prepare-op compile-op) :allocation :class)))
  (defclass compile-op (basic-compile-op downward-operation selfward-operation)
    ((selfward-operation :initform 'prepare-op :allocation :class)))

  (defclass prepare-source-op (upward-operation sideway-operation)
    ((sideway-operation :initform 'load-source-op :allocation :class)))
  (defclass load-source-op (basic-load-op downward-operation selfward-operation)
    ((selfward-operation :initform 'prepare-source-op :allocation :class)))

  (defclass test-op (selfward-operation)
    ((selfward-operation :initform 'load-op :allocation :class))))


;;;; prepare-op, compile-op and load-op

;;; prepare-op
(with-upgradability ()
  (defmethod action-description ((o prepare-op) (c component))
    (format nil (compatfmt "~@<loading dependencies of ~3i~_~A~@:>") c))
  (defmethod perform ((o prepare-op) (c component))
    nil)
  (defmethod input-files ((o prepare-op) (s system))
    (if-let (it (system-source-file s)) (list it))))

;;; compile-op
(with-upgradability ()
  (defmethod action-description ((o compile-op) (c component))
    (format nil (compatfmt "~@<compiling ~3i~_~A~@:>") c))
  (defmethod action-description ((o compile-op) (c parent-component))
    (format nil (compatfmt "~@<completing compilation for ~3i~_~A~@:>") c))
  (defgeneric call-with-around-compile-hook (component thunk))
  (defmethod call-with-around-compile-hook ((c component) function)
    (call-around-hook (around-compile-hook c) function))
  (defun perform-lisp-compilation (o c)
    (let (;; Before 2.26.53, that was unfortunately component-pathname. Now,
          ;; we consult input-files, the first of which should be the one to compile-file
          (input-file (first (input-files o c)))
          ;; on some implementations, there are more than one output-file,
          ;; but the first one should always be the primary fasl that gets loaded.
          (outputs (output-files o c)))
      (multiple-value-bind (output warnings-p failure-p)
          (destructuring-bind
              (output-file
               &optional
                 #+clisp lib-file
                 #+(or ecl mkcl) object-file
                 warnings-file) outputs
            (call-with-around-compile-hook
             c #'(lambda (&rest flags)
                   (apply 'compile-file* input-file
                          :output-file output-file
                          :external-format (component-external-format c)
                          :warnings-file warnings-file
                          (append
                           #+clisp (list :lib-file lib-file)
                           #+(or ecl mkcl) (list :object-file object-file)
                           flags (compile-op-flags o))))))
        (check-lisp-compile-results output warnings-p failure-p
                                    "~/asdf-action::format-action/" (list (cons o c))))))

  (defun report-file-p (f)
    (equalp (pathname-type f) "build-report"))
  (defun perform-lisp-warnings-check (o c)
    (let* ((expected-warnings-files (remove-if-not #'warnings-file-p (input-files o c)))
           (actual-warnings-files (loop :for w :in expected-warnings-files
                                        :when (get-file-stamp w)
                                          :collect w
                                        :else :do (warn "Missing warnings file ~S while ~A"
                                                        w (action-description o c)))))
      (check-deferred-warnings actual-warnings-files)
      (let* ((output (output-files o c))
             (report (find-if #'report-file-p output)))
        (when report
          (with-open-file (s report :direction :output :if-exists :supersede)
            (format s ":success~%"))))))
  (defmethod perform ((o compile-op) (c cl-source-file))
    (perform-lisp-compilation o c))
  (defun lisp-compilation-output-files (o c)
    (let* ((i (first (input-files o c)))
           (f (compile-file-pathname
               i #+mkcl :fasl-p #+mkcl t #+ecl :type #+ecl :fasl)))
      `(,f ;; the fasl is the primary output, in first position
        #+clisp
        ,@`(,(make-pathname :type "lib" :defaults f))
        #+ecl
        ,@(unless (use-ecl-byte-compiler-p)
            `(,(compile-file-pathname i :type :object)))
        #+mkcl
        ,(compile-file-pathname i :fasl-p nil) ;; object file
        ,@(when (and *warnings-file-type* (not (builtin-system-p (component-system c))))
            `(,(make-pathname :type *warnings-file-type* :defaults f))))))
  (defmethod output-files ((o compile-op) (c cl-source-file))
    (lisp-compilation-output-files o c))
  (defmethod perform ((o compile-op) (c static-file))
    nil)
  (defmethod perform ((o compile-op) (c system))
    (when (and *warnings-file-type* (not (builtin-system-p c)))
      (perform-lisp-warnings-check o c)))
  (defmethod input-files ((o compile-op) (c system))
    (when (and *warnings-file-type* (not (builtin-system-p c)))
      ;; The most correct way to do it would be to use:
      ;; (traverse-sub-actions o c :other-systems nil :keep-operation 'compile-op :keep-component 'cl-source-file)
      ;; but it's expensive and we don't care too much about file order or ASDF extensions.
      (loop :for sub :in (sub-components c :type 'cl-source-file)
            :nconc (remove-if-not 'warnings-file-p (output-files o sub)))))
  (defmethod output-files ((o compile-op) (c system))
    (when (and *warnings-file-type* (not (builtin-system-p c)))
      (if-let ((pathname (component-pathname c)))
        (list (subpathname pathname (coerce-filename c) :type "build-report"))))))

;;; load-op
(with-upgradability ()
  (defmethod action-description ((o load-op) (c cl-source-file))
    (format nil (compatfmt "~@<loading FASL for ~3i~_~A~@:>") c))
  (defmethod action-description ((o load-op) (c parent-component))
    (format nil (compatfmt "~@<completing load for ~3i~_~A~@:>") c))
  (defmethod action-description ((o load-op) (c component))
    (format nil (compatfmt "~@<loading ~3i~_~A~@:>") c))
  (defmethod perform-with-restarts ((o load-op) (c cl-source-file))
    (loop
      (restart-case
          (return (call-next-method))
        (try-recompiling ()
          :report (lambda (s)
                    (format s "Recompile ~a and try loading it again"
                            (component-name c)))
          (perform (find-operation o 'compile-op) c)))))
  (defun perform-lisp-load-fasl (o c)
    (if-let (fasl (first (input-files o c)))
      (load* fasl)))
  (defmethod perform ((o load-op) (c cl-source-file))
    (perform-lisp-load-fasl o c))
  (defmethod perform ((o load-op) (c static-file))
    nil))


;;;; prepare-source-op, load-source-op

;;; prepare-source-op
(with-upgradability ()
  (defmethod action-description ((o prepare-source-op) (c component))
    (format nil (compatfmt "~@<loading source for dependencies of ~3i~_~A~@:>") c))
  (defmethod input-files ((o prepare-source-op) (s system))
    (if-let (it (system-source-file s)) (list it)))
  (defmethod perform ((o prepare-source-op) (c component))
    nil))

;;; load-source-op
(with-upgradability ()
  (defmethod action-description ((o load-source-op) (c component))
    (format nil (compatfmt "~@<Loading source of ~3i~_~A~@:>") c))
  (defmethod action-description ((o load-source-op) (c parent-component))
    (format nil (compatfmt "~@<Loaded source of ~3i~_~A~@:>") c))
  (defun perform-lisp-load-source (o c)
    (call-with-around-compile-hook
     c #'(lambda ()
           (load* (first (input-files o c))
                  :external-format (component-external-format c)))))

  (defmethod perform ((o load-source-op) (c cl-source-file))
    (perform-lisp-load-source o c))
  (defmethod perform ((o load-source-op) (c static-file))
    nil))


;;;; test-op
(with-upgradability ()
  (defmethod perform ((o test-op) (c component))
    nil)
  (defmethod operation-done-p ((o test-op) (c system))
    "Testing a system is _never_ done."
    nil))

;;;; -------------------------------------------------------------------------
;;;; Plan

(uiop/package:define-package :asdf/plan
  (:recycle :asdf/plan :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
   :asdf/component :asdf/operation :asdf/system
   :asdf/cache :asdf/find-system :asdf/find-component
   :asdf/operation :asdf/action :asdf/lisp-action)
  (:export
   #:component-operation-time #:mark-operation-done
   #:plan #:plan-traversal #:sequential-plan #:*default-plan-class*
   #:planned-action-status #:plan-action-status #:action-already-done-p
   #:circular-dependency #:circular-dependency-actions
   #:node-for #:needed-in-image-p
   #:action-index #:action-planned-p #:action-valid-p
   #:plan-record-dependency
   #:normalize-forced-systems #:action-forced-p #:action-forced-not-p
   #:map-direct-dependencies #:reduce-direct-dependencies #:direct-dependencies
   #:compute-action-stamp #:traverse-action
   #:circular-dependency #:circular-dependency-actions
   #:call-while-visiting-action #:while-visiting-action
   #:make-plan #:plan-actions #:perform-plan #:plan-operates-on-p
   #:planned-p #:index #:forced #:forced-not #:total-action-count
   #:planned-action-count #:planned-output-action-count #:visited-actions
   #:visiting-action-set #:visiting-action-list #:plan-actions-r
   #:required-components #:filtered-sequential-plan
   #:plan-system
   #:plan-action-filter #:plan-component-type #:plan-keep-operation #:plan-keep-component
   #:traverse-actions #:traverse-sub-actions))
(in-package :asdf/plan)

;;;; Generic plan traversal class
(with-upgradability ()
  (defclass plan () ())
  (defclass plan-traversal (plan)
    ((system :initform nil :initarg :system :accessor plan-system)
     (forced :initform nil :initarg :force :accessor plan-forced)
     (forced-not :initform nil :initarg :force-not :accessor plan-forced-not)
     (total-action-count :initform 0 :accessor plan-total-action-count)
     (planned-action-count :initform 0 :accessor plan-planned-action-count)
     (planned-output-action-count :initform 0 :accessor plan-planned-output-action-count)
     (visited-actions :initform (make-hash-table :test 'equal) :accessor plan-visited-actions)
     (visiting-action-set :initform (make-hash-table :test 'equal) :accessor plan-visiting-action-set)
     (visiting-action-list :initform () :accessor plan-visiting-action-list))))


;;;; Planned action status
(with-upgradability ()
  (defgeneric plan-action-status (plan operation component)
    (:documentation "Returns the ACTION-STATUS associated to
the action of OPERATION on COMPONENT in the PLAN"))

  (defgeneric (setf plan-action-status) (new-status plan operation component)
    (:documentation "Sets the ACTION-STATUS associated to
the action of OPERATION on COMPONENT in the PLAN"))

  (defclass planned-action-status (action-status)
    ((planned-p
      :initarg :planned-p :reader action-planned-p
      :documentation "a boolean, true iff the action was included in the plan.")
     (index
      :initarg :index :reader action-index
      :documentation "an integer, counting all traversed actions in traversal order."))
    (:documentation "Status of an action in a plan"))

  (defmethod print-object ((status planned-action-status) stream)
    (print-unreadable-object (status stream :type t :identity nil)
      (with-slots (stamp done-p planned-p index) status
        (format stream "~@{~S~^ ~}" :stamp stamp :done-p done-p :planned-p planned-p :index index))))

  (defmethod action-planned-p ((action-status t))
    t) ; default method for non planned-action-status objects

  ;; TODO: eliminate NODE-FOR, use CONS.
  ;; Supposes cleaner protocol for operation initargs passed to MAKE-OPERATION.
  ;; However, see also component-operation-time and mark-operation-done
  (defun node-for (o c) (cons (type-of o) c))

  (defun action-already-done-p (plan operation component)
    (action-done-p (plan-action-status plan operation component)))

  (defmethod plan-action-status ((plan null) (o operation) (c component))
    (multiple-value-bind (stamp done-p) (component-operation-time o c)
      (make-instance 'action-status :stamp stamp :done-p done-p)))

  (defmethod (setf plan-action-status) (new-status (plan null) (o operation) (c component))
    (let ((to (type-of o))
          (times (component-operation-times c)))
      (if (action-done-p new-status)
          (remhash to times)
          (setf (gethash to times) (action-stamp new-status))))
    new-status))


;;;; forcing
(with-upgradability ()
  (defgeneric action-forced-p (plan operation component))
  (defgeneric action-forced-not-p (plan operation component))

  (defun normalize-forced-systems (x system)
    (etypecase x
      ((or (member nil :all) hash-table function) x)
      (cons (list-to-hash-set (mapcar #'coerce-name x)))
      ((eql t) (when system (list-to-hash-set (list (coerce-name system)))))))

  (defun normalize-forced-not-systems (x system)
    (let ((requested
            (etypecase x
              ((or (member nil :all) hash-table function) x)
              (cons (list-to-hash-set (mapcar #'coerce-name x)))
              ((eql t) (if system (let ((name (coerce-name system)))
                                    #'(lambda (x) (not (equal x name))))
                           t)))))
      (if (and *immutable-systems* requested)
          #'(lambda (x) (or (call-function requested x) (call-function *immutable-systems* x)))
          (or *immutable-systems* requested))))

  (defun action-override-p (plan operation component override-accessor)
    (declare (ignore operation))
    (call-function (funcall override-accessor plan)
                   (coerce-name (component-system (find-component () component)))))

  (defmethod action-forced-p (plan operation component)
    (and
     ;; Did the user ask us to re-perform the action?
     (action-override-p plan operation component 'plan-forced)
     ;; You really can't force a builtin system and :all doesn't apply to it,
     ;; except it it's the specifically the system currently being built.
     (not (let ((system (component-system component)))
            (and (builtin-system-p system)
                 (not (eq system (plan-system plan))))))))

  (defmethod action-forced-not-p (plan operation component)
    ;; Did the user ask us to not re-perform the action?
    ;; NB: force-not takes precedence over force, as it should
    (action-override-p plan operation component 'plan-forced-not))

  (defmethod action-forced-p ((plan null) (operation operation) (component component))
    nil)

  (defmethod action-forced-not-p ((plan null) (operation operation) (component component))
    nil))


;;;; action-valid-p
(with-upgradability ()
  (defgeneric action-valid-p (plan operation component)
    (:documentation "Is this action valid to include amongst dependencies?"))
  (defmethod action-valid-p ((plan t) (o operation) (c component))
    (if-let (it (component-if-feature c)) (featurep it) t))
  (defmethod action-valid-p ((plan t) (o null) (c t)) nil)
  (defmethod action-valid-p ((plan t) (o t) (c null)) nil)
  (defmethod action-valid-p ((plan null) (o operation) (c component)) t))

;;;; Is the action needed in this image?
(with-upgradability ()
  (defgeneric needed-in-image-p (operation component)
    (:documentation "Is the action of OPERATION on COMPONENT needed in the current image to be meaningful,
    or could it just as well have been done in another Lisp image?"))

  (defmethod needed-in-image-p ((o operation) (c component))
    ;; We presume that actions that modify the filesystem don't need be run
    ;; in the current image if they have already been done in another,
    ;; and can be run in another process (e.g. a fork),
    ;; whereas those that don't are meant to side-effect the current image and can't.
    (not (output-files o c))))


;;;; Visiting dependencies of an action and computing action stamps
(with-upgradability ()
  (defun (map-direct-dependencies) (plan operation component fun)
    (loop* :for (dep-o-spec . dep-c-specs) :in (component-depends-on operation component)
           :for dep-o = (find-operation operation dep-o-spec)
           :when dep-o
           :do (loop :for dep-c-spec :in dep-c-specs
                     :for dep-c = (and dep-c-spec (resolve-dependency-spec component dep-c-spec))
                     :when (and dep-c (action-valid-p plan dep-o dep-c))
                       :do (funcall fun dep-o dep-c))))

  (defun (reduce-direct-dependencies) (plan operation component combinator seed)
    (map-direct-dependencies
     plan operation component
     #'(lambda (dep-o dep-c)
         (setf seed (funcall combinator dep-o dep-c seed))))
    seed)

  (defun (direct-dependencies) (plan operation component)
    (reduce-direct-dependencies plan operation component #'acons nil))

  ;; In a distant future, get-file-stamp, component-operation-time and latest-stamp
  ;; shall also be parametrized by the plan, or by a second model object,
  ;; so they need not refer to the state of the filesystem,
  ;; and the stamps could be cryptographic checksums rather than timestamps.
  ;; Such a change remarkably would only affect COMPUTE-ACTION-STAMP.

  (defmethod compute-action-stamp (plan (o operation) (c component) &key just-done)
    ;; Given an action, figure out at what time in the past it has been done,
    ;; or if it has just been done, return the time that it has.
    ;; Returns two values:
    ;; 1- the TIMESTAMP of the action if it has already been done and is up to date,
    ;;   or T is either hasn't been done or is out of date.
    ;; 2- the DONE-IN-IMAGE-P boolean flag that is T if the action has already been done
    ;;   in the current image, or NIL if it hasn't.
    ;; Note that if e.g. LOAD-OP only depends on up-to-date files, but
    ;; hasn't been done in the current image yet, then it can have a non-T timestamp,
    ;; yet a NIL done-in-image-p flag.
    (nest
     (block ())
     (let ((dep-stamp ; collect timestamp from dependencies (or T if forced or out-of-date)
             (reduce-direct-dependencies
              plan o c
              #'(lambda (o c stamp)
                  (if-let (it (plan-action-status plan o c))
                    (latest-stamp stamp (action-stamp it))
                    t))
              nil)))
       ;; out-of-date dependency: don't bother expensively querying the filesystem
       (when (and (eq dep-stamp t) (not just-done)) (return (values t nil))))
     ;; collect timestamps from inputs, and exit early if any is missing
     (let* ((in-files (input-files o c))
            (in-stamps (mapcar #'get-file-stamp in-files))
            (missing-in (loop :for f :in in-files :for s :in in-stamps :unless s :collect f))
            (latest-in (stamps-latest (cons dep-stamp in-stamps))))
       (when (and missing-in (not just-done)) (return (values t nil))))
     ;; collect timestamps from outputs, and exit early if any is missing
     (let* ((out-files (output-files o c))
            (out-stamps (mapcar (if just-done 'register-file-stamp 'get-file-stamp) out-files))
            (missing-out (loop :for f :in out-files :for s :in out-stamps :unless s :collect f))
            (earliest-out (stamps-earliest out-stamps)))
       (when (and missing-out (not just-done)) (return (values t nil))))
     (let* (;; There are three kinds of actions:
            (out-op (and out-files t)) ; those that create files on the filesystem
            ;;(image-op (and in-files (null out-files))) ; those that load stuff into the image
            ;;(null-op (and (null out-files) (null in-files))) ; placeholders that do nothing
            ;; When was the thing last actually done? (Now, or ask.)
            (op-time (or just-done (component-operation-time o c)))
            ;; Time stamps from the files at hand, and whether any is missing
            (all-present (not (or missing-in missing-out)))
            ;; Has any input changed since we last generated the files?
            (up-to-date-p (stamp<= latest-in earliest-out))
            ;; If everything is up to date, the latest of inputs and outputs is our stamp
            (done-stamp (stamps-latest (cons latest-in out-stamps))))
       ;; Warn if some files are missing:
       ;; either our model is wrong or some other process is messing with our files.
       (when (and just-done (not all-present))
         (warn "~A completed without ~:[~*~;~*its input file~:p~2:*~{ ~S~}~*~]~
                ~:[~; or ~]~:[~*~;~*its output file~:p~2:*~{ ~S~}~*~]"
               (action-description o c)
               missing-in (length missing-in) (and missing-in missing-out)
               missing-out (length missing-out))))
     ;; Note that we use stamp<= instead of stamp< to play nice with generated files.
     ;; Any race condition is intrinsic to the limited timestamp resolution.
     (if (or just-done ;; The done-stamp is valid: if we're just done, or
             ;; if all filesystem effects are up-to-date and there's no invalidating reason.
             (and all-present up-to-date-p (operation-done-p o c) (not (action-forced-p plan o c))))
         (values done-stamp ;; return the hard-earned timestamp
                 (or just-done
                     out-op ;; a file-creating op is done when all files are up to date
                     ;; a image-effecting a placeholder op is done when it was actually run,
                     (and op-time (eql op-time done-stamp)))) ;; with the matching stamp
         ;; done-stamp invalid: return a timestamp in an indefinite future, action not done yet
         (values t nil)))))


;;;; Generic support for plan-traversal
(with-upgradability ()
  (defgeneric plan-record-dependency (plan operation component))

  (defgeneric call-while-visiting-action (plan operation component function)
    (:documentation "Detect circular dependencies"))

  (defmethod initialize-instance :after ((plan plan-traversal)
                                         &key force force-not system
                                         &allow-other-keys)
    (with-slots (forced forced-not) plan
      (setf forced (normalize-forced-systems force system))
      (setf forced-not (normalize-forced-not-systems force-not system))))

  (defmethod (setf plan-action-status) (new-status (plan plan-traversal) (o operation) (c component))
    (setf (gethash (node-for o c) (plan-visited-actions plan)) new-status))

  (defmethod plan-action-status ((plan plan-traversal) (o operation) (c component))
    (or (and (action-forced-not-p plan o c) (plan-action-status nil o c))
        (values (gethash (node-for o c) (plan-visited-actions plan)))))

  (defmethod action-valid-p ((plan plan-traversal) (o operation) (s system))
    (and (not (action-forced-not-p plan o s)) (call-next-method)))

  (defmethod call-while-visiting-action ((plan plan-traversal) operation component fun)
    (with-accessors ((action-set plan-visiting-action-set)
                     (action-list plan-visiting-action-list)) plan
      (let ((action (cons operation component)))
        (when (gethash action action-set)
          (error 'circular-dependency :actions
                 (member action (reverse action-list) :test 'equal)))
        (setf (gethash action action-set) t)
        (push action action-list)
        (unwind-protect
             (funcall fun)
          (pop action-list)
          (setf (gethash action action-set) nil))))))


;;;; Actual traversal: traverse-action
(with-upgradability ()
  (define-condition circular-dependency (system-definition-error)
    ((actions :initarg :actions :reader circular-dependency-actions))
    (:report (lambda (c s)
               (format s (compatfmt "~@<Circular dependency: ~3i~_~S~@:>")
                       (circular-dependency-actions c)))))

  (defmacro while-visiting-action ((p o c) &body body)
    `(call-while-visiting-action ,p ,o ,c #'(lambda () ,@body)))

  (defgeneric traverse-action (plan operation component needed-in-image-p))

  ;; TRAVERSE-ACTION, in the context of a given PLAN object that accumulates dependency data,
  ;; visits the action defined by its OPERATION and COMPONENT arguments,
  ;; and all its transitive dependencies (unless already visited),
  ;; in the context of the action being (or not) NEEDED-IN-IMAGE-P,
  ;; i.e. needs to be done in the current image vs merely have been done in a previous image.
  ;; For actions that are up-to-date, it returns a STAMP identifying the state of the action
  ;; (that's timestamp, but it could be a cryptographic digest in some ASDF extension),
  ;; or T if the action needs to be done again.
  ;;
  ;; Note that for an XCVB-like plan with one-image-per-file-outputting-action,
  ;; the below method would be insufficient, since it assumes a single image
  ;; to traverse each node at most twice; non-niip actions would be traversed only once,
  ;; but niip nodes could be traversed once per image, i.e. once plus once per non-niip action.

  (defmethod traverse-action (plan operation component needed-in-image-p)
    (block nil
      ;; ACTION-VALID-P among other things, handles forcing logic, including FORCE-NOT,
      ;; and IF-FEATURE filtering.
      (unless (action-valid-p plan operation component) (return nil))
      ;; the following hook is needed by POIU, which tracks a full dependency graph,
      ;; instead of just a dependency order as in vanilla ASDF
      (plan-record-dependency plan operation component)
      ;; needed in image distinguishes b/w things that must happen in the
      ;; current image and those things that simply need to have been done in a previous one.
      (let* ((aniip (needed-in-image-p operation component)) ; action-specific needed-in-image
             ;; effective niip: meaningful for the action and required by the plan as traversed
             (eniip (and aniip needed-in-image-p))
             ;; status: have we traversed that action previously, and if so what was its status?
             (status (plan-action-status plan operation component)))
        (when (and status (or (action-done-p status) (action-planned-p status) (not eniip)))
          (return (action-stamp status))) ; Already visited with sufficient need-in-image level!
        (labels ((visit-action (niip) ; We may visit the action twice, once with niip NIL, then T
                   (map-direct-dependencies ; recursively traverse dependencies
                    plan operation component #'(lambda (o c) (traverse-action plan o c niip)))
                   (multiple-value-bind (stamp done-p) ; AFTER dependencies have been traversed,
                       (compute-action-stamp plan operation component) ; compute action stamp
                     (let ((add-to-plan-p (or (eql stamp t) (and niip (not done-p)))))
                       (cond ; it needs be done if it's out of date or needed in image but absent
                         ((and add-to-plan-p (not niip)) ; if we need to do it,
                          (visit-action t)) ; then we need to do it *in the (current) image*!
                         (t
                          (setf (plan-action-status plan operation component) ; update status:
                                (make-instance
                                 'planned-action-status
                                 :stamp stamp ; computed stamp
                                 :done-p (and done-p (not add-to-plan-p)) ; done *and* up-to-date?
                                 :planned-p add-to-plan-p ; included in list of things to be done?
                                 :index (if status ; index of action amongst all nodes in traversal
                                            (action-index status) ;; if already visited, keep index
                                            (incf (plan-total-action-count plan))))) ; else new index
                          (when add-to-plan-p ; if it needs to be added to the plan,
                            (incf (plan-planned-action-count plan)) ; count it
                            (unless aniip ; if it's output-producing,
                              (incf (plan-planned-output-action-count plan)))) ; count it
                          stamp)))))) ; return the stamp
          (while-visiting-action (plan operation component) ; maintain context, handle circularity.
            (visit-action eniip))))))) ; visit the action


;;;; Sequential plans (the default)
(with-upgradability ()
  (defclass sequential-plan (plan-traversal)
    ((actions-r :initform nil :accessor plan-actions-r)))

  (defgeneric plan-actions (plan))
  (defmethod plan-actions ((plan list))
    plan)
  (defmethod plan-actions ((plan sequential-plan))
    (reverse (plan-actions-r plan)))

  (defmethod plan-record-dependency ((plan sequential-plan) (o operation) (c component))
    (values))

  (defmethod (setf plan-action-status) :after
      (new-status (p sequential-plan) (o operation) (c component))
    (when (action-planned-p new-status)
      (push (cons o c) (plan-actions-r p)))))

;;;; High-level interface: traverse, perform-plan, plan-operates-on-p
(with-upgradability ()
  (defgeneric make-plan (plan-class operation component &key &allow-other-keys)
    (:documentation
     "Generate and return a plan for performing OPERATION on COMPONENT."))
  (define-convenience-action-methods make-plan (plan-class operation component &key))

  (defgeneric perform-plan (plan &key))
  (defgeneric plan-operates-on-p (plan component))

  (defvar *default-plan-class* 'sequential-plan)

  (defmethod make-plan (plan-class (o operation) (c component) &rest keys &key &allow-other-keys)
    (let ((plan (apply 'make-instance (or plan-class *default-plan-class*)
                       :system (component-system c) keys)))
      (traverse-action plan o c t)
      plan))

  (defmethod perform-plan :around ((plan t) &key)
    #+xcl (declare (ignorable plan))
    (let ((*package* *package*)
          (*readtable* *readtable*))
      (with-compilation-unit () ;; backward-compatibility.
        (call-next-method))))   ;; Going forward, see deferred-warning support in lisp-build.

  (defmethod perform-plan ((plan t) &rest keys &key &allow-other-keys)
    (apply 'perform-plan (plan-actions plan) keys))

  (defmethod perform-plan ((steps list) &key force &allow-other-keys)
    (loop* :for (o . c) :in steps
           :when (or force (not (nth-value 1 (compute-action-stamp nil o c))))
           :do (perform-with-restarts o c)))

  (defmethod plan-operates-on-p ((plan plan-traversal) (component-path list))
    (plan-operates-on-p (plan-actions plan) component-path))

  (defmethod plan-operates-on-p ((plan list) (component-path list))
    (find component-path (mapcar 'cdr plan)
          :test 'equal :key 'component-find-path)))


;;;; Incidental traversals

;;; Making a FILTERED-SEQUENTIAL-PLAN can be used to, e.g., all of the source
;;; files required by a bundling operation.
(with-upgradability ()
  (defclass filtered-sequential-plan (sequential-plan)
    ((action-filter :initform t :initarg :action-filter :reader plan-action-filter)
     (component-type :initform t :initarg :component-type :reader plan-component-type)
     (keep-operation :initform t :initarg :keep-operation :reader plan-keep-operation)
     (keep-component :initform t :initarg :keep-component :reader plan-keep-component)))

  (defmethod initialize-instance :after ((plan filtered-sequential-plan)
                                         &key force force-not
                                           other-systems)
    (declare (ignore force force-not))
    (with-slots (forced forced-not action-filter system) plan
      (setf forced (normalize-forced-systems (if other-systems :all t) system))
      (setf forced-not (normalize-forced-not-systems (if other-systems nil t) system))
      (setf action-filter (ensure-function action-filter))))

  (defmethod action-valid-p ((plan filtered-sequential-plan) o c)
    (and (funcall (plan-action-filter plan) o c)
         (typep c (plan-component-type plan))
         (call-next-method)))

  (defmethod traverse-actions (actions &rest keys &key plan-class &allow-other-keys)
    (let ((plan (apply 'make-instance (or plan-class 'filtered-sequential-plan) keys)))
      (loop* :for (o . c) :in actions :do (traverse-action plan o c t))
      plan))

  (define-convenience-action-methods traverse-sub-actions (operation component &key))
  (defmethod traverse-sub-actions ((operation operation) (component component)
                                   &rest keys &key &allow-other-keys)
    (apply 'traverse-actions (direct-dependencies t operation component)
           :system (component-system component) keys))

  (defmethod plan-actions ((plan filtered-sequential-plan))
    (with-slots (keep-operation keep-component) plan
      (loop* :for (o . c) :in (call-next-method)
             :when (and (typep o keep-operation) (typep c keep-component))
             :collect (cons o c))))

  (defmethod required-components (system &rest keys &key (goal-operation 'load-op) &allow-other-keys)
    (remove-duplicates
     (mapcar 'cdr (plan-actions
                   (apply 'traverse-sub-actions goal-operation system
                          (remove-plist-key :goal-operation keys))))
     :from-end t)))

;;;; -------------------------------------------------------------------------
;;;; Invoking Operations

(uiop/package:define-package :asdf/operate
  (:recycle :asdf/operate :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/cache
   :asdf/component :asdf/system :asdf/operation :asdf/action
   :asdf/find-system :asdf/find-component :asdf/lisp-action :asdf/plan)
  (:export
   #:operate #:oos
   #:*systems-being-operated*
   #:build-op #:make
   #:load-system #:load-systems #:load-systems*
   #:compile-system #:test-system #:require-system
   #:*load-system-operation* #:module-provide-asdf
   #:component-loaded-p #:already-loaded-systems))
(in-package :asdf/operate)

(with-upgradability ()
  (defgeneric* (operate) (operation component &key &allow-other-keys)
    (:documentation
     "Operate does three things:

1. It creates an instance of OPERATION-CLASS using any keyword parameters as initargs.
2. It finds the  asdf-system specified by SYSTEM (possibly loading it from disk).
3. It then calls MAKE-PLAN with the operation and system as arguments

The operation of making a plan is wrapped in WITH-COMPILATION-UNIT and error
handling code.  If a VERSION argument is supplied, then operate also ensures
that the system found satisfies it using the VERSION-SATISFIES method.

Note that dependencies may cause the operation to invoke other operations on the system
or its components: the new operations will be created with the same initargs as the original one.

The :FORCE or :FORCE-NOT argument to OPERATE can be:
  T to force the inside of the specified system to be rebuilt (resp. not),
    without recursively forcing the other systems we depend on.
  :ALL to force all systems including other systems we depend on to be rebuilt (resp. not).
  (SYSTEM1 SYSTEM2 ... SYSTEMN) to force systems named in a given list
:FORCE has precedence over :FORCE-NOT; builtin systems cannot be forced."))

  (define-convenience-action-methods
      operate (operation component &key)
      ;; I'd like to at least remove-plist-keys :force :force-not :verbose,
      ;; but swank.asd relies on :force (!).
      :operation-initargs t ;; backward-compatibility with ASDF1. Yuck.
      :if-no-component (error 'missing-component :requires component))

  (defvar *systems-being-operated* nil
    "A boolean indicating that some systems are being operated on")

  (defmethod operate :around (operation component &rest keys
                              &key verbose
                                (on-warnings *compile-file-warnings-behaviour*)
                                (on-failure *compile-file-failure-behaviour*) &allow-other-keys)
    (let* ((systems-being-operated *systems-being-operated*)
           (*systems-being-operated* (or systems-being-operated (make-hash-table :test 'equal)))
           (operation-remaker ;; how to remake the operation after ASDF was upgraded (if it was)
             (etypecase operation
               (operation (let ((name (type-of operation))
                                (initargs (operation-original-initargs operation)))
                            #'(lambda () (make-operation name :original-initargs initargs initargs))))
               ((or symbol string) (constantly operation))))
           (component-path (typecase component ;; to remake the component after ASDF upgrade
                             (component (component-find-path component))
                             (t component))))
      ;; Before we operate on any system, make sure ASDF is up-to-date,
      ;; for if an upgrade is ever attempted at any later time, there may be BIG trouble.
      (unless systems-being-operated
        (when (upgrade-asdf)
          ;; If we were upgraded, restart OPERATE the hardest of ways, for
          ;; its function may have been redefined, its symbol uninterned, its package deleted.
          (return-from operate
            (apply 'operate (funcall operation-remaker) component-path keys))))
      ;; Setup proper bindings around any operate call.
      (with-asdf-cache ()
        (let* ((*verbose-out* (and verbose *standard-output*))
               (*compile-file-warnings-behaviour* on-warnings)
               (*compile-file-failure-behaviour* on-failure))
          (call-next-method)))))

  (defmethod operate :before ((operation operation) (component component)
                              &key version &allow-other-keys)
    (let ((system (component-system component)))
      (setf (gethash (coerce-name system) *systems-being-operated*) system))
    (unless (version-satisfies component version)
      (error 'missing-component-of-version :requires component :version version)))

  (defmethod operate ((operation operation) (component component)
                      &rest keys &key plan-class &allow-other-keys)
    (let ((plan (apply 'make-plan plan-class operation component keys)))
      (apply 'perform-plan plan keys)
      (values operation plan)))

  (defun oos (operation component &rest args &key &allow-other-keys)
    (apply 'operate operation component args))

  (setf (documentation 'oos 'function)
        (format nil "Short for _operate on system_ and an alias for the OPERATE function.~%~%~a"
                (documentation 'operate 'function))))


;;;; Common operations
(with-upgradability ()
  (defvar *load-system-operation* 'load-op
    "Operation used by ASDF:LOAD-SYSTEM. By default, ASDF:LOAD-OP.
You may override it with e.g. ASDF:LOAD-FASL-OP from asdf-bundle
or ASDF:LOAD-SOURCE-OP if your fasl loading is somehow broken.

The default operation may change in the future if we implement a
component-directed strategy for how to load or compile systems.")

  (defmethod component-depends-on ((o prepare-op) (s system))
    `((,*load-system-operation* ,@(component-sideway-dependencies s))))

  (defclass build-op (non-propagating-operation) ()
    (:documentation "Since ASDF3, BUILD-OP is the recommended 'master' operation,
to operate by default on a system or component, via the function BUILD.
Its meaning is configurable via the :BUILD-OPERATION option of a component.
which typically specifies the name of a specific operation to which to delegate the build,
as a symbol or as a string later read as a symbol (after loading the defsystem-depends-on);
if NIL is specified (the default), BUILD-OP falls back to the *LOAD-SYSTEM-OPERATION*
that will load the system in the current image, and its typically LOAD-OP."))
  (defmethod component-depends-on ((o build-op) (c component))
    `((,(or (component-build-operation c) *load-system-operation*) ,c)))

  (defun make (system &rest keys)
    "The recommended way to interact with ASDF3.1 is via (ASDF:MAKE :FOO).
It will build system FOO using the operation BUILD-OP,
the meaning of which is configurable by the system, and
defaults to *LOAD-SYSTEM-OPERATION*, usually LOAD-OP,
to load it in current image."
    (apply 'operate 'build-op system keys)
    t)

  (defun load-system (system &rest keys &key force force-not verbose version &allow-other-keys)
    "Shorthand for `(operate 'asdf:load-op system)`. See OPERATE for details."
    (declare (ignore force force-not verbose version))
    (apply 'operate *load-system-operation* system keys)
    t)

  (defun load-systems* (systems &rest keys)
    "Loading multiple systems at once."
    (dolist (s systems) (apply 'load-system s keys)))

  (defun load-systems (&rest systems)
    "Loading multiple systems at once."
    (load-systems* systems))

  (defun compile-system (system &rest args &key force force-not verbose version &allow-other-keys)
    "Shorthand for `(asdf:operate 'asdf:compile-op system)`. See OPERATE for details."
    (declare (ignore force force-not verbose version))
    (apply 'operate 'compile-op system args)
    t)

  (defun test-system (system &rest args &key force force-not verbose version &allow-other-keys)
    "Shorthand for `(asdf:operate 'asdf:test-op system)`. See OPERATE for details."
    (declare (ignore force force-not verbose version))
    (apply 'operate 'test-op system args)
    t))


;;;; Define require-system, to be hooked into CL:REQUIRE when possible,
;; i.e. for ABCL, CLISP, ClozureCL, CMUCL, ECL, MKCL and SBCL
(with-upgradability ()
  (defun component-loaded-p (c)
    (action-already-done-p nil (make-instance 'load-op) (find-component c ())))

  (defun already-loaded-systems ()
    (remove-if-not 'component-loaded-p (registered-systems)))

  (defun require-system (s &rest keys &key &allow-other-keys)
    (apply 'load-system s :force-not (already-loaded-systems) keys))

  (defvar *modules-being-required* nil)

  (defclass require-system (system)
    ((module :initarg :module :initform nil :accessor required-module))
    (:documentation "A SYSTEM subclass whose processing is handled by
the implementation's REQUIRE rather than by internal ASDF mechanisms."))

  (defmethod perform ((o compile-op) (c require-system))
    nil)

  (defmethod perform ((o load-op) (s require-system))
    (let* ((module (or (required-module s) (coerce-name s)))
           (*modules-being-required* (cons module *modules-being-required*)))
      (assert (null (component-children s)))
      (require module)))

  (defmethod resolve-dependency-combination (component (combinator (eql :require)) arguments)
    (unless (length=n-p arguments 1)
      (error (compatfmt "~@<Bad dependency ~S for ~S. ~S takes only one argument~@:>")
             (cons combinator arguments) component combinator))
    (let* ((module (car arguments))
           (name (string-downcase module))
           (system (find-system name nil)))
      (assert module)
      ;;(unless (typep system '(or null require-system))
      ;;  (warn "~S depends on ~S but ~S is registered as a ~S"
      ;;        component (cons combinator arguments) module (type-of system)))
      (or system (let ((system (make-instance 'require-system :name name)))
                   (register-system system)
                   system))))

  (defun module-provide-asdf (name)
    (let ((module (string-downcase name)))
      (unless (member module *modules-being-required* :test 'equal)
        (let ((*modules-being-required* (cons module *modules-being-required*))
              #+sbcl (sb-impl::*requiring* (remove module sb-impl::*requiring* :test 'equal)))
          (handler-bind
              ((style-warning #'muffle-warning)
               (missing-component (constantly nil))
               (error #'(lambda (e)
                          (format *error-output* (compatfmt "~@<ASDF could not load ~(~A~) because ~A.~@:>~%")
                                  name e))))
            (let ((*verbose-out* (make-broadcast-stream)))
              (let ((system (find-system module nil)))
                (when system
                  (require-system system :verbose nil)
                  t)))))))))


;;;; Some upgrade magic
(with-upgradability ()
  (defun restart-upgraded-asdf ()
    ;; If we're in the middle of something, restart it.
    (when *asdf-cache*
      (let ((l (loop* :for (x y) :being :the hash-keys :of *asdf-cache*
                      :when (eq x 'find-system) :collect y)))
        (clrhash *asdf-cache*)
        (dolist (s l) (find-system s nil)))))
  (register-hook-function '*post-upgrade-restart-hook* 'restart-upgraded-asdf))


;;;; ---------------------------------------------------------------------------
;;;; asdf-output-translations

(uiop/package:define-package :asdf/output-translations
  (:recycle :asdf/output-translations :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade)
  (:export
   #:*output-translations* #:*output-translations-parameter*
   #:invalid-output-translation
   #:output-translations #:output-translations-initialized-p
   #:initialize-output-translations #:clear-output-translations
   #:disable-output-translations #:ensure-output-translations
   #:apply-output-translations
   #:validate-output-translations-directive #:validate-output-translations-form
   #:validate-output-translations-file #:validate-output-translations-directory
   #:parse-output-translations-string #:wrapping-output-translations
   #:user-output-translations-pathname #:system-output-translations-pathname
   #:user-output-translations-directory-pathname #:system-output-translations-directory-pathname
   #:environment-output-translations #:process-output-translations
   #:compute-output-translations
   #+abcl #:translate-jar-pathname
   ))
(in-package :asdf/output-translations)

(when-upgrading () (undefine-function '(setf output-translations)))

(with-upgradability ()
  (define-condition invalid-output-translation (invalid-configuration warning)
    ((format :initform (compatfmt "~@<Invalid asdf output-translation ~S~@[ in ~S~]~@{ ~@?~}~@:>"))))

  (defvar *output-translations* ()
    "Either NIL (for uninitialized), or a list of one element,
said element itself being a sorted list of mappings.
Each mapping is a pair of a source pathname and destination pathname,
and the order is by decreasing length of namestring of the source pathname.")

  (defun output-translations ()
    (car *output-translations*))

  (defun set-output-translations (new-value)
    (setf *output-translations*
          (list
           (stable-sort (copy-list new-value) #'>
                        :key #'(lambda (x)
                                 (etypecase (car x)
                                   ((eql t) -1)
                                   (pathname
                                    (let ((directory (pathname-directory (car x))))
                                      (if (listp directory) (length directory) 0))))))))
    new-value)
  (defun* ((setf output-translations)) (new-value) (set-output-translations new-value))

  (defun output-translations-initialized-p ()
    (and *output-translations* t))

  (defun clear-output-translations ()
    "Undoes any initialization of the output translations."
    (setf *output-translations* '())
    (values))
  (register-clear-configuration-hook 'clear-output-translations)

  (defun validate-output-translations-directive (directive)
    (or (member directive '(:enable-user-cache :disable-cache nil))
        (and (consp directive)
             (or (and (length=n-p directive 2)
                      (or (and (eq (first directive) :include)
                               (typep (second directive) '(or string pathname null)))
                          (and (location-designator-p (first directive))
                               (or (location-designator-p (second directive))
                                   (location-function-p (second directive))))))
                 (and (length=n-p directive 1)
                      (location-designator-p (first directive)))))))

  (defun validate-output-translations-form (form &key location)
    (validate-configuration-form
     form
     :output-translations
     'validate-output-translations-directive
     :location location :invalid-form-reporter 'invalid-output-translation))

  (defun validate-output-translations-file (file)
    (validate-configuration-file
     file 'validate-output-translations-form :description "output translations"))

  (defun validate-output-translations-directory (directory)
    (validate-configuration-directory
     directory :output-translations 'validate-output-translations-directive
               :invalid-form-reporter 'invalid-output-translation))

  (defun parse-output-translations-string (string &key location)
    (cond
      ((or (null string) (equal string ""))
       '(:output-translations :inherit-configuration))
      ((not (stringp string))
       (error (compatfmt "~@<Environment string isn't: ~3i~_~S~@:>") string))
      ((eql (char string 0) #\")
       (parse-output-translations-string (read-from-string string) :location location))
      ((eql (char string 0) #\()
       (validate-output-translations-form (read-from-string string) :location location))
      (t
       (loop
         :with inherit = nil
         :with directives = ()
         :with start = 0
         :with end = (length string)
         :with source = nil
         :with separator = (inter-directory-separator)
         :for i = (or (position separator string :start start) end) :do
           (let ((s (subseq string start i)))
             (cond
               (source
                (push (list source (if (equal "" s) nil s)) directives)
                (setf source nil))
               ((equal "" s)
                (when inherit
                  (error (compatfmt "~@<Only one inherited configuration allowed: ~3i~_~S~@:>")
                         string))
                (setf inherit t)
                (push :inherit-configuration directives))
               (t
                (setf source s)))
             (setf start (1+ i))
             (when (> start end)
               (when source
                 (error (compatfmt "~@<Uneven number of components in source to destination mapping: ~3i~_~S~@:>")
                        string))
               (unless inherit
                 (push :ignore-inherited-configuration directives))
               (return `(:output-translations ,@(nreverse directives)))))))))

  (defparameter* *default-output-translations*
    '(environment-output-translations
      user-output-translations-pathname
      user-output-translations-directory-pathname
      system-output-translations-pathname
      system-output-translations-directory-pathname))

  (defun wrapping-output-translations ()
    `(:output-translations
    ;; Some implementations have precompiled ASDF systems,
    ;; so we must disable translations for implementation paths.
      #+(or #|clozure|# ecl mkcl sbcl)
      ,@(let ((h (resolve-symlinks* (lisp-implementation-directory))))
          (when h `(((,h ,*wild-path*) ()))))
      #+mkcl (,(translate-logical-pathname "CONTRIB:") ())
      ;; All-import, here is where we want user stuff to be:
      :inherit-configuration
      ;; These are for convenience, and can be overridden by the user:
      #+abcl (#p"/___jar___file___root___/**/*.*" (:user-cache #p"**/*.*"))
      #+abcl (#p"jar:file:/**/*.jar!/**/*.*" (:function translate-jar-pathname))
      ;; We enable the user cache by default, and here is the place we do:
      :enable-user-cache))

  (defparameter *output-translations-file* (parse-unix-namestring "asdf-output-translations.conf"))
  (defparameter *output-translations-directory* (parse-unix-namestring "asdf-output-translations.conf.d/"))

  (defun user-output-translations-pathname (&key (direction :input))
    (in-user-configuration-directory *output-translations-file* :direction direction))
  (defun system-output-translations-pathname (&key (direction :input))
    (in-system-configuration-directory *output-translations-file* :direction direction))
  (defun user-output-translations-directory-pathname (&key (direction :input))
    (in-user-configuration-directory *output-translations-directory* :direction direction))
  (defun system-output-translations-directory-pathname (&key (direction :input))
    (in-system-configuration-directory *output-translations-directory* :direction direction))
  (defun environment-output-translations ()
    (getenv "ASDF_OUTPUT_TRANSLATIONS"))

  (defgeneric process-output-translations (spec &key inherit collect))

  (defun inherit-output-translations (inherit &key collect)
    (when inherit
      (process-output-translations (first inherit) :collect collect :inherit (rest inherit))))

  (defun* (process-output-translations-directive) (directive &key inherit collect)
    (if (atom directive)
        (ecase directive
          ((:enable-user-cache)
           (process-output-translations-directive '(t :user-cache) :collect collect))
          ((:disable-cache)
           (process-output-translations-directive '(t t) :collect collect))
          ((:inherit-configuration)
           (inherit-output-translations inherit :collect collect))
          ((:ignore-inherited-configuration :ignore-invalid-entries nil)
           nil))
        (let ((src (first directive))
              (dst (second directive)))
          (if (eq src :include)
              (when dst
                (process-output-translations (pathname dst) :inherit nil :collect collect))
              (when src
                (let ((trusrc (or (eql src t)
                                  (let ((loc (resolve-location src :ensure-directory t :wilden t)))
                                    (if (absolute-pathname-p loc) (resolve-symlinks* loc) loc)))))
                  (cond
                    ((location-function-p dst)
                     (funcall collect
                              (list trusrc (ensure-function (second dst)))))
                    ((eq dst t)
                     (funcall collect (list trusrc t)))
                    (t
                     (let* ((trudst (if dst
                                        (resolve-location dst :ensure-directory t :wilden t)
                                        trusrc)))
                       (funcall collect (list trudst t))
                       (funcall collect (list trusrc trudst)))))))))))

  (defmethod process-output-translations ((x symbol) &key
                                                       (inherit *default-output-translations*)
                                                       collect)
    (process-output-translations (funcall x) :inherit inherit :collect collect))
  (defmethod process-output-translations ((pathname pathname) &key inherit collect)
    (cond
      ((directory-pathname-p pathname)
       (process-output-translations (validate-output-translations-directory pathname)
                                    :inherit inherit :collect collect))
      ((probe-file* pathname :truename *resolve-symlinks*)
       (process-output-translations (validate-output-translations-file pathname)
                                    :inherit inherit :collect collect))
      (t
       (inherit-output-translations inherit :collect collect))))
  (defmethod process-output-translations ((string string) &key inherit collect)
    (process-output-translations (parse-output-translations-string string)
                                 :inherit inherit :collect collect))
  (defmethod process-output-translations ((x null) &key inherit collect)
    (inherit-output-translations inherit :collect collect))
  (defmethod process-output-translations ((form cons) &key inherit collect)
    (dolist (directive (cdr (validate-output-translations-form form)))
      (process-output-translations-directive directive :inherit inherit :collect collect)))

  (defun compute-output-translations (&optional parameter)
    "read the configuration, return it"
    (remove-duplicates
     (while-collecting (c)
       (inherit-output-translations
        `(wrapping-output-translations ,parameter ,@*default-output-translations*) :collect #'c))
     :test 'equal :from-end t))

  (defvar *output-translations-parameter* nil)

  (defun initialize-output-translations (&optional (parameter *output-translations-parameter*))
    "read the configuration, initialize the internal configuration variable,
return the configuration"
    (setf *output-translations-parameter* parameter
          (output-translations) (compute-output-translations parameter)))

  (defun disable-output-translations ()
    "Initialize output translations in a way that maps every file to itself,
effectively disabling the output translation facility."
    (initialize-output-translations
     '(:output-translations :disable-cache :ignore-inherited-configuration)))

  ;; checks an initial variable to see whether the state is initialized
  ;; or cleared. In the former case, return current configuration; in
  ;; the latter, initialize.  ASDF will call this function at the start
  ;; of (asdf:find-system).
  (defun ensure-output-translations ()
    (if (output-translations-initialized-p)
        (output-translations)
        (initialize-output-translations)))

  (defun* (apply-output-translations) (path)
    (etypecase path
      (logical-pathname
       path)
      ((or pathname string)
       (ensure-output-translations)
       (loop* :with p = (resolve-symlinks* path)
              :for (source destination) :in (car *output-translations*)
              :for root = (when (or (eq source t)
                                    (and (pathnamep source)
                                         (not (absolute-pathname-p source))))
                            (pathname-root p))
              :for absolute-source = (cond
                                       ((eq source t) (wilden root))
                                       (root (merge-pathnames* source root))
                                       (t source))
              :when (or (eq source t) (pathname-match-p p absolute-source))
              :return (translate-pathname* p absolute-source destination root source)
              :finally (return p)))))

  ;; Hook into uiop's output-translation mechanism
  #-cormanlisp
  (setf *output-translation-function* 'apply-output-translations)

  #+abcl
  (defun translate-jar-pathname (source wildcard)
    (declare (ignore wildcard))
    (flet ((normalize-device (pathname)
             (if (find :windows *features*)
                 pathname
                 (make-pathname :defaults pathname :device :unspecific))))
      (let* ((jar
               (pathname (first (pathname-device source))))
             (target-root-directory-namestring
               (format nil "/___jar___file___root___/~@[~A/~]"
                       (and (find :windows *features*)
                            (pathname-device jar))))
             (relative-source
               (relativize-pathname-directory source))
             (relative-jar
               (relativize-pathname-directory (ensure-directory-pathname jar)))
             (target-root-directory
               (normalize-device
                (pathname-directory-pathname
                 (parse-namestring target-root-directory-namestring))))
             (target-root
               (merge-pathnames* relative-jar target-root-directory))
             (target
               (merge-pathnames* relative-source target-root)))
        (normalize-device (apply-output-translations target))))))

;;;; -----------------------------------------------------------------
;;;; Source Registry Configuration, by Francois-Rene Rideau
;;;; See the Manual and https://bugs.launchpad.net/asdf/+bug/485918

(uiop/package:define-package :asdf/source-registry
  (:recycle :asdf/source-registry :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/find-system)
  (:export
   #:*source-registry-parameter* #:*default-source-registries*
   #:invalid-source-registry
   #:source-registry-initialized-p
   #:initialize-source-registry #:clear-source-registry #:*source-registry*
   #:ensure-source-registry #:*source-registry-parameter*
   #:*default-source-registry-exclusions* #:*source-registry-exclusions*
   #:*wild-asd* #:directory-asd-files #:register-asd-directory
   #:collect-asds-in-directory #:collect-sub*directories-asd-files
   #:validate-source-registry-directive #:validate-source-registry-form
   #:validate-source-registry-file #:validate-source-registry-directory
   #:parse-source-registry-string #:wrapping-source-registry
   #:default-user-source-registry #:default-system-source-registry
   #:user-source-registry #:system-source-registry
   #:user-source-registry-directory #:system-source-registry-directory
   #:environment-source-registry #:process-source-registry
   #:compute-source-registry #:flatten-source-registry
   #:sysdef-source-registry-search))
(in-package :asdf/source-registry)

(with-upgradability ()
  (define-condition invalid-source-registry (invalid-configuration warning)
    ((format :initform (compatfmt "~@<Invalid source registry ~S~@[ in ~S~]~@{ ~@?~}~@:>"))))

  ;; Using ack 1.2 exclusions
  (defvar *default-source-registry-exclusions*
    '(".bzr" ".cdv"
      ;; "~.dep" "~.dot" "~.nib" "~.plst" ; we don't support ack wildcards
      ".git" ".hg" ".pc" ".svn" "CVS" "RCS" "SCCS" "_darcs"
      "_sgbak" "autom4te.cache" "cover_db" "_build"
      "debian")) ;; debian often builds stuff under the debian directory... BAD.

  (defvar *source-registry-exclusions* *default-source-registry-exclusions*)

  (defvar *source-registry* nil
    "Either NIL (for uninitialized), or an equal hash-table, mapping
system names to pathnames of .asd files")

  (defun source-registry-initialized-p ()
    (typep *source-registry* 'hash-table))

  (defun clear-source-registry ()
    "Undoes any initialization of the source registry."
    (setf *source-registry* nil)
    (values))
  (register-clear-configuration-hook 'clear-source-registry)

  (defparameter *wild-asd*
    (make-pathname* :directory nil :name *wild* :type "asd" :version :newest))

  (defun directory-asd-files (directory)
    (directory-files directory *wild-asd*))

  (defun collect-asds-in-directory (directory collect)
    (map () collect (directory-asd-files directory)))

  (defun collect-sub*directories-asd-files
      (directory &key (exclude *default-source-registry-exclusions*) collect)
    (collect-sub*directories
     directory
     (constantly t)
     #'(lambda (x &aux (l (car (last (pathname-directory x))))) (not (member l exclude :test #'equal)))
     #'(lambda (dir) (collect-asds-in-directory dir collect))))

  (defun validate-source-registry-directive (directive)
    (or (member directive '(:default-registry))
        (and (consp directive)
             (let ((rest (rest directive)))
               (case (first directive)
                 ((:include :directory :tree)
                  (and (length=n-p rest 1)
                       (location-designator-p (first rest))))
                 ((:exclude :also-exclude)
                  (every #'stringp rest))
                 ((:default-registry)
                  (null rest)))))))

  (defun validate-source-registry-form (form &key location)
    (validate-configuration-form
     form :source-registry 'validate-source-registry-directive
          :location location :invalid-form-reporter 'invalid-source-registry))

  (defun validate-source-registry-file (file)
    (validate-configuration-file
     file 'validate-source-registry-form :description "a source registry"))

  (defun validate-source-registry-directory (directory)
    (validate-configuration-directory
     directory :source-registry 'validate-source-registry-directive
               :invalid-form-reporter 'invalid-source-registry))

  (defun parse-source-registry-string (string &key location)
    (cond
      ((or (null string) (equal string ""))
       '(:source-registry :inherit-configuration))
      ((not (stringp string))
       (error (compatfmt "~@<Environment string isn't: ~3i~_~S~@:>") string))
      ((find (char string 0) "\"(")
       (validate-source-registry-form (read-from-string string) :location location))
      (t
       (loop
         :with inherit = nil
         :with directives = ()
         :with start = 0
         :with end = (length string)
         :with separator = (inter-directory-separator)
         :for pos = (position separator string :start start) :do
           (let ((s (subseq string start (or pos end))))
             (flet ((check (dir)
                      (unless (absolute-pathname-p dir)
                        (error (compatfmt "~@<source-registry string must specify absolute pathnames: ~3i~_~S~@:>") string))
                      dir))
               (cond
                 ((equal "" s) ; empty element: inherit
                  (when inherit
                    (error (compatfmt "~@<Only one inherited configuration allowed: ~3i~_~S~@:>")
                           string))
                  (setf inherit t)
                  (push ':inherit-configuration directives))
                 ((string-suffix-p s "//") ;; TODO: allow for doubling of separator even outside Unix?
                  (push `(:tree ,(check (subseq s 0 (- (length s) 2)))) directives))
                 (t
                  (push `(:directory ,(check s)) directives))))
             (cond
               (pos
                (setf start (1+ pos)))
               (t
                (unless inherit
                  (push '(:ignore-inherited-configuration) directives))
                (return `(:source-registry ,@(nreverse directives))))))))))

  (defun register-asd-directory (directory &key recurse exclude collect)
    (if (not recurse)
        (collect-asds-in-directory directory collect)
        (collect-sub*directories-asd-files
         directory :exclude exclude :collect collect)))

  (defparameter* *default-source-registries*
    '(environment-source-registry
      user-source-registry
      user-source-registry-directory
      default-user-source-registry
      system-source-registry
      system-source-registry-directory
      default-system-source-registry)
    "List of default source registries" "3.1.0.102")

  (defparameter *source-registry-file* (parse-unix-namestring "source-registry.conf"))
  (defparameter *source-registry-directory* (parse-unix-namestring "source-registry.conf.d/"))

  (defun wrapping-source-registry ()
    `(:source-registry
      #+(or ecl sbcl) (:tree ,(resolve-symlinks* (lisp-implementation-directory)))
      :inherit-configuration
      #+mkcl (:tree ,(translate-logical-pathname "CONTRIB:"))
      #+cmu (:tree #p"modules:")
      #+scl (:tree #p"file://modules/")))
  (defun default-user-source-registry ()
    `(:source-registry
      (:tree (:home "common-lisp/"))
      #+sbcl (:directory (:home ".sbcl/systems/"))
      ,@(loop :for dir :in
              `(,@(when (os-unix-p)
                    `(,(or (getenv-absolute-directory "XDG_DATA_HOME")
                           (subpathname (user-homedir-pathname) ".local/share/"))))
                ,@(when (os-windows-p)
                    (mapcar 'get-folder-path '(:local-appdata :appdata))))
              :collect `(:directory ,(subpathname* dir "common-lisp/systems/"))
              :collect `(:tree ,(subpathname* dir "common-lisp/source/")))
      :inherit-configuration))
  (defun default-system-source-registry ()
    `(:source-registry
      ,@(loop :for dir :in
              `(,@(when (os-unix-p)
                    (or (getenv-absolute-directories "XDG_DATA_DIRS")
                        '("/usr/local/share" "/usr/share")))
                ,@(when (os-windows-p)
                    (list (get-folder-path :common-appdata))))
              :collect `(:directory ,(subpathname* dir "common-lisp/systems/"))
              :collect `(:tree ,(subpathname* dir "common-lisp/source/")))
      :inherit-configuration))
  (defun user-source-registry (&key (direction :input))
    (in-user-configuration-directory *source-registry-file* :direction direction))
  (defun system-source-registry (&key (direction :input))
    (in-system-configuration-directory *source-registry-file* :direction direction))
  (defun user-source-registry-directory (&key (direction :input))
    (in-user-configuration-directory *source-registry-directory* :direction direction))
  (defun system-source-registry-directory (&key (direction :input))
    (in-system-configuration-directory *source-registry-directory* :direction direction))
  (defun environment-source-registry ()
    (getenv "CL_SOURCE_REGISTRY"))

  (defgeneric* (process-source-registry) (spec &key inherit register))

  (defun* (inherit-source-registry) (inherit &key register)
    (when inherit
      (process-source-registry (first inherit) :register register :inherit (rest inherit))))

  (defun* (process-source-registry-directive) (directive &key inherit register)
    (destructuring-bind (kw &rest rest) (if (consp directive) directive (list directive))
      (ecase kw
        ((:include)
         (destructuring-bind (pathname) rest
           (process-source-registry (resolve-location pathname) :inherit nil :register register)))
        ((:directory)
         (destructuring-bind (pathname) rest
           (when pathname
             (funcall register (resolve-location pathname :ensure-directory t)))))
        ((:tree)
         (destructuring-bind (pathname) rest
           (when pathname
             (funcall register (resolve-location pathname :ensure-directory t)
                      :recurse t :exclude *source-registry-exclusions*))))
        ((:exclude)
         (setf *source-registry-exclusions* rest))
        ((:also-exclude)
         (appendf *source-registry-exclusions* rest))
        ((:default-registry)
         (inherit-source-registry '(default-source-registry) :register register))
        ((:inherit-configuration)
         (inherit-source-registry inherit :register register))
        ((:ignore-inherited-configuration)
         nil)))
    nil)

  (defmethod process-source-registry ((x symbol) &key inherit register)
    (process-source-registry (funcall x) :inherit inherit :register register))
  (defmethod process-source-registry ((pathname pathname) &key inherit register)
    (cond
      ((directory-pathname-p pathname)
       (let ((*here-directory* (resolve-symlinks* pathname)))
         (process-source-registry (validate-source-registry-directory pathname)
                                  :inherit inherit :register register)))
      ((probe-file* pathname :truename *resolve-symlinks*)
       (let ((*here-directory* (pathname-directory-pathname pathname)))
         (process-source-registry (validate-source-registry-file pathname)
                                  :inherit inherit :register register)))
      (t
       (inherit-source-registry inherit :register register))))
  (defmethod process-source-registry ((string string) &key inherit register)
    (process-source-registry (parse-source-registry-string string)
                             :inherit inherit :register register))
  (defmethod process-source-registry ((x null) &key inherit register)
    (inherit-source-registry inherit :register register))
  (defmethod process-source-registry ((form cons) &key inherit register)
    (let ((*source-registry-exclusions* *default-source-registry-exclusions*))
      (dolist (directive (cdr (validate-source-registry-form form)))
        (process-source-registry-directive directive :inherit inherit :register register))))

  (defun flatten-source-registry (&optional parameter)
    (remove-duplicates
     (while-collecting (collect)
       (with-pathname-defaults () ;; be location-independent
         (inherit-source-registry
          `(wrapping-source-registry
            ,parameter
            ,@*default-source-registries*)
          :register #'(lambda (directory &key recurse exclude)
                        (collect (list directory :recurse recurse :exclude exclude))))))
     :test 'equal :from-end t))

  ;; Will read the configuration and initialize all internal variables.
  (defun compute-source-registry (&optional parameter (registry *source-registry*))
    (dolist (entry (flatten-source-registry parameter))
      (destructuring-bind (directory &key recurse exclude) entry
        (let* ((h (make-hash-table :test 'equal))) ; table to detect duplicates
          (register-asd-directory
           directory :recurse recurse :exclude exclude :collect
           #'(lambda (asd)
               (let* ((name (pathname-name asd))
                      (name (if (typep asd 'logical-pathname)
                                ;; logical pathnames are upper-case,
                                ;; at least in the CLHS and on SBCL,
                                ;; yet (coerce-name :foo) is lower-case.
                                ;; won't work well with (load-system "Foo")
                                ;; instead of (load-system 'foo)
                                (string-downcase name)
                                name)))
                 (cond
                   ((gethash name registry) ; already shadowed by something else
                    nil)
                   ((gethash name h) ; conflict at current level
                    (when *verbose-out*
                      (warn (compatfmt "~@<In source-registry entry ~A~@[/~*~] ~
                                found several entries for ~A - picking ~S over ~S~:>")
                            directory recurse name (gethash name h) asd)))
                   (t
                    (setf (gethash name registry) asd)
                    (setf (gethash name h) asd))))))
          h)))
    (values))

  (defvar *source-registry-parameter* nil)

  (defun initialize-source-registry (&optional (parameter *source-registry-parameter*))
    ;; Record the parameter used to configure the registry
    (setf *source-registry-parameter* parameter)
    ;; Clear the previous registry database:
    (setf *source-registry* (make-hash-table :test 'equal))
    ;; Do it!
    (compute-source-registry parameter))

  ;; Checks an initial variable to see whether the state is initialized
  ;; or cleared. In the former case, return current configuration; in
  ;; the latter, initialize.  ASDF will call this function at the start
  ;; of (asdf:find-system) to make sure the source registry is initialized.
  ;; However, it will do so *without* a parameter, at which point it
  ;; will be too late to provide a parameter to this function, though
  ;; you may override the configuration explicitly by calling
  ;; initialize-source-registry directly with your parameter.
  (defun ensure-source-registry (&optional parameter)
    (unless (source-registry-initialized-p)
      (initialize-source-registry parameter))
    (values))

  (defun sysdef-source-registry-search (system)
    (ensure-source-registry)
    (values (gethash (primary-system-name system) *source-registry*))))


;;;; -------------------------------------------------------------------------
;;; Internal hacks for backward-compatibility

(uiop/package:define-package :asdf/backward-internals
  (:recycle :asdf/backward-internals :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
   :asdf/system :asdf/component :asdf/operation
   :asdf/find-system :asdf/action :asdf/lisp-action)
  (:export ;; for internal use
   #:load-sysdef #:make-temporary-package
   #:%refresh-component-inline-methods
   #:make-sub-operation
   #:load-sysdef #:make-temporary-package))
(in-package :asdf/backward-internals)

;;;; Backward compatibility with "inline methods"
(with-upgradability ()
  (defparameter* +asdf-methods+
    '(perform-with-restarts perform explain output-files operation-done-p))

  (defun %remove-component-inline-methods (component)
    (dolist (name +asdf-methods+)
      (map ()
           ;; this is inefficient as most of the stored
           ;; methods will not be for this particular gf
           ;; But this is hardly performance-critical
           #'(lambda (m)
               (remove-method (symbol-function name) m))
           (component-inline-methods component)))
    (component-inline-methods component) nil)

  (defun %define-component-inline-methods (ret rest)
    (loop* :for (key value) :on rest :by #'cddr
           :for name = (and (keywordp key) (find key +asdf-methods+ :test 'string=))
           :when name :do
           (destructuring-bind (op &rest body) value
             (loop :for arg = (pop body)
                   :while (atom arg)
                   :collect arg :into qualifiers
                   :finally
                      (destructuring-bind (o c) arg
                        (pushnew
                         (eval `(defmethod ,name ,@qualifiers ((,o ,op) (,c (eql ,ret))) ,@body))
                         (component-inline-methods ret)))))))

  (defun %refresh-component-inline-methods (component rest)
    ;; clear methods, then add the new ones
    (%remove-component-inline-methods component)
    (%define-component-inline-methods component rest)))

(when-upgrading (:when (fboundp 'make-sub-operation))
  (defun make-sub-operation (c o dep-c dep-o)
    (declare (ignore c o dep-c dep-o)) (asdf-upgrade-error)))


;;;; load-sysdef
(with-upgradability ()
  (defun load-sysdef (name pathname)
    (load-asd pathname :name name))

  (defun make-temporary-package ()
    ;; For loading a .asd file, we don't make a temporary package anymore,
    ;; but use ASDF-USER. I'd like to have this function do this,
    ;; but since whoever uses it is likely to delete-package the result afterwards,
    ;; this would be a bad idea, so preserve the old behavior.
    (make-package (fresh-package-name :prefix :asdf :index 0) :use '(:cl :asdf))))


;;;; -------------------------------------------------------------------------
;;;; Defsystem

(uiop/package:define-package :asdf/parse-defsystem
  (:recycle :asdf/parse-defsystem :asdf/defsystem :asdf)
  (:nicknames :asdf/defsystem) ;; previous name, to be compatible with, in case anyone cares
  (:use :uiop/common-lisp :asdf/driver :asdf/upgrade
   :asdf/cache :asdf/component :asdf/system
   :asdf/find-system :asdf/find-component :asdf/lisp-action :asdf/operate
   :asdf/backward-internals)
  (:import-from :asdf/system #:depends-on #:weakly-depends-on)
  (:export
   #:defsystem #:register-system-definition
   #:class-for-type #:*default-component-class*
   #:determine-system-directory #:parse-component-form
   #:non-toplevel-system #:non-system-system
   #:sysdef-error-component #:check-component-input))
(in-package :asdf/parse-defsystem)

;;; Pathname
(with-upgradability ()
  (defun determine-system-directory (pathname)
    ;; The defsystem macro calls this function to determine
    ;; the pathname of a system as follows:
    ;; 1. if the pathname argument is an pathname object (NOT a namestring),
    ;;    that is already an absolute pathname, return it.
    ;; 2. otherwise, the directory containing the LOAD-PATHNAME
    ;;    is considered (as deduced from e.g. *LOAD-PATHNAME*), and
    ;;    if it is indeed available and an absolute pathname, then
    ;;    the PATHNAME argument is normalized to a relative pathname
    ;;    as per PARSE-UNIX-NAMESTRING (with ENSURE-DIRECTORY T)
    ;;    and merged into that DIRECTORY as per SUBPATHNAME.
    ;;    Note: avoid *COMPILE-FILE-PATHNAME* because .asd is loaded,
    ;;    and may be from within the EVAL-WHEN of a file compilation.
    ;; If no absolute pathname was found, we return NIL.
    (check-type pathname (or null string pathname))
    (pathname-directory-pathname
     (resolve-symlinks*
      (ensure-absolute-pathname
       (parse-unix-namestring pathname :type :directory)
       #'(lambda () (ensure-absolute-pathname
                     (load-pathname) 'get-pathname-defaults nil))
       nil)))))


;;; Component class
(with-upgradability ()
  (defvar *default-component-class* 'cl-source-file)

  (defun class-for-type (parent type)
      (or (coerce-class type :package :asdf/interface :super 'component :error nil)
          (and (eq type :file)
               (coerce-class
                (or (loop :for p = parent :then (component-parent p) :while p
                            :thereis (module-default-component-class p))
                    *default-component-class*)
                :package :asdf/interface :super 'component :error nil))
          (sysdef-error "don't recognize component type ~S" type))))


;;; Check inputs
(with-upgradability ()
  (define-condition non-system-system (system-definition-error)
    ((name :initarg :name :reader non-system-system-name)
     (class-name :initarg :class-name :reader non-system-system-class-name))
    (:report (lambda (c s)
               (format s (compatfmt "~@<Error while defining system ~S: class ~S isn't a subclass of ~S~@:>")
                       (non-system-system-name c) (non-system-system-class-name c) 'system))))

  (define-condition non-toplevel-system (system-definition-error)
    ((parent :initarg :parent :reader non-toplevel-system-parent)
     (name :initarg :name :reader non-toplevel-system-name))
    (:report (lambda (c s)
               (format s (compatfmt "~@<Error while defining system: component ~S claims to have a system ~S as a child~@:>")
                       (non-toplevel-system-parent c) (non-toplevel-system-name c)))))

  (defun sysdef-error-component (msg type name value)
    (sysdef-error (strcat msg (compatfmt "~&~@<The value specified for ~(~A~) ~A is ~S~@:>"))
                  type name value))

  (defun check-component-input (type name weakly-depends-on
                                depends-on components)
    "A partial test of the values of a component."
    (unless (listp depends-on)
      (sysdef-error-component ":depends-on must be a list."
                              type name depends-on))
    (unless (listp weakly-depends-on)
      (sysdef-error-component ":weakly-depends-on must be a list."
                              type name weakly-depends-on))
    (unless (listp components)
      (sysdef-error-component ":components must be NIL or a list of components."
                              type name components)))

  (defun* (normalize-version) (form &key pathname component parent)
    (labels ((invalid (&optional (continuation "using NIL instead"))
               (warn (compatfmt "~@<Invalid :version specifier ~S~@[ for component ~S~]~@[ in ~S~]~@[ from file ~S~]~@[, ~A~]~@:>")
                     form component parent pathname continuation))
             (invalid-parse (control &rest args)
               (unless (if-let (target (find-component parent component)) (builtin-system-p target))
                 (apply 'warn control args)
                 (invalid))))
      (if-let (v (typecase form
                   ((or string null) form)
                   (real
                    (invalid "Substituting a string")
                    (format nil "~D" form)) ;; 1.0 becomes "1.0"
                   (cons
                    (case (first form)
                      ((:read-file-form)
                       (destructuring-bind (subpath &key (at 0)) (rest form)
                         (safe-read-file-form (subpathname pathname subpath)
                                              :at at :package :asdf-user)))
                      ((:read-file-line)
                       (destructuring-bind (subpath &key (at 0)) (rest form)
                         (safe-read-file-line (subpathname pathname subpath)
                                              :at at)))
                      (otherwise
                       (invalid))))
                   (t
                    (invalid))))
        (if-let (pv (parse-version v #'invalid-parse))
          (unparse-version pv)
          (invalid))))))


;;; Main parsing function
(with-upgradability ()
  (defun* parse-dependency-def (dd)
    (if (listp dd)
        (case (first dd)
          (:feature
           (unless (= (length dd) 3)
             (sysdef-error "Ill-formed feature dependency: ~s" dd))
           (let ((embedded (parse-dependency-def (third dd))))
             `(:feature ,(second dd) ,embedded)))
          (feature
           (sysdef-error "`feature' has been removed from the dependency spec language of ASDF. Use :feature instead in ~s." dd))
          (:require
           (unless (= (length dd) 2)
             (sysdef-error "Ill-formed require dependency: ~s" dd))
           dd)
          (:version
           (unless (= (length dd) 3)
             (sysdef-error "Ill-formed version dependency: ~s" dd))
           `(:version ,(coerce-name (second dd)) ,(third dd)))
          (otherwise (sysdef-error "Ill-formed dependency: ~s" dd)))
      (coerce-name dd)))

  (defun* parse-dependency-defs (dd-list)
    "Parse the dependency defs in DD-LIST into canonical form by translating all
system names contained using COERCE-NAME. Return the result."
    (mapcar 'parse-dependency-def dd-list))

  (defun* (parse-component-form) (parent options &key previous-serial-component)
    (destructuring-bind
        (type name &rest rest &key
                                (builtin-system-p () bspp)
                                ;; the following list of keywords is reproduced below in the
                                ;; remove-plist-keys form.  important to keep them in sync
                                components pathname perform explain output-files operation-done-p
                                weakly-depends-on depends-on serial
                                do-first if-component-dep-fails version
                                ;; list ends
         &allow-other-keys) options
      (declare (ignore perform explain output-files operation-done-p builtin-system-p))
      (check-component-input type name weakly-depends-on depends-on components)
      (when (and parent
                 (find-component parent name)
                 (not ;; ignore the same object when rereading the defsystem
                  (typep (find-component parent name)
                         (class-for-type parent type))))
        (error 'duplicate-names :name name))
      (when do-first (error "DO-FIRST is not supported anymore as of ASDF 3"))
      (let* ((name (coerce-name name))
             (args `(:name ,name
                     :pathname ,pathname
                     ,@(when parent `(:parent ,parent))
                     ,@(remove-plist-keys
                        '(:components :pathname :if-component-dep-fails :version
                          :perform :explain :output-files :operation-done-p
                          :weakly-depends-on :depends-on :serial)
                        rest)))
             (component (find-component parent name))
             (class (class-for-type parent type)))
        (when (and parent (subtypep class 'system))
          (error 'non-toplevel-system :parent parent :name name))
        (if component ; preserve identity
            (apply 'reinitialize-instance component args)
            (setf component (apply 'make-instance class args)))
        (component-pathname component) ; eagerly compute the absolute pathname
        (when (typep component 'system)
          ;; cache information for introspection
          (setf (slot-value component 'depends-on)
                (parse-dependency-defs depends-on)
                (slot-value component 'weakly-depends-on)
                ;; these must be a list of systems, cannot be features or versioned systems
                (mapcar 'coerce-name weakly-depends-on)))
        (let ((sysfile (system-source-file (component-system component)))) ;; requires the previous
          (when (and (typep component 'system) (not bspp))
            (setf (builtin-system-p component) (lisp-implementation-pathname-p sysfile)))
          (setf version (normalize-version version :component name :parent parent :pathname sysfile)))
        ;; Don't use the accessor: kluge to avoid upgrade issue on CCL 1.8.
        ;; A better fix is required.
        (setf (slot-value component 'version) version)
        (when (typep component 'parent-component)
          (setf (component-children component)
                (loop
                  :with previous-component = nil
                  :for c-form :in components
                  :for c = (parse-component-form component c-form
                                                 :previous-serial-component previous-component)
                  :for name = (component-name c)
                  :collect c
                  :when serial :do (setf previous-component name)))
          (compute-children-by-name component))
        (when previous-serial-component
          (push previous-serial-component depends-on))
        (when weakly-depends-on
          ;; ASDF4: deprecate this feature and remove it.
          (appendf depends-on
                   (remove-if (complement #'(lambda (x) (find-system x nil))) weakly-depends-on)))
        ;; Used by POIU. ASDF4: rename to component-depends-on?
        (setf (component-sideway-dependencies component) depends-on)
        (%refresh-component-inline-methods component rest)
        (when if-component-dep-fails
          (error "The system definition for ~S uses deprecated ~
            ASDF option :IF-COMPONENT-DEP-FAILS. ~
            Starting with ASDF 3, please use :IF-FEATURE instead"
           (coerce-name (component-system component))))
        component)))

  (defun register-system-definition
      (name &rest options &key pathname (class 'system) (source-file () sfp)
                            defsystem-depends-on &allow-other-keys)
    ;; The system must be registered before we parse the body,
    ;; otherwise we recur when trying to find an existing system
    ;; of the same name to reuse options (e.g. pathname) from.
    ;; To avoid infinite recursion in cases where you defsystem a system
    ;; that is registered to a different location to find-system,
    ;; we also need to remember it in the asdf-cache.
    (with-asdf-cache ()
      (let* ((name (coerce-name name))
             (source-file (if sfp source-file (resolve-symlinks* (load-pathname))))
             (registered (system-registered-p name))
             (registered! (if registered
                              (rplaca registered (get-file-stamp source-file))
                              (register-system
                               (make-instance 'system :name name :source-file source-file))))
             (system (reset-system (cdr registered!)
                                   :name name :source-file source-file))
             (component-options
              (remove-plist-keys '(:defsystem-depends-on :class) options))
             (defsystem-dependencies (loop :for spec :in defsystem-depends-on :collect
                                           (resolve-dependency-spec nil spec))))
        ;; cache defsystem-depends-on in canonical form
        (when defsystem-depends-on
          (setf component-options
                (append `(:defsystem-depends-on ,(parse-dependency-defs defsystem-depends-on))
                        component-options)))
        (set-asdf-cache-entry `(find-system ,name) (list system))
        (load-systems* defsystem-dependencies)
        ;; We change-class AFTER we loaded the defsystem-depends-on
        ;; since the class might be defined as part of those.
        (let ((class (class-for-type nil class)))
          (unless (subtypep class 'system)
            (error 'non-system-system :name name :class-name (class-name class)))
          (unless (eq (type-of system) class)
            (change-class system class)))
        (parse-component-form
         nil (list*
              :module name
              :pathname (determine-system-directory pathname)
              component-options)))))

  (defmacro defsystem (name &body options)
    `(apply 'register-system-definition ',name ',options)))
;;;; -------------------------------------------------------------------------
;;;; ASDF-Bundle

(uiop/package:define-package :asdf/bundle
  (:recycle :asdf/bundle :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
   :asdf/component :asdf/system :asdf/find-system :asdf/find-component :asdf/operation
   :asdf/action :asdf/lisp-action :asdf/plan :asdf/operate :asdf/defsystem)
  (:export
   #:bundle-op #:bundle-type #:program-system
   #:bundle-system #:bundle-pathname-type #:bundlable-file-p #:direct-dependency-files
   #:monolithic-op #:monolithic-bundle-op #:operation-monolithic-p
   #:fasl-op #:load-fasl-op #:monolithic-fasl-op #:binary-op #:monolithic-binary-op
   #:basic-compile-bundle-op #:prepare-bundle-op
   #:compile-bundle-op #:load-bundle-op #:monolithic-compile-bundle-op #:monolithic-load-bundle-op
   #:lib-op #:monolithic-lib-op
   #:dll-op #:monolithic-dll-op
   #:deliver-asd-op #:monolithic-deliver-asd-op
   #:program-op #:image-op #:compiled-file #:precompiled-system #:prebuilt-system
   #:user-system-p #:user-system #:trivial-system-p
   #:make-build
   #:build-args #:name-suffix #:prologue-code #:epilogue-code #:static-library))
(in-package :asdf/bundle)

(with-upgradability ()
  (defclass bundle-op (basic-compile-op)
    ((build-args :initarg :args :initform nil :accessor extra-build-args)
     (name-suffix :initarg :name-suffix :initform nil)
     (bundle-type :initform :no-output-file :reader bundle-type)
     #+ecl (lisp-files :initform nil :accessor extra-object-files)))

  (defclass monolithic-op (operation) ()
    (:documentation "A MONOLITHIC operation operates on a system *and all of its
dependencies*.  So, for example, a monolithic concatenate operation will
concatenate together a system's components and all of its dependencies, but a
simple concatenate operation will concatenate only the components of the system
itself.")) ;; operation on a system and its dependencies

  (defclass monolithic-bundle-op (monolithic-op bundle-op)
    ;; Old style way of specifying prologue and epilogue on ECL: in the monolithic operation
    ((prologue-code :initform nil :accessor prologue-code)
     (epilogue-code :initform nil :accessor epilogue-code)))

  (defclass program-system (system)
    ;; New style (ASDF3.1) way of specifying prologue and epilogue on ECL: in the system
    ((prologue-code :initform nil :initarg :prologue-code :reader prologue-code)
     (epilogue-code :initform nil :initarg :epilogue-code :reader epilogue-code)
     (no-uiop :initform nil :initarg :no-uiop :reader no-uiop)
     (prefix-lisp-object-files :initarg :prefix-lisp-object-files
                               :initform nil :accessor prefix-lisp-object-files)
     (postfix-lisp-object-files :initarg :postfix-lisp-object-files
                                :initform nil :accessor postfix-lisp-object-files)
     (extra-object-files :initarg :extra-object-files
                         :initform nil :accessor extra-object-files)
     (extra-build-args :initarg :extra-build-args
                       :initform nil :accessor extra-build-args)))

  (defmethod prologue-code ((x t)) nil)
  (defmethod epilogue-code ((x t)) nil)
  (defmethod no-uiop ((x t)) nil)
  (defmethod prefix-lisp-object-files ((x t)) nil)
  (defmethod postfix-lisp-object-files ((x t)) nil)
  (defmethod extra-object-files ((x t)) nil)
  (defmethod extra-build-args ((x t)) nil)

  (defclass link-op (bundle-op) ()
    (:documentation "Abstract operation for linking files together"))

  (defclass gather-op (bundle-op)
    ((gather-op :initform nil :allocation :class :reader gather-op))
    (:documentation "Abstract operation for gathering many input files from a system"))

  (defun operation-monolithic-p (op)
    (typep op 'monolithic-op))

  (defmethod component-depends-on ((o gather-op) (s system))
    (let* ((mono (operation-monolithic-p o))
           (deps
             (required-components
              s :other-systems mono :component-type (if mono 'system '(not system))
                :goal-operation (find-operation o 'load-op)
                :keep-operation 'compile-op)))
      ;; NB: the explicit make-operation on ECL and MKCL
      ;; ensures that we drop the original-initargs and its magic flags when recursing.
      `((,(make-operation (or (gather-op o) (if mono 'lib-op 'compile-op))) ,@deps)
        ,@(call-next-method))))

  ;; create a single fasl for the entire library
  (defclass basic-compile-bundle-op (bundle-op)
    ((bundle-type :initform :fasl)))

  (defclass prepare-bundle-op (sideway-operation)
    ((sideway-operation :initform #+(or ecl mkcl) 'load-bundle-op #-(or ecl mkcl) 'load-op
                        :allocation :class)))

  (defclass lib-op (link-op gather-op non-propagating-operation)
    ((bundle-type :initform :lib))
    (:documentation "compile the system and produce linkable (.a) library for it."))

  (defclass compile-bundle-op (basic-compile-bundle-op selfward-operation
                               #+(or ecl mkcl) link-op #-ecl gather-op)
    ((selfward-operation :initform '(prepare-bundle-op #+ecl lib-op) :allocation :class)))

  (defclass load-bundle-op (basic-load-op selfward-operation)
    ((selfward-operation :initform '(prepare-bundle-op compile-bundle-op) :allocation :class)))

  ;; NB: since the monolithic-op's can't be sideway-operation's,
  ;; if we wanted lib-op, dll-op, deliver-asd-op to be sideway-operation's,
  ;; we'd have to have the monolithic-op not inherit from the main op,
  ;; but instead inherit from a basic-FOO-op as with basic-compile-bundle-op above.

  (defclass dll-op (link-op gather-op non-propagating-operation)
    ((bundle-type :initform :dll))
    (:documentation "compile the system and produce dynamic (.so/.dll) library for it."))

  (defclass deliver-asd-op (basic-compile-op selfward-operation)
    ((selfward-operation :initform '(compile-bundle-op #+(or ecl mkcl) lib-op) :allocation :class))
    (:documentation "produce an asd file for delivering the system as a single fasl"))


  (defclass monolithic-deliver-asd-op (monolithic-bundle-op deliver-asd-op)
    ((selfward-operation :initform '(monolithic-compile-bundle-op #+(or ecl mkcl) monolithic-lib-op)
                         :allocation :class))
    (:documentation "produce fasl and asd files for combined system and dependencies."))

  (defclass monolithic-compile-bundle-op (monolithic-bundle-op basic-compile-bundle-op
                                          #+(or ecl mkcl) link-op gather-op non-propagating-operation)
    ((gather-op :initform #+(or ecl mkcl) 'lib-op #-(or ecl mkcl) 'compile-bundle-op :allocation :class))
    (:documentation "Create a single fasl for the system and its dependencies."))

  (defclass monolithic-load-bundle-op (monolithic-bundle-op load-bundle-op)
    ((selfward-operation :initform 'monolithic-compile-bundle-op :allocation :class))
    (:documentation "Load a single fasl for the system and its dependencies."))

  (defclass monolithic-lib-op (monolithic-bundle-op lib-op non-propagating-operation) ()
    (:documentation "Create a single linkable library for the system and its dependencies."))

  (defclass monolithic-dll-op (monolithic-bundle-op dll-op non-propagating-operation)
    ((bundle-type :initform :dll))
    (:documentation "Create a single dynamic (.so/.dll) library for the system and its dependencies."))

  (defclass image-op (monolithic-bundle-op selfward-operation
                      #+(or ecl mkcl) link-op #+(or ecl mkcl) gather-op)
    ((bundle-type :initform :image)
     (selfward-operation :initform '(#-(or ecl mkcl) load-op) :allocation :class))
    (:documentation "create an image file from the system and its dependencies"))

  (defclass program-op (image-op)
    ((bundle-type :initform :program))
    (:documentation "create an executable file from the system and its dependencies"))

  (defun bundle-pathname-type (bundle-type)
    (etypecase bundle-type
      ((eql :no-output-file) nil) ;; should we error out instead?
      ((or null string) bundle-type)
      ((eql :fasl) #-(or ecl mkcl) (compile-file-type) #+(or ecl mkcl) "fasb")
      #+ecl
      ((member :dll :lib :shared-library :static-library :program :object :program)
       (compile-file-type :type bundle-type))
      ((member :image) #-allegro "image" #+allegro "dxl")
      ((member :dll :shared-library) (cond ((os-macosx-p) "dylib") ((os-unix-p) "so") ((os-windows-p) "dll")))
      ((member :lib :static-library) (cond ((os-unix-p) "a")
                                           ((os-windows-p) (if (featurep '(:or :mingw32 :mingw64)) "a" "lib"))))
      ((eql :program) (cond ((os-unix-p) nil) ((os-windows-p) "exe")))))

  (defun bundle-output-files (o c)
    (let ((bundle-type (bundle-type o)))
      (unless (or (eq bundle-type :no-output-file) ;; NIL already means something regarding type.
                  (and (null (input-files o c)) (not (member bundle-type '(:image :program)))))
        (let ((name (or (component-build-pathname c)
                        (format nil "~A~@[~A~]" (component-name c) (slot-value o 'name-suffix))))
              (type (bundle-pathname-type bundle-type)))
          (values (list (subpathname (component-pathname c) name :type type))
                  (eq (type-of o) (component-build-operation c)))))))

  (defmethod output-files ((o bundle-op) (c system))
    (bundle-output-files o c))

  #-(or ecl mkcl)
  (progn
    (defmethod perform ((o image-op) (c system))
      (dump-image (output-file o c) :executable (typep o 'program-op)))
    (defmethod perform :before ((o program-op) (c system))
      (setf *image-entry-point* (ensure-function (component-entry-point c)))))

  (defclass compiled-file (file-component)
    ((type :initform #-(or ecl mkcl) (compile-file-type) #+(or ecl mkcl) "fasb")))

  (defclass precompiled-system (system)
    ((build-pathname :initarg :fasl)))

  (defclass prebuilt-system (system)
    ((build-pathname :initarg :static-library :initarg :lib
                     :accessor prebuilt-system-static-library))))


;;;
;;; BUNDLE-OP
;;;
;;; This operation takes all components from one or more systems and
;;; creates a single output file, which may be
;;; a FASL, a statically linked library, a shared library, etc.
;;; The different targets are defined by specialization.
;;;
(with-upgradability ()
  (defmethod initialize-instance :after ((instance bundle-op) &rest initargs
                                         &key (name-suffix nil name-suffix-p)
                                         &allow-other-keys)
    (declare (ignore initargs name-suffix))
    (unless name-suffix-p
      (setf (slot-value instance 'name-suffix)
            (unless (typep instance 'program-op)
              (if (operation-monolithic-p instance) "--all-systems" #-(or ecl mkcl) "--system")))) ; . no good for Logical Pathnames
    (when (typep instance 'monolithic-bundle-op)
      (destructuring-bind (&key lisp-files prologue-code epilogue-code
                           &allow-other-keys)
          (operation-original-initargs instance)
        (setf (prologue-code instance) prologue-code
              (epilogue-code instance) epilogue-code)
        #-ecl (assert (null (or lisp-files #-mkcl epilogue-code #-mkcl prologue-code)))
        #+ecl (setf (extra-object-files instance) lisp-files)))
    (setf (extra-build-args instance)
          (remove-plist-keys
           '(:type :monolithic :name-suffix :epilogue-code :prologue-code :lisp-files
             :force :force-not :plan-class) ;; TODO: refactor so we don't mix plan and operation arguments
           (operation-original-initargs instance))))

  (defun bundlable-file-p (pathname)
    (let ((type (pathname-type pathname)))
      (declare (ignorable type))
      (or #+ecl (or (equalp type (compile-file-type :type :object))
                    (equalp type (compile-file-type :type :static-library)))
          #+mkcl (or (equalp type (compile-file-type :fasl-p nil))
                     #+(or unix mingw32 mingw64) (equalp type "a") ;; valid on Unix and MinGW
                     #+(and windows (not (or mingw32 mingw64))) (equalp type "lib"))
          #+(or abcl allegro clisp clozure cmu lispworks sbcl scl xcl) (equalp type (compile-file-type)))))

  (defgeneric* (trivial-system-p) (component))

  (defun user-system-p (s)
    (and (typep s 'system)
         (not (builtin-system-p s))
         (not (trivial-system-p s)))))

(eval-when (#-lispworks :compile-toplevel :load-toplevel :execute)
  (deftype user-system () '(and system (satisfies user-system-p))))

;;;
;;; First we handle monolithic bundles.
;;; These are standalone systems which contain everything,
;;; including other ASDF systems required by the current one.
;;; A PROGRAM is always monolithic.
;;;
;;; MONOLITHIC SHARED LIBRARIES, PROGRAMS, FASL
;;;
(with-upgradability ()
  (defun direct-dependency-files (o c &key (test 'identity) (key 'output-files) &allow-other-keys)
    ;; This file selects output files from direct dependencies;
    ;; your component-depends-on method better gathered the correct dependencies in the correct order.
    (while-collecting (collect)
      (map-direct-dependencies
       t o c #'(lambda (sub-o sub-c)
                 (loop :for f :in (funcall key sub-o sub-c)
                       :when (funcall test f) :do (collect f))))))

  (defmethod input-files ((o gather-op) (c system))
    (unless (eq (bundle-type o) :no-output-file)
      (direct-dependency-files o c :test 'bundlable-file-p :key 'output-files)))

  (defun select-bundle-operation (type &optional monolithic)
    (ecase type
      ((:dll :shared-library)
       (if monolithic 'monolithic-dll-op 'dll-op))
      ((:lib :static-library)
       (if monolithic 'monolithic-lib-op 'lib-op))
      ((:fasl)
       (if monolithic 'monolithic-compile-bundle-op 'compile-bundle-op))
      ((:image)
       'image-op)
      ((:program)
       'program-op)))

  ;; DEPRECATED. This is originally from asdf-ecl.lisp. Does anyone use it?
  (defun make-build (system &rest args &key (monolithic nil) (type :fasl)
                             (move-here nil move-here-p)
                             &allow-other-keys)
    (let* ((operation-name (select-bundle-operation type monolithic))
           (move-here-path (if (and move-here
                                    (typep move-here '(or pathname string)))
                               (ensure-pathname move-here :namestring :lisp :ensure-directory t)
                               (system-relative-pathname system "asdf-output/")))
           (operation (apply #'operate operation-name
                             system
                             (remove-plist-keys '(:monolithic :type :move-here) args)))
           (system (find-system system))
           (files (and system (output-files operation system))))
      (if (or move-here (and (null move-here-p)
                             (member operation-name '(:program :image))))
          (loop :with dest-path = (resolve-symlinks* (ensure-directories-exist move-here-path))
                :for f :in files
                :for new-f = (make-pathname :name (pathname-name f)
                                            :type (pathname-type f)
                                            :defaults dest-path)
                :do (rename-file-overwriting-target f new-f)
                :collect new-f)
          files)))

  ;; DEPRECATED. Does anyone use this?
  (defun bundle-system (system &rest args &key force (verbose t) version &allow-other-keys)
    (declare (ignore force verbose version))
    (apply #'operate 'deliver-asd-op system args)))

;;;
;;; LOAD-BUNDLE-OP
;;;
;;; This is like ASDF's LOAD-OP, but using bundle fasl files.
;;;
(with-upgradability ()
  (defmethod component-depends-on ((o load-bundle-op) (c system))
    `((,o ,@(component-sideway-dependencies c))
      (,(if (user-system-p c) 'compile-bundle-op 'load-op) ,c)
      ,@(call-next-method)))

  (defmethod input-files ((o load-bundle-op) (c system))
    (when (user-system-p c)
      (output-files (find-operation o 'compile-bundle-op) c)))

  (defmethod perform ((o load-bundle-op) (c system))
    (when (input-files o c)
      (perform-lisp-load-fasl o c)))

  (defmethod mark-operation-done :after ((o load-bundle-op) (c system))
    (mark-operation-done (find-operation o 'load-op) c)))

;;;
;;; PRECOMPILED FILES
;;;
;;; This component can be used to distribute ASDF systems in precompiled form.
;;; Only useful when the dependencies have also been precompiled.
;;;
(with-upgradability ()
  (defmethod trivial-system-p ((s system))
    (every #'(lambda (c) (typep c 'compiled-file)) (component-children s)))

  (defmethod input-files ((o operation) (c compiled-file))
    (list (component-pathname c)))
  (defmethod perform ((o load-op) (c compiled-file))
    (perform-lisp-load-fasl o c))
  (defmethod perform ((o load-source-op) (c compiled-file))
    (perform (find-operation o 'load-op) c))
  (defmethod perform ((o operation) (c compiled-file))
    nil))

;;;
;;; Pre-built systems
;;;
(with-upgradability ()
  (defmethod trivial-system-p ((s prebuilt-system))
    t)

  (defmethod perform ((o link-op) (c prebuilt-system))
    nil)

  (defmethod perform ((o basic-compile-bundle-op) (c prebuilt-system))
    nil)

  (defmethod perform ((o lib-op) (c prebuilt-system))
    nil)

  (defmethod perform ((o dll-op) (c prebuilt-system))
    nil)

  (defmethod component-depends-on ((o gather-op) (c prebuilt-system))
    nil)

  (defmethod output-files ((o lib-op) (c prebuilt-system))
    (values (list (prebuilt-system-static-library c)) t)))


;;;
;;; PREBUILT SYSTEM CREATOR
;;;
(with-upgradability ()
  (defmethod output-files ((o deliver-asd-op) (s system))
    (list (make-pathname :name (component-name s) :type "asd"
                         :defaults (component-pathname s))))

  (defmethod perform ((o deliver-asd-op) (s system))
    (let* ((inputs (input-files o s))
           (fasl (first inputs))
           (library (second inputs))
           (asd (first (output-files o s)))
           (name (if (and fasl asd) (pathname-name asd) (return-from perform)))
           (version (component-version s))
           (dependencies
             (if (operation-monolithic-p o)
                 (remove-if-not 'builtin-system-p
                                (required-components s :component-type 'system
                                                       :keep-operation 'load-op))
                 (while-collecting (x) ;; resolve the sideway-dependencies of s
                   (map-direct-dependencies
                    t 'load-op s
                    #'(lambda (o c)
                        (when (and (typep o 'load-op) (typep c 'system))
                          (x c)))))))
           (depends-on (mapcar 'coerce-name dependencies)))
      (when (pathname-equal asd (system-source-file s))
        (cerror "overwrite the asd file"
                "~/asdf-action:format-action/ is going to overwrite the system definition file ~S which is probably not what you want; you probably need to tweak your output translations."
                (cons o s) asd))
      (with-open-file (s asd :direction :output :if-exists :supersede
                             :if-does-not-exist :create)
        (format s ";;; Prebuilt~:[~; monolithic~] ASDF definition for system ~A~%"
                (operation-monolithic-p o) name)
        (format s ";;; Built for ~A ~A on a ~A/~A ~A~%"
                (lisp-implementation-type)
                (lisp-implementation-version)
                (software-type)
                (machine-type)
                (software-version))
        (let ((*package* (find-package :asdf-user)))
          (pprint `(defsystem ,name
                     :class prebuilt-system
                     :version ,version
                     :depends-on ,depends-on
                     :components ((:compiled-file ,(pathname-name fasl)))
                     ,@(when library `(:lib ,(file-namestring library))))
                  s)
          (terpri s)))))

  #-(or ecl mkcl)
  (defmethod perform ((o basic-compile-bundle-op) (c system))
    (let* ((input-files (input-files o c))
           (fasl-files (remove (compile-file-type) input-files :key #'pathname-type :test-not #'equalp))
           (non-fasl-files (remove (compile-file-type) input-files :key #'pathname-type :test #'equalp))
           (output-files (output-files o c))
           (output-file (first output-files)))
      (assert (eq (not input-files) (not output-files)))
      (when input-files
        (when non-fasl-files
          (error "On ~A, asdf/bundle can only bundle FASL files, but these were also produced: ~S"
                 (implementation-type) non-fasl-files))
        (when (or (prologue-code o) (epilogue-code o)
                  (prologue-code c) (epilogue-code c))
          (error "prologue-code and epilogue-code are not supported on ~A"
                 (implementation-type)))
        (with-staging-pathname (output-file)
          (combine-fasls fasl-files output-file)))))

  (defmethod input-files ((o load-op) (s precompiled-system))
    (bundle-output-files (find-operation o 'compile-bundle-op) s))

  (defmethod perform ((o load-op) (s precompiled-system))
    (perform-lisp-load-fasl o s))

  (defmethod component-depends-on ((o load-bundle-op) (s precompiled-system))
    #+xcl (declare (ignorable o))
    `((load-op ,s) ,@(call-next-method))))

#| ;; Example use:
(asdf:defsystem :precompiled-asdf-utils :class asdf::precompiled-system :fasl (asdf:apply-output-translations (asdf:system-relative-pathname :asdf-utils "asdf-utils.system.fasl")))
(asdf:load-system :precompiled-asdf-utils)
|#

#+(or ecl mkcl)
(with-upgradability ()
  ;; I think that Juanjo intended for this to be,
  ;; but beware the weird bug in test-xach-update-bug.script,
  ;; and also it makes mkcl fail test-logical-pathname.script,
  ;; and ecl fail test-bundle.script.
  ;;(unless (or #+ecl (use-ecl-byte-compiler-p))
  ;;  (setf *load-system-operation* 'load-bundle-op))

  (defun asdf-library-pathname ()
    #+ecl (or (probe-file* (compile-file-pathname "sys:asdf" :type :lib)) ;; new style
              (probe-file* (compile-file-pathname "sys:asdf" :type :object))) ;; old style
    #+mkcl (make-pathname :type (bundle-pathname-type :lib) :defaults #p"sys:contrib;asdf"))

  (defun compiler-library-pathname ()
    #+ecl (compile-file-pathname "sys:cmp" :type :lib)
    #+mkcl (make-pathname :type (bundle-pathname-type :lib) :defaults #p"sys:cmp"))

  (defun make-library-system (name pathname)
    (make-instance 'prebuilt-system
                   :name (coerce-name name) :static-library (resolve-symlinks* pathname)))

  (defmethod component-depends-on :around ((o image-op) (c system))
    (destructuring-bind ((lib-op . deps)) (call-next-method)
      (flet ((has-it-p (x) (find x deps :test 'equal :key 'coerce-name)))
        `((,lib-op
           ,@(unless (or (no-uiop c) (has-it-p "cmp"))
               `(,(make-library-system
                   "cmp" (compiler-library-pathname))))
           ,@(unless (or (no-uiop c) (has-it-p "uiop") (has-it-p "asdf"))
               `(,(cond
                    ((system-source-directory :uiop) (find-system :uiop))
                    ((system-source-directory :asdf) (find-system :asdf))
                    (t (make-library-system "asdf" (asdf-library-pathname))))))
           ,@deps)))))

  (defmethod perform ((o link-op) (c system))
    (let* ((object-files (input-files o c))
           (output (output-files o c))
           (bundle (first output))
           (programp (typep o 'program-op))
           (kind (bundle-type o)))
      (when output
        (apply 'create-image
               bundle (append
                       (when programp (prefix-lisp-object-files c))
                       object-files
                       (when programp (postfix-lisp-object-files c)))
               :kind kind
               :prologue-code (or (prologue-code o) (when programp (prologue-code c)))
               :epilogue-code (or (epilogue-code o) (when programp (epilogue-code c)))
               :build-args (or (extra-build-args o) (when programp (extra-build-args c)))
               :extra-object-files (or (extra-object-files o) (when programp (extra-object-files c)))
               :no-uiop (no-uiop c)
               (when programp `(:entry-point ,(component-entry-point c))))))))

#+(and (not asdf-use-unsafe-mac-bundle-op)
       (or (and ecl darwin)
           (and abcl darwin (not abcl-bundle-op-supported))))
(defmethod perform :before ((o basic-compile-bundle-op) (c component))
  (unless (featurep :asdf-use-unsafe-mac-bundle-op)
    (cerror "Continue after modifying *FEATURES*."
            "BASIC-COMPILE-BUNDLE-OP operations are not supported on Mac OS X for this lisp.~%~T~
To continue, push :asdf-use-unsafe-mac-bundle-op onto *FEATURES*.~%~T~
Please report to ASDF-DEVEL if this works for you.")))


;;; Backward compatibility with pre-3.1.1 names
(defclass fasl-op (selfward-operation)
  ((selfward-operation :initform 'compile-bundle-op :allocation :class)))
(defclass load-fasl-op (selfward-operation)
  ((selfward-operation :initform 'load-bundle-op :allocation :class)))
(defclass binary-op (selfward-operation)
  ((selfward-operation :initform 'deliver-asd-op :allocation :class)))
(defclass monolithic-fasl-op (selfward-operation)
  ((selfward-operation :initform 'monolithic-compile-bundle-op :allocation :class)))
(defclass monolithic-load-fasl-op (selfward-operation)
  ((selfward-operation :initform 'monolithic-load-bundle-op :allocation :class)))
(defclass monolithic-binary-op (selfward-operation)
  ((selfward-operation :initform 'monolithic-deliver-asd-op :allocation :class)))
;;;; -------------------------------------------------------------------------
;;;; Concatenate-source

(uiop/package:define-package :asdf/concatenate-source
  (:recycle :asdf/concatenate-source :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
   :asdf/component :asdf/operation
   :asdf/system :asdf/find-system
   :asdf/action :asdf/lisp-action :asdf/bundle)
  (:export
   #:concatenate-source-op
   #:load-concatenated-source-op
   #:compile-concatenated-source-op
   #:load-compiled-concatenated-source-op
   #:monolithic-concatenate-source-op
   #:monolithic-load-concatenated-source-op
   #:monolithic-compile-concatenated-source-op
   #:monolithic-load-compiled-concatenated-source-op))
(in-package :asdf/concatenate-source)

;;;
;;; Concatenate sources
;;;
(with-upgradability ()
  (defclass basic-concatenate-source-op (bundle-op)
    ((bundle-type :initform "lisp")))
  (defclass basic-load-concatenated-source-op (basic-load-op selfward-operation) ())
  (defclass basic-compile-concatenated-source-op (basic-compile-op selfward-operation) ())
  (defclass basic-load-compiled-concatenated-source-op (basic-load-op selfward-operation) ())

  (defclass concatenate-source-op (basic-concatenate-source-op non-propagating-operation) ())
  (defclass load-concatenated-source-op (basic-load-concatenated-source-op)
    ((selfward-operation :initform '(prepare-op concatenate-source-op) :allocation :class)))
  (defclass compile-concatenated-source-op (basic-compile-concatenated-source-op)
    ((selfward-operation :initform '(prepare-op concatenate-source-op) :allocation :class)))
  (defclass load-compiled-concatenated-source-op (basic-load-compiled-concatenated-source-op)
    ((selfward-operation :initform '(prepare-op compile-concatenated-source-op) :allocation :class)))

  (defclass monolithic-concatenate-source-op (basic-concatenate-source-op monolithic-bundle-op non-propagating-operation) ())
  (defclass monolithic-load-concatenated-source-op (basic-load-concatenated-source-op)
    ((selfward-operation :initform 'monolithic-concatenate-source-op :allocation :class)))
  (defclass monolithic-compile-concatenated-source-op (basic-compile-concatenated-source-op)
    ((selfward-operation :initform 'monolithic-concatenate-source-op :allocation :class)))
  (defclass monolithic-load-compiled-concatenated-source-op (basic-load-compiled-concatenated-source-op)
    ((selfward-operation :initform 'monolithic-compile-concatenated-source-op :allocation :class)))

  (defmethod input-files ((operation basic-concatenate-source-op) (s system))
    (loop :with encoding = (or (component-encoding s) *default-encoding*)
          :with other-encodings = '()
          :with around-compile = (around-compile-hook s)
          :with other-around-compile = '()
          :for c :in (required-components
                      s :goal-operation 'compile-op
                        :keep-operation 'compile-op
                        :other-systems (operation-monolithic-p operation))
          :append
          (when (typep c 'cl-source-file)
            (let ((e (component-encoding c)))
              (unless (equal e encoding)
                (let ((a (assoc e other-encodings)))
                  (if a (push (component-find-path c) (cdr a))
                      (push (list a (component-find-path c)) other-encodings)))))
            (unless (equal around-compile (around-compile-hook c))
              (push (component-find-path c) other-around-compile))
            (input-files (make-operation 'compile-op) c)) :into inputs
          :finally
             (when other-encodings
               (warn "~S uses encoding ~A but has sources that use these encodings:~{ ~A~}"
                     operation encoding
                     (mapcar #'(lambda (x) (cons (car x) (list (reverse (cdr x)))))
                             other-encodings)))
             (when other-around-compile
               (warn "~S uses around-compile hook ~A but has sources that use these hooks: ~A"
                     operation around-compile other-around-compile))
             (return inputs)))
  (defmethod output-files ((o basic-compile-concatenated-source-op) (s system))
    (lisp-compilation-output-files o s))

  (defmethod perform ((o basic-concatenate-source-op) (s system))
    (let* ((ins (input-files o s))
           (out (output-file o s))
           (tmp (tmpize-pathname out)))
      (concatenate-files ins tmp)
      (rename-file-overwriting-target tmp out)))
  (defmethod perform ((o basic-load-concatenated-source-op) (s system))
    (perform-lisp-load-source o s))
  (defmethod perform ((o basic-compile-concatenated-source-op) (s system))
    (perform-lisp-compilation o s))
  (defmethod perform ((o basic-load-compiled-concatenated-source-op) (s system))
    (perform-lisp-load-fasl o s)))

;;;; -------------------------------------------------------------------------
;;; Backward-compatible interfaces

(uiop/package:define-package :asdf/backward-interface
  (:recycle :asdf/backward-interface :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
   :asdf/component :asdf/system :asdf/find-system :asdf/operation :asdf/action
   :asdf/lisp-action :asdf/plan :asdf/operate :asdf/output-translations)
  (:export
   #:*asdf-verbose*
   #:operation-error #:compile-error #:compile-failed #:compile-warned
   #:error-component #:error-operation #:traverse
   #:component-load-dependencies
   #:enable-asdf-binary-locations-compatibility
   #:operation-forced
   #:operation-on-failure #:operation-on-warnings #:on-failure #:on-warnings
   #:component-property
   #:run-shell-command
   #:system-definition-pathname))
(in-package :asdf/backward-interface)

(with-upgradability ()
  (define-condition operation-error (error) ;; Bad, backward-compatible name
    ;; Used by SBCL, cffi-tests, clsql-mysql, clsql-uffi, qt, elephant, uffi-tests, sb-grovel
    ((component :reader error-component :initarg :component)
     (operation :reader error-operation :initarg :operation))
    (:report (lambda (c s)
               (format s (compatfmt "~@<~A while invoking ~A on ~A~@:>")
                       (type-of c) (error-operation c) (error-component c)))))
  (define-condition compile-error (operation-error) ())
  (define-condition compile-failed (compile-error) ())
  (define-condition compile-warned (compile-error) ())

  (defun component-load-dependencies (component)
    ;; Old deprecated name for the same thing. Please update your software.
    (component-sideway-dependencies component))

  (defgeneric operation-forced (operation)) ;; Used by swank.asd for swank-loader.
  (defmethod operation-forced ((o operation)) (getf (operation-original-initargs o) :force))

  (defgeneric operation-on-warnings (operation))
  (defgeneric operation-on-failure (operation))
  (defgeneric (setf operation-on-warnings) (x operation))
  (defgeneric (setf operation-on-failure) (x operation))
  (defmethod operation-on-warnings ((o operation))
    *compile-file-warnings-behaviour*)
  (defmethod operation-on-failure ((o operation))
    *compile-file-failure-behaviour*)
  (defmethod (setf operation-on-warnings) (x (o operation))
    (setf *compile-file-warnings-behaviour* x))
  (defmethod (setf operation-on-failure) (x (o operation))
    (setf *compile-file-failure-behaviour* x))

  (defun system-definition-pathname (x)
    ;; As of 2.014.8, we mean to make this function obsolete,
    ;; but that won't happen until all clients have been updated.
    ;;(cerror "Use ASDF:SYSTEM-SOURCE-FILE instead"
    "Function ASDF:SYSTEM-DEFINITION-PATHNAME is obsolete.
It used to expose ASDF internals with subtle differences with respect to
user expectations, that have been refactored away since.
We recommend you use ASDF:SYSTEM-SOURCE-FILE instead
for a mostly compatible replacement that we're supporting,
or even ASDF:SYSTEM-SOURCE-DIRECTORY or ASDF:SYSTEM-RELATIVE-PATHNAME
if that's whay you mean." ;;)
    (system-source-file x))

  (defgeneric* (traverse) (operation component &key &allow-other-keys)
    (:documentation
     "Generate and return a plan for performing OPERATION on COMPONENT.

The plan returned is a list of dotted-pairs. Each pair is the CONS
of ASDF operation object and a COMPONENT object. The pairs will be
processed in order by OPERATE."))
  (define-convenience-action-methods traverse (operation component &key))

  (defmethod traverse ((o operation) (c component) &rest keys &key plan-class &allow-other-keys)
    (plan-actions (apply 'make-plan plan-class o c keys))))


;;;; ASDF-Binary-Locations compatibility
;; This remains supported for legacy user, but not recommended for new users.
(with-upgradability ()
  (defun enable-asdf-binary-locations-compatibility
      (&key
       (centralize-lisp-binaries nil)
       (default-toplevel-directory
        (subpathname (user-homedir-pathname) ".fasls/")) ;; Use ".cache/common-lisp/" instead ???
       (include-per-user-information nil)
       (map-all-source-files (or #+(or clisp ecl mkcl) t nil))
       (source-to-target-mappings nil)
       (file-types `(,(compile-file-type)
                     "build-report"
                     #+ecl (compile-file-type :type :object)
                     #+mkcl (compile-file-type :fasl-p nil)
                     #+clisp "lib" #+sbcl "cfasl"
                     #+sbcl "sbcl-warnings" #+clozure "ccl-warnings")))
    #+(or clisp ecl mkcl)
    (when (null map-all-source-files)
      (error "asdf:enable-asdf-binary-locations-compatibility doesn't support :map-all-source-files nil on CLISP, ECL and MKCL"))
    (let* ((patterns (if map-all-source-files (list *wild-file*)
                         (loop :for type :in file-types
                               :collect (make-pathname :type type :defaults *wild-file*))))
           (destination-directory
             (if centralize-lisp-binaries
                 `(,default-toplevel-directory
                   ,@(when include-per-user-information
                       (cdr (pathname-directory (user-homedir-pathname))))
                   :implementation ,*wild-inferiors*)
                 `(:root ,*wild-inferiors* :implementation))))
      (initialize-output-translations
       `(:output-translations
         ,@source-to-target-mappings
         #+abcl (#p"jar:file:/**/*.jar!/**/*.*" (:function translate-jar-pathname))
         #+abcl (#p"/___jar___file___root___/**/*.*" (,@destination-directory))
         ,@(loop :for pattern :in patterns
                 :collect `((:root ,*wild-inferiors* ,pattern)
                            (,@destination-directory ,pattern)))
         (t t)
         :ignore-inherited-configuration))))

  (defmethod operate :before (operation-class system &rest args &key &allow-other-keys)
    (declare (ignore operation-class system args))
    (when (find-symbol* '#:output-files-for-system-and-operation :asdf nil)
      (error "ASDF 2 is not compatible with ASDF-BINARY-LOCATIONS, which you are using.
ASDF 2 now achieves the same purpose with its builtin ASDF-OUTPUT-TRANSLATIONS,
which should be easier to configure. Please stop using ASDF-BINARY-LOCATIONS,
and instead use ASDF-OUTPUT-TRANSLATIONS. See the ASDF manual for details.
In case you insist on preserving your previous A-B-L configuration, but
do not know how to achieve the same effect with A-O-T, you may use function
ASDF:ENABLE-ASDF-BINARY-LOCATIONS-COMPATIBILITY as documented in the manual;
call that function where you would otherwise have loaded and configured A-B-L."))))


;;; run-shell-command
;; WARNING! The function below is not just deprecated but also dysfunctional.
;; Please use asdf/run-program:run-program instead.
(with-upgradability ()
  (defun run-shell-command (control-string &rest args)
    "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, with
output to *VERBOSE-OUT*.  Returns the shell's exit code.

PLEASE DO NOT USE.
Deprecated function, for backward-compatibility only.
Please use UIOP:RUN-PROGRAM instead."
    (let ((command (apply 'format nil control-string args)))
      (asdf-message "; $ ~A~%" command)
      (let ((exit-code
              (ignore-errors
               (nth-value 2 (run-program command :force-shell t :ignore-error-status t
                                                 :output *verbose-out*)))))
        (typecase exit-code
          ((integer 0 255) exit-code)
          (t 255))))))

(with-upgradability ()
  (defvar *asdf-verbose* nil)) ;; backward-compatibility with ASDF2 only. Unused.

;; backward-compatibility methods. Do NOT use in new code. NOT SUPPORTED.
(with-upgradability ()
  (defgeneric component-property (component property))
  (defgeneric (setf component-property) (new-value component property))

  (defmethod component-property ((c component) property)
    (cdr (assoc property (slot-value c 'properties) :test #'equal)))

  (defmethod (setf component-property) (new-value (c component) property)
    (let ((a (assoc property (slot-value c 'properties) :test #'equal)))
      (if a
          (setf (cdr a) new-value)
          (setf (slot-value c 'properties)
                (acons property new-value (slot-value c 'properties)))))
    new-value))
;;;; -------------------------------------------------------------------------
;;;; Package systems in the style of quick-build or faslpath

(uiop:define-package :asdf/package-inferred-system
  (:recycle :asdf/package-inferred-system :asdf/package-system :asdf)
  (:use :uiop/common-lisp :uiop
        :asdf/defsystem ;; Using the old name of :asdf/parse-defsystem for compatibility
        :asdf/upgrade :asdf/component :asdf/system :asdf/find-system :asdf/lisp-action)
  (:export
   #:package-inferred-system #:sysdef-package-inferred-system-search
   #:package-system ;; backward compatibility only. To be removed.
   #:register-system-packages
   #:*defpackage-forms* #:*package-inferred-systems* #:package-inferred-system-missing-package-error))
(in-package :asdf/package-inferred-system)

(with-upgradability ()
  (defparameter *defpackage-forms* '(cl:defpackage uiop:define-package))

  (defun initial-package-inferred-systems-table ()
    (let ((h (make-hash-table :test 'equal)))
      (dolist (p (list-all-packages))
        (dolist (n (package-names p))
          (setf (gethash n h) t)))
      h))

  (defvar *package-inferred-systems* (initial-package-inferred-systems-table))

  (defclass package-inferred-system (system)
    ())

  ;; For backward compatibility only. To be removed in an upcoming release:
  (defclass package-system (package-inferred-system) ())

  (defun defpackage-form-p (form)
    (and (consp form)
         (member (car form) *defpackage-forms*)))

  (defun stream-defpackage-form (stream)
    (loop :for form = (read stream nil nil) :while form
          :when (defpackage-form-p form) :return form))

  (defun file-defpackage-form (file)
    "Return the first DEFPACKAGE form in FILE."
    (with-input-file (f file)
      (stream-defpackage-form f)))

  (define-condition package-inferred-system-missing-package-error (system-definition-error)
    ((system :initarg :system :reader error-system)
     (pathname :initarg :pathname :reader error-pathname))
    (:report (lambda (c s)
               (format s (compatfmt "~@<No package form found while ~
                                     trying to define package-inferred-system ~A from file ~A~>")
                       (error-system c) (error-pathname c)))))

  (defun package-dependencies (defpackage-form)
    "Return a list of packages depended on by the package
defined in DEFPACKAGE-FORM.  A package is depended upon if
the DEFPACKAGE-FORM uses it or imports a symbol from it."
    (assert (defpackage-form-p defpackage-form))
    (remove-duplicates
     (while-collecting (dep)
       (loop* :for (option . arguments) :in (cddr defpackage-form) :do
              (ecase option
                ((:use :mix :reexport :use-reexport :mix-reexport)
                 (dolist (p arguments) (dep (string p))))
                ((:import-from :shadowing-import-from)
                 (dep (string (first arguments))))
                ((:nicknames :documentation :shadow :export :intern :unintern :recycle)))))
     :from-end t :test 'equal))

  (defun package-designator-name (package)
    (etypecase package
      (package (package-name package))
      (string package)
      (symbol (string package))))

  (defun register-system-packages (system packages)
    "Register SYSTEM as providing PACKAGES."
    (let ((name (or (eq system t) (coerce-name system))))
      (dolist (p (ensure-list packages))
        (setf (gethash (package-designator-name p) *package-inferred-systems*) name))))

  (defun package-name-system (package-name)
    "Return the name of the SYSTEM providing PACKAGE-NAME, if such exists,
otherwise return a default system name computed from PACKAGE-NAME."
    (check-type package-name string)
    (if-let ((system-name (gethash package-name *package-inferred-systems*)))
      system-name
      (string-downcase package-name)))

  (defun package-inferred-system-file-dependencies (file &optional system)
    (if-let (defpackage-form (file-defpackage-form file))
      (remove t (mapcar 'package-name-system (package-dependencies defpackage-form)))
      (error 'package-inferred-system-missing-package-error :system system :pathname file)))

  (defun same-package-inferred-system-p (system name directory subpath dependencies)
    (and (eq (type-of system) 'package-inferred-system)
         (equal (component-name system) name)
         (pathname-equal directory (component-pathname system))
         (equal dependencies (component-sideway-dependencies system))
         (let ((children (component-children system)))
           (and (length=n-p children 1)
                (let ((child (first children)))
                  (and (eq (type-of child) 'cl-source-file)
                       (equal (component-name child) "lisp")
                       (and (slot-boundp child 'relative-pathname)
                            (equal (slot-value child 'relative-pathname) subpath))))))))

  (defun sysdef-package-inferred-system-search (system)
    (let ((primary (primary-system-name system)))
      (unless (equal primary system)
        (let ((top (find-system primary nil)))
          (when (typep top 'package-inferred-system)
            (if-let (dir (system-source-directory top))
              (let* ((sub (subseq system (1+ (length primary))))
                     (f (probe-file* (subpathname dir sub :type "lisp")
                                     :truename *resolve-symlinks*)))
                (when (file-pathname-p f)
                  (let ((dependencies (package-inferred-system-file-dependencies f system))
                        (previous (cdr (system-registered-p system))))
                    (if (same-package-inferred-system-p previous system dir sub dependencies)
                        previous
                        (eval `(defsystem ,system
                                 :class package-inferred-system
                                 :source-file nil
                                 :pathname ,dir
                                 :depends-on ,dependencies
                                 :components ((cl-source-file "lisp" :pathname ,sub)))))))))))))))

(with-upgradability ()
  (pushnew 'sysdef-package-inferred-system-search *system-definition-search-functions*)
  (setf *system-definition-search-functions*
        (remove (find-symbol* :sysdef-package-system-search :asdf/package-system nil)
                *system-definition-search-functions*)))
;;;; ---------------------------------------------------------------------------
;;;; Handle ASDF package upgrade, including implementation-dependent magic.

(uiop/package:define-package :asdf/interface
  (:nicknames :asdf :asdf-utilities)
  (:recycle :asdf/interface :asdf)
  (:unintern
   #:loaded-systems ; makes for annoying SLIME completion
   #:output-files-for-system-and-operation) ; ASDF-BINARY-LOCATION function we use to detect ABL
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/cache
   :asdf/component :asdf/system :asdf/find-system :asdf/find-component
   :asdf/operation :asdf/action :asdf/lisp-action
   :asdf/output-translations :asdf/source-registry
   :asdf/plan :asdf/operate :asdf/parse-defsystem :asdf/bundle :asdf/concatenate-source
   :asdf/backward-internals :asdf/backward-interface :asdf/package-inferred-system)
  ;; Note: (1) we are NOT automatically reexporting everything from previous packages.
  ;; (2) we only reexport UIOP functionality when backward-compatibility requires it.
  (:export
   #:defsystem #:find-system #:locate-system #:coerce-name #:primary-system-name
   #:oos #:operate #:make-plan #:perform-plan #:sequential-plan
   #:system-definition-pathname
   #:search-for-system-definition #:find-component #:component-find-path
   #:compile-system #:load-system #:load-systems #:load-systems*
   #:require-system #:test-system #:clear-system
   #:operation #:make-operation #:find-operation
   #:upward-operation #:downward-operation #:sideway-operation #:selfward-operation
                      #:non-propagating-operation
   #:build-op #:make
   #:load-op #:prepare-op #:compile-op
   #:prepare-source-op #:load-source-op #:test-op
   #:feature #:version #:version-satisfies #:upgrade-asdf
   #:implementation-identifier #:implementation-type #:hostname
   #:input-files #:output-files #:output-file #:perform #:perform-with-restarts
   #:operation-done-p #:explain #:action-description #:component-sideway-dependencies
   #:needed-in-image-p
   #:component-load-dependencies #:run-shell-command ; deprecated, do not use
   #:bundle-op #:monolithic-bundle-op #:precompiled-system #:compiled-file #:bundle-system
   #:program-system #:make-build
   #:fasl-op #:load-fasl-op #:monolithic-fasl-op #:binary-op #:monolithic-binary-op
   #:basic-compile-bundle-op #:prepare-bundle-op
   #:compile-bundle-op #:load-bundle-op #:monolithic-compile-bundle-op #:monolithic-load-bundle-op
   #:lib-op #:dll-op #:deliver-asd-op #:program-op #:image-op
   #:monolithic-lib-op #:monolithic-dll-op #:monolithic-deliver-asd-op
   #:concatenate-source-op
   #:load-concatenated-source-op
   #:compile-concatenated-source-op
   #:load-compiled-concatenated-source-op
   #:monolithic-concatenate-source-op
   #:monolithic-load-concatenated-source-op
   #:monolithic-compile-concatenated-source-op
   #:monolithic-load-compiled-concatenated-source-op
   #:operation-monolithic-p
   #:required-components
   #:component-loaded-p

   #:component #:parent-component #:child-component #:system #:module
   #:file-component #:source-file #:c-source-file #:java-source-file
   #:cl-source-file #:cl-source-file.cl #:cl-source-file.lsp
   #:static-file #:doc-file #:html-file
   #:file-type #:source-file-type

   #:package-inferred-system #:register-system-packages
   #:package-system ;; backward-compatibility during migration, to be removed in a further release.

   #:component-children          ; component accessors
   #:component-children-by-name
   #:component-pathname
   #:component-relative-pathname
   #:component-name
   #:component-version
   #:component-parent
   #:component-system
   #:component-encoding
   #:component-external-format

   #:component-depends-on ; backward-compatible name rather than action-depends-on
   #:module-components ; backward-compatibility
   #:operation-on-warnings #:operation-on-failure ; backward-compatibility
   #:component-property ; backward-compatibility
   #:traverse ; backward-compatibility

   #:system-description
   #:system-long-description
   #:system-author
   #:system-maintainer
   #:system-license
   #:system-licence
   #:system-source-file
   #:system-source-directory
   #:system-relative-pathname
   #:system-homepage
   #:system-mailto
   #:system-bug-tracker
   #:system-long-name
   #:system-source-control
   #:map-systems
   #:system-defsystem-depends-on
   #:system-depends-on
   #:system-weakly-depends-on

   #:*system-definition-search-functions*   ; variables
   #:*central-registry*
   #:*compile-file-warnings-behaviour*
   #:*compile-file-failure-behaviour*
   #:*resolve-symlinks*
   #:*load-system-operation* #:*immutable-systems*
   #:*asdf-verbose* ;; unused. For backward-compatibility only.
   #:*verbose-out*

   #:asdf-version

   #:compile-condition #:compile-file-error #:compile-warned-error #:compile-failed-error
   #:compile-warned-warning #:compile-failed-warning
   #:operation-error #:compile-failed #:compile-warned #:compile-error ;; backward compatibility
   #:error-name
   #:error-pathname
   #:load-system-definition-error
   #:error-component #:error-operation
   #:system-definition-error
   #:missing-component
   #:missing-component-of-version
   #:missing-dependency
   #:missing-dependency-of-version
   #:circular-dependency        ; errors
   #:duplicate-names #:non-toplevel-system #:non-system-system
   #:package-inferred-system-missing-package-error
   #:operation-definition-warning #:operation-definition-error

   #:try-recompiling
   #:retry
   #:accept                     ; restarts
   #:coerce-entry-to-directory
   #:remove-entry-from-registry

   #:*encoding-detection-hook*
   #:*encoding-external-format-hook*
   #:*default-encoding*
   #:*utf-8-external-format*

   #:clear-configuration
   #:*output-translations-parameter*
   #:initialize-output-translations
   #:disable-output-translations
   #:clear-output-translations
   #:ensure-output-translations
   #:apply-output-translations
   #:compile-file*
   #:compile-file-pathname*
   #:*warnings-file-type* #:enable-deferred-warnings-check #:disable-deferred-warnings-check
   #:enable-asdf-binary-locations-compatibility
   #:*default-source-registries*
   #:*source-registry-parameter*
   #:initialize-source-registry
   #:compute-source-registry
   #:clear-source-registry
   #:ensure-source-registry
   #:process-source-registry
   #:system-registered-p #:registered-systems #:already-loaded-systems
   #:resolve-location
   #:asdf-message
   #:*user-cache*
   #:user-output-translations-pathname
   #:system-output-translations-pathname
   #:user-output-translations-directory-pathname
   #:system-output-translations-directory-pathname
   #:user-source-registry
   #:system-source-registry
   #:user-source-registry-directory
   #:system-source-registry-directory))

;;;; ---------------------------------------------------------------------------
;;;; ASDF-USER, where the action happens.

(uiop/package:define-package :asdf/user
  (:nicknames :asdf-user)
  ;; NB: releases before 3.1.1 this :use'd only uiop/package instead of uiop below.
  ;; They also :use'd uiop/common-lisp, that reexports common-lisp and is not included in uiop.
  ;; ASDF3 releases from 2.27 to 2.31 called uiop asdf-driver and asdf/foo uiop/foo.
  ;; ASDF1 and ASDF2 releases (2.26 and earlier) create a temporary package
  ;; that only :use's :cl and :asdf
  (:use :uiop/common-lisp :uiop :asdf/interface))
;;;; -----------------------------------------------------------------------
;;;; ASDF Footer: last words and cleanup

(uiop/package:define-package :asdf/footer
  (:recycle :asdf/footer :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/operate :asdf/bundle))
(in-package :asdf/footer)

;;;; Hook ASDF into the implementation's REQUIRE and other entry points.
#+(or abcl clisp clozure cmu ecl mkcl sbcl)
(with-upgradability ()
  (if-let (x (and #+clisp (find-symbol* '#:*module-provider-functions* :custom nil)))
    (eval `(pushnew 'module-provide-asdf
                    #+abcl sys::*module-provider-functions*
                    #+clisp ,x
                    #+clozure ccl:*module-provider-functions*
                    #+(or cmu ecl) ext:*module-provider-functions*
                    #+mkcl mk-ext:*module-provider-functions*
                    #+sbcl sb-ext:*module-provider-functions*)))

  #+(or ecl mkcl)
  (progn
    (pushnew '("fasb" . si::load-binary) si::*load-hooks* :test 'equal :key 'car)

    #+(or (and ecl win32) (and mkcl windows))
    (unless (assoc "asd" #+ecl ext:*load-hooks* #+mkcl si::*load-hooks* :test 'equal)
      (appendf #+ecl ext:*load-hooks* #+mkcl si::*load-hooks* '(("asd" . si::load-source))))

    (setf #+ecl ext:*module-provider-functions* #+mkcl mk-ext::*module-provider-functions*
          (loop :for f :in #+ecl ext:*module-provider-functions*
                #+mkcl mk-ext::*module-provider-functions*
                :collect
                (if (eq f 'module-provide-asdf) f
                    #'(lambda (name)
                        (let ((l (multiple-value-list (funcall f name))))
                          (and (first l) (register-pre-built-system (coerce-name name)))
                          (values-list l))))))))

#+cmu ;; Hook into the CMUCL herald.
(with-upgradability ()
  (defun herald-asdf (stream)
    (format stream "    ASDF ~A" (asdf-version)))
  (setf (getf ext:*herald-items* :asdf) `(herald-asdf)))


;;;; Done!
(with-upgradability ()
  #+allegro
  (when (boundp 'excl:*warn-on-nested-reader-conditionals*)
    (setf excl:*warn-on-nested-reader-conditionals* asdf/common-lisp::*acl-warn-save*))

  (dolist (f '(:asdf :asdf2 :asdf3 :asdf3.1 :asdf-package-system)) (pushnew f *features*))

  ;; Provide both lowercase and uppercase, to satisfy more people, especially LispWorks users.
  (provide "asdf") (provide "ASDF")

  (cleanup-upgraded-asdf))

(when *load-verbose*
  (asdf-message ";; ASDF, version ~a~%" (asdf-version)))

