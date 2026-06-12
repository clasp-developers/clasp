;;;; cffi-fsbv-stress.lisp --- Regression test for CFFI struct-by-value on Clasp
;;;;
;;;; Reproduces and guards against the use-after-free that corrupted memory
;;;; after a few hundred struct-by-value calls (libffi cif sub-structures were
;;;; freed by the GC finalizer of their ForeignData wrapper). See the two fixes
;;;; in src/cffi-clasp.lisp (foreign-alloc registry + :default symbol lookup).
;;;;
;;;; Usage (from the clasp top dir):
;;;;   build/boehm/iclasp --non-interactive --load src/tests/fli/cffi-fsbv-stress.lisp
;;;;
;;;; Exit code 0 = all tests passed, 1 = a failure/crash was observed.

(require :asdf)

(defparameter *here*
  (make-pathname :directory (pathname-directory *load-truename*)))

(defparameter *contrib*
  (truename (merge-pathnames "../../lisp/kernel/contrib/" *here*)))

(asdf:initialize-source-registry
 `(:source-registry (:tree ,*contrib*) :inherit-configuration))

(handler-bind ((warning #'muffle-warning))
  (asdf:load-system :cffi-libffi))

;;; Build libfsbv.dylib from the C source that already lives in this directory.
(defparameter *lib* (namestring (merge-pathnames "libfsbv.dylib" *here*)))

(let ((src (namestring (merge-pathnames "libfsbv.c" *here*))))
  (uiop:run-program (list "cc" "-dynamiclib" "-o" *lib* src)
                    :error-output *error-output*))

(cffi:load-foreign-library *lib*)

;;; === Foreign struct + function definitions (mirror libfsbv.c) ===

(cffi:defcstruct struct-pair (a :int) (b :int))
(cffi:defcstruct struct-pair-double (pr (:struct struct-pair)) (dbl :double))

(cffi:defcfun ("sumpair" c-sumpair) :int (sp (:struct struct-pair)))
(cffi:defcfun ("doublepair" c-doublepair) (:struct struct-pair) (dp (:struct struct-pair)))
(cffi:defcfun ("prodsumpair" c-prodsumpair) :double (spd (:struct struct-pair-double)))
(cffi:defcfun ("doublepairdouble" c-doublepairdouble)
    (:struct struct-pair-double) (pd (:struct struct-pair-double)))
(cffi:defcfun ("enumpair" c-enumpair) :int (n :int) (sp (:struct struct-pair)))

(defun sumpair (a b) (c-sumpair (list 'a a 'b b)))
(defun doublepair (a b)
  (let ((r (c-doublepair (list 'a a 'b b)))) (cons (getf r 'a) (getf r 'b))))
(defun prodsumpair (a b d) (c-prodsumpair (list 'pr (list 'a a 'b b) 'dbl d)))
(defun enumpair (n a b) (c-enumpair n (list 'a a 'b b)))

;;; === Test harness ===

(defvar *failures* 0)

(defmacro expect (form expected)
  `(let ((got ,form))
     (if (equalp got ,expected)
         (format t "  ok   ~s => ~s~%" ',form got)
         (progn (incf *failures*)
                (format t "  FAIL ~s => ~s (expected ~s)~%" ',form got ,expected)))))

(format t "~&== struct-by-value correctness ==~%")
(expect (sumpair 3 4) 7)
(expect (sumpair 100 -58) 42)
(expect (doublepair 5 6) '(10 . 12))
(expect (prodsumpair 3 4 2.5d0) 17.5d0)
(expect (enumpair 2 10 20) 50)

;;; Stress: on the unfixed backend this faults around iteration 4000 once the
;;; first GC reclaims the cif's argument-type descriptors.
(defparameter *iterations* 300000)

(format t "== stress: ~d mixed struct-by-value calls ==~%" *iterations*)
(block stress
  (handler-bind
      ((serious-condition
        (lambda (e)
          (incf *failures*)
          (format t "  CRASH: ~a~%" (type-of e))
          (return-from stress))))
    (dotimes (i *iterations*)
      (let ((s (sumpair 3 4)) (dp (doublepair 5 6)) (ps (prodsumpair 3 4 2.5d0)))
        (unless (and (eql s 7) (equal dp '(10 . 12)) (= ps 17.5d0))
          (incf *failures*)
          (format t "  WRONG result at iteration ~d: s=~a dp=~a ps=~a~%" i s dp ps)
          (return-from stress))))
    (format t "  ok   no crash, all results correct~%")))

;;; Hygiene: balanced alloc/free must not accumulate. Only measurable when the
;;; cffi-clasp registry fix (a) is loaded; the fli.cc fix (b) has no registry.
(let ((reg (find-symbol "*FOREIGN-ALLOCATIONS*" "CFFI-SYS")))
  (cond
    ((and reg (boundp reg))
     (format t "== registry hygiene (fix a) ==~%")
     (let ((before (hash-table-count (symbol-value reg))))
       (dotimes (i 100000)
         (cffi:with-foreign-object (p :int) (setf (cffi:mem-ref p :int) i)))
       (let ((delta (- (hash-table-count (symbol-value reg)) before)))
         (if (zerop delta)
             (format t "  ok   registry stable after 100k with-foreign-object pairs (delta=~d)~%" delta)
             (progn (incf *failures*)
                    (format t "  FAIL registry grew on balanced alloc/free (delta=~d)~%" delta))))
       ;; KNOWN UPSTREAM ISSUE (not the crash, not Clasp-specific): CFFI's
       ;; TRANSLATE-INTO-FOREIGN-MEMORY (structures.lisp) CONVERT-TO-FOREIGNs
       ;; each nested-struct slot into a temp buffer it never frees, so a
       ;; by-value arg with a nested struct leaks one alloc per call on every
       ;; backend. Reported here, non-fatal.
       (let ((b2 (hash-table-count (symbol-value reg))))
         (dotimes (i 2000) (prodsumpair 3 4 2.5d0))
         (format t "  note nested-struct arg leaks ~,2f alloc/call (upstream CFFI, structures.lisp)~%"
                 (/ (- (hash-table-count (symbol-value reg)) b2) 2000.0)))))
    (t
     (format t "== registry hygiene ==~%")
     (format t "  skip no cffi-sys registry (fix b / fli.cc keeps foreign-alloc permanent)~%"))))

(format t "~&~:[ALL TESTS PASSED~;~:*~d FAILURE(S)~]~%" (if (zerop *failures*) nil *failures*))
(core:quit (if (zerop *failures*) 0 1))
