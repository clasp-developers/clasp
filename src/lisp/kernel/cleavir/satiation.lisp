;;;; Satiate relevant Cleavir functions so that
;;;; startup and the first compile go a little faster.
;;;; (This file is thus essentially optional.)

(in-package #:clasp-cleavir)

#| cracauer disabled this 20200123


;;; Initialization stuff.
;;; Note that we'll include some abstract classes that are never actually instantiated.
;;; Just putting them in the call history is no problem, though.
(eval-when (:load-toplevel)
  (macrolet ((frob ()
               `(clos:satiate-initialization
                 ;; KLUDGE: We incorporate all the classes satiated in clos/satiation.lisp
                 ;; here so that the discriminating function still includes them etc.
                 'standard-generic-function
                 'standard-method
                 'standard-class
                 'structure-class
                 'clos:standard-reader-method
                 'clos:standard-writer-method
                 'clos:standard-direct-slot-definition
                 'clos:standard-effective-slot-definition
                 'clos:eql-specializer
                 'clos:method-combination
                 'clos:funcallable-standard-class)))
    (frob)))

;;; See explanation in clos/static-gfs/compiler-macros.lisp
(static-gfs::precompile-build-constructors)

;;; Functions go here (TODO)

;;; cleavir-compilation-policy
(eval-when (:load-toplevel)
  (clos:satiate #'policy:compute-policy
                '(cons clasp-global-environment))
  (clos:satiate #'policy:policy-qualities
                '(clasp-global-environment) '(null))
  (clos:satiate #'policy:normalize-optimize
                '(cons clasp-global-environment))
  (clos:satiate #'policy:compute-policy-quality
                #+(or)
                '((eql cleavir-kildall-type-inference:insert-type-checks) cons clasp-global-environment)
                '((eql core::insert-array-bounds-checks) cons clasp-global-environment)
                '((eql save-register-args) cons clasp-global-environment)
                '((eql do-type-inference) cons clasp-global-environment)
                '((eql do-dx-analysis) cons clasp-global-environment)))

;;; clasp-cleavir
(eval-when (:load-toplevel)
  (clos:satiate #'%default-int-type '(abi-x86-64))
  (clos:satiate #'%sadd.with-overflow '(llvm-sys:value llvm-sys:value abi-x86-64))
  (clos:satiate #'%ssub.with-overflow '(llvm-sys:value llvm-sys:value abi-x86-64))
  )
;  (clos:satiate #'cclasp-eval-with-env '(cons null)

|#
