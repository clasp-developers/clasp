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
                 'clos:funcallable-standard-class
                 ;; OK on to the real shit.
                 ,@(rest (clos:subclasses* (find-class 'cleavir-env::entry)))
                 'cleavir-env:lexical-variable-info
                 'cleavir-env:special-variable-info
                 'cleavir-env:constant-variable-info
                 'cleavir-env:symbol-macro-info
                 'cleavir-env:local-function-info
                 'cleavir-env:global-function-info
                 'cleavir-env:local-macro-info
                 'cleavir-env:special-operator-info
                 'cleavir-env:block-info
                 'cleavir-env:tag-info
                 'cleavir-env:optimize-info)))
    (frob)))

;;; See explanation in clos/static-gfs/compiler-macros.lisp
(static-gfs::precompile-build-constructors)

;;; Functions go here (TODO)

;;; cleavir-env
(eval-when (:load-toplevel)
  (clos:satiate #'cleavir-env:name
                '(cleavir-env:function) '(cleavir-env:block) '(cleavir-env:macro) '(cleavir-env:tag)
                '(cleavir-env:lexical-variable) '(cleavir-env:special-variable)
                '(cleavir-env:variable-ignore) '(cleavir-env:variable-type)
                '(cleavir-env:local-function-info) '(cleavir-env:global-function-info)
                '(cleavir-env:global-macro-info) '(cleavir-env:special-variable-info)
                '(cleavir-env:lexical-variable-info))
  (clos:satiate #'cleavir-env:policy '(cleavir-env:optimize-info))
  (clos:satiate #'cleavir-env:ast '(cleavir-env:local-function-info) '(cleavir-env:global-function-info))
  (clos:satiate #'cleavir-env:expander
                '(cleavir-env:local-macro-info) '(cleavir-env:global-macro-info) '(cleavir-env:macro))
  (clos:satiate #'cleavir-env:compiler-macro
                '(cleavir-env:global-function-info) '(cleavir-env:global-function-info))
  (clos:satiate #'cleavir-env:identity
                '(cleavir-env:local-function-info) '(cleavir-env:block-info) '(cleavir-env:tag-info)
                '(cleavir-env:lexical-variable-info) '(cleavir-env:lexical-variable)
                '(cleavir-env:function) '(cleavir-env:block) '(cleavir-env:tag))
  (clos:satiate #'cleavir-env:inline '(cleavir-env:local-function-info) '(cleavir-env:global-function-info))
  (clos:satiate #'cleavir-env:type
                '(cleavir-env:variable-type)
                '(cleavir-env:local-function-info) '(cleavir-env:lexical-variable-info)
                '(cleavir-env:global-function-info) '(cleavir-env:special-variable-info))
  (clos:satiate #'cleavir-env:ignore
                '(cleavir-env:variable-ignore)
                '(cleavir-env:lexical-variable-info)
                '(cleavir-env:local-function-info) '(cleavir-env:global-function-info))
  (macrolet ((for-entries (name &rest remaining-args)
               (let* ((entries (list* 'null 'clasp-global-environment
                                      (rest (clos:subclasses* (find-class 'cleavir-env::entry)))))
                      (tail (if remaining-args
                                (loop for things in remaining-args
                                      nconcing (mapcar (lambda (entry) `'(,entry ,@things)) entries))
                                (mapcar (lambda (entry) `'(,entry)) entries))))
                 `(clos:satiate #',name ,@tail))))
    (for-entries cleavir-env:block-info (symbol))
    ;; i include conses for (setf x) functions, but the specializer profile makes it irrelevant
    (for-entries cleavir-env:function-info (symbol) (cons))
    (for-entries cleavir-env:optimize-info)
    (for-entries cleavir-env:tag-info (symbol))
    (for-entries cleavir-env:variable-info (symbol))
    (for-entries cleavir-env:global-environment)
    (for-entries cleavir-env:declarations)
    (for-entries cleavir-env:compile-time)
    (for-entries cleavir-env:optimize-qualities)
    (for-entries cleavir-env:function-dynamic-extent
                 (cleavir-env:global-function-info) (cleavir-env:local-function-info))
    (for-entries cleavir-env:function-ignore
                 (cleavir-env:global-function-info) (cleavir-env:local-function-info))
    (for-entries cleavir-env:function-type
                 (cleavir-env:global-function-info) (cleavir-env:local-function-info))
    (for-entries cleavir-env:variable-dynamic-extent
                 (cleavir-env:lexical-variable-info) (cleavir-env:special-variable-info))
    (for-entries cleavir-env:variable-ignore
                 (cleavir-env:lexical-variable-info) (cleavir-env:special-variable-info))
    (for-entries cleavir-env:variable-type
                 (cleavir-env:lexical-variable-info) (cleavir-env:special-variable-info))
    (for-entries cleavir-env:add-block (symbol))
    ;;(for-entries cleavir-env:add-function-dynamic-extent (symbol) (cons))
    ;;(for-entries cleavir-env:add-function-ignore (symbol symbol) (cons symbol) (symbol null) (cons null))
    ;;(for-entries cleavir-env:add-function-type (symbol cons) (cons cons) (symbol symbol) (cons symbol))
    ;;add-inline, add-inline-expansion
    (for-entries cleavir-env:add-lexical-variable (symbol))
    (for-entries cleavir-env:add-local-function (symbol))
    (for-entries cleavir-env:add-local-macro (symbol core:closure))
    (for-entries cleavir-env:add-local-symbol-macro (symbol symbol cons)) ; not sure history is correct
    ;;add-optimize
    (for-entries cleavir-env:add-special-variable (symbol))
    (for-entries cleavir-env:add-tag (symbol))
    ;;add-variable-dynamic-extent
    (for-entries cleavir-env:add-variable-ignore (symbol symbol))
    (for-entries cleavir-env:add-variable-type (symbol symbol) (symbol cons)))
  (macrolet ((for-entries-after (name &rest preceding-args)
               (let* ((entries (list* 'null 'clasp-global-environment
                                      (rest (clos:subclasses* (find-class 'cleavir-env::entry)))))
                      (tail (if preceding-args
                                (loop for things in preceding-args
                                      nconcing (mapcar (lambda (entry) `'(,@things ,entry)) entries))
                                (mapcar (lambda (entry) `'(,entry)) entries))))
                 `(clos:satiate #',name ,@tail))))
    (for-entries-after cleavir-env:symbol-macro-expansion (symbol))
    (for-entries-after cleavir-env:macro-function (symbol)))
  (macrolet ((satiate-eval ()
               (let ((entries (list* 'null 'clasp-global-environment
                                     (rest (clos:subclasses* (find-class 'cleavir-env::entry))))))
                 `(clos:satiate #'cleavir-env:eval
                                ,@(loop for entry1 in entries
                                        nconc (loop for entry2 in entries
                                                    collect `'(cons ,entry1 ,entry2)))))))
    (satiate-eval)))

;;; cleavir-compilation-policy
(eval-when (:load-toplevel)
  (clos:satiate #'policy:compute-policy
                '(cons clasp-global-environment))
  (clos:satiate #'policy:policy-qualities
                '(clasp-global-environment) '(null)
                '(cleavir-env:lexical-variable))
  (clos:satiate #'policy:normalize-optimize
                '(cons clasp-global-environment))
  (clos:satiate #'policy:compute-policy-quality
                #+(or)
                '((eql cleavir-kildall-type-inference:insert-type-checks) cons clasp-global-environment)
                '((eql core::insert-array-bounds-checks) cons clasp-global-environment)
                '((eql save-register-args) cons clasp-global-environment)
                '((eql do-type-inference) cons clasp-global-environment)
                '((eql do-dx-analysis) cons clasp-global-environment)))

#+cst
(eval-when (:load-toplevel)
  (clos:satiate #'cst:raw '(cst:atom-cst) '(cst:cons-cst))
  (clos:satiate #'cst:source '(cst:atom-cst) '(cst:cons-cst))
  ;; SP NIL NIL
  (clos:satiate #'cst:cons '(cst:atom-cst cst:atom-cst))
  (clos:satiate #'cst:listify '(cst:atom-cst) '(cst:cons-cst))
  (clos:satiate #'cst:cstify '(null) '(cons))
  (clos:satiate #'cst:parse '(cst:parser))
  (clos:satiate #'cst:atom '(cst:atom-cst) '(cst:cons-cst))
  (clos:satiate #'cst:null '(cst:atom-cst) '(cst:atom-cst))
  (clos:satiate #'cst:consp '(cst:atom-cst) '(cst:cons-cst))
  (clos:satiate #'cst:rest '(cst:cons-cst))
  (clos:satiate #'cst:first '(cst:cons-cst))
  (clos:satiate #'cst:second '(cst:cons-cst))
  (clos:satiate #'cst:third '(cst:cons-cst))
  (clos:satiate #'cst:nth '(fixnum cst:cons-cst))
  (clos:satiate #'cst:nthrest '(fixnum cst:cons-cst))
  ;; SP NIL T T
  (clos:satiate #'cst:reconstruct
                '(clasp-64bit cons cst:atom-cst)
                '(clasp-64bit cons cst:cons-cst)
                '(clasp-64bit cons null)
                '(clasp-64bit cons cons)))

;;; clasp-cleavir
(eval-when (:load-toplevel)
  (clos:satiate #'%default-int-type '(abi-x86-64))
  (clos:satiate #'%sadd.with-overflow '(llvm-sys:value llvm-sys:value abi-x86-64))
  (clos:satiate #'%ssub.with-overflow '(llvm-sys:value llvm-sys:value abi-x86-64))
  )
;  (clos:satiate #'cclasp-eval-with-env '(cons null)

|#
