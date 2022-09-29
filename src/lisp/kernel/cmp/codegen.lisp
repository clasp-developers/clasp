(in-package #:cmp)

(defun potentially-save-module ()
  (when *save-module-for-disassemble*
    (setq *saved-module-from-clasp-jit*
          (with-output-to-string (*standard-output*)
            (llvm-sys:dump-module *the-module* *standard-output*)))))

;;; Return true if the symbol should be treated as a special operator
;;; Special operators that are handled as macros are exempt
(defun treat-as-special-operator-p (sym)
  (and (symbolp sym)
       ;;; perhaps the test (symbolp sym) should be done in the callers
       ;;; done here, since special-operator-p should type-error on a non-symbol
       ;;; and bclasp is calling treat-as-special-operator-p on forms too
       (cond
         ((eq sym 'cl:unwind-protect) nil)     ;; handled with macro
         ((eq sym 'cl:catch) nil)              ;; handled with macro
         ((eq sym 'cl:throw) nil)              ;; handled with macro
         ((eq sym 'cl:progv) nil)              ;; handled with macro
         ((eq sym 'core:debug-message) t)      ;; special operator
         ((eq sym 'core:debug-break) t)      ;; special operator
         ((eq sym 'core:multiple-value-foreign-call) t) ;; Call intrinsic functions
         ((eq sym 'core:foreign-call-pointer) t) ;; Call function pointers
         ((eq sym 'core:foreign-call) t)         ;; Call foreign function
         ((eq sym 'core:bind-vaslist) t)         ;; bind-vaslist
         ((eq sym 'core::vector-length) t)
         ((eq sym 'core::%array-dimension) t)
         ((eq sym 'core::fence) t)

         ((eq sym 'cleavir-primop:funcall) t)
         ((eq sym 'cleavir-primop:unreachable) t)
         ((eq sym 'cleavir-primop:case) t)

;;; batch 7
         #-bytecodelike((eq sym 'core::header-stamp) t)
         #-bytecodelike((eq sym 'core::derivable-stamp) t)
         #-bytecodelike((eq sym 'core::wrapped-stamp) t)
         #-bytecodelike((eq sym 'core::rack-stamp) t)

;;; batch 6
         #-bytecodelike((eq sym 'core::header-stamp-case) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; batch 5
         ;;; Still need to run these
         #-bytecodelike((eq sym 'cleavir-primop:car) t)
         #-bytecodelike((eq sym 'cleavir-primop:cdr) t)
         #-bytecodelike((eq sym 'core::car-atomic) t)
         #-bytecodelike((eq sym 'core::cdr-atomic) t)
         #-bytecodelike((eq sym 'core::rplaca-atomic) t)
         #-bytecodelike((eq sym 'core::rplacd-atomic) t)
         #-bytecodelike((eq sym 'core::cas-car) t)
         #-bytecodelike((eq sym 'core::cas-cdr) t)

;;; batch 4
         #-bytecodelike((eq sym 'core:bind-vaslist) t)         ;; bind-vaslist
         #-bytecodelike((eq sym 'core:vaslist-pop) t)
         #-bytecodelike((eq sym 'core:vaslist-length) t)
;;; batch 3
         #-bytecodelike((eq sym 'core::local-block) t)
         #-bytecodelike((eq sym 'core::local-tagbody) t)
;;; batch 2
         #-bytecodelike((eq sym 'core:instance-ref) t)
         #-bytecodelike((eq sym 'core:instance-set) t)
         #-bytecodelike((eq sym 'core::instance-cas) t)
         #-bytecodelike((eq sym 'core:instance-rack) t)
         #-bytecodelike((eq sym 'core:instance-rack-set) t)
         #-bytecodelike((eq sym 'core:rack-ref) t)
         #-bytecodelike((eq sym 'core:rack-set) t)
;;; batch 1
         #-bytecodelike((eq sym 'core::atomic-rack-read) t)
         #-bytecodelike((eq sym 'core::atomic-rack-write) t)
         #-bytecodelike((eq sym 'core::cas-rack) t)
         ((eq sym 'core:defcallback) t)
         (t (special-operator-p sym)))))

(export 'treat-as-special-operator-p)
