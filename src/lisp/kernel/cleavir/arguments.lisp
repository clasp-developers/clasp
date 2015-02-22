(in-package :clasp-cleavir)


(defclass calling-convention ()
  ((%nargs :initarg :nargs :accessor calling-convention-nargs)
   (%register-args :initarg :register-args :accessor calling-convention-register-args)))



(defun compile-<=3-required-arguments (reqargs outputs cc)
;;  (cmp:compile-error-if-wrong-number-of-arguments (calling-convention-nargs cc) (car reqargs))
  (let ((fixed-args (calling-convention-register-args cc)))
    (do* ((cur-target (cdr reqargs) (cdr cur-target))
	  (cur-fixed-args fixed-args (cdr cur-fixed-args))
	  (cur-output outputs (cdr cur-output))
	  (target (car cur-output) (car cur-output))
	  (arg (car cur-fixed-args) (car cur-fixed-args)))
	 ((null cur-target))
      (let ((dest (translate-datum target)))
	(format t "compile-<=3-required-arguments store: ~a to ~a  target: ~a~%" arg dest target)
	(llvm-sys:create-store cmp:*irbuilder* arg dest nil)))))


(defun compile-lambda-list-code (lambda-list outputs calling-conv)
  (multiple-value-bind (reqargs optargs rest-var key-flag keyargs allow-other-keys)
      (core:process-lambda-list lambda-list 'core::function)
    (let ((req-opt-only (and (not rest-var)
                             (not key-flag)
                             (eql 0 (car keyargs))
                             (not allow-other-keys)))
          (num-req (car reqargs))
          (num-opt (car optargs)))
      (progn
	(format t "reqs: ~a~%" reqargs)
	(format t "opts: ~a~%" optargs)
	(format t "rest: ~a~%" rest-var)
	(format t "key-flag: ~a~%" key-flag)
	(format t "keys: ~a~%" keyargs)
	(format t "allow-other-keys: ~a~%" allow-other-keys))
      (cond
        ;; Special cases (foo) (foo x) (foo x y) (foo x y z)  - passed in registers
        ((and req-opt-only (<= num-req 3) (eql 0 num-opt) )
         (compile-<=3-required-arguments reqargs outputs calling-conv))
        ;; Test for
        ;; (x &optional y)
        ;; (x y &optional z)
        (t
         (compile-general-lambda-list-code lambda-list-handler old-env args new-env))))))
