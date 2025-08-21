(in-package #:cmpref)

(defconstant +mask-arg+     #b011000)
(defconstant +constant-arg+ #b001000)
(defconstant +keys-arg+     #b011000)
(defconstant +label-arg+    #b010000)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun constant-arg (val)
    (logior +constant-arg+ val))

  (defun label-arg (val)
    (logior +label-arg+ val))

  (defun keys-arg (val)
    (logior +keys-arg+ val)))

(defun constant-arg-p (val)
  (= (logand +mask-arg+ val) +constant-arg+))

(defun label-arg-p (val)
  (= (logand +mask-arg+ val) +label-arg+))

(defun keys-arg-p (val)
  (= (logand +mask-arg+ val) +keys-arg+))

(defun unmask-arg (val) (logandc2 val +mask-arg+))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(constant-arg-p label-arg-p keys-arg-p unmask-arg
            decode-instr)))

(macrolet ((defops (&rest ops)
             (let (rev-fullcodes
                   rev-codes
                   rev-defconstants)
               (dolist (op ops)
                 (destructuring-bind (name code &optional argument-codes long-argument-codes)
                     op
                   (let ((arguments (mapcar (lambda (code) (eval code)) argument-codes))
                         (long-arguments (mapcar (lambda (code) (eval code)) long-argument-codes)))
                     (push (list name code arguments long-arguments) rev-fullcodes))
                   (push name rev-codes)
                   (let ((sym (intern (format nil "+~a+" (string-upcase name))))
                         (cd code))
                     (push `(defconstant ,sym ,cd) rev-defconstants))))
               `(progn
                  (defvar *full-codes* ',(reverse rev-fullcodes))
                  (defvar *codes* ',(reverse rev-codes))
                  ,@rev-defconstants))))
  (defops
    ("ref" 0 (1) (2))
    ("const" 1 ((constant-arg 1)) ((constant-arg 2)))
    ("closure" 2 (1) (2))
    ("call" 3 (1) (2))
    ("call-receive-one" 4 (1) (2))
    ("call-receive-fixed" 5 (1 1) (2 2))
    ("bind" 6 (1 1) (2 2))
    ("set" 7 (1) (2))
    ("make-cell" 8)
    ("cell-ref" 9)
    ("cell-set" 10)
    ("make-closure" 11 ((constant-arg 1)) ((constant-arg 2)))
    ("make-uninitialized-closure" 12 ((constant-arg 1)) ((constant-arg 2)))
    ("initialize-closure" 13 (1) (2))
    ("return" 14)
    ("bind-required-args" 15 (1) (2))
    ("bind-optional-args" 16 (1 1) (2 2))
    ("listify-rest-args" 17 (1) (2))
    ("vaslistify-rest-args" 18 (1))
    ("parse-key-args" 19 (1 1 (keys-arg 1)) (2 2 (keys-arg 2)))
    ("jump-8" 20 ((label-arg 1)))
    ("jump-16" 21 ((label-arg 2)))
    ("jump-24" 22 ((label-arg 3)))
    ("jump-if-8" 23 ((label-arg 1)))
    ("jump-if-16" 24 ((label-arg 2)))
    ("jump-if-24" 25 ((label-arg 3)))
    ("jump-if-supplied-8" 26 ((label-arg 1)))
    ("jump-if-supplied-16" 27 ((label-arg 2)))
    ("check-arg-count-LE" 28 (1) (2))
    ("check-arg-count-GE" 29 (1) (2))
    ("check-arg-count-EQ" 30 (1) (2))
    ("push-values" 31)
    ("append-values" 32)
    ("pop-values" 33)
    ("mv-call" 34)
    ("mv-call-receive-one" 35)
    ("mv-call-receive-fixed" 36 (1) (2))
    ("save-sp" 37 (1) (2))
    ("restore-sp" 38 (1) (2))
    ("entry" 39 (1) (2))
    ("exit-8" 40 ((label-arg 1)))
    ("exit-16" 41 ((label-arg 2)))
    ("exit-24" 42 ((label-arg 3)))
    ("entry-close" 43)
    ("catch-8" 44 ((label-arg 1)))
    ("catch-16" 45 ((label-arg 2)))
    ("throw" 46)
    ("catch-close" 47)
    ("special-bind" 48 ((constant-arg 1)) ((constant-arg 2)))
    ("symbol-value" 49 ((constant-arg 1)) ((constant-arg 2)))
    ("symbol-value-set" 50 ((constant-arg 1)) ((constant-arg 2)))
    ("unbind" 51)
    ("progv" 52 ((constant-arg 1)) ((constant-arg 2)))
    ("fdefinition" 53 ((constant-arg 1)) ((constant-arg 2)))
    ("nil" 54)
    ("eq" 55)
    ("push" 56)
    ("pop" 57)
    ("dup" 58)
    ("fdesignator" 59 ((constant-arg 1)) ((constant-arg 2)))
    ("called-fdefinition" 60 ((constant-arg 1)) ((constant-arg 2)))
    ("protect" 61 ((constant-arg 1)) ((constant-arg 2)))
    ("cleanup" 62)
    ("encell" 63 (1) (2))
    ("long" 255)))

;;; *full-codes* contains descriptions of the instructions in the following format:
;;; (name opcode (args...) (long-args...))
;;; the name is a string.
;;; the args and long args are encoded as a number of bytes from 1 to 3, LOGIOR'd
;;; with the constant, label, and keys code that is appropriate, if any.
;;; One of these "instruction description" lists is what DECODE-INSTR returns.

(defun decode-instr (opcode)
  (let ((res (member opcode cmpref:*full-codes* :key #'second)))
    (if res
        (first res)
        nil)))

;;; load time values machine

(defstruct ltv-info type c++-type suffix gcroots)

(defparameter *ltv-info* (make-hash-table :test #'equal))

(defun set-ltv-info (symbol c++-type suffix &optional gcroots)
  (setf (gethash symbol *ltv-info*) (make-ltv-info :type symbol :c++-type c++-type :suffix suffix :gcroots gcroots)))

(eval-when (:load-toplevel :execute)
  (set-ltv-info :i8 "char" "char")
  (set-ltv-info :size_t "size_t" "size_t")
  (set-ltv-info :t* "T_O*" "object" t)
  (set-ltv-info :i8* "string" "string")
  (set-ltv-info :short-float "short_float_t" "binary16")
  (set-ltv-info :single-float "float" "float")
  (set-ltv-info :double-float "double" "double")
  (set-ltv-info :binary80 "long_float_t" "binary80")
  (set-ltv-info :binary128 "long_float_t" "binary128")
  (set-ltv-info :uintptr_t "uintptr_t" "size_t")
  (set-ltv-info :bignum "T_O*" "bignum")
  (set-ltv-info :unknown "UNKNOWN" "UNKNOWN")
  )
