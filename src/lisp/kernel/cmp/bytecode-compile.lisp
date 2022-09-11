#+(or)
(defpackage #:compile-to-vm
  (:use #:cl)
  (:shadow #:compile))

#-sbcl
(in-package #:cmp)

(setq *print-circle* t)

(defmacro logf (message &rest args)
  (declare (ignore message args))
  nil)
#+(or)
(progn
  (defvar *bclog* (progn
                    (format t "!~%!~%!   Opening /tmp/allcode.log - logging all bytecode compilation~%!~%!~%")
                    (open "/tmp/allcode.log" :direction :output :if-exists :supersede)))
  (defun log-function (cfunction compile-info bytecode)
    (format *bclog* "Name: ~s~%" (cmp:cfunction/name cfunction))
    (let ((*print-circle* t))
      (format *bclog* "Form: ~s~%" (car compile-info))
      (format *bclog* "Bytecode: ~s~%" bytecode)
      (finish-output *bclog*)))
  (defmacro logf (message &rest args)
    `(format *bclog* ,message ,@args)))



;;; FIXME: New package

(let ((rev-codes nil)
      (forms nil))
  (macrolet ((new-instr (name code &optional arguments long-arguments)
               `(progn
                  (push (list ,name ,code ,arguments ,long-arguments) rev-codes)
                  (let ((sym (intern (format nil "+~a+" ,(string-upcase name))))
                        (cd ,code))
                    (eval `(defconstant ,sym ,cd))
                    ))))
    (new-instr "ref" 0 '(1) '(2))
    (new-instr "const" 1 '((constant-arg 1)) '((constant-arg 2)))
    (new-instr "closure" 2 '(1) '(2))
    (new-instr "call" 3 '(1) '(2))
    (new-instr "call-receive-one" 4 '(1) '(2))
    (new-instr "call-receive-fixed" 5 '(1 1) '(2 2))
    (new-instr "bind" 6 '(1 1) '(2 2))
    (new-instr "set" 7 '(1) '(2))
    (new-instr "make-cell" 8)
    (new-instr "cell-ref" 9)
    (new-instr "cell-set" 10)
    (new-instr "make-closure" 11 '((constant-arg 1)) '((constant-arg 2)))
    (new-instr "make-uninitialized-closure" 12 '((constant-arg 1)) '((constant-arg 2)))
    (new-instr "initialize-closure" 13 '(1) '(2))
    (new-instr "return" 14)
    (new-instr "bind-required-args" 15 '(1) '(2))
    (new-instr "bind-optional-args" 16 '(1 1) '(2 2))
    (new-instr "listify-rest-args" 17 '(1) '(2))
    (new-instr "vaslistify-rest-args" 18 '(1))
    (new-instr "parse-key-args" 19 '(1 1 (keys-arg 1) 1) '(2 2 (keys-arg 2) 2))
    (new-instr "jump-8" 20 '((label-arg 1)))
    (new-instr "jump-16" 21 '((label-arg 2)))
    (new-instr "jump-24" 22 '((label-arg 3)))
    (new-instr "jump-if-8" 23 '((label-arg 1)))
    (new-instr "jump-if-16" 24 '((label-arg 2)))
    (new-instr "jump-if-24" 25 '((label-arg 3)))
    (new-instr "jump-if-supplied-8" 26 '(1 (label-arg 1)))
    (new-instr "jump-if-supplied-16" 27 '(1 (label-arg 2)))
    (new-instr "check-arg-count-LE" 28 '(1) '(2))
    (new-instr "check-arg-count-GE" 29 '(1) '(2))
    (new-instr "check-arg-count-EQ" 30 '(1) '(2))
    (new-instr "push-values" 31)
    (new-instr "append-values" 32)
    (new-instr "pop-values" 33)
    (new-instr "mv-call" 34)
    (new-instr "mv-call-receive-one" 35)
    (new-instr "mv-call-receive-fixed" 36 '(1) '(2))
    (new-instr "entry" 37 '(1))
    (new-instr "exit-8" 38 '((label-arg 1)))
    (new-instr "exit-16" 39 '((label-arg 2)))
    (new-instr "exit-24" 40 '((label-arg 3)))
    (new-instr "entry-close" 41)
    (new-instr "catch-8" 42)
    (new-instr "catch-16" 43)
    (new-instr "throw" 44)
    (new-instr "catch-close" 45)
    (new-instr "special-bind" 46 '((constant-arg 1)) '((constant-arg 2)))
    (new-instr "symbol-value" 47 '((constant-arg 1)) '((constant-arg 2)))
    (new-instr "symbol-value-set" 48 '((constant-arg 1)) '((constant-arg 2)))
    (new-instr "unbind" 49)
    (new-instr "progv" 50)
    (new-instr "fdefinition" 51 '((constant-arg 1)) '((constant-arg 2)))
    (new-instr "nil" 52)
    (new-instr "eq" 53)
    (new-instr "push" 54)
    (new-instr "pop" 55)
    (new-instr "long" 56)
    (defparameter *full-codes* (nreverse rev-codes))
    (defparameter *codes* (mapcar #'first (nreverse rev-codes)))
    (eval `(progn ,@forms))
    (defun decode-instr (code)
      code)
    ))

#+(or)
(macrolet ((defcodes (&rest names)
             `(progn
                ,@(let ((forms nil))
                    (do ((i 0 (1+ i))
                         (names names (cdr names)))
                        ((endp names) forms)
                      (push `(defconstant ,(first names) ,i) forms)))
                (defparameter *codes* '(,@names))
                (defun decode-instr (code)
                  code)
                (defun encode-instr (code)
                  (nth code '(,@names)))                )))
  (defcodes +ref+ +const+ +closure+
    +call+ +call-receive-one+ +call-receive-fixed+
    +bind+ +set+
    +make-cell+ +cell-ref+ +cell-set+
    +make-closure+ +make-uninitialized-closure+ +initialize-closure+
    +return+
    +bind-required-args+ +bind-optional-args+
    +listify-rest-args+ +vaslistify-rest-args+ +parse-key-args+
    +jump-8+ +jump-16+ +jump-24+
    +jump-if-8+ +jump-if-16+ +jump-if-24+
    +jump-if-supplied-8+ +jump-if-supplied-16+
    +check-arg-count<=+ +check-arg-count>=+ +check-arg-count=+
    +push-values+ +append-values+ +pop-values+
    +mv-call+ +mv-call-receive-one+ +mv-call-receive-fixed+
    +entry+
    +exit-8+ +exit-16+ +exit-24+
    +entry-close+
    +catch-8+ +catch-16+
    +throw+ +catch-close+
    +special-bind+ +symbol-value+ +symbol-value-set+ +unbind+
    +progv+
    +fdefinition+
    +nil+
    +eq+
    +push+ +pop+
    +long+))

;;;

(defun make-lexical-environment (parent &key (vars (cmp:lexenv/vars parent))
                                          (tags (cmp:lexenv/tags parent))
                                          (blocks (cmp:lexenv/blocks parent))
                                          (frame-end (cmp:lexenv/frame-end parent))
                                          (funs (cmp:lexenv/funs parent))
                                          (notinlines (cmp:lexenv/notinlines parent)))
  (cmp:lexenv/make vars tags blocks funs notinlines frame-end))
