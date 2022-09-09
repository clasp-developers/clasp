#+(or)
(defpackage #:compile-to-vm
  (:use #:cl)
  (:shadow #:compile))

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
(macrolet ((defcodes (&rest names)
             `(progn
                ,@(let ((forms nil))
                    (do ((i 0 (1+ i))
                         (names names (cdr names)))
                        ((endp names) forms)
                      (push `(defconstant ,(first names) ,i) forms)))
                (defparameter *codes* '(,@names))
                #-clasp ; collides with core:decode, and we don't need it.
                (defun decode (code)
                  (nth code '(,@names))))))
  (defcodes +ref+ +const+ +closure+
    +call+ +call-receive-one+ +call-receive-fixed+
    +bind+ +set+
    +make-cell+ +cell-ref+ +cell-set+
    +make-closure+ +make-uninitialized-closure+ +initialize-closure+
    +return+
    +bind-required-args+ +bind-optional-args+
    +listify-rest-args+ +parse-key-args+
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
                                          (funs (cmp:lexenv/funs parent)))
  (cmp:lexenv/make vars tags blocks funs frame-end))
