(defpackage #:clasp-bytecode-tablegen
  (:use #:cl)
  (:export #:compute-control-flow-table)
  (:export #:function-entry #:function-entry-start #:function-entry-end
           #:function-entry-bcfun #:function-entry-extra)
  (:export #:block-entry-start #:block-entry-end #:block-entry-function
           #:block-entry-successors #:block-entry-predecessors
           #:block-entry-extra))

(in-package #:clasp-bytecode-tablegen)

(defun bcfun/entry (bcfun)
  (core:global-bytecode-simple-fun/entry-pc-n bcfun))

(defstruct (function-entry
            (:constructor make-function-entry
              (bcfun &aux (start (bcfun/entry bcfun)))))
  start end bcfun extra)

(defstruct (block-entry (:constructor make-block-entry (start)))
  start end (successors nil) (predecessors nil) function extra)

(defun next-arg (argspec bytecode opip ip nbytes)
  (cond
    ((cmpref::constant-arg-p argspec)
     (cons :constant (cmpref::bc-unsigned bytecode ip nbytes)))
    ((cmpref::label-arg-p argspec)
     (cons :label (+ opip (cmpref::bc-signed bytecode ip nbytes))))
    ((cmpref::keys-arg-p argspec)
     (cons :keys (cmpref::bc-unsigned bytecode ip nbytes)))
    (t (cons :operand (cmpref::bc-unsigned bytecode ip nbytes)))))

(defmacro do-instructions ((mnemonic args opip ip)
                           (bytecode &key (start 0) end)
                           &body body)
  (let ((bsym (gensym "BYTECODE"))
        (gend (gensym "END"))
        (longp (gensym "LONGP"))
        (op (gensym "OP")))
    `(loop with ,bsym = ,bytecode
           with ,ip = ,start
           with ,longp = nil
           with ,gend = ,(or end `(+ ,ip (length ,bsym)))
           for ,op = (cmpref::decode-instr (aref ,bsym ,ip))
           do (let ((,opip ,ip))
                (incf ,ip)
                (let ((,args
                        (loop for argspec
                                in (if ,longp (fourth ,op) (third ,op))
                              for nbytes = (logandc2 argspec
                                                     cmpref::+mask-arg+)
                              collect (next-arg argspec ,bsym ,opip ,ip
                                                nbytes)
                              do (incf ,ip nbytes)))
                      (,mnemonic
                        (intern (string-upcase (first ,op)) "KEYWORD")))
                  (declare (ignorable ,args ,ip ,mnemonic))
                  ,@body
                  (setf ,longp (eq ,mnemonic :long))))
           until (>= ,ip ,gend))))

(defun compute-block-starts (bytecode &rest entry-points)
  (assert (> (length entry-points) 0))
  (let ((block-starts ()))
    (do-instructions (mnemonic args opip ip) (bytecode)
      (when (eql opip (first entry-points))
        (pop entry-points)
        (pushnew opip block-starts))
      (dolist (arg args)
        (when (eq (car arg) :label)
          (pushnew (cdr arg) block-starts)))
      (when (member mnemonic
                    '(:push-values :append-values :pop-values
                      :entry :catch-8 :catch-16
                      :exit-8 :exit-16 :exit-24
                      :special-bind :progv
                      :mv-call :mv-call-receive-one :mv-call-receive-fixed
                      :entry-close :catch-close :unbind
                      :jump-8 :jump-16 :jump-24
                      :jump-if-8 :jump-if-16 :jump-if-24
                      :jump-if-supplied-8 :jump-if-supplied-16))
        (pushnew ip block-starts)))
    (sort block-starts #'<)))

(defun compute-control-flow-table (bytecode &rest bcfuns)
  (let* ((entry-points (mapcar #'bcfun/entry bcfuns))
         (block-starts (apply #'compute-block-starts bytecode entry-points))
         (all-blocks (mapcar #'make-block-entry block-starts))
         (blocks all-blocks)
         (block (pop blocks))
         (all-functions (mapcar #'make-function-entry bcfuns))
         (functions all-functions)
         (function (pop functions)))
    (assert (eql (function-entry-start function) 0))
    (setf (block-entry-function block) function)
    (do-instructions (mnemonic args opip ip) (bytecode)
      (when (and functions (eql ip (function-entry-start (first functions))))
        ;; Starting a new function.
        (setf (function-entry-end function) ip
              function (pop functions)))
      (when (and blocks (eql ip (block-entry-start (first blocks))))
        ;; We are at a terminator.
        ;; Set successors and predecessors based on what mnemonic we're at.
        (case mnemonic
          ((:jump-8 :jump-16 :jump-24
                    :exit-8 :exit-16 :exit-24)
           (let ((next (find (cdr (first args)) all-blocks
                             :key #'block-entry-start)))
             (assert next)
             (setf (block-entry-successors block) (list next))
             (push block (block-entry-predecessors next))))
          ((:jump-if-8 :jump-if-16 :jump-if-24)
           (let ((branch (find (cdr (first args)) all-blocks
                               :key #'block-entry-start))
                 (next (first blocks)))
             (assert branch)
             (setf (block-entry-successors block) (list branch next))
             (push block (block-entry-predecessors branch))
             (push block (block-entry-predecessors next))))
          ((:jump-if-supplied-8 :jump-if-supplied-16)
           ;; same as jump-if, but with second argument
           (let ((branch (find (cdr (second args)) all-blocks
                               :key #'block-entry-start))
                 (next (first blocks)))
             (assert branch)
             (setf (block-entry-successors block) (list branch next))
             (push block (block-entry-predecessors branch))
             (push block (block-entry-predecessors next))))
          ((:return)
           (setf (block-entry-successors block) nil))
          (otherwise ; fallthrough
           (let ((next (first blocks)))
             (setf (block-entry-successors block) (list next))
             (push block (block-entry-predecessors next)))))
        (setf (block-entry-end block) ip
              block (pop blocks)
              (block-entry-function block) function)))
    (setf (block-entry-end block) (length bytecode))
    (values all-functions all-blocks)))

#|
(defgeneric compute-new-stack (mnemonic args stack))

(defun drop (n stack)
  (assert (loop repeat n
                for pair on stack
                always (and pair (eql (car pair) :value))))
  (nthcdr n stack))

(defmethod compute-new-stack ((mnemonic (eql :ref)) args stack)
  (declare (ignore args))
  (cons :value stack))
(defmethod compute-new-stack ((mnemonic (eql :const)) args stack)
  (declare (ignore args))
  (cons :value stack))
(defmethod compute-new-stack ((mnemonic (eql :closure)) args stack)
  (declare (ignore args))
  (cons :value stack))
(defmethod compute-new-stack ((mnemonic (eql :call)) args stack)
  (destructuring-bind (nargs) args
    (drop (1+ nargs) stack)))
(defmethod compute-new-stack ((mnemonic (eql :call-receive-one)) args stack)
  (destructuring-bind (nargs) args
    (cons :value (drop (1+ nargs) stack))))
(defmethod compute-new-stack ((mnemonic (eql :call-receive-fixed)) args stack)
  (destructuring-bind (nargs receiving) args
    (append (make-list receiving :initial-element :value)
            (drop (1+ nargs) stack))))
(defmethod compute-new-stack ((mnemonic (eql :bind)) args stack)
  (destructuring-bind (count offset) args
    (declare (ignore offset))
    (drop count stack)))
(defmethod compute-new-stack ((mnemonic (eql :set)) args stack)
  (declare (ignore args))
  (drop 1 stack))
(defmethod compute-new-stack ((mnemonic (eql :make-cell)) args stack)
  (declare (ignore args))
  (cons :cell (drop 1 stack)))
(defmethod compute-new-stack ((mnemonic (eql :cell-ref)) args stack)
  (declare (ignore args))
  (assert (and stack (eql (car stack) :cell)))
  (cons :value (cdr stack)))
(defmethod compute-new-stack ((mnemonic (eql :cell-set)) args stack)
  (declare (ignore args))
  (assert (and stack (eql (car stack) :cell)))
  (cons :value (drop 1 (cdr stack))))
(defmethod compute-new-stack ((mnemonic (eql :make-closure)) args stack)
  ...)
;; make-uninitialized-closure
;; initialize-closure
(defmethod compute-new-stack ((mnemonic (eql :return)) args stack)
  (declare (ignore args stack))
  ())
(defmethod compute-new-stack ((mnemonic (eql :bind-required-args)) args stack))

(defun compute-stackmap-table (bytecode)
  (let ((stack nil)
        (stackmaps nil))
    (do-instructions (mnemonic args opip ip) (bytecode)
      (push stack stackmaps)
      
      |#
