(defpackage #:clasp-bytecode-tablegen
  (:use #:cl)
  (:export #:compute-control-flow-table)
  (:export #:function-entry #:function-entry-start #:function-entry-end
           #:function-entry-bcfun #:function-entry-extra)
  (:export #:block-entry-start #:block-entry-end #:block-entry-function
           #:block-entry-successors #:block-entry-predecessors
           #:block-entry-name #:block-entry-receiving #:block-entry-extra))

(in-package #:clasp-bytecode-tablegen)

(defun bcfun/entry (bcfun)
  (core:global-bytecode-simple-fun/entry-pc-n bcfun))

(defstruct (function-entry
            (:constructor make-function-entry
              (bcfun &aux (start (bcfun/entry bcfun)))))
  start end bcfun extra)

(defstruct (block-entry (:constructor make-block-entry (start)))
  start end (successors nil) (predecessors nil) function
  name (receiving 0) extra)

(defun next-arg (argspec bytecode opip ip nbytes)
  (cond
    ((cmpref::constant-arg-p argspec)
     (cons :constant (cmpref::bc-unsigned bytecode ip nbytes)))
    ((cmpref::label-arg-p argspec)
     (cons :label (+ opip (cmpref::bc-signed bytecode ip nbytes))))
    ((cmpref::keys-arg-p argspec)
     (cons :keys (cmpref::bc-unsigned bytecode ip nbytes)))
    (t (cons :operand (cmpref::bc-unsigned bytecode ip nbytes)))))

(defun new-annotations (annotations index ip)
  ;; Compute a list of annotations that start at the given IP.
  ;; Return the list, and the index of the next annotation.
  (values
   (loop with len = (length annotations)
         while (< index len)
         while (<= (core:bytecode-debug-info/start (aref annotations index)) ip)
         when (= (core:bytecode-debug-info/start (aref annotations index)) ip)
           collect (aref annotations index)
         do (incf index))
   index))

(defmacro do-instructions ((mnemonic args opip ip
                            &optional (annots (gensym "ANNOTATIONS") annotsp))
                           (bytecode &key (start 0) end annotations)
                           &body body)
  (let ((bsym (gensym "BYTECODE"))
        (gend (gensym "END"))
        (longp (gensym "LONGP"))
        (gannotations (gensym "ANNOTATIONS"))
        (next-annotation-index (gensym "NEXT-ANNOTATION-INDEX"))
        (op (gensym "OP")))
    `(loop with ,bsym = ,bytecode
           with ,ip = ,start
           with ,longp = nil
           with ,gend = ,(or end `(+ ,ip (length ,bsym)))
           with ,gannotations = ,annotations
           with ,next-annotation-index = 0
           with ,annots = nil
           for ,op = (cmpref::decode-instr (aref ,bsym ,ip))
           for ,mnemonic = (intern (string-upcase (first ,op)) "KEYWORD")
           if (eql ,mnemonic :long)
             do (setf ,longp t)
           else
             do (let ((,opip ,ip))
                  (incf ,ip)
                  (let ((,args
                          (loop for argspec
                                  in (if ,longp (fourth ,op) (third ,op))
                                for nbytes = (logandc2 argspec
                                                       cmpref::+mask-arg+)
                                collect (next-arg argspec ,bsym ,opip ,ip
                                                  nbytes)
                                do (incf ,ip nbytes))))
                    (declare (ignorable ,args ,ip))
                    (setf (values ,annots ,next-annotation-index)
                          (new-annotations ,gannotations
                                           ,next-annotation-index ,ip))
                    ,@body
                    (setf ,longp nil)))
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

(defun compute-control-flow-table (bytecode annotations &rest bcfuns)
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
    (do-instructions (mnemonic args opip ip annots)
        (bytecode :annotations annotations)
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
        ;; Move along
        (setf (block-entry-end block) ip
              block (pop blocks)
              (block-entry-function block) function)
        ;; If there's an annotation for the new block, note it.
        (let ((annot (find-if (lambda (a) (typep a 'core:bytecode-debug-block))
                              annots)))
          (when annot
            (setf (block-entry-name block) (core:bytecode-debug-block/name annot)
                  (block-entry-receiving block) (core:bytecode-debug-block/receiving annot))))))
    (setf (block-entry-end block) (length bytecode))
    (values all-functions all-blocks)))
