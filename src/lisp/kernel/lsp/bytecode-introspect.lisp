(in-package #:core)

(defun bytecode-next-arg (argspec bytecode opip ip nbytes)
  (cond
    ((cmpref::constant-arg-p argspec)
     (cons :constant (cmpref::bc-unsigned bytecode ip nbytes)))
    ((cmpref::label-arg-p argspec)
     (cons :label (+ opip (cmpref::bc-signed bytecode ip nbytes))))
    ((cmpref::keys-arg-p argspec)
     (cons :keys (cmpref::bc-unsigned bytecode ip nbytes)))
    (t (cons :operand (cmpref::bc-unsigned bytecode ip nbytes)))))

;;; Compute a list of annotations that start at the given IP.
;;; Return the list, and the index of the next annotation.
(defun new-annotations (annotations index ip)
  (values
   (loop with len = (length annotations)
         while (< index len)
         while (<= (core:bytecode-debug-info/start (aref annotations index)) ip)
         when (= (core:bytecode-debug-info/start (aref annotations index)) ip)
           collect (aref annotations index)
         do (incf index))
   index))

(defmacro do-instructions ((mnemonic args opip ip
                            &optional (annots (gensym "ANNOTATIONS")))
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
                                collect (bytecode-next-arg argspec ,bsym ,opip ,ip
                                                           nbytes)
                                do (incf ,ip nbytes))))
                    (declare (ignorable ,args ,ip))
                    (setf (values ,annots ,next-annotation-index)
                          (new-annotations ,gannotations
                                           ,next-annotation-index ,ip))
                    ,@body
                    (setf ,longp nil)))
           until (>= ,ip ,gend))))

(defmacro do-module-instructions ((mnemonic args opip ip
                                   &optional (annots (gensym "ANNOTATIONS")))
                                  (module)
                                  &body body)
  (let ((gmodule (gensym "MODULE")))
    `(let ((,gmodule ,module))
       (do-instructions (,mnemonic ,args ,opip ,ip ,annots)
         ((core:bytecode-module/bytecode ,gmodule)
          :annotations (core:bytecode-module/debug-info ,gmodule))
         ,@body))))
