(in-package #:cmp)

(defmethod eclector.readtable:syntax-type  ((readtable cl:readtable) char)
  (core:syntax-type readtable char))

(defmethod eclector.readtable:get-macro-character ((readtable cl:readtable) char)
  (cl:get-macro-character char readtable))

(defmethod eclector.readtable:set-macro-character
    ((readtable cl:readtable) char function &optional non-terminating-p)
  (cl:set-macro-character char function non-terminating-p readtable))
 
(defmethod eclector.readtable:get-dispatch-macro-character ((readtable cl:readtable) disp sub)
  (cl:get-dispatch-macro-character disp sub readtable))
 
(defmethod eclector.readtable:set-dispatch-macro-character
    ((readtable cl:readtable) disp sub function)
  (cl:set-dispatch-macro-character disp sub function readtable))
 
(defmethod eclector.readtable:copy-readtable ((readtable cl:readtable))
  (cl:copy-readtable readtable))

(defmethod eclector.readtable:copy-readtable-into ((from cl:readtable) (to cl:readtable))
  (cl:copy-readtable from to))
 
(defmethod eclector.readtable:make-dispatch-macro-character
    ((readtable cl:readtable) char &optional non-terminating-p)
  (cl:make-dispatch-macro-character char non-terminating-p readtable))

(defmethod eclector.readtable:readtable-case (readtable)
  (error 'type-error :datum readtable :EXPECTED-TYPE 'cl:readtable))

(defmethod eclector.readtable:readtable-case ((readtable cl:readtable))
  (cl:readtable-case readtable))
 
(defmethod (setf eclector.readtable:readtable-case) (mode (readtable cl:readtable))
  (setf (cl:readtable-case readtable) mode))

(defmethod (setf eclector.readtable:readtable-case) (mode readtable)
  (declare (ignore mode))
  (error 'type-error :datum readtable :EXPECTED-TYPE 'cl:readtable))
 
(defmethod eclector.readtable:readtablep ((object cl:readtable)) t)

(defvar core:*read-hook*)
(defvar core:*read-preserving-whitespace-hook*)

;;; to avoid eclector.parse-result::*stack* being unbound, when *client* is bound to a parse-result-client
;;; Not sure whether this a a fortunate design in eclector

(defclass clasp-non-cst-elector-client (cmp:clasp-eclector-client-mixin) ())
(defvar *clasp-normal-eclector-client* (make-instance 'clasp-non-cst-elector-client))

(defclass clasp-tracking-elector-client (cmp:clasp-eclector-client-mixin eclector.parse-result:parse-result-client) ())

(defmethod eclector.base:source-position
    ((client clasp-tracking-elector-client) stream)
  (cmp:compile-file-source-pos-info stream))

(defmethod eclector.parse-result:make-expression-result
    ((client clasp-tracking-elector-client) result children source)
  (declare (ignore children))
  (when cmp:*source-locations*
    (setf (gethash result cmp:*source-locations*) (car source)))
  result)

(defmethod eclector.reader:state-value
    ((client cmp:clasp-eclector-client-mixin) (aspect (eql 'cl:*readtable*)))
  cl:*readtable*)

(defmethod eclector.reader:call-with-state-value
    ((client cmp:clasp-eclector-client-mixin) thunk (aspect (eql 'cl:*readtable*)) value)
  (let ((cl:*readtable* value))
    (funcall thunk)))

(defun read-with-eclector (&optional (input-stream *standard-input*)
                                     (eof-error-p t)
                                     (eof-value nil)
                                     (recursive-p nil))
  (let ((eclector.reader:*client* *clasp-normal-eclector-client*))
    (eclector.reader:read input-stream eof-error-p eof-value recursive-p)))

(defun read-preserving-whitespace-with-eclector
    (&optional (input-stream *standard-input*)
               (eof-error-p t)
               (eof-value nil)
               (recursive-p nil))
  (let ((eclector.reader:*client* *clasp-normal-eclector-client*))
    (eclector.reader:read-preserving-whitespace input-stream eof-error-p
                                                eof-value recursive-p)))

(defun cl:read-from-string (string
                            &optional (eof-error-p t) eof-value
                            &key (start 0) (end (length string))
                              preserve-whitespace)
  (let ((eclector.reader:*client* *clasp-normal-eclector-client*))
    (eclector.reader:read-from-string string eof-error-p eof-value
                                      :start start :end end
                                      :preserve-whitespace preserve-whitespace)))

;;; Fixed in https://github.com/s-expressionists/Eclector/commit/19d2d903bb04e3e59ff0557051e134e8ee6195c7
(defun cl:read-delimited-list (char &optional (input-stream *standard-input*) recursive-p)
  (let ((eclector.reader:*client* *clasp-normal-eclector-client*))
    (eclector.reader:read-delimited-list char input-stream recursive-p)))

(defun core::set-eclector-reader-readmacros (readtable)
  (eclector.reader::set-standard-macro-characters readtable)
  (eclector.reader::set-standard-dispatch-macro-characters readtable)
  (cl:set-dispatch-macro-character #\# #\A 'core:sharp-a-reader readtable)
  (cl:set-dispatch-macro-character #\# #\D 'core::do-read-dense-specialized-array readtable)
  (cl:set-dispatch-macro-character #\# #\I 'core::read-cxx-object readtable))

(defun init-clasp-as-eclector-reader ()
  (core::set-eclector-reader-readmacros cl:*readtable*)
  (core::set-eclector-reader-readmacros (symbol-value 'core:+standard-readtable+))
  ;;; also change read 
  ;;; read-from-string is overwritten above
  (setq core:*read-hook* 'read-with-eclector)
  (setq core:*read-preserving-whitespace-hook* 'read-preserving-whitespace-with-eclector))

(init-clasp-as-eclector-reader)

(defun patch-object (client value-old seen-objects)
  (multiple-value-bind (state object*)
      (eclector.reader:labeled-object-state client value-old)
    (case state
      ((nil) ; normal object
       (eclector.reader:fixup client value-old seen-objects)
       value-old)
      ((:final :final/circular) object*) ; fully resolved circular reference
      (otherwise value-old)))) ; unresolved reference - leave for later

(defmethod eclector.reader:fixup (client (object core:cxx-object) seen-objects)
  (let ((patcher (core:make-record-patcher (lambda (object)
                                             (patch-object client object seen-objects)))))
    (core:patch-object object patcher)))
