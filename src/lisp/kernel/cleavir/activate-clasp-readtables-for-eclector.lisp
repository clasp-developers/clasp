(in-package :eclector.readtable)

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


;;; to avoid that cl:*readtable* and eclector.readtable:*readtable* get out of sync
;;; to avoid eclector.parse-result::*stack* being unbound, when *client* is bound to a parse-result-client
;;; Not sure whether this a a fortunate design in eclector

(defclass clasp-non-cst-elector-client (clasp-cleavir::clasp-eclector-client-mixin) ())
(defvar *clasp-normal-eclector-client* (make-instance 'clasp-non-cst-elector-client))

;;; From eclector macro functions:
;;; So we need a way for readers for lists and vectors to explicitly
;;; allow for backquote and comma, whereas BY DEFAULT, they should not
;;; be allowed.  We solve this by introducing two variables:
;;; *BACKQUOTE-ALLOWED-P* and *BACKQUOTE-IN-SUBFORMS-ALLOWED-P*.
;;; Initially the two are TRUE.  Whenever READ is called, it binds the
;;; variable *BACKQUOTE-ALLOWED-P* to the value of
;;; *BACKQUOTE-IN-SUBFORMS-ALLOWED-P*, and it binds
;;; *BACKQUOTE-IN-SUBFORMS-ALLOWED-P* to FALSE.  If no special action
;;; is taken, when READ is called recursively from a reader macro,
;;; the value of *BACKQUOTE-ALLOWED-P* will be FALSE.

(defun read-with-readtable-synced (&optional
                                      (input-stream *standard-input*)
                                      (eof-error-p t)
                                      (eof-value nil)
                                      (recursive-p nil))
  (let ((eclector.readtable:*readtable* cl:*readtable*)
        (eclector.reader:*client* *clasp-normal-eclector-client*)
        #+(or)(eclector.reader::*backquote-in-subforms-allowed-p* t))
    (eclector.reader:read input-stream eof-error-p eof-value recursive-p)))

;;; to avoid cl:*readtable* and eclector.readtable:*readtable* get out of sync
(defun read-preserving-whitespace-with-readtable-synced (&optional
                                                           (input-stream *standard-input*)
                                                           (eof-error-p t)
                                                           (eof-value nil)
                                                           (recursive-p nil))
  (let ((eclector.readtable:*readtable* cl:*readtable*)
        (eclector.reader:*client* *clasp-normal-eclector-client*)
        #+(or)(eclector.reader::*backquote-in-subforms-allowed-p* t))
    (eclector.reader:read-preserving-whitespace input-stream eof-error-p eof-value recursive-p)))

;;; need also sync in clasp-cleavir::cclasp-loop-read-and-compile-file-forms

(defun cl:read-from-string (string
                            &optional (eof-error-p t) eof-value
                            &key (start 0) (end (length string))
                              preserve-whitespace)
  (let ((eclector.readtable:*readtable* cl:*readtable*)
        (eclector.reader:*client* *clasp-normal-eclector-client*))
    (eclector.reader:read-from-string string eof-error-p eof-value
                                      :start start :end end :preserve-whitespace preserve-whitespace)))

;;; Fixed in https://github.com/s-expressionists/Eclector/commit/19d2d903bb04e3e59ff0557051e134e8ee6195c7
(defun cl:read-delimited-list (char &optional (input-stream *standard-input*) recursive-p)
  (let ((eclector.readtable:*readtable* cl:*readtable*)
        (eclector.reader:*client* *clasp-normal-eclector-client*))
    (eclector.reader:read-delimited-list char input-stream recursive-p)))

(defun core::set-eclector-reader-readmacros (readtable)
  (eclector.reader::set-standard-macro-characters readtable)
  (eclector.reader::set-standard-dispatch-macro-characters readtable)
  (cl:set-dispatch-macro-character #\# #\a 'core:sharp-a-reader readtable)
  (cl:set-dispatch-macro-character #\# #\A 'core:sharp-a-reader readtable)
  (cl:set-dispatch-macro-character #\# #\I 'core::read-cxx-object readtable)
  ;;; see issue https://github.com/s-expressionists/Eclector/issues/59
  ;;; sharpsign-single-quote/relaxed will be exported, but isn't yet
  (cl:set-dispatch-macro-character #\# #\' (if (fboundp 'eclector.reader::sharpsign-single-quote/relaxed)
                                               'eclector.reader::sharpsign-single-quote/relaxed
                                               'eclector.reader::sharpsign-single-quote)
                                               readtable))

(defun init-clasp-as-eclector-reader ()
  (setq eclector.readtable:*readtable* cl:*readtable*)
  (core::set-eclector-reader-readmacros cl:*readtable*)
  (core::set-eclector-reader-readmacros (symbol-value 'core:+standard-readtable+))
  ;;; also change read 
  (setq core:*read-hook* 'read-with-readtable-synced)
  (setq core:*read-preserving-whitespace-hook* 'read-preserving-whitespace-with-readtable-synced)
  ;;; read-from-string is overwritten above
  )

(eclector.readtable::init-clasp-as-eclector-reader)  

(defun patch-object (client value-old seen-objects)
  (multiple-value-bind (state object*)
      (labeled-object-state client value-old)
    (cond ((null state)
           (eclector.reader:fixup client value-old seen-objects)
           value-old)
          ((eq state :final)
           object*))))

(defmethod eclector.reader:fixup (client (object core:cxx-object) seen-objects)
  (let ((patcher (core:make-record-patcher (lambda (object)
                                             (patch-object client object seen-objects)))))
    (core:patch-object object patcher)))
