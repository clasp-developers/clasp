(in-package #:cmp)

;;; So that non-cst-client can inherit behaviour
(defclass clasp-eclector-client-mixin ()())

(defclass clasp-cst-client (eclector.concrete-syntax-tree:cst-client clasp-eclector-client-mixin) ())

;; singleton- don't bother with constructor
(defvar *cst-client*
  (locally (declare (notinline make-instance))
    (make-instance 'clasp-cst-client)))

;; Copy from build environment
(defvar *additional-clasp-character-names* '#.*additional-clasp-character-names*)
(defvar *mapping-char-code-to-char-names* '#.*mapping-char-code-to-char-names*)

(defmethod eclector.base:source-position
    ((client clasp-cst-client) stream)
  (compile-file-source-pos-info stream))

(defun simple-unicode-name (name)
  "If NAME is a string from \"U00\" to \"U10FFFF\", return the corresponding Unicode character."
  (if (and (>= (length name) 3) (char-equal (char name 0) #\U))
      (let ((number (parse-integer name :start 1 :radix 16 :junk-allowed t)))
        (if (and (numberp number) (<= #X00 number #X10FFFF))
            (code-char number)
            nil))
      nil))

(defmethod eclector.reader:find-character ((client clasp-eclector-client-mixin) name)
  ;; This variable is defined in define-unicode-tables, which is loaded later.
  (declare (special *additional-clasp-character-names*))
  (or (call-next-method)
      (gethash name *additional-clasp-character-names*)
      (simple-unicode-name name)))

(defun map-char-to-char-name (char)
  (gethash char cmp::*mapping-char-code-to-char-names*))

(defun minimal-unicode-name (char)
  (let ((code (char-code char)))
    (format nil "U~x" code)))

(defun cl:name-char (string-designator)
  (let ((name (etypecase string-designator
                (string string-designator)
                (symbol (symbol-name string-designator))
                (character (string string-designator)))))
    (eclector.reader:find-character *cst-client* name)))

(defun cl:char-name (char)
  (or 
   (values (map-char-to-char-name char))
   ;;; If there is no mapping, at least return "U<char-code-as-hex>"
   ;;; Should be the exception
   (minimal-unicode-name char)))

(defun map-make-structure-arguments (initargs)
  (loop for all = initargs then (cddr all)
        for key = (first all) and value = (second all)
        while all
        append (list
                (if (keywordp key) key (intern (string key) :keyword))
                value)))

(defmethod eclector.reader:make-structure-instance
    ((client clasp-eclector-client-mixin) name initargs)
  ;;; see discussion in https://github.com/s-expressionists/Eclector/issues/63
  ;;; initargs might be string-designators, not keywords, need to transform
  (core::make-structure
   name
   (map-make-structure-arguments initargs)))

(defmethod eclector.reader:wrap-in-quasiquote
    ((client clasp-eclector-client-mixin) form)
  (list 'core:quasiquote form))
(defmethod eclector.reader:wrap-in-unquote
    ((client clasp-eclector-client-mixin) form)
  (list 'core::unquote form))
(defmethod eclector.reader:wrap-in-unquote-splicing
    ((client clasp-eclector-client-mixin) form)
  (list 'core::unquote-splice form))

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

(defclass clasp-non-cst-eclector-client (clasp-eclector-client-mixin) ())
(defvar *clasp-normal-eclector-client* (make-instance 'clasp-non-cst-eclector-client))

(defclass clasp-tracking-eclector-client (clasp-eclector-client-mixin eclector.parse-result:parse-result-client) ())

(defmethod eclector.base:source-position
    ((client clasp-tracking-eclector-client) stream)
  (compile-file-source-pos-info stream))

(defmethod eclector.parse-result:make-expression-result
    ((client clasp-tracking-eclector-client) result children source)
  (declare (ignore children))
  (when *source-locations*
    (setf (gethash result *source-locations*) (car source)))
  result)

(defmethod eclector.reader:state-value
    ((client clasp-eclector-client-mixin) (aspect (eql 'cl:*readtable*)))
  cl:*readtable*)

(defmethod eclector.reader:call-with-state-value
    ((client clasp-eclector-client-mixin) thunk (aspect (eql 'cl:*readtable*)) value)
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
