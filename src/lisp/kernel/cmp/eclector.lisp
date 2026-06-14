(in-package #:cmp)

;;; general source tracking

(defmacro ext:with-source-tracking
    ((stream &key (pathname nil pathnamep) (lineno 0) (offset 0))
     &body body)
  "Evaluate BODY such that STREAM tracks source locations.
EXT:STREAM-SOURCE-LOCATION can be used to get the current source location from the stream."
  `(let ((*compile-file-source-debug-pathname*
           ;; (pathname stream) will signal an error if stream is not
           ;; associated with a file. We want an error, since if a
           ;; string-stream or something was passed in, the user needs
           ;; to specify their own pathname.
           ;; Maybe the error should be more specific though? FIXME
           ,(if pathnamep
                `(pathname ,pathname)
                `(truename ,stream)))
         (*compile-file-file-scope*
           (core:file-scope *compile-file-source-debug-pathname*))
         (*compile-file-source-debug-lineno* ,lineno)
         (*compile-file-source-debug-offset* ,offset))
     ,@body))

(defun ext:stream-source-location (stream)
  "Get the current source location from STREAM. This can only be used within EXT:WITH-SOURCE-TRACKING."
  (core:input-stream-source-pos-info
   stream *compile-file-file-scope*
   *compile-file-source-debug-lineno*
   *compile-file-source-debug-offset*))

;;; these two basically exist to keep uniform source-location- names
(declaim (inline ext:source-location-lineno ext:source-location-column))
(defun ext:source-location-lineno (spi)
  (core:source-pos-info-lineno spi))
(defun ext:source-location-column (spi)
  (core:source-pos-info-column spi))

;;; So that non-cst-client can inherit behaviour
(defclass clasp-eclector-client-mixin ()())

;; Copy from build environment
(defvar *additional-clasp-character-names* '#.*additional-clasp-character-names*)
(defvar *mapping-char-code-to-char-names* '#.*mapping-char-code-to-char-names*)

(defun simple-unicode-name (name)
  "If NAME is a string from \"U00\" to \"U10FFFF\", return the corresponding Unicode character."
  (if (and (>= (length name) 3) (char-equal (char name 0) #\U))
      (let ((number (parse-integer name :start 1 :radix 16 :junk-allowed t)))
        (if (and (numberp number) (<= #X00 number #X10FFFF))
            (code-char number)
            nil))
      nil))

(defmethod eclector.reader:find-character ((client clasp-eclector-client-mixin) name)
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
    ;; mimics FIND-CHARACTER method above.
    (or (eclector.reader:find-character nil name) ; default method, for standard chars
        (gethash name *additional-clasp-character-names*)
        (simple-unicode-name name))))

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

(defclass clasp-normal-eclector-client (clasp-eclector-client-mixin) ())
(defvar *clasp-normal-eclector-client* (make-instance 'clasp-normal-eclector-client))

(defclass clasp-tracking-eclector-client (clasp-eclector-client-mixin eclector.parse-result:parse-result-client) ())

;;; Used when compiling in a provided first-class global environment.
(defclass clasp-alternate-env-client (clasp-tracking-eclector-client)
  ((%environment :initarg :environment :reader client-environment)))

(defmethod eclector.base:source-position
    ((client clasp-tracking-eclector-client) stream)
  (ext:stream-source-location stream))

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

(defmethod eclector.reader:state-value
    ((client clasp-alternate-env-client) aspect)
  (core:variable-cell/value
   (core:fcge-ensure-vcell (client-environment client) aspect)))

(defmacro progv-env (environment symbols values &body forms)
  `(core:progv-env-function ,environment ,symbols ,values
                            (lambda ()
                              (declare (core:lambda-name core::progv-env-lambda))
                              (progn ,@forms))))

;;; Check local packages then defer to fcge-find-package.
;;; Also do the string coercion.
(defun env-find-package (environment name)
  (let ((name (string name))
        (package (core:variable-cell/value
                  (core:fcge-ensure-vcell environment '*package*))))
    (or (cdr (assoc name (ext:package-local-nicknames package) :test #'string=))
        (core::fcge-find-package environment name))))

(defmethod eclector.reader:call-with-state-value
    ((client clasp-alternate-env-client) thunk aspect value)
  (progv-env (client-environment client)
      (list aspect) (list value)
    (funcall thunk)))
;; *package* has to have its designator coerced
(defmethod eclector.reader:call-with-state-value
    ((client clasp-alternate-env-client) thunk (aspect (eql 'cl:*package*))
     value)
  (let ((package (env-find-package (client-environment client) value)))
    (assert (packagep package))
    (progv-env (client-environment client)
        (list aspect) (list package)
      (funcall thunk))))

(defmethod eclector.reader:evaluate-expression
    ((client clasp-alternate-env-client) expression)
  (core:interpret expression (client-environment client)))

(defun find-package-or-err (environment name)
  (or (env-find-package environment name)
      (error 'package-error :package name)))

(defmethod eclector.reader:interpret-symbol ((client clasp-alternate-env-client)
                                             input-stream package-indicator
                                             symbol-name internp)
  (declare (ignore input-stream))
  (if (null package-indicator)
      (make-symbol symbol-name)
      (let ((package (case package-indicator
                       (:current
                        (let ((cur (eclector.reader:state-value
                                    client '*package*)))
                          ;; We disallow this through Eclector above, but
                          ;; there are other ways we can't control in which
                          ;; *package* could be set to something illegal.
                          (assert (packagep cur) ()
                                  "~s is not bound to a package" '*package*)
                          cur))
                       (:keyword (find-package-or-err (client-environment client)
                                                      "KEYWORD"))
                       (t (find-package-or-err (client-environment client)
                                               package-indicator)))))
        (if internp
            (intern symbol-name package)
            (multiple-value-bind (symbol status)
                (find-symbol symbol-name package)
              (ecase status
                ((:external) symbol)
                ((:internal :inherited)
                 (error "~a is not external in ~a"
                        symbol-name package-indicator))
                ((nil)
                 (error "No symbol ~a:~a" package-indicator symbol-name))))))))

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
  (eclector.reader:set-standard-macro-characters readtable)
  (eclector.reader:set-standard-dispatch-macro-characters readtable)
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

(defun patch-labeled-object (client value-old seen-objects)
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
                                             (patch-labeled-object client object seen-objects)))))
    (core:patch-object object patcher)))

(defvar *source-client* (make-instance 'clasp-tracking-eclector-client))

(defun ext:read-source (&optional
                          input-stream-designator (eof-error-p t)
                          eof-value recursive-p environment)
  "READ with source tracking. Returns two values: the form read, and an object representing a source location map for that form. This object is not defined for users, but may be passed to EVAL-SOURCE or COMPILE-SOURCE to give the evaluator and/or compiled code source locations.
If EOF-ERROR-P is NIL and the stream hits EOF, the first value will be ELF-VALUE and the second value is undefined."
  (let ((eclector.reader:*client*
          (if environment
              (make-instance 'cmp::clasp-alternate-env-client
                :environment *environment*)
              *source-client*))
        (*source-locations* (make-hash-table :test #'eq)))
    (values (eclector.parse-result:read eclector.reader:*client*
                                        (or input-stream-designator
                                            *standard-input*)
                                        eof-error-p eof-value)
            *source-locations*)))

(defun ext:eval-source (form &optional source environment)
  "Evaluate FORM in ENVIRONMENT. If SOURCE is non-null it must be a source object returned from EXT:READ-SOURCE or EXT:AUGMENT-SOURCE, and is used to provide source locations for code and error messages."
  (cond (source
         (check-type source hash-table)
         (let ((*source-locations* source))
           (core:interpret form environment)))
        (t (core:interpret form environment))))

(defun ext:augment-source (new-form source
                           &optional (default (ext:current-source-location)))
  "Create a new source object (as returned by READ-SOURCE) for NEW-FORM, based on the mapping SOURCE which is presumably to some but not all subforms of NEW-FORM. Any subforms of NEW-FORM that are not present in the SOURCE mapping will be given DEFAULT as their source location if provided; DEFAULT must be a source location as returned by EXT:STREAM-SOURCE-LOCATION or EXT:CURRENT-SOURCE-LOCATION, and defaults to the latter."
  ;; This function is basically like CST:RECONSTRUCT. It has this
  ;; interface in case we ever go back to using something like CSTs.
  ;; Imagine for example we've read a form and now want to wrap it in a
  ;; lambda expression: `(lambda () ,form). If the SOURCE for FORM is a
  ;; CST, passing this lambda expression and that CST would not work,
  ;; because the compiler would refer to the CST rather than the form
  ;; and choke on the non-lambda-expression it was given.
  ;; SOURCE being a hash table, as it is, does not have this particular
  ;; issue, but it's nice to be able to augment with a default.
  (check-type source hash-table)
  (let ((new-source (make-hash-table :test #'eq)))
    ;; copy the old source map
    (maphash (lambda (k v) (setf (gethash k new-source) v)) source)
    ;; install defaults
    (when default
      (check-type default core:source-pos-info)
      (labels ((default (subform)
                 (unless (gethash subform new-source)
                   (setf (gethash subform new-source) default)
                   ;; cycle checking is handled by the above UNLESS
                   (when (consp subform)
                     (default (car subform))
                     (default (cdr subform))))))
        (default new-form)))
    new-source))
