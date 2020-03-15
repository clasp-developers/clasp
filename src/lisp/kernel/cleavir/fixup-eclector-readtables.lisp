(cl:in-package #:eclector.readtable.simple)

;;; SYMBOL_EXPORT_SC_(EclectorReadtablePkg,readtable_case);
(defmethod eclector.readtable:readtable-case ((readtable t))
  (error 'type-error :datum readtable :EXPECTED-TYPE 'eclector.readtable.simple:readtable))

;;; SYMBOL_EXPORT_SC_(EclectorReadtablePkg,setf_readtable_case);
;;; need to dispach to (setf)
(defun ECLECTOR.READTABLE::SETF-READTABLE-CASE (mode readtable)
  ;;; Check that the readtable is of a type understood by eclector
  ;;; mode can be :upcase :downcase :invert :preserve
  (unless (member mode '(:upcase :downcase :invert :preserve))
    (error 'type-error :datum mode :EXPECTED-TYPE '(member :upcase :downcase :invert :preserve)))
  (unless (typep readtable 'ECLECTOR.READTABLE.SIMPLE:READTABLE)
    (error 'type-error :datum readtable :EXPECTED-TYPE 'ECLECTOR.READTABLE.SIMPLE:READTABLE))
  (setf (eclector.readtable:readtable-case readtable) mode))

#+(or)
(defmethod (setf eclector.readtable:readtable-case) (mode (readtable t))
  (error 'type-error :datum readtable :EXPECTED-TYPE 'eclector.readtable.simple:readtable))

;;; SYMBOL_EXPORT_SC_(EclectorReadtablePkg,make_dispatch_macro_character);
(defmethod eclector.readtable:make-dispatch-macro-character
    ((readtable t) char &optional non-terminating-p)
  (error 'type-error :datum readtable :EXPECTED-TYPE 'eclector.readtable.simple:readtable))

;;; SYMBOL_EXPORT_SC_(EclectorReadtablePkg,get_macro_character);

(defmethod eclector.readtable:get-macro-character ((readtable t) char)
  (if (null readtable)
      ;;; to avoid breaking (get-macro-character #\{ nil)
      (cl:get-macro-character char cl:*readtable*)
      (error 'type-error :datum readtable :EXPECTED-TYPE 'eclector.readtable.simple:readtable)))

;;; SYMBOL_EXPORT_SC_(EclectorReadtablePkg,set_macro_character);
(defmethod eclector.readtable:set-macro-character
    ((readtable t) char function &optional non-terminating-p)
  (error 'type-error :datum readtable :EXPECTED-TYPE 'eclector.readtable.simple:readtable))

;;; SYMBOL_EXPORT_SC_(EclectorReadtablePkg,get_dispatch_macro_character);
(defmethod eclector.readtable:get-dispatch-macro-character
    ((readtable t) disp-char sub-char)
  (error 'type-error :datum readtable :EXPECTED-TYPE 'eclector.readtable.simple:readtable))

;;; SYMBOL_EXPORT_SC_(EclectorReadtablePkg,set_dispatch_macro_character);
(defmethod eclector.readtable:set-dispatch-macro-character
    ((readtable t) disp-char sub-char function)
  (error 'type-error :datum readtable :EXPECTED-TYPE 'eclector.readtable.simple:readtable))

;;; SYMBOL_EXPORT_SC_(EclectorReadtablePkg,syntax_type);
(defmethod eclector.readtable:syntax-type ((readtable t) char)
  (error 'type-error :datum readtable :EXPECTED-TYPE 'eclector.readtable.simple:readtable))

;;; SYMBOL_EXPORT_SC_(EclectorReadtablePkg,setf_syntax_type);
(defmethod (setf eclector.readtable:syntax-type)
    (syntax-type (readtable t) char)
  (error 'type-error :datum readtable :EXPECTED-TYPE 'eclector.readtable.simple:readtable))

;;; SYMBOL_EXPORT_SC_(EclectorReadtablePkg,copy_readtable_into);
(defmethod eclector.readtable:copy-readtable-into
    ((from-readtable t) (to-readtable readtable))
  (error 'type-error :datum from-readtable :EXPECTED-TYPE 'eclector.readtable.simple:readtable))

(defmethod eclector.readtable:copy-readtable-into
    ((from-readtable readtable) (to-readtable t))
  (error 'type-error :datum to-readtable :EXPECTED-TYPE 'eclector.readtable.simple:readtable))

(defmethod eclector.readtable:copy-readtable-into
    ((from-readtable t) (to-readtable t))
  (error 'type-error :datum from-readtable :EXPECTED-TYPE 'eclector.readtable.simple:readtable))

;;; SYMBOL_EXPORT_SC_(EclectorReadtablePkg,copy_readtable);
(defmethod eclector.readtable:copy-readtable ((readtable t))
  (error 'type-error :datum readtable :EXPECTED-TYPE 'eclector.readtable.simple:readtable))

;;; SYMBOL_EXPORT_SC_(EclectorReadtablePkg,readtablep);
(defmethod eclector.readtable:readtablep ((readtable t))
  nil)

;;; was forgotten in Eclector, fixed in de134e75ddee442a1811f5f9561133836e36fcf8
(defmethod eclector.readtable:readtablep ((readtable ECLECTOR.READTABLE.SIMPLE:READTABLE))
  t)

;;; SYMBOL_EXPORT_SC_(EclectorReadtablePkg,set_syntax_from_char);
;;; ECLECTOR.READTABLE:SET-SYNTAX-FROM-CHAR is a defun

