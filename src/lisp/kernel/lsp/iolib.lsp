;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;        The IO library.

(in-package "SYSTEM")

(defmacro with-open-stream ((var stream) &rest body)
  "Syntax: (with-open-stream (var stream-form) {decl}* {form}*)
Evaluates FORMs with VAR bound to the value of STREAM-FORM.  The stream is
automatically closed on exit."
  (multiple-value-bind (ds b)
      (find-declarations body)
    `(LET ((,var ,stream))
       ,@ds
       (UNWIND-PROTECT
         (PROGN ,@b)
         (CLOSE ,var)))))

(defmacro with-input-from-string ((var string &key index (start 0) end) &rest body)
  "Syntax: (with-input-from-string (var string-form {keyword value}*)
           {decl}* {form}*)
Evaluates FORMs with VAR bound to a string input stream from the string that
is the value of STRING-FORM.  The stream is automatically closed on exit.
Possible keywords are :INDEX, :START, and :END."
  (if index
      (multiple-value-bind (ds b)
          (find-declarations body)
        `(LET ((,var (MAKE-STRING-INPUT-STREAM ,string ,start ,end)))
           ,@ds
           (UNWIND-PROTECT
             (MULTIPLE-VALUE-PROG1
	      (PROGN ,@b)
	      (SETF ,index (FILE-POSITION ,var)))
             (CLOSE ,var))))
      `(LET ((,var (MAKE-STRING-INPUT-STREAM ,string ,start ,end)))
         ,@body)))

(defmacro with-output-to-string ((var &optional string &rest r &key element-type) &rest body)
  "Syntax: (with-output-to-string (var [string-form]) {decl}* {form}*)
Evaluates FORMs with VAR bound to a string output stream to the string that is
the value of STRING-FORM.  If STRING-FORM is not given, a new string is used.
The stream is automatically closed on exit and the string is returned."
  (if string
      `(LET* ((,var (MAKE-STRING-OUTPUT-STREAM-FROM-STRING ,string))
	      (,(gensym) ,element-type))
	;; We must evaluate element-type if it has been supplied by the user.
	;; Even if we ignore the value afterwards.
         ,@body)
      `(LET ((,var (MAKE-STRING-OUTPUT-STREAM ,@r)))
         ,@body
         (GET-OUTPUT-STREAM-STRING ,var))))

(defun read-from-string (string
                         &optional (eof-error-p t) eof-value
                         &key (start 0) (end (length string))
                              preserve-whitespace)
  "Args: (string &optional (eof-error-p t) (eof-value nil)
              &key (start 0) (end (length string)) (preserve-whitespace nil))
Reads an object from STRING and returns the object.  As the second value,
returns the index to the character next to the object's representation.
PRESERVE-WHITESPACE specifies whether to leave the character next to the
object's representation."
  (let ((stream (make-string-input-stream string start end)))
    (if preserve-whitespace
        (values (read-preserving-whitespace stream eof-error-p eof-value)
                (file-position stream))
        (values (read stream eof-error-p eof-value)
                (file-position stream)))))

;;; This function does what write-to-string does to a symbol name,
;;; when printer escaping is off.
;;; It's over five times as far as write-to-string.
(defun printcasify (symbol-name readtable-case print-case)
  (let ((result (copy-seq symbol-name))
        (len (length symbol-name)))
    (case readtable-case
      ((:preserve) result)
      ((:invert)
       (dotimes (i len result)
         (let ((c (aref result i)))
           (setf (aref result i)
                 (cond ((upper-case-p c)
                        (char-downcase c))
                       ((lower-case-p c)
                        (char-upcase c))
                       (t c))))))
      ((:upcase)
       (let ((capitalize t))
         (dotimes (i len result)
           (let ((c (aref result i)))
             (setf (aref result i)
                   (if (and (upper-case-p c)
                            (or (eq print-case :downcase)
                                (and (eq print-case :capitalize)
                                     (not capitalize))))
                       (char-downcase c)
                       c)
                   capitalize (not (alphanumericp c)))))))
      ((:downcase)
       (let ((capitalize t))
         (dotimes (i len result)
           (let ((c (aref result i)))
             (setf (aref result i)
                   (if (and (lower-case-p c)
                            (or (eq print-case :downcase)
                                (and (eq print-case :capitalize)
                                     capitalize)))
                       (char-downcase c)
                       c)
                   capitalize (not (alphanumericp c))))))))))

(defun stringify (object)
  (when (and (not *print-escape*) (not *print-readably*) (not *print-pretty*))
    (typecase object
      (symbol
       (return-from stringify
         (printcasify (symbol-name object)
                      (readtable-case *readtable*)
                      *print-case*)))
      (string (return-from stringify (copy-seq object)))
      (character (return-from stringify (string object)))))
  ;; By not making a fresh stream every time, we save some time.
  (let ((stream (core:thread-local-write-to-string-output-stream)))
    (write-object object stream)
    (core:get-thread-local-write-to-string-output-stream-string stream)))

(defun write-to-string (object &key ((:escape *print-escape*) *print-escape*)
                                 ((:radix *print-radix*) *print-radix*)
                                 ((:base *print-base*) *print-base*)
                                 ((:circle *print-circle*) *print-circle*)
                                 ((:pretty *print-pretty*) *print-pretty*)
                                 ((:level *print-level*) *print-level*)
                                 ((:length *print-length*) *print-length*)
                                 ((:case *print-case*) *print-case*)
                                 ((:array *print-array*) *print-array*)
                                 ((:gensym *print-gensym*) *print-gensym*)
                                 ((:readably *print-readably*) *print-readably*)
                                 ((:right-margin *print-right-margin*)
                                  *print-right-margin*)
                                 ((:miser-width *print-miser-width*)
                                  *print-miser-width*)
                                 ((:lines *print-lines*) *print-lines*)
                                 ((:pprint-dispatch *print-pprint-dispatch*)
                                  *print-pprint-dispatch*))
  "Returns as a string the printed representation of OBJECT in the specified
mode.  See the variable docs of *PRINT-...* for the mode."
  (stringify object))

(defun prin1-to-string (object)
  "Args: (object)
PRIN1s OBJECT to a new string and returns the result.  Equivalent to
 (WRITE-TO-STRING OBJECT :ESCAPE T)."
  (let ((*print-escape* t))
    (stringify object)))

(defun princ-to-string (object)
  "Args: (object)
PRINCs OBJECT to a new string and returns the result.  Equivalent to
 (WRITE-TO-STRING OBJECT :ESCAPE NIL :READABLY NIL)."
  (let ((*print-escape* nil) (*print-readably* nil))
    (stringify object)))

(defmacro with-open-file ((stream . filespec) &rest body)
  "Syntax: (with-open-file (var filespec-form {options}*) {decl}* {form}*)
Opens the specified file using OPTIONs, and evaluates FORMs with VAR bound to
a stream to/from the file.  The file is automatically closed on exit.  See
OPEN for the options."
  (multiple-value-bind (ds b)
      (find-declarations body)
    `(LET ((,stream (OPEN ,@filespec)))
       ,@ds
       (UNWIND-PROTECT
         (MULTIPLE-VALUE-PROG1 (PROGN ,@b) (WHEN ,stream (CLOSE ,stream)))
         (WHEN ,stream (CLOSE ,stream :ABORT T))))))

(defun y-or-n-p (&optional string &rest args)
  "Args: (&optional format-string &rest args)
Asks the user a Y-or-N question.  Does FRESH-LINE, prints a message as if
FORMAT-STRING and ARGs were given to FORMAT, and then prints \"(Y or N)\" is
printed.  If FORMAT-STRING is NIL, however, no prompt will appear."
  (do ((reply))
      (nil)
    (when string (format *query-io* "~&~?  (Y or N) " string args))
    (setq reply (read *query-io*))
    (cond ((string-equal (symbol-name reply) "Y")
           (return-from y-or-n-p t))
          ((string-equal (symbol-name reply) "N")
           (return-from y-or-n-p nil)))))

(defun yes-or-no-p (&optional string &rest args)
  "Args: (&optional format-string &rest args)
Asks the user an YES-or-NO question.  Does FRESH-LINE, prints a message as if
FORMAT-STRING and ARGs were given to FORMAT, and then prints \"(Y or N)\" is
printed.  If FORMAT-STRING is NIL, however, no prompt will appear."
  (do ((reply))
      (nil)
    (when string (format *query-io* "~&~?  (Yes or No) " string args))
    (setq reply (read *query-io*))
    (cond ((string-equal (symbol-name reply) "YES")
           (return-from yes-or-no-p t))
          ((string-equal (symbol-name reply) "NO")
           (return-from yes-or-no-p nil)))))

(defun sharp-a-reader (stream subchar arg)
  (declare (ignore subchar))
  (let ((initial-contents (read stream nil nil t)))
    (cond
      (*read-suppress* nil)
      ((null arg)
        ;; readably-pretty-printed array: #A(type dims initial-contents)
        (let ((elt-type (car initial-contents))
	      (dims (cadr initial-contents))
	      (initial-contents (caddr initial-contents)))
	  (make-array dims :element-type elt-type :initial-contents initial-contents)))
      (t
        (do* ((i 0 (1+ i))
	      (d nil (cons (length ic) d))
	      (ic initial-contents (if (zerop (length ic)) ic (elt ic 0))))
            ((>= i arg)
             (make-array (nreverse d) :initial-contents initial-contents))
	  (declare (fixnum i)))))))

(set-dispatch-macro-character #\# #\a 'sharp-a-reader)
(set-dispatch-macro-character #\# #\A 'sharp-a-reader)

(defun sharp-s-reader (stream subchar arg)
  (declare (ignore subchar))
  (when (and arg (null *read-suppress*))
        (error "~S is an extra argument for the #s readmacro." arg))
  (let ((l (read stream t nil t)))
    (when *read-suppress*
      (return-from sharp-s-reader nil))
    ;; Intern keywords in the keyword package.
    (do ((ll (cdr l) (cddr ll)))
        ((endp ll) (make-structure (car l) (cdr l)))
      (rplaca ll (intern (string (car ll)) 'keyword)))))

(set-dispatch-macro-character #\# #\s 'sharp-s-reader)
(set-dispatch-macro-character #\# #\S 'sharp-s-reader)

(defvar *dribble-closure* nil)

(defun dribble (&optional (pathname "DRIBBLE.LOG" psp))
  "Args: (&optional filespec)
If FILESPEC is given, starts recording the interaction to the specified file.
FILESPEC may be a symbol, a string, a pathname, or a file stream.  If FILESPEC
is not given, ends the recording."
  (cond (*dribble-closure*
	 (funcall *dribble-closure* psp))
	((null psp)
	 (error "Not in dribble."))
	(t
	 (let* ((namestring (namestring pathname))
                (stream (open pathname :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create))
		(dribble-stream (make-two-way-stream
				 (make-echo-stream *terminal-io* stream)
				 (make-broadcast-stream *terminal-io* stream)))
		(standard-input *standard-input*)
		(standard-output *standard-output*)
		(closure #'(lambda (pathname-p)
			     (when pathname-p
			       (error "Already in dribble (to ~A)" namestring))
			     (unless (and (eq dribble-stream *standard-input*)
					  (eq dribble-stream *standard-output*))
			       (warn "Stream variables rebound while DRIBBLE is on.~%Some output may be lost."))
			     (format stream "~&Finished dribbling to ~A." namestring)
			     (close stream)
			     (setq *standard-input* standard-input
				   *standard-output* standard-output
				   *dribble-closure* nil))))
           (multiple-value-bind (sec min hour day month year)
               (get-decoded-time)
             (format dribble-stream "~&Starts dribbling to ~A (~d/~d/~d, ~2,'0d:~2,'0d:~2,'0d)."
                     namestring year month day hour min sec)
	     (setq *standard-input* dribble-stream
		   *standard-output* dribble-stream
		   *dribble-closure* closure)))))
  (values))

;(provide 'iolib)

(defmacro with-standard-io-syntax (&body body)
  "Syntax: ({forms}*)
The forms of the body are executed in a print environment that corresponds to
the one defined in the ANSI standard. *print-base* is 10, *print-array* is t,
*package* is \"CL-USER\", etc."
  (with-clean-symbols (%progv-list)
    `(let ((%progv-list +io-syntax-progv-list+))
       (progv (si:cons-car %progv-list)
	   (si:cons-cdr %progv-list)
	 ,@body))))

(defmacro with-ecl-io-syntax (&body body)
  "Syntax: ({forms}*)
The forms of the body are executed in a print environment that corresponds to
the one used internally by ECL compiled files."
  (with-clean-symbols (%progv-list)
    `(let ((%progv-list +ecl-syntax-progv-list+))
       (progv (si:cons-car %progv-list)
	   (si:cons-cdr %progv-list)
	 ,@body))))

(defmacro print-unreadable-object
	  ((object stream &key type identity) &body body)
  (if body
      `(flet ((.print-unreadable-object-body. () ,@body))
	 (print-unreadable-object-function
	   ,object ,stream ,type ,identity #'.print-unreadable-object-body.))
    `(print-unreadable-object-function ,object ,stream ,type ,identity nil)))
