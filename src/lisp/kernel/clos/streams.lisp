;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 2004, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;        The CLOS IO library.

(in-package "GRAY")

(import 'ext:ansi-stream)

(unexport '(%close
            %input-stream-p
            %open-stream-p
            %output-stream-p
            %pathname
            %stream-advance-to-column
            %stream-element-type
            %stream-external-format
            %stream-file-descriptor
            %stream-input-column
            %stream-input-line
            %stream-interactive-p
            %stream-line-column
            %stream-line-number
            %stream-set-element-type
            %stream-set-external-format
            %stream-start-line-p
            %truename)
          "GRAY")

;;;
;;; This is the generic function interface for CLOS streams.
;;;
;;; The following is a port of SBCL's implementation of Gray Streams. Minor
;;; caveats with respect to the proposal are that we rather keep CLOSE,
;;; STREAM-ELEMENT-TYPE, INPUT-STREAM-P, OUTPUT-STREAM-P and OPEN-STREAM-P
;;; these as normal functions that call the user extensible EXT:STREAM-{CLOSE,
;;; ELT-TYPE, INPUT-P, OUTPUT-P, OPEN-P}.
;;;

(defgeneric stream-advance-to-column (stream column)
  (:documentation
   "Write enough blank space so that the next character will be
  written at the specified column. Returns true if the operation is
  successful, or NIL if it is not supported for this stream. This is
  intended for use by by PPRINT and FORMAT ~T. The default method uses
  STREAM-LINE-COLUMN and repeated calls to STREAM-WRITE-CHAR with a
  #\SPACE character; it returns NIL if STREAM-LINE-COLUMN returns NIL."))

(defgeneric stream-clear-input (stream)
  (:documentation
   "This is like CL:CLEAR-INPUT, but for Gray streams, returning NIL.
  The default method does nothing."))

(defgeneric stream-clear-output (stream)
  (:documentation
   "This is like CL:CLEAR-OUTPUT, but for Gray streams: clear the given
  output STREAM. The default method does nothing."))

(defgeneric close (stream &key abort)
  (:documentation
   "Close the given STREAM. No more I/O may be performed, but
  inquiries may still be made. If :ABORT is true, an attempt is made
  to clean up the side effects of having created the stream."))

(defgeneric stream-element-type (stream)
  (:documentation
   "Return a type specifier for the kind of object returned by the
  STREAM. The class FUNDAMENTAL-CHARACTER-STREAM provides a default method
  which returns CHARACTER."))

(defgeneric (setf stream-element-type) (new-value stream)
  (:documentation
   "Set the type specifier of the kind of object returned by the
  STREAM. There is no default method as this is optional and only
  needed for bivalent streams."))

(defgeneric stream-external-format (stream)
  (:documentation
   "Return the external format of the STREAM. The default method returns
  :default."))

(defgeneric (setf stream-external-format) (new-value stream)
  (:documentation
   "Set the external format of the STREAM. There is no default method
   as this is optional."))

(defgeneric stream-finish-output (stream)
  (:documentation
   "Attempts to ensure that all output sent to the Stream has reached
  its destination, and only then returns false. Implements
  FINISH-OUTPUT. The default method does nothing."))

(defgeneric stream-force-output (stream)
  (:documentation
   "Attempts to force any buffered output to be sent. Implements
  FORCE-OUTPUT. The default method does nothing."))

(defgeneric stream-fresh-line (stream)
  (:documentation
   "Outputs a new line to the Stream if it is not positioned at the
  beginning of a line. Returns T if it output a new line, nil
  otherwise. Used by FRESH-LINE. The default method uses
  STREAM-START-LINE-P and STREAM-TERPRI."))

(defgeneric input-stream-p (stream)
  (:documentation "Can STREAM perform input operations?"))

(defgeneric streamp (stream)
  (:documentation "Is this object a STREAM?"))

(defgeneric pathname (pathspec)
  (:documentation "Returns the pathname denoted by pathspec."))

(defgeneric truename (filespec)
  (:documentation "truename tries to find the file indicated by filespec and returns its
truename."))

(defgeneric stream-interactive-p (stream)
  (:documentation "Is stream interactive (For instance, a tty)?"))

(defgeneric stream-line-column (stream)
  (:documentation
   "Return the column number where the next character
  will be written, or NIL if that is not meaningful for this stream.
  The first column on a line is numbered 0. This function is used in
  the implementation of PPRINT and the FORMAT ~T directive. For every
  character output stream class that is defined, a method must be
  defined for this function, although it is permissible for it to
  always return NIL."))

(defgeneric stream-line-number (stream)
  (:documentation
   "Return the line number where the next character
  will be written, or NIL if that is not meaningful for this stream.
  The first line is numbered 1. The default method returns NIL."))

;; Extension from CMUCL, SBCL, Mezzano and SICL

(defgeneric stream-line-length (stream)
  (:documentation "Return the stream line length or NIL."))

(defgeneric stream-listen (stream)
  #+sb-doc
  (:documentation
   "This is used by LISTEN. It returns true or false. The default method uses
  STREAM-READ-CHAR-NO-HANG and STREAM-UNREAD-CHAR. Most streams should
  define their own method since it will usually be trivial and will
  always be more efficient than the default method."))

(defgeneric open-stream-p (stream)
  (:documentation
   "Return true if STREAM is not closed. A default method is provided
  by class FUNDAMENTAL-STREAM which returns true if CLOSE has not been
  called on the stream."))

(defgeneric output-stream-p (stream)
  (:documentation "Can STREAM perform output operations?"))

(defgeneric stream-peek-char (stream)
  (:documentation
   "This is used to implement PEEK-CHAR; this corresponds to PEEK-TYPE of NIL.
  It returns either a character or :EOF. The default method calls
  STREAM-READ-CHAR and STREAM-UNREAD-CHAR."))

(defgeneric stream-read-byte (stream)
  (:documentation
   "Used by READ-BYTE; returns either an integer, or the symbol :EOF
  if the stream is at end-of-file."))

(defgeneric stream-read-char (stream)
  (:documentation
   "Read one character from the stream. Return either a
  character object, or the symbol :EOF if the stream is at end-of-file.
  Every subclass of FUNDAMENTAL-CHARACTER-INPUT-STREAM must define a
  method for this function."))

(defgeneric stream-read-char-no-hang (stream)
  (:documentation
   "This is used to implement READ-CHAR-NO-HANG. It returns either a
  character, or NIL if no input is currently available, or :EOF if
  end-of-file is reached. The default method provided by
  FUNDAMENTAL-CHARACTER-INPUT-STREAM simply calls STREAM-READ-CHAR; this
  is sufficient for file streams, but interactive streams should define
  their own method."))

(defgeneric stream-read-line (stream)
  (:documentation
   "This is used by READ-LINE. A string is returned as the first value. The
  second value is true if the string was terminated by end-of-file
  instead of the end of a line. The default method uses repeated
  calls to STREAM-READ-CHAR."))

(defgeneric stream-read-sequence (stream sequence &optional start end)
  (:documentation
   "This is like CL:READ-SEQUENCE, but for Gray streams."))

(defgeneric stream-start-line-p (stream)
  (:documentation
   "Is STREAM known to be positioned at the beginning of a line?
  It is permissible for an implementation to always return
  NIL. This is used in the implementation of FRESH-LINE. Note that
  while a value of 0 from STREAM-LINE-COLUMN also indicates the
  beginning of a line, there are cases where STREAM-START-LINE-P can be
  meaningfully implemented although STREAM-LINE-COLUMN can't be. For
  example, for a window using variable-width characters, the column
  number isn't very meaningful, but the beginning of the line does have
  a clear meaning. The default method for STREAM-START-LINE-P on class
  FUNDAMENTAL-CHARACTER-OUTPUT-STREAM uses STREAM-LINE-COLUMN, so if
  that is defined to return NIL, then a method should be provided for
  either STREAM-START-LINE-P or STREAM-FRESH-LINE."))

(defgeneric stream-terpri (stream)
  (:documentation
   "Writes an end of line, as for TERPRI. Returns NIL. The default
  method does (STREAM-WRITE-CHAR stream #\NEWLINE)."))

(defgeneric stream-unread-char (stream character)
  (:documentation
   "Un-do the last call to STREAM-READ-CHAR, as in UNREAD-CHAR.
  Return NIL. Every subclass of FUNDAMENTAL-CHARACTER-INPUT-STREAM
  must define a method for this function."))

(defgeneric stream-write-byte (stream integer)
  (:documentation
   "Implements WRITE-BYTE; writes the integer to the stream and
  returns the integer as the result."))

(defgeneric stream-write-char (stream character)
  (:documentation
   "Write CHARACTER to STREAM and return CHARACTER. Every
  subclass of FUNDAMENTAL-CHARACTER-OUTPUT-STREAM must have a method
  defined for this function."))

(defgeneric stream-write-string (stream string &optional start end)
  (:documentation
   "This is used by WRITE-STRING. It writes the string to the stream,
  optionally delimited by start and end, which default to 0 and NIL.
  The string argument is returned. The default method provided by
  FUNDAMENTAL-CHARACTER-OUTPUT-STREAM uses repeated calls to
  STREAM-WRITE-CHAR."))

(defgeneric stream-write-sequence (stream sequence &optional start end)
  (:documentation
   "This is like CL:WRITE-SEQUENCE, but for Gray streams."))

(defgeneric stream-file-position (stream &optional position)
  (:documentation
   "This is like CL:FILE-POSITION, but for Gray streams."))

(defgeneric stream-file-length (stream)
  (:documentation
   "This is like CL:FILE-LENGTH, but for Gray streams."))

(defgeneric stream-file-string-length (stream object)
  (:documentation
   "This is like CL:FILE-LENGTH, but for Gray streams. The
   default method for FUNDAMENTAL-CHARACTER-OUTPUT-STREAM
   returns NIL."))

(defgeneric stream-file-descriptor (stream direction)
  (:documentation
   "Return the file-descriptor underlaying STREAM, or NIL if not
   available. DIRECTION must be either :INPUT, :OUTPUT, :IO or
   :PROBE. The direction argument is used to discrimate which
   file descriptor to return for bidirectional streams versus
   unidirectional streams."))

(defgeneric stream-input-column (stream)
  (:documentation
   "Return the column number where the next character
  will be read, or NIL if that is not meaningful for this stream.
  The first column on a line is numbered 0."))

(defgeneric stream-input-line (stream)
  (:documentation
   "Return the line number where the next character
  will be read, or NIL if that is not meaningful for this stream.
  The first line is numbered 1. The default method returns NIL."))


;;;
;;; Our class hierarchy looks like the one from Gray streams
;;;
;;; character output streams
;;;
;;; A character output stream can be created by defining a class that
;;; includes FUNDAMENTAL-CHARACTER-OUTPUT-STREAM and defining methods
;;; for the generic functions below.
;;;
;;; binary streams
;;;
;;; Binary streams can be created by defining a class that includes
;;; either FUNDAMENTAL-BINARY-INPUT-STREAM or
;;; FUNDAMENTAL-BINARY-OUTPUT-STREAM (or both) and defining a method
;;; for STREAM-ELEMENT-TYPE and for one or both of the following
;;; generic functions.
;;;

(let ((clos::*clos-booted* 'clos:map-dependents))
  (defclass fundamental-stream (standard-object stream)
    ((open-p :accessor open-stream-p
             :initform t ))
    (:documentation "the base class for all CLOS streams")))

(defclass fundamental-input-stream (fundamental-stream)
  ())

(defclass fundamental-output-stream (fundamental-stream)
  ())

(defclass fundamental-character-stream (fundamental-stream)
  ())

(defclass fundamental-binary-stream (fundamental-stream)
  ())

(defclass fundamental-character-input-stream
    (fundamental-input-stream fundamental-character-stream)
  ())

(defclass fundamental-character-output-stream
    (fundamental-output-stream fundamental-character-stream)
  ())

(defclass fundamental-binary-input-stream
    (fundamental-input-stream fundamental-binary-stream)
  ())

(defclass fundamental-binary-output-stream
    (fundamental-output-stream fundamental-binary-stream)
  ())


;;;
;;; The following methods constitute default implementations.
;;;

(defun bug-or-error (stream fun)
  (if (typep stream 'stream)
      (error "The stream ~S has no suitable method for ~S." stream fun)
      (error 'type-error :datum stream :expected-type 'stream)))

;; STREAM-ADVANCE-TO-COLUMN

(defmethod stream-advance-to-column ((stream ansi-stream) column)
  (%stream-advance-to-column stream column))

(defmethod stream-advance-to-column ((stream fundamental-character-output-stream)
				     column)
  (let ((current-column (stream-line-column stream)))
    (when current-column
      (let ((fill (floor (- column current-column))))
	(dotimes (i fill)
	  (stream-write-char stream #\Space)))
      t)))


;; CLEAR-INPUT

(defmethod stream-clear-input ((stream fundamental-input-stream))
  (declare (ignore stream))
  nil)

(defmethod stream-clear-input ((stream ansi-stream))
  (cl:clear-input stream))

(defmethod stream-clear-input ((stream t))
  (bug-or-error stream 'stream-clear-input))


;; CLEAR-OUTPUT

(defmethod stream-clear-output ((stream fundamental-output-stream))
  (declare (ignore stream))
  nil)

(defmethod stream-clear-output ((stream ansi-stream))
  (cl:clear-output stream))

(defmethod stream-clear-output ((stream t))
  (bug-or-error stream 'stream-clear-output))


;; CLOSE

(defmethod close ((stream fundamental-stream) &key abort)
  (declare (ignore abort))
  (setf (open-stream-p stream) nil)
  t)

(defmethod close ((stream ansi-stream) &key abort)
  ;; To avoid an recursive loop after redefine-cl-symbols - use
  ;; gray:%close here.
  (%close stream abort))

(defmethod close ((stream t) &key abort)
  (declare (ignore abort))
  (bug-or-error stream 'close))


;; STREAM-ELEMENT-TYPE

(defmethod stream-element-type ((stream fundamental-character-stream))
  (declare (ignore stream))
  'character)

(defmethod stream-element-type ((stream ansi-stream))
  (%stream-element-type stream))

(defmethod (setf stream-element-type) (new-value (stream ansi-stream))
  (%stream-set-element-type stream new-value))

(defmethod stream-element-type ((stream t))
  (bug-or-error stream 'stream-element-type))

;; STREAM-EXTERNAL-FORMAT

(defmethod stream-external-format ((stream ansi-stream))
  (%stream-external-format stream))

(defmethod (setf stream-external-format) (new-value (stream ansi-stream))
  (%stream-set-external-format stream new-value))

(defmethod stream-external-format ((stream fundamental-stream))
  :default)

(defmethod stream-external-format ((stream t))
  (bug-or-error stream 'stream-external-format))

;; FINISH-OUTPUT

(defmethod stream-finish-output ((stream fundamental-output-stream))
  (declare (ignore stream))
  nil)

(defmethod stream-finish-output ((stream ansi-stream))
  (cl:finish-output stream))

(defmethod stream-finish-output ((stream t))
  (bug-or-error stream 'stream-finish-output))


;; FORCE-OUTPUT

(defmethod stream-force-output ((stream fundamental-output-stream))
  (declare (ignore stream))
  nil)

(defmethod stream-force-output ((stream ansi-stream))
  (cl:force-output stream))

(defmethod stream-force-output ((stream t))
  (bug-or-error stream 'stream-force-output))


;; FRESH-LINE

(defmethod stream-fresh-line ((stream fundamental-character-output-stream))
  (unless (stream-start-line-p stream)
    (stream-terpri stream)
    t))

(defmethod stream-fresh-line ((stream ansi-stream))
  (cl:fresh-line stream))

(defmethod stream-fresh-line ((stream t))
  (bug-or-error stream 'stream-fresh-line))

;; INPUT-STREAM-P

(defmethod input-stream-p ((stream fundamental-stream))
  (declare (ignore stream))
  nil)

(defmethod input-stream-p ((stream fundamental-input-stream))
  (declare (ignore stream))
  t)

(defmethod input-stream-p ((stream ansi-stream))
  (%input-stream-p stream))

(defmethod input-stream-p ((stream t))
  (bug-or-error stream 'input-stream-p))


;; INTERACTIVE-STREAM-P

(defmethod stream-interactive-p ((stream fundamental-input-stream))
  nil)

(defmethod stream-interactive-p ((stream ansi-stream))
  (%stream-interactive-p stream))

(defmethod stream-interactive-p ((stream t))
  (bug-or-error stream 'stream-interactive-p))

;; PATHNAME

(defmethod pathname (pathspec)
  (%pathname pathspec))

;; TRUENAME

(defmethod truename (filespec)
  (%truename filespec))

;; LINE-COLUMN

(defmethod stream-line-column ((stream fundamental-character-output-stream))
  (declare (ignore stream))
  nil)

(defmethod stream-line-column ((stream ansi-stream))
  (%stream-line-column stream))

(defmethod stream-line-column ((stream t))
  (bug-or-error stream 'stream-line-column))

;; LINE-NUMBER

(defmethod stream-line-number ((stream t))
  nil)

(defmethod stream-line-number ((stream ansi-stream))
  (%stream-line-number stream))

;; LINE-LENGTH

(defmethod stream-line-length ((stream fundamental-character-output-stream))
  nil)

(defmethod stream-line-length ((stream ansi-stream))
  nil)

(defmethod stream-line-length ((stream t))
  (bug-or-error stream 'stream-line-length))

;; LISTEN

(defmethod stream-listen ((stream fundamental-character-input-stream))
  (let ((char (stream-read-char-no-hang stream)))
    (when (characterp char)
      (stream-unread-char stream char)
      t)))

(defmethod stream-listen ((stream ansi-stream))
  (cl:listen stream))

(defmethod stream-listen ((stream t))
  (bug-or-error stream 'stream-listen))


;; OPEN-STREAM-P

(defmethod open-stream-p ((stream ansi-stream))
  (%open-stream-p stream))

(defmethod open-stream-p ((stream t))
  (bug-or-error stream 'open-stream-p))


;; OUTPUT-STREAM-P

(defmethod output-stream-p ((stream fundamental-stream))
  (declare (ignore stream))
  nil)

(defmethod output-stream-p ((stream fundamental-output-stream))
  (declare (ignore stream))
  t)

(defmethod output-stream-p ((stream ansi-stream))
  (%output-stream-p stream))

(defmethod output-stream-p ((stream t))
  (bug-or-error stream 'output-stream-p))


;; PEEK-CHAR

(defmethod stream-peek-char ((stream fundamental-character-input-stream))
  (let ((char (stream-read-char stream)))
    (unless (eq char :eof)
      (stream-unread-char stream char))
    char))

(defmethod stream-peek-char ((stream ansi-stream))
  (cl:peek-char stream nil nil :eof))

(defmethod stream-peek-char ((stream t))
  (bug-or-error stream 'stream-peek-char))


;; READ-BYTE

(defmethod stream-read-byte ((stream ansi-stream))
  (cl:read-byte stream))

(defmethod stream-read-byte ((stream t))
  (bug-or-error stream 'stream-read-byte))


;; READ-CHAR

(defmethod stream-read-char ((stream ansi-stream))
  (cl:read-char stream nil :eof))

(defmethod stream-read-char ((stream t))
  (bug-or-error stream 'stream-read-char))


;; UNREAD-CHAR

(defmethod stream-unread-char ((stream ansi-stream) character)
  (cl:unread-char character stream))

(defmethod stream-unread-char ((stream ansi-stream) character)
  (declare (ignore character))
  (bug-or-error stream 'stream-unread-char))


;; READ-CHAR-NO-HANG

(defmethod stream-read-char-no-hang ((stream fundamental-character-input-stream))
  (stream-read-char stream))

(defmethod stream-read-char-no-hang ((stream ansi-stream))
  (cl:read-char-no-hang stream))

(defmethod stream-read-char-no-hang ((stream t))
  (bug-or-error stream 'stream-read-char-no-hang))


;; READ-LINE

(defmethod stream-read-line ((stream fundamental-character-input-stream))
  (let ((res (make-string 80))
	(len 80)
	(index 0))
    (loop
     (let ((ch (stream-read-char stream)))
       (cond ((eq ch :eof)
              (return (values (si::shrink-vector res index)
                              t)))
             (t
              (when (char= ch #\newline)
                (return (values (si::shrink-vector res index) nil)))
              (when (= index len)
                (setq len (* len 2))
                (let ((new (make-string len)))
                  (replace new res)
                  (setq res new)))
              (setf (schar res index) ch)
              (incf index)))))))

(defmethod stream-read-line ((stream ansi-stream))
  (cl:read-line stream))

(defmethod stream-read-line ((stream t))
  (bug-or-error stream 'stream-read-line))


;; READ-SEQUENCE

;; Simple default stream-write-sequence method for CLOS streams. Note
;; that we cannot use si:do-read-sequence for this purpose, since it
;; will call stream-read-sequence again.
(defun clos-default-read-sequence (stream sequence start end)
  (declare (type t stream) ; check for c::stream-designator ignored
           (type sequence sequence)
           (fixnum start)
           (ext:check-arguments-type))
  (let ((end (or end (length sequence)))
        (elttype (stream-element-type stream)))
    (declare (fixnum end))
    (if (or (eql elttype 'character) (eql elttype 'base-char))
        (loop for pos from start below end
           do (let ((c (stream-read-char stream)))
                (if (eql c :eof)
                    (return pos)
                    (setf (elt sequence pos) c)))
           finally (return pos))
        (loop for pos from start below end
           do (let ((b (stream-read-byte stream)))
                (if (eql b :eof)
                    (return pos)
                    (setf (elt sequence pos) b)))
           finally (return pos)))))

(defmethod stream-read-sequence ((stream fundamental-character-input-stream)
                                 sequence &optional (start 0) (end nil))
  (clos-default-read-sequence stream sequence start end))

(defmethod stream-read-sequence ((stream fundamental-binary-input-stream)
                                 sequence &optional (start 0) (end nil))
  (clos-default-read-sequence stream sequence start end))

(defmethod stream-read-sequence ((stream ansi-stream) sequence
				 &optional (start 0) (end (length sequence)))
  ;; it is safe to call cl:read-sequence because stream_read_sequence
  ;; does not call the generic gray:stream-read-sequence if the stream
  ;; is an ansi-stream.
  (cl:read-sequence sequence stream :start start :end end))

(defmethod stream-read-sequence ((stream t) sequence &optional start end)
  (declare (ignore sequence start end))
  (bug-or-error stream 'stream-read-sequence))


;; START-LINE-P

(defmethod stream-start-line-p ((stream fundamental-character-output-stream))
  (eql (stream-line-column stream) 0))

(defmethod stream-start-line-p ((stream ansi-stream))
  (%stream-start-line-p stream))

(defmethod stream-start-line-p ((stream t))
  (bug-or-error stream 'stream-start-line-p))

;; FILE-POSITION

(defmethod stream-file-position ((stream ansi-stream) &optional position)
  (file-position stream position))

(defmethod stream-file-position ((stream t) &optional position)
  (declare (ignore stream position))
  nil)

;; FILE-LENGTH

(defmethod stream-file-length ((stream ansi-stream))
  (file-length stream))

(defmethod stream-file-length ((stream t))
  (error 'type-error :datum stream :expected-type 'file-stream))

;; FILE-STRING-LENGTH

(defmethod stream-file-string-length ((stream ansi-stream) object)
  (file-string-length stream object))

(defmethod stream-file-string-length ((stream fundamental-character-output-stream) object)
  (declare (ignore stream object))
  nil)

;; STREAMP

(defmethod streamp ((stream stream))
  (declare (ignore stream))
  t)

(defmethod streamp ((stream t))
  (declare (ignore stream))
  nil)


;; WRITE-BYTE

(defmethod stream-write-byte ((stream ansi-stream) integer)
  (cl:write-byte integer stream))

(defmethod stream-write-byte ((stream t) integer)
  (declare (ignore integer))
  (bug-or-error stream 'stream-write-byte))


;; WRITE-CHAR

(defmethod stream-write-char ((stream ansi-stream) character)
  (cl:write-char character stream))

(defmethod stream-write-char ((stream t) character)
  (declare (ignore character))
  (bug-or-error stream 'stream-write-char))


;; WRITE-SEQUENCE

;; Simple default stream-write-sequence method for CLOS streams. Note
;; that we cannot use si:do-write-sequence for this purpose, since it
;; will call stream-write-sequence again.
(defun clos-default-write-sequence (stream sequence start end)
  (declare (type t stream) ; check for c::stream-designator ignored
           (type sequence sequence)
           (fixnum start)
           (ext:check-arguments-type))
  (let ((end (or end (length sequence)))
        (elttype (stream-element-type stream)))
    (declare (fixnum end))
    (if (or (eql elttype 'character) (eql elttype 'base-char))
        (loop for pos from start below end
           do (stream-write-char stream (elt sequence pos)))
        (loop for pos from start below end
           do (stream-write-byte stream (elt sequence pos)))))
  sequence)

(defmethod stream-write-sequence ((stream fundamental-character-output-stream) sequence
                                  &optional (start 0) end)
  (if (stringp sequence)
      (stream-write-string stream sequence start end)
      (clos-default-write-sequence stream sequence start end)))

(defmethod stream-write-sequence ((stream fundamental-binary-output-stream) sequence
                                  &optional (start 0) end)
  (clos-default-write-sequence stream sequence start end))

(defmethod stream-write-sequence ((stream ansi-stream) sequence
                                  &optional (start 0)
                                            (end (length sequence)))
  ;; it is safe to call cl:write-sequence because
  ;; stream_write_sequence does not call the generic
  ;; gray:stream-write-sequence if the stream is an ansi-stream.
  (cl:write-sequence sequence stream :start start :end end))

(defmethod stream-write-sequence ((stream t) sequence &optional start end)
  (declare (ignore sequence start end))
  (bug-or-error stream 'stream-write-sequence))


;; WRITE-STRING

(defmethod stream-write-string ((stream fundamental-character-output-stream)
				string &optional (start 0) end)
  (declare (type t stream) ; check for c::stream-designator ignored
           (string string)
	   (fixnum start)
           (ext:check-arguments-type))
  (let ((end (or end (length string))))
    (declare (fixnum end))
    (do ((pos start (1+ pos)))
	((>= pos end))
      (declare (type si::index pos))
      (stream-write-char stream (aref string pos))))
  string)

(defmethod stream-write-string ((stream ansi-stream) string &optional (start 0) end)
  (cl:write-string string stream :start start :end end))

(defmethod stream-write-string ((stream t) string &optional start end)
  (declare (ignore string start end))
  (bug-or-error stream 'stream-write-string))


;; TERPRI

(defmethod stream-terpri ((stream fundamental-character-output-stream))
  (stream-write-char stream #\Newline))

(defmethod stream-terpri ((stream ansi-stream))
  (cl:terpri stream))

(defmethod stream-terpri ((stream t))
  (bug-or-error stream 'stream-terpri))


;;; FILE-DESCRIPTOR

(defmethod stream-file-descriptor (stream direction)
  (declare (ignore stream direction))
  nil)

(defmethod stream-file-descriptor ((stream ansi-stream) direction)
  (%stream-file-descriptor stream direction))


;;; STREAM-INPUT-COLUMN

(defmethod stream-input-column (stream)
  (declare (ignore stream))
  nil)

(defmethod stream-input-column ((stream ansi-stream))
  (%stream-input-column stream))


;;; STREAM-INPUT-LINE

(defmethod stream-input-line (stream)
  (declare (ignore stream))
  nil)

(defmethod stream-input-line ((stream ansi-stream))
  (%stream-input-line stream))


;;; Setup

(core:defconstant-equal +conflicting-symbols+
  '(cl:close
    cl:stream-element-type
    cl:stream-external-format
    cl:input-stream-p
    cl:open-stream-p
    cl:output-stream-p
    cl:streamp
    cl:pathname
    cl:truename))

(defun redefine-cl-functions ()
  "Some functions in CL package are expected to be generic. We make them so."
  (unless (and (not (member :eclasp *features*)) (member :staging *features*))
    (provide '#:gray-streams)
    (let ((lockedcl (si::package-is-locked "COMMON-LISP")))
      (si::package-unlock "COMMON-LISP") 
      (loop with gray-package = (find-package "GRAY")
            finally (if lockedcl (si::package-lock "COMMON-LISP"))   
            for cl-symbol in '#.+conflicting-symbols+
            for gray-symbol = (find-symbol (symbol-name cl-symbol) gray-package)
            unless (typep (fdefinition cl-symbol) 'generic-function)
              do (setf (fdefinition cl-symbol)
                       (fdefinition gray-symbol))
                 (when (fboundp `(setf ,gray-symbol))
                   (setf (fdefinition `(setf ,cl-symbol))
                         (fdefinition `(setf ,gray-symbol))))
                 (unintern gray-symbol gray-package)
                 (import cl-symbol gray-package)
                 (export cl-symbol gray-package))
      nil)))

(pushnew :gray-streams-module *features*)

(defun gray-streams-module-provider (name)
  (when (string= name '#:gray-streams)
    (redefine-cl-functions)
    t))

(pushnew 'gray-streams-module-provider ext:*module-provider-functions*)

#+(or cclasp eclasp) (eval-when (:load-toplevel) (setf clos:*clos-booted* t))
