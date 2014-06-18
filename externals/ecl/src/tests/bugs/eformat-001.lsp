;-*- Mode:     Lisp -*-
;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Sat Jan 03 2:56:03 CEST 2007
;;;; Contains: External format tests
;;;;
;;;; Based on the code and files from FLEXI-STREAMS 1.0.7
;;;;

#+(or)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :cl-test)
    (make-package :cl-test)))

(in-package :cl-test)

(defconstant +buffer-size+ 8192
  "Size of buffers for COPY-STREAM* below.")

(defparameter *copy-function* 'copy-stream
  "Which function to use when copying from one stream to the other -
see for example COPY-FILE below.")

(defparameter *eformat-test-files*
  '(("unicode_demo" (:utf8 :ucs2 :ucs4))
    ("kafka" (:utf8 :latin1 :cp1252))
    ("hebrew" (:utf8 :latin8))
    ("russian" (:utf8 :koi8r))
    ("tilton" (:utf8 :ascii))
    )
  "A list of test files where each entry consists of the name
prefix and a list of encodings.")

(defun create-file-variants (file-name symbol)
  "For a name suffix FILE-NAME and a symbol SYMBOL denoting an
encoding returns a list of pairs where the car is a full file
name and the cdr is the corresponding external format.  This list
contains all possible variants w.r.t. to line-end conversion and
endianness."
  (let ((variants (ecase symbol
		    (:ascii '(:us-ascii))
		    (:latin1 '(:latin-1))
		    (:latin8 '(:iso-8859-8))
		    (:cp1252 '(:windows-cp1252))
		    (:koi8r '(:koi8-r))
		    (:utf8 '(:utf-8))
		    (:ucs2 '(:ucs-2be :ucs-2le))
		    (:ucs4 '(:ucs-4be :ucs-4le)))))
    (loop for arg in variants
          nconc (let* ((endian-suffix (case arg
					((:ucs-2be :ucs-4be) "_be")
					((:ucs-2le :ucs-4le) "_le")
					(t ""))))
		  (loop for eol-style in '(:lf :cr :crlf)
		     collect (cons (format nil "~A_~(~A~)_~(~A~)~A.txt"
					   file-name symbol eol-style endian-suffix)
				   (list eol-style arg)))))))

(defun create-test-combinations (file-name symbols &optional simplep)
  "For a name suffix FILE-NAME and a list of symbols SYMBOLS denoting
different encodings of the corresponding file returns a list of lists
which can be used as arglists by DO-EFORMAT-TEST-001.  If SIMPLEP is true, a
list which can be used for the string and sequence tests below is
returned."
  (let ((file-variants (loop for symbol in symbols
                             nconc (create-file-variants file-name symbol))))
    (loop for (name-in . external-format-in) in file-variants
          when simplep
          collect (list name-in external-format-in)
          else
          nconc (loop for (name-out . external-format-out) in file-variants
                      collect (list name-in external-format-in name-out external-format-out)))))

(defun file-equal (file1 file2)
  "Returns a true value iff FILE1 and FILE2 have the same
contents \(viewed as binary files)."
  (with-open-file (stream1 file1 :element-type '(unsigned-byte 8))
    (with-open-file (stream2 file2 :element-type '(unsigned-byte 8))
      (if (= (file-length stream1) (file-length stream2))
	  (loop for p1 = (file-position stream1)
	     for byte1 = (read-byte stream1 nil nil)
	     for byte2 = (read-byte stream2 nil nil)
	     while (and byte1 byte2)
	     unless (= byte1 byte2)
	     do (return (values nil p1))
	     finally (return (values t 0)))
	  (values nil -1)))))

(defun copy-stream (in out)
  "Copies the contents of the binary stream STREAM-IN to the
binary stream STREAM-OUT using flexi streams - STREAM-IN is read
with the external format EXTERNAL-FORMAT-IN and STREAM-OUT is
written with EXTERNAL-FORMAT-OUT."
  (loop for line = (read-line in nil nil)
     while line
     do (write-line line out)))

(defun copy-stream* (in out)
  "Like COPY-STREAM, but uses READ-SEQUENCE and WRITE-SEQUENCE instead
of READ-LINE and WRITE-LINE."
  (let ((buffer (make-array +buffer-size+ :element-type 'char*)))
    (loop
     (let ((position (read-sequence buffer in)))
       (when (zerop position) (return))
       (write-sequence buffer out :end position)))))

(defun do-eformat-test-001 (*copy-function*)
  "Each test in this suite copies the contents of one file \(in the
`test' directory) to another file \(in a temporary directory) using
flexi streams with different external formats.  The resulting file is
compared with an existing file in the `test' directory to check if the
outcome is as expected.  Uses various variants of the :DIRECTION
keyword when opening the files.

Returns a true value iff all tests succeeded.  Prints information
about each individual comparison if VERBOSE is true."
  (labels
      ((copy-file (path-in external-format-in path-out external-format-out
			   direction-out direction-in)
	 (with-open-file (in path-in
			     :element-type 'character
			     :direction direction-in
			     :if-does-not-exist :error
			     :if-exists :overwrite
			     :external-format external-format-in)
	   (with-open-file (out path-out
				:element-type 'character
				:direction direction-out
				:if-does-not-exist :create
					:if-exists :supersede
					:external-format external-format-out)
		     (funcall *copy-function* in out))))
       (one-comparison (path-in external-format-in path-out external-format-out)
	 (format t "~%;;; ~A -> ~A" path-in path-out)
	 (loop with full-path-in = (merge-pathnames path-in "./eformat-tests/")
	    and full-path-out = (ensure-directories-exist
				 (merge-pathnames path-out "./eformat-tmp/"))
	    and full-path-orig = (merge-pathnames path-out "./eformat-tests/")
	    for direction-out in '(:output :io)
	    nconc (loop for direction-in in '(:input :io)
		       for args = (list path-in external-format-in direction-in
					path-out external-format-out direction-out)
		     with ok = nil
		     with pos = 0
		     unless (progn
			      (copy-file full-path-in external-format-in
					 full-path-out external-format-out
					 direction-out direction-in)
			      (multiple-value-setq (ok pos)
				(file-equal full-path-out full-path-orig)))
		     collect (progn
				 (format t "~%;;; Discordance at pos ~D~%between ~A~% and ~A~%"
					 pos full-path-out full-path-orig)
				 args)))))
    (loop with do-eformat-test-001-args-list =
	 (loop for (file-name symbols) in *eformat-test-files*
	    nconc (create-test-combinations file-name symbols))
       for (path-in external-format-in path-out external-format-out) in do-eformat-test-001-args-list
       nconc (one-comparison path-in external-format-in path-out external-format-out))))

;;; Date: 02/01/2007
;;; From: Juanjo
;;; Fixed: Not a bug
;;; Description:
;;;
;;;	Test external formats by transcoding several files into all possible
;;;	supported formats and checking against the expected results. This
;;;	test uses READ/WRITE-CHAR via READ/WRITE-LINE.
;;;
(deftest eformat-0001-transcode-read-char
    (do-eformat-test-001 'copy-stream)
  nil)

;;; Date: 02/01/2007
;;; From: Juanjo
;;; Fixed: Not a bug
;;; Description:
;;;
;;;	Test external formats by transcoding several files into all possible
;;;	supported formats and checking against the expected results. This
;;;	test uses READ/WRITE-CHAR via READ/WRITE-LINE.
;;;
(deftest eformat-0002-transcode-read-char
    (do-eformat-test-001 'copy-stream*)
  nil)
