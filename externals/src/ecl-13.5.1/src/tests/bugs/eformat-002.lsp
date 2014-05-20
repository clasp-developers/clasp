;-*- Mode:     Lisp -*-
;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Sat Jan 03 2:56:03 CEST 2007
;;;; Contains: External format tests
;;;;
;;;; Based on the code and files from FLEXI-STREAMS 1.0.7
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :cl-test)
    (make-package :cl-test)))

(in-package :cl-test)

(load "sys:encodings;tools")

(setf *print-circle* t) ; some mappings contain circular structures

(defun binary-dump (filename &optional (position 0) (limit nil))
  (format t "~%FILE: ~A from ~D, ~D bytes" filename position limit)
  (with-open-file (file filename :element-type '(unsigned-byte 8))
    (file-position file position)
    (loop for i from 0
       for byte = (read-byte file nil nil)
       for c = (and byte (code-char byte))
       while (and byte (or (null limit) (< i limit)))
       do (progn (when (zerop (mod i 8)) (terpri))
		 (format t "~5X ~3A" byte
			 (cond ((and (< 31 byte 127) (standard-char-p c))
				c)
			       ((eql c #\Esc) "ESC")
			       (t " ")))
		 )))
  (terpri)
  (force-output))

(defun random-strings (char-bag n)
  (if (consp char-bag)
      (apply #'concatenate 'string
	     (loop for i from 0 below 2
		for actual-bag = (elt char-bag (random (length char-bag)))
		collect (random-strings actual-bag (random n))))
      (concatenate 'string
		   (loop for i from 0 to n
		      for c = (char char-bag (random (length char-bag)))
		      unless (eql c #\Newline)
		      collect c))))

(defun compare-files (a b &optional all-chars)
  (with-open-file (sa a :direction :input :element-type '(unsigned-byte 8))
    (with-open-file (sb b :direction :input :element-type '(unsigned-byte 8))
      (loop for b1 = (read-byte sa nil nil)
	 for b2 = (read-byte sb nil nil)
	 while (or b1 b2)
	 do (unless (eql b1 b2)
	      (let* ((position (1- (file-position sa)))
		     (start-dump (max 0 (- position 8))))
		(setf position (logandc2 position 3))
		(binary-dump a start-dump 32)
		(binary-dump b start-dump 32)
		(format t "~%Mismatch between~%~T~A~% and~T~A~% at file position ~D~%"
			a b position)
		(when all-chars
		  (loop with imin = (floor start-dump 4)
		     with imax = (min (+ imin 9) (length all-chars))
		     for i from imin below imax
		     for j from 0
		     for c = (char all-chars i)
		     do (progn (when (zerop (mod j 8)) (terpri))
			       (format t "~4X " (char-code c))))
		  (terpri))
		(return nil)))
	 finally (return t)))))

(defun test-output (format-name &optional iconv-name (nlines 128) (nchars 10))
  (set 'ext::foo format-name)
  (let* ((*print-circle* t)
	 (mappings (loop for table = (ext::make-encoding format-name)
		      while (and table (symbolp table))
		      do (setf format-name table)
		      finally (return (or table format-name))))
	 (char-bags (all-valid-unicode-chars mappings))
	 (encoded-filename (format nil "eformat-tmp/iconv-~A.txt" format-name))
	 (decoded-filename (format nil "eformat-tmp/iconv-~A-utf32.txt" format-name))
	 (iconv-filename (format nil "eformat-tmp/iconv-~A-iconv-utf32.txt" format-name))
	 (random-lines (loop for line from 1 to nlines
			  collect (random-strings char-bags nchars)))
	 (all-chars (apply #'concatenate 'string
			   (loop for i in random-lines
			      nconc (list i (list #\Newline))))))
    (ensure-directories-exist encoded-filename)
    ;; Output in that format
    (with-open-file (out encoded-filename :direction :output :external-format format-name
			 :if-exists :supersede)
      (loop for i in random-lines
	 do (write-line i out)))
    (with-open-file (out decoded-filename :direction :output :external-format :ucs-4be
			 :if-exists :supersede)
      (loop for i in random-lines
	 do (write-line i out)))
    (with-open-file (in encoded-filename :direction :input :external-format format-name)
      (loop for line = (read-line in nil nil)
	 for i in random-lines
	 for n from 1
	 while line
	 unless (string= i line)
	 do (progn
	      (format t "Mismatch on line ~D between~% ~S and~% ~S" n line i)
	      (return-from test-output nil))))	   
    (when iconv-name
      (si::system (format nil "iconv -f ~A -t UTF-32BE ~A > ~A"
			  iconv-name encoded-filename iconv-filename))
      (compare-files decoded-filename iconv-filename all-chars))))

;;; Date: 09/01/2007
;;; From: Juanjo
;;; Fixed: Not a bug
;;; Description:
;;;
;;;	Test external formats by transcoding random sequences of characters using
;;;	ECL and iconv.
;;;
(deftest eformat-0002-simple-iconv-check
    (loop for name in '(:ISO-8859-1 :ISO-8859-2 :ISO-8859-3 :ISO-8859-4
			:ISO-8859-5 :ISO-8859-6 :ISO-8859-7 :ISO-8859-8
			:ISO-8859-9 :ISO-8859-10 :ISO-8859-11 :ISO-8859-13
			:ISO-8859-14 :ISO-8859-15 :ISO-8859-16

			:KOI8-R :KOI8-U

			:IBM437 :IBM850 :IBM852 :IBM855 :IBM857 :IBM860
			:IBM861 :IBM862 :IBM863 :IBM864 :IBM865 :IBM866
			:IBM869

			:CP936 :CP949 :CP950

			:WINDOWS-1250 :WINDOWS-1251 :WINDOWS-1252 :WINDOWS-1253
			:WINDOWS-1254 :WINDOWS-1256 :WINDOWS-1257

			; :CP932 :WINDOWS-1255 :WINDOWS-1258 with
			; iconv may output combined characters, when ECL would
			; output the base and the comibining one. Hence, no simple
			; comparison is possible.

			:ISO-2022-JP :ISO-2022-JP-1)
       unless (progn
		(format t "~%;;; Testing ~A " name)
		(loop for i from 1 to 10
		   always (test-output name (symbol-name name))))
       collect name)
  nil)
