;;;  -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll
;;;
;;;    This program is free software; you can redistribute it and/or
;;;    modify it under the terms of the GNU Library General Public
;;;    License as published by the Free Software Foundation; either
;;;    version 2 of the License, or (at your option) any later version.
;;;
;;;    See file '../Copyright' for full details.

(defconstant +sequence-type+ '(unsigned-byte 16))

(defconstant +source-pathname+
  (make-pathname :name nil :type nil
		 :directory (append (pathname-directory *load-pathname*)
				    (list "sources"))
		 :host (pathname-host *load-pathname*)
		 :device (pathname-device *load-pathname*)))

(defconstant +all-mappings+
  '(("ATARIST" "http://unicode.org/Public/MAPPINGS/VENDORS/MISC/ATARIST.TXT")

    ("ISO-8859-1" "http://unicode.org/Public/MAPPINGS/ISO8859/8859-1.TXT")
    ("ISO-8859-2" "http://unicode.org/Public/MAPPINGS/ISO8859/8859-2.TXT")
    ("ISO-8859-3" "http://unicode.org/Public/MAPPINGS/ISO8859/8859-3.TXT")
    ("ISO-8859-4" "http://unicode.org/Public/MAPPINGS/ISO8859/8859-4.TXT")
    ("ISO-8859-5" "http://unicode.org/Public/MAPPINGS/ISO8859/8859-5.TXT")
    ("ISO-8859-6" "http://unicode.org/Public/MAPPINGS/ISO8859/8859-6.TXT")
    ("ISO-8859-7" "http://unicode.org/Public/MAPPINGS/ISO8859/8859-7.TXT")
    ("ISO-8859-8" "http://unicode.org/Public/MAPPINGS/ISO8859/8859-8.TXT")
    ("ISO-8859-9" "http://unicode.org/Public/MAPPINGS/ISO8859/8859-9.TXT")
    ("ISO-8859-10" "http://unicode.org/Public/MAPPINGS/ISO8859/8859-10.TXT")
    ("ISO-8859-11" "http://unicode.org/Public/MAPPINGS/ISO8859/8859-11.TXT")
    ("ISO-8859-13" "http://unicode.org/Public/MAPPINGS/ISO8859/8859-13.TXT")
    ("ISO-8859-14" "http://unicode.org/Public/MAPPINGS/ISO8859/8859-14.TXT")
    ("ISO-8859-15" "http://unicode.org/Public/MAPPINGS/ISO8859/8859-15.TXT")
    ("ISO-8859-16" "http://unicode.org/Public/MAPPINGS/ISO8859/8859-16.TXT")
    ("KOI8-R" "http://unicode.org/Public/MAPPINGS/VENDORS/MISC/KOI8-R.TXT")
    ("KOI8-U" "http://unicode.org/Public/MAPPINGS/VENDORS/MISC/KOI8-U.TXT")
    ("CP-856" "http://unicode.org/Public/MAPPINGS/VENDORS/MISC/CP856.TXT")
    ("CP-856" "http://unicode.org/Public/MAPPINGS/VENDORS/MISC/CP856.TXT")
    
    ("DOS-CP437" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP437.TXT")
    ("DOS-CP737" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP737.TXT")
    ("DOS-CP775" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP775.TXT")
    ("DOS-CP850" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP850.TXT")
    ("DOS-CP852" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP852.TXT")
    ("DOS-CP855" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP855.TXT")
    ("DOS-CP857" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP857.TXT")
    ("DOS-CP860" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP860.TXT")
    ("DOS-CP861" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP861.TXT")
    ("DOS-CP862" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP862.TXT")
    ("DOS-CP863" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP863.TXT")
    ("DOS-CP864" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP864.TXT")
    ("DOS-CP865" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP865.TXT")
    ("DOS-CP866" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP866.TXT")
    ("DOS-CP869" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP869.TXT")
    ("DOS-CP874" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP874.TXT") 

    ; Redundant WINDOWS-CP874 DOS-CP874
    ;("WINDOWS-CP874" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP874.TXT")

    ("WINDOWS-CP932" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP932.TXT")
    ("WINDOWS-CP936" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP936.TXT")
    ("WINDOWS-CP949" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP949.TXT")
    ("WINDOWS-CP950" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP950.TXT")

    ("WINDOWS-CP1250" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1250.TXT")
    ("WINDOWS-CP1251" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1251.TXT")
    ("WINDOWS-CP1252" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1252.TXT")
    ("WINDOWS-CP1253" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1253.TXT")
    ("WINDOWS-CP1254" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1254.TXT")
    ("WINDOWS-CP1255" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1255.TXT")
    ("WINDOWS-CP1256" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1256.TXT")
    ("WINDOWS-CP1257" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1257.TXT")
    ("WINDOWS-CP1258" "http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1258.TXT")

    ("JISX0201" "http://unicode.org/Public/MAPPINGS/OBSOLETE/EASTASIA/JIS/JIS0201.TXT")
    ("JISX0208" "http://unicode.org/Public/MAPPINGS/OBSOLETE/EASTASIA/JIS/JIS0208.TXT"
     ;; Fixes compatible with libiconv: we replace a reverse solidus with a
     ;; fullwidth reverse solidus, so that JISX0208 does not contain characters
     ;; in the ASCII range (Needed by ISO-2022-JP-1)
     ((#x815F #x2140 #xff3c)))

    ("JISX0212" "http://unicode.org/Public/MAPPINGS/OBSOLETE/EASTASIA/JIS/JIS0212.TXT"
     ;; Fixes compatible with libiconv: we replace a tilde with a
     ;; fullwidth tilde, so that JISX0212 does not contain characters
     ;; in the ASCII range (Needed by ISO-2022-JP-1)
     ((#x2237 #xff5e)))

    ("SHIFT-JIS" "http://unicode.org/Public/MAPPINGS/OBSOLETE/EASTASIA/JIS/SHIFTJIS.TXT")

    ;Unable to parse because they output more than one Unicode character
    ;("SJIS-0213" "http://x0213.org/codetable/sjis-0213-2004-std.txt")
    ;("EUC-JISX0213" "http://x0213.org/codetable/euc-jis")
    ))

(defun download (filename url)
  (unless (probe-file filename)
    (let ((command (format nil "curl \"~A\" > ~A" url filename)))
      (unless (zerop (si::system command))
	(error "Unable to retrieve file ~A" url)))))

(defun reformat (line)
  (loop with l = (length line)
     for i from 0 below l
     for c = (char line i)
     do (cond ((eql c #\#)
	       (return (if (zerop i) "" (subseq line 0 (1- i)))))
	      ((not (standard-char-p c))
	       (setf (char line i) #\space))
	      ((and (eql c #\0)
		    (let ((j (1+ i)))
		      (and (< j l) (member (char line j) '(#\x #\X)))))
	       (setf (char line i) #\#)))
     finally (return line)))

(defun read-mapping (name &optional (n 2))
  (let* ((source-file (make-pathname :name name :defaults +source-pathname+))
	 (record (find name +all-mappings+ :key #'first :test #'equalp))
	 (fixes (third record))
	 (source-url (fourth record)))
    (unless (probe-file source-file)
      (unless source-url
	(error "Unknown encoding ~A" name))
      (download file source-url))
    (with-open-file (in source-file :direction :input)
      (loop with output = '()
	 for line = (reformat (read-line in nil nil))
	 while line
	 unless (zerop (length line))
	 do (with-input-from-string (aux line)
	      (let ((byte-list (loop for byte = (read aux nil nil)
				  while byte
				  collect byte)))
		(unless (/= (length byte-list) n)
		  (loop for i in fixes
		     when (= (first i) (first byte-list))
		     do (progn (setf byte-list i) (return)))
		  (push byte-list output))))
	 finally (return (nreverse output))))))

(defun mapping-hash-table (mapping)
  (loop with hash = (make-hash-table :size (floor (* 1.5 (length mapping)))
				     :test 'eq)
     for (multibyte codepoint) in mapping
     for unicode-char = (code-char codepoint)
     do (progn
	  (setf (gethash multibyte hash) unicode-char)
	  (setf (gethash unicode-char hash) multibyte)
	  (when (> multibyte #xFF)
	    (setf (gethash (ash multibyte -8) hash) t)))
     finally (return hash)))

(defun dump-mapping-array (mapping-assoc output-file)
  (let* ((mapping-list (reduce #'nconc mapping-assoc))
	 (mapping-array (make-array (length mapping-list) :element-type +sequence-type+
				    :initial-contents mapping-list)))
    (format t "~%;;; Generating ~A" output-file)
    (force-output t)
    (with-open-file (s output-file :direction :output :if-exists :supersede
		       :element-type +sequence-type+ :external-format :big-endian)
      (write-byte (length mapping-array) s)
      (write-sequence mapping-array s))))

(defun copy-encoding-file (in out)
  (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8))))
    (format t "~%;;; Copying ~A to ~A" in out)
    (with-open-file (sin in :direction :input :element-type '(unsigned-byte 8))
      (with-open-file (sout out :direction :output :element-type '(unsigned-byte 8)
			    :if-exists :supersede :if-does-not-exist :create)
	(loop for nbytes = (read-sequence buffer sin)
	   until (zerop nbytes)
	   do (write-sequence buffer sout :end nbytes))))))

(defun all-valid-unicode-chars (mapping)
  (cond ((consp mapping)
	 (loop for sublist on mapping
	    for i from 0 below 10
	    until (and (eq sublist mapping) (plusp i))
	    collect (all-valid-unicode-chars (first sublist))))
	((hash-table-p mapping)
	 (concatenate 'string (loop for key being the hash-key in mapping
				 when (characterp key)
				 collect key)))
	((eq mapping :iso-8859-1)
	 (coerce 'string (loop for i from 0 to 255 collect (code-char i))))
	(t
	 (error "Unknown encoding"))))

(defun compare-hashes (h1 h2)
  (flet ((h1-in-h2 (h1 h2)
	   (loop for k being the hash-key in h1 using (hash-value v)
	      for v2 = (gethash k h2 nil)
	      unless (or (consp v2) (consp v) (equal v v2))
	      do (progn (print (list h1 k v h2 k v2))
			(error)
			(return nil))
	      finally (return t))))
    (and (h1-in-h2 h1 h2)
	 (h1-in-h2 h2 h1))))
