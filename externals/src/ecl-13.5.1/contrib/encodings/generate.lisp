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

(load (merge-pathnames "tools" *load-pathname*))

(defconstant +encodings-root+ (translate-logical-pathname #p"ext:encodings;"))

(loop for entry in +all-mappings+
   for name = (first entry)
   for orig = (make-pathname :name name :type "BIN"
                             :defaults +encodings-root+)
   for copy = (ensure-directories-exist
               (make-pathname :name name :type "BIN"
                              :defaults "build:encodings;"))
   do (progn
	(unless (probe-file orig)
          (error "Missing mapping")
	  (let ((mapping (if (equalp name "JISX0208")
			     (mapcar #'rest (read-mapping name 3))
			     (read-mapping name))))
	    (dump-mapping-array mapping orig)))
	(copy-encoding-file orig copy)))

(defconstant +aliases+
  '((:us-ascii ext::ascii)
    (:utf-8 ext::utf8)
    (:ucs-2 ext::ucs2 ext::utf-16 ext::utf16 ext::unicode)
    (:ucs-2le ext::ucs2le ext::utf-16le)
    (:ucs-2be ext::ucs2be ext::utf-16be)
    (:ucs-4 ext::ucs4 ext::utf-32 ext::utf32)
    (:ucs-4be ext::ucs4be ext::utf-32be)
    (:ucs-4le ext::ucs4le ext::utf-32le)

    (ext::koi8-r ext::koi8r)
    
    (ext::iso-8859-1 ext::iso8859-1 ext::latin-1 ext::cp819 ext::ibm819)
    (ext::iso-8859-2 ext::iso8859-2 ext::latin-2 ext::latin2)
    (ext::iso-8859-3 ext::iso8859-3 ext::latin-3 ext::latin3)
    (ext::iso-8859-4 ext::iso8859-4 ext::latin-4 ext::latin4)
    (ext::iso-8859-5 ext::iso8859-5 ext::cyrillic)
    (ext::iso-8859-6 ext::iso8859-6 ext::arabic ext::asmo-708 ext::ecma-114)
    (ext::iso-8859-7 ext::iso8859-7 ext::greek8 ext::greek ext::ecma-118)
    (ext::iso-8859-8 ext::iso8859-8 ext::hebrew)
    (ext::iso-8859-9 ext::iso8859-9 ext::latin-5 ext::latin5)
    (ext::iso-8859-10 ext::iso8859-10 ext::latin-6 ext::latin6)
    ;(:iso-8859-11 :iso8859-11 :thai)
    (ext::iso-8859-13 ext::iso8859-13 ext::latin-7 ext::latin7)
    (ext::iso-8859-14 ext::iso8859-14 ext::latin-8 ext::latin8)
    (ext::iso-8859-15 ext::iso8859-15 ext::latin-9 ext::latin9)

    (ext::dos-cp437 ext::ibm437)
    (ext::dos-cp850 ext::ibm850 ext::cp850)
    (ext::dos-cp852 ext::ibm852)
    (ext::dos-cp855 ext::ibm855)
    (ext::dos-cp857 ext::ibm857)
    (ext::dos-cp860 ext::ibm860)
    (ext::dos-cp861 ext::ibm861)
    (ext::dos-cp862 ext::ibm862 ext::cp862)
    (ext::dos-cp863 ext::ibm863)
    (ext::dos-cp864 ext::ibm864)
    (ext::dos-cp865 ext::ibm865)
    (ext::dos-cp866 ext::ibm866 ext::cp866)
    (ext::dos-cp869 ext::ibm869)

    (ext::windows-cp932 ext::windows-932 ext::cp932)
    (ext::windows-cp936 ext::windows-936 ext::cp936)
    (ext::windows-cp949 ext::windows-949 ext::cp949)
    (ext::windows-cp950 ext::windows-950 ext::cp950)

    (ext::windows-cp1250 ext::windows-1250 ext::ms-ee)
    (ext::windows-cp1251 ext::windows-1251 ext::ms-cyrl)
    (ext::windows-cp1252 ext::windows-1252 ext::ms-ansi)
    (ext::windows-cp1253 ext::windows-1253 ext::ms-greek)
    (ext::windows-cp1254 ext::windows-1254 ext::ms-turk)
    (ext::windows-cp1255 ext::windows-1255 ext::ms-hebr)
    (ext::windows-cp1256 ext::windows-1256 ext::ms-arab)
    (ext::windows-cp1257 ext::windows-1257 ext::winbaltrim)
    (ext::windows-cp1258 ext::windows-1258)
    ))

(loop for (name . aliases) in +aliases+
   do (loop with *package* = (find-package "CL")
	 for alias in aliases
	 for filename0 = (make-pathname :name (symbol-name alias)
                                        :defaults "build:encodings;")
	 for filename = (ensure-directories-exist filename0)
	 do (with-open-file (out filename :direction :output :if-exists :supersede
				 :if-does-not-exist :create :element-type 'base-char)
	      (format t "~%;;; Creating alias ~A -> ~A, ~A" alias name filename)
	      (if (keywordp name)
		  (format out "(defparameter ~S '~S)" alias name)
		  (format out "(defparameter ~S (ext::make-encoding '~S))" alias name))
	      )))

(copy-encoding-file "ext:encodings;tools.lisp" "build:encodings;tools.lisp")
(copy-encoding-file (merge-pathnames "ISO-2022-JP" +encodings-root+)
                    "build:encodings;ISO-2022-JP")
(copy-encoding-file (merge-pathnames "ISO-2022-JP-1" +encodings-root+)
                    "build:encodings;ISO-2022-JP-1")
