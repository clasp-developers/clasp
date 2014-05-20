#|#########################################################################
# Copyright (C) 1997-2011 Ufasoft                                         #
# http://ufasoft.com   mailto:support@ufasoft.com                         #
#                                                                         #
# This program is free software; you can redistribute it and/or modify it #
# under the terms of the GNU General Public License as published by the   #
# Free Software Foundation; either version 3, or (at your option) any     #
# later version.                                                          #
#                                                                         #
# This program is distributed in the hope that it will be useful,         #
# but WITHOUT ANY WARRANTY; without even the implied warranty of          #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           #
# GNU General Public License for more details.                            #
#                                                                         #
# You should have received a copy of the GNU General Public License       #
# along with this program; If not, see <http://www.gnu.org/licenses/>     #
########################################################################=|#

(in-package "SYS")

(defun string-left-trim (cb s)
  (do ((i 0 (1+ i)))
      ((or (>= i (length s))
           (not (find (char s i) cb)))
         (subseq s i))))

(defun string-right-trim (cb s)
  (do ((i (1- (length s)) (1- i)))
      ((or (minusp i)
           (not (find (char s i) cb)))
         (subseq s 0 (1+ i)))))

(defun string-trim (cb s)
  (string-left-trim cb (string-right-trim cb s)))
  
(defun string/= (x y &rest rest &key (start1 0) end1 (start2 0) end2)
		(declare (ignore start1 end1 start2 end2))
  (apply #'mismatch (string x) (string y) :test #'char= rest))

(defun _string-compare< (c= c< x y &rest rest &key (start1 0) end1 (start2 0) end2)
  (setq x (string x)
        y (string y))
  (let ((m (apply #'mismatch x y :test c= rest)))
    (when m
      (let ((off (- m start1)))
        (cond ((>= off (- (_end end2 y) start2)) nil)
              ((>= m (_end end1 x)) m)          
              ((funcall c< (char x m) (char y (+ start2 off))) m))))))
#|          
(defun _string-compare> (c= c> x y &rest rest &key (start1 0) end1 (start2 0) end2)
  (let ((m (apply #'mismatch x y :test c= rest)))
    (cond ((or (null m) (>= m (_end end1 x))) nil)
          ((>= m (_end end2 y)) m)          
          ((funcall c> (char x m) (char y m)) m))))
|#

(defun string< (x y &rest rest &key (start1 0) end1 (start2 0) end2)
		(declare (ignore start1 end1 start2 end2))
  (apply #'_string-compare< #'char= #'char< x y rest))

(defun string> (x y &rest rest &key (start1 0) end1 (start2 0) end2)
		(declare (ignore rest))
  (let ((m (string< y x :start1 start2 :end1 end2 :start2 start1 :end2 end1)))
    (if m (+ (- m start2) start1))))


;  (apply #'_string-compare> #'char= #'char> x y rest))

(defun string<= (x y &rest rest &key (start1 0) end1 (start2 0) end2)
		(declare (ignore start1 start2 end2))
  (if (apply #'string= x y rest)
    (_end end1 x)
    (apply #'string< x y rest)))

(defun string>= (x y &rest rest &key (start1 0) end1 (start2 0) end2)
		(declare (ignore start1 start2 end2))
  (if (apply #'string= x y rest)
    (_end end1 x)
    (apply #'string> x y rest)))

(defun string-not-equal (x y &rest rest &key (start1 0) end1 (start2 0) end2)
		(declare (ignore start1 end1 start2 end2))
  (apply #'mismatch (string x) (string y) :test #'char-equal rest))

(defun string-lessp (x y &rest rest &key (start1 0) end1 (start2 0) end2)
		(declare (ignore start1 end1 start2 end2))
  (apply #'_string-compare< #'char-equal #'char-lessp x y rest))

(defun string-greaterp (x y &rest rest &key (start1 0) end1 (start2 0) end2)
		(declare (ignore rest))
  (let ((m (string-lessp y x :start1 start2 :end1 end2 :start2 start1 :end2 end1)))
    (if m (+ (- m start2) start1))))
                        
;  (apply #'_string-compare> #'char-equal #'char-greaterp x y rest))

(defun string-not-lessp (x y &rest rest &key (start1 0) end1 (start2 0) end2)
		(declare (ignore start1 start2 end2))
  (if (apply #'string-equal x y rest)
    (_end end1 x)
    (apply #'string-greaterp x y rest)))

(defun string-not-greaterp (x y &rest rest &key (start1 0) end1 (start2 0) end2)
		(declare (ignore start1 start2 end2))
  (if (apply #'string-equal x y rest)
    (if end1 end1 (length x))
    (apply #'string-lessp x y rest)))

(defmacro with-one-string (string start end cum-offset &rest forms)
  `(let ((,string (if (stringp ,string) ,string (string ,string))))
     (with-array-data ((,string ,string :offset-var ,cum-offset)
		       (,start ,start)
		       (,end (or ,end (length (the vector ,string)))))
       ,@forms)))


#|!!!R
(defun string-upcase (string &key (start 0) end)
  "Given a string, returns a new string that is a copy of it with
  all lower case alphabetic characters converted to uppercase."
  (declare (fixnum start))
  (let* ((string (if (stringp string) string (string string)))
	 (slen (length string)))
    (declare (fixnum slen))
    (with-one-string string start end offset
      (let ((offset-slen (+ slen offset))
	    (newstring (make-string slen)))
	(declare (fixnum offset-slen))
	(do ((index offset (1+ index))
	     (new-index 0 (1+ new-index)))
	    ((= index start))
	  (declare (fixnum index new-index))
	  (setf (schar newstring new-index) (schar string index)))
	(do ((index start (1+ index))
	     (new-index (- start offset) (1+ new-index)))
	    ((= index (the fixnum end)))
	  (declare (fixnum index new-index))
	  (setf (schar newstring new-index)
		(char-upcase (schar string index))))
	(do ((index end (1+ index))
	     (new-index (- (the fixnum end) offset) (1+ new-index)))
	    ((= index offset-slen))
	  (declare (fixnum index new-index))
	  (setf (schar newstring new-index) (schar string index)))
	newstring))))

(defun string-downcase (string &key (start 0) end)
  "Given a string, returns a new string that is a copy of it with
  all upper case alphabetic characters converted to lowercase."
  (declare (fixnum start))
  (let* ((string (if (stringp string) string (string string)))
	 (slen (length string)))
    (declare (fixnum slen))
    (with-one-string string start end offset
      (let ((offset-slen (+ slen offset))
	    (newstring (make-string slen)))
	(declare (fixnum offset-slen))
	(do ((index offset (1+ index))
	     (new-index 0 (1+ new-index)))
	    ((= index start))
	  (declare (fixnum index new-index))
	  (setf (schar newstring new-index) (schar string index)))
	(do ((index start (1+ index))
	     (new-index (- start offset) (1+ new-index)))
	    ((= index (the fixnum end)))
	  (declare (fixnum index new-index))
	  (setf (schar newstring new-index)
		(char-downcase (schar string index))))
	(do ((index end (1+ index))
	     (new-index (- (the fixnum end) offset) (1+ new-index)))
	    ((= index offset-slen))
	  (declare (fixnum index new-index))
	  (setf (schar newstring new-index) (schar string index)))
	newstring))))
|#

(defun string-capitalize (string &key (start 0) end)
  "Given a string, returns a copy of the string with the first
  character of each ``word'' converted to upper-case, and remaining
  chars in the word converted to lower case. A ``word'' is defined
  to be a string of case-modifiable characters delimited by
  non-case-modifiable chars."
  (declare (fixnum start))
  (let* ((string (if (stringp string) string (string string)))
	 (slen (length string)))
    (declare (fixnum slen))
    (with-one-string string start end offset
      (let ((offset-slen (+ slen offset))
	    (newstring (make-string slen)))
	(declare (fixnum offset-slen))
	(do ((index offset (1+ index))
	     (new-index 0 (1+ new-index)))
	    ((= index start))
	  (declare (fixnum index new-index))
	  (setf (schar newstring new-index) (schar string index)))
	(do ((index start (1+ index))
	     (new-index (- start offset) (1+ new-index))
	     (newword t)
	     (char ()))
	    ((= index (the fixnum end)))
	  (declare (fixnum index new-index))
	  (setq char (schar string index))
	  (cond ((not (alphanumericp char))
		 (setq newword t))
		(newword
		 ;;char is first case-modifiable after non-case-modifiable
		 (setq char (char-upcase char))
		 (setq newword ()))
		;;char is case-modifiable, but not first
		(t (setq char (char-downcase char))))
	  (setf (schar newstring new-index) char))
	(do ((index end (1+ index))
	     (new-index (- (the fixnum end) offset) (1+ new-index)))
	    ((= index offset-slen))
	  (declare (fixnum index new-index))
	  (setf (schar newstring new-index) (schar string index)))
	newstring))))

(defun nstring-upcase (string &key (start 0) end)
  "Given a string, returns that string with all lower case alphabetic
  characters converted to uppercase."
  (declare (fixnum start))
  (let ((save-header string))
    (with-one-string string start end offset
      (do ((index start (1+ index)))
	  ((= index (the fixnum end)))
	(declare (fixnum index))
	(setf (schar string index) (char-upcase (schar string index)))))
    save-header))

(defun nstring-downcase (string &key (start 0) end)
  "Given a string, returns that string with all upper case alphabetic
  characters converted to lowercase."
  (declare (fixnum start))
  (let ((save-header string))
    (with-one-string string start end offset
      (do ((index start (1+ index)))
	  ((= index (the fixnum end)))
	(declare (fixnum index))
	(setf (schar string index) (char-downcase (schar string index)))))
    save-header))

(defun nstring-capitalize (string &key (start 0) end)
  "Given a string, returns that string with the first
  character of each ``word'' converted to upper-case, and remaining
  chars in the word converted to lower case. A ``word'' is defined
  to be a string of case-modifiable characters delimited by
  non-case-modifiable chars."
  (declare (fixnum start))
  (let ((save-header string))
    (with-one-string string start end offset
      (do ((index start (1+ index))
	   (newword t)
	   (char ()))
	  ((= index (the fixnum end)))
	(declare (fixnum index))
	(setq char (schar string index))
	(cond ((not (alphanumericp char))
	       (setq newword t))
	      (newword
	       ;;char is first case-modifiable after non-case-modifiable
	       (setf (schar string index) (char-upcase char))
	       (setq newword ()))
	      (t
	       (setf (schar string index) (char-downcase char))))))
    save-header))
