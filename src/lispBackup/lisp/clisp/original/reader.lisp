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

(set-dispatch-macro-character #\# #\( #'(lambda (stm ch p)
		(declare (ignore ch))
  (let* ((x (read-delimited-list #\) stm t))
         (len (length x)))
    (when (not *read-suppress*)
      (if (and p (or (> len p)
                     (and (zerop len) (plusp p))))
        (error 'reader-error))
      (fill (replace (make-array (or p len)) x) (car (last x)) :start len)))))

(set-dispatch-macro-character #\# #\" #'(lambda (stm ch p) ;!!! #""
		(declare (ignore p))
  (unread-char ch stm)
  (read stm t nil t)))


        
(defun _sharp-comment (stm ch p)
		(declare (ignore p))
  (do ((c (read-char stm t nil t) (read-char stm t nil t)))
      ((and (eql c ch)
            (eql (peek-char nil stm) #\#))
         (read-char stm) (values))
     (when (and (eql c #\#) (eql (peek-char nil stm) ch))
       (read-char stm)
       (_sharp-comment stm ch nil))))
       
;(set-dispatch-macro-character #\# #\| #' _sharp-comment)
      
(set-dispatch-macro-character #\# #\S #'(lambda (stm ch p)
    (declare (ignore ch))
  (if p (err))
  (cond (*read-suppress* (read stm t nil t) nil)
	    (t (let ((r (let* ((*reading-struct* t))
	                    (read stm t nil t))))
	           (if (atom r) (err)
	                        (let ((name (car r))
	                              (d (cdr r)))
	                          (if (not (symbolp name))
	                            (err)
	                            (case name
	                              (hash-table (if (atom d) (err)
	                                                       (make-hash-table :test (car d) :initial-contents (cdr d))))
	                              (pathname (apply #'make-pathname d))
	                              (byte (apply #'make-byte d))
	                              (t (let ((desc (get name 'DEFSTRUCT-DESCRIPTION)))
	                                   (if (and desc (vectorp desc))
	                                     (apply (svref desc 2) d)
	                                     (err)))))))))))))
      

(set-dispatch-macro-character #\# #\C
  #'(lambda (stream sub-char n)
      (declare (ignore sub-char))
      (if *read-suppress*
        (progn (read stream t nil t) nil)
        (if n
          (error "~: Zwischen # und C ist keine Zahl erlaubt." 'read)
          (let ((h (read stream t nil t)))
            (if (and (consp h) (consp (cdr h)) (null (cddr h))
                     (numberp (first h)) (not (complexp (first h)))
                     (numberp (second h)) (not (complexp (second h)))
                )
              (apply #'complex h)
              (error "~: Wrong Syntax for complex Number: #C~" 'read h)
)   ) ) ) ) )

(set-dispatch-macro-character #\# #\A
  #'(lambda (stream sub-char n)
      (declare (ignore sub-char))
      (if *read-suppress*
        (progn (read stream t nil t) nil)
        (if (null n)
          (let ((h (read stream t nil t)))
            (if (and (consp h) (consp (cdr h)) (consp (cddr h)) (null (cdddr h)))
              (make-array (second h) :element-type (first h) :initial-contents (third h))
              (error "~: Wrong Syntax for Array: #A~" 'read h)
          ) )
          (let* ((rank n)
                 (cont (read stream t nil t))
                 (dims '())
                 (eltype 't))
            (when (plusp rank)
              (let ((subcont cont) (i 0))
                (loop
                  (let ((l (length subcont)))
                    (push l dims)
                    (incf i) (when (>= i rank) (return))
                    (when (plusp l) (setq subcont (elt subcont 0)))
                ) )
                (cond ((stringp subcont) (setq eltype 'character))
                      ((bit-vector-p subcont) (setq eltype 'bit))
            ) ) )
            (make-array (nreverse dims) :element-type eltype :initial-contents cont)
)   ) ) ) )

(set-dispatch-macro-character #\# #\P
     (lambda (stream sub-char n)
       (declare (ignore sub-char))
       (if *read-suppress*
         (progn (read stream t nil t) nil)
         (if n
           (error "~ of ~: no infix for #P" (quote read) stream)
           (let ((obj (read stream t nil t)))
             (if (stringp obj)
               (values (parse-namestring obj))
               (if (listp obj)
                 (apply (function make-pathname) obj)
                 (error "~ of ~: Wrong Syntax for Pathname: #P~"
                        (quote read) stream obj))))))))

                        
(defun read-from-string (string &optional (eof-error-p t) (eof-value nil)
                         &key (start 0) (end nil) (preserve-whitespace nil)
                         &aux index)
  (values
    (with-input-from-string (stream string :start start :end end :index index)
      (funcall (if preserve-whitespace #'read-preserving-whitespace #'read)
               stream eof-error-p eof-value nil
    ) )
    index
) )    




#|
(defvar *read-reference-table* nil)

(set-dispatch-macro-character #\# #\*
   #'(lambda (stream sub-char n)
       (declare (ignore sub-char))
       (let* ((token (read-token stream)))
         (unless *read-suppress*
           (unless (or [Escape-Zeichen im Token verwendet]
                       (every #'(lambda (ch) (member ch '(#\0 #\1))) token))
             (error "~ von ~: Nach #* dürfen nur Nullen und Einsen kommen."
                     'read stream))
           (let ((l (length token)))
             (if n
               (cond ((< n l)
                      (error "~ von ~: Bit-Vektor länger als angegebene Länge ~."
                              'read stream n))
                     ((and (plusp n) (zerop l))
                      (error "~ von ~: Element für Bit-Vektor der Länge ~ muss spezifiziert werden."
                              'read stream n)))
               (setq n l))
             (let ((bv (make-array n :element-type 'bit))
                   (i 0)
                   b)
               (loop
                 (when (= i n) (return))
                 (when (< i l) (setq b (case (char token i) (#\0 0) (#\1 1))))
                 (setf (sbit bv i) b)
                 (incf i))
               bv))))))
|#

(defun _sharp-r (stm ch p)
  (cond (*read-suppress* (read stm t nil t) nil)
        ((not p) (err))
				((not (<= 2 p 36))
				  (err))
				(t (let ((res (let ((*read-base* p))
		                    (read stm t nil t))))
						 (unless (typep res 'rational)
							 (error stm "#~A (base ~D) value is not a rational: ~S."
								 ch p res))
						 res))))


(set-dispatch-macro-character #\# #\R #'_sharp-r)

(set-dispatch-macro-character #\# #\B #'(lambda (stm ch p)
		(declare (ignore p))
	(_sharp-r stm ch 2)))

(set-dispatch-macro-character #\# #\O #'(lambda (stm ch p)
		(declare (ignore p))
	(_sharp-r stm ch 8)))

(set-dispatch-macro-character #\# #\X #'(lambda (stm ch p)
		(declare (ignore p))
	(_sharp-r stm ch 16)))

#|
(set-dispatch-macro-character #\# #\Y #'(lambda (stm ch p)
	(if p (err))
	(cond (*read-suppress* (read stm t nil t) nil)
	      (t (let ((x (read stm t nil t)))
	           (%make-closure (car x) (make-code-vector (cadr x)) (caddr x) nil))))))
|#

(copy-readtable nil *_standard-readtable*)


