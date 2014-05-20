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

(setq *prin-traillength* 0)
(setq *prin-level* 0)

(defmethod print-object ((x integer) s)
  (_print-integer x s))

(defmethod print-object ((x method) s)
  (write-string "#<METHOD>" s)
  x)

(with-output-to-string (stm)  ;!!! to precompile print-object
  (print-object (find-class 'method) stm))
  
(setq sys::*_use-print-object* t)  

#|!!!R
(defun write-unreadable (fun x stm &key type identity)
  (write-string "#<" stm)
  (when type
    (write (type-of x) :stream stm :readably nil)
    (if (or fun identity)
      (write-string " " stm)))
  (if fun
    (funcall fun))
  (write-string ">" stm)
  nil)
|#

(defmacro print-unreadable-object
    ((&whole args object stream &key type identity) &body body)
  (declare (ignore object stream type identity))
  `(SYSTEM::WRITE-UNREADABLE
     ,(if body `(FUNCTION (LAMBDA () ,@body)) 'NIL)
     ,@args
   ))

(defmethod print-object ((x t) stm)
  (print-unreadable-object (x stm :type t :identity t)
    )
  x)
  
(defmethod print-object ((x package) s)
  (_print-package x s))
  
(defmethod print-object ((x character) s)
	(_print-character x s))
  
(defmethod print-object ((x float) stm)
  (_print-float x stm))  
  
(defmethod print-object ((x symbol) stm)
	(_print-symbol x stm))

(defmethod print-object ((x stream) stm)
	(_print-stream x stm))

(defmethod print-object ((x list) s)
	(_print-list x s)
	x)
  
(defmethod print-object ((x string) s)
  (_print-string x s))

(defmethod print-object ((x ratio) s)
	(_print-ratio x s))
  
(defmethod print-object ((x complex) s)
	(_print-complex x s))

(defmethod print-object ((x vector) s)
  (_print-vector x s))
  
(defmethod print-object ((x bit-vector) s)
  (_print-bit-vector x s))
  
(defmethod print-object ((x array) s)
	(_print-array x s))

(defmethod print-object ((x function) s)
	(_print-function x s))
	
(defmethod print-object ((x pathname) s)
  (cond (*print-readably* (_print-pathname x s))
        (t (write-string "#P" s)
           (write (namestring x) :stream s)))
  x)

(defmethod print-object ((x hash-table) s)
  (_print-hash-table x s)
  x)
  
(defun print-structure (structure stream)
  (let* ((name (type-of structure))
		 (class (get name 'CLOS::CLOSCLASS)))
     (if class
       (let ((readable (clos::class-kconstructor class)))
         (write-string (if readable "#S(" "#<") stream)
         (prin1 name stream)
         (dolist (slot (clos:class-slots class))
           (write-char #\space stream)
           (prin1 (intern (symbol-name (clos:slot-definition-name slot)) *KEYWORD-PACKAGE*) stream)
           (write-char #\space stream)
           (prin1 (%structure-ref name structure (clos:slot-definition-location slot)) stream))
         (write-string (if readable ")" ">") stream))
       (print-unreadable-object (structure stream :type t)
         (do ((l (%record-length structure))
              (i 1 (1+ i)))
             ((>= i l))
           (write-char #\space stream)
           (prin1 (%structure-ref name structure i) stream))))))

#|!!!R
(defun print-structure (structure stream)
  (let* ((name (type-of structure))
         (description (get name 'DEFSTRUCT-DESCRIPTION)))
			(if description
				(let ((readable (svref description 2)))
					(write-string (if readable "#S(" "#<") stream)
					(prin1 name stream)
					(dolist (slot (svref description 3))
					  (cond ((not (simple-vector-p slot)) (err))
					        ((svref slot 0) (let ((obj (svref slot 0)))
																		(write-char #\space stream)
																		(prin1 (intern (symbol-name obj) *KEYWORD-PACKAGE*) stream)
																		(write-char #\space stream)
																		(prin1 (%structure-ref name structure (svref slot 2)) stream)))))
					(write-string (if readable ")" ">") stream)
				)
				(progn
					(write-string "#<" stream)
					(prin1 name stream)
					(do ((l (%record-length structure))
							 (i 1 (1+ i)))
							((>= i l))
						(write-char #\space stream)
						(prin1 (%structure-ref name structure i) stream)
					)
					(write-string ">" stream)))))
|#
  
