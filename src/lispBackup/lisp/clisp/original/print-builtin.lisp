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


(defun _print-character (c s)
  (if (or *print-readably* *print-escape*)
    (let ((r (char-name c)))
      (write-string "#\\" s)
      (if r (write-string r s)
            (write-char c s)))
    (write-char c s))
  c)

  
(defun _print-obj (x stm &rest params)
  (format stm "#<~A " (type-of x))
  (dolist (a params)
    (princ a stm)
    (write-char #\Space stm))
  (format stm "~8,'0X>" (_obj-ptr x))
  x)

(defun _print-stream (x stm)
  (_print-obj x stm))

(defun _print-pathname (x s)
  (write-string "#S" s)
  (write (append (list 'PATHNAME :host (pathname-host x))
                 (if (logical-pathname-p x)
                   (list :logical t)
                   (list :device (pathname-device x)))
                 (list :directory (pathname-directory x)
                       :name      (pathname-name x)
                       :type      (pathname-type x)
                       :version   (pathname-version x)))
         :stream s))

(defun _print-hash-table (x s)
  (write-string "#S(HASH-TABLE " s)
  (write (hash-table-test x) :stream s)
  (maphash #'(lambda (k v)
              (write-string " (" s)
              (write k :stream s)
              (write-string " . " s)
              (write v :stream s)
              (write-char #\) s))
           x)
  (write-char #\) s)
  x)
  
(defun _print-package (x s &aux (n (package-name x)))  
  (cond (*print-readably* (write-string "#." s)
                          (write (list 'find-package n) :stream s))
        (t (print-unreadable-object (x s :type t)
             (write n :stream s :escape nil))))
  x)
  
(defvar *_use-print-object* nil)  

