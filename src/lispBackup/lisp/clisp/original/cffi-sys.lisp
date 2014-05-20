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

(in-package "CFFI-SYS")

(export '(foreign-pointer pointerp make-pointer null-pointer null-pointer-p inc-pointer
          canonicalize-symbol-name-case
          %foreign-alloc foreign-free
          %foreign-type-size %foreign-type-alignment %mem-ref %mem-set
          %foreign-funcall %foreign-funcall-pointer %foreign-symbol-pointer
          %load-foreign-library %close-foreign-library
          foreign-library-handle get-foreign-library
          ))

(defun pointer-eq (ptr1 ptr2)
  (eql ptr1 ptr2))


(defun pointerp (ptr)
  (eq (type-of ptr) 'foreign-pointer))

(defun null-pointer ()
  (make-pointer 0))

(defun null-pointer-p (ptr)
  (zerop (pointer-address ptr)))

(defun inc-pointer (ptr offset)
  (make-pointer (+ offset (pointer-address ptr))))

(defun canonicalize-symbol-name-case (name)
  (string-upcase name))

(defun native-namestring (pathname)
  (namestring pathname))

(defmacro %foreign-funcall-pointer (ptr args &key library calling-convention)
  `(_%foreign-funcall ,ptr nil ,calling-convention ,@args))

(defmacro %foreign-funcall (name args &key library calling-convention)
  `(_%foreign-funcall ,name (foreign-library-handle (get-foreign-library ',library)) ,calling-convention ,@args))


(defun register-callback (name function parsed-type)
  (setf (get name 'callback) (list function (%create-callback name parsed-type))))

(defun %callback (name)
  (let ((v (get name 'callback)))
    (if v (second v)
          (error "Undefined callback: ~S" name))))



