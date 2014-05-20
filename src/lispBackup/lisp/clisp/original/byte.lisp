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

(defun byte (size position)
  (cons size position))

(defun byte-size (b)
  (car b))
  
(defun byte-position (b)
  (cdr b))
  
(defun dpb (n b i)
  (deposit-field (ash n (byte-position b)) b i))
         
(define-setf-expander LDB (bytespec integer) ;!!!  &environment env
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method integer) ;!!! env
    (let* ((bytespecvar (gensym))
           (storevar (gensym)))
      (values (cons bytespecvar SM1)
              (cons bytespec SM2)
              `(,storevar)
              `(LET ((,(first SM3) (DPB ,storevar ,bytespecvar ,SM5)))
                 ,SM4
                 ,storevar
               )
              `(LDB ,bytespecvar ,SM5)
) ) ) )
         
(define-setf-expander MASK-FIELD (bytespec integer) ;!!!  &environment env
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method integer) ;!!! env
    (let* ((bytespecvar (gensym))
           (storevar (gensym)))
      (values (cons bytespecvar SM1)
              (cons bytespec SM2)
              `(,storevar)
              `(LET ((,(first SM3) (DEPOSIT-FIELD ,storevar ,bytespecvar ,SM5)))
                 ,SM4
                 ,storevar
               )
              `(MASK-FIELD ,bytespecvar ,SM5)
) ) ) )


(defun ldb-test (bs n)
	(not (zerop (ldb bs n))))
  

