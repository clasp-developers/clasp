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

(_pr "---------Pass Test--------")

(in-package "SYSTEM")

(load "byte")

;(_pr (multiple-value-list (get-setf-method 'q)))

;(_pr (macroexpand '(setf (ldb (byte 1 i) q) 1)))

(setq q 0)

(setf (ldb (byte 1 5) q) 1)

(_pr "------------------")

(_pr q)

;(load "clisp/compiler" :print t)  
;(compile-file "clisp/compiler" :verbose t :print t :listing t)

;(compile-file "clisp/format" :verbose t :print t :listing t)
;(compile-file "foo" :verbose t :print t :listing t)



(exit)  


(defmethod clos::print-condition ((c type-error) stm)
  (format stm "~S is not a ~S" (type-error-datum c) (type-error-expected-type c)))

(defmethod clos::print-condition ((c cell-error) stm)
  (format stm "~S has not dynamic value" (cell-error-name c)))

    
(exit)

#|
	(let ((*default-pathname-defaults* (pathname "S:/SRC/LISP/CODE/TESTS/")))
  (load "tests")
  (run-test "loop")
  ;(run-all-tests)  
  
  )
|#  


