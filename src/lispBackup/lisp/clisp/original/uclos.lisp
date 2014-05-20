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

(import '(TEXT error-of-type) "CLOS")

(in-package "CLOS")	
      
(defun find-class (symbol &optional (errorp t) environment)
  (declare (ignore environment)) ; what should be the meaning of the environment?
  (unless (symbolp symbol)
    (error-of-type 'type-error
      :datum symbol :expected-type 'symbol
      (TEXT "~S: argument ~S is not a symbol")
      'find-class symbol))
  (let ((class (get symbol 'CLOSCLASS)))
    (if (not (potential-class-p class))
      (if errorp
        (error-of-type 'error
          (TEXT "~S: ~S does not name a class")
          'find-class symbol)
        nil)
      class)))

(defun %shared-initialize (instance slot-names &rest initargs)
 (dolist (slot (class-slots (class-of instance)) instance)
   (let ((slotname (slot-definition-name slot)))
     (multiple-value-bind (init-key init-value foundp)
         (get-properties initargs (slot-definition-initargs slot))
       (declare (ignore init-key))
       (if foundp
           (set-slot-value instance slotname init-value)
         (unless (slot-boundp instance slotname)
           (let ((init (slot-definition-initfunction slot)))
             (when init
               (when (or (eq slot-names 'T)
                         (member slotname slot-names :test #'eq))
                 (set-slot-value instance slotname (funcall init)))))))))))
            

(defun %initialize-instance (instance &rest initargs &key &allow-other-keys)
;  (sys::_pr "%initialize-instance:")
;  (sys::_pr instance)  
   (let ((h (gethash (class-of instance) *make-instance-table*)))
     (if h
        (apply (svref h 3) instance 'T initargs)
      (apply #'initial-initialize-instance instance initargs))))
      
(defun %reinitialize-instance (instance &rest initargs &key &allow-other-keys)
   (let ((h (gethash (class-of instance) *reinitialize-instance-table*)))
     (if h
       (progn
         (sys::keyword-test initargs (car h))
         (apply (cdr h) instance 'NIL initargs))
       (apply #'initial-reinitialize-instance instance initargs))))
      
            
(defun %defclos (std-cv struct-cv builtin-cv defined potential v)
	(setq *CLASS-VERSION-STANDARD-CLASS* std-cv
  			*CLASS-VERSION-STRUCTURE-CLASS* struct-cv
  			*CLASS-BUILT-IN-STANDARD_CLASS* builtin-cv
  			*CLASS-DEFINED-CLASS* defined
  			*CLASS-POTENTIAL-CLASS* potential)
	(let ((vv (vector 'array 'bit-vector 'character 'complex 'cons 'float 'function
          'hash-table 'integer 'list 'null 'package 'pathname
          #+LOGICAL-PATHNAMES 'logical-pathname
;!!!if UCFG_LISP_BUILTIN_RANDOM_STATE 						'random-state
		 'ratio 'readtable
          'stream 'file-stream 'synonym-stream 'broadcast-stream
          'concatenated-stream 'two-way-stream 'echo-stream 'string-stream
          'string 'symbol 't 'vector)))
     (dotimes (i (length vv))
       (setf (get (svref vv i) 'closclass) (svref v i)))))


#|               
       
(let (unbound) (declare (compile)) ; unbound = #<unbound>
	(defun u-def-unbound (x) (declare (compile)) (setq unbound x))
       

	 (defun slot-value-using-class (class object slot-name)
		(no-slot-error class object slot-name))
	 (defun setf-slot-value-using-class (new-value class object slot-name)
		(declare (ignore new-value))
		(no-slot-error class object slot-name))
	 (defun slot-boundp-using-class (class object slot-name)
		(no-slot-error class object slot-name))
	 (defun slot-makunbound-using-class (class object slot-name)
		(no-slot-error class object slot-name))
	 (defun slot-exists-p-using-class (class object slot-name)
		(no-slot-error class object slot-name))

	 (defun no-slot-error (class object slot-name)
		(declare (ignore slot-name))
		(error-of-type 'error
			(TEXT "instance ~S of class ~S has no slots (wrong metaclass)")
			object class))
)

|#
       
	
	
	
