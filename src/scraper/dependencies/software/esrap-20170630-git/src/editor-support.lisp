;;;; Copyright (c) 2007-2013 Nikodemus Siivola <nikodemus@random-state.net>
;;;; Copyright (c) 2012-2016 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(cl:in-package #:esrap)

(defvar *indentation-hint-table* nil)

(defun hint-slime-indentation ()
  ;; See https://github.com/nikodemus/esrap/issues/24.
  (unless (member "SWANK-INDENTATION" *modules* :test #'string=)
    (return-from hint-slime-indentation))
  (when-let* ((swank (find-package :swank))
              (tables (find-symbol (string '#:*application-hints-tables*) swank))
              (table (make-hash-table :test #'eq)))
    (setf (gethash 'defrule table)
          '(4 4 &rest (&whole 2 &lambda &body)))
    (set tables (cons table (remove *indentation-hint-table* (symbol-value tables))))
    (setf *indentation-hint-table* table)
    t))

(hint-slime-indentation)
