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

(cl:in-package #:esrap-tests)

(defmacro with-silent-compilation-unit (() &body body)
  `(let ((*error-output* (make-broadcast-stream)))
     (with-compilation-unit (:override t)
       ,@body)))

(defun call-expecting-signals-esrap-error (thunk input condition position
                                           &optional messages)
  (ecase condition
    (esrap-parse-error
     (signals (esrap-parse-error) (funcall thunk))))
  (handler-case (funcall thunk)
    (esrap-error (condition)
      (is (string= (esrap-error-text condition) input))
      (when position
        (is (= (esrap-error-position condition) position)))
      (let ((report (with-standard-io-syntax
                      (let ((*print-pretty* t))
                        (with-output-to-string (stream)
                          (pprint-logical-block (stream nil)
                            (princ condition stream))))))
            (start 0))
        (mapc (lambda (message)
                (let ((position (search message report :start2 start)))
                  (is (integerp position)
                      "~@<The string ~S does not occur in ~S after ~
                       position ~D.~@:>"
                      message report start)
                  (when position
                    (setf start position))))
              messages)))))

(defmacro signals-esrap-error ((input condition position &optional messages)
                               &body body)
  `(call-expecting-signals-esrap-error
    (lambda () ,@body) ,input
    ',condition ,position (list ,@(ensure-list messages))))

(defmacro test-both-modes (name &body body)
  (multiple-value-bind (body declarations documentation)
      (parse-body body :documentation t)
    (declare (ignore declarations))
    (let ((name/interpreted (symbolicate name '#:.interpreted))
          (name/compiled    (symbolicate name '#:.compiled)))
      `(progn
         (test ,name/interpreted
               ,@(when documentation `(,documentation))
               (let ((esrap::*eval-nonterminals* t))
                 (#-sbcl progn #+sbcl locally
                  #+sbcl (declare (sb-ext:disable-package-locks esrap:parse))
                  (flet ((parse (&rest args)
                           (apply #'parse args)))
                    ,@body))))
         (test ,name/compiled
               ,@(when documentation `(,documentation))
               ,@body)))))
