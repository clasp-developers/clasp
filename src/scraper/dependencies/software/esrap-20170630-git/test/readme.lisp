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

(in-suite esrap)

(test-both-modes examples-from-readme.foo
  "README examples related to \"foo+\" rule."
  (is (equal '("foo" nil t)
             (multiple-value-list (parse '(or "foo" "bar") "foo"))))
  (is (eq 'foo+ (add-rule 'foo+
                          (make-instance 'rule :expression '(+ "foo")))))
  (is (equal '(("foo" "foo" "foo") nil t)
             (multiple-value-list (parse 'foo+ "foofoofoo")))))

(test-both-modes examples-from-readme.decimal
  "README examples related to \"decimal\" rule."
  (is (eq 'decimal
          (add-rule
           'decimal
           (make-instance
            'rule
            :expression `(+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
            :transform (lambda (list start end)
                         (declare (ignore start end))
                         (parse-integer (format nil "~{~A~}" list)))))))
  (is (eql 123 (parse '(oddp decimal) "123")))
  (is (equal '(nil 0) (multiple-value-list
                       (parse '(evenp decimal) "123" :junk-allowed t)))))
