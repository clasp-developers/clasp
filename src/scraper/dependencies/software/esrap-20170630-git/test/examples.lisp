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

(test-both-modes example-left-recursion.left-associative
  "Left associate grammar from example-left-recursion.lisp."
  ;; This grammar should work without left recursion.
  (let ((*on-left-recursion* :error))
    (is (equal '(+ (* 1 2) (+ (* 3 4) 5))
               (parse 'left-recursive-grammars:la-expr "1*2+3*4+5")))))

(test-both-modes example-left-recursion.right-associative
  "Right associate grammar from example-left-recursion.lisp."
  ;; This grammar combination of grammar and input would require left
  ;; recursion.
  (let ((*on-left-recursion* :error))
    (signals left-recursion
      (parse 'left-recursive-grammars:ra-expr "1*2+3*4+5")))

  (is (equal '(+ (+ (* 1 2) (* 3 4)) 5)
             (parse 'left-recursive-grammars:ra-expr "1*2+3*4+5"))))

(test-both-modes example-left-recursion.warth.smoke
  "Warth's Java expression example from example-left-recursion.lisp."
  (mapc
   (curry #'apply
          (lambda (input expected)
            (is (equal expected
                       (parse 'left-recursive-grammars:primary input)))))
   '(("this"       "this")
     ("this.x"     (:field-access "this" "x"))
     ("this.x.y"   (:field-access (:field-access "this" "x") "y"))
     ("this.x.m()" (:method-invocation (:field-access "this" "x") "m"))
     ("x[i][j].y"  (:field-access (:array-access (:array-access "x" "i") "j") "y")))))

(test-both-modes example-function-terminals.indented-block.smoke
  "Context-sensitive parsing via function terminals."
  (is (equal '("foo" "bar" "quux"
               (if "foo"
                   ("bla"
                    (if "baz"
                        ("bli" "blo")
                        ("whoop"))))
               "blu")
             (parse 'esrap-example.function-terminals:indented-block
                    "   foo
   bar
   quux
   if foo:
    bla
    if baz:
       bli
       blo
    else:
     whoop
   blu
"))))

(test-both-modes example-function-terminals.indented-block.condition
  "Context-sensitive parsing via function terminals."
  (let ((input "if foo:
bla
"))
    (signals-esrap-error (input esrap-parse-error 0
                                ("In context INDENTED-BLOCK:"
                                 "While parsing INDENTED-BLOCK."
                                 "Problem:" "Expected indent"
                                 "Expected:"
                                 "a string that can be parsed by the function"))
      (parse 'esrap-example.function-terminals:indented-block input))))

(test-both-modes example-function-terminals.read.smoke
  "Using CL:READ as a terminal."
  (macrolet ((test-case (input expected)
               `(is (equal ,expected
                           (with-standard-io-syntax
                             (parse 'esrap-example.function-terminals:common-lisp
                                    ,input))))))
    (test-case "(1 2 3)" '(1 2 3))
    (test-case "foo" 'cl-user::foo)
    (test-case "#C(1 3/4)" #C(1 3/4))))

(test-both-modes example-function-terminals.read.condition
  "Test error reporting in the CL:READ-based rule"
  (handler-case
      (with-standard-io-syntax
        (parse 'esrap-example.function-terminals:common-lisp
               "(list 'i :::love 'lisp"))
    (esrap-parse-error (condition)
      #-sbcl (declare (ignore condition))
      ;; Different readers may report this differently.
      #+sbcl (is (<= 9 (esrap-error-position condition) 16))
      ;; Not sure how other lisps report this.
      #+sbcl (is (search "too many colons"
                         (princ-to-string condition))))))
