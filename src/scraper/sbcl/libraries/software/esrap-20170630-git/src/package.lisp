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

(cl:defpackage #:esrap
  (:use #:cl #:alexandria)
  #+sbcl
  (:lock t)
  (:export
   #:&bounds

   #:! #:? #:+ #:* #:& #:~
   #:character-ranges

   #:*on-left-recursion*

   #:add-rule
   #:call-transform
   #:change-rule
   #:defrule
   #:describe-grammar
   #:describe-terminal
   #:esrap-error
   #:esrap-error-position
   #:esrap-error-text
   #:esrap-parse-error
   #:esrap-parse-error-result
   #:esrap-parse-error-context
   #:expression-start-terminals
   #:find-rule
   #:invalid-expression-error
   #:invalid-expression-error-expression
   #:left-recursion
   #:left-recursion-nonterminal
   #:left-recursion-path
   #:parse
   #:remove-rule
   #:rule
   #:rule-dependencies
   #:rule-expression
   #:rule-symbol
   #:text
   #:trace-rule
   #:untrace-rule
   #:undefined-rule-error
   #:undefined-rule-symbol
   ))
