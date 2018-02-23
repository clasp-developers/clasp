;;;; Copyright (c) 2007-2013 Nikodemus Siivola <nikodemus@random-state.net>
;;;; Copyright (c) 2012-2017 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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

;;; Input types

(deftype input-position ()
  'array-index)

(deftype input-length ()
  'array-length)

;;; Parser behavior types

(deftype left-recursion-policy ()
  '(or null (eql :error)))

;;; Expression types

(deftype nonterminal ()
  "Any symbol except CHARACTER and NIL can be used as a nonterminal symbol."
  '(and symbol (not (member character nil))))

(deftype terminal ()
  "Literal strings and characters are used as case-sensitive terminal symbols,
and expressions of the form \(~ <literal>) denote case-insensitive terminals."
  '(or string character
       (cons (eql ~) (cons (or string character) null))))

(deftype character-range ()
  "A character range is either a single character or a list of two
characters."
  '(or character
       (cons character (cons character null))))

(deftype predicate-name ()
  '(and symbol
        (not (member character-ranges string
                     and or not
                     * + ? & ! ~ < >
                     function))))

(deftype predicate ()
  '(cons predicate-name (cons (not null) null)))

;;; Rule-related types

(deftype error-report-part ()
  "Named part of a parse error report."
  `(member :context :detail))

(deftype rule-error-report ()
  "Suitability of a rule for error report parts.

In addition to the ERROR-REPORT-PART values, NIL indicates
unsuitability for all error report parts, while T indicates
suitability for all parts."
  '(or (member t nil) error-report-part))

(deftype rule-error-report-pattern ()
  "ERROR-REPORT-PART or a list thereof."
  '(or (member t nil) error-report-part (cons error-report-part)))

(declaim (ftype (function (rule-error-report rule-error-report-pattern)
                          (values boolean &optional))
                error-report-behavior-suitable-for-report-part-p))
(defun error-report-behavior-suitable-for-report-part-p
    (query part-or-parts)
  "Return true if QUERY is suitable for PART-OR-PARTS."
  (when (or (eq query part-or-parts)
            (eq query t)
            (and (consp part-or-parts)
                 (member query part-or-parts :test #'eq)))
    t))
