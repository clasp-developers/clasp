;;;; Esrap example: a simple S-expression grammar

(cl:require :esrap)

(cl:defpackage #:sexp-grammar
  (:use #:cl #:esrap))

(cl:in-package #:sexp-grammar)

;;; A semantic predicate for filtering out double quotes.

(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-integer (string)
  (when (find-if-not #'digit-char-p string)
    t))

;;; Utility rules.

(defrule whitespace (+ (or #\space #\tab #\newline))
  (:constant nil))

(defrule alphanumeric (alphanumericp character))

(defrule string-char (or (not-doublequote character) (and #\\ #\")))

;;; Here we go: an S-expression is either a list or an atom, with possibly leading whitespace.

(defrule sexp (and (? whitespace) (or magic list atom))
  (:function second)
  (:lambda (s &bounds start end)
    (list s (cons start end))))

(defrule magic "foobar"
  (:constant :magic)
  (:when (eq * :use-magic)))

(defrule list (and #\( sexp (* sexp) (? whitespace) #\))
  (:destructure (p1 car cdr w p2)
    (declare (ignore p1 p2 w))
    (cons car cdr)))

(defrule atom (or string integer symbol))

(defrule string (and #\" (* string-char) #\")
  (:destructure (q1 string q2)
    (declare (ignore q1 q2))
    (text string)))

(defrule integer (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (:lambda (list)
    (parse-integer (text list) :radix 10)))

(defrule symbol (not-integer (+ alphanumeric))
  ;; NOT-INTEGER is not strictly needed because ATOM considers INTEGER before
  ;; a STRING, we know can accept all sequences of alphanumerics -- we already
  ;; know it isn't an integer.
  (:lambda (list)
    (intern (text list))))

;;;; Try these

(parse 'sexp "FOO123")

(parse 'sexp "123")

(parse 'sexp "\"foo\"")

(parse 'sexp "  (  1 2  3 (FOO\"foo\"123 )   )")

(parse 'sexp "foobar")

(let ((* :use-magic))
  (parse 'sexp "foobar"))

(describe-grammar 'sexp)

(trace-rule 'sexp :recursive t)

(parse 'sexp "(foo bar 1 quux)")

(untrace-rule 'sexp :recursive t)

(defparameter *orig* (rule-expression (find-rule 'sexp)))

(change-rule 'sexp '(and (? whitespace) (or list symbol)))

(parse 'sexp "(foo bar quux)")

(parse 'sexp "(foo bar 1 quux)" :junk-allowed t)

(change-rule 'sexp *orig*)

(parse 'sexp "(foo bar 1 quux)" :junk-allowed t)
