;;;; Esrap example: some grammars with left-recursive rules.

(cl:require :esrap)

(cl:defpackage #:left-recursive-grammars
  (:use #:cl #:alexandria #:esrap)
  (:export #:la-expr #:ra-expr #:primary))

(cl:in-package :left-recursive-grammars)

;;; Left associative expressions

(defrule la-expr
    la-term)

(defrule la-literal
    (digit-char-p character)
  (:lambda (x) (parse-integer (text x))))

(defrule la-term
    (and la-factor (? (and (or #\+ #\-) la-term)))
  (:destructure (left (&optional op right))
    (if op
        (list (find-symbol op :cl) left right)
        left)))

(defrule la-factor
    (and (or la-literal la-expr) (? (and (or #\* #\/) la-factor)))
  (:destructure (left (&optional op right))
    (if op
        (list (find-symbol op :cl) left right)
        left)))

(defun test-la ()
  (let ((*on-left-recursion* :error))
    (assert (equal (parse 'la-expr "1*2+3*4+5")
                   '(+ (* 1 2) (+ (* 3 4) 5))))))

;;; Right associative expressions

(defrule ra-expr
    ra-term)

(defrule ra-literal
    (digit-char-p character)
  (:lambda (x) (parse-integer (text x))))

(defrule ra-term
    (and (? (and ra-term (or #\+ #\-))) ra-factor)
  (:destructure ((&optional left op) right)
    (if op
        (list (find-symbol op :cl) left right)
        right)))

(defrule ra-factor
    (and (? (and ra-factor (or #\* #\/))) (or ra-literal ra-expr))
  (:destructure ((&optional left op) right)
    (if op
        (list (find-symbol op :cl) left right)
        right)))

(defun test-ra ()
  (let ((*on-left-recursion* :error))
    (parse 'ra-expr "1*2+3*4+5")) ; |- Error

  (assert (equal (parse 'ra-expr "1*2+3*4+5")
                 '(+ (+ (* 1 2) (* 3 4)) 5))))


;;; The following example is given in
;;;
;;;   Alessandro Warth, James R. Douglass, Todd Millstein, 2008,
;;;   "Packrat Parsers Can Support Left Recursion".
;;;   http://www.vpri.org/pdf/tr2007002_packrat.pdf

(defrule primary
    primary-no-new-array)

(defrule primary-no-new-array
    (or class-instance-creation-expression
        method-invocation
        field-access
        array-access
        "this"))

(defrule class-instance-creation-expression
    (or (and "new" class-or-interface-type "()")
        (and primary ".new" identifier "()")))

;; Note: in the paper, the first case is
;;
;;   (and primary "." identifier "()")
;;
;; but that seems to be an error.
(defrule method-invocation
    (or (and primary "." method-name "()")
        (and (and) (and) method-name "()"))
  (:destructure (structure dot name parens)
    (declare (ignore dot parens))
    (list :method-invocation structure name)))

(defrule field-access
    (or (and primary "." identifier)
        (and "super." identifier))
  (:destructure (structure dot field)
    (declare (ignore dot))
    (list :field-access structure field)))

(defrule array-access
    (or (and primary "[" expression "]")
        (and expression-name "[" expression "]"))
  (:destructure (structure open index close)
    (declare (ignore open close))
    (list :array-access structure index)))

(defrule class-or-interface-type
    (or class-name interface-type-name))

(defrule class-name
    (or "C" "D"))

(defrule interface-type-name
    (or "I" "J"))

(defrule identifier
    (or "x" "y" class-or-interface-type))

(defrule method-name
    (or "m" "n"))

(defrule expression-name
    identifier)

(defrule expression
    (or "i" "j"))

(defun test-warth ()
  (mapc
   (curry #'apply
          (lambda (input expected)
            (assert (equal (parse 'primary input) expected))))
   '(("this"       "this")
     ("this.x"     (:field-access "this" "x"))
     ("this.x.y"   (:field-access (:field-access "this" "x") "y"))
     ("this.x.m()" (:method-invocation (:field-access "this" "x") "m"))
     ("x[i][j].y"  (:field-access (:array-access (:array-access "x" "i") "j") "y")))))
