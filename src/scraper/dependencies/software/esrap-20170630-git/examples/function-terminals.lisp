;;;; Esrap example: some grammars with function-based terminals.

(cl:require :esrap)

(cl:defpackage #:esrap-example.function-terminals
  (:use #:cl #:esrap)
  (:export #:indented-block #:common-lisp))

(cl:in-package #:esrap-example.function-terminals)

;;; Ex. 1. Using a custom terminal for context sensitive parsing.
;;;
;;; Among many other things, this can be used to implement
;;; indentation-based grammars such as Python's.

(defrule whitespace (+ #\space)
  (:constant nil))

;; *CURRENT-INDENT* tracks the current indentation and CURRENT-INDENT
;; *succeeds when it can consume exactly CURRENT-INDENT* units of
;; *indentation.
(defvar *current-indent* 0)

(defun current-indent-p (indent)
  (= indent *current-indent*))

(defrule indent (* #\space)
  (:function length))

(defrule current-indent
    (current-indent-p indent))

;; Just a dummy rule for the statement-like elements of the
;; grammar. This is not the focus of this example. For simplicity,
;; each statement is on one line.
(defrule statement (+ (character-ranges (#\a #\z)))
  (:text t))

(defrule line (and statement #\newline)
  (:function first))

(defrule block-content
    (or if line))

(defrule indented-block-content
    (and current-indent block-content)
  (:function second))

;; PARSE-INDENTED-BLOCK is the real meat. It determines the new
;; indentation depth via a nested (PARSE INDENT ...) call which does
;; not consume input. The block's content can then be parsed with a
;; suitably increased current indent.
;;
;; The result of the second PARSE call is returned "raw" in case of
;; success. This allows the associated result tree to be attached to
;; the global result tree and permits lazy computation of rule
;; productions within the sub-tree (beneficial if e.g. the result of
;; the parse, despite successful, is not used in the global result).
(defun parse-indented-block (text position end)
  (multiple-value-bind (new-indent new-position)
      (parse 'indent text :start position :end end
                          :junk-allowed t)
    (if (> new-indent *current-indent*)
        (let ((*current-indent* new-indent))
          (parse '(+ indented-block-content) text
                 :start position :end end :raw t))
        (values nil new-position "Expected indent"))))

(defrule indented-block #'parse-indented-block)

(defrule if
    (and (and "if" whitespace) statement (and #\: #\Newline)
         indented-block
         (? (and (and current-indent "else" #\: #\Newline)
                 indented-block)))
  (:destructure (if-keyword condition colon then
                 (&optional else-keyword else))
    (declare (ignore if-keyword colon else-keyword))
    (list* 'if condition then (when else (list else)))))

(defun test-indentation ()
  (parse 'indented-block
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
"))

;;; Ex. 2. Using CL:READ to parse lisp.

(defun parse-using-read (text position end)
  (handler-case
      ;; When successful, READ-FROM-STRING returns the read object and
      ;; the position up to which TEXT has been consumed.
      (read-from-string text t nil :start position :end end)
    ;; When READ-FROM-STRING fails, indicate the parse failure,
    ;; including CONDITION as explanation.
    (stream-error (condition)
      ;; For STREAM-ERRORs, we can try to determine and return the
      ;; exact position of the failure.
      (let ((position (ignore-errors
                       (file-position (stream-error-stream condition)))))
        (values nil position condition)))
    (error (condition)
      ;; For general ERRORs, we cannot determine the exact position of
      ;; the failure.
      (values nil nil condition))))

(defrule common-lisp #'parse-using-read)

;; When parsing anything by using CL:READ, it is probably a good idea
;; to disable *READ-EVAL*. The package in which symbols will be
;; interned has to be kept in mind as well.
(defun test-read ()
  (with-standard-io-syntax
    (let (; (*package* (find-package :my-package-for-symbols))
          (*read-eval* nil))
      ;; This contains deliberate syntax errors to highlight the error
      ;; position and error message reporting implemented in
      ;; PARSE-USING-READ.
      (parse 'common-lisp "(list 'i :::love 'lisp"))))
