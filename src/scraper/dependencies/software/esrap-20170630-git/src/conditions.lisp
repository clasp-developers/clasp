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

(define-condition invalid-expression-error (error)
  ((expression :initarg :expression :reader invalid-expression-error-expression))
  (:default-initargs
   :expression (required-argument :expression))
  (:documentation
   "Signaled when an invalid expression is encountered."))

(defmethod print-object ((condition invalid-expression-error) stream)
  (format stream "Invalid expression: ~S"
          (invalid-expression-error-expression condition)))

(defun invalid-expression-error (expression)
  (error 'invalid-expression-error :expression expression))

(define-condition esrap-error (parse-error)
  ((text :initarg :text :initform nil :reader esrap-error-text))
  (:documentation
   "Signaled when an Esrap parse fails. Use ESRAP-ERROR-TEXT to obtain the
string that was being parsed, and ESRAP-ERROR-POSITION the position at which
the error occurred."))

(defmethod print-object :before ((condition esrap-error) stream)
  (when (or *print-escape*
            *print-readably*
            (and *print-lines* (<= *print-lines* 5)))
    (return-from print-object))

  ;; FIXME: this looks like it won't do the right thing when used as
  ;; part of a logical block.
  (if-let ((text (esrap-error-text condition))
           (position (esrap-error-position condition)))
    (labels ((safe-index (index)
               (min (max index 0) (length text)))
             (find-newline (&key (start 0) (end (length text)) (from-end t))
               (let ((start (safe-index start))
                     (end   (safe-index end)))
                 (cond
                   ((when-let ((position (position #\Newline text
                                                   :start start :end end
                                                   :from-end from-end)))
                      (1+ position)))
                   ((and from-end (zerop start))
                    start)
                   ((and (not from-end) (= end (length text)))
                    end)))))
      ;; FIXME: magic numbers
      (let* ((line       (count #\Newline text :end position))
             (column     (- position (or (find-newline :end position) 0) 1))
             (min-start  (- position 160))
             (max-end    (+ position 24))
             (line-start (or (find-newline :start min-start
                                           :end   position)
                             (safe-index min-start)))
             (start      (cond
                           ((= (safe-index min-start) line-start)
                            line-start)
                           ((find-newline :start min-start
                                          :end   (1- line-start)))
                           (t

                            line-start)))
             (end        (or (find-newline :start    position
                                           :end      max-end
                                           :from-end nil)
                             (safe-index max-end)))
             (*print-circle* nil))
        (format stream "At~:[~; end of input~]~2%~
                        ~2@T~<~@;~A~:>~%~
                        ~2@T~V@T^ (Line ~D, Column ~D, Position ~D)~2%"
                (= position (length text))
                (list (subseq text start end))
                (- position line-start)
                (1+ line) (1+ column) position)))

    (format stream "~2&<text and position not available>~2%")))

(define-condition esrap-parse-error (esrap-error)
  ((result :initarg :result
           :type    result
           :reader  esrap-parse-error-result)
   (%context :accessor esrap-parse-error-%context
             :initform nil))
  (:default-initargs :result (required-argument :result))
  (:documentation
   "This error is signaled when a parse attempt fails in a way that ."))

(defmethod esrap-error-position ((condition esrap-parse-error))
  (result-position (esrap-parse-error-context condition)))

(defmethod esrap-parse-error-context ((condition esrap-parse-error))
  (or (esrap-parse-error-%context condition)
      (setf (esrap-parse-error-%context condition)
            (let ((result (esrap-parse-error-result condition)))
              (or (result-context result) result)))))

(defmethod print-object ((object esrap-parse-error) stream)
  (cond
    (*print-readably*
     (call-next-method))
    (*print-escape*
     (print-unreadable-object (object stream :type t :identity t)
       (format stream "~@[~S~]~@[ @~D~]"
               (esrap-parse-error-context object)
               (esrap-error-position object))))
    (t
     (error-report (esrap-parse-error-context object) stream))))

(declaim (ftype (function (string result) (values &optional nil))
                esrap-parse-error))
(defun esrap-parse-error (text result)
  (error 'esrap-parse-error
         :text   text
         :result result))

(define-condition left-recursion (esrap-error)
  ((position :initarg :position :initform nil :reader esrap-error-position)
   (nonterminal :initarg :nonterminal :initform nil :reader left-recursion-nonterminal)
   (path :initarg :path :initform nil :reader left-recursion-path))
  (:documentation
   "May be signaled when left recursion is detected during Esrap parsing.

LEFT-RECURSION-NONTERMINAL names the symbol for which left recursion
was detected, and LEFT-RECURSION-PATH lists nonterminals of which the
left recursion cycle consists.

Note: This error is only signaled if *ON-LEFT-RECURSION* is bound
to :ERROR."))

(defmethod print-object ((condition left-recursion) stream)
  (format stream "Left recursion in nonterminal ~S. ~_Path: ~
                  ~{~S~^ -> ~}"
          (left-recursion-nonterminal condition)
          (left-recursion-path condition)))

(defun left-recursion (text position nonterminal path-butlast)
  (error 'left-recursion
         :text text
         :position position
         :nonterminal nonterminal
         :path (append path-butlast (list nonterminal))))

(define-condition undefined-rule (condition)
  ((symbol :initarg :symbol
           :type symbol
           :reader undefined-rule-symbol)))

(defmethod print-object ((condition undefined-rule) stream)
  (format stream "~@<The rule ~S is undefined.~@:>"
          (undefined-rule-symbol condition)))

(define-condition undefined-rule-error (undefined-rule error)
  ()
  (:documentation
   "Signaled when an undefined rule is encountered."))

(defun undefined-rule (symbol)
  (error 'undefined-rule-error :symbol symbol))
