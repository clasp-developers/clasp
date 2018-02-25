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

;;; Utilities

(declaim (ftype (function * (values function &optional))
                resolve-function))

(defun resolve-function (name arguments expression)
  (check-function-reference name expression)
  (cond
    ((member (symbol-package name)
             (load-time-value (mapcar #'find-package '(#:cl #:esrap))))
     (symbol-function name))
    (t
     ;; KLUDGE: Calling via a variable symbol can be slow, but if we
     ;; grab the SYMBOL-FUNCTION here we will not see redefinitions.
     (handler-bind ((style-warning #'muffle-warning))
       (compile nil `(lambda ,arguments (,name ,@arguments)))))))

;;; COMPILING RULES

(defvar *current-rule* nil)

(defun compile-rule (symbol expression condition transform around)
  (declare (type (or boolean function) condition transform around))
  (let* ((*current-rule* symbol)
         ;; Must bind *CURRENT-RULE* before compiling the expression!
         (function (compile-expression expression))
         ;; We use a single static INACTIVE-RULE instance to represent
         ;; (error) results produced by inactive rules. The actual
         ;; error position has to be added in a post-processing step.
         (rule-not-active (make-inactive-rule symbol 0)))
    (cond ((not condition)
           (named-lambda inactive-rule (text position end)
             (declare (ignore text position end))
             rule-not-active))
          (transform
           (locally (declare (type function transform))
             (flet ((exec-rule/transform (text position end)
                      (let ((result (funcall function text position end)))
                        (if (error-result-p result)
                            (make-failed-parse/no-position symbol result)
                            (if around
                                (locally (declare (type function around))
                                  (make-successful-parse
                                   symbol (result-position result)
                                   result (flet ((call-rule ()
                                                   (funcall transform
                                                            (successful-parse-production result)
                                                            position
                                                            (result-position result))))
                                            (funcall around position (result-position result) #'call-rule))))
                                (make-successful-parse
                                 symbol (result-position result)
                                 result (funcall transform
                                                 (successful-parse-production result)
                                                 position
                                                 (result-position result))))))))
               (if (eq t condition)
                   (named-lambda rule/transform (text position end)
                     (with-cached-result (symbol position text)
                       (exec-rule/transform text position end)))
                   (locally (declare (type function condition))
                     (named-lambda condition-rule/transform (text position end)
                       (with-cached-result (symbol position text)
                         (if (funcall condition)
                             (exec-rule/transform text position end)
                             rule-not-active))))))))
          (t
           (if (eq t condition)
               (named-lambda rule (text position end)
                 (with-cached-result (symbol position text)
                   (funcall function text position end)))
               (locally (declare (type function condition))
                 (named-lambda conditional-rule (text position end)
                   (with-cached-result (symbol position text)
                     (if (funcall condition)
                         (funcall function text position end)
                         rule-not-active)))))))))

;;; EXPRESSION COMPILER & EVALUATOR

(defun eval-expression (expression text position end)
  (expression-case expression
    (character
     (eval-character text position end))
    (terminal
     (if (consp expression)
         (eval-terminal (string (second expression)) text position end nil)
         (eval-terminal (string expression) text position end t)))
    (nonterminal
     (eval-nonterminal expression text position end))
    (string
     (eval-string expression text position end))
    (and
     (eval-sequence expression text position end))
    (or
     (eval-ordered-choise expression text position end))
    (not
     (eval-negation expression text position end))
    (*
     (eval-greedy-repetition expression text position end))
    (+
     (eval-greedy-positive-repetition expression text position end))
    (?
     (eval-optional expression text position end))
    (&
     (eval-followed-by expression text position end))
    (!
     (eval-not-followed-by expression text position end))
    (<
     (eval-look-behind expression text position end))
    (>
     (eval-look-ahead expression text position end))
    (character-ranges
     (eval-character-ranges expression text position end))
    (function
     (eval-terminal-function expression text position end))
    (predicate
     (eval-semantic-predicate expression text position end))))

(declaim (ftype (function (*) (values function &optional)) compile-expression))
(defun compile-expression (expression)
  (expression-case expression
    (character        (compile-character))
    (terminal         (if (consp expression)
                          (compile-terminal (string (second expression)) nil)
                          (compile-terminal (string expression) t)))
    (nonterminal      (compile-nonterminal expression))
    (string           (compile-string expression))
    (and              (compile-sequence expression))
    (or               (compile-ordered-choise expression))
    (not              (compile-negation expression))
    (*                (compile-greedy-repetition expression))
    (+                (compile-greedy-positive-repetition expression))
    (?                (compile-optional expression))
    (&                (compile-followed-by expression))
    (!                (compile-not-followed-by expression))
    (<                (compile-look-behind expression))
    (>                (compile-look-ahead expression))
    (character-ranges (compile-character-ranges expression))
    (function         (compile-terminal-function expression))
    (predicate        (compile-semantic-predicate expression))))

(defmacro expression-lambda (name args &body body)
  (unless (length= 3 (parse-ordinary-lambda-list args))
    (error "~@<Lambda-list must have three required arguments.~@:>"))
  (let ((name (symbolicate '#:compiled- name)))
    (destructuring-bind (text-var position-var end-var) args
      `(named-lambda ,name ,args
         (declare (type string ,text-var)
                  (type input-position ,position-var)
                  (type input-length ,end-var))
         ,@body))))

;;; Characters and strings

(declaim (ftype (function (string input-position input-length)
                          (values result &optional))
                eval-character))
(defun eval-character (text position end)
  (if (< position end)
      (%make-successful-parse
       'character (1+ position) nil (list (char text position)))
      (make-failed-parse 'character end nil)))

(defun compile-character ()
  #'eval-character)

(declaim (inline exec-string))
(defun exec-string (expression length text position end)
  (let ((limit (+ length position)))
    (if (<= limit end)
        (make-successful-parse
         expression limit nil (subseq text position limit))
        (make-failed-parse expression end nil))))

(declaim (ftype (function (* string input-position input-length)
                          (values result &optional))
                eval-string))
(defun eval-string (expression text position end)
  (with-expression (expression (string length))
    (declare (type input-position length))
    (exec-string expression length text position end)))

(defun compile-string (expression)
  (with-expression (expression (string length))
    (declare (type input-position length))
    (expression-lambda #:string (text position end)
      (exec-string expression length text position end))))

;;; Terminals

(declaim (inline match-terminal/case-sensitive-p
                 match-terminal/case-insensitive-p
                 match-terminal/1/case-sensitive-p
                 match-terminal/1/case-insensitive-p))

(defun match-terminal/case-sensitive-p (string length text position end)
  (and (<= (+ length position) end)
       (string= string text :start2 position :end2 (+ position length))))

(defun match-terminal/case-insensitive-p (string length text position end)
  (and (<= (+ length position) end)
       (string-equal string text :start2 position :end2 (+ position length))))

(defun match-terminal/1/case-sensitive-p (char text position end)
  (and (< position end) (char= (char text position) char)))

(defun match-terminal/1/case-insensitive-p (char text position end)
  (and (< position end) (char-equal (char text position) char)))

(defun eval-terminal (string text position end case-sensitive-p)
  (let ((length (length string)))
    (if (if case-sensitive-p
            (match-terminal/case-sensitive-p string length text position end)
            (match-terminal/case-insensitive-p string length text position end))
        (make-successful-parse
         string (the input-position (+ length position)) nil string)
        (make-failed-parse string position nil))))

(defun compile-terminal (string case-sensitive-p)
  (macrolet ((with-results ((expression length result) form)
               `(if ,form
                    (%make-successful-parse
                     ,expression (the input-position (+ ,length position))
                     nil ,result)
                    (make-failed-parse ,expression position nil))))
    (let ((length (length string))
          (result (list string)))
      (cond
        ((and (= 1 length) case-sensitive-p)
         (let ((char (char string 0)))
           (expression-lambda #:terminal/1/case-sensitive (text position end)
             (with-results (string 1 result)
               (match-terminal/1/case-sensitive-p
                char text position end)))))
        ((= 1 length)
         (let ((char (char string 0)))
           (expression-lambda #:terminal/1/case-insensitive (text position end)
             (with-results (string 1 result)
               (match-terminal/1/case-insensitive-p
                char text position end)))))
        (case-sensitive-p
         (expression-lambda #:terminal/case-sensitive (text position end)
           (with-results (string length result)
             (match-terminal/case-sensitive-p
              string length text position end))))
        (t
         (expression-lambda #:terminal/case-insensitive (text position end)
           (with-results (string length result)
             (match-terminal/case-insensitive-p
              string length text position end))))))))

(declaim (ftype (function (* function string input-position input-length)
                          (values result &optional))
                exec-terminal-function))
(defun exec-terminal-function (expression function text position end)
  ;; The protocol is as follows:
  ;;
  ;; FUNCTION succeeded if one of
  ;; 1) returns three values and RESULT is T
  ;; 2) returns two values and END-POSITION is NIL
  ;; 3) returns two values and (> END-POSITION POSITION)
  ;; 4) returns one value of type SUCCESSFUL-PARSE
  ;;
  ;; FUNCTION failed if one of
  ;; 1) returns at least two values and (= END-POSITION POSITION)
  ;;    (since no progress has been made), but only if RESULT is not T
  ;; 2) returns three values and RESULT is a string or a condition
  ;; 3) returns one value of type ERROR-RESULT
  ;;
  ;; When RESULT is a string or a condition, END-POSITION can indicate
  ;; the exact position of the failure but is also allowed to be NIL.
  ;;
  ;; RESULT can be T to indicate success even if (= END-POSITION
  ;; POSITION).
  (multiple-value-bind (production end-position result)
      (funcall function text position end)
    (declare (type (or null input-position) end-position)
             (type (or null string condition (eql t)) result))
    (cond
      ((result-p production)
       production)
      ((or (eq result t)
           (and (null result)
                (or (null end-position)
                    (> end-position position))))
       (make-successful-parse expression (or end-position end) nil production))
      (t
       (make-failed-parse expression (or end-position position) result)))))

(defun eval-terminal-function (expression text position end)
  (with-expression (expression (function function))
    (let ((function (ensure-function function)))
      (exec-terminal-function expression function text position end))))

(defun compile-terminal-function (expression)
  (with-expression (expression (function function-name))
    (let ((function (resolve-function
                     function-name '(text position end) expression)))
      (expression-lambda #:terminal-function (text position end)
        (exec-terminal-function expression function text position end)))))

;;; Nonterminals

(defun exec-nonterminal (symbol text position end)
  (let* ((rule (or (find-rule symbol)
                   (undefined-rule symbol)))
         (expression (rule-expression rule))
         (condition (rule-condition rule))
         (around (rule-around rule))
         (transform (rule-transform rule)))
    (with-cached-result (symbol position text)
      (labels ((call-transform (result)
                 (declare (type function transform))
                 (funcall transform
                          (successful-parse-production result)
                          position
                          (result-position result)))
               (exec-rule/transform (result)
                 (cond
                   ((error-result-p result)
                    (make-failed-parse/no-position symbol result))
                   (around
                    (locally (declare (type function around))
                      (make-successful-parse
                       symbol (result-position result)
                       result (funcall around position (result-position result)
                                       (curry #'call-transform result)))))
                   (t
                    (make-successful-parse
                     symbol (result-position result)
                     result (call-transform result)))))
               (exec-expression ()
                 (let ((result (eval-expression expression text position end)))
                   (if transform
                       (exec-rule/transform result)
                       result)))
               (process-condition ()
                 (cond
                   ((not condition)
                    (make-inactive-rule symbol 0))
                   ((or (eq t condition) (funcall (the function condition)))
                    (exec-expression))
                   (t
                    (make-inactive-rule symbol 0)))))
        (declare (dynamic-extent #'exec-rule/transform
                                 #'exec-expression #'process-condition))
        (process-condition)))))

(defun eval-nonterminal (symbol text position end)
  (if *eval-nonterminals*
      (exec-nonterminal symbol text position end)
      (funcall (cell-function (ensure-rule-cell symbol)) text position end)))

(defun compile-nonterminal (symbol)
  (let ((cell (reference-rule-cell symbol *current-rule*)))
    (declare (type rule-cell cell))
    (expression-lambda #:nonterminal (text position end)
      (funcall (cell-function cell) text position end))))

;;; Sequences
;;;
;;; FIXME: It might be better if we actually chained the closures
;;; here, instead of looping over them -- benchmark first, though.

(defun eval-sequence (expression text position end)
  (with-expression (expression (and &rest subexprs))
    (let ((results '()))
      (dolist (expr subexprs
               (make-successful-parse
                expression position (nreverse results)
                #'list-of-result-productions))
        (let ((result (eval-expression expr text position end)))
          (if (error-result-p result)
              (return (make-failed-parse
                       expression position (nreverse (list* result results))))
              (setf position (result-position result)))
          (push result results))))))

(defun compile-sequence (expression)
  (with-expression (expression (and &rest subexprs))
    (let ((functions (mapcar #'compile-expression subexprs)))
      (expression-lambda #:sequence (text position end)
        (let ((results '()))
          (dolist (fun functions
                   (make-successful-parse
                    expression position (nreverse results)
                    #'list-of-result-productions))
            (let ((result (funcall fun text position end)))
              (if (error-result-p result)
                  (return (make-failed-parse
                           expression position
                           (nreverse (list* result results))))
                  (setf position (result-position result)))
              (push result results))))))))

;;; Ordered choises

(declaim (inline make-ordered-choise-result))
(defun make-ordered-choise-result (expression result errors)
  (if errors
      (make-successful-parse
       expression (result-position result)
       (nreverse (list* result errors))
       (successful-parse-production result))
      result))

(defun eval-ordered-choise (expression text position end)
  (with-expression (expression (or &rest subexprs))
    (let ((errors '()))
      (dolist (expr subexprs
               (make-failed-parse/no-position expression (nreverse errors)))
        (let ((result (eval-expression expr text position end)))
          (if (error-result-p result)
              (push result errors)
              (return (make-ordered-choise-result
                       expression result errors))))))))

(defun check-ordered-choise-prefix (string previous-strings)
  ;; Check for "FOO" followed by "FOOBAR" -- the latter would never
  ;; match, but it's an easy mistake to make.
  (not (some (lambda (previous)
               (let ((end (min (length previous) (length string))))
                 (not (or (mismatch string previous :end1 end)
                          (warn "~@<Prefix ~S before ~S in an ESRAP ~A ~
                                 expression.~@:>"
                                previous string 'or)))))
             previous-strings)))

(defun analyze-ordered-choise (sub-expressions)
  (let ((type :characters)
        (canonized '()))
    (dolist (sub sub-expressions)
      (when (and (typep sub '(or character string)))
        (let ((string (string sub)))
          (when (check-ordered-choise-prefix string canonized)
            (push string canonized))))
      (case type
        (:general)
        (:strings
         (unless (typep sub '(or character string))
           (setf type :general)))
        (:characters
         (unless (typep sub '(or character (string 1)))
           (setf type (if (typep sub 'string) :strings :general))))))
    (values type (nreverse canonized))))

(defun compile-ordered-choise (expression)
  (with-expression (expression (or &rest subexprs))
    (multiple-value-bind (type canonized) (analyze-ordered-choise subexprs)
      ;; FIXME: Optimize case-insensitive terminals as well.
      (ecase type
        (:characters
         ;; If every subexpression is a length 1 string, we can represent the whole
         ;; choise with a single string.
         (let ((choises (apply #'concatenate 'string canonized))
               (productions (map 'vector #'list canonized)))
           (declare (type string choises))
           (expression-lambda #:character-choise/characters (text position end)
             (if-let ((index (and (< position end)
                                  (position (char text position) choises))))
               (%make-successful-parse
                expression (+ 1 position) nil (aref productions index))
               (make-failed-parse expression position nil)))))
        (:strings
         ;; If every subexpression is a string, we can represent the whole choise
         ;; with a list of strings.
         (let ((choises (mapcar #'list canonized)))
           (expression-lambda #:character-choise/strings (text position end)
             (dolist (choise choises
                      (make-failed-parse expression position nil))
               (let* ((string (car choise))
                      (len (length string)))
                 (declare (type string string))
                 (when (match-terminal/case-sensitive-p
                        string len text position end)
                   (return
                     (%make-successful-parse
                      expression (the input-position (+ len position))
                      nil choise))))))))
        (:general
         ;; In the general case, compile subexpressions and call.
         (let ((functions (mapcar #'compile-expression subexprs)))
           (expression-lambda #:ordered-choise/general (text position end)
             (let ((errors '()))
               (dolist (fun functions
                        (make-failed-parse/no-position
                         expression (nreverse errors)))
                 (declare (type function fun))
                 (let ((result (funcall fun text position end)))
                   (if (error-result-p result)
                       (push result errors)
                       (return (make-ordered-choise-result
                                expression result errors)))))))))))))

;;; Negations

(declaim (ftype (function (function * string input-position input-position)
                          (values result &optional))
                exec-negation))
(defun exec-negation (fun expr text position end)
  (let ((result))
    (if (and (< position end)
             (error-result-p (setf result (funcall fun text position end))))
        (%make-successful-parse
         expr (1+ position) result (list (char text position)))
        (make-failed-parse expr position result))))

(defun eval-negation (expression text position end)
  (with-expression (expression (not subexpr))
    (flet ((eval-sub (text position end)
             (eval-expression subexpr text position end)))
      (declare (dynamic-extent #'eval-sub))
      (exec-negation #'eval-sub expression text position end))))

(defun compile-negation (expression)
  (with-expression (expression (not subexpr))
    (let ((sub (compile-expression subexpr)))
      (expression-lambda #:negation (text position end)
        (exec-negation sub expression text position end)))))

;;; Greedy repetitions

(defun eval-greedy-repetition (expression text position end)
  (funcall (compile-greedy-repetition expression) text position end))

(defun compile-greedy-repetition (expression)
  (with-expression (expression (* subexpr))
    (let ((function (compile-expression subexpr)))
      (expression-lambda #:greedy-repetition (text position end)
        (let ((last)
              (results '()))
          (loop for result = (funcall function text position end)
             until (error-result-p (setf last result))
             do (setf position (result-position result))
               (push result results))
          (make-successful-parse
           expression position (nreverse (list* last results))
           #'list-of-result-productions/butlast))))))

;;; Greedy positive repetitions

(defun eval-greedy-positive-repetition (expression text position end)
  (funcall (compile-greedy-positive-repetition expression)
           text position end))

(defun compile-greedy-positive-repetition (expression)
  (with-expression (expression (+ subexpr))
    (let ((function (compile-expression subexpr)))
      (expression-lambda #:greedy-positive-repetition (text position end)
        (let* ((last nil)
               (results))
          (loop for result = (funcall function text position end)
             until (error-result-p (setf last result))
             do (setf position (result-position result))
               (push result results))
          (if results
              (make-successful-parse
               expression position (nreverse (list* last results))
               #'list-of-result-productions/butlast)
              (make-failed-parse expression position last)))))))

;;; Optionals

(defun eval-optional (expression text position end)
  (with-expression (expression (? subexpr))
    (let ((result (eval-expression subexpr text position end)))
      (if (error-result-p result)
          (%make-successful-parse expression position result '(nil))
          result))))

(defun compile-optional (expression)
  (with-expression (expression (? subexpr))
    (let ((function (compile-expression subexpr)))
      (expression-lambda #:optional (text position end)
        (let ((result (funcall function text position end)))
          (if (error-result-p result)
              (%make-successful-parse expression position result '(nil))
              result))))))

;;; Followed-by's

(defun eval-followed-by (expression text position end)
  (with-expression (expression (& subexpr))
    (let ((result (eval-expression subexpr text position end)))
      (if (error-result-p result)
          (make-failed-parse expression position result)
          (make-successful-parse
           expression position result #'successful-parse-production)))))

(defun compile-followed-by (expression)
  (with-expression (expression (& subexpr))
    (let ((function (compile-expression subexpr)))
      (expression-lambda #:followed-by (text position end)
        (let ((result (funcall function text position end)))
          (if (error-result-p result)
              (make-failed-parse expression position result)
              (make-successful-parse
               expression position result #'successful-parse-production)))))))

;;; Not followed-by's

(defun eval-not-followed-by (expression text position end)
  (with-expression (expression (! subexpr))
    (let ((result (eval-expression subexpr text position end)))
      (if (error-result-p result)
          (%make-successful-parse expression position result '(nil))
          (make-failed-parse expression position result)))))

(defun compile-not-followed-by (expression)
  (with-expression (expression (! subexpr))
    (let ((function (compile-expression subexpr)))
      (expression-lambda #:not-followed-by (text position end)
        (let ((result (funcall function text position end)))
          (if (error-result-p result)
              (%make-successful-parse expression position result '(nil))
              (make-failed-parse expression position result)))))))

;;; Look{ahead,behind}

(macrolet
    ((define-look (direction operator look-position test)
       (let ((eval-name (symbolicate '#:eval-look- direction))
             (compile-name (symbolicate '#:compile-look- direction))
             (lambda-name (symbolicate '#:compiled-look- direction)))
         `(progn
            (defun ,eval-name (expression text position end)
              (with-expression (expression (,operator n subexpr))
                (declare (type input-position n))
                (let* ((look-position ,look-position)
                       (result
                         (when ,test
                           (eval-expression subexpr text look-position end))))
                  (if (or (not result) (error-result-p result))
                      (make-failed-parse expression position result)
                      (make-successful-parse
                       expression position result #'successful-parse-production)))))

            (defun ,compile-name (expression)
              (with-expression (expression (,operator n subexpr))
                (declare (type input-position n))
                (let ((function (compile-expression subexpr)))
                  (expression-lambda ,lambda-name (text position end)
                    (let* ((look-position ,look-position)
                           (result
                             (when ,test
                               (funcall function text look-position end))))
                      (if (or (not result) (error-result-p result))
                          (make-failed-parse expression position result)
                          (make-successful-parse
                           expression position result #'successful-parse-production)))))))))))

  (define-look :behind < (- position n) (>= look-position 0))
  (define-look :ahead  > (+ position n) (<= look-position end)))

;;; Semantic predicates

(defun eval-semantic-predicate (expression text position end)
  (with-expression (expression ((t predicate-name) subexpr))
    (let ((result (eval-expression subexpr text position end)))
      (if (error-result-p result)
          (make-failed-parse expression position result)
          (let ((production (successful-parse-production result)))
            (if (funcall (symbol-function predicate-name) production)
                result
                (make-failed-parse expression position result)))))))

(defun compile-semantic-predicate (expression)
  (with-expression (expression ((t predicate-name) subexpr))
    (let* ((function (compile-expression subexpr))
           (predicate (resolve-function
                       predicate-name '(production) expression)))
      (expression-lambda #:semantic-predicate (text position end)
        (let ((result (funcall function text position end)))
          (if (error-result-p result)
              (make-failed-parse expression position result)
              (let ((production (successful-parse-production result)))
                (if (funcall predicate production)
                    result
                    (make-failed-parse expression position result)))))))))

;;; Character ranges

(declaim (ftype (function (* * string input-position input-length)
                          (values result &optional))
                exec-character-ranges))
(defun exec-character-ranges (expression ranges text position end)
  (flet ((oops ()
           (make-failed-parse expression position nil)))
    (if (< position end)
        (let ((char (char text position)))
          (if (loop for range in ranges
                 do (if (characterp range)
                        (when (char= range char)
                          (return t))
                        (when (char<= (first range) char (second range))
                          (return t))))
              (make-successful-parse expression (1+ position) nil char)
              (oops)))
        (oops))))

(defun eval-character-ranges (expression text position end)
  (with-expression (expression (character-ranges &rest ranges))
    (exec-character-ranges expression ranges text position end)))

(defun compile-character-ranges (expression)
  (with-expression (expression (character-ranges &rest ranges))
    (expression-lambda #:character-ranges (text position end)
      (exec-character-ranges expression ranges text position end))))
