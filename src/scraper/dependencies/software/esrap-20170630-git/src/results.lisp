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

;;;; We always return a RESULT -- ERROR-RESULT for failed parses, and
;;;; SUCCESSFUL-PARSE for successes.
;;;;
;;;; We implement a simple lazy evaluation for the productions. This
;;;; is used to perform semantic actions only when necessary -- either
;;;; when we call a semantic predicate or once parse has finished.

(cl:in-package #:esrap)

(defstruct (result (:constructor nil) (:copier nil))
  ;; Expression that succeeded/failed to match.
  (expression nil                                                                :read-only t)
  ;; Position at which match was attempted.
  ;; Either
  ;; * the position at which the parse failed
  ;; * or function returning that position when called with the
  ;;   FAILED-PARSE instance and optionally a minimum position as its
  ;;   arguments.
  (%position  #'max-of-result-positions  :type (or function input-position))
  ;; One of the following things:
  ;; * nested error, closer to actual failure site
  ;; * a (possibly empty) list thereof
  ;; * a string describing the failure
  ;; * a condition instance describing the failure
  (detail     nil                        :type (or result list string condition) :read-only t))

;; The following function is only called from slow paths.
(declaim (ftype (function (result) (values input-position &optional))
                result-position))
(defun result-position (result)
  (let ((position (result-%position result)))
    (if (functionp position)
        (setf (result-%position result)
              (funcall position (ensure-list (result-detail result))))
        position)))

(defmethod print-object ((object result) stream)
  (print-unreadable-object (object stream :type t)
    (let ((*print-level* (min 2 (or *print-level* 2)))
          (*print-length* (min 3 (or *print-length* 3))))
      (format stream "~S~@[ @~D~]"
              (result-expression object) (result-position object)))))

(defstruct (error-result (:include result) (:constructor nil) (:copier nil)))

(defstruct (inactive-rule (:include error-result)
                          (:constructor make-inactive-rule
                                        (expression %position))
                          (:copier nil)))

(declaim (ftype (function (inactive-rule) (values nonterminal &optional))
                inactive-rule-rule))
(defun inactive-rule-rule (result)
  (result-expression result))

(defstruct (failed-parse
             (:include error-result)
             (:constructor make-failed-parse (expression %position detail))
             (:constructor make-failed-parse/no-position (expression detail))
             (:copier nil)))

;; This is placed in the cache as a place in which information
;; regarding left recursion can be stored temporarily.
(declaim (inline make-left-recursion-result
                 left-recursion-result-p))
(defstruct (left-recursion-result
             (:include error-result)
             (:constructor make-left-recursion-result (expression))
             (:copier nil))
  (head nil :type (or null head)))

(declaim (ftype (function (left-recursion-result) (values nonterminal &optional))
                left-recursion-result-rule))
(defun left-recursion-result-rule (result)
  (result-expression result))

(defstruct (successful-parse
             (:include result)
             (:constructor %make-successful-parse
                           (expression %position detail %production))
             (:copier nil))
  ;; Either a list of results, whose first element is the production,
  ;; or a function to call that will return the production.
  (%production nil :type (or list function)))

(defun successful-parse-production (result)
  (let ((thunk (successful-parse-%production result)))
    (if (functionp thunk)
        (let ((value (funcall thunk (result-detail result))))
          (setf (successful-parse-%production result) (list value))
          value)
        (first thunk))))

;; Result helper functions

(defmacro make-successful-parse (expression position detail production)
  `(%make-successful-parse
    ,expression ,position ,detail
    ,(typecase production
       (symbol
        `(list ,production))
       ((cons (eql function))
        production)
       (t
        `(lambda (detail)
           (declare (ignore detail))
           ,production)))))

(defun result-nonterminal-p (result)
  (typep (result-expression result) 'nonterminal))

(defun result-unsatisfied-predicate-p (result)
  (and (failed-parse-p result)
       (typep (result-expression result) 'predicate)
       (successful-parse-p (result-detail result))))

(defun result-trivial-predicate-p (result)
  (and (typep (result-expression result) 'predicate)
       (expression-case (second (result-expression result))
         ((character character-ranges string terminal)
          t)
         (t
          nil))))

(declaim (ftype (function (result rule-error-report-pattern)
                          (values boolean &optional))
                result-suitable-for-report-part-p))
(defun result-suitable-for-report-part-p (result part)
  (when (result-nonterminal-p result)
    (rule-suitable-for-report-part-p
     (result-expression result) part)))

(declaim (ftype (function (list &optional input-position)
                          (values input-position &optional))
                max-of-result-positions))
(defun max-of-result-positions (results &optional (start 0))
  (reduce #'max results :key #'result-position :initial-value start))

(declaim (ftype (function (list) (values list &optional))
                list-of-result-productions
                list-of-result-productions/butlast))

(defun list-of-result-productions (results)
  (mapcar #'successful-parse-production results))

(defun list-of-result-productions/butlast (results)
  (loop :for rest :on results :while (rest rest)
     :collect (successful-parse-production (first rest))))

;;; For technical reasons, INACTIVE-RULE instances cannot be directly
;;; created with the correct value in the POSITION slot. Fix this by
;;; copying the position from adjacent results, if possible.
(defun maybe-augment-inactive-rules (results)
  (unless (some #'inactive-rule-p results)
    (return-from maybe-augment-inactive-rules results))
  (loop :for previous = nil :then (if (result-p current)
                                      current
                                      previous)
     :for current :in results
     :collect (if (and (inactive-rule-p current)
                       (result-p previous))
                  (make-inactive-rule (result-expression current)
                                      (result-position previous))
                  current)))

(declaim (ftype (function (function result &key (:augment-inactive-rules t)))
                map-results)
         (ftype (function (function result
                           &key (:when-error-report rule-error-report-pattern)))
                map-max-results map-max-leaf-results))

;;; Apply FUNCTION to RESULT and potentially all its ancestor results
;;; (by providing a RECURSE function to FUNCTION) and return whatever
;;; FUNCTION returns.
;;;
;;; More concretely, the lambda-list of FUNCTION has to be compatible
;;; to
;;;
;;;   (result recurse)
;;;
;;; where RESULT is the result object currently being visited and
;;; RECURSE is a function of no arguments that, when called, continues
;;; the traversal into children of RESULT and returns whatever
;;; FUNCTION returns for the sub-tree of ancestor results.
(defun map-results (function result &key (augment-inactive-rules t))
  (let ((function (ensure-function function))
        (augment (if augment-inactive-rules
                     #'maybe-augment-inactive-rules
                     #'identity)))
    (labels ((do-result (result)
               (flet ((recurse ()
                        (let ((detail (result-detail result)))
                          (typecase detail
                            (cons
                             (mapcar #'do-result (funcall augment detail)))
                            (result
                             (do-result detail))))))
                 (declare (dynamic-extent #'recurse))
                 (funcall function result #'recurse))))
      (declare (dynamic-extent #'do-result))
      (do-result result))))

;;; Like MAP-RESULTS but only process results the position of which
;;; (computed as the recursive maximum over ancestors for inner result
;;; nodes) is maximal within the result tree RESULT.
;;;
;;; Furthermore, stop the traversal at results corresponding to !, NOT
;;; and PREDICATE expressions since failed parses among their
;;; respective ancestors are not causes of a failed (or successful)
;;; parse in the usual sense.
;;;
;;; Also restrict processing of nonterminals according to their
;;; :ERROR-REPORT option and WHEN-ERROR-REPORT.
(defun map-max-results (function result
                        &key (when-error-report nil when-error-report-p))
  ;; Process result tree in two passes:
  ;;
  ;; 1. Use MAP-RESULTS to visit results, processing each with either
  ;;    PROCESS-{LEAF or INNER}-RESULT, and collecting results into a
  ;;    tree with nodes of the form
  ;;
  ;;      (RECURSIVE-MAX-POSITION RESULT LIST-OF-CHILDREN)
  ;;
  ;; 2. Use local function MAP-MAX-RESULTS to traverse the tree
  ;;    calling FUNCTION on the RESULT of each node.
  (let ((function (ensure-function function)))
    (labels ((process-leaf-result (result)
               (list (result-position result) result '()))
             (process-inner-result (result recurse)
               (declare (type function recurse))
               (let ((children (remove nil (typecase (result-detail result)
                                             (result (list (funcall recurse)))
                                             (cons   (funcall recurse))))))
                 (cond
                   (children
                    (let* ((max          (reduce #'max children :key #'first))
                           (max-children (remove max children
                                                 :test-not #'= :key #'first)))
                      (list max result max-children)))
                   ((not (successful-parse-p result))
                    (process-leaf-result result)))))
             (process-result (result recurse)
               ;; Treat results produced by inactive rules as if the
               ;; rule was not part of the grammar.
               (unless (inactive-rule-p result)
                 (let ((expression (result-expression result)))
                   (expression-case expression
                     ;; Do not recurse into results for negation-ish
                     ;; expressions.
                     ((! not < >)
                      (process-leaf-result result))
                     ;; If the associated rule is a nonterminal, maybe
                     ;; suppress the result depending on the error-report
                     ;; slot of the rule.
                     (nonterminal
                      (when (or (not when-error-report-p)
                                (rule-suitable-for-report-part-p
                                 expression when-error-report))
                        (process-inner-result result recurse)))
                     (t
                      (process-inner-result result recurse))))))
             (map-max-results (node)
               (destructuring-bind (position result children) node
                 (declare (ignore position))
                 (flet ((recurse ()
                          (mapcar #'map-max-results children)))
                   (declare (dynamic-extent #'recurse))
                   (funcall function result #'recurse)))))
      (declare (dynamic-extent #'process-leaf-result #'process-inner-result
                               #'process-result #'map-max-results))
      (if-let ((max-result-root (map-results #'process-result result)))
        (map-max-results max-result-root)
        (funcall function result (constantly '()))))))

(defun map-max-leaf-results (function result
                             &rest args &key when-error-report)
  (declare (ignore when-error-report))
  (let ((function (ensure-function function)))
    (apply #'map-max-results
           (lambda (result recurse)
             (declare (type function recurse))
             ;; In addition to actual leafs, treat unsatisfied
             ;; predicate results or trivial predicates as leafs (the
             ;; latter are one level above leafs anyway and allow for
             ;; better "expected" messages).
             (when (or (result-unsatisfied-predicate-p result)
                       (result-trivial-predicate-p result)
                       (not (funcall recurse)))
               (funcall function result)))
           result args)))

(declaim (inline flattened-children))
(defun flattened-children (recurse)
  (let ((all-children (funcall (the function recurse))))
    (remove-duplicates (reduce #'append all-children) :test #'eq)))

;;; Return a "context"-providing child result of RESULT, i.e. the most
;;; specific ancestor result of RESULT the path to which contains no
;;; forks:
;;;
;;;   RESULT
;;;   |
;;;   `-child1
;;;     |
;;;     `-child2
;;;       |
;;;       `-nonterminal <- context
;;;         |
;;;         +-child4
;;;         | |
;;;         | ...
;;;         `-child5
;;;           |
;;;           ...
;;;
(defun result-context (result)
  (first
   (map-max-results
    (lambda (result recurse)
      (declare (type function recurse))
      (let ((children (flattened-children recurse)))
        (cond
          ;; unsatisfied predicate result => collect into the result.
          ;;
          ;; This suppresses children of RESULT. The actual context
          ;; will normally be a nonterminal result above RESULT.
          ((result-unsatisfied-predicate-p result)
           (list result))
          ;; nonterminal with a single child => return the child.
          ((and (length= 1 children)
                (or (result-suitable-for-report-part-p
                     (first children) :context)
                    (not (result-suitable-for-report-part-p
                          result :context))))
           children)
          ;; nonterminal with multiple children, i.e. common
          ;; derivation ends here => return RESULT.
          (t
           (list result)))))
    result :when-error-report '(:context :detail))))

;;; Return an explicit description (i.e. a STRING or CONDITION) of the
;;; cause of the parse failure if such a thing can be found in the
;;; result tree rooted at RESULT.
(defun result-root-cause (result)
  (first
   (map-max-results
    (lambda (result recurse)
      (cond
        ((typep result 'inactive-rule)
         (list (let ((*package* (load-time-value (find-package :keyword))))
                 (format nil "Rule ~S is not active"
                         (result-expression result)))))
        ((typep (result-detail result) '(or string condition))
         (list (result-detail result)))
        ((result-unsatisfied-predicate-p result)
         (list (format nil "The production~
                            ~2%~
                            ~2@T~<~S~:>~
                            ~2%~
                            does not satisfy the predicate ~S."
                       (list (successful-parse-production
                              (result-detail result)))
                       (first (result-expression result)))))
        (t
         (flattened-children recurse))))
    result)))

;;; Return a list of terminals that would have allowed the failed
;;; parsed represented by RESULT to succeed.
(defun result-expected-input (result)
  (let ((expected '()))
    (map-max-leaf-results
     (lambda (leaf)
       (mapc (lambda (start-terminal)
               (pushnew start-terminal expected :test #'expression-equal-p))
             (typecase leaf
               (failed-parse
                (expression-start-terminals
                 (result-expression leaf) :when-rule-error-report :detail))
               (successful-parse
                '((not (character)))))))
     result :when-error-report :detail)
    (sort expected #'expression<)))

;;; Return a list of children of RESULT that are the roots of disjoint
;;; result sub-trees.
;;;
;;; Precondition: RESULT is a nonterminal with multiple children
;;; (I.e. RESULT is typically the return value of RESULT-CONTEXT).
(defun partition-results (result)
  (flet ((child-closure (result)
           (let ((results (list result)))
             (map-max-results (lambda (result recurse)
                                (pushnew result results :test #'eq)
                                (funcall recurse))
                              result)
             results)))
    (declare (dynamic-extent #'child-closure))
    (map-max-results
     (lambda (result recurse)
       (let ((children (flattened-children recurse)))
         (cond
           ;; Unsatisfied predicate result => return RESULT.
           ((result-unsatisfied-predicate-p result)
            (list result))
           ;; No children => certainly no fork in ancestors => return
           ;; RESULT.
           ((null children)
            (list result))
           ;; Only a single child, i.e. children have not been
           ;; partitioned => return RESULT.
           ((length= 1 children)
            (if (result-suitable-for-report-part-p (first children) :detail)
                children
                (list result)))
           ;; Multiple children, but not all of them are nonterminals
           ;; and RESULT is a nonterminal => do not use the partition
           ;; into CHILDREN and instead return RESULT.
           ((and (result-suitable-for-report-part-p result :detail)
                 (notevery #'result-nonterminal-p children))
            (list result))
           ;; Multiple children, all of which are nonterminals. If the
           ;; child-closures of all children are disjoint => use the
           ;; partition into children. Otherwise => do not use the
           ;; partition and instead return RESULT.
           (t
            (let ((closures (mapcar #'child-closure children)))
              (loop :named outer :for (closure1 . rest) :on closures :do
                 (loop :for closure2 :in rest :do
                    (when (intersection closure1 closure2 :test #'eq)
                      (return-from outer (list result))))
                 :finally (return-from outer children)))))))
     result :when-error-report :detail)))

;;; Given the "context" result (see RESULT-CONTEXT) CONTEXT, determine
;;; the set of failed ancestor results (see PARTITION-RESULTS).
;;; Display the context and all failed ancestor results optionally
;;; printing the reason for the failure and listing the respective
;;; expected inputs that would have allowed the failed results to
;;; succeed.
(defun error-report (context stream)
  (let* ((partitioned (partition-results context))
         (expected    (mapcar (lambda (root)
                                (let ((reason (result-root-cause root))
                                      (expected (result-expected-input root)))
                                  (list root
                                        (when reason (list reason))
                                        (length expected)
                                        expected)))
                              partitioned))
         (expected    (sort expected #'expression< :key #'first)))
    ;; Print context (if any), then print each failure result from the
    ;; PARTITIONED set with its name and the set of expected inputs,
    ;; if any.
    (format stream "~@<~@[In context ~/esrap:print-result/:~
                         ~@:_~@:_~
                       ~]~
                       ~{~{~
                         While parsing ~/esrap:print-result/. ~
                         ~@[Problem:~@:_~@:_~
                           ~2@T~<~@;~A~:>~
                           ~[~:;~@:_~@:_~]~:*~
                         ~]~
                         ~[~
                           ~*~
                         ~:;~
                           ~:*Expected:~@:_~@:_~
                           ~[~
                             ~2@T~{~/esrap::print-terminal/~}~
                           ~:;~
                             ~5@T~{~/esrap::print-terminal/~^~@:_  or ~}~
                           ~]~
                         ~]~
                       ~}~^~@:_~@:_~}~
                    ~:>"
            context expected)))

(defvar *result-pprint-dispatch*
  (let ((dispatch (copy-pprint-dispatch)))
    (set-pprint-dispatch
     'string (lambda (stream x)
               (write x :stream stream :escape t :pretty nil))
     0 dispatch)
    (set-pprint-dispatch
     'character (lambda (stream x)
                  (if (or (not (graphic-char-p x))
                          (member x '(#\Space #\Tab #\Newline)))
                      (write-string (char-name x) stream)
                      (write (string x) :stream stream :escape t :pretty nil)))
     0 dispatch)
    dispatch))

;; For use as ~/esrap::print-result/ in format control.
(defun print-result (stream result &optional colon? at?)
  (declare (ignore colon? at?))
  (let ((*print-pprint-dispatch* *result-pprint-dispatch*))
    (princ (result-expression result) stream)))
