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

(defun parse (expression text &key (start 0) end junk-allowed raw)
  "Parses TEXT using EXPRESSION from START to END.

Incomplete parses, that is not consuming the entirety of TEXT, are
allowed only if JUNK-ALLOWED is true.

Returns three values:

1) A production, if the parse succeeded, NIL otherwise.
2) The position up to which TEXT has been consumed or NIL if the
   entirety of TEXT has been consumed.
3) If the parse succeeded, even if it did not consume any input, T is
   returned as a third value.

The third return value is necessary to distinguish successful and
failed parses for cases like

  (parse '(! #\\a) \"a\" :junk-allowed t)
  (parse '(! #\\a) \"b\" :junk-allowed t)

in which the first two return values cannot indicate failures.

RAW controls whether the parse result is interpreted and translated
into the return values described above. If RAW is true, a parse result
of type RESULT or ERROR-RESULT is returned as a single value.

Note that the combination of arguments :junk-allowed t :raw t does not
make sense since the JUNK-ALLOWED parameter is used when parse results
are interpreted and translated into return values which does not
happen when :raw t."
  ;; There is no backtracking in the toplevel expression -- so there's
  ;; no point in compiling it as it will be executed only once -- unless
  ;; it's a constant, for which we have a compiler-macro.
  (when (and junk-allowed raw)
    (error "~@<The combination of arguments ~{~S~^ ~} does not make ~
            sense.~@:>"
           (list :junk-allowed junk-allowed :raw raw)))
  (let* ((end (or end (length text)))
         (*context* (make-context))
         (result (eval-expression expression text start end)))
    (declare (dynamic-extent *context*))
    (if raw
        result
        (process-parse-result result text start end junk-allowed))))

(define-compiler-macro parse (&whole form expression text
                              &rest arguments &key &allow-other-keys)
  (flet ((make-expansion (result-var rawp junk-allowed-p body)
           ;; This inline-lambda provides keyword defaults and
           ;; parsing, so the compiler-macro doesn't have to worry
           ;; about evaluation order.
           (with-gensyms (expr-fun)
             `(let ((,expr-fun (load-time-value (compile-expression ,expression))))
                ((lambda (text &key (start 0) end
                                    ,@(if rawp '(raw))
                                    ,@(if junk-allowed-p '(junk-allowed)))
                   (let* ((end (or end (length text)))
                          (*context* (make-context))
                          (,result-var (funcall ,expr-fun text start end)))
                     (declare (dynamic-extent *context*))
                     ,body))
                 ,text ,@(remove-from-plist arguments :raw))))))
   (cond
     ((not (constantp expression)) ; cannot use ENV due to LOAD-TIME-VALUE
      form)
     ((let ((raw (getf arguments :raw 'missing)))
        (when (and (not (eq raw 'missing))
                   (constantp raw)) ; cannot used ENV due to following EVAL
          (let ((rawp (eval raw)))
            (make-expansion 'result nil (not rawp)
                            (if rawp
                                'result
                                '(process-parse-result
                                  result text start end junk-allowed)))))))
     (t
      (make-expansion 'result t t
                      '(if raw
                           result
                           (process-parse-result
                            result text start end junk-allowed)))))))

(defun process-parse-result (result text start end junk-allowed)
  (cond
    ;; Successfully parsed something.
    ((successful-parse-p result)
     (with-accessors ((position result-position)
                      (production successful-parse-production))
         result
       (cond
         ((= position end)                ; Consumed all input.
          (values production nil t))
         (junk-allowed                    ; Did not consume all input; junk
          (values production position t)) ; is OK.
         (t                               ; Junk is not OK.
          (esrap-parse-error text result)))))
    ;; Did not parse anything, but junk is allowed.
    (junk-allowed
     (values nil start))
    ;; Did not parse anything and junk is not allowed.
    (t
     (esrap-parse-error text result))))

(defmacro defrule (&whole form symbol expression &body options)
  "Define SYMBOL as a nonterminal, using EXPRESSION as associated the parsing expression.

Multiple OPTIONS specifying transforms are composed in the order of
appearance:

  (:text t)
  (:function parse-integer)
  =>
  (alexandria:compose #'parse-integer #'text)

Following OPTIONS can be specified:

  * (:WHEN TEST)

    The rule is active only when TEST evaluates to true. This can be used
    to specify optional extensions to a grammar.

    This option can only be supplied once.

  * (:CONSTANT CONSTANT)

    No matter what input is consumed or what EXPRESSION produces, the production
    of the rule is always CONSTANT.

  * (:FUNCTION FUNCTION)

    If provided the production of the expression is transformed using
    FUNCTION. FUNCTION can be a function name or a lambda-expression.

  * (:IDENTITY BOOLEAN)

    If true, the production of expression is used as-is, as if (:FUNCTION IDENTITY)
    has been specified. If no production option is specified, this is the default.

  * (:TEXT BOOLEAN)

    If true, the production of expression is flattened and concatenated into a string
    as if by (:FUNCTION TEXT) has been specified.

  * (:LAMBDA LAMBDA-LIST &BODY BODY)

    If provided, same as using the corresponding lambda-expression with :FUNCTION.

    As an extension of the standard lambda list syntax, LAMBDA-LIST accepts
    the optional pseudo lambda-list keyword ESRAP:&BOUNDS, which (1) must appear
    after all standard lambda list keywords. (2) can be followed by one or two
    variables to which bounding indexes of the matching substring are bound.

    Therefore:

      LAMBDA-LIST ::= (STANDARD-LAMBDA-LIST-ELEMENTS [&BOUNDS START [END]])

  * (:DESTRUCTURE DESTRUCTURING-LAMBDA-LIST &BODY BODY)

    If provided, same as using a lambda-expression that destructures its argument
    using DESTRUCTURING-BIND and the provided lambda-list with :FUNCTION.

    DESTRUCTURING-LAMBDA-LIST can use ESRAP:&BOUNDS in the same way
    as described for :LAMBDA.

  * (:AROUND ([&BOUNDS START [END]]) &BODY BODY)

    If provided, execute BODY around the construction of the production of the
    rule. BODY has to call ESRAP:CALL-TRANSFORM to trigger the computation of
    the production. Any transformation provided via :LAMBDA, :FUNCTION
    or :DESTRUCTURE is executed inside the call to ESRAP:CALL-TRANSFORM. As a
    result, modification to the dynamic state are visible within the
    transform.

    ESRAP:&BOUNDS can be used in the same way as described for :LAMBDA
    and :DESTRUCTURE.

    This option can be used to safely track nesting depth, manage symbol
    tables or for other stack-like operations.

  * (:ERROR-REPORT ( T | NIL | :CONTEXT | :DETAIL ))

    Defaults to T if not provided. Controls whether and how the rule
    is used in parse error reports:

    * T

      The rule is used in parse error reports without
      restriction (i.e. when describing the context of a failure as
      well as listing failed rules and expected inputs).

    * NIL

      The rule is not used in parse error reports in any capacity. In
      particular, inputs expected by the rule are not mentioned.

      This value is useful for things like whitespace rules since
      something like \"expected space, tab or newline\", even if it
      would have allowed the parser to continue for one character, is
      rarely helpful.

    * :CONTEXT

      The rule is used in the \"context\" part of parse error
      reports. The rule is neither mentioned in the list of failed
      rules nor are inputs expected by it.

    * :DETAIL

      The rule is not used in the \"context\" part of parse error
      reports, but can appear in the list of failed rules. Inputs
      expected by the rule are mentioned as well.
"
  (multiple-value-bind (transforms around when error-report)
      (parse-defrule-options options form)
    (let ((transform (expand-transforms transforms)))
      `(eval-when (:load-toplevel :execute)
         (add-rule ',symbol (make-instance 'rule
                                           :expression ',expression
                                           :guard-expression ',(cdr when)
                                           :condition ,(car when)
                                           :transform ,transform
                                           :around ,around
                                           :error-report ,error-report))))))

(defun add-rule (symbol rule)
  "Associates RULE with the nonterminal SYMBOL. Signals an error if the
rule is already associated with a nonterminal. If the symbol is already
associated with a rule, the old rule is removed first."
  ;; FIXME: This needs locking and WITHOUT-INTERRUPTS.
  (check-type symbol nonterminal)
  (when (rule-symbol rule)
    (error "~S is already associated with the nonterminal ~S -- remove it first."
           rule (rule-symbol rule)))
  (let* ((cell (ensure-rule-cell symbol))
         (function (compile-rule symbol
                                 (rule-expression rule)
                                 (rule-condition rule)
                                 (rule-transform rule)
                                 (rule-around rule)))
         (trace-info (cell-trace-info cell)))
    (set-cell-info cell function rule)
    (setf (cell-trace-info cell)     nil
          (slot-value rule '%symbol) symbol)
    (when trace-info
      (destructuring-bind (break condition) (rest trace-info)
        (trace-rule symbol :break break :condition condition)))
    symbol))

(defun find-rule (symbol)
  "Returns rule designated by SYMBOL, if any. Symbol must be a nonterminal
symbol."
  (check-type symbol nonterminal)
  (when-let ((cell (find-rule-cell symbol)))
    (cell-rule cell)))

(defun remove-rule (symbol &key force)
  "Makes the nonterminal SYMBOL undefined. If the nonterminal is defined an
already referred to by other rules, an error is signalled unless :FORCE is
true."
  (check-type symbol nonterminal)
  ;; FIXME: Lock and WITHOUT-INTERRUPTS.
  (let* ((cell (find-rule-cell symbol))
         (rule (cell-rule cell))
         (trace-info (cell-trace-info cell)))
    (when cell
      (flet ((frob ()
               (set-cell-info cell (undefined-rule-function symbol) nil) ; TODO update trace-info as part of this function?
               (when trace-info
                 (setf (cell-trace-info cell) (list* (cell-%info cell) (rest trace-info))))
               (when rule
                 (detach-rule rule))))
        (cond ((and rule (cell-referents cell))
               (unless force
                 (error "Nonterminal ~S is used by other nonterminal~P:~% ~{~S~^, ~}"
                        symbol (length (cell-referents cell)) (cell-referents cell)))
               (frob))
              ((not (cell-referents cell))
               (frob)
               ;; There are no references to the rule at all, so
               ;; we can remove the cell.
               (unless trace-info
                 (delete-rule-cell symbol)))))
      rule)))

(defvar *trace-level* 0)

(defun trace-rule (symbol &key recursive break condition)
  "Turn on tracing of nonterminal SYMBOL.

If RECURSIVE is true, turn on tracing for the whole grammar rooted at
SYMBOL. If RECURSIVE is a positive integer, turn on tracing for all
rules reachable from the nonterminal SYMBOL in that number of steps.

If BREAK is true, break is entered when the rule is invoked.

If supplied, CONDITION has to be a function whose lambda-list is
compatible to (symbol text position end). This function is called to
determine whether trace actions should be executed for the traced
rule.

  SYMBOL is the name of the rule being executed.

  TEXT is the whole text being parsed.

  POSITION is the position within TEXT at which the rule is executed.

  END is the end position of the portion of TEXT being parsed."
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((traced (symbol break fun text position end)
               (when break
                 (break "rule ~S" symbol))
               (format *trace-output* "~&~V@T~D: ~S ~S?~%"
                       *trace-level* (1+ *trace-level*) symbol position)
               (finish-output *trace-output*)
               (let* ((*trace-level* (1+ *trace-level*))
                      (result (funcall fun text position end)))
                 (format *trace-output* "~&~V@T~D: ~S "
                         (1- *trace-level*) *trace-level* symbol)
                 (if (error-result-p result)
                     (format *trace-output* "-|~%")
                     (format *trace-output* "~S-~S -> ~S~%"
                             position (result-position result)
                             (successful-parse-production result)))
                 (finish-output *trace-output*)
                 result))
             (traced/condition (condition symbol break fun text position end)
               (if (funcall condition symbol text position end)
                   (traced symbol break fun text position end)
                   (funcall fun text position end)))
             (trace-one (symbol cell depth)
               ;; Avoid infinite recursion and processing sub-trees
               ;; multiple times.
               (if (gethash cell seen)
                   (return-from trace-one)
                   (setf (gethash cell seen) t))
               ;; If there is old trace information, removed it first.
               (when (cell-trace-info cell)
                 (untrace-rule symbol))
               ;; Wrap the cell function in a tracing function. Store
               ;; old info in trace-info slot of CELL.
               (let ((fun (cell-function cell))
                     (rule (cell-rule cell))
                     (info (cell-%info cell)))
                 (set-cell-info
                  cell (if condition
                           (curry #'traced/condition condition symbol break fun)
                           (curry #'traced symbol break fun))
                  rule)
                 (setf (cell-trace-info cell) (list info break condition))
                 ;; If requested, trace dependencies
                 ;; recursively. Checking RULE avoids recursing into
                 ;; referenced but undefined rules.
                 (when (and rule
                            (if (integerp depth) (plusp depth) depth))
                   (dolist (dep (%rule-direct-dependencies rule))
                     (trace-one dep (find-rule-cell dep)
                                (if (integerp depth) (1- depth) depth)))))
               t))
      (trace-one symbol
                 (or (find-rule-cell symbol)
                     (undefined-rule symbol))
                 recursive))))

(defun untrace-rule (symbol &key recursive break condition)
  "Turn off tracing of nonterminal SYMBOL.

If RECURSIVE is true, turn off tracing for the whole grammar rooted at
SYMBOL. If RECURSIVE is a positive integer, turn off tracing for all
rules reachable from the nonterminal SYMBOL in that number of steps.

BREAK and CONDITION are ignored, and are provided only for symmetry
with TRACE-RULE."
  (declare (ignore break condition))
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((untrace-one (cell depth)
               ;; Avoid infinite recursion and processing sub-trees
               ;; multiple times.
               (if (gethash cell seen)
                   (return-from untrace-one)
                   (setf (gethash cell seen) t))
               ;; Restore info from trace-info slot of CELL.
               (let ((rule (cell-rule cell))
                     (trace-info (cell-trace-info cell)))
                 (when trace-info
                   (setf (cell-%info cell) (first trace-info)
                         (cell-trace-info cell) nil))
                 ;; If requested, trace dependencies
                 ;; recursively. Checking RULE avoids recursing into
                 ;; referenced but undefined rules.
                  (when (and rule
                             (if (integerp depth) (plusp depth) depth))
                   (dolist (dep (%rule-direct-dependencies rule))
                     (untrace-one (find-rule-cell dep)
                                  (if (integerp depth) (1- depth) depth)))))
               nil))
      (untrace-one (or (find-rule-cell symbol)
                       (undefined-rule symbol))
                   recursive))))

(defun rule-expression (rule)
  "Return the parsing expression associated with the RULE."
  (slot-value rule '%expression))

(defun (setf rule-expression) (expression rule)
  "Modify RULE to use EXPRESSION as the parsing expression. The rule must be
detached beforehand."
  (let ((name (rule-symbol rule)))
    (when name
      (error "~@<Cannot change the expression of an active rule, ~
              remove ~S first, or use CHANGE-RULE.~:@>"
             name))
    (setf (slot-value rule '%expression) expression)))

(defun change-rule (symbol expression)
  "Modifies the nonterminal SYMBOL to use EXPRESSION instead. Temporarily
removes the rule while it is being modified."
  (let ((rule (remove-rule symbol :force t)))
    (unless rule
      (undefined-rule symbol))
    (setf (rule-expression rule) expression)
    (add-rule symbol rule)))

(defun describe-grammar (symbol &optional (stream *standard-output*))
  "Prints the grammar tree rooted at nonterminal SYMBOL to STREAM for human
inspection."
  (check-type symbol nonterminal)
  (flet ((max-symbol-length (symbols)
           (reduce #'max symbols
                   :key (compose #'length #'prin1-to-string)
                   :initial-value 0))
         (output-rule (length rule)
           (format stream "~3T~S~VT<- ~S~@[ : ~S~]~%"
                   (rule-symbol rule)
                   length
                   (rule-expression rule)
                   (when (rule-condition rule)
                     (rule-guard-expression rule)))))
    (if-let ((rule (find-rule symbol)))
      (progn
        (format stream "~&Grammar ~S:~%" symbol)
        (multiple-value-bind (defined undefined) (rule-dependencies rule)
          (let ((length
                 (+ 4 (max (max-symbol-length defined)
                           (max-symbol-length undefined)))))
            (output-rule length rule)
            (mapc (compose (curry #'output-rule length) #'find-rule) defined)
            (when undefined
              (format stream "~%Undefined nonterminal~P:~%~{~3T~S~%~}"
                      (length undefined) undefined)))))
      (format stream "Symbol ~S is not a defined nonterminal." symbol))))
