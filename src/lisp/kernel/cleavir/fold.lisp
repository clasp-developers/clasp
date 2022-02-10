(in-package #:clasp-cleavir)

;;;; Constant folding.

(defmethod bir-transformations:fold-call ((system clasp)
                                          operator
                                          (call bir:abstract-call)
                                          arguments)
  (let ((folder (gethash operator *folds*)))
    (if folder
        (multiple-value-call #'values
          t
          (handler-case (apply folder arguments)
            (serious-condition (c)
              (warn 'cmp:fold-failure :operation operator :operands arguments
                                      :condition c :origin (bir:origin call))
              (return-from bir-transformations:fold-call nil))))
        nil)))

(macrolet ((deffold (name)
             `(setf (gethash ',name *folds*) ',name))
           (deffolds (&rest names)
             `(progn ,@(loop for name in names
                             collect `(deffold ,name)))))
  ;; A few NOTES.
  ;; This list does not include every possible constant fold,
  ;; just simple ones that are always valid. As an example,
  ;; (constantp 'pi) => T, whereas for non-CL symbols it would depend on
  ;; the current environment.
  ;; We fold arithmetic even though for floating point operations there is
  ;; concern about the floating point trap environment. I think a sufficient
  ;; but not necessary way to do it would be to enable all traps while
  ;; folding; then if anything could possibly trap at runtime, an error will
  ;; be signaled and we won't fold.
  ;; This doesn't cover cases where argument count is implicated. For example
  ;; FIND can be folded only with two arguments, since :test etc. functions
  ;; can be redefined.
  ;; We mark some things as foldable that will probably never come up in
  ;; practice, like standard condition readers. I think this is basically
  ;; harmless, other than maybe leading to unexpected successes at dumping
  ;; if someone puts literal conditions in their code.
  ;; Lines marked with * indicate the given fold is only valid if we assume
  ;; that constant data is really not modified.
  (deffolds
      ;; CLHS Ch. 4
      type-error-datum type-error-expected-type
    ;; 5
    not eq eql
    equal equalp ; *
    identity values values-list
    ;; 7
    unbound-slot-instance
    ;; 8 ; structure slot readers TODO
    ;; 9
    cell-error-name
    simple-condition-format-control simple-condition-format-arguments
    restart-name
    ;; 10
    symbolp ; keywordp not sure wrt unintern
    symbol-name ; symbol-package ditto
    ;; 11
    packagep package-error-package
    ;; 12 ; see above note about floating point arithmetic
    = /= < > <= >=
    core:two-arg-= core:two-arg-< core:two-arg->
    core:two-arg-<= core:two-arg->=
    max min minusp plusp zerop
    floor ffloor ceiling fceiling truncate ftruncate round fround
    sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh
    * + - / 1+ 1- abs evenp oddp exp expt gcd lcm log mod rem
    core:two-arg-* core:two-arg-+ core:two-arg-- core:two-arg-/
    core:negate core:reciprocal
    signum sqrt isqrt numberp cis
    complex complexp
    conjugate phase realpart imagpart
    realp numerator denominator rational rationalize rationalp
    ash integer-length integerp core:fixnump
    boole logand logandc1 logandc2 logeqv logior lognand lognor
    lognot logorc1 logorc2 logxor logbitp logcount logtest
    core:logand-2op core:logeqv-2op core:logior-2op core:logxor-2op
    byte byte-size byte-position
    deposit-field dpb ldb ldb-test mask-field
    core::%ldb core::%ldb-test core::%mask-field
    core::%dpb core::%deposit-field
    decode-float scale-float float-radix float-sign float-digits
    float-precision integer-decode-float
    float floatp
    arithmetic-error-operands arithmetic-error-operation
    ;; 13
    char= char/= char< char> char<= char>= char-equal char-not-equal
    char-lessp char-greaterp char-not-greaterp char-not-lessp
    core:two-arg-char-equal core:two-arg-char< core:two-arg-char<=
    core:two-arg-char> core:two-arg-char>= core:two-arg-char-lessp
    core:two-arg-char-greaterp core:two-arg-char-not-lessp
    character characterp alpha-char-p alphanumericp
    digit-char digit-char-p
    graphic-char-p standard-char-p char-upcase char-downcase
    upper-case-p lower-case-p both-case-p
    char-code char-int code-char char-name name-char
    ;; 14
    consp atom
    car cdr caar cadr cdar cddr ; *
    caaar caadr cadar caddr cdaar cdadr cddar cdddr ; *
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr ; *
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr ; *
    listp
    first second third fourth fifth sixth seventh eighth ninth tenth ; *
    nth nthcdr ; *
    endp null
    butlast last tailp get-properties getf ; *
    ;; 15
    adjustable-array-p array-element-type
    aref array-dimension array-dimensions array-has-fill-pointer-p ; *
    array-displacement array-in-bounds-p array-rank ; *
    array-row-major-index array-total-size ; *
    arrayp
    fill-pointer row-major-aref ; *
    simple-vector-p simple-bit-vector-p
    vectorp bit-vector-p ; * could adjust into non-vector
    svref bit sbit ; *
    ;; 16
    simple-string-p
    char schar ; *
    string= string/= string< string> string<= string>= ; *
    string-equal string-not-equal string-lessp string-greaterp ; *
    string-not-greaterp string-not-lessp ; *
    stringp ; * could adjust dimensions to make a non-vector
    ;; 17
    elt length ; *
    ;; 18
    hash-table-p
    hash-table-count hash-table-rehash-size ; *
    hash-table-rehash-threshold hash-table-size ; *
    gethash sxhash ; *
    ;; 19
    pathnamep
    ;; 20
    file-error-pathname
    ;; 21
    streamp stream-error-stream
    ;; 22
    print-not-readable-object
    ;; 23
    readtablep
    ;; 24
    ))
