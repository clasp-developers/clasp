;;;; -------------------------------------------------------------------------
;;;; General Purpose Utilities for ASDF

(uiop/package:define-package :uiop/utility
  (:nicknames :asdf/utility)
  (:recycle :uiop/utility :asdf/utility :asdf)
  (:use :uiop/common-lisp :uiop/package)
  ;; import and reexport a few things defined in :uiop/common-lisp
  (:import-from :uiop/common-lisp #:compatfmt #:loop* #:frob-substrings
   #+ecl #:use-ecl-byte-compiler-p #+mcl #:probe-posix)
  (:export #:compatfmt #:loop* #:frob-substrings #:compatfmt
   #+ecl #:use-ecl-byte-compiler-p #+mcl #:probe-posix)
  (:export
   ;; magic helper to define debugging functions:
   #:uiop-debug #:load-uiop-debug-utility #:*uiop-debug-utility*
   #:with-upgradability ;; (un)defining functions in an upgrade-friendly way
   #:undefine-function #:undefine-functions #:defun* #:defgeneric*
   #:nest #:if-let ;; basic flow control
   #:while-collecting #:appendf #:length=n-p #:ensure-list ;; lists
   #:remove-plist-keys #:remove-plist-key ;; plists
   #:emptyp ;; sequences
   #:+non-base-chars-exist-p+ ;; characters
   #:+max-character-type-index+ #:character-type-index #:+character-types+
   #:base-string-p #:strings-common-element-type #:reduce/strcat #:strcat ;; strings
   #:first-char #:last-char #:split-string #:stripln #:+cr+ #:+lf+ #:+crlf+
   #:string-prefix-p #:string-enclosed-p #:string-suffix-p
   #:coerce-class ;; CLOS
   #:stamp< #:stamps< #:stamp*< #:stamp<= ;; stamps
   #:earlier-stamp #:stamps-earliest #:earliest-stamp
   #:later-stamp #:stamps-latest #:latest-stamp #:latest-stamp-f
   #:list-to-hash-set #:ensure-gethash ;; hash-table
   #:ensure-function #:access-at #:access-at-count ;; functions
   #:call-function #:call-functions #:register-hook-function
   #:match-condition-p #:match-any-condition-p ;; conditions
   #:call-with-muffled-conditions #:with-muffled-conditions
   #:lexicographic< #:lexicographic<=
   #:parse-version #:unparse-version #:version< #:version<= #:version-compatible-p)) ;; version
(in-package :uiop/utility)

;;;; Defining functions in a way compatible with hot-upgrade:
;; DEFUN* and DEFGENERIC* use FMAKUNBOUND to delete any previous fdefinition,
;; thus replacing the function without warning or error
;; even if the signature and/or generic-ness of the function has changed.
;; For a generic function, this invalidates any previous DEFMETHOD.
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun undefine-function (function-spec)
    (cond
      ((symbolp function-spec)
       ;; undefining the previous function is the portable way
       ;; of overriding any incompatible previous gf,
       ;; but CLISP needs extra help with getting rid of previous methods.
       #+clisp
       (let ((f (and (fboundp function-spec) (fdefinition function-spec))))
         (when (typep f 'clos:standard-generic-function)
           (loop :for m :in (clos:generic-function-methods f)
                 :do (remove-method f m))))
       (fmakunbound function-spec))
      ((and (consp function-spec) (eq (car function-spec) 'setf)
            (consp (cdr function-spec)) (null (cddr function-spec)))
       (fmakunbound function-spec))
      (t (error "bad function spec ~S" function-spec))))
  (defun undefine-functions (function-spec-list)
    (map () 'undefine-function function-spec-list))
  (macrolet
      ((defdef (def* def)
         `(defmacro ,def* (name formals &rest rest)
            (destructuring-bind (name &key (supersede t))
                (if (or (atom name) (eq (car name) 'setf))
                    (list name :supersede nil)
                    name)
              (declare (ignorable supersede))
              `(progn
                 ;; We usually try to do it only for the functions that need it,
                 ;; which happens in asdf/upgrade - however, for ECL, we need this hammer.
                 ,@(when (or supersede #+ecl t)
                     `((undefine-function ',name)))
                 ,@(when (and #+ecl (symbolp name)) ; fails for setf functions on ecl
                     `((declaim (notinline ,name))))
                 (,',def ,name ,formals ,@rest))))))
    (defdef defgeneric* defgeneric)
    (defdef defun* defun))
  (defmacro with-upgradability ((&optional) &body body)
    "Evaluate BODY at compile- load- and run- times, with DEFUN and DEFGENERIC modified
to also declare the functions NOTINLINE and to accept a wrapping the function name
specification into a list with keyword argument SUPERSEDE (which defaults to T if the name
is not wrapped, and NIL if it is wrapped). If SUPERSEDE is true, call UNDEFINE-FUNCTION
to supersede any previous definition."
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(loop :for form :in body :collect
               (if (consp form)
                   (destructuring-bind (car . cdr) form
                     (case car
                       ((defun) `(defun* ,@cdr))
                       ((defgeneric) `(defgeneric* ,@cdr))
                       (otherwise form)))
                   form)))))

;;; Magic debugging help. See contrib/debug.lisp
(with-upgradability ()
  (defvar *uiop-debug-utility*
    '(or (ignore-errors
          (symbol-call :asdf :system-relative-pathname :uiop "contrib/debug.lisp"))
      (symbol-call :uiop/pathname :subpathname (user-homedir-pathname) "cl/asdf/uiop/contrib/debug.lisp"))
    "form that evaluates to the pathname to your favorite debugging utilities")

  (defmacro uiop-debug (&rest keys)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (load-uiop-debug-utility ,@keys)))

  (defun load-uiop-debug-utility (&key package utility-file)
    (let* ((*package* (if package (find-package package) *package*))
           (keyword (read-from-string
                     (format nil ":DBG-~:@(~A~)" (package-name *package*)))))
      (unless (member keyword *features*)
        (let* ((utility-file (or utility-file *uiop-debug-utility*))
               (file (ignore-errors (probe-file (eval utility-file)))))
          (if file (load file)
              (error "Failed to locate debug utility file: ~S" utility-file)))))))

;;; Flow control
(with-upgradability ()
  (defmacro nest (&rest things)
    "Macro to do keep code nesting and indentation under control." ;; Thanks to mbaringer
    (reduce #'(lambda (outer inner) `(,@outer ,inner))
            things :from-end t))

  (defmacro if-let (bindings &body (then-form &optional else-form)) ;; from alexandria
    ;; bindings can be (var form) or ((var1 form1) ...)
    (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                             (list bindings)
                             bindings))
           (variables (mapcar #'car binding-list)))
      `(let ,binding-list
         (if (and ,@variables)
             ,then-form
             ,else-form)))))

;;; List manipulation
(with-upgradability ()
  (defmacro while-collecting ((&rest collectors) &body body)
    "COLLECTORS should be a list of names for collections.  A collector
defines a function that, when applied to an argument inside BODY, will
add its argument to the corresponding collection.  Returns multiple values,
a list for each collection, in order.
   E.g.,
\(while-collecting \(foo bar\)
           \(dolist \(x '\(\(a 1\) \(b 2\) \(c 3\)\)\)
             \(foo \(first x\)\)
             \(bar \(second x\)\)\)\)
Returns two values: \(A B C\) and \(1 2 3\)."
    (let ((vars (mapcar #'(lambda (x) (gensym (symbol-name x))) collectors))
          (initial-values (mapcar (constantly nil) collectors)))
      `(let ,(mapcar #'list vars initial-values)
         (flet ,(mapcar #'(lambda (c v) `(,c (x) (push x ,v) (values))) collectors vars)
           ,@body
           (values ,@(mapcar #'(lambda (v) `(reverse ,v)) vars))))))

  (define-modify-macro appendf (&rest args)
    append "Append onto list") ;; only to be used on short lists.

  (defun length=n-p (x n) ;is it that (= (length x) n) ?
    (check-type n (integer 0 *))
    (loop
      :for l = x :then (cdr l)
      :for i :downfrom n :do
        (cond
          ((zerop i) (return (null l)))
          ((not (consp l)) (return nil)))))

  (defun ensure-list (x)
    (if (listp x) x (list x))))


;;; remove a key from a plist, i.e. for keyword argument cleanup
(with-upgradability ()
  (defun remove-plist-key (key plist)
    "Remove a single key from a plist"
    (loop* :for (k v) :on plist :by #'cddr
           :unless (eq k key)
           :append (list k v)))

  (defun remove-plist-keys (keys plist)
    "Remove a list of keys from a plist"
    (loop* :for (k v) :on plist :by #'cddr
           :unless (member k keys)
           :append (list k v))))


;;; Sequences
(with-upgradability ()
  (defun emptyp (x)
    "Predicate that is true for an empty sequence"
    (or (null x) (and (vectorp x) (zerop (length x))))))


;;; Characters
(with-upgradability () ;; base-char != character on ECL, LW, SBCL, Genera. LW also has SIMPLE-CHAR.
  (defconstant +non-base-chars-exist-p+ #.(not (subtypep 'character 'base-char)))
  #-scl ;; In SCL, all characters seem to be 16-bit base-char, but this flag gets set somehow???
  (when +non-base-chars-exist-p+ (pushnew :non-base-chars-exist-p *features*)))

(with-upgradability ()
  (defparameter +character-types+ ;; assuming a simple hierarchy
    #(#+non-base-chars-exist-p base-char #+lispworks lw:simple-char character))
  (defparameter +max-character-type-index+ (1- (length +character-types+))))

(with-upgradability ()
  (defun character-type-index (x)
    (declare (ignorable x))
    #.(case +max-character-type-index+
        (0 0)
        (1 '(etypecase x
             (character (if (typep x 'base-char) 0 1))
             (symbol (if (subtypep x 'base-char) 0 1))))
        (otherwise
         '(or (position-if (etypecase x
                             (character  #'(lambda (type) (typep x type)))
                             (symbol #'(lambda (type) (subtypep x type))))
               +character-types+)
           (error "Not a character or character type: ~S" x))))))


;;; Strings
(with-upgradability ()
  (defun base-string-p (string)
    "Does the STRING only contain BASE-CHARs?"
    (declare (ignorable string))
    (and #+non-base-chars-exist-p (eq 'base-char (array-element-type string))))

  (defun strings-common-element-type (strings)
    "What least subtype of CHARACTER can contain all the elements of all the STRINGS?"
    (declare (ignorable strings))
    #.(if +non-base-chars-exist-p+
          `(aref +character-types+
            (loop :with index = 0 :for s :in strings :do
              (cond
                ((= index ,+max-character-type-index+) (return index))
                ((emptyp s)) ;; NIL or empty string
                ((characterp s) (setf index (max index (character-type-index s))))
                ((stringp s) (unless (>= index (character-type-index (array-element-type s)))
                               (setf index (reduce 'max s :key #'character-type-index
                                                          :initial-value index))))
                (t (error "Invalid string designator ~S for ~S" s 'strings-common-element-type)))
                  :finally (return index)))
          ''character))

  (defun reduce/strcat (strings &key key start end)
    "Reduce a list as if by STRCAT, accepting KEY START and END keywords like REDUCE.
NIL is interpreted as an empty string. A character is interpreted as a string of length one."
    (when (or start end) (setf strings (subseq strings start end)))
    (when key (setf strings (mapcar key strings)))
    (loop :with output = (make-string (loop :for s :in strings
                                            :sum (if (characterp s) 1 (length s)))
                                      :element-type (strings-common-element-type strings))
          :with pos = 0
          :for input :in strings
          :do (etypecase input
                (null)
                (character (setf (char output pos) input) (incf pos))
                (string (replace output input :start1 pos) (incf pos (length input))))
          :finally (return output)))

  (defun strcat (&rest strings)
    "Concatenate strings.
NIL is interpreted as an empty string, a character as a string of length one."
    (reduce/strcat strings))

  (defun first-char (s)
    "Return the first character of a non-empty string S, or NIL"
    (and (stringp s) (plusp (length s)) (char s 0)))

  (defun last-char (s)
    "Return the last character of a non-empty string S, or NIL"
    (and (stringp s) (plusp (length s)) (char s (1- (length s)))))

  (defun split-string (string &key max (separator '(#\Space #\Tab)))
    "Split STRING into a list of components separated by
any of the characters in the sequence SEPARATOR.
If MAX is specified, then no more than max(1,MAX) components will be returned,
starting the separation from the end, e.g. when called with arguments
 \"a.b.c.d.e\" :max 3 :separator \".\" it will return (\"a.b.c\" \"d\" \"e\")."
    (block ()
      (let ((list nil) (words 0) (end (length string)))
        (when (zerop end) (return nil))
        (flet ((separatorp (char) (find char separator))
               (done () (return (cons (subseq string 0 end) list))))
          (loop
            :for start = (if (and max (>= words (1- max)))
                             (done)
                             (position-if #'separatorp string :end end :from-end t))
            :do (when (null start) (done))
                (push (subseq string (1+ start) end) list)
                (incf words)
                (setf end start))))))

  (defun string-prefix-p (prefix string)
    "Does STRING begin with PREFIX?"
    (let* ((x (string prefix))
           (y (string string))
           (lx (length x))
           (ly (length y)))
      (and (<= lx ly) (string= x y :end2 lx))))

  (defun string-suffix-p (string suffix)
    "Does STRING end with SUFFIX?"
    (let* ((x (string string))
           (y (string suffix))
           (lx (length x))
           (ly (length y)))
      (and (<= ly lx) (string= x y :start1 (- lx ly)))))

  (defun string-enclosed-p (prefix string suffix)
    "Does STRING begin with PREFIX and end with SUFFIX?"
    (and (string-prefix-p prefix string)
         (string-suffix-p string suffix))))

  (defvar +cr+ (coerce #(#\Return) 'string))
  (defvar +lf+ (coerce #(#\Linefeed) 'string))
  (defvar +crlf+ (coerce #(#\Return #\Linefeed) 'string))

  (defun stripln (x)
    "Strip a string X from any ending CR, LF or CRLF.
Return two values, the stripped string and the ending that was stripped,
or the original value and NIL if no stripping took place.
Since our STRCAT accepts NIL as empty string designator,
the two results passed to STRCAT always reconstitute the original string"
    (check-type x string)
    (block nil
      (flet ((c (end) (when (string-suffix-p x end)
                        (return (values (subseq x 0 (- (length x) (length end))) end)))))
        (when x (c +crlf+) (c +lf+) (c +cr+) (values x nil)))))


;;; stamps: a REAL or a boolean where NIL=-infinity, T=+infinity
(eval-when (#-lispworks :compile-toplevel :load-toplevel :execute)
  (deftype stamp () '(or real boolean)))
(with-upgradability ()
  (defun stamp< (x y)
    (etypecase x
      (null (and y t))
      ((eql t) nil)
      (real (etypecase y
              (null nil)
              ((eql t) t)
              (real (< x y))))))
  (defun stamps< (list) (loop :for y :in list :for x = nil :then y :always (stamp< x y)))
  (defun stamp*< (&rest list) (stamps< list))
  (defun stamp<= (x y) (not (stamp< y x)))
  (defun earlier-stamp (x y) (if (stamp< x y) x y))
  (defun stamps-earliest (list) (reduce 'earlier-stamp list :initial-value t))
  (defun earliest-stamp (&rest list) (stamps-earliest list))
  (defun later-stamp (x y) (if (stamp< x y) y x))
  (defun stamps-latest (list) (reduce 'later-stamp list :initial-value nil))
  (defun latest-stamp (&rest list) (stamps-latest list))
  (define-modify-macro latest-stamp-f (&rest stamps) latest-stamp))


;;; Function designators
(with-upgradability ()
  (defun ensure-function (fun &key (package :cl))
    "Coerce the object FUN into a function.

If FUN is a FUNCTION, return it.
If the FUN is a non-sequence literal constant, return constantly that,
i.e. for a boolean keyword character number or pathname.
Otherwise if FUN is a non-literally constant symbol, return its FDEFINITION.
If FUN is a CONS, return the function that applies its CAR
to the appended list of the rest of its CDR and the arguments,
unless the CAR is LAMBDA, in which case the expression is evaluated.
If FUN is a string, READ a form from it in the specified PACKAGE (default: CL)
and EVAL that in a (FUNCTION ...) context."
    (etypecase fun
      (function fun)
      ((or boolean keyword character number pathname) (constantly fun))
      (hash-table #'(lambda (x) (gethash x fun)))
      (symbol (fdefinition fun))
      (cons (if (eq 'lambda (car fun))
                (eval fun)
                #'(lambda (&rest args) (apply (car fun) (append (cdr fun) args)))))
      (string (eval `(function ,(with-standard-io-syntax
                                  (let ((*package* (find-package package)))
                                    (read-from-string fun))))))))

  (defun access-at (object at)
    "Given an OBJECT and an AT specifier, list of successive accessors,
call each accessor on the result of the previous calls.
An accessor may be an integer, meaning a call to ELT,
a keyword, meaning a call to GETF,
NIL, meaning identity,
a function or other symbol, meaning itself,
or a list of a function designator and arguments, interpreted as per ENSURE-FUNCTION.
As a degenerate case, the AT specifier may be an atom of a single such accessor
instead of a list."
    (flet ((access (object accessor)
             (etypecase accessor
               (function (funcall accessor object))
               (integer (elt object accessor))
               (keyword (getf object accessor))
               (null object)
               (symbol (funcall accessor object))
               (cons (funcall (ensure-function accessor) object)))))
      (if (listp at)
          (dolist (accessor at object)
            (setf object (access object accessor)))
          (access object at))))

  (defun access-at-count (at)
    "From an AT specification, extract a COUNT of maximum number
of sub-objects to read as per ACCESS-AT"
    (cond
      ((integerp at)
       (1+ at))
      ((and (consp at) (integerp (first at)))
       (1+ (first at)))))

  (defun call-function (function-spec &rest arguments)
    "Call the function designated by FUNCTION-SPEC as per ENSURE-FUNCTION,
with the given ARGUMENTS"
    (apply (ensure-function function-spec) arguments))

  (defun call-functions (function-specs)
    "For each function in the list FUNCTION-SPECS, in order, call the function as per CALL-FUNCTION"
    (map () 'call-function function-specs))

  (defun register-hook-function (variable hook &optional call-now-p)
    "Push the HOOK function (a designator as per ENSURE-FUNCTION) onto the hook VARIABLE.
When CALL-NOW-P is true, also call the function immediately."
    (pushnew hook (symbol-value variable) :test 'equal)
    (when call-now-p (call-function hook))))


;;; CLOS
(with-upgradability ()
  (defun coerce-class (class &key (package :cl) (super t) (error 'error))
    "Coerce CLASS to a class that is subclass of SUPER if specified,
or invoke ERROR handler as per CALL-FUNCTION.

A keyword designates the name a symbol, which when found in either PACKAGE, designates a class.
-- for backward compatibility, *PACKAGE* is also accepted for now, but this may go in the future.
A string is read as a symbol while in PACKAGE, the symbol designates a class.

A class object designates itself.
NIL designates itself (no class).
A symbol otherwise designates a class by name."
    (let* ((normalized
             (typecase class
              (keyword (or (find-symbol* class package nil)
                           (find-symbol* class *package* nil)))
              (string (symbol-call :uiop :safe-read-from-string class :package package))
              (t class)))
           (found
             (etypecase normalized
               ((or standard-class built-in-class) normalized)
               ((or null keyword) nil)
               (symbol (find-class normalized nil nil)))))
      (or (and found
               (or (eq super t) (#-cormanlisp subtypep #+cormanlisp cl::subclassp found super))
               found)
          (call-function error "Can't coerce ~S to a ~@[class~;subclass of ~:*~S]" class super)))))


;;; Hash-tables
(with-upgradability ()
  (defun ensure-gethash (key table default)
    "Lookup the TABLE for a KEY as by GETHASH, but if not present,
call the (possibly constant) function designated by DEFAULT as per CALL-FUNCTION,
set the corresponding entry to the result in the table.
Return two values: the entry after its optional computation, and whether it was found"
    (multiple-value-bind (value foundp) (gethash key table)
      (values
       (if foundp
           value
           (setf (gethash key table) (call-function default)))
       foundp)))

  (defun list-to-hash-set (list &aux (h (make-hash-table :test 'equal)))
    "Convert a LIST into hash-table that has the same elements when viewed as a set,
up to the given equality TEST"
    (dolist (x list h) (setf (gethash x h) t))))


;;; Version handling
(with-upgradability ()
  (defun unparse-version (version-list)
    (format nil "~{~D~^.~}" version-list))

  (defun parse-version (version-string &optional on-error)
    "Parse a VERSION-STRING as a series of natural integers separated by dots.
Return a (non-null) list of integers if the string is valid;
otherwise return NIL.

When invalid, ON-ERROR is called as per CALL-FUNCTION before to return NIL,
with format arguments explaining why the version is invalid.
ON-ERROR is also called if the version is not canonical
in that it doesn't print back to itself, but the list is returned anyway."
    (block nil
      (unless (stringp version-string)
        (call-function on-error "~S: ~S is not a string" 'parse-version version-string)
        (return))
      (unless (loop :for prev = nil :then c :for c :across version-string
                    :always (or (digit-char-p c)
                                (and (eql c #\.) prev (not (eql prev #\.))))
                    :finally (return (and c (digit-char-p c))))
        (call-function on-error "~S: ~S doesn't follow asdf version numbering convention"
                       'parse-version version-string)
        (return))
      (let* ((version-list
               (mapcar #'parse-integer (split-string version-string :separator ".")))
             (normalized-version (unparse-version version-list)))
        (unless (equal version-string normalized-version)
          (call-function on-error "~S: ~S contains leading zeros" 'parse-version version-string))
        version-list)))

  (defun lexicographic< (< x y)
    (cond ((null y) nil)
          ((null x) t)
          ((funcall < (car x) (car y)) t)
          ((funcall < (car y) (car x)) nil)
          (t (lexicographic< < (cdr x) (cdr y)))))

  (defun lexicographic<= (< x y)
    (not (lexicographic< < y x)))

  (defun version< (version1 version2)
    (let ((v1 (parse-version version1 nil))
          (v2 (parse-version version2 nil)))
      (lexicographic< '< v1 v2)))

  (defun version<= (version1 version2)
    (not (version< version2 version1)))

  (defun version-compatible-p (provided-version required-version)
    "Is the provided version a compatible substitution for the required-version?
If major versions differ, it's not compatible.
If they are equal, then any later version is compatible,
with later being determined by a lexicographical comparison of minor numbers."
    (let ((x (parse-version provided-version nil))
          (y (parse-version required-version nil)))
      (and x y (= (car x) (car y)) (lexicographic<= '< (cdr y) (cdr x))))))


;;; Condition control

(with-upgradability ()
  (defparameter +simple-condition-format-control-slot+
    #+abcl 'system::format-control
    #+allegro 'excl::format-control
    #+clisp 'system::$format-control
    #+clozure 'ccl::format-control
    #+(or cmu scl) 'conditions::format-control
    #+(or ecl mkcl) 'si::format-control
    #+(or gcl lispworks) 'conditions::format-string
    #+sbcl 'sb-kernel:format-control
    #-(or abcl allegro clisp clozure cmu ecl gcl lispworks mkcl sbcl scl) nil
    "Name of the slot for FORMAT-CONTROL in simple-condition")

  (defun match-condition-p (x condition)
    "Compare received CONDITION to some pattern X:
a symbol naming a condition class,
a simple vector of length 2, arguments to find-symbol* with result as above,
or a string describing the format-control of a simple-condition."
    (etypecase x
      (symbol (typep condition x))
      ((simple-vector 2)
       (ignore-errors (typep condition (find-symbol* (svref x 0) (svref x 1) nil))))
      (function (funcall x condition))
      (string (and (typep condition 'simple-condition)
                   ;; On SBCL, it's always set and the check triggers a warning
                   #+(or allegro clozure cmu lispworks scl)
                   (slot-boundp condition +simple-condition-format-control-slot+)
                   (ignore-errors (equal (simple-condition-format-control condition) x))))))

  (defun match-any-condition-p (condition conditions)
    "match CONDITION against any of the patterns of CONDITIONS supplied"
    (loop :for x :in conditions :thereis (match-condition-p x condition)))

  (defun call-with-muffled-conditions (thunk conditions)
    "calls the THUNK in a context where the CONDITIONS are muffled"
    (handler-bind ((t #'(lambda (c) (when (match-any-condition-p c conditions)
                                      (muffle-warning c)))))
      (funcall thunk)))

  (defmacro with-muffled-conditions ((conditions) &body body)
    "Shorthand syntax for CALL-WITH-MUFFLED-CONDITIONS"
    `(call-with-muffled-conditions #'(lambda () ,@body) ,conditions)))

