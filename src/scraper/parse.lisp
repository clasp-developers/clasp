(in-package :cscrape)

(defvar +white-space+ '(#\space #\return #\tab))

(defun maybe-remove-one-prefix-from-start (instr prefixes)
  (let ((str (string-trim '(#\newline #\space #\tab) instr)))
    (block done
      (dolist (prefix prefixes)
        (let ((pos (search prefix str)))
          (when pos
              (when (= pos 0)
                  (return-from done (string-trim '(#\newline #\space #\tab) (subseq str (length prefix))))))))
      str)))


(defun backwards-space (str pos)
  (let ((pos (position-if (lambda (c) (or (char= c #\space) (char= c #\tab) (char= c #\newline))) str :from-end t :end pos)))
    (or pos -1)))
    

(defun backwards-not-space (str pos)
  (let ((pos (position-if (lambda (c) (not (or (char= c #\space) (char= c #\tab) (char= c #\newline)))) str :from-end t :end pos)))
    (or pos -1)))

(defun type-and-name-from-signature (sig)
  "* Arguments
- sig :: A string.
* Description
Split a signature like 'a b c d(...' and return (values c d)"
  (let* ((close-paren (position #\( sig))
	 (cur (backwards-not-space sig close-paren))
	 (name-end (1+ cur))
	 (cur (backwards-space sig cur))
	 (name-start (1+ cur))
	 (type-end (1+ (backwards-not-space sig cur)))
	 (type-start (1+ (backwards-space sig cur))))
    (values (subseq sig type-start type-end) (subseq sig name-start name-end))))

(defun extract-function-name-from-signature (raw-sig tag)
  "* Arguments
- sig :: Function signature as string.
- tag :: the tag that provided the signature.
* Return
values function-name full-function-name simple-function
* Description
Extract the function name from the signature.  If the function name does not contain '::' then
return the (values function-name function-name T).
If the name has the form: class::name then it's a static class method 
and not a simple-function so return (values name class::name nil)"
  (declare (optimize (speed 3)) (ignore tag))
  (let* ((sig (maybe-remove-one-prefix-from-start raw-sig '("inline" "static")))
	 (tsig (string-trim '(#\newline #\space #\tab) sig))
         (first-space (position-if
                       (lambda (c) (or (char= c #\newline)
                                       (char= c #\space)
                                       (char= c #\tab)))
                       tsig))
         (open-paren (position #\( tsig :test #'char=)))
    (when (> first-space open-paren)
      (error "The function signature \"~a\"has a problem - it may be missing a return type~%" tsig))
    (let* ((full-function-name (string-left-trim '(#\newline #\space #\tab #\*)
                                                 (string-right-trim '(#\newline #\space #\tab) (subseq tsig first-space open-paren))))
           (colon-colon-pos (search "::" full-function-name)))
      (if colon-colon-pos
          (values (subseq full-function-name (+ 2 colon-colon-pos)) full-function-name nil)
          (values full-function-name full-function-name t)))))

(defun maybe-remove-cast (str)
  (let* ((tstr (string-trim '(#\newline #\space #\tab) str))
         (close-paren (position #\( tstr :from-end t)))
    (if close-paren
        (string-trim '(#\newline #\space #\tab) (subseq tstr (1+ close-paren)))
        tstr)))

(defun extract-class-method-name-from-signature (sig)
  (multiple-value-bind (return-type class-method-name)
      (type-and-name-from-signature sig)
    (let ((colon-colon-pos (search "::" class-method-name)))
      (if colon-colon-pos
          (values (subseq class-method-name 0 colon-colon-pos)
                  (subseq class-method-name (+ 2 colon-colon-pos)))
          (values nil class-method-name)))))
    
(defun extract-method-name-from-signature (sig)
  (declare (optimize (debug 3)))
  (let* ((tsig (maybe-remove-one-prefix-from-start sig '("virtual" "inline")))
         (first-sep (position-if
                     (lambda (c) (or (char= c #\newline)
                                     (char= c #\space)
                                     (char= c #\tab)))
                     tsig))
         (first-name-char (position-if
                           (lambda (c) (not (or (char= c #\newline)
                                                (char= c #\space)
                                                (char= c #\tab)
                                                (char= c #\*)))) ;; skip *
                           tsig
                           :start first-sep))
         (open-paren (position #\( tsig :test #'char= :start first-name-char))
         (class-method (string-left-trim '(#\newline #\space #\tab #\*) (string-right-trim '(#\newline #\space #\tab) (subseq tsig first-name-char open-paren))))
         (colon-colon-pos (search "::" class-method)))
    (if colon-colon-pos
        (values (subseq class-method 0 colon-colon-pos)
                (subseq class-method (+ 2 colon-colon-pos)))
        (values nil class-method))))

(defun extract-function-name-from-pointer (typed-pointer tag)
  (declare (optimize debug))
  (let ((pointer-pos (position #\& typed-pointer)))
    (unless pointer-pos
      (error 'bad-pointer :pointer-text typed-pointer :tag tag))
    (let* ((just-pointer (subseq typed-pointer pointer-pos))
           (trimmed-pointer (string-trim '(#\& #\newline #\space #\tab) just-pointer))
           (str1 (maybe-remove-cast trimmed-pointer))
           (colon-colon-pos (search "::" str1 :test #'string= :from-end t)))
      (if colon-colon-pos
          (subseq trimmed-pointer (+ 2 colon-colon-pos) nil)
          trimmed-pointer))))

(defun extract-method-name-from-pointer (pointer tag)
  (extract-function-name-from-pointer pointer tag))


(defun split-by-one-char (string split-char)
  "* Arguments
- string :: A string.
- split-char :: A character to split at.
* Description
Returns a list of substrings of string
divided by ONE split-char each.
Note: Two consecutive split-char will be seen as
if there were an empty string between them."
  (loop for i = 0 then (1+ j)
     as j = (position split-char string :start i)
     collect (string-trim '(#\space #\return #\tab) (subseq string i j))
     while j))

(defun split-type-name (type-name)
  "* Arguments
- type-name :: A string.
* Description
Split a string like \"const string &b\" into (values \"const string &\" \"b\") pair.  
Trim whitespace from each member of the pair."
  (declare (optimize (speed 3)))
  (let* ((name-start (position-if #'(lambda (c)
                                      (not (or (alphanumericp c) (char= c #\_))))
                                  type-name
                                  :from-end t))
         (first (subseq type-name 0 (1+ name-start)))
         (second (subseq type-name (1+ name-start))))
    (values (string-trim +white-space+ first) (string-trim +white-space+ second))))

(defun extract-lambda-list-from-c++-arguments (typed-arguments)
  "* Arguments
- typed-arguments :: A string
* Description
Split the typed arguments (int a, int b, string c)
into two lists (int int string) and (a b c) and return as two values"
  (declare (optimize (speed 3)))
  (if (or (string= typed-arguments "")
          (string= typed-arguments "void"))
      ""
      (let* ((split-args (split-by-one-char typed-arguments #\,))
             (lambda-list (with-output-to-string (sout)
                            (dolist (type-arg split-args)
                              (multiple-value-bind (type argname)
                                  (split-type-name type-arg)
                                (when (string= argname "t")
                                  (error "An argument list ~a had the name T - Common Lisp won't like that - change it to something else" typed-arguments))
                                (format sout "~a " argname))))))
        (string-trim +white-space+ lambda-list))))

(defun prepend-dispatch-variable (lambda-list class)
  (with-output-to-string (sout)
    (format sout "(this !) ")
    (format sout "~a" lambda-list)))

(defun parse-lambda-list-from-signature (signature &key class)
  "* Arguments 
- signature :: A string.
* Description
- Extract from the C++ function arguments a lambda list."
  (let* ((open-paren (position #\( signature))
         (close-paren (position #\) signature))
         (arguments (string-trim +white-space+ (subseq signature (1+ open-paren) close-paren)))
         (lambda-list (extract-lambda-list-from-c++-arguments arguments)))
    (if class
        (prepend-dispatch-variable lambda-list class)
        lambda-list)))


(defun strip-non-type-keywords (maybe-type)
  (flet ((strip-keyword (keyword line)
           (let ((pos (search keyword line)))
             (if pos
                 (string-trim
                  +white-space+
                  (concatenate 'string
                               (subseq line 0 pos)
                               (subseq line (+ pos (length keyword)) nil)))
                 line))))
    (strip-keyword "inline" (strip-keyword "static" maybe-type))))


(defun parse-types-from-signature (rsignature)
  (declare (optimize debug))
  (let* ((trimmed (string-trim +white-space+ rsignature))
         (open-paren (position #\( trimmed))
         (close-paren (position #\) trimmed))
         (front (subseq trimmed 0 open-paren))
         (args (string-trim +white-space+ (subseq trimmed (1+ open-paren) close-paren)))
         (split-args (progn
                       (if (not (or (string= args "void") (string= args "")))
                           (mapcar (lambda (x) (string-trim +white-space+ x))
                                   (split-by-one-char args #\,))
                           nil)))
         (arg-types (progn
                      (if split-args
                          (mapcar (lambda (x)
                                    (let ((pos (position-if
                                                (lambda (c)
                                                  (member c (list* #\& #\* +white-space+)))
                                                x :from-end t)))
                                      (string-trim
                                       +white-space+
                                       (subseq x 0 (1+ pos)))))
                                  split-args))))
         (return-pos (position-if
                      (lambda (c)
                        (member c (list* #\& #\* +white-space+)))
                      front :from-end t))
         (maybe-return-type (string-trim
                             +white-space+
                             (subseq front 0 return-pos)))
         (return-type (strip-non-type-keywords maybe-return-type)))
    (values return-type arg-types)))


(defun maybe-fix-magic-name (maybe-magic-name)
  "* Arguments
:: maybe-magic-name - A string
* Description
If the string contains core::magic_name then transform it into a
CL call to (core:magic-name ...)"
  (let ((magic-pos (search "core::magic_name" maybe-magic-name)))
    (if magic-pos
        (let* ((open-paren (position #\( maybe-magic-name))
               (close-paren (position #\) maybe-magic-name))
               (args (subseq maybe-magic-name (1+ open-paren) close-paren))
               (comma-pos (position #\, args)))
          (if comma-pos
              (format nil "(core:magic-intern ~a ~a)" (subseq args 0 comma-pos) (subseq args (1+ comma-pos) nil))
              (format nil "(core:magic-intern ~a)" args)))
        (format nil "(core:magic-intern ~a)" maybe-magic-name))))

          
