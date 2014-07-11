;;;; -------------------------------------------------------------------------
;;;; run-program initially from xcvb-driver.

(uiop/package:define-package :uiop/run-program
  (:nicknames :asdf/run-program)
  (:recycle :uiop/run-program :asdf/run-program :xcvb-driver)
  (:use :uiop/common-lisp :uiop/package :uiop/utility
   :uiop/pathname :uiop/os :uiop/filesystem :uiop/stream)
  (:export
   ;;; Escaping the command invocation madness
   #:easy-sh-character-p #:escape-sh-token #:escape-sh-command
   #:escape-windows-token #:escape-windows-command
   #:escape-token #:escape-command

   ;;; run-program
   #:slurp-input-stream #:vomit-output-stream
   #:run-program
   #:subprocess-error
   #:subprocess-error-code #:subprocess-error-command #:subprocess-error-process
   ))
(in-package :uiop/run-program)

;;;; ----- Escaping strings for the shell -----

(with-upgradability ()
  (defun requires-escaping-p (token &key good-chars bad-chars)
    "Does this token require escaping, given the specification of
either good chars that don't need escaping or bad chars that do need escaping,
as either a recognizing function or a sequence of characters."
    (some
     (cond
       ((and good-chars bad-chars)
        (error "only one of good-chars and bad-chars can be provided"))
       ((functionp good-chars)
        (complement good-chars))
       ((functionp bad-chars)
        bad-chars)
       ((and good-chars (typep good-chars 'sequence))
        #'(lambda (c) (not (find c good-chars))))
       ((and bad-chars (typep bad-chars 'sequence))
        #'(lambda (c) (find c bad-chars)))
       (t (error "requires-escaping-p: no good-char criterion")))
     token))

  (defun escape-token (token &key stream quote good-chars bad-chars escaper)
    "Call the ESCAPER function on TOKEN string if it needs escaping as per
REQUIRES-ESCAPING-P using GOOD-CHARS and BAD-CHARS, otherwise output TOKEN,
using STREAM as output (or returning result as a string if NIL)"
    (if (requires-escaping-p token :good-chars good-chars :bad-chars bad-chars)
        (with-output (stream)
          (apply escaper token stream (when quote `(:quote ,quote))))
        (output-string token stream)))

  (defun escape-windows-token-within-double-quotes (x &optional s)
    "Escape a string token X within double-quotes
for use within a MS Windows command-line, outputing to S."
    (labels ((issue (c) (princ c s))
             (issue-backslash (n) (loop :repeat n :do (issue #\\))))
      (loop
        :initially (issue #\") :finally (issue #\")
        :with l = (length x) :with i = 0
        :for i+1 = (1+ i) :while (< i l) :do
          (case (char x i)
            ((#\") (issue-backslash 1) (issue #\") (setf i i+1))
            ((#\\)
             (let* ((j (and (< i+1 l) (position-if-not
                                       #'(lambda (c) (eql c #\\)) x :start i+1)))
                    (n (- (or j l) i)))
               (cond
                 ((null j)
                  (issue-backslash (* 2 n)) (setf i l))
                 ((and (< j l) (eql (char x j) #\"))
                  (issue-backslash (1+ (* 2 n))) (issue #\") (setf i (1+ j)))
                 (t
                  (issue-backslash n) (setf i j)))))
            (otherwise
             (issue (char x i)) (setf i i+1))))))

  (defun escape-windows-token (token &optional s)
    "Escape a string TOKEN within double-quotes if needed
for use within a MS Windows command-line, outputing to S."
    (escape-token token :stream s :bad-chars #(#\space #\tab #\") :quote nil
                        :escaper 'escape-windows-token-within-double-quotes))

  (defun escape-sh-token-within-double-quotes (x s &key (quote t))
    "Escape a string TOKEN within double-quotes
for use within a POSIX Bourne shell, outputing to S;
omit the outer double-quotes if key argument :QUOTE is NIL"
    (when quote (princ #\" s))
    (loop :for c :across x :do
      (when (find c "$`\\\"") (princ #\\ s))
      (princ c s))
    (when quote (princ #\" s)))

  (defun easy-sh-character-p (x)
    "Is X an \"easy\" character that does not require quoting by the shell?"
    (or (alphanumericp x) (find x "+-_.,%@:/")))

  (defun escape-sh-token (token &optional s)
    "Escape a string TOKEN within double-quotes if needed
for use within a POSIX Bourne shell, outputing to S."
    (escape-token token :stream s :quote #\" :good-chars #'easy-sh-character-p
                        :escaper 'escape-sh-token-within-double-quotes))

  (defun escape-shell-token (token &optional s)
    "Escape a token for the current operating system shell"
    (cond
      ((os-unix-p) (escape-sh-token token s))
      ((os-windows-p) (escape-windows-token token s))))

  (defun escape-command (command &optional s
                                  (escaper 'escape-shell-token))
    "Given a COMMAND as a list of tokens, return a string of the
spaced, escaped tokens, using ESCAPER to escape."
    (etypecase command
      (string (output-string command s))
      (list (with-output (s)
              (loop :for first = t :then nil :for token :in command :do
                (unless first (princ #\space s))
                (funcall escaper token s))))))

  (defun escape-windows-command (command &optional s)
    "Escape a list of command-line arguments into a string suitable for parsing
by CommandLineToArgv in MS Windows"
    ;; http://msdn.microsoft.com/en-us/library/bb776391(v=vs.85).aspx
    ;; http://msdn.microsoft.com/en-us/library/17w5ykft(v=vs.85).aspx
    (escape-command command s 'escape-windows-token))

  (defun escape-sh-command (command &optional s)
    "Escape a list of command-line arguments into a string suitable for parsing
by /bin/sh in POSIX"
    (escape-command command s 'escape-sh-token))

  (defun escape-shell-command (command &optional stream)
    "Escape a command for the current operating system's shell"
    (escape-command command stream 'escape-shell-token)))


;;;; Slurping a stream, typically the output of another program
(with-upgradability ()
  (defun call-stream-processor (fun processor stream)
    "Given FUN (typically SLURP-INPUT-STREAM or VOMIT-OUTPUT-STREAM,
a PROCESSOR specification which is either an atom or a list specifying
a processor an keyword arguments, call the specified processor with
the given STREAM as input"
    (if (consp processor)
        (apply fun (first processor) stream (rest processor))
        (funcall fun processor stream)))

  (defgeneric slurp-input-stream (processor input-stream &key)
    (:documentation
     "SLURP-INPUT-STREAM is a generic function with two positional arguments
PROCESSOR and INPUT-STREAM and additional keyword arguments, that consumes (slurps)
the contents of the INPUT-STREAM and processes them according to a method
specified by PROCESSOR.

Built-in methods include the following:
* if PROCESSOR is a function, it is called with the INPUT-STREAM as its argument
* if PROCESSOR is a list, its first element should be a function.  It will be applied to a cons of the
  INPUT-STREAM and the rest of the list.  That is (x . y) will be treated as
    \(APPLY x <stream> y\)
* if PROCESSOR is an output-stream, the contents of INPUT-STREAM is copied to the output-stream,
  per copy-stream-to-stream, with appropriate keyword arguments.
* if PROCESSOR is the symbol CL:STRING or the keyword :STRING, then the contents of INPUT-STREAM
  are returned as a string, as per SLURP-STREAM-STRING.
* if PROCESSOR is the keyword :LINES then the INPUT-STREAM will be handled by SLURP-STREAM-LINES.
* if PROCESSOR is the keyword :LINE then the INPUT-STREAM will be handled by SLURP-STREAM-LINE.
* if PROCESSOR is the keyword :FORMS then the INPUT-STREAM will be handled by SLURP-STREAM-FORMS.
* if PROCESSOR is the keyword :FORM then the INPUT-STREAM will be handled by SLURP-STREAM-FORM.
* if PROCESSOR is T, it is treated the same as *standard-output*. If it is NIL, NIL is returned.

Programmers are encouraged to define their own methods for this generic function."))

  #-genera
  (defmethod slurp-input-stream ((function function) input-stream &key)
    (funcall function input-stream))

  (defmethod slurp-input-stream ((list cons) input-stream &key)
    (apply (first list) input-stream (rest list)))

  #-genera
  (defmethod slurp-input-stream ((output-stream stream) input-stream
                                 &key linewise prefix (element-type 'character) buffer-size)
    (copy-stream-to-stream
     input-stream output-stream
     :linewise linewise :prefix prefix :element-type element-type :buffer-size buffer-size))

  (defmethod slurp-input-stream ((x (eql 'string)) stream &key stripped)
    (slurp-stream-string stream :stripped stripped))

  (defmethod slurp-input-stream ((x (eql :string)) stream &key stripped)
    (slurp-stream-string stream :stripped stripped))

  (defmethod slurp-input-stream ((x (eql :lines)) stream &key count)
    (slurp-stream-lines stream :count count))

  (defmethod slurp-input-stream ((x (eql :line)) stream &key (at 0))
    (slurp-stream-line stream :at at))

  (defmethod slurp-input-stream ((x (eql :forms)) stream &key count)
    (slurp-stream-forms stream :count count))

  (defmethod slurp-input-stream ((x (eql :form)) stream &key (at 0))
    (slurp-stream-form stream :at at))

  (defmethod slurp-input-stream ((x (eql t)) stream &rest keys &key &allow-other-keys)
    (apply 'slurp-input-stream *standard-output* stream keys))

  (defmethod slurp-input-stream ((x null) (stream t) &key)
    nil)

  (defmethod slurp-input-stream ((pathname pathname) input
                                 &key
                                   (element-type *default-stream-element-type*)
                                   (external-format *utf-8-external-format*)
                                   (if-exists :rename-and-delete)
                                   (if-does-not-exist :create)
                                   buffer-size
                                   linewise)
    (with-output-file (output pathname
                              :element-type element-type
                              :external-format external-format
                              :if-exists if-exists
                              :if-does-not-exist if-does-not-exist)
      (copy-stream-to-stream
       input output
       :element-type element-type :buffer-size buffer-size :linewise linewise)))

  (defmethod slurp-input-stream (x stream
                                 &key linewise prefix (element-type 'character) buffer-size)
    (declare (ignorable stream linewise prefix element-type buffer-size))
    (cond
      #+genera
      ((functionp x) (funcall x stream))
      #+genera
      ((output-stream-p x)
       (copy-stream-to-stream
        stream x
        :linewise linewise :prefix prefix :element-type element-type :buffer-size buffer-size))
      (t
       (error "Invalid ~S destination ~S" 'slurp-input-stream x)))))


(with-upgradability ()
  (defgeneric vomit-output-stream (processor output-stream &key)
    (:documentation
     "VOMIT-OUTPUT-STREAM is a generic function with two positional arguments
PROCESSOR and OUTPUT-STREAM and additional keyword arguments, that produces (vomits)
some content onto the OUTPUT-STREAM, according to a method specified by PROCESSOR.

Built-in methods include the following:
* if PROCESSOR is a function, it is called with the OUTPUT-STREAM as its argument
* if PROCESSOR is a list, its first element should be a function.
  It will be applied to a cons of the OUTPUT-STREAM and the rest of the list.
  That is (x . y) will be treated as \(APPLY x <stream> y\)
* if PROCESSOR is an input-stream, its contents will be copied the OUTPUT-STREAM,
  per copy-stream-to-stream, with appropriate keyword arguments.
* if PROCESSOR is a string, its contents will be printed to the OUTPUT-STREAM.
* if PROCESSOR is T, it is treated the same as *standard-input*. If it is NIL, nothing is done.

Programmers are encouraged to define their own methods for this generic function."))

  #-genera
  (defmethod vomit-output-stream ((function function) output-stream &key)
    (funcall function output-stream))

  (defmethod vomit-output-stream ((list cons) output-stream &key)
    (apply (first list) output-stream (rest list)))

  #-genera
  (defmethod vomit-output-stream ((input-stream stream) output-stream
                                 &key linewise prefix (element-type 'character) buffer-size)
    (copy-stream-to-stream
     input-stream output-stream
     :linewise linewise :prefix prefix :element-type element-type :buffer-size buffer-size))

  (defmethod vomit-output-stream ((x string) stream &key fresh-line terpri)
    (princ x stream)
    (when fresh-line (fresh-line stream))
    (when terpri (terpri stream))
    (values))

  (defmethod vomit-output-stream ((x (eql t)) stream &rest keys &key &allow-other-keys)
    (apply 'vomit-output-stream *standard-input* stream keys))

  (defmethod vomit-output-stream ((x null) (stream t) &key)
    (values))

  (defmethod vomit-output-stream ((pathname pathname) input
                                 &key
                                   (element-type *default-stream-element-type*)
                                   (external-format *utf-8-external-format*)
                                   (if-exists :rename-and-delete)
                                   (if-does-not-exist :create)
                                   buffer-size
                                   linewise)
    (with-output-file (output pathname
                              :element-type element-type
                              :external-format external-format
                              :if-exists if-exists
                              :if-does-not-exist if-does-not-exist)
      (copy-stream-to-stream
       input output
       :element-type element-type :buffer-size buffer-size :linewise linewise)))

  (defmethod vomit-output-stream (x stream
                                 &key linewise prefix (element-type 'character) buffer-size)
    (declare (ignorable stream linewise prefix element-type buffer-size))
    (cond
      #+genera
      ((functionp x) (funcall x stream))
      #+genera
      ((input-stream-p x)
       (copy-stream-to-stream
        x stream
        :linewise linewise :prefix prefix :element-type element-type :buffer-size buffer-size))
      (t
       (error "Invalid ~S source ~S" 'vomit-output-stream x)))))


;;;; ----- Running an external program -----
;;; Simple variant of run-program with no input, and capturing output
;;; On some implementations, may output to a temporary file...
(with-upgradability ()
  (define-condition subprocess-error (error)
    ((code :initform nil :initarg :code :reader subprocess-error-code)
     (command :initform nil :initarg :command :reader subprocess-error-command)
     (process :initform nil :initarg :process :reader subprocess-error-process))
    (:report (lambda (condition stream)
               (format stream "Subprocess~@[ ~S~]~@[ run with command ~S~] exited with error~@[ code ~D~]"
                       (subprocess-error-process condition)
                       (subprocess-error-command condition)
                       (subprocess-error-code condition)))))

  ;;; Internal helpers for run-program
  (defun %normalize-command (command)
    "Given a COMMAND as a list or string, transform it in a format suitable
for the implementation's underlying run-program function"
    (etypecase command
      #+os-unix (string `("/bin/sh" "-c" ,command))
      #+os-unix (list command)
      #+os-windows
      (string
       #+mkcl (list "cmd" '#:/c command)
       ;; NB: We do NOT add cmd /c here. You might want to.
       #+(or allegro clisp) command
       ;; On ClozureCL for Windows, we assume you are using
       ;; r15398 or later in 1.9 or later,
       ;; so that bug 858 is fixed http://trac.clozure.com/ccl/ticket/858
       #+clozure (cons "cmd" (strcat "/c " command))
       ;; NB: On other Windows implementations, this is utterly bogus
       ;; except in the most trivial cases where no quoting is needed.
       ;; Use at your own risk.
       #-(or allegro clisp clozure mkcl) (list "cmd" "/c" command))
      #+os-windows
      (list
       #+allegro (escape-windows-command command)
       #-allegro command)))

  (defun %active-io-specifier-p (specifier)
    "Determines whether a run-program I/O specifier requires Lisp-side processing
via SLURP-INPUT-STREAM or VOMIT-OUTPUT-STREAM (return T),
or whether it's already taken care of by the implementation's underlying run-program."
    (not (typep specifier '(or null string pathname (member :interactive :output)
                            #+(or cmu (and sbcl os-unix) scl) (or stream (eql t))
                            #+lispworks file-stream)))) ;; not a type!? comm:socket-stream

  (defun %normalize-io-specifier (specifier &optional role)
    "Normalizes a portable I/O specifier for %RUN-PROGRAM into an implementation-dependent
argument to pass to the internal RUN-PROGRAM"
    (declare (ignorable role))
    (etypecase specifier
      (null (or #+(or allegro lispworks) (null-device-pathname)))
      (string (parse-native-namestring specifier))
      (pathname specifier)
      (stream specifier)
      ((eql :stream) :stream)
      ((eql :interactive)
       #+allegro nil
       #+clisp :terminal
       #+(or clozure cmu ecl mkcl sbcl scl) t)
      #+(or allegro clozure cmu ecl lispworks mkcl sbcl scl)
      ((eql :output)
       (if (eq role :error-output)
           :output
           (error "Wrong specifier ~S for role ~S" specifier role)))))

  (defun %interactivep (input output error-output)
    (member :interactive (list input output error-output)))

  #+clisp
  (defun clisp-exit-code (raw-exit-code)
    (typecase raw-exit-code
      (null 0) ; no error
      (integer raw-exit-code) ; negative: signal
      (t -1)))

  (defun %run-program (command
                       &rest keys
                       &key input (if-input-does-not-exist :error)
                         output (if-output-exists :overwrite)
                         error-output (if-error-output-exists :overwrite)
                         directory wait
                         #+allegro separate-streams
                         &allow-other-keys)
    "A portable abstraction of a low-level call to the implementation's run-program or equivalent.
It spawns a subprocess that runs the specified COMMAND (a list of program and arguments).
INPUT, OUTPUT and ERROR-OUTPUT specify a portable IO specifer,
to be normalized by %NORMALIZE-IO-SPECIFIER.
It returns a process-info plist with possible keys:
     PROCESS, EXIT-CODE, INPUT-STREAM, OUTPUT-STREAM, BIDIR-STREAM, ERROR-STREAM."
    ;; NB: these implementations have unix vs windows set at compile-time.
    (declare (ignorable directory if-input-does-not-exist if-output-exists if-error-output-exists))
    (assert (not (and wait (member :stream (list input output error-output)))))
    #-(or allegro clisp clozure cmu (and lispworks os-unix) mkcl sbcl scl)
    (progn command keys directory
           (error "run-program not available"))
    #+(or allegro clisp clozure cmu (and lispworks os-unix) mkcl sbcl scl)
    (let* ((%command (%normalize-command command))
           (%input (%normalize-io-specifier input :input))
           (%output (%normalize-io-specifier output :output))
           (%error-output (%normalize-io-specifier error-output :error-output))
           #+(and allegro os-windows) (interactive (%interactivep input output error-output))
           (process*
             #+allegro
             (multiple-value-list
              (apply
               'excl:run-shell-command
               #+os-unix (coerce (cons (first %command) %command) 'vector)
               #+os-windows %command
               :input %input
               :output %output
               :error-output %error-output
               :directory directory :wait wait
               #+os-windows :show-window #+os-windows (if interactive nil :hide)
               :allow-other-keys t keys))
             #-allegro
             (with-current-directory (#-(or sbcl mkcl) directory)
               #+clisp
               (flet ((run (f x &rest args)
                        (multiple-value-list
                         (apply f x :input %input :output %output
                                    :allow-other-keys t `(,@args ,@keys)))))
                 (assert (eq %error-output :terminal))
                 ;;; since we now always return a code, we can't use this code path, anyway!
                 (etypecase %command
                   #+os-windows (string (run 'ext:run-shell-command %command))
                   (list (run 'ext:run-program (car %command)
                              :arguments (cdr %command)))))
               #+(or clozure cmu ecl mkcl sbcl scl)
               (#-(or ecl mkcl) progn #+(or ecl mkcl) multiple-value-list
                (apply
                 '#+(or cmu ecl scl) ext:run-program
                 #+clozure ccl:run-program #+sbcl sb-ext:run-program #+mkcl mk-ext:run-program
                 (car %command) (cdr %command)
                 :input %input
                 :output %output
                 :error %error-output
                 :wait wait
                 :allow-other-keys t
                 (append
                  #+(or clozure cmu mkcl sbcl scl)
                  `(:if-input-does-not-exist ,if-input-does-not-exist
                    :if-output-exists ,if-output-exists
                    :if-error-exists ,if-error-output-exists)
                  #+sbcl `(:search t
                           :if-output-does-not-exist :create
                           :if-error-does-not-exist :create)
                  #-sbcl keys #+sbcl (if directory keys (remove-plist-key :directory keys)))))
               #+(and lispworks os-unix) ;; note: only used on Unix in non-interactive case
               (multiple-value-list
                (apply
                 'system:run-shell-command
                 (cons "/usr/bin/env" %command) ; lispworks wants a full path.
                 :input %input :if-input-does-not-exist if-input-does-not-exist
                 :output %output :if-output-exists if-output-exists
                 :error-output %error-output :if-error-output-exists if-error-output-exists
                 :wait wait :save-exit-status t :allow-other-keys t keys))))
           (process-info-r ()))
      (flet ((prop (key value) (push key process-info-r) (push value process-info-r)))
        #+allegro
        (cond
          (wait (prop :exit-code (first process*)))
          (separate-streams
           (destructuring-bind (in out err pid) process*
             (prop :process pid)
             (when (eq input :stream) (prop :input-stream in))
             (when (eq output :stream) (prop :output-stream out))
             (when (eq error-output :stream) (prop :error-stream err))))
          (t
           (prop :process (third process*))
           (let ((x (first process*)))
             (ecase (+ (if (eq input :stream) 1 0) (if (eq output :stream) 2 0))
               (0)
               (1 (prop :input-stream x))
               (2 (prop :output-stream x))
               (3 (prop :bidir-stream x))))
           (when (eq error-output :stream)
             (prop :error-stream (second process*)))))
        #+clisp
        (cond
          (wait (prop :exit-code (clisp-exit-code (first process*))))
          (t
           (ecase (+ (if (eq input :stream) 1 0) (if (eq output :stream) 2 0))
             (0)
             (1 (prop :input-stream (first process*)))
             (2 (prop :output-stream (first process*)))
             (3 (prop :bidir-stream (pop process*))
                (prop :input-stream (pop process*))
                (prop :output-stream (pop process*))))))
        #+(or clozure cmu sbcl scl)
        (progn
          (prop :process process*)
          (when (eq input :stream)
            (prop :input-stream
                  #+clozure (ccl:external-process-input-stream process*)
                  #+(or cmu scl) (ext:process-input process*)
                  #+sbcl (sb-ext:process-input process*)))
          (when (eq output :stream)
            (prop :output-stream
                  #+clozure (ccl:external-process-output-stream process*)
                  #+(or cmu scl) (ext:process-output process*)
                  #+sbcl (sb-ext:process-output process*)))
          (when (eq error-output :stream)
            (prop :error-output-stream
                  #+clozure (ccl:external-process-error-stream process*)
                  #+(or cmu scl) (ext:process-error process*)
                  #+sbcl (sb-ext:process-error process*))))
        #+(or ecl mkcl)
        (destructuring-bind #+ecl (stream code process) #+mkcl (stream process code) process*
          (let ((mode (+ (if (eq input :stream) 1 0) (if (eq output :stream) 2 0))))
            (cond
              ((zerop mode))
              ((null process*) (prop :exit-code -1))
              (t (prop (case mode (1 :input-stream) (2 :output-stream) (3 :bidir-stream)) stream))))
          (when code (prop :exit-code code))
          (when process (prop :process process)))
        #+lispworks
        (if wait
            (prop :exit-code (first process*))
            (let ((mode (+ (if (eq input :stream) 1 0) (if (eq output :stream) 2 0))))
              (if (zerop mode)
                  (prop :process (first process*))
                  (destructuring-bind (x err pid) process*
                    (prop :process pid)
                    (prop (ecase mode (1 :input-stream) (2 :output-stream) (3 :bidir-stream)) x)
                    (when (eq error-output :stream) (prop :error-stream err))))))
        (nreverse process-info-r))))

  (defun %process-info-pid (process-info)
    (let ((process (getf process-info :process)))
      (declare (ignorable process))
      #+(or allegro lispworks) process
      #+clozure (ccl::external-process-pid process)
      #+ecl (si:external-process-pid process)
      #+(or cmu scl) (ext:process-pid process)
      #+mkcl (mkcl:process-id process)
      #+sbcl (sb-ext:process-pid process)
      #-(or allegro cmu mkcl sbcl scl) (error "~S not implemented" '%process-info-pid)))

  (defun %wait-process-result (process-info)
    (or (getf process-info :exit-code)
        (let ((process (getf process-info :process)))
          (when process
            ;; 1- wait
            #+clozure (ccl::external-process-wait process)
            #+(or cmu scl) (ext:process-wait process)
            #+(and ecl os-unix) (ext:external-process-wait process)
            #+sbcl (sb-ext:process-wait process)
            ;; 2- extract result
            #+allegro (sys:reap-os-subprocess :pid process :wait t)
            #+clozure (nth-value 1 (ccl:external-process-status process))
            #+(or cmu scl) (ext:process-exit-code process)
            #+ecl (nth-value 1 (ext:external-process-status process))
            #+lispworks
            (if-let ((stream (or (getf process-info :input-stream)
                                 (getf process-info :output-stream)
                                 (getf process-info :bidir-stream)
                                 (getf process-info :error-stream))))
              (system:pipe-exit-status stream :wait t)
              (if-let ((f (find-symbol* :pid-exit-status :system nil)))
                (funcall f process :wait t)))
            #+sbcl (sb-ext:process-exit-code process)
            #+mkcl (mkcl:join-process process)))))

  (defun %check-result (exit-code &key command process ignore-error-status)
    (unless ignore-error-status
      (unless (eql exit-code 0)
        (cerror "IGNORE-ERROR-STATUS"
                'subprocess-error :command command :code exit-code :process process)))
    exit-code)

  (defun %call-with-program-io (gf tval stream-easy-p fun direction spec activep returner
                                &key element-type external-format &allow-other-keys)
    ;; handle redirection for run-program and system
    ;; SPEC is the specification for the subprocess's input or output or error-output
    ;; TVAL is the value used if the spec is T
    ;; GF is the generic function to call to handle arbitrary values of SPEC
    ;; STREAM-EASY-P is T if we're going to use a RUN-PROGRAM that copies streams in the background
    ;; (it's only meaningful on CMUCL, SBCL, SCL that actually do it)
    ;; DIRECTION is :INPUT, :OUTPUT or :ERROR-OUTPUT for the direction of this io argument
    ;; FUN is a function of the new reduced spec and an activity function to call with a stream
    ;; when the subprocess is active and communicating through that stream.
    ;; ACTIVEP is a boolean true if we will get to run code while the process is running
    ;; ELEMENT-TYPE and EXTERNAL-FORMAT control what kind of temporary file we may open.
    ;; RETURNER is a function called with the value of the activity.
    ;; --- TODO (fare@tunes.org): handle if-output-exists and such when doing it the hard way.
    (declare (ignorable stream-easy-p))
    (let* ((actual-spec (if (eq spec t) tval spec))
           (activity-spec (if (eq actual-spec :output)
                              (ecase direction
                                ((:input :output)
                                 (error "~S not allowed as a ~S ~S spec"
                                        :output 'run-program direction))
                                ((:error-output)
                                 nil))
                              actual-spec)))
      (labels ((activity (stream)
                 (call-function returner (call-stream-processor gf activity-spec stream)))
               (easy-case ()
                 (funcall fun actual-spec nil))
               (hard-case ()
                 (if activep
                     (funcall fun :stream #'activity)
                     (with-temporary-file (:pathname tmp)
                       (ecase direction
                         (:input
                          (with-output-file (s tmp :if-exists :overwrite
                                               :external-format external-format
                                               :element-type element-type)
                            (activity s))
                          (funcall fun tmp nil))
                         ((:output :error-output)
                          (multiple-value-prog1 (funcall fun tmp nil)
                            (with-input-file (s tmp
                                               :external-format external-format
                                               :element-type element-type)
                              (activity s)))))))))
        (typecase activity-spec
          ((or null string pathname (eql :interactive))
           (easy-case))
          #+(or cmu (and sbcl os-unix) scl) ;; streams are only easy on implementations that try very hard
          (stream
           (if stream-easy-p (easy-case) (hard-case)))
          (t
           (hard-case))))))

  (defmacro place-setter (place)
    (when place
      (let ((value (gensym)))
        `#'(lambda (,value) (setf ,place ,value)))))

  (defmacro with-program-input (((reduced-input-var
                                  &optional (input-activity-var (gensym) iavp))
                                 input-form &key setf stream-easy-p active keys) &body body)
    `(apply '%call-with-program-io 'vomit-output-stream *standard-input* ,stream-easy-p
            #'(lambda (,reduced-input-var ,input-activity-var)
                ,@(unless iavp `((declare (ignore ,input-activity-var))))
                ,@body)
            :input ,input-form ,active (place-setter ,setf) ,keys))

  (defmacro with-program-output (((reduced-output-var
                                  &optional (output-activity-var (gensym) oavp))
                                  output-form &key setf stream-easy-p active keys) &body body)
    `(apply '%call-with-program-io 'slurp-input-stream *standard-output* ,stream-easy-p
            #'(lambda (,reduced-output-var ,output-activity-var)
                ,@(unless oavp `((declare (ignore ,output-activity-var))))
                ,@body)
            :output ,output-form ,active (place-setter ,setf) ,keys))

  (defmacro with-program-error-output (((reduced-error-output-var
                                         &optional (error-output-activity-var (gensym) eoavp))
                                        error-output-form &key setf stream-easy-p active keys)
                                       &body body)
    `(apply '%call-with-program-io 'slurp-input-stream *error-output* ,stream-easy-p
            #'(lambda (,reduced-error-output-var ,error-output-activity-var)
                ,@(unless eoavp `((declare (ignore ,error-output-activity-var))))
                ,@body)
            :error-output ,error-output-form ,active (place-setter ,setf) ,keys))

  (defun %use-run-program (command &rest keys
                           &key input output error-output ignore-error-status &allow-other-keys)
    ;; helper for RUN-PROGRAM when using %run-program
    #+(or abcl cormanlisp gcl (and lispworks os-windows) mcl xcl)
    (progn
      command keys input output error-output ignore-error-status ;; ignore
      (error "Not implemented on this platform"))
    (assert (not (member :stream (list input output error-output))))
    (let* ((active-input-p (%active-io-specifier-p input))
           (active-output-p (%active-io-specifier-p output))
           (active-error-output-p (%active-io-specifier-p error-output))
           (activity
             (cond
               (active-output-p :output)
               (active-input-p :input)
               (active-error-output-p :error-output)
               (t nil)))
           (wait (not activity))
           output-result error-output-result exit-code)
      (with-program-output ((reduced-output output-activity)
                            output :keys keys :setf output-result
                            :stream-easy-p t :active (eq activity :output))
        (with-program-error-output ((reduced-error-output error-output-activity)
                                    error-output :keys keys :setf error-output-result
                                    :stream-easy-p t :active (eq activity :error-output))
          (with-program-input ((reduced-input input-activity)
                               input :keys keys
                               :stream-easy-p t :active (eq activity :input))
            (let ((process-info
                    (apply '%run-program command
                           :wait wait :input reduced-input :output reduced-output
                           :error-output (if (eq error-output :output) :output reduced-error-output)
                           keys)))
              (labels ((get-stream (stream-name &optional fallbackp)
                         (or (getf process-info stream-name)
                             (when fallbackp
                               (getf process-info :bidir-stream))))
                       (run-activity (activity stream-name &optional fallbackp)
                         (if-let (stream (get-stream stream-name fallbackp))
                           (funcall activity stream)
                           (error 'subprocess-error
                                  :code `(:missing ,stream-name)
                                  :command command :process process-info))))
                (unwind-protect
                     (ecase activity
                       ((nil))
                       (:input (run-activity input-activity :input-stream t))
                       (:output (run-activity output-activity :output-stream t))
                       (:error-output (run-activity error-output-activity :error-output-stream)))
                  (loop :for (() val) :on process-info :by #'cddr
                        :when (streamp val) :do (ignore-errors (close val)))
                  (setf exit-code
                        (%check-result (%wait-process-result process-info)
                                       :command command :process process-info
                                       :ignore-error-status ignore-error-status))))))))
      (values output-result error-output-result exit-code)))

  (defun %normalize-system-command (command) ;; helper for %USE-SYSTEM
    (etypecase command
      (string command)
      (list (escape-shell-command
             (if (os-unix-p) (cons "exec" command) command)))))

  (defun %redirected-system-command (command in out err directory) ;; helper for %USE-SYSTEM
    (flet ((redirect (spec operator)
             (let ((pathname
                     (typecase spec
                       (null (null-device-pathname))
                       (string (parse-native-namestring spec))
                       (pathname spec)
                       ((eql :output)
                        (assert (equal operator " 2>"))
                        (return-from redirect '(" 2>&1"))))))
               (when pathname
                 (list operator " "
                       (escape-shell-token (native-namestring pathname)))))))
      (multiple-value-bind (before after)
          (let ((normalized (%normalize-system-command command)))
            (if (os-unix-p)
                (values '("exec") (list " ; " normalized))
                (values (list normalized) ())))
        (reduce/strcat
         (append
          before (redirect in " <") (redirect out " >") (redirect err " 2>")
          (when (and directory (os-unix-p)) ;; NB: unless on Unix, %system uses with-current-directory
            `(" ; cd " ,(escape-shell-token (native-namestring directory))))
          after)))))

  (defun %system (command &rest keys
                  &key input output error-output directory &allow-other-keys)
    "A portable abstraction of a low-level call to libc's system()."
    (declare (ignorable input output error-output directory keys))
    #+(or allegro clozure cmu (and lispworks os-unix) sbcl scl)
    (%wait-process-result
     (apply '%run-program (%normalize-system-command command) :wait t keys))
    #+(or abcl cormanlisp clisp ecl gcl genera (and lispworks os-windows) mkcl xcl)
    (let ((%command (%redirected-system-command command input output error-output directory)))
      #+(and lispworks os-windows)
      (system:call-system %command :current-directory directory :wait t)
      #+clisp
      (%wait-process-result
       (apply '%run-program %command :wait t
              :input :interactive :output :interactive :error-output :interactive keys))
      #-(or clisp (and lispworks os-windows))
      (with-current-directory ((unless (os-unix-p) directory))
        #+abcl (ext:run-shell-command %command)
        #+cormanlisp (win32:system %command)
        #+ecl (let ((*standard-input* *stdin*)
                    (*standard-output* *stdout*)
                    (*error-output* *stderr*))
                (ext:system %command))
        #+gcl (system:system %command)
        #+genera (error "~S not supported on Genera, cannot run ~S"
                        '%system %command)
        #+mcl (ccl::with-cstrs ((%%command %command)) (_system %%command))
        #+mkcl (mkcl:system %command)
        #+xcl (system:%run-shell-command %command))))

  (defun %use-system (command &rest keys
                      &key input output error-output ignore-error-status &allow-other-keys)
    ;; helper for RUN-PROGRAM when using %system
    (let (output-result error-output-result exit-code)
      (with-program-output ((reduced-output)
                            output :keys keys :setf output-result)
        (with-program-error-output ((reduced-error-output)
                                    error-output :keys keys :setf error-output-result)
          (with-program-input ((reduced-input) input :keys keys)
            (setf exit-code
                  (%check-result (apply '%system command
                                        :input reduced-input :output reduced-output
                                        :error-output reduced-error-output keys)
                                 :command command
                                 :ignore-error-status ignore-error-status)))))
      (values output-result error-output-result exit-code)))

  (defun run-program (command &rest keys
                       &key ignore-error-status force-shell
                         (input nil inputp) (if-input-does-not-exist :error)
                         output (if-output-exists :overwrite)
                         (error-output nil error-output-p) (if-error-output-exists :overwrite)
                         (element-type #-clozure *default-stream-element-type* #+clozure 'character)
                         (external-format *utf-8-external-format*)
                      &allow-other-keys)
    "Run program specified by COMMAND,
either a list of strings specifying a program and list of arguments,
or a string specifying a shell command (/bin/sh on Unix, CMD.EXE on Windows).

Always call a shell (rather than directly execute the command when possible)
if FORCE-SHELL is specified.

Signal a continuable SUBPROCESS-ERROR if the process wasn't successful (exit-code 0),
unless IGNORE-ERROR-STATUS is specified.

If OUTPUT is a pathname, a string designating a pathname, or NIL designating the null device,
the file at that path is used as output.
If it's :INTERACTIVE, output is inherited from the current process;
beware that this may be different from your *STANDARD-OUTPUT*,
and under SLIME will be on your *inferior-lisp* buffer.
If it's T, output goes to your current *STANDARD-OUTPUT* stream.
Otherwise, OUTPUT should be a value that is a suitable first argument to
SLURP-INPUT-STREAM (qv.), or a list of such a value and keyword arguments.
In this case, RUN-PROGRAM will create a temporary stream for the program output;
the program output, in that stream, will be processed by a call to SLURP-INPUT-STREAM,
using OUTPUT as the first argument (or the first element of OUTPUT, and the rest as keywords).
The primary value resulting from that call (or NIL if no call was needed)
will be the first value returned by RUN-PROGRAM.
E.g., using :OUTPUT :STRING will have it return the entire output stream as a string.
And using :OUTPUT '(:STRING :STRIPPED T) will have it return the same string
stripped of any ending newline.

ERROR-OUTPUT is similar to OUTPUT, except that the resulting value is returned
as the second value of RUN-PROGRAM. T designates the *ERROR-OUTPUT*.
Also :OUTPUT means redirecting the error output to the output stream,
in which case NIL is returned.

INPUT is similar to OUTPUT, except that VOMIT-OUTPUT-STREAM is used,
no value is returned, and T designates the *STANDARD-INPUT*.

Use ELEMENT-TYPE and EXTERNAL-FORMAT are passed on
to your Lisp implementation, when applicable, for creation of the output stream.

One and only one of the stream slurping or vomiting may or may not happen
in parallel in parallel with the subprocess,
depending on options and implementation,
and with priority being given to output processing.
Other streams are completely produced or consumed
before or after the subprocess is spawned, using temporary files.

RUN-PROGRAM returns 3 values:
0- the result of the OUTPUT slurping if any, or NIL
1- the result of the ERROR-OUTPUT slurping if any, or NIL
2- either 0 if the subprocess exited with success status,
or an indication of failure via the EXIT-CODE of the process"
    (declare (ignorable ignore-error-status))
    #-(or abcl allegro clisp clozure cmu cormanlisp ecl gcl lispworks mcl mkcl sbcl scl xcl)
    (error "RUN-PROGRAM not implemented for this Lisp")
    (flet ((default (x xp output) (cond (xp x) ((eq output :interactive) :interactive))))
      (apply (if (or force-shell
                     #+(or clisp ecl) (or (not ignore-error-status) t)
                     #+clisp (eq error-output :interactive)
                     #+(or abcl clisp) (eq :error-output :output)
                     #+(and lispworks os-unix) (%interactivep input output error-output)
                     #+(or abcl cormanlisp gcl (and lispworks os-windows) mcl xcl) t)
                 '%use-system '%use-run-program)
             command
             :input (default input inputp output)
             :error-output (default error-output error-output-p output)
             :if-input-does-not-exist if-input-does-not-exist
             :if-output-exists if-output-exists
             :if-error-output-exists if-error-output-exists
             :element-type element-type :external-format external-format
           keys))))
