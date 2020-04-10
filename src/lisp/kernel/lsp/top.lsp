;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  top.lsp -- Top-level loop, break loop, and error handlers
;;;;
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;;  Revised on July 11, by Carl Hoffman.
;;;;  Modified Oct 1986 by Ken Rimey.
;;;;  Reworked March 1987, by Edward Wang.
;;;;  Merged into new distribution Sept 1987, by Edward Wang.
;;;;  Reworked for Threads November 1988, by Giuseppe Attardi.
;;;;  Reworked for CLOS November 1988, by Giuseppe Attardi.
;;;;  Updated May 2009, by Jean-Claude Beaudoin

(in-package "SYSTEM")

(export '(*break-readtable* *tpl-prompt-hook*
          *allow-recursive-debug*))

(defvar sys:*echo-repl-tpl-read* nil "Set to t if you want to echo what was typed at the REPL top-level")
(defparameter *quit-tag* (cons nil nil))
(defparameter *quit-tags* nil)
(defparameter *break-level* 0)		; nesting level of error loops
(defparameter *break-env* nil)
(defparameter *tpl-prompt-hook* nil)
(defparameter *eof* (cons nil nil))


(defparameter *allow-recursive-debug* nil)
(defparameter *break-message* nil)
(defparameter *break-condition* nil)

(defparameter *break-readtable* nil)
(defparameter *tpl-level* -1)			; nesting level of top-level loops
(defparameter *step-level* 0)			; repeated from trace.lsp

(defvar *break-base*) ; Lowest frame available to debugger.
(defvar *break-frame*) ; Current frame being examined in debugger.

;;; A command is a list (commands function nature short-help long-help).
(defconstant-equal tpl-commands
  '(("Top level commands"
     ((:cf :compile-file) tpl-compile-command :string
      ":cf		Compile file"
      ":compile-file &string &rest files		[Top level command]~@
	:cf &string &rest files				[Abbreviation]~@
	~@
	Compile files.  With no arguments, uses values from latest :cf~@
	command.  File extensions are optional.~%")
     ((:exit :eof) quit :eval
      ":exit or ^D	Exit Lisp"
      ":exit &eval &optional (status 0)		[Top level command]~@
	~@
	Exit Lisp without further confirmation.~%")
     ((:ld :load) tpl-load-command :string
      ":ld		Load file"
      ":load &string &rest files			[Top level command]~@
	:ld &string &rest files				[Abbreviation]~@
	~@
	Load files.  With no arguments, uses values from latest :ld~@
	or :cf command. File extensions are optional.~%")
     ((:step) tpl-step-command nil
      ":step		Single step form"
      ":step form					[Top level command]~@
	~@
	Evaluate form in single step mode.  While stepping, a new break~@
	level is invoked before every evaluation.  Extra commands are~@
	available at this time to control stepping and form evaluation.~%")
     ((:pwd :print-working-directory) tpl-default-pathname-defaults-command nil
      ":pwd	Print the current value of *default-pathname-defaults*"
      "See also: :cd.~%")
     ((:cd :change-default-pathname-defaults) tpl-change-default-pathname-defaults-dir-command :string
      ":cd	Change the current value of *default-pathname-defaults*"
      "See also: :dpd.~%")
     #+threads
     ((:s :switch) tpl-switch-command nil
      ":s(witch)       Switch to next process to debug"
      ":switch process                                 [Break command]~@
        :s processs                                     [Abbreviation]~@
        ~@
        Switch to next process in need to debugger attention. Argument~@
        process, when provided, must be an integer indicating the rank~@
        of the process in the debugger waiting list.~%")
     #+threads
     ((:br :break) tpl-interrupt-command nil
      ":br(eak)        Stop a given process"
      ":break process                                  [Break command]~@
        :br processs                                    [Abbreviation]~@
        ~@
        Interrupt a given process. Argument process, must be provided and
        it must be an integer indicating the rank~@
        of the process in the debugger waiting list (:waiting).~%")
     #+threads
     ((:w :waiting) tpl-waiting-command nil
      ":w(aiting)      Display list of active toplevels"
      ":waiting                                        [Break command]~@
        :w                                              [Abbreviation]~@
        ~@
        Display list of active toplevels, including open debug sessions.~%")
     )
    ("Help commands"
     ((:apropos) tpl-apropos-command nil
      ":apropos	Apropos"
      ":apropos string &optional package		[Top level command]~@
	~@
	Finds all available symbols whose print names contain string.~@
	If a non NIL package is specified, only symbols in that package are considered.~@
	~%")
     ((:doc document) tpl-document-command nil
      ":doc(ument)	Document"
      ":document symbol				[Top level command]~@
	~@
	Displays documentation about function, print names contain string.~%")
     ((? :h :help) tpl-help-command nil
      ":h(elp) or ?    Help.  Type \":help help\" for more information"
      ":help &optional topic                           [Top level command]~@
        :h &optional topic                              [Abbreviation]~@
        ~@
        Print information on specified topic.  With no arguments, print~@
        quick summary of top level commands.~@
        ~@
        Help information for top level commands follows the documentation~@
        style found in \"Common Lisp, the Language\"; and, in general, the~@
        commands themselves follow the conventions of Common Lisp functions,~@
        with the exception that arguments are normally not evaluated.~@
        Those commands that do evaluate their arguments are indicated by the~@
        keyword &eval in their description.  A third class of commands~@
        treat their arguments as whitespace-separated, case-sensitive~@
        strings, requiring double quotes only when necessary.  This style~@
        of argument processing is indicated by the keyword &string.~@
        For example, the :load command accepts a list of file names:
        ~@
        :load &string &rest files                       [Top level Command]~@
        ~@
        whereas :exit, which requires an optional evaluated argument, is~@
        ~@
        :exit &eval &optional status                    [Top level Command]~%")
     )))

(defparameter *tpl-commands* tpl-commands)

(defconstant-equal break-commands
    '("Break commands"
      ((:q :quit) tpl-quit-command nil
       ":q(uit)		Return to some previous break level"
       ":quit &optional n				[Break command]~@
	:q &optional n					[Abbreviation]~@
	~@
	Without argument, return to top level;~@
	otherwise return to break level n.~%")
      ((:pop) (tpl-pop-command) :constant
       ":pop		Pop to previous break level"
       ":pop						[Break command]~@
	~@
	Pop to previous break level, or if already in top level,~@
	exit Lisp after confirmation.~%")
      ((:c :continue) continue nil
       ":c(ontinue)	Continue execution"
       ":continue					[Break command]~@
	:c						[Abbreviation]~@
	~@
	Continue execution.  Return from current break level to the caller.~@
	This command is only available when the break level is continuable~@
	(i.e., the CONTINUE restart is available).~%")
      ((:b :backtrace) tpl-backtrace nil
       ":b(acktrace)	Print backtrace"
       ":backtrace &optional count			[Break command]~@
	:b &optional count				[Abbreviation]~@
	~@
	Show function call history.  Only those functions called since~@
	the previous break level are shown.  In addition, functions compiled~@
	in-line or explicitly hidden are not displayed.  If a count is~@
        provided, only up to that many frames are displayed.~@
	See also: :function, :previous, :next.~%")
      ((:r :restarts) tpl-restarts nil
       ":r(estarts)   Print available restarts"
       ":restarts                                       [Break command]~@
        :r                                              [Abbreviation]~@
        Display the list of available restarts again. A restart can be~@
        invoked by using the :rN command, where N is the displayed~@
        numerical identifier for that restart.~%")
      ((:f :frame) tpl-print-current nil
       ":f(rame)	Show current frame"
       ":frame					[Break command]~@
	:f						[Abbreviation]~@
	~@
	Show current frame.  The current frame is the implicit focus~@
	of attention for several other commands.~@
	~@
	See also: :backtrace, :next, previous, :disassemble, :variables.~%")
      ((:u :up) tpl-previous nil
       ":u(p)	Go to previous frame"
       ":up &optional (n 1)			[Break command]~@
	:u &optional (n 1)				[Abbreviation]~@
	~@
	Move to the nth previous visible frame in the backtrace.~@
 	It becomes the new current frame.~@
	~@
	See also: :backtrace, :frame, :go, :next.~%")
      ((:d :down) tpl-next nil
       ":d(own)		Go to next frame"
       ":down &optional (n 1)				[Break command]~@
	:d &optional (n 1)				[Abbreviation]~@
	~@
	Move to the nth next visible frame in the backtrace.  It becomes~@
	the new current frame.~@
	~@
	See also: :backtrace, :frame, :go, :previous.~%")
      ((:g :go) tpl-go nil
       ":g(o)		Go to frame"
       ":go &optional (n 1)				[Break command]~@
	:g &optional (n 1)				[Abbreviation]~@
	~@
	Move to the i-th frame.~@
	See also: :backtrace, :frame, :next, :previous.~%")
      #+(or)
      ((:fs :forward-search) tpl-forward-search :string
       ":fs             Search forward for function"
       ":forward-search &string substring		[Break command]~@
	:fs &string substring				[Abbreviation]~@
	~@
	Search forward in the backtrace for function containing substring.~@
	The match is case insensitive.~@
	~@
	See also: :backtrace, :function, :next.~%")
      #+(or)
      ((:bs :backward-search) tpl-backward-search :string
       ":bs             Search backward for function"
       ":backward-search &string substring		[Break command]~@
	:bs &string substring				[Abbreviation]~@
	~@
	Search backward in the backtrace for function containing substring.~@
	The match is case insensitive.~@
	~@
	See also: :backtrace, :function, :previous.~%")
      ((:disassemble) tpl-disassemble-command nil
       ":disassemble	Disassemble current function"
       ":disassemble					[Break command]~@
	:disassemble					[Abbreviation]~@
	~@
	Disassemble the current frame's function, if possible.~%")
      ((:le :lambda-expression) tpl-lambda-expression-command nil
       ":l(ambda-)e(expression)	Show lisp code for current function"
       ":lambda-expression				[Break command]~@
	:le						[Abbreviation]~@
	~@
	Show the lisp code of the current frame's function.~@
        Only works for interpreted functions.~%")
      ((:v :variables) tpl-variables-command nil
       ":v(ariables)	Show local variables, functions, blocks, and tags"
       ":variables &optional no-values			[Break command]~@
	:v &optional no-values				[Abbreviation]~@
	~@
	Show lexical variables, functions, block names, and tags local~@
	to the current function.  The current function must be interpreted.~@
	The values of local variables and functions are also shown,~@
	unless the argument is non-null.~%")
      ((:hide) tpl-hide nil
       ":hide		Hide function"
       ":hide function					[Break command]~@
	~@
	Hide function.  A hidden function is not displayed in a backtrace.~@
	~@
	See also: :backtrace, :unhide, :hide-package.~%")
      ((:unhide) tpl-unhide nil
       ":unhide		Unhide function"
       ":unhide function				[Break command]~@
	~@
	Unhide function.  The specified function will be displayed in future~@
	backtraces, unless its home package is also hidden.~@
	~@
	See also: :backtrace, :hide, :unhide-package.~%")
      ((:hp :hide-package) tpl-hide-package nil
       ":hp		Hide package"
       ":hide-package package				[Break command]~@
	:hp package					[Abbreviation]~@
	~@
	Hide package.  Functions in a hidden package are not displayed~@
	in a backtrace.~@
	~@
	See also: :backtrace, :unhide-package.~%")
      ((:unhp :unhide-package) tpl-unhide-package nil
       ":unhp		Unhide package"
       ":unhide-package package				[Break command]~@
	:unhp package					[Abbreviation]~@
	~@
	Unhide package.  Functions in the specified package will be displayed~@
	in future backtraces, unless they are individually hidden.~@
	~@
	See also: :backtrace, :hide-package, :hide, :unhide.~%")
      ((:unhide-all) tpl-unhide-all nil
       ":unhide-all     Unhide all variables and packages"
       ":unhide-all					[Break command]~@
	~@
	Unhide all variables and packages.  All functions will be displayed~@
	in future backtraces.~@
	~@
	See also: :hide, :unhide, :hide-package, :unhide-package.~%")
      ((:m :message) tpl-print-message nil
       ":m(essage)      Show error message"
       ":message					[Break command]~@
	:m						[Abbreviation]~@
	~@
	Show current error message.~%")
      ((:hs :help-stack) tpl-help-stack-command nil
       ":hs             Help stack"
       ":help-stack                                     [Break command]~@
        :hs                                             [Abbreviation]~@
        ~@
        Lists the functions to access backtrace information more directly.~%")
      #+(or)
      ((:i :inspect) tpl-inspect-command nil
       ":i(nspect)      Inspect value of local variable"
       ":inspect var-name                               [Break command]~@
        :i var-name                                     [Abbreviation]~@
        ~@
        Inspect value of local variable named by var-name. Argument~@
        var-name can be a string or a symbol whose name string will~@
        then be used regardless of of the symbol's package.~@
        ~@
        See also: :variables.~%")
  ))

(defparameter *lisp-initialized* nil)

(defun top-level (&optional (process-command-line nil) (set-package nil))
  "Args: ()
Clasp specific.
The top-level loop of Clasp. It is called by default when Clasp is invoked."
  (catch *quit-tag*
    (let* ((*debugger-hook* nil)
	   + ++ +++ - * ** *** / // ///)

      (when set-package
        (in-package "CL-USER"))

      (setq *lisp-initialized* t)

      (let ((*tpl-level* -1))
	(tpl))
      0)))

#+threads
(progn

(defparameter *console-lock* (mp:make-lock :name "Console lock"))
(defparameter *console-available* (mp:make-condition-variable))
(defparameter *console-owner* nil)
(defparameter *console-waiting-list* '())

(defun candidate-to-get-console-p (process)
  (or (null *console-owner*)
      (eq *console-owner* process)
      (not (mp:process-active-p *console-owner*))))

(defun register-in-waiting-list (process)
  (mp:with-lock (*console-lock*)
    (push process *console-waiting-list*)))

(defun delete-from-waiting-list (process)
  (mp:with-lock (*console-lock*)
    (setf *console-waiting-list* (delete process *console-waiting-list*))))

(defun grab-console (process)
  (loop with repeat = t
     while repeat
     do (mp:with-lock (*console-lock*)
          (cond ((candidate-to-get-console-p process)
                 (setf *console-owner* process)
                 (setf repeat nil))
                (t
                 (mp:condition-variable-wait *console-available* *console-lock*))))))

(defun release-console (process)
  (mp:with-lock (*console-lock*)
    (and (eq process *console-owner*) (setf *console-owner* nil))
    (mp:condition-variable-signal *console-available*)))

) ; #+threads

(defmacro with-grabbed-console (&rest body)
  #-threads
  `(progn ,@body)
  #+threads
  `(unwind-protect
        (progn
          (register-in-waiting-list mp:*current-process*)
          (grab-console mp:*current-process*)
          ,@body)
     (delete-from-waiting-list mp:*current-process*)
     (release-console mp:*current-process*)))

(defun simple-terminal-interrupt ()
  (error 'ext:interactive-interrupt))

#+threads
(defun show-process-list (&optional (process-list (mp:all-processes)))
  (loop with current = mp:*current-process*
	for rank from 1
	for process in process-list
	do (format t (if (eq process current)
			 "~%  >~D: ~s"
		       "~%   ~D: ~s")
		   rank process)))

#+threads
(defun query-process (&optional (process-list (mp:all-processes)))
  (format t "~&Choose the integer code of process to interrupt.
Use special code 0 to cancel this operation.")
  (loop for code = (progn
		     (show-process-list process-list)
		     (tpl-prompt)
		     (tpl-read))
	do (cond ((zerop code)
		  (return nil))
		 ((and (fixnump code) (<= 1 code (length process-list)))
		  (return (list (elt process-list (1- code)))))
		 (t
		  (format t "~&Not a valid process number")))))

(defparameter *interrupt-lonely-threads-p* t)

(defun single-threaded-terminal-interrupt ()
  (restart-case (simple-terminal-interrupt)
    (continue ())))

(defun terminal-interrupt (&key process (correctablep t))
  (declare (ignore correctablep))
  #+threads
  (mp:without-interrupts
   (let* ((suspended '())
	  (break-process nil)
	  (all-processes
	   (loop with this = mp:*current-process*
		 for p in (mp:all-processes)
		 unless (or (eq p this)
			    (member (mp:process-name p)
                                    '(si:signal-servicing si::handle-signal)))
		 collect p)))
     (when (and (= (length all-processes) 1) *interrupt-lonely-threads-p*)
       (mp:interrupt-process (first all-processes)
			     #'single-threaded-terminal-interrupt)
       (return-from terminal-interrupt))
     (loop for i in all-processes
	   do (progn (format t "~%;;; Suspending process ~A" i)
		     (push i suspended)
		     (mp:process-suspend i)))
     (flet ((do-query-process ()
              (print all-processes)
	      (query-process all-processes)))
       (mp:with-local-interrupts
	(restart-case (simple-terminal-interrupt)
          (continue () (setf break-process nil))
	  (mp:interrupt-process (process)
	    :interactive do-query-process
	    :report (lambda (stream) (princ "Interrupt a certain process." stream))
	    (setf break-process process)))))
     (loop for process in suspended
        unless (eq process break-process)
        do (mp:process-resume process))
     (when break-process
       (mp:interrupt-process break-process
			     #'single-threaded-terminal-interrupt))))
  #-threads
  (single-threaded-terminal-interrupt))

(defun tpl (&key ((:commands *tpl-commands*) tpl-commands)
	      ((:prompt-hook *tpl-prompt-hook*) *tpl-prompt-hook*)
	      (broken-at nil)
	      (quiet nil))
  (let* ((*quit-tags* (cons *quit-tag* *quit-tags*))
	 (*quit-tag* *quit-tags*)	; any unique new value
	 (*tpl-level* (1+ *tpl-level*))
	 (break-level *break-level*)
	 values)
    (flet ((rep ()
             ;; We let warnings pass by. This way "warn" does the
             ;; work.  It is conventional not to trap anything
             ;; that is not a SERIOUS-CONDITION. Otherwise we
             ;; would be interferring the behavior of code that relies
             ;; on conditions for communication (for instance our compiler)
             ;; and which expect nothing to happen by default.
             (handler-bind
                 ((serious-condition
		   (lambda (condition)
		     (cond ((< break-level 1)
			    ;; Toplevel should enter the debugger on any condition.
			    )
			   (*allow-recursive-debug*
			    ;; We are told to let the debugger handle this.
			    )
			   (t
			    (format t "~&Debugger received error of type: ~A~%~A~%~
                                         Error flushed.~%"
				    (type-of condition) condition)
			    (clear-input)
			    (return-from rep t) ;; go back into the debugger loop.
			    )
			   )
		     )))
               (with-grabbed-console
                   (unless quiet
                     (break-where)
                     (setf quiet t))
                 (setq - (locally (declare (notinline tpl-read))
                           (tpl-prompt)
                           (let ((expr (tpl-read)))
                             (when sys:*echo-repl-tpl-read*
                               (format t "#|REPL echo|# ~s~%" expr))
                             expr)))
                 (setq values (multiple-value-list
                               (funcall core:*eval-with-env-hook* - *break-env*)
                               )
                       /// // // / / values *** ** ** * * (car /))
                 (tpl-print values)))))
          (loop
           (setq +++ ++ ++ + + -)
           (when
               (catch *quit-tag*
                 (if (zerop break-level)
                   (with-simple-restart 
                    (restart-toplevel "Go back to Top-Level REPL.")
                    (rep))
                   (with-simple-restart
                    (restart-debugger "Go back to debugger level ~D." break-level)
                    (rep)))
                 nil)
             (setf quiet nil))))))

(defun tpl-prompt ()
  (typecase *tpl-prompt-hook*
    (string (format t *tpl-prompt-hook*))
    (function (funcall *tpl-prompt-hook*))
    (t (fresh-line)
       (format t "~A~V,,,'>A "
	       (if (eq *package* (find-package 'user))
		   ""
		 (package-name *package*))
	       (- *tpl-level* *step-level* -1)
	       ""))))

(defun tpl-read (&aux (*read-suppress* nil))
  (finish-output)
  (loop
    (case (peek-char nil *standard-input* nil :EOF)
      ((#\))
       (warn "Ignoring an unmatched right parenthesis.")
       (read-char))
      ((#\space #\tab)
       (read-char))
      ((#\newline #\return)
       (read-char)
       ;; avoid repeating prompt on successive empty lines:
       (let ((command (tpl-make-command :newline "")))
	 (when command (return command))))
      (:EOF
       (terpri)
       (return (tpl-make-command :EOF "")))
      (#\:
       (return (tpl-make-command (read-preserving-whitespace)
				 (read-line))))
      (#\?
       (read-char)
       (case (peek-char nil *standard-input* nil :EOF)
	 ((#\space #\tab #\newline #\return :EOF)
	  (return (tpl-make-command :HELP (read-line))))
	 (t
	  (unread-char #\?)
	  (return (read-preserving-whitespace)))))
      ;; We use READ-PRESERVING-WHITESPACE because with READ, if an
      ;; error happens within the reader, and we perform a ":C" or
      ;; (CONTINUE), the reader will wait for an inexistent #\Newline.
      (t
       (return (read))))))

;;; Set to true if a command is signaling an error
;;; and you want that to invoke the debugger so you can fix it.
(defparameter *debug-tpl-commands* nil)

(defun harden-command (cmd-form)
  `(block
    tpl-command
    (handler-bind
     ((error (lambda (condition)
	       (unless *debug-tpl-commands*
		 (format t "~&Command aborted.~%Received condition of type: ~A~%~A"
			 (type-of condition) condition)
		 (clear-input)
		 (return-from tpl-command nil)
		 )
	       )
	     ))
     ,cmd-form
     )
    )
  )

(defun tpl-make-command (name line &aux (c nil))
  (dolist (commands *tpl-commands*)
    (when (setq c (assoc name (cdr commands) :test #'member))
      (return)))
  (cond ((null c)
	 (if (eq name :newline)		; special handling for Newline.
	     nil
	     `(tpl-unknown-command ',name)))
	((eq (third c) :restart)
	 `(progn
	    (invoke-restart-interactively ,(second c))))
	((eq (third c) :eval)
	 `(,(second c) . ,(tpl-parse-forms line)))
	((eq (third c) :string)
	 (harden-command `(,(second c) . ,(tpl-parse-strings line))))
	((eq (third c) :constant)
	 (harden-command (second c)))
	(t
	 (harden-command `(,(second c) . ,(tpl-parse-forms line t))))))

(defun tpl-parse-forms (line &optional quote)
  (with-input-from-string (stream line)
    (do ((form (read stream nil *eof*) (read stream nil *eof*))
	 (list nil))
	((eq form *eof*) (nreverse list))
      (push (if quote `',form form) list))))

(defun tpl-parse-strings (line)
  (do ((i 0 end)
       (start)
       (end)
       (list nil)
       (space-p #'(lambda (c) (or (eql c #\space) (eql c #\tab))))
       (length (length line)))
      ((>= i length) (nreverse list))
    (cond ((null (setq start (position-if-not space-p line :START i)))
	   (setq end length))
	  ((eql (schar line start) #\")
	   (multiple-value-bind
	       (string n)
	       (read-from-string line t nil :START start)
	     (push string list)
	     (setq end n)))
	  (t
	   (setq end (or (position-if space-p line :START start) length))
	   (push (subseq line start end) list)))))

(defun tpl-print (values)
  (fresh-line)
  (dolist (v values)
    (prin1 v)
    (terpri)))

(defun tpl-unknown-command (command)
  (format t "Unknown top level command: ~s~%" command)
  (values))

(defun tpl-pop-command (&rest any)
  (declare (ignore any))
  (throw (pop *quit-tags*) t))

(defun tpl-quit-command (&optional (level 0))
  (when (and (>= level 0) (< level *tpl-level*))
    (let ((x (nth (- *tpl-level* level 1) *quit-tags*)))
      (throw x x)))
  (tpl-print-current))

(defun tpl-print-current ()
  (fresh-line)
  (clasp-debug:prin1-frame-call *break-frame*)
  (terpri)
  (values))

(defun ext:tpl-frame ()
  "In Clasp's debugger, return the CLASP-DEBUG:FRAME object for the frame under examination."
  *break-frame*)

(defun ext:tpl-argument (n)
  "In Clasp's debugger, return the nth argument in the current frame."
  (nth n (clasp-debug:frame-arguments *break-frame*)))

(defun ext:tpl-arguments ()
  "In Clasp's debugger, return the list of arguments in the current frame."
  (clasp-debug:frame-arguments *break-frame*))

(defun tpl-previous (&optional (n 1))
  (setf *break-frame* (clasp-debug:up *break-frame* n))
  (tpl-print-current))

(defun tpl-next (&optional (n 1))
  (setf *break-frame* (clasp-debug:down *break-frame* n))
  (tpl-print-current))

(defun tpl-go (ihs-index)
  (setf *break-frame*
        (clasp-debug:visible
         (clasp-debug:goto *break-base* ihs-index)))
  (tpl-print-current))

(defun tpl-print-message ()
  (when *break-message*
    (princ *break-message*)
    (terpri))
  (values))

(defun tpl-disassemble-command ()
  (clasp-debug:disassemble-frame *break-frame*)
  (values))

(defun tpl-lambda-expression-command ()
  (let ((form (clasp-debug:frame-function-form *break-frame*)))
    (if form
        (pprint form)
        (format t " No source code available for this function.~%")))
  (values))

(defun tpl-variables-command (&optional no-values)
  (let ((locals (clasp-debug:frame-locals *break-frame*)))
    (if (null locals)
        (format t "none")
        (if no-values
            (loop for (var) in locals
                  do (format t "~& ~s" var))
            (loop for (var . value) in locals
                  do (format t "~& ~s: ~s" var value)))))
  (terpri)
  (values))

(defun clasp-backtrace (&optional (n 99999999))
  (core:btcl))

(defun tpl-backtrace (&optional count)
  (clasp-debug:print-stack *break-base* :count count))

(defun break-where ()
  (if (<= *tpl-level* 0)
      #-threads (format t "~&Top level.~%")
      #+threads (format t "~&Top level in: ~A.~%" mp:*current-process*)
    (tpl-print-current)))

(defun set-current-frame ()
  (setf *break-frame* (clasp-debug:visible *break-frame*))
  (tpl-print-current))

(defun tpl-hide (fname)
  (clasp-debug:hide fname)
  (set-current-frame))

(defun tpl-unhide (fname)
  (clasp-debug:unhide fname)
  (values))

(defun tpl-hide-package (package-designator)
  (clasp-debug:hide-package package-designator)
  (set-current-frame))

(defun tpl-unhide-package (package-designator)
  (clasp-debug:unhide-package package-designator)
  (values))

(defun tpl-unhide-all ()
  (clasp-debug:unhide-all)
  (values))

(defun tpl-apropos-command (&optional string pkg)
  (when string (apropos string pkg)))

(defun tpl-document-command (&optional symbol)
  (error "tpl-document-command doesn't work because clasp doesn't supply help")
  #+(or)(when symbol (help symbol)))

(defun tpl-step-command (&optional form)
  (when form (step* form)))

(defun tpl-default-pathname-defaults-command ()
  (print *default-pathname-defaults*))

(defun tpl-change-default-pathname-defaults-command (string)
  "This only works for UNIX right now"
  (format t "This is where I would use ~a to change the *default-pathname-defaults*" string))

(defparameter *tpl-last-load* nil)

(defun tpl-load-command (&rest files)
  (when files
    (setq *tpl-last-load* files))
  (dolist (file *tpl-last-load*) (load file))
  *tpl-last-load*)

(defparameter *tpl-last-compile* nil)

(defun tpl-compile-command (&rest files)
  (when files
    (setq *tpl-last-compile* files))
  (dolist (file *tpl-last-compile*) (compile-file file))
  (setq *tpl-last-load* *tpl-last-compile*))

(defun tpl-help-command (&optional topic)
  (cond ((null topic)
	 (dolist (commands *tpl-commands*)
	   (format t "~%~A:~%" (car commands))
	   (dolist (c (cdr commands))
	     (when (fourth c)
	       (format t "~A.~%" (fourth c))))))
	((or (stringp topic) (symbolp topic))
	 (let (c)
	   (setq topic (intern (string topic) (find-package 'keyword)))
	   (dolist (commands *tpl-commands*)
	     (when (setq c (assoc topic (cdr commands) :test #'member))
	       (return)))
	   (cond ((null (fifth c))
		  (format t "No such help topic: ~s~%"
			  (string topic)))
		 (t
		  (terpri)
		  (format t (fifth c))
		  (terpri)))))
	(t
	 (format t "Not a valid help topic: ~s~%" topic)))
  (values))

(defun tpl-help-stack-command ()
  (format t "Use the following functions to access backtrace info more directly.

(EXT:TPL-FRAME) Return the FRAME object for the current frame.
(EXT:TPL-ARGUMENT n) Return the nth argument in the current frame.
(EXT:TPL-ARGUMENTS) Return the list of arguments in the current frame.

See the CLASP-DEBUG package for more information about FRAME objects.")
  (values))

(defun compute-restart-commands (condition &key display)
  (let ((restarts (compute-restarts condition))
	(restart-commands (list "Restart commands")))
    (when display
      (format display
              (if restarts
                  "~&Available restarts:~&(use :r1 to invoke restart 1, etc.)~2%"
                  "~&No restarts available.~%")))
    (loop for restart in restarts
       and i from 1
       do (let ((user-command (format nil "r~D" i))
		(name (format nil "~@[(~A)~]" (restart-name restart)))
		(helpstring (princ-to-string restart)))
	    (push (list
		   (list (intern (string-upcase user-command) :keyword))
		   restart :restart
		   (format nil ":~A~16T~A~24T~A" user-command helpstring name)
		   (format nil ":~A~48T~A~& ~&~A~A" (string-downcase user-command)
                           "[Restart command]" name helpstring))
		  restart-commands)
	    (when display
	      (format display "~D. ~A ~A~%" i name restart))))
    (when display (terpri display))
    (nreverse restart-commands)))

(defun update-debug-commands (restart-commands)
  (let ((commands (copy-list *tpl-commands*)))
    (unless (member break-commands commands)
      (setq commands (nconc commands (list break-commands))))
    (delete-if
     #'(lambda (x) (string= "Restart commands" (car x)))
     commands)
    (nconc commands (list restart-commands))))

(defun tpl-restarts ()
  ;; Make sure this displays consistently with compute-restart-commands.
  (let ((restart-commands (cdr (assoc "Restart commands" *tpl-commands*
                                      :test #'string=))))
    (format t (if restart-commands
                  "~&Available restarts:~&(use :r1 to invoke restart 1, etc.)~2%"
                  "~&No restarts available.~%"))
    (loop for command in restart-commands
          for restart = (second command)
          for i from 1
          do (format t "~d. ~@[(~a)~] ~a~%"
                     i (restart-name restart) restart)))
  (terpri)
  (values))

(defparameter *default-debugger-maximum-depth* 16)

(defun check-default-debugger-runaway ()
  (when (< *default-debugger-maximum-depth* *break-level*)
    #+threads
    (progn
      (format *error-output*
	      "~&Excessive debugger depth! Probable infinite recursion!~%~
             Quitting process: ~A.~%" mp:*current-process*)
      (when (< (+ *default-debugger-maximum-depth* 3) *break-level*)
	;; we tried to be polite but it does not seem to work.
	(quit -1))
      (format t "Exiting from check-default-debugger-runaway~%")
      (core:exit -1))
    #-threads
    (progn
      (format *error-output*
	    "~&Excessive debugger depth! Probable infinite recursion!~%~
             Quitting.~%")
        (quit -1))))

#+threads
(defun tpl-switch-command (&optional rank)
  (let (elect)
    (cond ((integerp rank)
           (let ((max (list-length *console-waiting-list*)))
             (unless (and (< 0 rank) (<= rank max))
               (error "Debugger switch command: Invalid argument value.")))
           (setf elect (elt *console-waiting-list* (1- rank))))
          (t
           (setf elect (find rank *console-waiting-list* :key #'mp:process-name))))
    (when elect
      (setq *console-owner* elect)))
  (values))

#+threads
(defun tpl-waiting-command ()
  (format t "~&~%Debugger's waiting list:~2%")
  (loop for process in *console-waiting-list*
     for rank from 1
     do (format t (if (eq process mp:*current-process*)
                      "   >~D: ~A~%"
                      "    ~D: ~A~%")
                rank process))
  (terpri)
  (values))

(defun default-debugger (condition)
  (with-standard-io-syntax
    (let* ((*standard-input* *debug-io*)
           (*standard-output* *debug-io*)
           ;;(*tpl-prompt-hook* "[dbg] ")
	   (*print-readably* nil)
           (*print-pretty* nil)
           (*print-circle* t)
           (*print-length* 16)
           (*readtable* (or *break-readtable* *readtable*))
	   (*break-condition* condition)
           (*break-message* (format nil "~&Condition of type: ~A~%~A~%"
				    (type-of condition) condition))
           (*break-level* (1+ *break-level*))
           (break-level *break-level*)
           (*break-env* nil)
	   (cmp:*implicit-compile-hook* #'cmp:implicit-compile-hook-default))
      (check-default-debugger-runaway)
      #+threads
      ;; We give our process priority for grabbing the console.
      (setq *console-owner* mp:*current-process*)
      ;; As of ECL 9.4.1 making a normal function return from the debugger
      ;; seems to be a very bad idea! Basically, it dumps core...
      (ignore-errors
       (when (listen *debug-io*)
         (clear-input *debug-io*)))
      ;; Like in SBCL, the error message is output through *error-output*
      ;; The rest of the interaction is performed through *debug-io*
      (ignore-errors (finish-output))
      ;; We wrap the following in `ignore-errors' because error may be
      ;; caused by writing to the `*error-output*', what leads to
      ;; infinite recursion!
      (ignore-errors
       (fresh-line *error-output*)
       (terpri *error-output*)
       (princ *break-message* *error-output*))
      (loop
	;; Here we show a list of restarts and invoke the toplevel with
	;; an extended set of commands which includes invoking the associated
	;; restarts.
	(let* ((restart-commands (compute-restart-commands condition :display t))
	       (debug-commands
		 (update-debug-commands restart-commands)))
          (clasp-debug:with-stack (*break-base*)
            (let ((*break-frame* (clasp-debug:visible *break-base*)))
              (tpl :commands debug-commands))))))))

(defun invoke-debugger (condition)
  ;; call *INVOKE-DEBUGGER-HOOK* first, so that *DEBUGGER-HOOK* is not
  ;; called when the debugger is disabled. We adopt this mechanism
  ;; from SBCL.
  (let ((old-hook ext:*invoke-debugger-hook*))
    (when old-hook
      (let ((ext:*invoke-debugger-hook* nil))
        (funcall old-hook condition old-hook))))
  (let* ((old-hook *debugger-hook*))
    (when old-hook
      (let ((*debugger-hook* nil))
        (funcall old-hook condition old-hook))))
  (locally
    (declare (notinline default-debugger))
    (if (<= 0 *tpl-level*) ;; Do we have a top-level REPL above us?
        (default-debugger condition)
        (let* (;; We do not have a si::top-level invocation above us
               ;; so we have to provide the environment for interactive use.
               (ext:*invoke-debugger-hook* ext:*invoke-debugger-hook*)
               (*debugger-hook* *debugger-hook*)
               (*tpl-level* *tpl-level*) ;; Or should we simply say 0.
               (*tpl-commands* *tpl-commands*)
               + ++ +++ - * ** *** / // ///)
          (default-debugger condition))))
  (finish-output))

(defun core::debugger-disabled-hook (condition old-hook)
  (declare (ignore old-hook))
  (format *error-output* "~&Condition of type: ~a~%~a~%"
          (type-of condition) condition)
  (let ((clasp-debug:*frame-filters* nil))
    (clasp-debug:print-backtrace :stream *error-output*
                                 :source-positions t
                                 :delimited nil))
  (format *error-output* "~&Unhandled condition with debugger disabled, quitting~%")
  (core:quit 1))

(eval-when (:execute :load-toplevel)
  (when (null (core:is-interactive-lisp))
    (setq ext:*invoke-debugger-hook* 'debugger-disabled-hook)))
