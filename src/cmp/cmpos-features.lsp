;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPFEATURES.LSP -- Gather a list of features from the compiler

(in-package "COMPILER")

(defun run-and-collect (command args &optional file)
  (flet ((collect-lines (stream)
           (loop for line = (read-line stream nil nil)
              while line
              collect line)))
    (handler-case
        (multiple-value-bind (stream process)
            (ext:run-program command args :input nil :output :stream :error :output)
          (let ((lines (collect-lines stream)))
            (cond ((null file)
                   lines)
                  ((probe-file file)
                   (with-open-file (s file :direction :input)
                     (collect-lines s)))
                  (t
                   (warn "Unable to find file ~A" file)))))
      (error (c)
        (format t "~&;;; Unable to execute program ~S~&;;; Condition~&;;; ~A"
                command c)))))

(defun split-words (string)
  (loop with output = '()
     with word = '()
     for i from 0 below (length string)
     for c = (elt string i)
     do (if (member c '(#\Space #\Tab #\Newline))
            (when word
              (push (make-array (length word) :element-type 'base-char
                                :initial-contents (nreverse word))
                    output)
              (setf word nil))
            (push c word))
     finally (return output)))

(defconstant +known-keywords+
  '("sparc*" "x86*" "*-bit" "32*" "64*" "*32" "*64"
    "elf" "coff" "mach-o"
    "universal"
    "gcc" "icc"))

(defun known-keyword (string &optional (patterns +known-keywords+))
  (loop with base = (make-pathname :directory nil :name (string-upcase string))
     for p in patterns
     for pattern-path = (make-pathname :directory nil :name (string-upcase p))
     thereis (pathname-match-p base pattern-path)))

(defun gather-keywords (strings patterns)
  (let ((strings (reduce #'append (mapcar #'split-words strings))))
    (mapcar (lambda (s)
              (intern (string-upcase s) (find-package :keyword)))
            (remove-if-not #'known-keyword strings))))

(defun compiler-defines (macros)
  "Test for existence of a set of C preprocessor macros for the compiler flags
we are currently using with ECL."
  (let* ((f (ext:mkstemp "tmp:foo"))
         (fc (make-pathname :type "c" :defaults f))
         (fs (make-pathname :type "i" :defaults f)))
    (with-open-file (s fc :direction :output :if-exists :overwrite
                       :if-does-not-exist :create)
      (loop for i from 0
         for (macro . rest) in macros
         do (format s "~%#ifdef ~A~%ECLVALUE ~4,'0D ~A~%#endif"
                    macro i macro)))
    (loop with list = (mapcar #'list (mapcar #'first macros))
       with lines = (run-and-collect c::*cc*
                                     (append (c::split-program-options c::*cc-flags*)
                                             (list "-E" (namestring fc)
                                                   "-o" (namestring fs)))
                                     fs)
       for l in lines
       when (eql (search "ECLVALUE" l) 0)
       do (let* ((number (parse-integer (subseq l 9 13)))
                 (defines (subseq l 14)))
            (setf (elt list number)
                  (elt macros number)))
       finally (progn
                 ;;(and (probe-file f) (delete-file f))
                 ;;(and (probe-file fc) (delete-file fc))
                 ;;(and (probe-file fs) (delete-file fs))
                 (return list)))))

(defconstant +compiler-macros+
  '(;; Compiler names
    ("__INTEL_COMPILER" :intel-compiler)
    ("__GNUC__" :gcc-compiler)
    ("__SUNPRO_CC" :sun-c++-compiler)
    ("__SUNPRO_C" :sun-c-compiler)
    ("__xlc__" :ibm-c-compiler)
    ("__xlC__" :ibm-c++-compiler)

    ;; Processor features
    ("__MMX__" :mmx)
    ("__SSE__" :sse)
    ("__SSE2__" :sse2)
    ("__ELF__" :elf)
    ("__i386" :i386)
    ("__i386__" :i386)
    ("__amd64" :amd64)
    ("__x86_64__" :x86-64)
    ("__X86_64__" :x86-64)
    ("__LP64__" :lp64)
    ("_LP64" :lp64)
    ("__ILP32__" :ilp32)
    ("_ILP32" :ilp32)
    ("__powerpc" :powerpc)
    ("__PPC" :ppc)
    ("__PPC__" :ppc)
    ("__PPC64__" :ppc64)
    ("_PPC64_" :ppc64)
    ))

(defun run-and-collect-keywords (&rest args)
  (gather-keywords (apply #'run-and-collect args) +known-keywords+))

(defun gather-system-features (&key (executable
                                     #+(or windows cygwin) "sys:ecl_min.exe"
                                     #-(or windows cygwin) "sys:ecl_min"))
  (let* ((ecl-binary (namestring (truename executable)))
         (executable-features
          #-windows
           (run-and-collect-keywords "file" (list ecl-binary)))
         (compiler-version (run-and-collect-keywords c::*cc* '("--version")))
         (compiler-features (reduce #'append
                                    (mapcar #'rest
                                            (compiler-defines +compiler-macros+)))))
    (delete-duplicates (nconc executable-features
                              compiler-version
                              compiler-features)
       :test #'string-equal)))

(defun update-compiler-features (&rest args)
  (setf *compiler-features* (apply #'gather-system-features args)))

#+ecl-min
(update-compiler-features
 :executable
 #+(or windows cygwin) "build:ecl_min.exe"
 #-(or windows cygwin) "build:ecl_min")

#+ecl-min
(format t ";;; System features: ~A~%" *compiler-features*)
