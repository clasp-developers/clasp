;;; -*- mode: Common-Lisp; Base: 10 ; Syntax: ANSI-Common-Lisp ; buffer-read-only: t; -*-
;;; This is ASDF 3.1.2.7: Another System Definition Facility.
;;;
;;; Feedback, bug reports, and patches are all welcome:
;;; please mail to <asdf-devel@common-lisp.net>.
;;; Note first that the canonical source for ASDF is presently
;;; <URL:http://common-lisp.net/project/asdf/>.
;;;
;;; If you obtained this copy from anywhere else, and you experience
;;; trouble using it, or find bugs, you may want to check at the
;;; location above for a more recent version (and for documentation
;;; and test files, if your copy came without them) before reporting
;;; bugs.  There are usually two "supported" revisions - the git master
;;; branch is the latest development version, whereas the git release
;;; branch may be slightly older but is considered `stable'

;;; -- LICENSE START
;;; (This is the MIT / X Consortium license as taken from
;;;  http://www.opensource.org/licenses/mit-license.html on or about
;;;  Monday; July 13, 2009)
;;;
;;; Copyright (c) 2001-2014 Daniel Barlow and contributors
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; -- LICENSE END

;;; The problem with writing a defsystem replacement is bootstrapping:
;;; we can't use defsystem to compile it.  Hence, all in one file.

#+xcvb (module ())

(in-package :cl-user)

#+cmu
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf ext:*gc-verbose* nil))

;;; pre 1.3.0 ABCL versions do not support the bundle-op on Mac OS X
#+abcl
(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (and (member :darwin *features*)
               (second (third (sys::arglist 'directory))))
    (push :abcl-bundle-op-supported *features*)))

;; Punt on hard package upgrade: from ASDF1 always, and even from ASDF2 on most implementations.
(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (member :asdf3 *features*)
    (let* ((existing-version
             (when (find-package :asdf)
               (or (symbol-value (find-symbol (string :*asdf-version*) :asdf))
                   (let ((ver (symbol-value (find-symbol (string :*asdf-revision*) :asdf))))
                     (etypecase ver
                       (string ver)
                       (cons (format nil "~{~D~^.~}" ver))
                       (null "1.0"))))))
           (first-dot (when existing-version (position #\. existing-version)))
           (second-dot (when first-dot (position #\. existing-version :start (1+ first-dot))))
           (existing-major-minor (subseq existing-version 0 second-dot))
           (existing-version-number (and existing-version (read-from-string existing-major-minor)))
           (away (format nil "~A-~A" :asdf existing-version)))
      (when (and existing-version
                 (< existing-version-number
                    #+(or allegro clisp lispworks sbcl) 2.0
                    #-(or allegro clisp lispworks sbcl) 2.27))
        (rename-package :asdf away)
        (when *load-verbose*
          (format t "~&; Renamed old ~A package away to ~A~%" :asdf away))))))
