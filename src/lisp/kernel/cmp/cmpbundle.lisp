;;;
;;;    File: cmpbundle.lisp
;;;

;; Copyright (c) 2014, Christian E. Schafmeister
;;
;; CLASP is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Library General Public
;; License as published by the Free Software Foundation; either
;; version 2 of the License, or (at your option) any later version.
;;
;; See directory 'clasp/licenses' for full details.
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;; -^-

(in-package :cmp)

(defun as-shell-command (list-of-args)
  (with-output-to-string (sout)
    (princ (car list-of-args) sout)
    (dolist (c (cdr list-of-args))
      (core:fmt sout " {}" c))))

(defvar *safe-system-echo* nil)
(defvar *safe-system-max-retries* 4)
(defvar *safe-system-retry-wait-time* 0.1d0) ;; 100 milliseconds
;; The wait time will be doubled at each retry!

(defun safe-system (cmd-list &key output-file-name)
  (if *safe-system-echo*
      (core:fmt t "safe-system: {}%N" cmd-list))

  (multiple-value-bind (retval error-message)
      (ext:vfork-execvp cmd-list)

    (unless (eql retval 0)
      (error "Could not execute command with ext:vfork-execvp with ~s~%  return-value: ~d  error-message: ~s~%" cmd-list retval error-message)))

  (when output-file-name
    (let ((sleep-time *safe-system-retry-wait-time*))
      (dotimes (nm1 (- *safe-system-max-retries* 1))
        (let ((n (+ nm1 1)))
          (unless (probe-file output-file-name)
            (if (>= n *safe-system-max-retries*)
                (error "The file ~a was not created by shell command: ~a" output-file-name (as-shell-command cmd-list))
                (progn
                  (if *safe-system-echo*
                      (core:fmt t "safe-system: Retry count = {} of {}%N" n *safe-system-max-retries*))
                  (core::sleep sleep-time)
                  (setq sleep-time (* 2 sleep-time)))))))))

  ;; Return T if all went well
  t)

(defun link-bitcode-modules-impl (output-pathname part-pathnames
                                  &key output-type)
  "Link a bunch of modules together, return the linked module"
  (let* ((module (link-bitcode-modules-together (namestring output-pathname) part-pathnames :output-type output-type))
         (*compile-file-pathname* (pathname (merge-pathnames output-pathname)))
         (*compile-file-truename* (translate-logical-pathname *compile-file-pathname*)))
    (write-bitcode module (core:coerce-to-filename (pathname (if output-pathname
                                                                 output-pathname
                                                                 (error "The output pathname is NIL"))))
                   :output-type output-type)
    module))

(defun link-fasobc-modules (output-pathname part-pathnames)
  (link-bitcode-modules-impl output-pathname part-pathnames
                             :output-type :fasobc))

(defun link-fasoll-modules (output-pathname part-pathnames)
  (link-bitcode-modules-impl output-pathname part-pathnames
                             :output-type :fasoll))

(export '(link-fasoll-modules link-fasobc-modules))

(defun builder (kind destination &rest keywords)
  "This is used by ASDF to build fasl files."
  (declare (ignore kind))
  (apply 'build-fasl destination keywords))

(export '(builder))

(defun build-faso-parallel (out-file &key lisp-files)
  #+(or)
  (progn
    (format t "Linking ~s --> ~s~%" lisp-files out-file)
    (format t "About to do link of ~s to ~s~%" lisp-files out-file))
  (core:link-faso-files out-file lisp-files)
  (truename out-file))

(defun build-fasl (out-file &key lisp-files init-name)
  (declare (ignore init-name))
  (let ((output-name (case *default-output-type*
                       (:faso
                        (build-faso-parallel out-file :lisp-files lisp-files))
                       (:fasoll
                        (link-fasoll-modules out-file lisp-files)
                        (truename out-file))
                       (:fasobc
                        (link-fasobc-modules out-file lisp-files)
                        (truename out-file))
                       (:bytecode
                        (core:link-fasl-files out-file lisp-files)
                        (truename out-file))
                       (otherwise
                        (error "Handle *default-output-type* ~a" *default-output-type*)))))
    output-name))

(export 'build-fasl)
