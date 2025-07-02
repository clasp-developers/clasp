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

(in-package #:cmp)

(defun builder (kind destination &rest keywords)
  "This is used by ASDF to build fasl files."
  (declare (ignore kind))
  (apply 'build-fasl destination keywords))

(export '(builder))

(defun build-fasl (out-file &key lisp-files init-name)
  (declare (ignore init-name))
  (let ((output-name (case *default-output-type*
                       (:bytecode
                        (core:link-fasl-files out-file lisp-files)
                        (truename out-file))
                       (otherwise
                        (error "Handle *default-output-type* ~a" *default-output-type*)))))
    output-name))

(export 'build-fasl)
