(defparameter *c++-copyright-body* "
/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/")
(defparameter *c++-copyright-tail* "/* -^- */")


(defparameter *lisp-copyright-body* "
;; Copyright (c) 2014, Christian E. Schafmeister
;; 
;; CLASP is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Library General Public
;; License as published by the Free Software Foundation; either
;; version 2.1 of the License, or (at your option) any later version.
;; 
;; See directory 'clasp/licenses' for full details.
;; 
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.
")
(defparameter *lisp-copyright-tail* ";; -^-")


(defstruct copyright-notice
  header-lambda
  body
  tail)

(defparameter *copyright-notices* (make-hash-table :test #'equal))
(defparameter *c++-copyright-notice* (make-copyright-notice :header-lambda #'(lambda (fn) (format nil "/*~%    File: ~a~%*/~%" fn)) :body *c++-copyright-body* :tail *c++-copyright-tail*))
(defparameter *lisp-copyright-notice* (make-copyright-notice :header-lambda #'(lambda (fn) (format nil ";;;~%;;;    File: ~a~%;;;~%" fn)) :body *lisp-copyright-body* :tail *lisp-copyright-tail*))
(progn
  (setf (gethash "cc" *copyright-notices*) *c++-copyright-notice*)
  (setf (gethash "h" *copyright-notices*) *c++-copyright-notice*)
  (setf (gethash "lsp" *copyright-notices*) *lisp-copyright-notice*)
  )

(defun read-entire-file (pn)
  (with-open-file (fin pn :direction :input)
    (let ((seq (make-string (file-length fin))))
      (read-sequence seq fin)
      seq)))

(defun write-entire-file (contents pn)
  (with-open-file (fout pn :direction :output :if-exists :rename)
    (write contents :escape nil :stream fout)
    (terpri fout)))

(defun remove-copyright (contents notice)
  "Remove the copyright and return the contents.  
If the copyright was removed the return the second value t"
  (let* ((cr-tail (copyright-notice-tail notice))
         (pos (search cr-tail contents)))
    (if pos
        (values (subseq contents (+ pos (length cr-tail))) t)
        (values contents nil))))

(defun insert-copyright (contents pathname notice saw-copyright)
  (let ((filename (format nil "~a.~a" (pathname-name pathname) (pathname-type pathname)))
        (body (copyright-notice-body notice))
        (tail (copyright-notice-tail notice)))
    (with-output-to-string (sout)
      (let ((header (funcall (copyright-notice-header-lambda notice) filename)))
        (format sout "~a" header)
        (format sout "~a~%" body)
        (format sout "~a" tail)
        (if (not saw-copyright) (format sout "~%"))
        (format sout "~a~%" contents)))))

(defun copywrite-one-file (path)
  (let* ((pn (pathname path))
         (filename (format nil "~a.~a" (pathname-name pn) (pathname-type pn)))
         (notice (gethash (pathname-type pn) *copyright-notices*))
         (contents (read-entire-file pn)))
    (multiple-value-bind (no-copyright saw-copyright)
        (remove-copyright contents notice)
      (let* ((result (insert-copyright no-copyright pn notice saw-copyright))
             (trimmed (string-right-trim '(#\newline #\linefeed #\nul #\space) result)))
        (write-entire-file trimmed pn)))))


(defun copywrite-all-files (pathname-list)
  (dolist (pn pathname-list)
    (format t "copywriting ~a~%" pn)
    (copywrite-one-file pn)))


;;(copywrite-one-file "/Users/meister/Development/clasp/src/core/foundation.h")
;;(copywrite-one-file "/Users/meister/Development/clasp/src/lisp/kernel/cmp/cmpeh.lsp")

(defparameter *all-files* (append
                           (directory "/Users/meister/Development/clasp/src/**/*.h")
                           (directory "/Users/meister/Development/clasp/src/**/*.cc")
                           (directory "/Users/meister/Development/clasp/src/lisp/kernel/cmp/*.lsp")))

(copywrite-all-files *all-files*)
