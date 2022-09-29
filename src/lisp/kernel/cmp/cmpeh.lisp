;;;
;;;    File: cmpeh.lisp
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


#|
For sbcl
(sb-ext:restrict-compiler-policy '!debug 3)

|#

(defvar *try.clause-stack* nil
  "Keep track of the nested try clauses")

(defmacro with-begin-end-catch ((exn exception-ptr rethrow-bb) &rest body)
  (let ((exn-gs (gensym)))
    `(let ((,exn-gs ,exn))
       (multiple-value-prog1
           (with-landing-pad ,rethrow-bb
             (let ((,exception-ptr (irc-intrinsic "__cxa_begin_catch" ,exn-gs)))
               #+debug-eh(irc-intrinsic "debugPrintI32" (jit-constant-i32 (incf *next-i32*)))
               ,@body))
         (with-landing-pad nil
           (irc-intrinsic "__cxa_end_catch"))))))


(defmacro with-block-name-prefix ( &optional (prefix "") &rest body )
  `(let ((*block-name-prefix* ,prefix))
     ,@body))


(defvar *current-unwind-landing-pad-dest* nil)

(defmacro with-landing-pad (unwind-landing-pad-dest &rest body)
  `(let ((*current-unwind-landing-pad-dest* ,unwind-landing-pad-dest))
     ,@body))

(defvar *exception-handling-level*)
(defvar *current-function-exn.slot*)
(defvar *current-function-ehselector.slot*)
(defvar *current-function-terminate-landing-pad* nil)

(defparameter *exception-handler-cleanup-block* nil)
(defparameter *exception-clause-types-to-handle* nil)

(defvar *next-i32* 1000)
