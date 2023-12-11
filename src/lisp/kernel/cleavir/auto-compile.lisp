;;;
;;;    File: auto-compile.lisp
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
;;
;; Insert the compiler into the repl
;;
;; Don't use FORMAT here use core:fmt 
;; otherwise you will have problems when format.lisp is bootstrapped

(in-package :clasp-cleavir)

;;; Dump modules to ensure that the proper functions have 'llvm-sys:external-linkage
#+(or)
(eval-when (:compile-toplevel :execute)
  (setq cmp::*jit-dump-module* t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up the cmp:*CLEAVIR-COMPILE-HOOK* so that COMPILE uses Cleavir
;;
(eval-when (:execute :load-toplevel)
  (setq cmp:*cleavir-compile-hook* 'bir-compile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up the cmp:*CLEAVIR-COMPILE-FILE-HOOK* so that COMPILE-FILE uses Cleavir
;;
(eval-when (:execute :load-toplevel)
  (setq cmp:*cleavir-compile-file-hook* 'bir-loop-read-and-compile-file-forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up the core:*use-cleavir-compiler*
;; so that walk-method-lambda in method.lisp uses the cleavir compiler.
;;
(eval-when (:execute :load-toplevel)
  (setq core:*use-cleavir-compiler* t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; cleavir-implicit-compile-hook - compile the form in the given environment
;;;

(eval-when (:execute :load-toplevel)
  (setq core:*eval-with-env-hook* 
        #+bytecode 'core:interpret-eval-with-env
        #-bytecode 'cclasp-eval))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Hook the bytecode-to-bir compiler into cl:compile.
;;;

(setq cmp:*btb-compile-hook* 'clasp-bytecode-to-bir:compile-hook)
