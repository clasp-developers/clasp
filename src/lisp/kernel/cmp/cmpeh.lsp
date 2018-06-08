;;;
;;;    File: cmpeh.lsp
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
(sb-ext:restrict-compiler-policy 'debug 3)

|#

(defun try.attach-dispatch-blocks-to-clauses (clauses)
  "Get a list of clauses of the form '((exception var) code...) and
generate a block for each exception and return a list of conses of the blocks with the clauses.
eg: '(block ((exception var) code...))"
  (mapcar #'(lambda (x)
	      (cons (irc-basic-block-create "dispatch") x)) clauses))


(defun try.separate-clauses (clauses)
  "Separate out the normal clauses from the default clause"
  (let (cleanup-clause-body exception-clauses all-other-exceptions-clause)
    (dolist (clause clauses)
      (unless (consp (car clause))
	(error "Every with-try clause head must be wrapped in a list - illegal clause: ~a" clause))
      (let ((head (caar clause)))
	(cond
	  ((eq head 'cleanup) (setq cleanup-clause-body (cdr clause)))
	  ((eq head 'all-other-exceptions) (setq all-other-exceptions-clause clause))
	  (t (push clause exception-clauses)))))
    (when all-other-exceptions-clause
      (push all-other-exceptions-clause exception-clauses))
    (values cleanup-clause-body (nreverse exception-clauses))))



(defvar *try.clause-stack* nil
  "Keep track of the nested try clauses")

(defun try.flatten (structure)
  (cond ((null structure ) nil)
	((atom structure) `(,structure))
	(t (mapcan #'try.flatten structure))))

(defun try.identify-all-unique-clause-types (all-clause-types)
  (let ((flattened-clause-types all-clause-types)
	unique-clause-types)
    (mapc #'(lambda (ct)
	      (if (member ct unique-clause-types)
		  nil
		  (push ct unique-clause-types)))
	  flattened-clause-types)
    unique-clause-types))


(defun try.add-landing-pad-clauses (landpad catch-clause-types)
  (let* ((types (reverse catch-clause-types))
	 (includes-all-other-exceptions (member 'all-other-exceptions types)))
    (mapc #'(lambda (ct)
	      (cond
		((eq ct 'all-other-exceptions)
		 nil ) ;; add the most general exception at the end
		(t
		 (irc-add-clause landpad (irc-exception-typeid* ct)))))
	  types)
    (when includes-all-other-exceptions
      (irc-add-clause landpad (llvm-sys:constant-pointer-null-get %i8*%)))))



(defmacro with-begin-end-catch ((exn.slot exception-ptr rethrow-bb) &rest body)
  (let ((cont-gs (gensym))
        (exn-gs (gensym)))
    `(let ((,exn-gs (llvm-sys:create-load-value-twine *irbuilder* ,exn.slot "exn")))
       (with-landing-pad ,rethrow-bb
         (let ((,exception-ptr (irc-intrinsic "__cxa_begin_catch" ,exn-gs)))
           #+debug-eh(irc-intrinsic "debugPrintI32" (jit-constant-i32 (incf *next-i32*)))
           ,@body))
       (with-landing-pad nil
         (irc-intrinsic "__cxa_end_catch")))))




(defun try.one-dispatcher-and-handler (cur-dispatcher-block
				       next-dispatcher-block
				       clause
				       successful-catch-block
				       exn.slot ehselector.slot
                                       rethrow-bb)
  (let ((sel-gs (gensym "sel"))
	(typeid-gs (gensym "typeid"))
	(matches-type-gs (gensym "matches-type"))
	(handler-block-gs (gensym "handler-block"))
	(clause-type (caar clause))
	(clause-exception-name (cadr (car clause)))
	(clause-body (cdr clause))
	)
    (cond
      ((eq (caar clause) 'all-other-exceptions)
       `(progn
	  (irc-begin-block ,cur-dispatcher-block)
	  (with-begin-end-catch (,exn.slot dummy-exception ,rethrow-bb)
	    ,@clause-body
	    ))
       )
      (t
       `(progn
	  (irc-begin-block ,cur-dispatcher-block)
	  (let* ((,sel-gs (irc-load ,ehselector.slot "ehselector-slot"))
		 (,typeid-gs (irc-intrinsic "llvm.eh.typeid.for"
                                            (irc-exception-typeid* ',clause-type)))
                 #+debug-eh(_          (irc-intrinsic "debugPrintI32" ,sel-gs))
                 #+debug-eh(_          (irc-intrinsic "debugPrintI32" ,typeid-gs))
		 (,matches-type-gs (irc-icmp-eq ,sel-gs ,typeid-gs))
		 (,handler-block-gs (irc-basic-block-create ,(symbol-name handler-block-gs)))
		 )
;;	    (irc-intrinsic "debugPrintI32" ,sel-gs)
;;	    (irc-intrinsic "debugPrintI32" ,typeid-gs)
	    (irc-cond-br ,matches-type-gs ,handler-block-gs ,next-dispatcher-block)
	    (irc-begin-block ,handler-block-gs)
	    (with-begin-end-catch (,exn.slot ,clause-exception-name ,rethrow-bb)
	      ,@clause-body)
	    (irc-branch-if-no-terminator-inst ,successful-catch-block) ;; Why is this commented out?
	    ))
       ))
    )
  )



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

(defun generate-rethrow-landing-pad (function
                                     previous-exception-handler-cleanup-block
                                     previous-exception-clause-types-to-handle
                                     exn.slot
                                     ehselector.slot)
  (let* ((rethrow-cleanup       (irc-basic-block-create "rethrow-cleanup" function))
         (_                     (irc-begin-block rethrow-cleanup))
         (lp                    (irc-create-landing-pad (length previous-exception-clause-types-to-handle) "rethrow-cleanup"))
         (_                     (try.add-landing-pad-clauses lp previous-exception-clause-types-to-handle))
         (_                     (irc-set-cleanup lp t))
         (_                     (preserve-exception-info lp exn.slot ehselector.slot))
         #+debug-eh(_                     (irc-intrinsic "debugPrintI32" (jit-constant-i32 (incf *next-i32*))))
         (_                     (with-landing-pad *current-function-terminate-landing-pad*
                                    (irc-intrinsic "__cxa_end_catch")))
         (_                     (irc-br previous-exception-handler-cleanup-block)))
    rethrow-cleanup))

(defun generate-ehcleanup-and-resume-code (function exn.slot ehselector.slot &optional cleanup-lambda)
  (let* ((ehbuilder       (llvm-sys:make-irbuilder *llvm-context*))
         (ehcleanup       (irc-basic-block-create "ehcleanup" function))
         (ehresume        (irc-basic-block-create "ehresume" function))
         (_               (irc-set-insert-point-basic-block ehcleanup ehbuilder))
         (_               (and cleanup-lambda
                               (with-irbuilder (ehbuilder)
                                 (funcall cleanup-lambda))))
         (_               (llvm-sys:create-br ehbuilder ehresume))
         (_               (irc-set-insert-point-basic-block ehresume ehbuilder))
         (exn7            (llvm-sys:create-load-value-twine ehbuilder exn.slot "exn7"))
         (sel             (llvm-sys:create-load-value-twine ehbuilder ehselector.slot "sel"))
         (undef           (llvm-sys:undef-value-get %exception-struct% ))
         (lpad.val        (llvm-sys:create-insert-value ehbuilder undef exn7 '(0) "lpad.val"))
         (lpad.val8       (llvm-sys:create-insert-value ehbuilder lpad.val sel '(1) "lpad.val8"))
         (_               (llvm-sys:create-resume ehbuilder lpad.val8)))
    ehcleanup))


(defun generate-terminate-code (function)
  (let* ((terminate-basic-block     (irc-basic-block-create "terminate" function))
         (ehbuilder                 (llvm-sys:make-irbuilder *llvm-context*))
         (_                         (irc-set-insert-point-basic-block terminate-basic-block ehbuilder)))
    (with-irbuilder (ehbuilder)
      (let* ((landpad                   (irc-create-landing-pad 1))
             (_                         (llvm-sys:add-clause landpad (llvm-sys:constant-pointer-null-get %i8*%)))
             (_                         (dbg-set-current-debug-location-here))
             (_                         (irc-intrinsic "clasp_terminate"))
             (_                         (irc-unreachable)))))
    terminate-basic-block))
                           


;;; ------------------------------------------------------------
;;;
;;; This macro sets up the function for exception handling.
;;; It allocates space for the exn.slot and ehselector.slot
;;; and creates the ehcleanup and ehresume blocks.
;;; ehcleanup doesn't do anything but jump to ehresume
;;; ehresume evokes the 'resume' instruction
(defmacro with-new-function-prepare-for-try ((function irbuilder-entry) &body body)
  `(let* ((*exception-handling-level* 0)
          (*exception-clause-types-to-handle* nil)
          (*current-function-exn.slot* (irc-alloca-i8* :irbuilder ,irbuilder-entry :label "exn.slot"))
          (*current-function-ehselector.slot* (irc-alloca-i32-no-init :irbuilder ,irbuilder-entry :label "ehselector.slot"))
          (*exception-handler-cleanup-block* (generate-ehcleanup-and-resume-code ,function *current-function-exn.slot* *current-function-ehselector.slot*))
          (*current-function-terminate-landing-pad* (generate-terminate-code ,function))
          (*current-unwind-landing-pad-dest* *current-function-terminate-landing-pad*))
     ,@body))

(defmacro with-try (code &rest catch-clauses)
  `(let ((*exception-handling-level* (1+ *exception-handling-level*)))
     (with-try*
         ,code
       ,@catch-clauses)))

(defun preserve-exception-info (lpad exn.slot ehselector.slot)
  (let ((exception-structure (llvm-sys:create-extract-value *irbuilder* lpad (list 0) "")))
    (llvm-sys:create-store *irbuilder* exception-structure exn.slot nil))
  (let ((exception-selector (llvm-sys:create-extract-value *irbuilder* lpad (list 1) "")))
    (llvm-sys:create-store *irbuilder* exception-selector ehselector.slot nil)))

(defvar *next-try-id* 0)
#+threads (defvar *next-try-id-mutex* (mp:make-mutex 'next-try-id))
(defun next-try-id ()
  #-threads (incf *next-try-id*)
  #+threads (unwind-protect
                 (progn
                   (mp:lock *next-try-id-mutex* nil)
                   (incf *next-try-id*))
              (mp:unlock *next-try-id-mutex*)))

(defmacro with-try* (code &rest catch-clauses)
  "with-try macro sets up exception handling for a block of code.
with-try creates one landing-pad that lists the exception clauses for this with-try block
and all other with-try blocks that this with-try is nested within.
WITH-TRY then sets up a chain of dispatchers that test if any exception that
lands at the landing pad match any of the catch-clauses and generates code for each of
the catch-clauses. If a catch-clause is evaluated then the flow drops out of the bottom
of the with-try.  The chain of dispatchers is connected to the chain of dispatchers
from the with-try that nests this with-try.
Cleanup code is codegen'd right after the CODE and just after the landing-pad instruction
just before any of the dispatchers.
A very important internal parameter is HIGHER-CLEANUP-BLOCK - this is a keyword symbol
that is used to store and lookup in the environment the next cleanup-block for passing
exceptions to higher levels of the code and unwinding the stack.
"
;;;  (declare (optimize (debug 0) (safety 0) (speed 0)))
  (let ((parent-cleanup-block-gs (gensym "parent-cleanup-block"))
	(landing-pad-block-gs (gensym "landing-pad-block"))
	(all-clause-types-gs (gensym "all-clause-types"))
	(unique-clause-types-gs (gensym "unique-clause-types"))
	(cont-block-gs (gensym "cont-block"))
	;;	(cleanup-block-gs (gensym "cleanup-block"))
	(landpad-gs (gensym "landingpad"))
	;;	(clause-types-gs (gensym "clause-types"))
	(ehselector.slot-gs (gensym "ehselector.slot"))
	(dispatch-header-gs (gensym "dispatch-header"))
	;;	(cur-disp-block-gs (gensym "cur-disp-block"))
	;;	(cur-clause-gs (gensym "cur-clause"))
	(exn.slot-gs (gensym "exn.slot"))
        (rethrow-bb-gs (gensym "rethrow-bb"))
        (previous-exception-clause-types-to-handle-gs (gensym "previous-exception-clause-types-to-handle"))
        (previous-exception-handler-cleanup-block-gs (gensym "previous-exception-handler-cleanup-block"))
	)
    (multiple-value-bind (cleanup-clause-body exception-clauses)
	(try.separate-clauses catch-clauses)
      (or cleanup-clause-body (warn "You should include a cleanup-clause otherwise eh may - catch clauses: ~a" catch-clauses))
      (let* ((my-clause-types (mapcar #'caar exception-clauses))
	     (dispatcher-block-gensyms
               (mapcar #'(lambda (x) (gensym (bformat nil "dispatch-%s-" (symbol-name (caar x)))))
                       exception-clauses))
	     (first-dispatcher-block-gs (car dispatcher-block-gensyms)))
	;;	     (cleanup-clause-list (cons cleanup-clause-body (make-list (- (length exception-clauses) 1)))))
	`(with-block-name-prefix (bformat nil "(TRY%d)." (next-try-id))
           (irc-branch-to-and-begin-block (irc-basic-block-create "top"))
           (let* ((,all-clause-types-gs (if ',my-clause-types
                                            (append ',my-clause-types *exception-clause-types-to-handle*)
                                            *exception-clause-types-to-handle*))
                  (,previous-exception-clause-types-to-handle-gs *exception-clause-types-to-handle*)
                  (*exception-clause-types-to-handle* ,all-clause-types-gs)
                  (,parent-cleanup-block-gs *exception-handler-cleanup-block*)
                  (,landing-pad-block-gs (irc-basic-block-create "landing-pad"))
                  (,dispatch-header-gs (irc-basic-block-create "dispatch-header"))
                  (,cont-block-gs (irc-basic-block-create "try-cont"))
                  (,previous-exception-handler-cleanup-block-gs *exception-handler-cleanup-block*)
                  (*exception-handler-cleanup-block* ,dispatch-header-gs ))
             (cmp-log "====>> In TRY --> parent-cleanup-block: %s%N" ,parent-cleanup-block-gs)
             (let ,(mapcar #'(lambda (var-name)
                               (list var-name `(irc-basic-block-create ,(symbol-name var-name))))
                    dispatcher-block-gensyms)
               (multiple-value-prog1
                   (with-landing-pad ,landing-pad-block-gs
                     ,code)
                 ,(when cleanup-clause-body
                    `(progn
                       (irc-branch-to-and-begin-block (irc-basic-block-create "normal-cleanup"))
                       ,@cleanup-clause-body))
                 (irc-branch-if-no-terminator-inst ,cont-block-gs)
                 (irc-begin-landing-pad-block ,landing-pad-block-gs)
                 (let* ((,unique-clause-types-gs (try.identify-all-unique-clause-types ,all-clause-types-gs))
                        (,landpad-gs (irc-create-landing-pad (length ,unique-clause-types-gs) "")))
                   (try.add-landing-pad-clauses ,landpad-gs ,unique-clause-types-gs)
                   (dbg-set-current-debug-location-here)
                   (irc-low-level-trace :eh-landing-pads)
                   ,(when cleanup-clause-body
                      `(irc-set-cleanup ,landpad-gs t))
                   (let ((,exn.slot-gs *current-function-exn.slot*)
                         (,ehselector.slot-gs *current-function-ehselector.slot*))
                     (preserve-exception-info ,landpad-gs ,exn.slot-gs ,ehselector.slot-gs)
                     #+debug-eh(irc-intrinsic "debugPrintI32" (jit-constant-i32 (incf *next-i32*)))
                     (irc-low-level-trace :flow)
                     (irc-branch-to-and-begin-block ,dispatch-header-gs)
                     #+debug-eh(irc-intrinsic "debugPrintI32" (jit-constant-i32 (incf *next-i32*)))
                     ,@(when cleanup-clause-body
                         cleanup-clause-body)
                     ,(if first-dispatcher-block-gs
                          `(progn
                             #+debug-eh(irc-intrinsic "debugPrintI32" (jit-constant-i32 (incf *next-i32*)))
                             (irc-br ,first-dispatcher-block-gs "first-dispatcher-block-gs"))
                          `(irc-br ,parent-cleanup-block-gs "parent-cleanup-block-gs"))
                     ,(when exception-clauses
                        `(let ((,rethrow-bb-gs (generate-rethrow-landing-pad *current-function*
                                                                             ,previous-exception-handler-cleanup-block-gs
                                                                             ,previous-exception-clause-types-to-handle-gs
                                                                             *current-function-exn.slot*
                                                                             *current-function-ehselector.slot*)))
                           ,@(maplist #'(lambda (cur-disp-block-gs cur-clause-gs)
                                          (try.one-dispatcher-and-handler (car cur-disp-block-gs)
                                                                          (if (cadr cur-disp-block-gs)
                                                                              (cadr cur-disp-block-gs)
                                                                              parent-cleanup-block-gs)
                                                                          (car cur-clause-gs)
                                                                          cont-block-gs
                                                                          exn.slot-gs ehselector.slot-gs
                                                                          rethrow-bb-gs))
                                      dispatcher-block-gensyms
                                      exception-clauses)))))
                 (irc-branch-if-no-terminator-inst ,cont-block-gs)
                 (irc-begin-block ,cont-block-gs)))))))))
