(in-package :clasp-cleavir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Actual auto compilation - use *autocompile-hook* to compile stuff.
;;; This is done in its own thread instead of inline to avoid weird
;;; time delays - if literally any call can result in a lengthy
;;; compilation, runtimes become unreliable/chaotic.
;;;

;;; Queue of commands to the autocompilation thread.
;;; A command is either :QUIT, meaning to stop compiling,
;;; or a cons (BYTECODE-FUNCTION . ENVIRONMENT) representing a job.
(defvar *autocompilation-queue* (core:make-queue 'autocompile))

;;; The autocompilation thread, if it yet exists, otherwise NIL.
(defvar *autocompilation-thread* nil)

;;; A flag telling the autocompilation thread to log its actions.
;;; The log can get pretty lengthy quickly. Useful for debugging
;;; the internal autocompilation mechanisms.
(defvar *autocompilation-logging* nil)

;;; A list of log entries, most recent first,
;;; output by the autocompilation thread.
(defvar *autocompilation-log* nil)

;;; Enqueue a command to the autocompilation thread.
;;; The queue implementation is not lock-free, so there is a
;;; small possibility that queue-autocompilation could be called
;;; while the lock is held, which could cause a deadlock. This
;;; is avoided by temporarily disabling autocompilation while
;;; the lock is held.
;;; Making the queue lock-free might be a more elegant solution.
(defun autocompilation-enqueue (command)
  (let ((cmp:*autocompile-hook* nil))
    (core:atomic-enqueue *autocompilation-queue* command)))

(defun autocompilation-dequeue ()
  (let ((cmp:*autocompile-hook* nil))
    (core:dequeue *autocompilation-queue*)))

(defun autocompilation-log ()
  ;; During build we're not set up to use cleavir processing to determine
  ;; specialness - FIXME - so we use explicit symbol-value.
  (mp:atomic (symbol-value '*autocompilation-log*) :order :relaxed))

(defun clear-autocompilation-log ()
  (setf (mp:atomic (symbol-value '*autocompilation-log*) :order :relaxed) nil))

;;; Value of *autocompile-hook*.
;;; We don't queue anything until start-autocompilation is run.
;;; Afterwards we queue even if the worker is not going - more work for later.
(defun queue-autocompilation (definition environment)
  (autocompilation-enqueue  (cons definition environment)))

(defun autocompile-worker ()
  (macrolet ((log (thing)
               `(when (mp:atomic (symbol-value '*autocompilation-logging*)
                                 :order :relaxed)
                  (mp:atomic-push-explicit ,thing
                                           ((symbol-value '*autocompilation-log*)
                                            :order :relaxed)))))
    (loop for item = (autocompilation-dequeue)
          when (eq item :quit)
            do (log item)
            and return nil
          when (consp item)
            do (let ((def (car item)) (env (cdr item)))
                 (declare (ignore env))
                 ;; Make sure it hasn't been compiled already.
                 (if (eq (core:entry-point def) def)
                     (handler-case (clasp-bytecode-to-bir:compile-module
                                    (core:simple-fun-code def))
                       (serious-condition (e)
                         (log `(:error ,def ,e)))
                       (:no-error (f)
                         (log `(:success ,def ,f))))
                     (log `(:redundant ,def))))
          else do (log `(:bad-queue ,item)))))


(defun start-autocompilation* ()
  (unless *autocompilation-thread*
    (write-line "Starting autocompilation...")
    (setf *autocompilation-thread*
          (mp:process-run-function 'autocompilation #'autocompile-worker)
          cmp:*autocompile-hook* 'queue-autocompilation)
    (mp:atomic-push-explicit :start ((symbol-value '*autocompilation-log*)
                                     :order :relaxed)))
  (values))

(defun stop-autocompilation* ()
  (when *autocompilation-thread*
    (setf *autocompilation-thread* nil)
    (core:atomic-enqueue *autocompilation-queue* :quit))
  (values))

(defun ext:start-autocompilation ()
  ;; If a thread already exists, ignore.
  ;; Note that this function is not thread safe. It's expected you'll only
  ;; use it manually.
  (unless *autocompilation-thread*
    ;; Setup turn off autocompilation for snapshot save
    ;;  and restart it after we snapshot load
    ;;  This means it will be on for repeated snapshot save/load cycles unless we
    ;;  remove ext:start-autocompilation from core:*initialize-hooks*
    (pushnew 'stop-autocompilation* core:*terminate-hooks*)
    (pushnew 'start-autocompilation* core:*initialize-hooks*)
    (start-autocompilation*)))

(defun ext:stop-autocompilation ()
  (when *autocompilation-thread*
    (setf core:*terminate-hooks* (remove 'start-autocompilation* core:*initialize-hooks*))
    (setf core:*terminate-hooks* (remove 'stop-autocompilation* core:*terminate-hooks*)))
  (stop-autocompilation*)
  (values))

(defun start-autocompilation-logging ()
  (setf (mp:atomic (symbol-value '*autocompilation-logging*) :order :relaxed) t))
(defun end-autocompilation-logging ()
  (setf (mp:atomic (symbol-value '*autocompilation-logging*) :order :relaxed) nil))

;;; map from simple funs to optimized versions,
;;; for reoptimization later
(defvar *deoptimized-funs* (make-hash-table :test #'eq))
;;; list of functions that have been deoptimized; used for presentation
;;; to the user etc
(defvar *deoptimized* nil)

(defun deoptimized () *deoptimized*)

(defun %deoptimize-function (function)
  (declare (type core:simple-fun function))
  (let ((opt (core:function/entry-point function)))
    (cond ((eq function opt) nil) ; no optimized version
          (t (setf (gethash function *deoptimized-funs*) opt)
             (core:set-simple-fun function function)
             t))))

(defun %map-module-functions (f module)
  (loop with any = nil
        for thing across (core:bytecode-module/debug-info module)
        ;; call on all functions - no quitting early so not THEREIS
        when (typep thing 'core:bytecode-simple-fun)
          do (setf any (or (funcall f thing) any))
        finally (return any)))

;;; When we deoptimize, we try to deoptimize everything in a
;;; function's module, such as inner functions. That's probably
;;; what the user wants, practically speaking, unlike with TRACE.
(defun %deoptimize (function)
  (if (typep function 'core:bytecode-simple-fun)
      (%map-module-functions #'%deoptimize-function
                             (core:simple-fun-code function))
      nil))

(defgeneric %map-simple-funs (f function)
  (:argument-precedence-order function f))

(defmethod %map-simple-funs (f (func core:simple-fun))
  (funcall f func))
(defmethod %map-simple-funs (f (function core:closure))
  (%map-simple-funs f (core:function/entry-point function)))
(defmethod %map-simple-funs (f (function clos:funcallable-standard-object))
  (%map-simple-funs f (clos:get-funcallable-instance-function function)))
(defmethod %map-simple-funs :after (f (function generic-function))
  (loop for method in (clos:generic-function-methods function)
        for mf = (clos:method-function method)
        do (%map-simple-funs f mf)))
;;; KLUDGE: placement?
(defmethod %map-simple-funs :after (f (function clos::%leaf-method-function))
  (%map-simple-funs f (clos::fmf function)))
(defmethod %map-simple-funs :after (f (function clos::%contf-method-function))
  (%map-simple-funs f (clos::contf function)))

(defun maybe-deoptimize (function)
  (let ((deoptimized-any nil))
    (flet ((deopt (sf)
             (when (%deoptimize sf) (setf deoptimized-any t))))
      (%map-simple-funs #'deopt function))
    (if deoptimized-any
        (push function *deoptimized*)
        (warn "~a has no optimized versions; skipping" function))))

(defmacro ext:deoptimize (&rest functions)
  `(progn ,@(loop for fdesig in functions
                  collect `(maybe-deoptimize #',fdesig))
          (deoptimized)))

(defun %reoptimize-function (function)
  (declare (type core:simple-fun function))
  (let ((opt (gethash function *deoptimized-funs*)))
    (cond (opt (remhash function *deoptimized-funs*)
               (core:set-simple-fun function opt)
               t)
          (t nil))))

(defun %reoptimize (function)
  (if (typep function 'core:bytecode-simple-fun)
      (%map-module-functions #'%reoptimize-function
                             (core:simple-fun-code function))
      nil))

(defun %reoptimize-all ()
  ;; Just reoptimize everything directly
  (maphash (lambda (fun opt) (core:set-simple-fun fun opt))
           *deoptimized-funs*)
  (clrhash *deoptimized-funs*)
  (setf *deoptimized* nil))

(defun maybe-reoptimize (function)
  (let ((reoptimized-any nil))
    (when (member function *deoptimized*)
      (flet ((reopt (sf)
               (when (%reoptimize sf) (setf reoptimized-any t))))
        (%map-simple-funs #'reopt function))
      (setf *deoptimized* (delete function *deoptimized*)))
    (unless reoptimized-any
      (warn "~a was not deoptimized; skipping" function))))

(defmacro ext:reoptimize (&rest functions)
  (if functions
      `(progn ,@(loop for fdesig in functions
                      collect `(maybe-reoptimize #',fdesig))
              (deoptimized))
      `(%reoptimize-all)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Also in here for reasons: the compile-file thread pool
;;; Native compilation is slow, but thread safe, so when we compile-file
;;; we do it in parallel. See cmpltv::*native-compile-file-all*

(in-package #:cmp)

;;; note: jobs are intended to be an implementation detail. the user of
;;; the thread pool only gets results; see THREAD-POOL-FINISH
(defclass native-compile-job ()
  ((%function :initarg :function :reader ncjob-function)
   (%arguments :initarg :arguments :reader ncjob-arguments)
   (%result :initform nil :accessor ncjob-result)
   (%serious-condition :initform nil :accessor ncjob-serious-condition)
   (%warnings :initform nil :accessor ncjob-warnings)
   (%notes :initform nil :accessor ncjob-notes)
   (%other-conditions :initform nil :accessor ncjob-other-conditions)
   ;; arbitrary data passed to thread-pool-enqueue
   ;; and returned by thread-pool-finish
   (%extra :initarg :extra :reader ncjob-extra)))

(defun native-compile-worker (queue)
  (lambda ()
    (declare (core:lambda-name native-compile-worker))
    (loop for job = (core:dequeue queue)
          until (eq job :quit)
          do (block nil
               (setf (ncjob-result job)
                     (handler-bind
                         ((serious-condition
                            (lambda (e)
                              (setf (ncjob-serious-condition job) e)
                              ;; can't continue, so go wait for more jobs
                              (return)))
                          (warning
                            (lambda (w)
                              (push w (ncjob-warnings job))
                              (muffle-warning w)))
                          (ext:compiler-note
                            (lambda (n)
                              (push n (ncjob-notes job))
                              (cmp::muffle-note n)))
                          ((not (or ext:compiler-note warning serious-condition))
                            (lambda (c)
                              (push c (ncjob-other-conditions job)))))
                       (apply (ncjob-function job)
                              (ncjob-arguments job))))))))

(defgeneric report-job-conditions (job))
(defmethod report-job-conditions ((job native-compile-job))
  (mapc #'signal (ncjob-other-conditions job))
  ;; The WARN calls here never actually print warnings - the
  ;; with-compilation-results handlers do, and then muffle the warnings
  ;; (which is why we use WARN and not SIGNAL). Kind of ugly.
  (mapc #'warn (ncjob-warnings job))
  (mapc #'cmp:note (ncjob-notes job))
  (when (ncjob-serious-condition job)
    ;; We use SIGNAL rather than ERROR although the condition is serious.
    ;; This is because the job has already exited and therefore there
    ;; is no way to debug the problem. with-compilation-results will
    ;; still understand that it's an error and report compilation failure.
    ;; It's possible we could save the original backtrace and so on, but
    ;; if you want to debug problems, it would probably be easier to
    ;; use the serial compiler and debug them as they appear.
    (signal (ncjob-serious-condition job))))

(defclass nc-thread-pool ()
  ((%threads :initarg :threads :reader nc-threads)
   (%queue :initarg :queue :reader nc-queue)
   (%jobs :initform nil :accessor nc-jobs)))

(defun make-nc-thread-pool (&key (name 'native-compile-thread-pool)
                              (nthreads (core:num-logical-processors))
                              special-bindings)
  (loop with queue = (core:make-queue name)
        with conc-name = (format nil "~(~a~)-" (symbol-name name))
        for thread-num below nthreads
        collect (mp:process-run-function
                 (format nil "~a-~d" conc-name thread-num)
                 (native-compile-worker queue)
                 special-bindings)
          into threads
        finally (return (make-instance 'nc-thread-pool
                          :queue queue :threads threads))))

(defun thread-pool-enqueue (pool function arguments &optional extra)
  (let ((job (make-instance 'native-compile-job
               :function function :arguments arguments :extra extra)))
    (push job (nc-jobs pool))
    (core:atomic-enqueue (nc-queue pool) job)))

(defun enqueue-native-compilation (pool module bytecode literals debug-info id
                                   debug-namestring)
  (thread-pool-enqueue pool #'clasp-bytecode-to-bir:compile-cmodule
                       (list bytecode literals debug-info id
                             debug-namestring)
                       module))

(defun thread-pool-quit (pool)
  (loop with queue = (nc-queue pool)
        for thread in (nc-threads pool)
        ;; only one is needed, but we do two out of an abundance of caution
        ;; and because it's harmless
        do (core:atomic-enqueue queue :quit)
           (core:atomic-enqueue queue :quit)))

(defun thread-pool-join (pool)
  (mapc #'mp:process-join (nc-threads pool)))

(defun thread-pool-finish (pool)
  (thread-pool-quit pool)
  (thread-pool-join pool)
  ;; ok, now everything is done: signal conditions and return results
  (loop for job in (nc-jobs pool)
        do (report-job-conditions job)
           ;; note that these are returned in the order they were enqueued.
           ;; probably not important? but it's nice
        collect (list (ncjob-extra job) (ncjob-result job))))
