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
