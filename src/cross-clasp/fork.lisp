(in-package #:cross-clasp)

(defclass fork-worker ()
  ((%pid :initarg :pid :reader pid)
   (%index :initarg :index :reader index)
   (%arguments :initarg :arguments :reader child-arguments)
   (%child-stdout :initarg :child-stdout :reader child-stdout)
   (%child-stderr :initarg :child-stderr :reader child-stderr)
   (%start-time :initform (get-internal-run-time)
                :reader start-time)
   (%finish-time :accessor finish-time)
   (%signal :initform nil :accessor child-signal)
   (%status :initform nil :accessor child-status)))

(defgeneric job-description (job)
  (:method ((job fork-worker))
    ;; FIXME: Durr
    (first (child-arguments job))))

(defun ansi-control (&optional level)
  (si:fmt t "%e[{:d}m"
          (cond ((eq level :err)  31)
                ((eq level :warn) 33)
                ((eq level :emph) 32)
                ((eq level :debug) 36)
                ((eq level :info) 37)
                (t 0))))

(defun message (level control-string &rest args)
  (ansi-control level)
  (apply #'format t control-string args)
  (ansi-control)
  (terpri))

(defun message-fd (level fd)
  (let ((buffer (make-array 1024 :element-type 'base-char :adjustable nil)))
    (ansi-control level)
    (si:lseek fd 0 :seek-set)
    (loop (multiple-value-bind (num-read errno)
              (si:read-fd fd buffer)
            (declare (ignore errno))
            (if (> num-read 0)
                (write-sequence buffer t :start 0 :end num-read)
                (return))))
    (si:close-fd fd) ; FIXME: unwind protect?
    (ansi-control)))

;;; Given JOBS, which is a hash table from PIDs to FORK-WORKERs,
;;; wait for one job to finish, remove the job from JOBS, and return
;;; the job.
(defun wait-on-children (jobs)
  (loop (multiple-value-bind (wpid status)
            (si:wait)
          (when (= -1 wpid)
            (message :err "No children left to wait on.")
            (si:exit 1))
          (let ((entry (gethash wpid jobs)))
            (cond ((si:wifsignaled status)
                   (setf (child-signal entry) (si:wtermsig status))
                   (remhash wpid jobs)
                   (return entry))
                  ((si:wifexited status)
                   (setf (child-status entry) (si:wexitstatus status))
                   (remhash wpid jobs)
                   (return entry)))))))

;;; Display information about a completed job.
(defun display-job (job)
  (let ((failedp
          (or (child-signal job) (not (zerop (child-status job))))))
    (message (if failedp :err :emph)
             "~:[Compiled~;Failed~] [~d~@[ of ~d~]] ~a~%"
             failedp
             (index job) *total-jobs* (job-description job)))
  (message-fd :info (child-stdout job))
  (message-fd :warn (child-stderr job))
  (cond ((child-signal job)
         (message :err "~&~tProcess exited with signal ~a"
                  (child-signal job)))
        ((not (zerop (child-status job)))
         (message :err "~&~tProcess exited with status ~a"
                  (child-status job)))))

;;; Fork off a process to apply FUNCTION to ARGUMENTS.
(defun %fork-worker (jobs index function &rest arguments)
  (let ((child-stdout (si:mkstemp-fd "clasp-build-stdout"))
        (child-stderr (si:mkstemp-fd "clasp-build-stderr")))
    (multiple-value-bind (maybe-error pid)
        (si:fork-redirect child-stdout child-stderr)
      (let ((job (make-instance 'fork-worker
                   :child-stdout child-stdout
                   :child-stderr child-stderr
                   :arguments arguments
                   :pid pid :index index)))
        (cond ((zerop pid)
               ;; Child
               ;; FIXME: immensely stupid kludge to get at host EXT
               (#.(let ((*package* (find-package "KEYWORD")))
                    (find-symbol "DISABLE-DEBUGGER" "EXT")))
               ;;(ext:disable-debugger)
               (apply function arguments)
               (setf (finish-time job) (get-internal-run-time))
               (sys:c_exit))
              (t
               ;; Parent
               (setf (gethash pid jobs) job)))))))

(defvar *jobs*)
(defvar *running-job-count*)
(defvar *max-running*)
(defvar *next-job-index*)
(defvar *total-jobs*)

(defun call-with-forking (parallel-jobs total-jobs thunk)
  (let ((*jobs* (make-hash-table))
        (*running-job-count* 0)
        (*max-running* parallel-jobs)
        (*next-job-index* 0)
        (*total-jobs* total-jobs))
    (multiple-value-prog1
        (funcall thunk)
      (finish-output)
      ;; Wait for all forks to finish.
      (loop until (zerop *running-job-count*)
            do (display-job (wait-on-children *jobs*))
               (decf *running-job-count*)))))

(defmacro with-forking ((&key (parallel-jobs '1)
                              (total-jobs nil))
                        &body body)
  `(call-with-forking ,parallel-jobs ,total-jobs
                      (lambda () (progn ,@body))))

(defun fork-worker (function &rest arguments)
  (loop while (> *running-job-count* *max-running*)
        do (display-job (wait-on-children *jobs*))
           (decf *running-job-count*))
  (apply #'%fork-worker *jobs* *next-job-index* function arguments)
  (incf *running-job-count*)
  (incf *next-job-index*))

;;; For each element in ARGLISTS, we fork and call WORK on the
;;; arglist, while in the main thread we call SEQWORK on the arglist.
;;; For building, WORK builds the native FASL, while SEQWORK loads
;;; the previously created CFASL.
(defun fork-loop (work seqwork arglists &key (parallel-jobs 1))
  (let ((jobs (make-hash-table))
        (running 0)
        (count (length arglists)))
    (loop for arglist in arglists
          for i from 0
          ;; Wait for jobs to finish until we can set up a new worker.
          do (loop until (< running parallel-jobs)
                   do (display-job (wait-on-children jobs) count)
                      (decf running))
             ;; now fork off the work
             (apply #'fork-worker jobs index work arglist)
             (incf running)
             ;; and do the sequential action
             (funcall seqwork arglist))
    ;; Wait for all forks to finish.
    (loop until (zerop running)
          do (display-job (wait-on-children jobs) count)
             (decf running)))
  (values))
