;;; -*- Mode: LISP; Syntax: Common-lisp; Package: PROCESS; Base: 10; Lowercase: Yes -*-

;;; Copyright (C) 1990 Symbolics, Inc.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Symbolics, Inc. provides this software "as is" without
;;; express or implied warranty.

(defflavor xlib::clx-lock () (simple-recursive-normal-lock)
  (:init-keywords :flavor))

(defwhopper (lock-internal xlib::clx-lock) (lock-argument)
  (catch 'timeout
    (continue-whopper lock-argument)))

(defmethod (lock-block-internal xlib::clx-lock) (lock-argument)
  (declare (dbg:locking-function describe-process-lock-for-debugger self))
  (when (null waiter-queue)
    (setf waiter-queue (make-scheduler-queue :name name))
    (setf timer (create-timer-call #'lock-timer-expired `(,self) :name name)))
  (let ((process (lock-argument-process lock-argument)))
    (unwind-protect
	(progn
	  (lock-map-over-conflicting-owners
	    self lock-argument
	    #'(lambda (other-lock-arg)
		(add-promotion process lock-argument
			       (lock-argument-process other-lock-arg) other-lock-arg)))
	  (unless (timer-pending-p timer)
	    (when (and (safe-to-use-timers %real-current-process)
		       (not dbg:*debugger-might-have-system-problems*))
	      (reset-timer-relative-timer-units timer *lock-timer-interval*)))
	  (assert (store-conditional (locf latch) process nil))
	  (sys:with-aborts-enabled (lock-latch)
	    (let ((timeout (lock-argument-getf lock-argument :timeout nil)))
	      (cond ((null timeout)
		     (promotion-block waiter-queue name #'lock-lockable self lock-argument))
		    ((and (plusp timeout)
			  (using-resource (timer process-block-timers)
			    ;; Yeah, we know about the internal representation
			    ;; of timers here.
			    (setf (car (timer-args timer)) %real-current-process)
			    (with-scheduler-locked
			      (reset-timer-relative timer timeout)
			      (flet ((lock-lockable-or-timeout (timer lock lock-argument)
				       (or (not (timer-pending-p timer))
					   (lock-lockable lock lock-argument))))
				(let ((priority (process-process-priority *current-process*)))
				  (if (ldb-test %%scheduler-priority-preemption-field priority)
				      (promotion-block waiter-queue name
						       #'lock-lockable-or-timeout
						       timer self lock-argument)
				      ;; Change to preemptive priority so that when
				      ;; unlock-internal wakes us up so we can have the lock,
				      ;; we will really wake up right away
				      (with-process-priority
					  (dpb 1 %%scheduler-priority-preemption-field
					       priority)
					(promotion-block waiter-queue name
						       #'lock-lockable-or-timeout
						       timer self lock-argument)))))
			      (lock-lockable self lock-argument)))))
		    (t (throw 'timeout nil))))))
      (unless (store-conditional (locf latch) nil process)
	(lock-latch-wait-internal self))
      (remove-promotions process lock-argument))))

(compile-flavor-methods xlib::clx-lock)
