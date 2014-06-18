;-*- Mode:     Lisp -*-
;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Fri Apr 12 CEST 2012
;;;; Contains: Mutex tests

(in-package :cl-test)

;;; Date: 12/04/2012
;;;	Non-recursive mutexes should signal an error when they
;;;	cannot be relocked.
(deftest mutex-001-recursive-error
    (let* ((mutex (mp:make-lock :name 'mutex-001-recursive-error)))
      (and
       (mp:get-lock mutex)
       (eq (mp:lock-owner mutex) mp:*current-process*)
       (handler-case
	   (progn (mp:get-lock mutex) nil)
	 (error (c) t))
       (mp:giveup-lock mutex)
       (null (mp:lock-owner mutex))
       (zerop (mp:lock-count mutex))
       t))
  t)

;;; Date: 12/04/2012
;;;	Recursive locks increase the counter.
(deftest mutex-002-recursive-count
    (let* ((mutex (mp:make-lock :name 'mutex-002-recursive-count :recursive t)))
      (and
       (loop for i from 1 upto 10
	  always (and (mp:get-lock mutex)
		      (= (mp:lock-count mutex) i)
		      (eq (mp:lock-owner mutex) mp:*current-process*)))
       (loop for i from 9 downto 0
	  always (and (eq (mp:lock-owner mutex) mp:*current-process*)
		      (mp:giveup-lock mutex)
		      (= (mp:lock-count mutex) i)))
       (null (mp:lock-owner mutex))
       (zerop (mp:lock-count mutex))
       t))
  t)


;;; Date: 12/04/2012
;;;	When multiple threads compete for a mutex, they should
;;;	all get the same chance of accessing the resource
;;;
(def-mp-test mutex-003-fairness
    (let* ((mutex (mp:make-lock :name 'mutex-001-fairness))
	   (nthreads 10)
	   (count 10)
	   (counter (* nthreads count))
	   (array (make-array count :element-type 'fixnum :initial-element 0)))
      (flet ((slave (n)
	       (loop with continue = t
		  for i from 1 by 1
		  while continue do
		    (mp:get-lock mutex)
		    (cond ((plusp counter)
			   (decf counter)
			   (setf (aref array n) i))
			  (t
			   (setf continue nil)))
		    (mp:giveup-lock mutex))))
	;; Launch all agents. They will be locked
	(let ((all-processes
	       (mp:with-lock (mutex)
		 (loop for n from 0 below nthreads
		    collect (mp:process-run-function n #'slave n)
		    ;; ... and give them some time to block on this mutex
		    finally (sleep 1)))))
	  ;; Now they are released and operate. They should all have
	  ;; the same share of counts.
	  (loop for p in all-processes
	     do (mp:process-join p))
	  (loop for i from 0 below nthreads
	     always (= (aref array i) count)))))
  t)

;;; Date: 12/04/2012
;;;	It is possible to kill processes waiting for a lock. We launch a lot of
;;;	processes, 50% of which are zombies: they acquire the lock and do not
;;;	do anything. These processes are then killed, resulting in the others
;;;	doing their job.
;;;
(def-mp-test mutex-004-interruptible
    (let* ((mutex (mp:make-lock :name "mutex-003-fairness"))
	   (nprocesses 20)
	   (counter 0))
      (flet ((normal-thread ()
	       (mp:with-lock (mutex)
		 (incf counter)))
	     (zombie-thread ()
	       (mp:with-lock (mutex)
		 (loop (sleep 10)))))
	(let* ((all-processes (loop for i from 0 below nprocesses
				 for zombie = (zerop (mod i 2))
				 for fn = (if zombie #'zombie-thread #'normal-thread)
				 collect (cons zombie
					       (mp:process-run-function
						"mutex-003-fairness"
						fn))))
	       (zombies (mapcar #'cdr (remove-if-not #'car all-processes))))
	  (and (zerop counter) ; No proces works because the first one is a zombie
	       (kill-and-wait zombies)
	       (progn (sleep 0.2) (= counter (/ nprocesses 2)))
	       (not (mp:lock-owner mutex))
	       t))))
  t)
