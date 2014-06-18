;-*- Mode:     Lisp -*-
;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Multithreading API regression tests

(in-package :cl-test)

;;; Date: 14/04/2012
;;;	Ensure that at creation name and counter are set
(deftest sem-make-and-counter
    (loop with name = "sem-make-and-counter"
       for count from 0 to 10
       for sem = (mp:make-semaphore :name name :count count)
       always (and (eq (mp:semaphore-name sem) name)
		   (= (mp:semaphore-count sem) count)
		   (zerop (mp:semaphore-wait-count sem))))
  t)

;;; Date: 14/04/2012
;;;	Ensure that signal changes the counter by the specified amount
(deftest sem-signal-semaphore-count
    (loop with name = "sem-signal-semaphore-count"
       for count from 0 to 10
       always (loop for delta from 0 to 10
		 for sem = (mp:make-semaphore :name name :count count)
		 always (and (= (mp:semaphore-count sem) count)
			     (null (mp:signal-semaphore sem delta))
			     (= (mp:semaphore-count sem ) (+ count delta)))))
  t)

;;; Date: 14/04/2012
;;;	A semaphore with a count of zero blocks a process
(def-mp-test sem-signal-one-process
    (let* ((flag nil)
	   (sem (mp:make-semaphore :name "sem-signal-one"))
	   (a-process (mp:process-run-function
		       "sem-signal-one-process"
		       #'(lambda ()
			   (mp:wait-on-semaphore sem)
			   (setf flag t)))))
      (and (null flag)
	   (mp:process-active-p a-process)
	   (progn (mp:signal-semaphore sem) (sleep 0.2) flag)
	   (= (mp:semaphore-count sem) 0)))
  t)

;;; Date: 14/04/2012
;;;	We can signal multiple processes
(def-mp-test sem-signal-n-processes
    (loop for count from 1 upto 10 always
	 (let* ((counter 0)
		(lock (mp:make-lock :name "sem-signal-n-processes"))
		(sem (mp:make-semaphore :name "sem-signal-n-processs"))
		(all-process
		 (loop for i from 1 upto count
		    collect (mp:process-run-function
			     "sem-signal-n-processes"
			     #'(lambda ()
				 (mp:wait-on-semaphore sem)
				 (mp:with-lock (lock) (incf counter)))))))
	   (and (zerop counter)
		(every #'mp:process-active-p all-process)
		(= (mp:semaphore-wait-count sem) count)
		(progn (mp:signal-semaphore sem count) (sleep 0.2)
		       (= counter count))
		(= (mp:semaphore-count sem) 0))))
  t)

;;; Date: 14/04/2012
;;;	When we signal N processes and N+M are waiting, only N awake
(def-mp-test sem-signal-only-n-processes
    (loop for m from 1 upto 3 always
      (loop for n from 1 upto 4 always
	 (let* ((counter 0)
		(lock (mp:make-lock :name "sem-signal-n-processes"))
		(sem (mp:make-semaphore :name "sem-signal-n-processs"))
		(all-process
		 (loop for i from 1 upto (+ n m)
		    collect (mp:process-run-function
			     "sem-signal-n-processes"
			     #'(lambda ()
				 (mp:wait-on-semaphore sem)
				 (mp:with-lock (lock) (incf counter)))))))
	   (and (zerop counter)
		(every #'mp:process-active-p all-process)
		(= (mp:semaphore-wait-count sem) (+ m n))
		(progn (mp:signal-semaphore sem n) (sleep 0.02)
		       (= counter n))
		(= (mp:semaphore-wait-count sem) m)
		(progn (mp:signal-semaphore sem m) (sleep 0.02)
			      (= counter (+ n m)))
		))))
  t)

;;; Date: 14/04/2012
;;;	It is possible to kill processes waiting for a semaphore.
;;;
(def-mp-test sem-interruptible
    (loop with sem = (mp:make-semaphore :name "sem-interruptible")
       with flag = nil
       for count from 1 to 10
       for all-processes = (loop for i from 1 upto count
			      collect (mp:process-run-function
				       "sem-interruptible"
				       #'(lambda ()
					   (mp:wait-on-semaphore sem)
					   (setf flag t))))
       always (and (progn (sleep 0.2) (null flag))
		   (every #'mp:process-active-p all-processes)
		   (= (mp:semaphore-wait-count sem) count)
		   (mapc #'mp:process-kill all-processes)
		   (progn (sleep 0.2) (notany #'mp:process-active-p all-processes))
		   (null flag)
		   (zerop (mp:semaphore-wait-count sem))
		   t))
  t)

;;; Date: 14/04/2012
;;;	When we kill a process, it is removed from the wait queue.
;;;
(def-mp-test sem-interrupt-updates-queue
    (let* ((sem (mp:make-semaphore :name "sem-interrupt-updates-queue"))
	   (process (mp:process-run-function
		     "sem-interrupt-updates-queue"
		     #'(lambda () (mp:wait-on-semaphore sem)))))
      (sleep 0.2)
      (and (= (mp:semaphore-wait-count sem) 1)
	   (mp:process-active-p process)
	   (progn (mp:process-kill process)
		  (sleep 0.2)
		  (not (mp:process-active-p process)))
	   (zerop (mp:semaphore-wait-count sem))
	   t))
  t)

;;; Date: 14/04/2012
;;;	When we kill a process, it signals another one. This is tricky,
;;;     because we need the awake signal to arrive _after_ the process is
;;;	killed, but the process must still be in the queue for the semaphore
;;;	to awake it. The way we solve this is by intercepting the kill signal.
;;;
(def-mp-test sem-interrupted-resignals
    (let* ((sem (mp:make-semaphore :name "sem-interrupted-resignals"))
	   (flag1 nil)
	   (flag2 nil)
	   (process1 (mp:process-run-function
		      "sem-interrupted-resignals"
		      #'(lambda ()
			  (unwind-protect
			       (mp:wait-on-semaphore sem)
			    (sleep 4)
			    (setf flag1 t)
			    ))))
	   (process2 (mp:process-run-function
		      "sem-interrupted-resignals"
		      #'(lambda ()
			  (mp:wait-on-semaphore sem)
			  (setf flag2 t)))))
      (sleep 0.2)
      (and (= (mp:semaphore-wait-count sem) 2)
	   (mp:process-active-p process1)
	   (mp:process-active-p process2)
	   ;; We kill the process but ensure it is still running
	   (progn (mp:process-kill process1)
		  (mp:process-active-p process1))
	   (null flag1)
	   ;; ... and in the queue
	   (= (mp:semaphore-wait-count sem) 2)
	   ;; We awake it and it should awake the other one
	   (progn (format t "~%;;; Signaling semaphore")
		  (mp:signal-semaphore sem)
		  (sleep 1)
		  (zerop (mp:semaphore-wait-count sem)))
	   flag2
	   t))
  t)

;;; Date: 14/04/2012
;;;	1 producer and N consumers, non-blocking, because the initial count
;;;	is larger than the consumed data.
(def-mp-test sem-1-to-n-non-blocking
    (loop with counter = 0
       with lock = (mp:make-lock :name "sem-1-to-n-communication")
       for n from 1 to 10
       for m = (round 128 n)
       for length = (* n m)
       for sem = (mp:make-semaphore :name "sem-1-to-n-communication" :count length)
       for producers = (progn
			 (setf counter 0)
			 (loop for i from 0 below n
			    collect (mp:process-run-function
				     "sem-1-to-n-consumer"
				     #'(lambda ()
					 (loop for i from 0 below m
					    do (mp:wait-on-semaphore sem)
					    do (mp:with-lock (lock) (incf counter)))))))
       do (mapc #'mp:process-join producers)
       always (and (= counter length)
		   (zerop (mp:semaphore-count sem))
		   (zerop (mp:semaphore-wait-count sem))))
  t)

;;; Date: 14/04/2012
;;;	1 producer and N consumers, blocking due to a slow producer.
(def-mp-test sem-1-to-n-blocking
    (loop with lock = (mp:make-lock :name "sem-1-to-n-communication")
       for n from 1 to 10
       for m = (round 10000 n)
       for length = (* n m)
       for sem = (mp:make-semaphore :name "sem-1-to-n-communication" :count 0)
       for counter = 0
       for producers = (loop for i from 0 below n
			  collect (mp:process-run-function
				   "sem-1-to-n-consumer"
				   #'(lambda ()
				       (loop for i from 0 below m
					  do (mp:wait-on-semaphore sem))
				       (mp:with-lock (lock) (incf counter)))))
       do (loop for i from 0 below length
	     do (mp:signal-semaphore sem))
       do (mapc #'mp:process-join producers)
       always (and (= counter n)
		   (zerop (mp:semaphore-count sem))
		   (zerop (mp:semaphore-wait-count sem))))
  t)
4