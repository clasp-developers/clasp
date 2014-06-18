;-*- Mode:     Lisp -*-
;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Multithreading API regression tests

(in-package :cl-test)

;;; Date: 14/04/2012
;;;	Ensure that at creation name and counter are set, and mailbox is empty.
(deftest mailbox-make-and-counter
    (loop with name = "mbox-make-and-counter"
       for count from 4 to 63
       for mbox = (mp:make-mailbox :name name :count count)
       always (and (eq (mp:mailbox-name mbox) name)
		   (>= (mp:mailbox-count mbox) count)
		   (mp:mailbox-empty-p mbox)
		   t))
  t)

;;; Date: 14/04/2012
;;;	Ensure that the mailbox works in a nonblocking fashion (when the
;;;	number of messages < mailbox size in a single producer and single
;;;	consumer  setting. We do not need to create new threads for this.
(deftest mbox-mailbox-nonblocking-io-1-to-1
    (loop with count = 30
       with name = "mbox-mailbox-nonblocking-io-1-to-1"
       with mbox = (mp:make-mailbox :name name :count count)
       for l from 1 to 10
       for messages = (loop for i from 1 to l
			 do (mp:mailbox-send mbox i)
			 collect i)
       always 
	 (and (not (mp:mailbox-empty-p mbox))
	      (equalp (loop for i from 1 to l
			 collect (mp:mailbox-read mbox))
		      messages)
	      (mp:mailbox-empty-p mbox)
	      t))
  t)

;;; Date: 14/04/2012
;;;	The mailbox blocks a process when it saturates the write queue.
(def-mp-test mbox-blocks-1-to-1
    (let* ((flag nil)
	   (mbox (mp:make-mailbox :name "mbox-signal-one" :count 32))
	   (size (mp:mailbox-count mbox))
	   (a-process (mp:process-run-function
		       "mbox-signal-one-process"
		       #'(lambda ()
			   ;; This does not block
			   (loop for i from 1 to size
			      do (mp:mailbox-send mbox i))
			   ;; Here we block
			   (setf flag t)
			   (mp:mailbox-send mbox (1+ size))
			   ;; Now we unblock
			   (setf flag nil)))))
      (sleep 0.2) ; give time for all messages to arrive
      (and (not (mp:mailbox-empty-p mbox)) ; the queue has messages
	   (mp:process-active-p a-process) ; the process is active
	   flag ; and it is blocked
	   (loop for i from 1 to (1+ size) ; messages arrive in order
	      always (= i (mp:mailbox-read mbox)))
	   (null flag) ; and process unblocked
	   (mp:mailbox-empty-p mbox)
	   t))
  t)

;;; Date: 14/04/2012
;;;	N producers and 1 consumer
(def-mp-test mbox-n-to-1-communication
    (loop with length = 10000
       with mbox = (mp:make-mailbox :name "mbox-n-to-1-communication" :count 128)
       for n from 1 to 10
       for m = (round length n)
       for messages = (loop for i from 0 below (* n m) collect i)
       for producers = (loop for i from 0 below n
			  do (mp:process-run-function
			      "mbox-n-to-1-producer"
			      (let ((proc-no i))
				#'(lambda ()
				    (loop for i from 0 below m
				       for msg = (+ i (* proc-no m))
				       do (mp:mailbox-send mbox msg))))))
       always (and (equalp
		    (sort (loop for i from 1 to (* n m)
			     collect (mp:mailbox-read mbox))
			  #'<)
		    messages)
		   (mp:mailbox-empty-p mbox)))
  t)

;;; Date: 14/04/2012
;;;	1 producer and N consumer, but they do not block, because the
;;;	queue is large enough and pre-filled with messages
(def-mp-test mbox-1-to-n-non-blocking
    (loop with lock = (mp:make-lock :name "mbox-1-to-n-communication")
       for n from 1 to 10
       for m = (round 128 n)
       for length = (* n m)
       for mbox = (mp:make-mailbox :name "mbox-1-to-n-communication" :count length)
       for flags = (make-array length :initial-element nil)
       for aux = (loop for i from 0 below length
		    do (mp:mailbox-send mbox i))
       for producers = (loop for i from 0 below n
			  do (mp:process-run-function
			      "mbox-1-to-n-consumer"
			      #'(lambda ()
				  (loop for i from 0 below m
				     for msg = (mp:mailbox-read mbox)
				     do (setf (aref flags msg) t)))))
       do (sleep 0.1)
       always (and (every #'identity flags)
		   (mp:mailbox-empty-p mbox)))
  t)

;;; Date: 14/04/2012
;;;	1 producer and N consumers, which block, because the producer
;;;	is started _after_ them and is slower.
(def-mp-test mbox-1-to-n-blocking
    (loop for n from 1 to 10
       for m = (round 10000 n)
       for length = (* n m)
       for mbox = (mp:make-mailbox :name "mbox-1-to-n-communication" :count length)
       for flags = (make-array length :initial-element nil)
       for producers = (loop for i from 0 below n
			  do (mp:process-run-function
			      "mbox-1-to-n-consumer"
			      #'(lambda ()
				  (loop for i from 0 below m
				     for msg = (mp:mailbox-read mbox)
				     do (setf (aref flags msg) t)))))
       do (loop for i from 0 below length
	     do (mp:mailbox-send mbox i))
       do (sleep 0.1)
       always (and (every #'identity flags)
		   (mp:mailbox-empty-p mbox)))
  t)

