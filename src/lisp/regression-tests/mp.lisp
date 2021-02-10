(in-package #:clasp-tests)

(test process-1
      (progn (mp:process-run-function nil (lambda ())) t))
(test process-2
      (typep (mp:process-run-function nil (lambda ())) 'mp:process))
(test process-3
      (typep (mp:make-process nil (lambda ())) 'mp:process))

(test process-name
      (let ((g (gensym)))
        (eq g (mp:process-name (mp:make-process g (lambda ()))))))

(test process-join
      (equal
       (multiple-value-list
        (mp:process-join
         (mp:process-run-function nil (lambda () (values 'a 2 'b)))))
       '(a 2 b)))

(test process-specials
      (mp:process-join
       (mp:process-run-function nil (lambda () (declare (special x)) x)
                                `((x . t)))))

(test mutex-1 (typep (mp:make-lock) 'mp:mutex))
(test mutex-2 (mp:get-lock (mp:make-lock)))
(test mutex-3 (mp:get-lock (mp:make-lock) nil))
(test mutex-4
      (let ((mut (mp:make-lock)))
        (mp:with-lock (mut)
          (not (mp:process-join
                (mp:process-run-function
                 nil (lambda () (mp:get-lock mut nil))))))))

(test process-active-p-1
      (let ((p (mp:process-run-function nil (lambda ()))))
        (mp:process-join p)
        (not (mp:process-active-p p))))
(test process-active-p-2
      (let ((mut (mp:make-lock)))
        (mp:with-lock (mut)
          (let ((p (mp:process-run-function
                    nil (lambda () (mp:get-lock mut)))))
            (mp:process-active-p p)))))
(test process-active-p-3
      (not (mp:process-active-p (mp:make-process nil (lambda ())))))
(test process-active-p-4
      (let ((mut (mp:make-lock)))
        (mp:with-lock (mut)
          (let ((p (mp:make-process nil (lambda () (mp:get-lock mut)))))
            (mp:process-start p)
            (mp:process-active-p p)))))

(test all-processes-1
      (let ((p (mp:process-run-function nil (lambda ()))))
        (mp:process-join p)
        (not (member p (mp:all-processes)))))
(test all-processes-2
      (let ((mut (mp:make-lock)))
        (mp:with-lock (mut)
          (let ((p (mp:process-run-function
                    nil (lambda () (mp:get-lock mut)))))
            (member p (mp:all-processes))))))
(test all-processes-3
      (not (member (mp:make-process nil (lambda ())) (mp:all-processes))))
(test all-processes-4
      (let ((mut (mp:make-lock)))
        (mp:with-lock (mut)
          (let ((p (mp:make-process nil (lambda () (mp:get-lock mut)))))
            (mp:process-start p)
            (member p (mp:all-processes))))))

(test current-thread-1
      (member mp:*current-process* (mp:all-processes)))
(test current-thread-2
      (mp:process-active-p mp:*current-process*))

;; Check process-join-error working at all
(test-expect-error process-abort-1
                   (mp:process-join
                    (mp:process-run-function nil #'mp:abort-process))
                   :type mp:process-join-error)

;; Check that if a condition is passed it's stored properly
(test process-abort-2
      (typep
       (mp:process-join-error-original-condition
        (nth-value 1
                   (ignore-errors
                    (mp:process-join
                     (mp:process-run-function
                      nil (lambda ()
                            (mp:abort-process 'type-error
                                              :datum 4
                                              :expected-type 'cons)))))))
       'type-error))

;; Check that the abort restart exists in new threads
(test process-abort-3
      (find 'abort
            (mp:process-join
             (mp:process-run-function
              nil (lambda ()
                    (mapcar #'restart-name (compute-restarts)))))))

;; Check that the condition can be passed to a restart
#+(or) ; doesn't work (yet?)
(test process-abort-4
      (typep
       (mp:process-join-error-original-condition
        (nth-value 1
                   (ignore-errors
                    (mp:process-join
                     (mp:process-run-function
                      nil (lambda ()
                            (handler-bind ((error #'abort)) (=))))))))
       'program-error))

(test process-abort-5
      (let ((thread (mp:process-run-function nil #'mp:abort-process)))
        (eq thread (mp:process-error-process
                    (nth-value 1 (ignore-errors (mp:process-join thread)))))))

(test atomic-counter-effect
      (let* ((counter (list 0))
             (nthreads 7)
             (threads
               (loop repeat nthreads
                     collect (mp:process-run-function
                              nil
                              (lambda () (mp:atomic-incf (car counter)))))))
        (mapc #'mp:process-join threads)
        (eql (car counter) nthreads)))

(test atomic-counter-value
      (let* ((counter (list 0))
             (nthreads 7)
             (threads
               (loop repeat nthreads
                     collect (mp:process-run-function
                              nil
                              (lambda () (mp:atomic-incf (car counter)))))))
        (equal (sort (mapcar #'mp:process-join threads) #'<)
               '(1 2 3 4 5 6 7))))

