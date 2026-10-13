(in-package #:clasp-tests)

(test-true process-1
      (progn (mp:process-run-function nil (lambda ())) t))
(test-type process-2 (mp:process-run-function nil (lambda ())) mp:process)
(test-type process-3 (mp:make-process nil (lambda ())) mp:process)

(test-true process-name
      (let ((g (gensym)))
        (eq g (mp:process-name (mp:make-process g (lambda ()))))))

(test process-join
      (mp:process-join
       (mp:process-run-function nil (lambda () (values 'a 2 'b))))
      (a 2 b))

(test process-specials
      (mp:process-join
       (mp:process-run-function nil (lambda () (declare (special x)) x)
                                `((x . t))))
      (t))

(test-type mutex-1 (mp:make-lock) mp:mutex)
(test-true mutex-2 (mp:get-lock (mp:make-lock)))
(test-true mutex-3 (mp:get-lock (mp:make-lock) nil))
(test mutex-4
      (let ((mut (mp:make-lock)))
        (mp:with-lock (mut)
          (mp:process-join
           (mp:process-run-function
            nil (lambda () (mp:get-lock mut nil))))))
      (nil))

;;; These are used in slime, so make sure we get them right.
(test-type recursive-mutex-anonymous
           (mp:make-recursive-mutex) mp:recursive-mutex)
(test-type recursive-mutex-symbol
           (mp:make-recursive-mutex 'bla) mp:recursive-mutex)
(test-type recursive-mutex-string
           (mp:make-recursive-mutex "bla") mp:recursive-mutex)

(test process-active-p-1
      (let ((p (mp:process-run-function nil (lambda ()))))
        (mp:process-join p)
        (mp:process-active-p p))
      (nil))
(test process-active-p-2
      (let ((mut (mp:make-lock)))
        (mp:with-lock (mut)
          (let ((p (mp:process-run-function
                    nil (lambda () (mp:get-lock mut)))))
            (mp:process-active-p p))))
      (t))
(test process-active-p-3
      (mp:process-active-p (mp:make-process nil (lambda ())))
      (nil))
(test process-active-p-4
      (let ((mut (mp:make-lock)))
        (mp:with-lock (mut)
          (let ((p (mp:make-process nil (lambda () (mp:get-lock mut)))))
            (mp:process-start p)
            (mp:process-active-p p))))
      (t))

(test-true all-processes-1
      (let ((p (mp:process-run-function nil (lambda ()))))
        (mp:process-join p)
        (not (member p (mp:all-processes)))))
(test-true all-processes-2
      (let ((mut (mp:make-lock)))
        (mp:with-lock (mut)
          (let ((p (mp:process-run-function
                    nil (lambda () (mp:get-lock mut)))))
            (member p (mp:all-processes))))))
(test-true all-processes-3
      (not (member (mp:make-process nil (lambda ())) (mp:all-processes))))
(test-true all-processes-4
      (let ((mut (mp:make-lock)))
        (mp:with-lock (mut)
          (let ((p (mp:make-process nil (lambda () (mp:get-lock mut)))))
            (mp:process-start p)
            (member p (mp:all-processes))))))

(test-true current-thread-1
      (member mp:*current-process* (mp:all-processes)))
(test-true current-thread-2
      (mp:process-active-p mp:*current-process*))

(test process-exit
      (mp:process-join
       (mp:process-run-function nil (lambda () (mp:exit-process 3 4))))
      (3 4))

;; Check process-join-error working at all
(test-expect-error process-abort-1
                   (mp:process-join
                    (mp:process-run-function nil #'mp:abort-process))
                   :type mp:process-join-error)

;; Check that if a condition is passed it's stored properly
(test-type process-abort-2
    (mp:process-join-error-original-condition
     (nth-value 1 (ignore-errors
                   (mp:process-join
                    (mp:process-run-function
                     nil (lambda ()
                           (mp:abort-process 'type-error
                                             :datum 4
                                             :expected-type 'cons)))))))
    type-error)

;; Check that the abort restart exists in new threads
(test-true process-abort-3
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

(test-true process-abort-5
      (let ((thread (mp:process-run-function nil #'mp:abort-process)))
        (eq thread (mp:process-error-process
                    (nth-value 1 (ignore-errors (mp:process-join thread)))))))

(test-expect-error not-atomic-1
                   (macroexpand-1 `(mp:atomic (,(gensym))))
                   :type mp:not-atomic)
(test-true not-atomic-2
      (let ((place (list (gensym))))
        (handler-case (macroexpand-1 `(mp:atomic ,place))
          (mp:not-atomic (e)
            (eq (mp:not-atomic-place e) place)))))

(macrolet ((atomic-place-test (name place create)
             `(test-true ,name
                    (let ((object ,create) (s (gensym)))
                      (setf (mp:atomic ,place) s)
                      (eq (mp:atomic ,place) s)))))
  (atomic-place-test atomic-car (car object) (list nil))
  (atomic-place-test atomic-cdr (cdr object) (list nil))
  (atomic-place-test atomic-first (first object) (list nil))
  (atomic-place-test atomic-rest (rest object) (list nil))
  (atomic-place-test atomic-symbol-value-1 (symbol-value object) (gensym))
  (atomic-place-test atomic-svref (svref object 0) (vector nil)))

(test-true atomic-symbol-value-2
      (let ((x nil) (s (gensym)))
        (declare (special x))
        (setf (mp:atomic x) s)
        (eq (mp:atomic x) s)))

(defun spam-processes (nthreads thunk)
  (let ((threads (loop repeat nthreads
                       collect (mp:process-run-function nil thunk))))
    (mapcar #'mp:process-join threads)))

(test atomic-acquire-release-1
      (let ((lock (list nil))
            (value 0)
            (nthreads 7))
        (labels ((acquire ()
                   (loop until (null (mp:cas (car lock) nil t
                                             :order :acquire-release))))
                 (release ()
                   (setf (mp:atomic (car lock) :order :release) nil))
                 (thunk ()
                   (acquire)
                   (unwind-protect (incf value) (release))))
          (spam-processes nthreads #'thunk)
          value))
      (7))

(test-true atomic-acquire-release-2
      (let* ((L (list 0 0 0 0 0))
             (consumer
               (mp:process-run-function
                nil (lambda ()
                      (loop while (zerop (mp:atomic (first L) :order :acquire)))
                      (apply #'= L))))
             (producer
               (mp:process-run-function
                nil (lambda ()
                      (setf (fifth L) 1 (fourth L) 1 (third L) 1 (second L) 1
                            (mp:atomic (first L) :order :release) 1)))))
        (mp:process-join producer)
        (mp:process-join consumer)))

(test atomic-incf
      (let ((x (list 0)))
        (mp:atomic-incf (car x) 319)
        (car x))
      (319))

(test-true atomic-sequential-consistency-1
      ;; from cppreference.com
      (let ((x (list nil)) (y (list nil)) (z (list 0)))
        (let ((write-x
                (mp:process-run-function
                 nil (lambda () (setf (mp:atomic (car x)) t))))
              (write-y
                (mp:process-run-function
                 nil (lambda () (setf (mp:atomic (car y)) t))))
              (read-x-then-y
                (mp:process-run-function
                 nil (lambda ()
                       (loop until (mp:atomic (car x)))
                       (when (mp:atomic (car y)) (mp:atomic-incf (car z))))))
              (read-y-then-x
                (mp:process-run-function
                 nil (lambda ()
                       (loop until (mp:atomic (car y)))
                       (when (mp:atomic (car x)) (mp:atomic-incf (car z)))))))
          (mp:process-join write-x) (mp:process-join write-y)
          (mp:process-join read-x-then-y) (mp:process-join read-y-then-x)
          (not (zerop (mp:atomic (car z)))))))

(test atomic-counter-effect
      (let ((counter (list 0))
            (nthreads 7))
        (spam-processes nthreads
                        (lambda ()
                          (mp:atomic-incf-explicit ((car counter)
                                                    :order :relaxed))))
        (car counter))
      (7))

(test atomic-counter-value
      (let ((counter (list 0))
            (nthreads 7))
        (sort (spam-processes
               nthreads
               (lambda ()
                 (mp:atomic-incf-explicit ((car counter) :order :relaxed))))
              #'<))
      ((1 2 3 4 5 6 7)))

(test atomic-push
      (let ((place (list nil))
            (nthreads 7))
        (spam-processes nthreads (lambda () (mp:atomic-push nil (car place))))
        (car place))
      ((nil nil nil nil nil nil nil)))

;;; MP:CAS on an SVREF place must arbitrate between threads, not merely compile.
;;; CORE:ACAS is reached through FDEFINITION in the "out-of-line" variants so that the
;;; runtime implementation is exercised even in a build where the compiler inlines a
;;; cmpxchg for the MP:CAS form instead.

(defun cas-svref-contention (casser nthreads per-thread)
  (let ((v (vector 0)))
    (spam-processes nthreads
                    (lambda ()
                      (dotimes (i per-thread)
                        (loop for old = (svref v 0)
                              until (eql old (funcall casser v old (1+ old)))))))
    (svref v 0)))

(defun cas-svref-inline (v old new) (mp:cas (svref v 0) old new))

(defun cas-svref-out-of-line (v old new)
  (funcall (fdefinition 'core::acas) :sequentially-consistent old new v 0))

(test cas-svref-single-thread
      (cas-svref-contention #'cas-svref-inline 1 10000)
      (10000))

(test cas-svref-contended
      (cas-svref-contention #'cas-svref-inline 4 10000)
      (40000))

(test cas-svref-out-of-line-contended
      (cas-svref-contention #'cas-svref-out-of-line 4 10000)
      (40000))

(test cas-svref-semantics
      (let ((v (vector 5)))
        (list (mp:cas (svref v 0) 5 6) (svref v 0)
              (mp:cas (svref v 0) 99 7) (svref v 0)))
      ((5 6 6 6)))

(test cas-svref-out-of-line-semantics
      (let ((v (vector 5)))
        (list (cas-svref-out-of-line v 5 6) (svref v 0)
              (cas-svref-out-of-line v 99 7) (svref v 0)))
      ((5 6 6 6)))

;;; The out-of-line CAS must still honour displacement.
(test cas-displaced-general-array
      (let* ((base (vector 0 0 0 0))
             (d (make-array 2 :displaced-to base :displaced-index-offset 1)))
        (list (funcall (fdefinition 'core::acas) :sequentially-consistent 0 'x d 1)
              (coerce base 'list)))
      ((0 (0 0 x 0))))

;;; A specialized array has no tagged word to swap, so it is refused rather than
;;; silently swapped non-atomically.
(test-expect-error cas-specialized-array
                   (funcall (fdefinition 'core::acas) :sequentially-consistent 0 1
                            (make-array 1 :element-type 'double-float
                                          :initial-element 0d0)
                            0)
                   :type type-error)
