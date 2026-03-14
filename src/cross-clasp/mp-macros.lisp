(in-package #:cross-clasp.clasp.mp)

(defmacro without-interrupts (&body body)
  (core::with-unique-names
      (outer-allow-with-interrupts outer-interrupts-enabled)
    `(multiple-value-prog1
         (macrolet ((allow-with-interrupts (&body allow-forms)
                      (list* 'let
                             (list (list 'core::*allow-with-interrupts*
                                         ',outer-allow-with-interrupts))
                             allow-forms))
                    (with-restored-interrupts (&body with-forms)
                      (list* 'let
                             (list (list 'core::*interrupts-enabled*
                                         ',outer-interrupts-enabled))
                             with-forms))
                    (with-local-interrupts (&body with-forms)
                      (list 'let*
                            (list (list 'core::*allow-with-interrupts*
                                        ',outer-allow-with-interrupts)
                                  (list 'core::*interrupts-enabled*
                                        ',outer-allow-with-interrupts))
                            (list 'when ',outer-allow-with-interrupts
                                  '(core::check-pending-interrupts))
                            (list* 'locally with-forms))))
           (let* ((,outer-interrupts-enabled core::*interrupts-enabled*)
                  (core::*interrupts-enabled* nil)
                  (,outer-allow-with-interrupts
                    core::*allow-with-interrupts*)
                  (core::*allow-with-interrupts* nil))
             (declare (ignorable ,outer-allow-with-interrupts
                                 ,outer-interrupts-enabled))
             ,@body))
       (when core::*interrupts-enabled*
         (core::check-pending-interrupts)))))

(defmacro with-interrupts (&body body)
  (core::with-unique-names (allowp enablep)
    ;; We could manage without ENABLEP here, but that would require
    ;; taking extra care not to ever have *ALLOW-WITH-INTERRUPTS* NIL
    ;; and *INTERRUPTS-ENABLED* T -- instead of risking future breakage
    ;; we take the tiny hit here.
    `(let* ((,allowp core::*allow-with-interrupts*)
            (,enablep core::*interrupts-enabled*)
            (core::*interrupts-enabled* (or ,enablep ,allowp)))
       (when (and ,allowp (not ,enablep))
         (core::check-pending-interrupts))
       (locally ,@body))))

(defmacro with-lock ((lock-form &rest options) &body body)
  (declare (ignore options)) ; none yet
  (core::with-unique-names (lock)
    `(let ((,lock ,lock-form))
       (unwind-protect
            (progn
              (get-lock ,lock)
              (locally ,@body))
         (giveup-lock ,lock)))))

(defmacro with-rwlock ((lock op) &body body)
    (assert (member op '(:read :write) :test #'eq))
    (let ((s-lock (gensym)))
      `(let ((,s-lock ,lock))
         (,(if (eq :read op) 'shared-lock 'write-lock) ,s-lock)
         (unwind-protect
             (progn ,@body)
           (,(if (eq :read op) 'shared-unlock 'write-unlock) ,s-lock)))))
