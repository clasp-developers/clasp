(in-package #:clos)

;;; There are different sets of initialization arguments. First we have
;;; those coming from the :INITARG option in the slots. Then we have
;;; all declared initargs which are keyword arguments to methods defined
;;; on SHARED-INITIALIZE, REINITIALIZE-INSTANCE, etc. (See ANSI 7.1.2)
;;; This file defines some common utilities for checking their validity.
;;; They're used in both make.lisp and change.lisp.

(defun check-initargs-uncached (class initargs
                                &optional calls (slots (class-slots class)))
  ;; We try to avoid calling compute-applicable-methods since that's work.
  ;; (In simple tests, avoiding it gave a speedup of 2-3 times.)
  ;; So we first check if all the initargs correspond to slots. If they do,
  ;; great. If not we compute-applicable-methods to get more valid keywords.
  ;; This assumes that the likely case is all the initargs corresponding to
  ;; slots, but it shouldn't really be any slower if they don't.
  ;; CALLS is a list of (function arglist). These can be passed directly
  ;; to compute-applicable-methods.
  (loop with aok = nil ; keep processing after aok to check for oddness/nonsymbols
        with aok-seen-p = nil
        with method-keys = nil
        with method-keys-p = nil
        with unknown-keys
        for cur on initargs by #'cddr
        for name = (first cur)
        when (and (eql name :allow-other-keys) (not aok-seen-p))
          do (setf aok (second cur) aok-seen-p t)
        do (cond
             ((null (rest cur))
              (core:simple-program-error "No value supplied for the init-name ~S."
                                         name))
             ((not (symbolp name))
              (core:simple-program-error "Not a valid initarg: ~s" name))
             ;; :allow-other-keys is always valid.
             ((eql name :allow-other-keys))
             ;; Check if the key is associated with a slot
             ((member name slots :test #'member :key #'slot-definition-initargs))
             ;; doesn't correspond to a slot, so check the methods.
             ;; Compute those keywords first if we haven't.
             ((progn
                (unless method-keys-p
                  (setf method-keys-p t
                        method-keys (valid-keywords-from-calls calls))
                  (when (eq method-keys t) ; &allow-other-keys
                    (return-from check-initargs-uncached)))
                (member name method-keys)))
             (t (push name unknown-keys)))
        finally (when (and unknown-keys (not aok))
                  (core:simple-program-error
                   "Unknown initialization options ~s for class ~a."
                   unknown-keys class))))

(defun valid-keywords-from-calls (calls)
  (loop for call in calls
        for methods = (apply #'compute-applicable-methods call)
        for keywords = (valid-keywords-from-methods methods)
        if (eq keywords t)
          return keywords
        else nconc keywords))

(defun valid-keywords-from-methods (methods)
  (loop for method in methods
        append (multiple-value-bind (keys aokp) (function-keywords method)
                 (when aokp (return t))
                 keys)))

;;; Like the above, but use a cached list of method initargs rather than
;;; grabbing them from methods.
(defun check-initargs (class initargs cached-keywords
                       &optional (slots (class-slots class)))
  (unless (eq cached-keywords t) ;; meaning we have &allow-other-keys
    (loop with aok = nil ; keep processing after aok to check for oddness/nonsymbols
          with aok-seen-p = nil
          with unknown-keys
          for cur on initargs by #'cddr
          for name = (first cur)
          when (and (eql name :allow-other-keys) (not aok-seen-p))
            do (setf aok (second cur) aok-seen-p t)
          do (cond
               ((null (rest cur))
                (core:simple-program-error "No value supplied for the init-name ~S."
                                           name))
               ((not (symbolp name))
                (core:simple-program-error "Not a valid initarg: ~s" name))
               ((eql name :allow-other-keys))
               ((member name slots :test #'member :key #'slot-definition-initargs))
               ((member name cached-keywords))
               (t (push name unknown-keys)))
          finally (when (and unknown-keys (not aok))
                    (core:simple-program-error
                     "Unknown initialization options ~s for class ~a."
                     unknown-keys class)))))

;;; There's not really a perfect place for this, but we use it below.
;;; This is not a simple reader because the prototype is computed lazily.
(defgeneric class-prototype (class))

(defmethod class-prototype ((class std-class))
  ;; FIXME? atomicity
  (if (slot-boundp class 'prototype)
      (slot-value class 'prototype)
      (setf (slot-value class 'prototype) (allocate-instance class))))

(defun precompute-valid-initarg-keywords (class)
  (setf (class-valid-initargs class)
        (loop with methods
                = (nconc
                   (compute-applicable-methods
                    #'allocate-instance (list class))
                   (compute-applicable-methods
                    #'initialize-instance (list (class-prototype class)))
                   (compute-applicable-methods
                    #'shared-initialize (list (class-prototype class) t)))
              for m in methods
              append (multiple-value-bind (keys aokp) (function-keywords m)
                       (when aokp (return t))
                       keys))))
