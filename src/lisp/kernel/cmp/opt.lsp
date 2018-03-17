;;; Stuff used by all the opt- files

(in-package #:ext)

;;; Must be synced with constantp in primitives.cc
(defun constant-form-value (form &optional env)
  "If (constantp form env) is true, returns the constant value of the form in the environment."
  (declare (ignore env))
  (cond ((symbolp form) (symbol-value form))
        ;; assume quote
        ((consp form) (second form))
        ;; self-evaluating
        (t form)))

(in-package #:cmp)

;; If a form refers to a function we can use as the head of a form, return something suitable
;; as head of form. Else NIL.
(defun constant-function-expression (form env)
  (declare (ignore env))
  (if (consp form)
      (cond ((eq (car form) 'function)
             (if (and (consp (cdr form)) (null (cddr form)))
                 (second form)
                 ;; invalid function form; could warn, but compiler should get it
                 nil))
            ;; FIXME: commented out because there could be a local binding; need to be smarter w/environment
            #+(or)
            ((eq (car form) 'quote)
             (if (and (consp (cdr form)) (null (cddr form)))
                 (second form)
                 nil))
            ((eq (car form) 'lambda) form)
            (t nil))
      nil))

;; Return a function of two forms that returns a condition form for testing them, as well as required bindings.
;; That is, testing by a two-arg test (CLHS 17.2.1)
;; E.g. (opt-test-function :test #'foo nil) => #'(lambda (v1 v2) `(foo ,v1 ,v2)), NIL
;; (opt-test-function :test (generate-test) nil) =>
;;   #'(lambda (v1 v2) `([gensym] ,v1 ,v2)), (([gensym] (generate-test)))
(defun opt-test-function (test-flag test env)
  (cond ((null test-flag)
         (values (opt-test-function :test '#'eql env) nil))
        ((eq test-flag :test-not)
         (multiple-value-bind (function init)
             (opt-test-function :test test env)
           (values #'(lambda (v1 v2) `(not ,(funcall function v1 v2)))
                   init)))
        (t (let ((maybe-head (constant-function-expression test env)))
             (if maybe-head
                 (values #'(lambda (v1 v2) `(,maybe-head ,v1 ,v2)) nil)
                 (si::with-unique-names (test-function)
                   (values #'(lambda (v1 v2) `(funcall ,test-function ,v1 ,v2))
                           (list (list test-function test)))))))))

;; Like the above, but with a key function.
(defun opt-key-function (key env)
  (cond ((null key)
         (values #'identity nil))
        (t (let ((maybe-head (constant-function-expression key env)))
             (if maybe-head
                 (values #'(lambda (elt) `(,maybe-head ,elt)) nil)
                 (si::with-unique-names (key-function)
                   (values #'(lambda (elt) `(funcall ,key-function ,elt))
                           (list (list key-function
                                       `(or ,key #'identity))))))))))

;;; Use the above to do an entire two-arg test, and allow for start and end as well.
;;; Returns (values keyf testf inits keyk testk test start end)
;;; keyf and testf are the functions returned by seq-opt-etc above. inits are from them as well.
;;; keyk and testk are the keywords for them, i.e. keyk is :key if :key was specified or else NIL,
;;;  and testk is :test or :test-not or nil.
;;; test is the form passed as a :test or :test-not, or #'eql if none was passed.
;;; test is used for dropping to memq and friends even if a full inline isn't done. (FIXME: will be used for.)
;;; start and end are the same parameters. the start-end argument controls whether they're valid.
;;; If the call is invalid - e.g. has both :test and :test-not - all values will be nil, including the
;;;  primary value keyf, which is otherwise a function.
(defun two-arg-test-parse-args (function args &key (start-end t) environment)
  (loop with key-flag = nil
        with key = nil
        with init = nil
        with test = '#'eql
        with test-flag = nil
        with start = 0
        with end = nil
        with keyword
        while args
        do (cond ((or (atom args)
                      (null (rest args))
                      (eq keyword :allow-other-keys)
                      (not (keywordp (setf keyword (pop args)))))
                  (return nil))
                 ((eq keyword :key)
                  (unless key-flag
                    (setf key (pop args)
                          key-flag keyword)))
                 ((or (eq keyword :test)
                      (eq keyword :test-not))
                  (cond ((null test-flag)
                         (setf test (pop args)
                               test-flag keyword))
                        ((not (eq test-flag keyword))
                         (warn "Cannot specify :TEST and :TEST-NOT arguments to ~A"
                               function)
                         (return nil))))
                 ((eq keyword :start)
                  (unless start-end
                    (warn "Unexpected keyword argument ~A in a call to function ~A"
                          keyword function)
                    (return nil))
                  (setf start (pop args)))
                 ((eq keyword :end)
                  (unless start-end
                    (warn "Unexpected keyword argument ~A in a call to function ~A"
                          keyword function)
                    (return nil))
                  (setf end (pop args)))
                 ((eq keyword :from-end)
                  (unless (null (pop args))
                    (return nil)))
                 (t (return nil)))
        finally
           (multiple-value-bind (key-function key-init)
               (opt-key-function key environment)
             (multiple-value-bind (test-function test-init)
                 (opt-test-function test-flag test environment)
               (return (values key-function
                               test-function
                               (nconc key-init test-init)
                               key-flag
                               test-flag
                               test
                               start
                               end))))))
