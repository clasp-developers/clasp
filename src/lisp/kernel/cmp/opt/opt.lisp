;;; Stuff used by all the opt- files
(in-package #:cmp)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun gensym-list (list &optional x)
  (loop
    :for _ :in list
    :collect (if x (gensym x) (gensym))))

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
            ;; FIXME: This covers common cases like (whatever :key 'eql),
            ;; but the good way to do it would be to query the environment to see
            ;; if the name has been shadowed. (CL names cannot be shadowed, ofc.)
            ((eq (car form) 'quote)
             (if (and (consp (cdr form)) (null (cddr form)))
                 (let ((name (second form)))
                   (if (and (symbolp name)
                            (eq (symbol-package name) (find-package "CL")))
                       name
                       nil))
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
  (ecase test-flag
    ((nil)
     (values (opt-test-function :test '#'eql env) nil))
    (:test-not
     (multiple-value-bind (function init)
         (opt-test-function :test test env)
       (values #'(lambda (v1 v2)
                   `(not ,(funcall function v1 v2)))
               init)))
    (:test
     (let ((maybe-head (constant-function-expression test env)))
       (if maybe-head
           (values #'(lambda (v1 v2)
                       `(,maybe-head ,v1 ,v2))
                   nil)
           (si::with-unique-names (test-function)
             (values #'(lambda (v1 v2)
                         `(funcall ,test-function ,v1 ,v2))
                     `((,test-function ,test)))))))))

;; Like the above, but with a key function.
(defun opt-key-function (key-flag key env)
  (if (null key-flag) ; no :key provided
      (values #'identity nil)
      (let ((maybe-head (constant-function-expression key env)))
        (if maybe-head
            (values #'(lambda (elt) `(,maybe-head ,elt))
                    nil)
            (si::with-unique-names (key-function)
              (values #'(lambda (elt) `(funcall ,key-function ,elt))
                      ;; explicit :key nil is allowed.
                      `((,key-function (or ,key #'identity)))))))))

;;; Use the above to do an entire two-arg test, and allow for START and END as well.
;;; Returns (VALUES KEYF TESTF INITS IGNORES KEYK TESTK START END)
;;; INITS are a list of variables to bind. IGNORES are symbols to ignore.
;;; KEYF and TESTF are the functions returned by OPT-x-FUNCTION above.
;;; KEYK and TESTK are the keywords for them, i.e. KEYK is :key if :key was specified or else NIL,
;;;  and TESTK is :test or :test-not or nil.
;;; START and END are the same parameters. the START-END argument controls whether they're valid.
;;; If the call is invalid - e.g. has both :test and :test-not - all values will be nil, including the
;;;  primary value KEYF, which is otherwise a function.
(defun two-arg-test-parse-args (function args &key (start-end t) environment)
  (loop with key-flag = nil
        with key = #'identity
        with init = nil
        with ignores = nil
        with test = (lambda (x y) `(eql ,x ,y))
        with test-flag = nil
        with start = 0
        with start-flag
        with end = nil
        with end-flag
        with keyword
        while args
        do (cond ((or (atom args)
                      (null (rest args))
                      (eq keyword :allow-other-keys)
                      (not (keywordp (setf keyword (pop args)))))
                  (return nil))
                 ((eq keyword :key)
                  (if key-flag
                      (let ((s (gensym "IGNORE-KEY")))
                        (push `(,s ,(pop args)) init)
                        (push s ignores))
                      (multiple-value-bind (key-function key-init)
                          (opt-key-function keyword (pop args) environment)
                        (setf key key-function
                              init (append key-init init)
                              key-flag keyword))))
                 ((or (eq keyword :test)
                      (eq keyword :test-not))
                  (cond ((null test-flag)
                         (multiple-value-bind (test-function test-init)
                             (opt-test-function keyword (pop args) environment)
                           (setf test test-function
                                 init (append test-init init)
                                 test-flag keyword)))
                        ((not (eq test-flag keyword))
                         (warn "Cannot specify :TEST and :TEST-NOT arguments to ~A"
                               function)
                         (return nil))
                        (t
                         (let ((s (gensym "IGNORE-TEST")))
                           (push `(,s ,(pop args)) init)
                           (push s ignores)))))
                 ((eq keyword :start)
                  (cond
                    ((not start-end)
                     (warn "Unexpected keyword argument ~A in a call to function ~A"
                           keyword function)
                     (return nil))
                    (start-flag
                     (let ((s (gensym "IGNORE-START")))
                       (push `(,s ,(pop args)) init)
                       (push s ignores)))
                    (t
                     (let ((s (gensym "START")))
                       (push `(,s ,(pop args)) init)
                       (setf start-flag t)))))
                 ((eq keyword :end)
                  (cond
                    ((not start-end)
                     (warn "Unexpected keyword argument ~A in a call to function ~A"
                           keyword function)
                     (return nil))
                    (end-flag
                     (let ((s (gensym "IGNORE-END")))
                       (push `(,s ,(pop args)) init)
                       (push s ignores)))
                    (t
                     (let ((s (gensym "END")))
                       (push `(,s ,(pop args)) init)
                       (setf end-flag t)))))
                 ((eq keyword :from-end)
                  (unless (null (pop args))
                    (return nil)))
                 (t (return nil)))
        finally
           (return (values key
                           test
                           (nreverse init)
                           ignores
                           key-flag
                           test-flag
                           start
                           end))))
) ; eval-when
