;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;; ----------------------------------------------------------------------
;;; Macros only used in the code of the compiler itself:

(in-package "C-DATA")

;; ----------------------------------------------------------------------
;; CACHED FUNCTIONS
;;
(defmacro defun-cached (name lambda-list test &body body)
  (let* ((cache-name (intern (concatenate 'string "*" (string name) "-CACHE*")
                             (symbol-package name)))
         (reset-name (intern (concatenate 'string (string name) "-EMPTY-CACHE")
                             (symbol-package name)))
         (hash-function (case test
                          (EQ 'SI::HASH-EQ)
                          (EQL 'SI::HASH-EQL)
                          (EQUAL 'SI::HASH-EQUAL)
                          (t (setf test 'EQUALP) 'SI::HASH-EQUALP)))
         (hash (gensym "HASH")))
    `(progn
       (defparameter ,cache-name (make-array 1024 :element-type t :adjustable nil))
       (defun ,reset-name ()
         (make-array 1024 :element-type t :adjustable nil))
       (defun ,name ,lambda-list
         (flet ((,name ,lambda-list ,@body))
           (let* ((hash (logand (,hash-function ,@lambda-list) 1023))
                  (elt (aref ,cache-name hash)))
             (declare (type (integer 0 1023) hash)
                      (type (array t (*)) ,cache-name))
             (if (and elt ,@(loop for arg in lambda-list
                               collect `(,test (pop (the cons elt)) ,arg)))
                 (first (the cons elt))
                 (let ((output (,name ,@lambda-list)))
                   (setf (aref ,cache-name hash) (list ,@lambda-list output))
                   output))))))))

(defmacro defun-equal-cached (name lambda-list &body body)
  `(defun-cached ,name ,lambda-list equal ,@body))

;;; ----------------------------------------------------------------------
;;; CONVENIENCE FUNCTIONS / MACROS
;;;

(defun-cached env-var-name (n) eql
  (format nil "env~D" n))

(defun-cached lex-env-var-name (n) eql
  (format nil "lex~D" n))

(defun same-fname-p (name1 name2) (equal name1 name2))

(defun next-label () (incf *last-label*))

(defun next-lcl () (list 'LCL (incf *lcl*)))
  
(defun lisp-to-c-name (obj)
  "Translate Lisp object prin1 representation to valid C identifier name"
  (and obj 
       (map 'string 
            #'(lambda (c)
                (let ((cc (char-code c)))
                  (if (or (<= #.(char-code #\a) cc #.(char-code #\z))
                          (<= #.(char-code #\0) cc #.(char-code #\9)))
                      c #\_)))
            (string-downcase (prin1-to-string obj)))))

(defun next-cfun (&optional (prefix "L~D~A") (lisp-name nil))
  (let ((code (incf *next-cfun*)))
    (format nil prefix code (lisp-to-c-name lisp-name))))

(defun next-lex ()
  (prog1 (cons *level* *lex*)
         (incf *lex*)
         (setq *max-lex* (max *lex* *max-lex*))))

(defun next-env ()
  (prog1 *env*
    (incf *env*)
    (setq *max-env* (max *env* *max-env*))))

(let ((p (find-package "C-DATA")))
  (do-symbols (s "C-DATA")
    (when (eq (symbol-package s) p)
      (export s p))))
