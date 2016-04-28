(in-package :cscrape)

(defparameter *begin-tag* "BEGIN_TAG_bfc54f90bafadf5")
(defparameter *end-tag* "END_TAG_bfc54f90bafadf5")


#|
(defun fill-config (config line)
  (let* ((trimmed (string-trim " " line))
         (var-start (position #\space trimmed))
         (data-start (position #\< trimmed :start var-start))
         (var (string-trim " " (subseq trimmed var-start data-start)))
         (data (string-trim " <>" (subseq trimmed data-start))))
    (setf (gethash var config) data)))

(defun read-application-config (filename)
  (let ((config (make-hash-table :test #'equal)))
    (with-open-file (fin filename :direction :input)
      (loop for l = (read-line fin nil 'eof)
         until (eq l 'eof)
         for tl = (string-trim '(#\space #\tab) l)
         do (cond
              ((string= (subseq tl 0 7) "#define")
               (fill-config config tl))
              ((string= (subseq tl 0 6) "#ifdef")
               (setq keyword (subseq tl (1+ (position-if (lambda (c) (or (char= #\space c) (char= #\tab))) tl)))))
              ((string= (subseq tl 0 8) "#include")
               (setq keyword (subseq tl (1+ (position-if (lambda (c) (or (char= #\space c) (char= #\tab))) tl)))))
               
              ((string= 
                ((string= (subseq l 0 6) "#endif")
                 #| Nothing |#)
                ((= (length (string-trim (list #\space #\tab))) 0)
                 #| Nothing |#)
                (error "Illegal application.config line: ~a" l))))
    config))

    |#

(defun setup-application-config ()
  (let ((config (make-hash-table :test #'eq)))
    (setf (gethash :init_functions_inc_h config) #P"include/generated/initFunctions_inc.h")
    (setf (gethash :init_classes_and_methods_inc_h config) #P"include/generated/initClassesAndMethods_inc.h")
    (setf (gethash :source_info_inc_h config) #P"include/generated/sourceInfo_inc.h")
    (setf (gethash :symbols_scraped_inc_h config) #P"include/generated/symbols_scraped_inc.h")
    (setf (gethash :enum_inc_h config) #P"include/generated/enum_inc.h")
    (setf (gethash :initializers_inc_h config) #P"include/generated/initializers_inc.h")
    (setf (gethash :lisp-wrappers config) #P"include/generated/lisp/cl-wrappers.lisp")
    (setf (gethash :c-wrappers config) #P"include/generated/c-wrappers.cc")
    config))
