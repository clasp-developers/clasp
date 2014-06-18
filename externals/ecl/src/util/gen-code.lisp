;;;
;;; Writing src/h/cons.h and src/c/cons.d
;;;

(defun process-file (filename)
  (let* ((filename (merge-pathnames filename))
	 (name (pathname-name filename))
	 (input (make-pathname :name (concatenate 'string "bak-" name)
			       :type (pathname-type filename)
			       :directory (pathname-directory filename)))
	 (output filename))
    (cond ((not (probe-file filename))
	   (error "Unable to find ~a" filename))
	  ((probe-file input)
	   (error "Backup already exists" input))
	  (t
	   (format t "~%;;; Renaming ~a -> ~a" filename input)
	   (rename-file filename input)))
    (with-open-file (in input :direction :input)
      (with-open-file (out output :direction :output :if-exists :supersede)
	(format t "~%;;; Transforming ~a -> ~a" input output)
	(loop with skip = nil
	      for l = (read-line in nil nil nil)
	      while l
	      do (cond (skip
			(when (search "END-GENERATED" l)
			  (write-line l out)
			  (setf skip nil)))
		       (t
			(write-line l out)
			(let ((ndx (search "BEGIN-GENERATED" l)))
			  (when ndx
			    (let* ((*standard-output* out)
				   (form-text (subseq l (+ ndx 15)))
				   (form (read-from-string form-text)))
			      (eval form)
			      (setf skip t)))))))))
    (format t "~%;;; Deleting the file ~a" input)
    (delete-file input)))

(defun write-rec (depth list flag &optional (prefix ""))
  (when (plusp depth)
    (write-rec (1- depth) (cons 'a list) flag)
    (write-rec (1- depth) (cons 'd list) flag)
    (return-from write-rec))
  (let* ((string (apply #'concatenate 'string (mapcar #'string-downcase list))))
    (case flag
      (:inline
       (write-rec depth list :unsafe "static ECL_INLINE "))
      (:unsafe-macro
       (format t "~%#define C~AR(x) _ecl_c~ar(x)" (string-upcase string) string))
      (:unsafe
       (format t "~%~acl_object _ecl_c~ar(cl_object x)~%{" prefix string)
       (loop for what in (reverse list)
	     for op = (if (eq what 'a) "ECL_CONS_CAR" "ECL_CONS_CDR")
	     do (format t "~%  if (Null(x)) return x;~%  x = ~A(x);" op))
       (format t "~%  return x;~%}~%"))
      (:safe
       (format t "~%cl_object ecl_c~ar(cl_object x)~%{" string)
       (loop for what in (reverse list)
	     for op = (if (eq what 'a) "ECL_CONS_CAR" "ECL_CONS_CDR")
	     do (format t "~%  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);")
	     do (format t "~%  if (Null(x)) return x;~%  x = ~A(x);" op))
       (format t "~%  return x;~%}~%"))
      (:common-lisp
       (format t "~%cl_object cl_c~ar(cl_object x)~%{~%  return1(ecl_c~ar(x));~%}~%"
	       string string))
      (:declare-unsafe
       (format t "~%extern ECL_API cl_object _ecl_c~ar(cl_object);" string))
      (:declare-safe
       (format t "~%extern ECL_API cl_object ecl_c~ar(cl_object);" string))
      (:declare-common-lisp
       (format t "~%extern ECL_API cl_object cl_c~ar(cl_object);" string))
      (:common-lisp-inline
       (format t "~%(def-inline c~ar :always (t) t \"ecl_c~ar(#0)\")" string string)
       (format t "~%(def-inline c~ar :unsafe (t) t \"_ecl_c~ar(#0)\")" string string)
       )
      )))

(defun gen-cons-h ()
  (format t "~%#if ECL_CAN_INLINE")
  (loop for depth from 1 below 5
	do (write-rec depth nil :inline))
  (format t "~%#else")
  (loop for depth from 1 below 5
	do (write-rec depth nil :declare-unsafe))
  (format t "~%#endif /* !ECL_CAN_INLINE */~%")
  (loop for depth from 1 below 5
	do (write-rec depth nil :declare-safe))
  (terpri)
  (gen-cons-legacy-h)
  (loop for depth from 1 below 5
	do (write-rec depth nil :declare-common-lisp))
  (terpri))

(defun gen-cons-d ()
  (format t "~%#if !ECL_CAN_INLINE")
  (loop for depth from 1 below 5
	do (write-rec depth nil :unsafe))
  (format t "~%#endif /* !ECL_CAN_INLINE */~%")
  (loop for depth from 1 below 5
	do (write-rec depth nil :safe))
  (terpri)
  (loop for depth from 1 below 5
	do (write-rec depth nil :common-lisp))
  (terpri))

(defun gen-cons-legacy-h ()
  (loop for depth from 1 below 5
	do (write-rec depth nil :unsafe-macro))
  (terpri))

(defun gen-cons-sysfun ()
  (loop for depth from 1 below 5
	do (write-rec depth nil :common-lisp-inline))
  (terpri))

(process-file "src/c/cons.d")
(process-file "src/h/cons.h")
;(process-file "src/h/legacy.h")
(process-file "src/cmp/sysfun.lsp")
(terpri)
#+ecl
(ext:quit)
