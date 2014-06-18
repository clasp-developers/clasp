(load "./load-names.lisp")

(declaim (optimize (debug 0) (speed 3)))

(setf *print-circle* t)

(defun compute-pairs (data table)
  (clrhash table)
  (loop with max = 0
     with max-pair = nil
     for (code name . l) in data
     do (loop for l2 on l
	   for a = (car l2)
	   for b = (cadr l2)
	   while b
	   do (let* ((pair (cons a b))
		     (c (gethash pair table)))
		(setf (gethash pair table)
		      (setf c (if c (1+ c) 1))
		      a b)
		(when (> c max)
		  (setf max c max-pair pair))))
     finally (return (cons max max-pair))))

(defun replace-pair (pair code data)
  (let ((old-a (car pair))
	(old-b (cdr pair)))
    (loop with more = 0
       for (ucd-code name . l) in data
       do (loop with l2 = l
	     for a = (first l2)
	     for b = (second l2)
	     while b
	     do (when (and (eql a old-a) (eql b old-b))
		  ;; replace (a b . c) with (pair . c)
		  (setf (car l2) code
			(cdr l2) (cddr l2)))
	     do (setf l2 (cdr l2)))
       do (setf more (+ more (1- (length l))))
       finally (return more))))

(defun compress (data)
  (loop with last-length = 0
     with table = (make-hash-table :size 2048 :test #'equal)
     with pairs = '()
     for new-symbol from (1+ *last-word-index*)
     for (frequency . pair) = (compute-pairs data table)
     while (and pair (> frequency 1))
     do
       (format t "~%;;; ~A, ~D -> ~D, ~D left" pair frequency new-symbol
	       (replace-pair pair new-symbol data))
       (setf pairs (acons new-symbol pair pairs))
     finally
     ;; There are no redundant pairs. We just define ad-hoc new
     ;; symbols for all remaining strings.
       (loop with n = new-symbol
	  for (code name . l) in data
	  do (loop with l2 = l
		for a = (first l2)
		for b = (second l2)
		while b
		do (setf pairs (acons n (cons a b) pairs)
			 (car l2) n
			 (cdr l2) (cddr l2)
			 n (1+ n))))
       (print 'finished)
       (return-from compress (nreverse pairs))))

(progn
  (defparameter *compressed-data* (copy-tree *data*))
  (defparameter *paired-data* (compress *compressed-data*)))

(defparameter *last-code* (first (first (last *paired-data*))))

(defparameter *code-ndx-size* (ceiling (integer-length *last-code*) 8))

(defparameter *pair-table-size* (* (length *paired-data*)
				   (* 2 *code-ndx-size*)))

(defparameter *code-to-name-bytes*
  (* (length *compressed-data*)
     (+ 3 ; Size of Unicode code
	;; Size of index into the data table
	*code-ndx-size*)))

(defparameter *sorted-names-bytes*
  ;; The sorted list of character names is just a list of indices into
  ;; the *code-to-name-bytes* table
  (* (length *compressed-data*) *code-ndx-size*))

(defparameter *word-dictionary*
  (+ *words-array-bytes*))

(format t "
;;; Codes dictionary = ~D bytes
;;; Pair table size = ~D bytes
;;; Code to names table = ~D bytes
;;; Names to codes table = ~D bytes
;;; Total = ~D bytes
"
	*word-dictionary*
	*pair-table-size*
	*code-to-name-bytes*
	*sorted-names-bytes*
	(+
	*word-dictionary*
	*pair-table-size*
	*code-to-name-bytes*
	*sorted-names-bytes*
	))

;;; WITH HANGUL
;;; Codes dictionary = 78566 bytes
;;; Pair table size = 198752 bytes
;;; Code to names table = 164570 bytes
;;; Names to codes table = 65828 bytes
;;; Total = 507716 bytes

;;; WITHOUT HANGUL
;;; Codes dictionary = 78555 bytes
;;; Pair table size = 150868 bytes
;;; Code to names table = 108710 bytes
;;; Names to codes table = 43484 bytes
;;; Total = 381617 bytes

;;; Without HANGUL (split by space and -)
;;; Codes dictionary = 58258 bytes
;;; Pair table size = 160576 bytes
;;; Code to names table = 108710 bytes
;;; Names to codes table = 43484 bytes
;;; Total = 371028 bytes

;;; With HANGUL (split by space and -)
;;; Codes dictionary = 58269 bytes
;;; Pair table size = 208460 bytes
;;; Code to names table = 164570 bytes
;;; Names to codes table = 65828 bytes
;;; Total = 497127 bytes
