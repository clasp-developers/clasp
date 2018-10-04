#!/bin/sh
#--eval '(set-dispatch-macro-character #\# #\! (lambda (s c n)(declare (ignore c n)) (read-line s) (values)))' \
#|
SCRIPT_DIR="$(dirname "$0")"
exec sbcl --dynamic-space-size 4096 --noinform --disable-ldb --lose-on-corruption --disable-debugger \
--no-sysinit --no-userinit --noprint \
--eval '(set-dispatch-macro-character #\# #\! (lambda (s c n)(declare (ignore c n)) (read-line s) (values)))' \
--eval "(defvar *script-args* '( $# \"$0\" \"$1\" \"$2\" \"$3\" \"$4\" \"$5\" \"$6\" \"$7\" ))" \
--eval "(require :asdf)" \
--load "$0"
|#


(defun read-dtrace-header (stream &optional eofp eof)
  (list (read-line stream eofp eof)
        (read-line stream eofp eof)
;;        (read-line stream eofp eof)
        (read-line stream eofp eof)
        (read-line stream eofp eof)))

(defun write-dtrace-header (stream lines)
  (dolist (l lines)
    (write-line l stream)))

(defun remove-offset (line)
  (let ((offset (search "+0x" line)))
    (if offset
        (subseq line 0 offset)
        line)))

(defun read-dtrace-backtrace (stream &optional eofp eof)
  (loop for line = (read-line stream nil :eof)
     when (eq line :eof)
     do (if eofp
            (error "End of file encountered")
            (return-from read-dtrace-backtrace eof))
     until (= (length line) 0)
     collect (remove-offset line)))

(defun write-block (stream backtrace)
  (loop for x in backtrace
     do (princ x stream)
     do (terpri stream))
  (terpri stream))

(defun cleanup-frame (line)
  (let (pos)
    (cond
      ((setf pos (search "cclasp-boehm-image.fasl`" line))
       (concatenate 'string (subseq line 0 pos) (subseq line (+ pos #.(length "cclasp-boehm-image.fasl`")) (length line))))
      ((setf pos (search "iclasp-boehm`" line))
       (concatenate 'string (subseq line 0 pos) (subseq line (+ pos #.(length "iclasp-boehm`")) (length line))))
      (t line))))

(defun cleanup-backtrace (backtrace)
  (declare (optimize speed))
  (list*
   (cleanup-frame (car backtrace))
   (loop for line in (cdr backtrace)
      unless (search "VariadicFunctor" line)
      unless (search "core__call_with_variable_bound" line)
      unless (search "core::apply_method" line)
      unless (search "core::funcall_va_list" line)
      unless (search "core::funcall_consume_valist" line)
      unless (search "core::cl__apply" line)
      unless (search "FuncallableInstance_O::entry_point" line)
      unless (search "standard_dispatch" line)
      unless (search "funcall_frame" line)
      unless (search "cc_call_multipleValueOneFormCall" line)
      unless (search "core::core__funwind_protect" line)
      unless (search "core::core__multiple_value_prog1_function" line)
      unless (search "LAMBDA^^COMMON-LISP_FN" line)
      unless (search "COMBINE-METHOD-FUNCTIONS3.LAMBDA" line)
      collect (cleanup-frame line))))

#+(or)(defun cleanup-dtrace-log (fin fout &key (verbose t))
  (let ((count 0))
    (let ((header (read-dtrace-header fin)))
      (write-block fout header)
      (loop for backtrace = (read-dtrace-backtrace fin nil :eof)
            until (eq backtrace :eof)
            for cleaned = (cleanup-backtrace backtrace)
            when (> (length backtrace) 4000)
#|              do (progn
                   (format *debug-io* "------------ input file pos: ~a~%" (file-position fin))
                   (format *debug-io* "Backtrace with ~a frames~%" (length backtrace))
                   (format *debug-io* "~a~%" cleaned #++(last backtrace 5)))
                    |#
            do (write-block fout cleaned)
            do (incf count)))
    (when verbose (format *debug-io* "Cleaned ~a stacks~%" count))))


(defun remove-address (line)
  (let ((pos-plus (search "+0x" line :from-end t)))
    (if pos-plus
        (subseq line 0 pos-plus)
        line)))
  
(defun pick-tip (backtrace)
  (let ((line (first backtrace)))
    (cond
      ((search "VariadicFunctor" line) (remove-address (second backtrace)))
      ((search "VariadicMethoid" line) (remove-address (second backtrace)))
      ((search "SingleDispatchMethodFunction" line) (remove-address (second backtrace)))
      (t (remove-address line)))))

(defun trace-tips (fin)
  (let ((header (read-dtrace-header fin)))
    (declare (ignore header))
      (let ((tips (loop for backtrace = (read-dtrace-backtrace fin nil :eof)
                        until (eq backtrace :eof)
                        when (> (length backtrace) 0)
                          collect (pick-tip backtrace))))
        tips)))

(defun count-tips (fin fout)
  (let ((tips (trace-tips fin))
        (counts (make-hash-table :test #'equal)))
    (loop for tip in tips
          do (incf (gethash tip counts 0)))
    (let (counted-tips)
      (maphash (lambda (k v)
                 (push (cons v k) counted-tips))
               counts)
      (let ((sorted (sort counted-tips #'< :key #'car))
            (total 0)
            (gc-counts 0)
            (hash-table-counts 0))
        (loop for (count . name) in sorted
              do (incf total count)
              do (format fout "~5d  ~5d  - ~a~%" count total name)
              do (cond
                   ((search "GC_" name)
                    (incf gc-counts count))
                   ((search "HashTable_O" name)
                    (incf hash-table-counts count))))
        (format fout "There are ~a backtraces~%" (length tips))
        (format fout "GC_ related counts: ~a~%" gc-counts)
        (format fout "HashTable_O related counts: ~a~%" hash-table-counts)))))


(defun trace-calls (fin)
  (let ((header (read-dtrace-header fin))
        (num-backtraces 0))
    (declare (ignore header))
    (let ((calls (loop for backtrace = (read-dtrace-backtrace fin nil :eof)
                  for backtrace-times = (if (consp backtrace) (parse-integer (string-trim " " (car (last backtrace)))) nil)
                  do (when (eq backtrace :eof) (loop-finish))
                    when (> (length backtrace) 0)
                    append (loop for times below backtrace-times
                            append (butlast backtrace 1))
                    and do (incf num-backtraces backtrace-times))))
      (values calls num-backtraces))))


(defun count-calls (fin fout)
  (multiple-value-bind (calls num-backtraces)
      (trace-calls fin)
    (let ((counts (make-hash-table :test #'equal)))
      (loop for call in calls
            do (incf (gethash call counts 0)))
      (let (counted-calls)
        (maphash (lambda (k v)
                   (push (cons v k) counted-calls))
                 counts)
        (let ((sorted (sort counted-calls #'< :key #'car))
              (total 0)
              (gc-counts 0)
              (hash-table-counts 0))
          (format fout "Functions with less than 0.01 fractional time are not displayed~%")
          (loop for (count . name ) in sorted
                do (let ((frac (/ count num-backtraces)))
                     (when (> frac 0.01)
                       (format fout "~5d ~5,3f  - ~a~%" count (float (/ count num-backtraces)) name)))
                do (incf total count)
                do (cond
                     ((search "GC_" name)
                      (incf gc-counts count))
                     ((search "HashTable_O" name)
                      (incf hash-table-counts count))))
          (format fout "GC_ related counts: ~a~%" gc-counts)
          (format fout "HashTable_O related counts: ~a~%" hash-table-counts))))))


;;; Prune backtraces down to a specific function in the backtraces.
;;; Look down each backtrace from until you find a frame with the function name that contains the focus-name.
;;; Write out the top of the backtrace to that point.
;;; If the backtrace doesn't contain that function - then write out an empty frame.
;;; This will let us build flame graphs by focusing in on function calls within a complex operation like compilation.
(defun prune (fin fout prune-name)
  (format *debug-io* "Looking for ~s~%" prune-name)
  (let ((header (read-dtrace-header fin)))
    (write-dtrace-header fout header))
  (loop for backtrace = (read-dtrace-backtrace fin nil :eof)
        until (eq backtrace :eof)
        do (let ((prune-name-index nil))
             (loop for line in (butlast backtrace)
                   for name = (string-trim " " line)
                   for index from 0
                   when (search prune-name name)
                     do (progn
                          (setf prune-name-index (1+ index))
                          (return)))
             (when prune-name-index
               (dotimes (index prune-name-index)
                 (write-line (elt backtrace index) fout)))
             (write-line (car (last backtrace)) fout)
             (terpri fout))))


(defun cleanup-stacks (fin fout)
  "Remove useless info from the backtraces like CALL-WITH-VARIABLE-BOUND calls"
  (format *debug-io* "Running cleanup-stacks~%")
  (finish-output *debug-io*)
  (let ((header (read-dtrace-header fin)))
    (write-dtrace-header fout header))
  (loop for backtrace = (read-dtrace-backtrace fin nil :eof)
        until (eq backtrace :eof)
        when backtrace
          do (let ((repeat-line (car (last backtrace)))
                   (reversed-backtrace (reverse (butlast backtrace)))
                   (new-backtrace nil))
               (unless repeat-line
                 (error "The repeat-line is NIL -  backtrace is ~%~s" backtrace))
               (let ((cleaned-backtrace
                       (progn
                         (loop for cur = reversed-backtrace then (cdr cur)
                               for line = (string-trim " " (car cur))
                               for start = (or (position #\` line) 0)
                               for end = (or (search "+0x" line) (length line))
                               for name = (subseq line (1+ start) end)
                               while cur
                               do (cond
                                    ((search "CALL-WITH-VARIABLE-BOUND" name)
                                     (setf cur (cdddr cur)))
                                    ((search "core__call_with_variable_bound" name)
                                     (setf cur (cddr cur)))
                                    ((search "cl__apply" name)
                                     (setf cur (cdr cur)))
                                    ((search "LAMBDA^COMMON-LISP" name)
                                     (setf cur (cdr cur)))
                                    (t (push (concatenate 'base-string "              " name) new-backtrace))))
                         new-backtrace)))
                 (loop for line in cleaned-backtrace
                       if line
                         do (write-line line fout)
                       else
                         do (error "About to write-line NIL - the backtrace is: ~%~s" backtrace ))
                 (write-line repeat-line fout)
                 (terpri fout)))))

(defun fraction (fin stop-at)
  (let ((header (read-dtrace-header fin)))
    (declare (ignore header))
    (let ((counts (make-hash-table :test #'equal))
          (num-backtraces 0))
      (loop for backtrace = (read-dtrace-backtrace fin nil :eof)
            until (eq backtrace :eof)
            when (> (length backtrace) 0)
              do (incf num-backtraces)
            do (let ((one-bt-counts (make-hash-table :test #'equal))
                     (saw-stop-at nil))
                 (loop for line in backtrace
                       for name = (string-trim " " line)
                       until (when (and stop-at (search stop-at name))
                               (setf saw-stop-at t))
                       finally (setf (gethash name one-bt-counts) 1)
                       do (setf (gethash name one-bt-counts) 1))
                 (unless (and stop-at (null saw-stop-at))
                   (maphash (lambda (name present)
                              (declare (ignore present))
                              (incf (gethash name counts 0)))
                            one-bt-counts))))
      (let ((results nil))
        (maphash (lambda (name count)
                   (push (cons count name) results))
                 counts)
        (let ((sorted (sort results #'< :key #'car)))
          (values sorted num-backtraces))))))

(defun print-fraction (fin fout &key stop-at)
  (multiple-value-bind (sorted num-backtraces)
      (fraction fin stop-at)
    (dolist (result sorted)
      (format fout "~5d ~5,3f ~a~%" (car result) (float (/ (car result) num-backtraces)) (cdr result)))))


(defun callers (in-stream out-stream callee-name)
  (declare (optimize (debug 3)))
  (read-dtrace-header in-stream)
  (format *debug-io* "In callers~%")
  (let ((callers (make-hash-table :test #'equal)))
    (loop for backtrace = (read-dtrace-backtrace in-stream nil :eof)
          until (eq backtrace :eof)
          do (loop for line in backtrace
                   for prev-name = nil then name
                   for name = (string-trim " " line)
                   for found = (search callee-name prev-name)
                   when (and prev-name found)
                     do (progn
                          (incf (gethash name callers 0)))))
    (let (counts
          (total-counts 0))
      (maphash (lambda (k v)
                 (incf total-counts v)
                 (push (cons v k) counts))
               callers)
      (let ((sorted (sort counts #'<= :key #'car)))
        (format out-stream "~a total calls to ~a~%" total-counts callee-name)
        (loop for (count . name) in sorted
              do (format out-stream "~5d ~20a~%" count name))))))


;;; ----------------------------------------------------------------------
;;;
;;;  Invoke functions using either ./stacks.lisp <operation> <arguments>
;;;
;;;  General arguments:
;;;    -i <input file>    Default is *standard-input*
;;;    -o <output file>   Default is *standard-output*
;;;
;;;  Commands can have other arguments:
;;;
;;;  callers -i <in> -o <out> -c <callee-name>
;;;      generates a list of callers of callee-name and how often they call


(finish-output *debug-io*)
(let* ((number-args (first *script-args*))
       (cmd (second *script-args*))
       (argv (subseq (cddr *script-args*) 0 number-args))
       (args (make-hash-table :test #'equal )))
  (declare (optimize (debug 3)))
  (loop for cur = argv then (cddr cur)
        for key = (car cur)
        for val = (cadr cur)
        while cur
        do (setf (gethash key args) val))
  (format *debug-io* "cmd = ~a~%" cmd)
  (let ((in-stream (let ((in-file (gethash "-i" args)))
                     (if in-file
                         (open in-file :direction :input :external-format :latin-1)
                         *standard-input*)))
        (out-file (gethash "-o" args))
        (out-stream (let ((out-file (gethash "-o" args)))
                      (if out-file
                          (open out-file :direction :output :if-exists :supersede :external-format :latin-1)
                          *standard-output*))))
    (cond
      ((search "cleanup-stacks" cmd)
       (cleanup-stacks in-stream out-stream))
      ((search "count-tips" cmd)
       (count-tips in-stream out-stream))
      ((search "count-calls" cmd)
       (count-calls in-stream out-stream))
      ((search "prune-count" cmd)
       (let ((stop-at (gethash "-s" args nil)))
         (print-fraction in-stream out-stream :stop-at stop-at)))
      ((search "prune" cmd)
       (let ((prune-name (gethash "-s" args)))
         (unless prune-name
           (error "You must provide the name (-s name) of a function to prune on"))
         (prune in-stream out-stream prune-name)))
      ((search "callers" cmd)
       (let ((callee-name (gethash "-c" args)))
         (unless callee-name
           (error "You must provide the name (-c name) of the callee function"))
         (format *debug-io* "Callers of ~a~%" callee-name)
         (callers in-stream out-stream callee-name)))
      (t (error "Unknown command")))
    (unless (eq in-stream *standard-input*)
      (close in-stream))
    (unless (eq out-stream *standard-output*)
      (when out-file (format *debug-io* "~a~%" out-file))
      (close out-stream))))

(sb-ext:exit)
