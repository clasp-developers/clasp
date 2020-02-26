#!/bin/sh
#|
SCRIPT_DIR="$(dirname "$0")"
exec sbcl --noinform --dynamic-space-size 2048 --disable-ldb --lose-on-corruption --disable-debugger \
--no-sysinit --no-userinit --noprint \
--eval '(set-dispatch-macro-character #\# #\! (lambda (s c n)(declare (ignore c n)) (read-line s) (values)))' \
--eval "(defvar *script-args* '( $# \"$0\" \"$1\" \"$2\" \"$3\" \"$4\" \"$5\" \"$6\" \"$7\" ))" \
--eval "(require :asdf)" \
--load "$0"
|#


(defun split-str-1 (string &optional (separator " ") (r nil))
  (let ((n (position separator string
                     :from-end t
                     :test #'(lambda (x y)
                               (find y x :test #'string=)))))
    (if n
        (split-str-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
        (cons string r))))

(defun split-str (string &optional (separator " "))
  (split-str-1 string separator))


(defparameter *cleanup-options* '("cleavir"))

(defun parse-cleanup-options (string)
  (let ((split (split-str string ",")))
    (format t "options split = ~s~%" split)
    (if (equal split '(nil))
        nil
        (mapcar (lambda (opt)
                  (if (member opt *cleanup-options* :test #'string=)
                      opt
                      (error "Illegal option ~a - must be one of ~a" opt *cleanup-options*)))
                split))))

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

(defstruct dtrace-backtrace frames count)

(defun backtrace-equal (x y)
  (equal (dtrace-backtrace-frames x) (dtrace-backtrace-frames y)))

(defun read-dtrace-backtrace-raw (stream &optional eofp eof)
  (loop named read-lines
        for line = (let ((ln (read-line stream nil :eof)))
                     (when (eq ln :eof)
                       (if eofp
                           (error "End of file encountered")
                           (return-from read-lines eof)))
                     ln)
        until (= (length line) 0)
        collect (remove-offset line)))

(defun read-dtrace-backtrace (stream &optional eofp eof)
  (let ((raw-backtrace (read-dtrace-backtrace-raw stream eofp eof)))
    (if (consp raw-backtrace)
        (let* ((frames (subseq raw-backtrace 0 (1- (length raw-backtrace))))
               (last-line (car (last raw-backtrace 1)))
               (count (parse-integer (string-trim " " last-line) :junk-allowed t)))
          (make-dtrace-backtrace :frames frames :count count))
        raw-backtrace)))

(defun write-block (stream backtrace)
  (loop for x in (dtrace-backtrace-frames backtrace)
     do (princ x stream)
        do (terpri stream))
  (format stream "                ~a~%" (dtrace-backtrace-count backtrace))
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
  (setf (dtrace-backtrace-frames backtrace)
        (loop for line in (dtrace-backtrace-frames backtrace)
              unless (or (search "VariadicFunctor" line)
                         (search "core__call_with_variable_bound" line)
                         (search "core::apply_method" line)
                         (search "core::funcall_va_list" line)
                         (search "core::funcall_consume_valist" line)
                         (search "core::cl__apply" line)
                         (search "FuncallableInstance_O::entry_point" line)
                         (search "standard_dispatch" line)
                         (search "funcall_frame" line)
                         (search "cc_call_multipleValueOneFormCall" line)
                         (search "core::core__funwind_protect" line)
                         (search "core::core__multiple_value_prog1_function" line)
                         (search "INVALIDATED-DISPATCH-FUNCTION^CLOS" line)
                         (search "INVALIDATED-DISCRIMINATING-FUNCTION^CLOS" line)
                         (search "core::FuncallableInstance_O::funcallable_entry_point" line)
                         (search "APPLY^COMMON-LISP" line)
                         (search "INTERPRETED-DISCRIMINATING-FUNCTION^CLOS" line)
                         (search "core::clos__interpret_dtree_program" line)
                         (search "LAMBDA^^COMMON-LISP_FN" line)
                         (search "COMBINE-METHOD-FUNCTIONS3.LAMBDA" line)
                         (search "^TOP-COMPILE-FILE" line))
                collect (cleanup-frame line)))
  backtrace)

#+(or)
(defun cleanup-dtrace-log (fin fout &key (verbose t))
  (let ((count 0))
    (let ((header (read-dtrace-header fin)))
      (write-block fout header)
      (loop for backtrace = (read-dtrace-backtrace-raw fin nil :eof)
            until (eq backtrace :eof)
            for cleaned = (cleanup-backtrace backtrace)
            when (> (length backtrace) 4000)
              #|              do (progn
                 (format *debug-io* "------------ input file pos: ~a~%" (file-position fin)) ;
                 (format *debug-io* "Backtrace with ~a frames~%" (length backtrace)) ;
                 (format *debug-io* "~a~%" cleaned #++(last backtrace 5))) ;
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
      (let ((tips (loop for backtrace = (read-dtrace-backtrace-raw fin nil :eof)
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
    (let ((calls (loop for backtrace = (read-dtrace-backtrace-raw fin nil :eof)
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

(defun prune-backtrace-height (backtrace max-frames)
  (let ((frames (dtrace-backtrace-frames backtrace)))
    (if (> (length frames) max-frames)
        (setf (dtrace-backtrace-frames backtrace) (subseq frames (- (length frames) max-frames) nil))))
  backtrace)

(defun remove-frame-offsets (backtrace)
  (let* ((frames (dtrace-backtrace-frames backtrace))
         (removed-offsets (loop for frame in frames
                                for plus-pos = (position #\+ frame :from-end t)
                                collect (if plus-pos (subseq frame 0 plus-pos) frame))))
    (setf (dtrace-backtrace-frames backtrace) removed-offsets))
  backtrace)

(defun write-folded-backtrace (backtrace fout)
  (let ((frames (reverse (dtrace-backtrace-frames backtrace))))
    (format fout "~{~a~^;~} ~d~%"
            (loop for frame in frames
             collect (let ((fr (string-trim " " frame)))
                      (if (> (length fr) 64)
                          (subseq fr 0 64)
                          fr)))
            (dtrace-backtrace-count backtrace))))


(defun invert-backtrace (backtrace depth)
  (declare (optimize (debug 3)))
  (if (integerp depth)
      (setf (dtrace-backtrace-frames backtrace) (nreverse (subseq (dtrace-backtrace-frames backtrace) 0 (min depth (length (dtrace-backtrace-frames backtrace))))))
      (setf (dtrace-backtrace-frames backtrace) (nreverse (dtrace-backtrace-frames backtrace))))
  backtrace)

(defun invert-frames (fin fout &key (depth nil))
  (let ((header (read-dtrace-header fin))
        (backtraces nil))
    (declare (ignore header))
    (loop named read-backtraces
          for raw-backtrace = (let ((bt (read-dtrace-backtrace fin nil :eof)))
                                (when (eq bt :eof) (return-from read-backtraces nil))
                                bt)
          for backtrace = (invert-backtrace raw-backtrace depth)
          do (push backtrace backtraces))
    (setf backtraces (reverse backtraces))
    (write-dtrace-header fout header)
    (loop for backtrace in backtraces
          do (write-block fout backtrace))))

(defun collapse (fin fout max-frames)
  (let ((header (read-dtrace-header fin))
        (folded nil))
    (declare (ignore header))
    (loop named read-backtraces
          for raw-backtrace = (let ((bt (read-dtrace-backtrace fin nil :eof)))
                            (when (eq bt :eof) (return-from read-backtraces nil))
                            bt)
          for backtrace = (cleanup-backtrace raw-backtrace)
          for backtrace-no-offsets = (remove-frame-offsets backtrace)
          for pruned = (prune-backtrace-height backtrace-no-offsets max-frames)
          do (if folded
                 (if (backtrace-equal pruned (first folded))
                     (incf (dtrace-backtrace-count (first folded)) (dtrace-backtrace-count pruned))
                     (push pruned folded))
                 (push pruned folded)))
    (setf folded (reverse folded))
    (loop for backtrace in folded
          do (write-folded-backtrace backtrace fout))))

(defun test-collapse
 (file-in file-out max-frames)
  (with-open-file (fin file-in :direction :input)
    (with-open-file (fout file-out :direction :output :if-exists :supersede)
      (collapse fin fout max-frames))))

;;; Prune backtraces down to a specific function in the backtraces.
;;; Look down each backtrace from until you find a frame with the function name that contains the focus-name.
;;; Write out the top of the backtrace to that point.
;;; If the backtrace doesn't contain that function - then write out an empty frame.
;;; This will let us build flame graphs by focusing in on function calls within a complex operation like compilation.
(defun prune (fin fout prune-name)
  (format *debug-io* "Looking for ~s~%" prune-name)
  (let ((header (read-dtrace-header fin)))
    (write-dtrace-header fout header))
  (loop for backtrace = (read-dtrace-backtrace-raw fin nil :eof)
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


(defun cleanup-stacks (fin fout options)
  "Remove useless info from the backtraces like CALL-WITH-VARIABLE-BOUND calls"
  (format *debug-io* "Running cleanup-stacks~%")
  (finish-output *debug-io*)
  (let ((header (read-dtrace-header fin))
        (cleavir-p (member "cleavir" options :test 'string=)))
     (declare (ignore cleavir-p))
    (write-dtrace-header fout header)
    (loop for backtrace = (read-dtrace-backtrace-raw fin nil :eof)
          until (eq backtrace :eof)
          when backtrace
            do (let ((repeat-line (car (last backtrace)))
                     (reversed-backtrace (reverse (butlast backtrace)))
                     (new-backtrace nil))
                 (unless repeat-line
                   (error "The repeat-line is NIL -  backtrace is ~%~s" backtrace))
                 (flet ((push-line (line)
                          (push (concatenate 'base-string "               " line) new-backtrace))
                        (pop-lines (num)
                          (dotimes (i num) (pop new-backtrace))))
                   (let ((cleaned-backtrace
                           (progn
                             (loop for cur = reversed-backtrace then (cdr cur)
                                   for line = (string-trim " " (car cur))
                                   for start = (let ((pos (position #\` line)))
                                                 (if pos pos 0))
                                   for end = (or (search "+0x" line) (length line))
                                   for name = (subseq line start end)
                                   while cur
                                   do (cond
                                        #+(or)((and cleavir-p (search "ClaspJIT_O::addModule" name))
                                         (pop-lines 6)
                                         (push-line "ORC::JIT-compiler")
                                         (setf cur nil) ; and we are done with this backtrace
                                         )
                                        ((search "MAPCAR^" name)
                                         (setf cur (cddr cur))
                                         (push-line name))
                                        ((search "CALL-WITH-VARIABLE-BOUND" name))
                                        ((search "core__call_with_variable_bound" name))
                                        ((search "core__funwind_protect" name))
                                        ((search "core__multiple_value_prog1_function" name))
                                        ((search "cl__apply" name))
                                        ((search "LAMBDA^COMMON-LISP" name))
                                        (t (push-line name))))
                             new-backtrace)))
                     (loop for line in cleaned-backtrace
                           if line
                             do (write-line line fout)
                           else
                             do (error "About to write-line NIL - the backtrace is: ~%~s" backtrace ))
                     (write-line repeat-line fout)
                     (terpri fout)))))))

(defun fraction (fin stop-at)
  (let ((header (read-dtrace-header fin)))
    (declare (ignore header))
    (let ((counts (make-hash-table :test #'equal))
          (num-backtraces 0))
      (loop for backtrace = (read-dtrace-backtrace-raw fin nil :eof)
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
    (loop for backtrace = (read-dtrace-backtrace-raw in-stream nil :eof)
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

(defun write-dtrace-header-for-perf (stream)
  (format stream "CPU     ID                    FUNCTION:NAME~%")
  (format stream "  0  64091                        :tick-60s~%")
  (loop repeat 2 do (terpri stream)))
                                        ; we don't use the dtrace headers anyway
(defun starts-with-p (str1 str2)
  "Determine whether `str1` starts with `str2`"
  (let ((p (search str2 str1)))
    (and p (= 0 p))))

(defun perf-frame-to-dtrace-frame (raw-frame)
  (let* ((frame (uiop:split-string
                 (string-left-trim " 	"  raw-frame)))
         (address (first frame))
         (process (car (last frame)))
         (function (format nil "~{~a~^ ~}" (subseq (butlast frame) 1)))
         (process-name (second (reverse (uiop:split-string process :separator '(#\( #\) #\/))))))
    (if (or (starts-with-p process-name "[unknown]") (starts-with-p process-name "perf"))
        (format nil "              `0x~a" address)
        (format nil "              ~a`~a" process-name function))
    ;(vector address function process-name)
   ))

(defun perf-to-dtrace-backtrace (backtrace-lines)
  ;(format t (first backtrace-lines))
  (let* ((count (parse-integer (second (reverse
                                        (uiop:split-string
                                         (string-right-trim " " (first backtrace-lines)))))))
         (raw-frames (cdr backtrace-lines))
         (frames (remove-if #'null
                            (mapcar #'perf-frame-to-dtrace-frame raw-frames))))
    (make-dtrace-backtrace :count 1 :frames frames)))

(defun collect-perf-data (in-stream)
  (let* ((raw-lines (loop with line = (read-line in-stream nil :eof)
                          collect line
                          do (setf line (read-line in-stream nil :eof))
                          until (eq line :eof)))
         (data (remove-if #'null
                          (loop with curr = nil
                                for x in raw-lines
                                when (string= x "")
                                collect curr into lists
                                when (string= x "")
                                do (setf curr nil)
                                when (not (string= x ""))
                                do (push x curr)
                                finally (return (mapcar #'reverse lists)))))
        (backtraces (mapcar #'perf-to-dtrace-backtrace data))
         )
    backtraces))

(defun perf-to-dtrace (perf-in-stream dtrace-out-stream)
  "Converts perf output to dtrace output for use with the profiler."
  (format *debug-io* "In perf-to-dtrace~%")
  (write-dtrace-header-for-perf dtrace-out-stream)
  (loop for backtrace in (collect-perf-data perf-in-stream)
        do (progn
             (format dtrace-out-stream "~{~a~%~}" (dtrace-backtrace-frames backtrace))
             (format dtrace-out-stream "                ~a~%~%" (dtrace-backtrace-count backtrace))))
  (format *debug-io* "Finished perf-to-dtrace~%")
  
  ;(format *standard-output* "~s" (first (collect-perf-data perf-in-stream)))
  )

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

(progn
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
          do (if (char= (char key 0) #\-)
                 (setf (gethash key args) val)
                 (error "Expected an option argument starting with '0' - got ~s" key)))
    (format *debug-io* "cmd = ~a~%" cmd)
    (let ((in-stream (let ((in-file (gethash "-i" args)))
                       (if in-file
                           (open in-file :direction :input :external-format '(:utf-8 :replacement #\?))
                           *standard-input*)))
          (out-file (gethash "-o" args))
          (out-stream (let ((out-file (gethash "-o" args)))
                        (if out-file
                            (open out-file :direction :output :if-exists :supersede :if-does-not-exist :create :external-format '(:utf-8 :replacement #\?))
                            *standard-output*))))
      (cond
        ((search "cleanup-stacks" cmd)
         (let ((options (parse-cleanup-options (gethash "-O" args nil))))
           (cleanup-stacks in-stream out-stream options)))
        ((search "count-tips" cmd)
         (format t "Usage: -i input.raw.stacks -o output-file.txt~%")
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
        ((search "perf2dtrace" cmd)
         (perf-to-dtrace in-stream out-stream))
        ((search "collapse" cmd)
         (unless (gethash "-m" args)
           (error "You must provide the -m <int> parameter"))
         (let ((max-frames (parse-integer (gethash "-m" args))))
           (format *debug-io* "Maximum number of frames: ~s~%" max-frames)
           (collapse in-stream out-stream max-frames)))
        ((search "invert" cmd)
         (unless (gethash "-m" args)
           (error "You must provide the number of frames to use from the top"))
         (let ((max-frames (parse-integer (gethash "-m" args))))
           (format *debug-io* "Maximum number of frames: ~s~%" max-frames)
           (invert-frames in-stream out-stream :depth max-frames)))
        (t (error "Unknown command")))
      (unless (eq in-stream *standard-input*)
        (close in-stream))
      (unless (eq out-stream *standard-output*)
        (when out-file (format *debug-io* "~a~%" out-file))
        (close out-stream))))
  (sb-ext:exit))
