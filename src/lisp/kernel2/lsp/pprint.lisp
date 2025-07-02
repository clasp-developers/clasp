;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;; -*- Package: PRETTY-PRINT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;; CMU Common Lisp pretty printer.
;;; Written by William Lott.  Algorithm stolen from Richard Waters' XP.
;;;
  
(in-package "SI")

(defconstant +ecl-safe-declarations+
  (if (boundp '+ecl-safe-declarations+)
      (symbol-value '+ecl-safe-declarations+)
      '(optimize (safety 2) (speed 1) (debug 1) (space 1))))

;;; FIXME: Move?
;;; FIXME: Better error (though these are internal structures, so it being signaled is a bug)
(defun required-argument ()
  (error "Missing required argument in struct constructor"))

;;; since (ext:check-arguments-type) is a no-op in clasp
;;; just used to check member types
(defmacro %verify-type (datum type)
  (let ((datum-symbol (gensym)))
    `(let ((,datum-symbol ,datum))
           (unless (typep ,datum-symbol ,type)
             (error 'type-error :datum ,datum-symbol :expected-type ,type)))))
;;;; Pretty streams

;;; There are three different units for measuring character positions:
;;;  COLUMN - offset (if characters) from the start of the current line.
;;;  INDEX - index into the output buffer.
;;;  POSN - some position in the stream of characters cycling through
;;;             the output buffer.
;;; 
(deftype column ()
  '(and fixnum unsigned-byte))
;;; The INDEX type is picked up from the kernel package.
(deftype posn ()
  'fixnum)

(defconstant initial-buffer-size 128)

(defconstant default-line-length 80)

(defclass pretty-stream (gray:fundamental-character-output-stream) (
  ;;
  ;; Where the output is going to finally go.
  ;;
  (target :initarg :target :initform t :type stream
	  :accessor pretty-stream-target)
  ;;
  ;; Line length we should format to.  Cached here so we don't have to keep
  ;; extracting it from the target stream.
  (line-length :initarg :line-length
	       :type column
	       :accessor pretty-stream-line-length)
  ;;
  ;; A simple string holding all the text that has been output but not yet
  ;; printed.
  (buffer :initform (make-string initial-buffer-size) :type simple-string
	  :accessor pretty-stream-buffer)
  ;;
  ;; The index into BUFFER where more text should be put.
  (buffer-fill-pointer :initform 0 :type index :accessor pretty-stream-buffer-fill-pointer)
  ;;
  ;; Whenever we output stuff from the buffer, we shift the remaining noise
  ;; over.  This makes it difficult to keep references to locations in
  ;; the buffer.  Therefore, we have to keep track of the total amount of
  ;; stuff that has been shifted out of the buffer.
  (buffer-offset :initform 0 :type posn :accessor pretty-stream-buffer-offset)
  ;;
  ;; The column the first character in the buffer will appear in.  Normally
  ;; zero, but if we end up with a very long line with no breaks in it we
  ;; might have to output part of it.  Then this will no longer be zero.
  (buffer-start-column :initarg :buffer-start-column :type column
		       :accessor pretty-stream-buffer-start-column)
  ;;
  ;; The line number we are currently on.  Used for *print-lines* abrevs and
  ;; to tell when sections have been split across multiple lines.
  (line-number :initform 0 :type index
	       :accessor pretty-stream-line-number)
  ;;
  ;; Stack of logical blocks in effect at the buffer start.
  (blocks :initform (list (make-logical-block)) :type list
	       :accessor pretty-stream-blocks)
  ;;
  ;; Buffer holding the per-line prefix active at the buffer start.
  ;; Indentation is included in this.  The length of this is stored
  ;; in the logical block stack.
  (prefix :initform (make-string initial-buffer-size) :type string
	       :accessor pretty-stream-prefix)
  ;;
  ;; Buffer holding the total remaining suffix active at the buffer start.
  ;; The characters are right-justified in the buffer to make it easier
  ;; to output the buffer.  The length is stored in the logical block
  ;; stack.
  (suffix :initform (make-string initial-buffer-size) :type string
	  :accessor pretty-stream-suffix)
  ;;
  ;; Queue of pending operations.  When empty, HEAD=TAIL=NIL.  Otherwise,
  ;; TAIL holds the first (oldest) cons and HEAD holds the last (newest)
  ;; cons.  Adding things to the queue is basically (setf (cdr head) (list
  ;; new)) and removing them is basically (pop tail) [except that care must
  ;; be taken to handle the empty queue case correctly.]
  (queue-tail :initform nil :type list :accessor pretty-stream-queue-tail)
  (queue-head :initform nil :type list :accessor pretty-stream-queue-head)
  ;;
  ;; Block-start queue entries in effect at the queue head.
  (pending-blocks :initform nil :type list :accessor pretty-stream-pending-blocks)
  )
)

(defun pretty-stream-p (stream)
  (typep stream 'pretty-stream))

(defun target-stream (stream)
  (if (pretty-stream-p stream)
      (pretty-stream-target stream)
      stream))

(defun make-pretty-stream (target)
  (make-instance 'pretty-stream
                 :target target
		 :buffer-start-column (or (stream-output-column target) 0)
                 :line-length (or *print-right-margin*
                                  (gray:stream-line-length target)
                                  default-line-length)))

(defmethod print-object ((pretty-stream pretty-stream) stream)
  (print-unreadable-object (pretty-stream stream :type t :identity t))
  #+nil
  (format stream "#<pretty stream {~8,'0X}>"
	  (kernel:get-lisp-obj-address pretty-stream)))

(declaim (inline index-posn posn-index posn-column))
(defun index-posn (index stream)
  (declare (type index index) (type pretty-stream stream))
  (+ index (pretty-stream-buffer-offset stream)))
(defun posn-index (posn stream)
  (declare (type posn posn) (type pretty-stream stream))
  (- posn (pretty-stream-buffer-offset stream)))
(defun posn-column (posn stream)
  (declare (type posn posn) (type pretty-stream stream))
  (index-column (posn-index posn stream) stream))


;;;; Stream interface routines.

(defmethod gray:stream-line-length ((stream pretty-stream))
  (pretty-stream-line-length stream))

(defmethod gray::stream-write-char ((stream pretty-stream) char)
  (pretty-out stream char))

(defmethod gray::stream-force-output ((stream pretty-stream))
  (declare (ignore stream))
  ;(force-pretty-output stream)
)

(defmethod gray::stream-clear-output ((stream pretty-stream))
  (declare (type pretty-stream stream))
  (clear-output (pretty-stream-target stream)))

(defun pretty-out (stream char)
  (declare (type pretty-stream stream)
	   (type character char))
  (cond ((char= char #\newline)
	 (enqueue-newline stream :literal))
	(t
	 (assure-space-in-buffer stream 1)
	 (let ((fill-pointer (pretty-stream-buffer-fill-pointer stream)))
	   (setf (schar (pretty-stream-buffer stream) fill-pointer) char)
	   (setf (pretty-stream-buffer-fill-pointer stream)
		 (1+ fill-pointer))))))

(defun pretty-sout (stream string start end)
  (declare (type pretty-stream stream)
	   (type string string)
	   (type index start)
	   (type (or index null) end))
  (let ((end (or end (length string))))
    (unless (= start end)
      (let ((newline (position #\newline string :start start :end end)))
	(cond
	 (newline
	  (pretty-sout stream string start newline)
	  (enqueue-newline stream :literal)
	  (pretty-sout stream string (1+ newline) end))
	 (t
	  (let ((chars (- end start)))
	    (loop
	      (let* ((available (assure-space-in-buffer stream chars))
		     (count (min available chars))
		     (fill-pointer (pretty-stream-buffer-fill-pointer stream))
		     (new-fill-ptr (+ fill-pointer count)))
		(replace (pretty-stream-buffer stream)
			 string
			 :start1 fill-pointer :end1 new-fill-ptr
			 :start2 start)
		(setf (pretty-stream-buffer-fill-pointer stream) new-fill-ptr)
		(decf chars count)
		(when (zerop count)
		  (return))
		(incf start count))))))))))


;;;; Logical blocks.

(defstruct logical-block
  ;;
  ;; The column this logical block started in.
  (start-column 0 :type column)
  ;;
  ;; The column the current section started in.
  (section-column 0 :type column)
  ;;
  ;; The length of the per-line prefix.  We can't move the indentation
  ;; left of this.
  (per-line-prefix-end 0 :type index)
  ;;
  ;; The overall length of the prefix, including any indentation.
  (prefix-length 0 :type index)
  ;;
  ;; The overall length of the suffix.
  (suffix-length 0 :type index)
  ;; 
  ;; The line number 
  (section-start-line 0 :type index))

(defun really-start-logical-block (stream column prefix suffix)
  (declare (type pretty-stream stream))
  (let* ((blocks (pretty-stream-blocks stream))
	 (prev-block (car blocks))
	 (per-line-end (logical-block-per-line-prefix-end prev-block))
	 (prefix-length (logical-block-prefix-length prev-block))
	 (suffix-length (logical-block-suffix-length prev-block))
	 (block (make-logical-block
		 :start-column column
		 :section-column column
		 :per-line-prefix-end per-line-end
		 :prefix-length prefix-length
		 :suffix-length suffix-length
		 :section-start-line (pretty-stream-line-number stream))))
    (setf (pretty-stream-blocks stream) (cons block blocks))
    (set-indentation stream column)
    (when prefix
      (setf (logical-block-per-line-prefix-end block) column)
      (replace (pretty-stream-prefix stream) prefix
	       :start1 (- column (length prefix)) :end1 column))
    (when suffix
      (let* ((total-suffix (pretty-stream-suffix stream))
	     (total-suffix-len (length total-suffix))
	     (additional (length suffix))
	     (new-suffix-len (+ suffix-length additional)))
	(when (> new-suffix-len total-suffix-len)
	  (let ((new-total-suffix-len
		 (max (* total-suffix-len 2)
		      (+ suffix-length
			 (floor (* additional 5) 4)))))
	    (setf total-suffix
		  (replace (make-string new-total-suffix-len) total-suffix
			   :start1 (- new-total-suffix-len suffix-length)
			   :start2 (- total-suffix-len suffix-length)))
	    (setf total-suffix-len new-total-suffix-len)
	    (setf (pretty-stream-suffix stream) total-suffix)))
	(replace total-suffix suffix
		 :start1 (- total-suffix-len new-suffix-len)
		 :end1 (- total-suffix-len suffix-length))
	(setf (logical-block-suffix-length block) new-suffix-len))))
  nil)

(defun set-indentation (stream column)
  (declare (type pretty-stream stream))
  (let* ((prefix (pretty-stream-prefix stream))
	 (prefix-len (length prefix))
	 (block (car (pretty-stream-blocks stream)))
	 (current (logical-block-prefix-length block))
	 (minimum (logical-block-per-line-prefix-end block))
	 (column (max minimum column)))
    (when (> column prefix-len)
      (setf prefix
	    (replace (make-string (max (* prefix-len 2)
				       (+ prefix-len
					  (floor (* (- column prefix-len) 5)
						 4))))
		     prefix
		     :end1 current))
      (setf (pretty-stream-prefix stream) prefix))
    (when (> column current)
      (fill prefix #\space :start current :end column))
    (setf (logical-block-prefix-length block) column)))

(defun really-end-logical-block (stream)
  (declare (type pretty-stream stream))
  (let* ((old (pop (pretty-stream-blocks stream)))
	 (old-indent (logical-block-prefix-length old))
	 (new (car (pretty-stream-blocks stream)))
	 (new-indent (logical-block-prefix-length new)))
    (when (> new-indent old-indent)
      (fill (pretty-stream-prefix stream) #\space
	    :start old-indent :end new-indent)))
  nil)


;;;; The pending operation queue.

(defstruct queued-op
  (posn 0 :type posn))

(eval-when (:compile-toplevel :execute)
(defmacro enqueue (stream type &rest args)
  (let ((constructor (intern (concatenate 'string
					  "MAKE-"
					  (symbol-name type)))))
    (once-only ((stream stream)
		(entry `(,constructor :posn
				      (index-posn
				       (pretty-stream-buffer-fill-pointer
					,stream)
				       ,stream)
				      ,@args))
		(op `(list ,entry))
		(head `(pretty-stream-queue-head ,stream)))
      `(progn
	 (if ,head
	     (setf (cdr ,head) ,op)
	     (setf (pretty-stream-queue-tail ,stream) ,op))
	 (setf (pretty-stream-queue-head ,stream) ,op)
	 ,entry))))
)

(defstruct (section-start
	    (:include queued-op))
  (depth 0 :type index)
  (section-end nil :type (or null newline block-end)))

(defstruct (newline
	    (:include section-start))
  (kind (required-argument)
	:type (member :linear :fill :miser :literal :mandatory)))

(defun enqueue-newline (stream kind)
  (declare (type pretty-stream stream))
  (let* ((depth (length (pretty-stream-pending-blocks stream)))
	 (newline (enqueue stream newline :kind kind :depth depth)))
    (dolist (entry (pretty-stream-queue-tail stream))
      (when (and (not (eq newline entry))
		 (section-start-p entry)
		 (null (section-start-section-end entry))
		 (<= depth (section-start-depth entry)))
	(setf (section-start-section-end entry) newline))))
  (maybe-output stream (or (eq kind :literal) (eq kind :mandatory))))

(defstruct (indentation
	    (:include queued-op))
  (kind (required-argument) :type (member :block :current))
  (amount 0 :type fixnum))

(defun enqueue-indent (stream kind amount)
  (declare (type pretty-stream stream))
  (enqueue stream indentation :kind kind :amount amount))

(defstruct (block-start
	    (:include section-start))
  (block-end nil :type (or null block-end))
  (prefix nil :type (or null string))
  (suffix nil :type (or null string)))

(defun start-logical-block (stream prefix per-line-p suffix)
  (declare (type string prefix suffix)
	   (type pretty-stream stream)
	   (ext:check-arguments-type))
  (let ((prefix-len (length prefix)))
    (when (plusp prefix-len)
      (pretty-sout stream prefix 0 prefix-len))
    (let* ((pending-blocks (pretty-stream-pending-blocks stream))
	   (start (enqueue stream block-start
			   :prefix (and (plusp prefix-len) per-line-p prefix)
			   :suffix (and (plusp (length suffix)) suffix)
			   :depth (length pending-blocks))))
      (setf (pretty-stream-pending-blocks stream)
	    (cons start pending-blocks)))))

(defstruct (block-end
	    (:include queued-op))
  (suffix nil :type (or null string)))

(defun end-logical-block (stream)
  (declare (type pretty-stream stream))
  (let* ((start (pop (pretty-stream-pending-blocks stream)))
	 (suffix (block-start-suffix start))
	 (end (enqueue stream block-end :suffix suffix)))
    (when suffix
      (pretty-sout stream suffix 0 (length suffix)))
    (setf (block-start-block-end start) end)))

(defstruct (tab
	    (:include queued-op))
  (sectionp nil :type (member t nil))
  (relativep nil :type (member t nil))
  (colnum 0 :type column)
  (colinc 0 :type column))

(defun enqueue-tab (stream kind colnum colinc)
  (declare (type pretty-stream stream))
  (multiple-value-bind
      (sectionp relativep)
      (ecase kind
	(:line (values nil nil))
	(:line-relative (values nil t))
	(:section (values t nil))
	(:section-relative (values t t)))
    (enqueue stream tab :sectionp sectionp :relativep relativep
	     :colnum colnum :colinc colinc)))


;;;; Tab support.

(defun compute-tab-size (tab section-start column)
  (let ((colnum (tab-colnum tab))
	(colinc (tab-colinc tab)))
    (when (tab-sectionp tab)
      (setf column (- column section-start)))
    (cond ((tab-relativep tab)
	   (unless (<= colinc 1)
	     (let ((newposn (+ column colnum)))
	       (let ((rem (rem newposn colinc)))
		 (unless (zerop rem)
		   (incf colnum (- colinc rem))))))
	   colnum)
	  ((< column colnum)
	   (- colnum column))
	  ((= column colnum)
	   colinc)
	  ((plusp colinc)
	   (- colinc (rem (- column colnum) colinc)))
	  (t
	   0))))

(defun index-column (index stream)
  (declare (type pretty-stream stream))
  (let ((column (pretty-stream-buffer-start-column stream))
	(section-start (logical-block-section-column
			(first (pretty-stream-blocks stream))))
	(end-posn (index-posn index stream)))
    (dolist (op (pretty-stream-queue-tail stream))
      (when (>= (queued-op-posn op) end-posn)
	(return))
      (typecase op
	(tab
	 (incf column
	       (compute-tab-size op
				 section-start
				 (+ column
				    (posn-index (tab-posn op)
						    stream)))))
	((or newline block-start)
	 (setf section-start
	       (+ column (posn-index (queued-op-posn op)
					 stream))))))
    (+ column index)))

(defun expand-tabs (stream through)
  (declare (type pretty-stream stream))
  (let ((insertions nil)
	(additional 0)
	(column (pretty-stream-buffer-start-column stream))
	(section-start (logical-block-section-column
			(first (pretty-stream-blocks stream)))))
    (dolist (op (pretty-stream-queue-tail stream))
      (typecase op
	(tab
	 (let* ((index (posn-index (tab-posn op) stream))
		(tabsize (compute-tab-size op
					   section-start
					   (+ column index))))
	   (unless (zerop tabsize)
	     (push (cons index tabsize) insertions)
	     (incf additional tabsize)
	     (incf column tabsize))))
	((or newline block-start)
	 (setf section-start
	       (+ column (posn-index (queued-op-posn op) stream)))))
      (when (eq op through)
	(return)))
    (when insertions
      (let* ((fill-ptr (pretty-stream-buffer-fill-pointer stream))
	     (new-fill-ptr (+ fill-ptr additional))
	     (buffer (pretty-stream-buffer stream))
	     (new-buffer buffer)
	     (length (length buffer))
	     (end fill-ptr))
	(when (> new-fill-ptr length)
	  (let ((new-length (max (* length 2)
				 (+ fill-ptr
				    (floor (* additional 5) 4)))))
	    (setf new-buffer (make-string new-length))
	    (setf (pretty-stream-buffer stream) new-buffer)))
	(setf (pretty-stream-buffer-fill-pointer stream) new-fill-ptr)
	(decf (pretty-stream-buffer-offset stream) additional)
	(dolist (insertion insertions)
	  (let* ((srcpos (car insertion))
		 (amount (cdr insertion))
		 (dstpos (+ srcpos additional)))
	    (replace new-buffer buffer :start1 dstpos :start2 srcpos :end2 end)
	    (fill new-buffer #\space :start (- dstpos amount) :end dstpos)
	    (decf additional amount)
	    (setf end srcpos)))
	(unless (eq new-buffer buffer)
	  (replace new-buffer buffer :end1 end :end2 end))))))


;;;; Stuff to do the actual outputting.

(defun assure-space-in-buffer (stream want)
  (declare (type pretty-stream stream)
	   (type index want))
  (let* ((buffer (pretty-stream-buffer stream))
	 (length (length buffer))
	 (fill-ptr (pretty-stream-buffer-fill-pointer stream))
	 (available (- length fill-ptr)))
    (cond ((plusp available)
	   available)
	  ((> fill-ptr (pretty-stream-line-length stream))
	   (unless (maybe-output stream nil)
	     (output-partial-line stream))
	   (assure-space-in-buffer stream want))
	  (t
	   (let* ((new-length (max (* length 2)
				   (+ length
				      (floor (* want 5) 4))))
		  (new-buffer (make-string new-length)))
	     (setf (pretty-stream-buffer stream) new-buffer)
	     (replace new-buffer buffer :end1 fill-ptr)
	     (- new-length fill-ptr))))))

(defun maybe-output (stream force-newlines-p)
  (declare (type pretty-stream stream))
  (let ((tail (pretty-stream-queue-tail stream))
	(output-anything nil))
    (loop
      (unless tail
	(setf (pretty-stream-queue-head stream) nil)
	(return))
      (let ((next (pop tail)))
	(etypecase next
	  (newline
	   (when (ecase (newline-kind next)
		   ((:literal :mandatory :linear) t)
		   (:miser (misering-p stream))
		   (:fill
		    (or (misering-p stream)
			(> (pretty-stream-line-number stream)
			   (logical-block-section-start-line
			    (first (pretty-stream-blocks stream))))
			(ecase (fits-on-line-p stream
					       (newline-section-end next)
					       force-newlines-p)
			  ((t) nil)
			  ((nil) t)
			  (:dont-know
			   (return))))))
	     (setf output-anything t)
	     (output-line stream next)))
	  (indentation
	   (unless (misering-p stream)
	     (set-indentation stream
			      (+ (ecase (indentation-kind next)
				   (:block
				    (logical-block-start-column
				     (car (pretty-stream-blocks stream))))
				   (:current
				    (posn-column
				     (indentation-posn next)
				     stream)))
				 (indentation-amount next)))))
	  (block-start
	   (ecase (fits-on-line-p stream (block-start-section-end next)
				  force-newlines-p)
	     ((t)
	      ;; Just nuke the whole logical block and make it look like one
	      ;; nice long literal.
	      (let ((end (block-start-block-end next)))
		(expand-tabs stream end)
		(setf tail (cdr (member end tail)))))
	     ((nil)
	      (really-start-logical-block
	       stream
	       (posn-column (block-start-posn next) stream)
	       (block-start-prefix next)
	       (block-start-suffix next)))
	     (:dont-know
	      (return))))
	  (block-end
	   (really-end-logical-block stream))
	  (tab
	   (expand-tabs stream next))))
      (setf (pretty-stream-queue-tail stream) tail))
    output-anything))

(defun misering-p (stream)
  (declare (type pretty-stream stream))
  (and *print-miser-width*
       (<= (- (pretty-stream-line-length stream)
	      (logical-block-start-column (car (pretty-stream-blocks stream))))
	   *print-miser-width*)))

(defun fits-on-line-p (stream until force-newlines-p)
  (declare (type pretty-stream stream))
  (let ((available (pretty-stream-line-length stream)))
    (when (and (not *print-readably*) *print-lines*
	       (= *print-lines* (pretty-stream-line-number stream)))
      (decf available 3) ; for the `` ..''
      (decf available (logical-block-suffix-length
		       (car (pretty-stream-blocks stream)))))
    (cond (until
	   (<= (posn-column (queued-op-posn until) stream) available))
	  (force-newlines-p nil)
	  ((> (index-column (pretty-stream-buffer-fill-pointer stream) stream)
	      available)
	   nil)
	  (t
	   :dont-know))))

(defun output-line (stream until)
  (declare (type pretty-stream stream)
	   (type newline until))
  (let* ((target (pretty-stream-target stream))
	 (buffer (pretty-stream-buffer stream))
	 (kind (newline-kind until))
	 (literal-p (eq kind :literal))
	 (amount-to-consume (posn-index (newline-posn until) stream))
	 (amount-to-print
	  (if literal-p
	      amount-to-consume
	      (let ((last-non-blank
		     (position #\space buffer :end amount-to-consume
			       :from-end t :test #'char/=)))
		(if last-non-blank
		    (1+ last-non-blank)
		    0)))))
    (write-string buffer target :end amount-to-print)
    (let ((line-number (pretty-stream-line-number stream)))
      (incf line-number)
      (when (and (not *print-readably*)
		 *print-lines* (>= line-number *print-lines*))
	(write-string " .." target)
	(let ((suffix-length (logical-block-suffix-length
			      (car (pretty-stream-blocks stream)))))
	  (unless (zerop suffix-length)
	    (let* ((suffix (pretty-stream-suffix stream))
		   (len (length suffix)))
	      (write-string suffix target
			    :start (- len suffix-length)
			    :end len))))
	(throw 'line-limit-abbreviation-happened t))
      (setf (pretty-stream-line-number stream) line-number)
      (write-char #\newline target)
      (setf (pretty-stream-buffer-start-column stream) 0)
      (let* ((fill-ptr (pretty-stream-buffer-fill-pointer stream))
	     (block (first (pretty-stream-blocks stream)))
	     (prefix-len
	      (if literal-p
		  (logical-block-per-line-prefix-end block)
		  (logical-block-prefix-length block)))
	     (shift (- amount-to-consume prefix-len))
	     (new-fill-ptr (- fill-ptr shift))
	     (new-buffer buffer)
	     (buffer-length (length buffer)))
	(when (> new-fill-ptr buffer-length)
	  (setf new-buffer
		(make-string (max (* buffer-length 2)
				  (+ buffer-length
				     (floor (* (- new-fill-ptr buffer-length)
					       5)
					    4)))))
	  (setf (pretty-stream-buffer stream) new-buffer))
	(replace new-buffer buffer
		 :start1 prefix-len :start2 amount-to-consume :end2 fill-ptr)
	(replace new-buffer (pretty-stream-prefix stream)
		 :end1 prefix-len)
	(setf (pretty-stream-buffer-fill-pointer stream) new-fill-ptr)
	(incf (pretty-stream-buffer-offset stream) shift)
	(unless literal-p
	  (setf (logical-block-section-column block) prefix-len)
	  (setf (logical-block-section-start-line block) line-number))))))

(defun output-partial-line (stream)
  (declare (type pretty-stream stream))
  (let* ((fill-ptr (pretty-stream-buffer-fill-pointer stream))
	 (tail (pretty-stream-queue-tail stream))
	 (count
	  (if tail
	      (posn-index (queued-op-posn (car tail)) stream)
	      fill-ptr))
	 (new-fill-ptr (- fill-ptr count))
	 (buffer (pretty-stream-buffer stream)))
    (when (zerop count)
      (error "Output-partial-line called when nothing can be output."))
    (write-string buffer (pretty-stream-target stream)
		  :start 0 :end count)
    (incf (pretty-stream-buffer-start-column stream) count)
    (replace buffer buffer :end1 new-fill-ptr :start2 count :end2 fill-ptr)
    (setf (pretty-stream-buffer-fill-pointer stream) new-fill-ptr)
    (incf (pretty-stream-buffer-offset stream) count)))

(defun force-pretty-output (stream)
  (declare (type pretty-stream stream))
  (maybe-output stream nil)
  (expand-tabs stream nil)
  (write-string (pretty-stream-buffer stream)
		(pretty-stream-target stream)
		:end (pretty-stream-buffer-fill-pointer stream)))


;;;; Utilities.

(defun pprint-pop-helper (object count stream &aux code)
  (cond ((or (not (listp object))
             (and (plusp count)
                  (consp (cdr object))
                  (null (cddr object))
                  (or (eq (car object) 'core:quasiquote)
                      (and (getf core:*quasiquote* (target-stream stream))
                           (member (car object)
                                   '(core:unquote core:unquote-splice core:unquote-nsplice))))))
	 (write-string ". " stream)
	 (write-object object stream)
	 nil)
	((and (not *print-readably*)
	      (eql count *print-length*))
	 (write-string "..." stream)
	 nil)
	((or (null object)
	     (zerop count)
	     (null *circle-counter*))
	 t)
	((eql 'NULL (setf code (gethash object *circle-stack* 'NULL)))
	 ;; We visit this part of the list for the first time and thus we must
	 ;; register it in the hash, or we are on the second pass and have
	 ;; found a completely new list. This should not happend, but anyway
	 ;; we try to print it.
	 (search-print-circle object)
	 t)
	((and (null code) (integerp *circle-counter*))
	 ;; This object is not visited twice.
	 t)
	(t
	 ;; In all other cases, WRITE-OBJECT
	 (write-string ". " stream)
	 (write-object object stream)
	 nil)))

;;;; User interface to the pretty printer.

(defun check-print-level ()
  "Automatically handle *print-level* abbreviation.  If we are too deep, then
   a # is printed to STREAM and BODY is ignored."
  (cond ((or *print-readably* (null *print-level*))
	 t)
	((zerop *print-level*)
	 nil)
	(t
	 (setf *print-level* (1- *print-level*)))))

(defun search-print-circle (object)
  (multiple-value-bind
        (code present-p)
      (gethash object *circle-stack*)
    (if (not (fixnump *circle-counter*))
        (cond ((not present-p)
	       ;; Was not found before
	       (setf (gethash object *circle-stack*) nil)
	       0)
	      ((null code)
	       ;; Second reference
	       (setf (gethash object *circle-stack*) t)
	       1)
	      (t
	       ;; Further references
	       2))
	(cond ((or (not present-p) (null code))
	       ;; Is not referenced or was not found before
	       0)
	      ((eql code t)
	       ;; Reference twice but had no code yet
               (incf *circle-counter*)
	       (setf (gethash object *circle-stack*)
		     *circle-counter*)
	       (- *circle-counter*))
	      (t code)))))
	       
(defun write-object-with-circle (object stream function)
  (if (and *print-circle*
           (not (null object))
           (not (fixnump object))
           (not (characterp object))
           (or (not (symbolp object)) (null (symbol-package object))))
            ;;; *print-circle* and an object that might have a circle
      (if (null *circle-counter*)
          (let* ((hash (make-hash-table :test 'eq
                                        :size 1024))
                 (*circle-counter* t)
                 (*circle-stack* hash))
            (write-object-with-circle object (make-broadcast-stream) function)
            (setf *circle-counter* 0)
            (write-object-with-circle object stream function)
            (clrhash hash)
            object)
          (let ((code (search-print-circle object)))
            (cond ((not (fixnump *circle-counter*))
                   ;; We are only inspecting the object to be printed.
                   ;; Only print X if it was not referenced before
                   (if (not (zerop code))
                       object
                       (funcall function object stream)))
                  ((zerop code)
                   ;; Object is not referenced twice
                   (funcall function object stream))
                  ((minusp code)
                   ;; Object is referenced twice. We print its definition 
                   (write-char #\# stream)
                   (let ((*print-radix* nil)
                         (*print-base* 10))
                     (write-ugly-object (- code) stream))
                   (write-char #\= stream)
                   (funcall function object stream))
                  (t
                   ;; Second reference to the object
                   (write-char #\# stream)
                   (let ((*print-radix* nil)
                         (*print-base* 10))
                     (write-ugly-object code stream))
                   (write-char #\# stream)
                   object))
            ))
      ;;; live is good, print simple
      (funcall function object stream)
      ))

(defun do-pprint-logical-block (function object stream prefix
				per-line-prefix-p suffix)
  (cond ((not (listp object))
         (write-object object stream))
        ((and (not *print-readably*) (eql *print-level* 0))
         (write-char #\# stream))
        (t (write-object-with-circle
            object stream
            #'(lambda (object stream1)
                (unless (pretty-stream-p stream1)
                  (setf stream1 (make-pretty-stream stream1)))
                (let ((*print-level* (and (not *print-readably*)
                                          *print-level*
                                          (1- *print-level*))))
                  (start-logical-block stream1 prefix per-line-prefix-p suffix)
                  (funcall function object stream1)
                  (end-logical-block stream1))))))
  nil)

(defun pprint-logical-block-helper (function object stream prefix
				    per-line-prefix-p suffix)
  (check-type prefix string)
  (check-type suffix string)
  (setf stream (case stream
		 ((nil) *standard-output*)
		 ((t) *terminal-io*)
		 (t stream)))
  (if (pretty-stream-p stream)
      (do-pprint-logical-block function object stream prefix
			       per-line-prefix-p suffix)
      (let ((stream (make-pretty-stream stream)))
	(catch 'line-limit-abbreviation-happened
	  (do-pprint-logical-block function object stream prefix
				   per-line-prefix-p suffix)
	  (force-pretty-output stream))
	nil)))

(defmacro pprint-logical-block
	  ((stream-symbol object &key (prefix "" prefix-p)
			  (per-line-prefix "" per-line-prefix-p)
			  (suffix ""))
	   &body body)
  "Group some output into a logical block.  STREAM-SYMBOL should be either a
   stream, T (for *TERMINAL-IO*), or NIL (for *STANDARD-OUTPUT*).  The printer
   control variable *PRINT-LEVEL* is automatically handled."
  (declare #.+ecl-safe-declarations+)
  (when per-line-prefix-p
    (when prefix-p
      (error "Cannot specify both a prefix and a per-line-prefix."))
    (setf prefix per-line-prefix))
  (let* ((object-var (gensym))
	 (block-name (gensym "PPRINT-LOGICAL-BLOCK-"))
	 (count-name (gensym "PPRINT-LOGICAL-BLOCK-LENGTH-"))
	 (stream-var (case stream-symbol
		       ((nil) '*standard-output*)
		       ((t) '*terminal-io*)
		       (t stream-symbol)))
         (function
	  `(lambda (,object-var ,stream-var &aux (,count-name 0))
             (declare (ignorable ,object-var ,stream-var ,count-name) 
                      (core:lambda-name ,block-name))
             (block ,block-name 
               (macrolet ((pprint-pop ()
                            '(progn
                              (unless (pprint-pop-helper ,object-var ,count-name
                                                         ,stream-var)
                                (return-from ,block-name nil))
                              (incf ,count-name)
                              ,(if object `(pop ,object-var) nil)))
                          (pprint-exit-if-list-exhausted ()
                            ,(if object
                                 `'(when (null ,object-var)
                                    (return-from ,block-name nil))
                                 `'(return-from ,block-name nil))))
                 ,@body)))))
    `(pprint-logical-block-helper #',function ,object ,stream-symbol
                                  ,prefix ,per-line-prefix-p ,suffix)))

(defmacro pprint-exit-if-list-exhausted ()
  "Cause the closest enclosing use of PPRINT-LOGICAL-BLOCK to return
   if it's list argument is exhausted.  Can only be used inside
   PPRINT-LOGICAL-BLOCK, and only when the LIST argument to
   PPRINT-LOGICAL-BLOCK is supplied."
  (declare #.+ecl-safe-declarations+)
  (error "PPRINT-EXIT-IF-LIST-EXHAUSTED must be lexically inside ~
	  PPRINT-LOGICAL-BLOCK."))

(defmacro pprint-pop ()
  "Return the next element from LIST argument to the closest enclosing
   use of PPRINT-LOGICAL-BLOCK, automatically handling *PRINT-LENGTH*
   and *PRINT-CIRCLE*.  Can only be used inside PPRINT-LOGICAL-BLOCK.
   If the LIST argument to PPRINT-LOGICAL-BLOCK was NIL, then nothing
   is poped, but the *PRINT-LENGTH* testing still happens."
  (declare #.+ecl-safe-declarations+)
  (error "PPRINT-POP must be lexically inside PPRINT-LOGICAL-BLOCK."))

(defun pprint-newline (kind &optional stream)
  "Output a conditional newline to STREAM (which defaults to
   *STANDARD-OUTPUT*) if it is a pretty-printing stream, and do
   nothing if not.  KIND can be one of:
     :LINEAR - A line break is inserted if and only if the immediatly
        containing section cannot be printed on one line.
     :MISER - Same as LINEAR, but only if ``miser-style'' is in effect.
        (See *PRINT-MISER-WIDTH*.)
     :FILL - A line break is inserted if and only if either:
       (a) the following section cannot be printed on the end of the
           current line,
       (b) the preceding section was not printed on a single line, or
       (c) the immediately containing section cannot be printed on one
           line and miser-style is in effect.
     :MANDATORY - A line break is always inserted.
   When a line break is inserted by any type of conditional newline, any
   blanks that immediately precede the conditional newline are ommitted
   from the output and indentation is introduced at the beginning of the
   next line.  (See PPRINT-INDENT.)"
  (declare (type (member :linear :miser :fill :mandatory) kind)
	   (type (or stream (member t nil)) stream)
	   (ext:check-arguments-type))
  (%verify-type kind '(member :linear :miser :fill :mandatory))
  (let ((stream (case stream
		  ((t) *terminal-io*)
		  ((nil) *standard-output*)
		  (t stream))))
    (when (and (pretty-stream-p stream) *print-pretty*)
      (enqueue-newline stream kind)))
  nil)

(defun pprint-indent (relative-to n &optional stream)
  "Specify the indentation to use in the current logical block if STREAM
   (which defaults to *STANDARD-OUTPUT*) is it is a pretty-printing stream
   and do nothing if not.  (See PPRINT-LOGICAL-BLOCK.)  N is the indention
   to use (in ems, the width of an ``m'') and RELATIVE-TO can be either:
     :BLOCK - Indent relative to the column the current logical block
        started on.
     :CURRENT - Indent relative to the current column.
   The new indention value does not take effect until the following line
   break."
  (declare (type (member :block :current) relative-to)
	   (type real n)
	   (type (or stream (member t nil)) stream)
	   (ext:check-arguments-type))
  (%verify-type relative-to '(member :block :current))
  (let ((stream (case stream
		  ((t) *terminal-io*)
		  ((nil) *standard-output*)
		  (t stream))))
    (when (and (pretty-stream-p stream) *print-pretty*)
      (enqueue-indent stream relative-to (round n))))
  nil)

(defun pprint-tab (kind colnum colinc &optional stream)
  "If STREAM (which defaults to *STANDARD-OUTPUT*) is a pretty-printing
   stream, perform tabbing based on KIND, otherwise do nothing.  KIND can
   be one of:
     :LINE - Tab to column COLNUM.  If already past COLNUM tab to the next
       multiple of COLINC.
     :SECTION - Same as :LINE, but count from the start of the current
       section, not the start of the line.
     :LINE-RELATIVE - Output COLNUM spaces, then tab to the next multiple of
       COLINC.
     :SECTION-RELATIVE - Same as :LINE-RELATIVE, but count from the start
       of the current section, not the start of the line."
  (declare (type (member :line :section :line-relative :section-relative) kind)
	   (type unsigned-byte colnum colinc)
	   (type (or stream (member t nil)) stream)
	   (ext:check-arguments-type))
  (%verify-type kind '(member :line :section :line-relative :section-relative))
  (let ((stream (case stream
		  ((t) *terminal-io*)
		  ((nil) *standard-output*)
		  (t stream))))
    (when (and (pretty-stream-p stream) *print-pretty*)
      (enqueue-tab stream kind colnum colinc)))
  nil)

(defun pprint-fill (stream list &optional (colon? t) atsign?)
  "Output LIST to STREAM putting :FILL conditional newlines between each
   element.  If COLON? is NIL (defaults to T), then no parens are printed
   around the output.  ATSIGN? is ignored (but allowed so that PPRINT-FILL
   can be used with the ~/.../ format directive."
  (declare (ignore atsign?)
           (type (or stream (member t nil)) stream)
	   (ext:check-arguments-type)
	   #.+ecl-safe-declarations+)
  (pprint-logical-block (stream list
				:prefix (if colon? "(" "")
				:suffix (if colon? ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop
      (write-object (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\space stream)
      (pprint-newline :fill stream))))

(defun pprint-linear (stream list &optional (colon? t) atsign?)
  "Output LIST to STREAM putting :LINEAR conditional newlines between each
   element.  If COLON? is NIL (defaults to T), then no parens are printed
   around the output.  ATSIGN? is ignored (but allowed so that PPRINT-LINEAR
   can be used with the ~/.../ format directive."
  (declare (ignore atsign?)
           (type (or stream (member t nil)) stream)
	   (ext:check-arguments-type)
	   #.+ecl-safe-declarations+)
  (pprint-logical-block (stream list
				:prefix (if colon? "(" "")
				:suffix (if colon? ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop
      (write-object (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\space stream)
      (pprint-newline :linear stream))))

(defun pprint-tabular (stream list &optional (colon? t) atsign? (tabsize 16))
  "Output LIST to STREAM tabbing to the next column that is an even multiple
   of TABSIZE (which defaults to 16) between each element.  :FILL style
   conditional newlines are also output between each element.  If COLON? is
   NIL (defaults to T), then no parens are printed around the output.
   ATSIGN? is ignored (but allowed so that PPRINT-TABULAR can be used with
   the ~/.../ format directive."
  (declare (ignore atsign?)
           (type (or stream (member t nil)) stream)
           (type (or unsigned-byte null) tabsize)
           (ext:check-arguments-type))
  (pprint-logical-block (stream list
				:prefix (if colon? "(" "")
				:suffix (if colon? ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop
      (write-object (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\space stream)
      (pprint-tab :section-relative 0 (or tabsize 16) stream)
      (pprint-newline :fill stream))))


;;;; Pprint-dispatch tables.

(defvar *standard-pprint-dispatch*)
(defvar *initial-pprint-dispatch*)

(defstruct (pprint-dispatch-entry
	    (:print-function %print-pprint-dispatch-entry))
  ;;
  ;; The type specifier for this entry.
  (type (required-argument) :type t)
  ;;
  ;; The priority for this guy.
  (priority 0 :type real)
  ;;
  ;; T iff one of the original entries.
  (initial-p (not (boundp '*initial-pprint-dispatch*)) :type (member t nil))
  ;;
  ;; And the associated function.
  (function (required-argument) :type (or function symbol)))

(defun %print-pprint-dispatch-entry (entry stream depth)
  (declare (ignore depth))
  (print-unreadable-object (entry stream :type t)
    (format stream "Type=~S, priority=~S~@[ [Initial]~]"
	    (pprint-dispatch-entry-type entry)
	    (pprint-dispatch-entry-priority entry)
	    (pprint-dispatch-entry-initial-p entry))))

(defstruct (pprint-dispatch-table
            (:print-function %print-pprint-dispatch-table))
  ;; Are we allowed to modify this table?
  (read-only-p nil)
  ;;
  ;; A list of all the entries (except for CONS entries below) in highest
  ;; to lowest priority.
  (entries nil :type list)
  ;;
  ;; A hash table mapping things to entries for type specifiers of the
  ;; form (CONS (MEMBER <thing>)).  If the type specifier is of this form,
  ;; we put it in this hash table instead of the regular entries table.
  (cons-entries (make-hash-table :test #'eql)))

(defun %print-pprint-dispatch-table (table stream depth)
  (declare (ignore depth))
  (print-unreadable-object (table stream :type t :identity t)))

(defun cons-type-specifier-p (spec)
  (and (consp spec)
       (eq (car spec) 'cons)
       (cdr spec)
       (null (cddr spec))
       (let ((car (cadr spec)))
	 (and (consp car)
	      (let ((carcar (car car)))
		(or (eq carcar 'member)
		    (eq carcar 'eql)))
	      (cdr car)
	      (null (cddr car))))))

(defun entry< (e1 e2)
  (declare (type pprint-dispatch-entry e1 e2))
  (if (pprint-dispatch-entry-initial-p e1)
      (if (pprint-dispatch-entry-initial-p e2)
	  (< (pprint-dispatch-entry-priority e1)
	     (pprint-dispatch-entry-priority e2))
	  t)
      (if (pprint-dispatch-entry-initial-p e2)
	  nil
	  (< (pprint-dispatch-entry-priority e1)
	     (pprint-dispatch-entry-priority e2)))))


(defun copy-pprint-dispatch (&optional (table *print-pprint-dispatch*))
  (declare (type (or pprint-dispatch-table null) table)
	   #.+ecl-safe-declarations+)
  (let* ((orig (or table *initial-pprint-dispatch*)))
    (let* ((new (make-pprint-dispatch-table
		 :entries (copy-list (pprint-dispatch-table-entries orig))))
	   (new-cons-entries (pprint-dispatch-table-cons-entries new)))
      (maphash #'(lambda (key value)
		   (setf (gethash key new-cons-entries) value))
	       (pprint-dispatch-table-cons-entries orig))
      new)))

(defun default-pprint-dispatch (stream object)
  (write-ugly-object object stream))

(defun pprint-dispatch (object &optional (table *print-pprint-dispatch*))
  (declare (type (or pprint-dispatch-table null) table)
	   (ext:check-arguments-type)
	   #.+ecl-safe-declarations+)
  (let* ((table (or table *initial-pprint-dispatch*))
	 (cons-entry
	  (and (consp object)
	       (gethash (car object)
			(pprint-dispatch-table-cons-entries table))))
	 (entry
	  (dolist (entry (pprint-dispatch-table-entries table) cons-entry)
	    (when (and cons-entry
		       (entry< entry cons-entry))
	      (return cons-entry))
	    (when (typep object (pprint-dispatch-entry-type entry))
	      (return entry)))))
    (if entry
	(values (pprint-dispatch-entry-function entry) t)
	(values #'default-pprint-dispatch nil))))

(defun set-pprint-dispatch (type function &optional
			    (priority 0) (table *print-pprint-dispatch*))
  (declare (type t type)
           (type (or null function symbol) function)
           (type real priority)
           (type pprint-dispatch-table table))
  (when (pprint-dispatch-table-read-only-p table)
    (cerror "Ignore and continue"
            "Tried to modify a read-only pprint dispatch table: ~A"
            table))
  ;; FIXME! This check should be automatically generated when compiling
  ;; with high enough safety mode.
  (unless (typep priority 'real)
    (error 'simple-type-error
	   :format-control "Not a valid priority for set-pprint-dispatch: ~A"
	   :format-arguments (list priority)
	   :expected-type 'real
	   :datum priority))
  (if function
      (if (cons-type-specifier-p type)
	  (setf (gethash (second (second type))
			 (pprint-dispatch-table-cons-entries table))
		(make-pprint-dispatch-entry :type type :priority priority
					    :function function))
	  (let ((list (delete type (pprint-dispatch-table-entries table)
			      :key #'pprint-dispatch-entry-type
			      :test #'equal))
		(entry (make-pprint-dispatch-entry
			:type type
			:priority priority :function function)))
	    (do ((prev nil next)
		 (next list (cdr next)))
		((null next)
		 (if prev
		     (setf (cdr prev) (list entry))
		     (setf list (list entry))))
	      (when (entry< (car next) entry)
		(if prev
		    (setf (cdr prev) (cons entry next))
		    (setf list (cons entry next)))
		(return)))
	    (setf (pprint-dispatch-table-entries table) list)))
      (if (cons-type-specifier-p type)
	  (remhash (second (second type))
		   (pprint-dispatch-table-cons-entries table))
	  (setf (pprint-dispatch-table-entries table)
		(delete type (pprint-dispatch-table-entries table)
			:key #'pprint-dispatch-entry-type
			:test #'equal))))
  nil)

;;; The guts of print-unreadable-object, inspired by SBCL. This is
;;; a redefinition of the function in iolib.lisp which add support
;;; for pprint-logical-block.
(defun %print-unreadable-object (object stream type identity body)
  (cond (*print-readably*
         (error 'print-not-readable :object object))
        ((and *print-pretty* (pretty-stream-p stream))
         (pprint-logical-block (stream nil :prefix "#<" :suffix ">")
           (print-unreadable-object-contents object stream type identity body)))
        (t
         (let ((stream (cond ((null stream)
                              *standard-output*)
                             ((eq t stream)
                              *terminal-io*)
                             (t
                              stream))))
           (write-string "#<" stream)
           (print-unreadable-object-contents object stream type identity body)
           (write-char #\> stream))))
  nil)
