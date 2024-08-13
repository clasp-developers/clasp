;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;; -*- Package: FORMAT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Functions to implement FORMAT and FORMATTER for CMU Common Lisp.
;;;
;;; Written by William Lott, with lots of stuff stolen from the previous
;;; version by David Adam and later rewritten by Bill Maddox.
;;;
;;; Various fixes and adaptations provided by Juan Jose Garcia-Ripoll and
;;; Daniel Kochmański for Embeddable Common-Lisp.
;;; 

(in-package "SYS")

;;;; Format directive definition macros and runtime support.

(defmacro expander-pprint-next-arg (string offset)
  `(progn
     (when (null args)
       (error 'format-error
              :complaint "No more arguments."
              :control-string ,string
              :offset ,offset))
     (pprint-pop)
     (pop args)))

;;;; Tab and simple pretty-printing noise.

(def-format-directive #\T (colonp atsignp params)
  (cond (colonp
         (check-output-layout-mode 1)
         (expand-bind-defaults ((n 1) (m 1)) params
                               `(pprint-tab ,(if atsignp :section-relative :section)
                                            ,n ,m stream)))
        (atsignp
         (expand-bind-defaults ((colrel 1) (colinc 1)) params
                               `(format-relative-tab stream ,colrel ,colinc)))
        (t
         (expand-bind-defaults ((colnum 1) (colinc 1)) params
                               `(format-absolute-tab stream ,colnum ,colinc)))))

(def-format-interpreter #\T (colonp atsignp params)
  (cond (colonp
         (check-output-layout-mode 1)
         (interpret-bind-defaults ((n 1) (m 1)) params
                                  (pprint-tab (if atsignp :section-relative :section) n m stream)))
        (atsignp
         (interpret-bind-defaults ((colrel 1) (colinc 1)) params
                                  (format-relative-tab stream colrel colinc)))
        (t
         (interpret-bind-defaults ((colnum 1) (colinc 1)) params
                                  (format-absolute-tab stream colnum colinc)))))

(defun output-spaces (stream n)
  (let ((spaces #.(make-string 100 :initial-element #\space)))
    (loop
      (when (< n (length spaces))
        (return))
      (write-string spaces stream)
      (decf n (length spaces)))
    (write-string spaces stream :end n)))

(defun format-relative-tab (stream colrel colinc)
  (if (#-(or ecl clasp) pp:pretty-stream-p #+(or ecl clasp) sys::pretty-stream-p stream)
      (pprint-tab :line-relative colrel colinc stream)
      (let* ((cur (stream-output-column stream))
             (spaces (if (and cur (plusp colinc))
                         (- (* (ceiling (+ cur colrel) colinc) colinc) cur)
                         colrel)))
        (output-spaces stream spaces))))

(defun format-absolute-tab (stream colnum colinc)
  (if (#-(or ecl clasp) pp:pretty-stream-p #+(or ecl clasp) sys::pretty-stream-p stream)
      (pprint-tab :line colnum colinc stream)
      (let ((cur (stream-output-column stream)))
        (cond ((null cur)
               (write-string "  " stream))
              ((< cur colnum)
               (output-spaces stream (- colnum cur)))
              (t
               (unless (zerop colinc)
                 (output-spaces stream
                                (- colinc (rem (- cur colnum) colinc)))))))))

(def-format-directive #\_ (colonp atsignp params)
  (check-output-layout-mode 1)
  (expand-bind-defaults () params
    `(pprint-newline ,(if colonp
                          (if atsignp
                              :mandatory
                              :fill)
                          (if atsignp
                              :miser
                              :linear))
                     stream)))

(def-format-interpreter #\_ (colonp atsignp params)
  (check-output-layout-mode 1)
  (interpret-bind-defaults () params
    (pprint-newline (if colonp
                        (if atsignp
                            :mandatory
                            :fill)
                        (if atsignp
                            :miser
                            :linear))
                    stream)))

(def-format-directive #\I (colonp atsignp params)
  (check-output-layout-mode 1)
  (when atsignp
    (error 'format-error
           :complaint "Cannot specify the at-sign modifier."))
  (expand-bind-defaults ((n 0)) params
    `(pprint-indent ,(if colonp :current :block) ,n stream)))

(def-format-interpreter #\I (colonp atsignp params)
  (check-output-layout-mode 1)
  (when atsignp
    (error 'format-error
           :complaint "Cannot specify the at-sign modifier."))
  (interpret-bind-defaults ((n 0)) params
    (pprint-indent (if colonp :current :block) n stream)))

;;;; Justification.

(defparameter *illegal-inside-justification*
  (mapcar (lambda (x) (parse-directive x 0))
          '("~W" "~:W" "~@W" "~:@W"
            "~_" "~:_" "~@_" "~:@_"
            "~:>" "~:@>"
            "~I" "~:I" "~@I" "~:@I"
            "~:T" "~:@T")))

(defun check-output-layout-mode (mode)
  (when (and *output-layout-mode*
             (not (eql *output-layout-mode* mode)))
    (error 'format-error
           :complaint "Cannot mix ~~W, ~~_, ~~<...~~:>, ~~I, or ~~T with ~~<...~~:;...~~>"))
  (setf *output-layout-mode* mode))

(defun illegal-inside-justification-p (directive)
  (member directive *illegal-inside-justification*
          :test (lambda (x y)
                  (and (format-directive-p x)
                       (format-directive-p y)
                       (eql (format-directive-character x) (format-directive-character y))
                       (eql (format-directive-colonp x) (format-directive-colonp y))
                       (eql (format-directive-atsignp x) (format-directive-atsignp y))))))

(def-complex-format-directive #\< (colonp atsignp params string end directives)
  (multiple-value-bind
      (segments first-semi close remaining)
      (parse-format-justification directives)
    (values
     (if (format-directive-colonp close)
         (multiple-value-bind
             (prefix per-line-p insides suffix)
             (parse-format-logical-block segments colonp first-semi
                                         close params string end)
           (expand-format-logical-block prefix per-line-p insides
                                        suffix atsignp))
         (let ((count (reduce #'+ (mapcar (lambda (x)
                                            (count-if #'illegal-inside-justification-p x))
                                          segments))))
           (when (> count 0)
             ;; ANSI specifies that "an error is signalled" in this
             ;; situation.
             (error 'format-error
                    :complaint "~D illegal directive~:P found inside justification block"
                    :arguments (list count)))
           (expand-format-justification segments colonp atsignp
                                      first-semi params)))
     remaining)))

(def-complex-format-interpreter #\<
                                (colonp atsignp params string end directives)
  (multiple-value-bind
      (segments first-semi close remaining)
      (parse-format-justification directives)
    (setf args
          (if (format-directive-colonp close)
              (multiple-value-bind
                  (prefix per-line-p insides suffix)
                  (parse-format-logical-block segments colonp first-semi
                                              close params string end)
                (interpret-format-logical-block stream orig-args args
                                                prefix per-line-p insides
                                                suffix atsignp))
              (let ((count (reduce #'+ (mapcar (lambda (x)
                                                 (count-if #'illegal-inside-justification-p x))
                                               segments))))
                (when (> count 0)
                  ;; ANSI specifies that "an error is signalled" in this
                  ;; situation.
                  (error 'format-error
                         :complaint "~D illegal directive~:P found inside justification block"
                         :arguments (list count)))
                (interpret-format-justification stream orig-args args
                                                segments colonp atsignp
                                                first-semi params))))
    remaining))

(defun parse-format-justification (directives)
  (let ((first-semi nil)
        (close nil)
        (remaining directives))
    (collect ((segments))
      (loop
        (let ((close-or-semi (find-directive remaining #\> t)))
          (unless close-or-semi
            (error 'format-error
                   :complaint "No corresponding close bracket."))
          (let ((posn (position close-or-semi remaining)))
            (segments (subseq remaining 0 posn))
            (setf remaining (nthcdr (1+ posn) remaining)))
          (when (char= (format-directive-character close-or-semi)
                       #\>)
            (setf close close-or-semi)
            (return))
          (unless first-semi
            (setf first-semi close-or-semi))))
      (values (segments) first-semi close remaining))))

(defun expand-format-justification (segments colonp atsignp first-semi params)
  (let ((newline-segment-p
         (and first-semi
              (format-directive-colonp first-semi))))
    (when newline-segment-p
      (check-output-layout-mode 2))
    (expand-bind-defaults
        ((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
        params
      `(let ((segments nil)
             ,@(when newline-segment-p
                 '((newline-segment nil)
                   (extra-space 0)
                   (line-len 72))))
         (block nil
           ,@(when newline-segment-p
               `((setf newline-segment
                       (with-output-to-string (stream)
                         ,@(expand-directive-list (pop segments))))
                 ,(expand-bind-defaults
                      ((extra 0)
                       (line-len '(or *print-right-margin*
                                      (gray:stream-line-length stream)
                                      default-line-length)))
                      (format-directive-params first-semi)
                    `(setf extra-space ,extra line-len ,line-len))))
           ,@(mapcar #'(lambda (segment)
                         `(push (with-output-to-string (stream)
                                  ,@(expand-directive-list segment))
                                segments))
                     segments))
         (format-justification stream
                               ,@(if newline-segment-p
                                     '(newline-segment extra-space line-len)
                                     '(nil 0 0))
                               segments ,colonp ,atsignp
                               ,mincol ,colinc ,minpad ,padchar)))))

(defun interpret-format-justification
       (stream orig-args args segments colonp atsignp first-semi params)
  (interpret-bind-defaults
      ((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
      params
    (let ((newline-string nil)
          (strings nil)
          (extra-space 0)
          (line-len 0))
      (setf args
            (catch 'up-and-out
              (when (and first-semi (format-directive-colonp first-semi))
                (check-output-layout-mode 2)
                (interpret-bind-defaults
                    ((extra 0)
                     (len (or *print-right-margin*
                              (gray:stream-line-length stream)
                              default-line-length)))
                    (format-directive-params first-semi)
                  (setf newline-string
                        (with-output-to-string (stream)
                          (setf args
                                (interpret-directive-list stream
                                                          (pop segments)
                                                          orig-args
                                                          args))))
                  (setf extra-space extra)
                  (setf line-len len)))
              (dolist (segment segments)
                (push (with-output-to-string (stream)
                        (setf args
                              (interpret-directive-list stream segment
                                                        orig-args args)))
                      strings))
              args))
      (format-justification stream newline-string extra-space line-len strings
                            colonp atsignp mincol colinc minpad padchar)))
  args)

(defun format-justification (stream newline-prefix extra-space line-len strings
                             pad-left pad-right mincol colinc minpad padchar)
  (setf strings (reverse strings))
  (when (and (not pad-left) (not pad-right) (null (cdr strings)))
    (setf pad-left t))
  (let* ((num-gaps (1- (length strings)))
         (chars (+ (* num-gaps minpad)
                   (loop for string in strings summing (length string))))
         (length (if (> chars mincol)
                     (+ mincol (* (ceiling (- chars mincol) colinc) colinc))
                     mincol))
         (padding (- length chars)))
    (when (and newline-prefix
               (> (+ (or (stream-output-column stream) 0)
                     length extra-space)
                  line-len))
      (write-string newline-prefix stream))
    (when pad-left
      (incf num-gaps))
    (when pad-right
      (incf num-gaps))
    (when (zerop num-gaps)
      (incf num-gaps)
      (setf pad-left t))
    (flet ((do-padding (border)
             (let ((pad-len (truncate padding num-gaps)))
               (decf padding pad-len)
               (decf num-gaps)
               (unless border
                 (incf pad-len minpad))
               (dotimes (i pad-len) (write-char padchar stream)))))
      (when pad-left
        (do-padding t))
      (when strings
        (write-string (car strings) stream)
        (dolist (string (cdr strings))
          (do-padding nil)
          (write-string string stream)))
      (when pad-right
        (do-padding t)))))

(defun parse-format-logical-block
       (segments colonp first-semi close params string end)
  (check-output-layout-mode 1)
  (when params
    (error 'format-error
           :complaint "No parameters can be supplied with ~~<...~~:>."
           :offset (caar params)))
  (multiple-value-bind
      (prefix insides suffix)
      (multiple-value-bind (prefix-default suffix-default)
                           (if colonp (values "(" ")") (values "" ""))
        (flet ((extract-string (list prefix-p)
                 (let ((directive (find-if #'format-directive-p list)))
                   (if directive
                       (error 'format-error
                              :complaint
                              "Cannot include format directives inside the ~
                               ~:[suffix~;prefix~] segment of ~~<...~~:>"
                              :arguments (list prefix-p)
                              :offset (1- (format-directive-end directive)))
                       (apply #'concatenate 'string list)))))
        (case (length segments)
          (0 (values prefix-default nil suffix-default))
          (1 (values prefix-default (car segments) suffix-default))
          (2 (values (extract-string (car segments) t)
                     (cadr segments) suffix-default))
          (3 (values (extract-string (car segments) t)
                     (cadr segments)
                     (extract-string (caddr segments) nil)))
          (t
           (error 'format-error
                  :complaint "Too many segments for ~~<...~~:>.")))))
    (when (format-directive-atsignp close)
      (setf insides
            (add-fill-style-newlines insides
                                     string
                                     (if first-semi
                                         (format-directive-end first-semi)
                                         end))))
    (values prefix
            (and first-semi (format-directive-atsignp first-semi))
            insides
            suffix)))

(defun add-fill-style-newlines (list string offset &optional last-directive)
  (cond
    (list
     (let ((directive (car list)))
       (cond
         ((simple-string-p directive)
          (let* ((non-space (position #\Space directive :test #'char/=))
                 (newlinep (and last-directive
                                (char= (format-directive-character last-directive)
                                       #\Newline))))
            (cond
              ((and newlinep non-space)
               (nconc
                (list (subseq directive 0 non-space))
                (add-fill-style-newlines-aux
                 (subseq directive non-space) string (+ offset non-space))
                (add-fill-style-newlines
                 (cdr list) string (+ offset (length directive)))))
              (newlinep
               (cons directive
                     (add-fill-style-newlines
                      (cdr list) string (+ offset (length directive)))))
              (t
               (nconc (add-fill-style-newlines-aux directive string offset)
                      (add-fill-style-newlines
                       (cdr list) string (+ offset (length directive))))))))
         (t
          (cons directive
                (add-fill-style-newlines
                 (cdr list) string
                 (format-directive-end directive) directive))))))
    (t nil)))

(defun add-fill-style-newlines-aux (literal string offset)
  (let ((end (length literal))
        (posn 0))
    (collect ((results))
      (loop
        (let ((blank (position #\space literal :start posn)))
          (when (null blank)
            (results (subseq literal posn))
            (return))
          (let ((non-blank (or (position #\space literal :start blank
                                         :test #'char/=)
                               end)))
            (results (subseq literal posn non-blank))
            (results (make-format-directive
                      :string string :character #\_
                      :start (+ offset non-blank) :end (+ offset non-blank)
                      :colonp t :atsignp nil :params nil))
            (setf posn non-blank))
          (when (= posn end)
            (return))))
      (results))))

(defun expand-format-logical-block (prefix per-line-p insides suffix atsignp)
  `(let ((arg ,(if atsignp 'args (expand-next-arg))))
     ,@(when atsignp
         (setf *only-simple-args* nil)
         '((setf args nil)))
     (pprint-logical-block
         (stream arg
                 ,(if per-line-p :per-line-prefix :prefix) ,prefix
                 :suffix ,suffix)
       (let ((args arg)
             ,@(unless atsignp
                 `((orig-args arg))))
         (declare (ignorable args ,@(unless atsignp '(orig-args))))
         (block nil
           ,@(let ((*expander-next-arg-macro* 'expander-pprint-next-arg)
                   (*only-simple-args* nil)
                   (*orig-args-available* (if atsignp *orig-args-available* t)))
               (expand-directive-list insides)))))))

(defun interpret-format-logical-block
       (stream orig-args args prefix per-line-p insides suffix atsignp)
  (let ((arg (if atsignp args (next-arg))))
    (if per-line-p
        (pprint-logical-block
            (stream arg :per-line-prefix prefix :suffix suffix)
          (let ((*logical-block-popper* #'(lambda () (pprint-pop))))
            (catch 'up-and-out
              (interpret-directive-list stream insides
                                        (if atsignp orig-args arg)
                                        arg))))
        (pprint-logical-block (stream arg :prefix prefix :suffix suffix)
          (let ((*logical-block-popper* #'(lambda () (pprint-pop))))
            (catch 'up-and-out
              (interpret-directive-list stream insides
                                        (if atsignp orig-args arg)
                                        arg))))))
  (if atsignp nil args))

(def-complex-format-directive #\> ()
  (error 'format-error
         :complaint "No corresponding open bracket."))

;;;; Standard pretty-printing routines.

(defun pprint-array (stream array)
  (cond ((or (and (null *print-array*) (null *print-readably*))
	     (stringp array)
	     (bit-vector-p array))
	 (write-ugly-object array stream))
	(*print-readably*
	 (pprint-raw-array stream array))
	((vectorp array)
	 (pprint-vector stream array))
	(t
	 (pprint-multi-dim-array stream array))))

(defun pprint-vector (stream vector)
  (write-object-with-circle
   vector stream
   #'(lambda (vector stream)
       (pprint-logical-block (stream nil :prefix "#(" :suffix ")")
         (dotimes (i (length vector))
           (unless (zerop i)
             (write-char #\space stream)
             (pprint-newline :fill stream))
           (pprint-pop)
           (write-object (aref vector i) stream))))))

(defun pprint-array-contents (stream array)
  (declare (array array))
  (labels ((output-guts (stream index dimensions)
	       (if (null dimensions)
		   (write-object (row-major-aref array index) stream)
		   (pprint-logical-block
		    (stream nil :prefix "(" :suffix ")")
		    (let ((dim (car dimensions)))
		      (unless (zerop dim)
			(let* ((dims (cdr dimensions))
			       (index index)
			       (step (reduce #'* dims))
			       (count 0))
			  (loop				
			   (pprint-pop)
			   (output-guts stream index dims)
			   (when (= (incf count) dim)
			     (return))
			   (write-char #\space stream)
			   (pprint-newline (if dims :linear :fill)
					   stream)
			   (incf index step)))))))))
    (output-guts stream 0 (array-dimensions array))))

(defun pprint-multi-dim-array (stream array)
  (write-object-with-circle
   array stream
   #'(lambda (array stream)
  (funcall (formatter "#~DA") stream (array-rank array))
  (pprint-array-contents stream array))))

(defun pprint-raw-array (stream array)
  (write-object-with-circle
   array stream
   #'(lambda (array stream)
       (pprint-logical-block (stream nil :prefix "#A(" :suffix ")")
    (write-object (array-element-type array) stream)
    (write-char #\Space stream)
    (pprint-newline :fill stream)
    (write-object (array-dimensions array) stream)
    (write-char #\Space stream)
    (pprint-newline :fill stream)
         (pprint-array-contents stream array)))))

(defun pprint-lambda-list (stream lambda-list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream lambda-list :prefix "(" :suffix ")")
    (let ((state :required)
	  (first t))
      (loop
	(pprint-exit-if-list-exhausted)
	(unless first
	  (write-char #\space stream))
	(let ((arg (pprint-pop)))
	  (unless first
	    (case arg
	      (&optional
	       (setf state :optional)
	       (pprint-newline :linear stream))
	      ((&rest &body)
	       (setf state :required)
	       (pprint-newline :linear stream))
	      (&key
	       (setf state :key)
	       (pprint-newline :linear stream))
	      (&aux
	       (setf state :optional)
	       (pprint-newline :linear stream))
	      (t
	       (pprint-newline :fill stream))))
	  (ecase state
	    (:required
	     (pprint-lambda-list stream arg))
	    ((:optional :key)
	     (pprint-logical-block
		 (stream arg :prefix "(" :suffix ")")
	       (pprint-exit-if-list-exhausted)
	       (if (eq state :key)
		   (pprint-logical-block
		       (stream (pprint-pop) :prefix "(" :suffix ")")
		     (pprint-exit-if-list-exhausted)
		     (write-object (pprint-pop) stream)
		     (pprint-exit-if-list-exhausted)
		     (write-char #\space stream)
		     (pprint-newline :fill stream)
		     (pprint-lambda-list stream (pprint-pop))
		     (loop
		       (pprint-exit-if-list-exhausted)
		       (write-char #\space stream)
		       (pprint-newline :fill stream)
		       (write-object (pprint-pop) stream)))
		   (pprint-lambda-list stream (pprint-pop)))
	       (loop
		 (pprint-exit-if-list-exhausted)
		 (write-char #\space stream)
		 (pprint-newline :linear stream)
		 (write-object (pprint-pop) stream))))))
	(setf first nil)))))

(defun pprint-lambda (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter
	    "~:<~^~W~^~3I ~:_~/SI:PPRINT-LAMBDA-LIST/~1I~@{ ~_~W~}~:>")
	   stream list))

(defun pprint-block (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~^~W~^~3I ~:_~W~1I~@{ ~_~W~}~:>") stream list))

(defun pprint-flet (stream list &rest noise)
  (declare (ignore noise))
  (if (and (consp list)
           (consp (cdr list))
           (not (null (cddr list))))
      (funcall (formatter
	        "~:<~^~W~^ ~@_~:<~@{~:<~^~W~^~3I ~:_~/SI:PPRINT-LAMBDA-LIST/~1I~:@_~@{~W~^ ~_~}~:>~^ ~_~}~:>~1I~@:_~@{~W~^ ~_~}~:>")
	       stream
	       list)
      ;; Things like (labels foo) function names.
      (pprint-logical-block (stream list :prefix "(" :suffix ")")
        (pprint-exit-if-list-exhausted)
        (write (pprint-pop) :stream stream)
        (loop (pprint-exit-if-list-exhausted)
              (write-char #\Space stream)
              (write (pprint-pop) :stream stream)))))

(defun pprint-let (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~^~W~^ ~@_~:<~@{~:<~^~W~@{ ~_~W~}~:>~^ ~_~}~:>~1I~:@_~@{~W~^ ~_~}~:>")
	   stream
	   list))

(defun pprint-progn (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~^~W~@{ ~_~W~}~:>") stream list))

(defun pprint-progv (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~^~W~^~3I ~_~W~^ ~_~W~^~1I~@{ ~_~W~}~:>")
	   stream list))

(defun pprint-quote (stream list &rest noise)
  (declare (ignore noise))
  (cond ((or (not (consp list))
             (not (consp (cdr list)))
             (cddr list))
	 (pprint-fill stream list))
        ((eq (car list) 'function)
	 (write-string "#'" stream)
	 (write-object (cadr list) stream))
	((eq (car list) 'quote)
	 (write-char #\' stream)
	 (write-object (cadr list) stream))
        ((eq (car list) 'core:quasiquote)
         (let ((core:*quasiquote* (list* (target-stream stream) t
                                         core:*quasiquote*)))
           (write-char #\` stream)
           (write-object (cadr list) stream)))
        ((not (getf core:*quasiquote* (target-stream stream)))
         (pprint-fill stream list))
        ((eq (car list) 'core:unquote)
         (let ((core:*quasiquote* (list* (target-stream stream) nil
                                         core:*quasiquote*)))
           (write-char #\, stream)
           (write-object (cadr list) stream)))
        ((eq (car list) 'core:unquote-splice)
         (let ((core:*quasiquote* (list* (target-stream stream) nil
                                         core:*quasiquote*)))
           (write-string ",@" stream)
           (write-object (cadr list) stream)))
        ((eq (car list) 'core:unquote-nsplice)
         (let ((core:*quasiquote* (list* (target-stream stream) nil
                                         core:*quasiquote*)))
           (write-string ",." stream)
           (write-object (cadr list) stream)))
	(t
	 (pprint-fill stream list))))

(defun pprint-setq (stream list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    (write-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    (pprint-newline :miser stream)
    (if (and (consp (cdr list)) (consp (cddr list)))
	(loop
	  (pprint-indent :current 2 stream)
	  (write-object (pprint-pop) stream)
	  (pprint-exit-if-list-exhausted)
	  (write-char #\space stream)
	  (pprint-newline :linear stream)
	  (pprint-indent :current -2 stream)
	  (write-object (pprint-pop) stream)
	  (pprint-exit-if-list-exhausted)
	  (write-char #\space stream)
	  (pprint-newline :linear stream))
	(progn
	  (pprint-indent :current 0 stream)
	  (write-object (pprint-pop) stream)
	  (pprint-exit-if-list-exhausted)
	  (write-char #\space stream)
	  (pprint-newline :linear stream)
	  (write-object (pprint-pop) stream)))))

;;#+clasp-min
(defmacro pprint-tagbody-guts (stream)
  `(loop
     (pprint-exit-if-list-exhausted)
     (write-char #\space ,stream)
     (let ((form-or-tag (pprint-pop)))
       (pprint-indent :block 
		      (if (atom form-or-tag) 0 1)
		      ,stream)
       (pprint-newline :linear ,stream)
       (write-object form-or-tag ,stream))))

(defun pprint-tagbody (stream list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    (write-object (pprint-pop) stream)
    (pprint-tagbody-guts stream)))

(defun pprint-case (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter
	    "~:<~^~W~^ ~3I~:_~W~1I~@{ ~_~:<~^~:/SI:PPRINT-FILL/~^~@{ ~_~W~}~:>~}~:>")
	   stream
	   list))

(defun pprint-defun (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter
	    "~:<~^~W~^ ~@_~:I~W~^ ~:_~/SI:PPRINT-LAMBDA-LIST/~1I~@{ ~_~W~}~:>")
	   stream
	   list))

(defun pprint-destructuring-bind (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter
	    "~:<~^~W~^~3I ~_~:/SI:PPRINT-LAMBDA-LIST/~^ ~_~W~^~1I~@{ ~_~W~}~:>")
	   stream list))

(defun pprint-do (stream list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    (write-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    (pprint-indent :current 0 stream)
    (funcall (formatter "~:<~@{~:<~^~W~^ ~@_~:I~W~@{ ~_~W~}~:>~^~:@_~}~:>")
	     stream
	     (pprint-pop))
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    (pprint-newline :linear stream)
    (pprint-linear stream (pprint-pop))
    (pprint-tagbody-guts stream)))

(defun pprint-dolist (stream list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    (write-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (pprint-indent :block 3 stream)
    (write-char #\space stream)
    (pprint-newline :fill stream)
    (funcall (formatter "~:<~^~W~^ ~:_~:I~W~@{ ~_~W~}~:>")
	     stream
	     (pprint-pop))
    (pprint-tagbody-guts stream)))

(defun pprint-typecase (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter
	    "~:<~^~W~^ ~3I~:_~W~1I~@{ ~_~:<~^~W~^~@{ ~_~W~}~:>~}~:>")
	   stream
	   list))

(defun pprint-prog (stream list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    (write-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    (pprint-newline :miser stream)
    (pprint-fill stream (pprint-pop))
    (pprint-tagbody-guts stream)))

(defun pprint-function-call (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~^~W~^ ~:_~:I~@{~W~^ ~_~}~:>")
	   stream
	   list))


;;;; Interface seen by regular (ugly) printer and initialization routines.

(eval-when (:compile-toplevel :execute)
(defparameter +magic-forms+
  '((lambda pprint-lambda)
    ;; Special forms.
    (block pprint-block)
    (catch pprint-block)
    (compiler-let pprint-let)
    (eval-when pprint-block)
    (flet pprint-flet)
    (function pprint-quote)
    (labels pprint-flet)
    (let pprint-let)
    (let* pprint-let)
    (locally pprint-progn)
    (macrolet pprint-flet)
    (multiple-value-call pprint-block)
    (multiple-value-prog1 pprint-block)
    (progn pprint-progn)
    (progv pprint-progv)
    (quote pprint-quote)
    (return-from pprint-block)
    (setq pprint-setq)
    (symbol-macrolet pprint-let)
    (tagbody pprint-tagbody)
    (throw pprint-block)
    (unwind-protect pprint-block)
    (core:quasiquote pprint-quote)
    (core:unquote pprint-quote)
    (core:unquote-splice pprint-quote)
    (core:unquote-nsplice pprint-quote)
    
    ;; Macros.
    (case pprint-case)
    (ccase pprint-case)
    (ctypecase pprint-typecase)
    (defconstant pprint-block)
    (define-modify-macro pprint-defun)
    (define-setf-expander pprint-defun)
    (defmacro pprint-defun)
    (defparameter pprint-block)
    (defsetf pprint-defun)
    (defstruct pprint-block)
    (deftype pprint-defun)
    (defun pprint-defun)
    (defvar pprint-block)
    (destructuring-bind pprint-destructuring-bind)
    (do pprint-do)
    (do* pprint-do)
    (do-all-symbols pprint-dolist)
    (do-external-symbols pprint-dolist)
    (do-symbols pprint-dolist)
    (dolist pprint-dolist)
    (dotimes pprint-dolist)
    (ecase pprint-case)
    (etypecase pprint-typecase)
    #+nil (handler-bind ...)
    #+nil (handler-case ...)
    #+nil (loop ...)
    (multiple-value-bind pprint-progv)
    (multiple-value-setq pprint-block)
    (pprint-logical-block pprint-block)
    (print-unreadable-object pprint-block)
    (prog pprint-prog)
    (prog* pprint-prog)
    (prog1 pprint-block)
    (prog2 pprint-progv)
    (psetf pprint-setq)
    (psetq pprint-setq)
    #+nil (restart-bind ...)
    #+nil (restart-case ...)
    (setf pprint-setq)
    (step pprint-progn)
    (time pprint-progn)
    (typecase pprint-typecase)
    (unless pprint-block)
    (when pprint-block)
    (with-compilation-unit pprint-block)
    #+nil (with-condition-restarts ...)
    (with-hash-table-iterator pprint-block)
    (with-input-from-string pprint-block)
    (with-open-file pprint-block)
    (with-open-stream pprint-block)
    (with-output-to-string pprint-block)
    (with-package-iterator pprint-block)
    (with-simple-restart pprint-block)
    (with-standard-io-syntax pprint-progn))))

(progn
  (let ((*print-pprint-dispatch* (make-pprint-dispatch-table)))
    ;; Printers for regular types.
    (set-pprint-dispatch 'array #'pprint-array)
    (set-pprint-dispatch '(cons (and symbol (satisfies fboundp)))
			 #'pprint-function-call -1)
    (set-pprint-dispatch 'cons #'pprint-fill -2)
    ;; Cons cells with interesting things for the car.
    (dolist (magic-form '#.+magic-forms+)
      (set-pprint-dispatch `(cons (eql ,(first magic-form)))
			   (symbol-function (second magic-form))))
    (setf *initial-pprint-dispatch* *print-pprint-dispatch*)
    )
  (setf *print-pprint-dispatch* (copy-pprint-dispatch nil)
        *standard-pprint-dispatch* *initial-pprint-dispatch*)
  (setf (pprint-dispatch-table-read-only-p *standard-pprint-dispatch*) t)
  (setf (first (cdr si::+io-syntax-progv-list+)) *standard-pprint-dispatch*)
  #-clasp-min
  (setf *print-pretty* t))
