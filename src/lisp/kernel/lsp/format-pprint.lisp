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

(defconstant default-line-length 100)

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
  (if (inravina-shim:pretty-stream-p stream)
      (pprint-tab :line-relative colrel colinc stream)
      (let* ((cur (stream-output-column stream))
             (spaces (if (and cur (plusp colinc))
                         (- (* (ceiling (+ cur colrel) colinc) colinc) cur)
                         colrel)))
        (output-spaces stream spaces))))

(defun format-absolute-tab (stream colnum colinc)
  (if (inravina-shim:pretty-stream-p stream)
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
                      :colonp t :atsignp nil :params nil
                      :virtual t))
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

(defmethod print-object ((obj structure-object) stream)
  (if *print-pretty*
      (inravina:pprint-structure-object inravina-shim:*client* stream obj)
      (clos::print-structure-object obj stream)))

#-clasp-min
(setf *print-pretty* t)
