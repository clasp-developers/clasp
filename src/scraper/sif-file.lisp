(in-package :cscrape)

;;; ---------------------------------------------------------------------------
;;; Writer.  Each record is emitted as a plain Common Lisp s-expression:
;;;     (TAG-CLASS :slot value :slot value ... :contains (...))
;;; The :contains slot of a kind-tag holds child tags recursively.
;;; ---------------------------------------------------------------------------

(defun tag->sexp (tag)
  "Convert a tag CLOS instance to a plain s-expression of the form
   (TAG-CLASS :initarg value ...).  If a slot's initarg is :contains and
   the value is a list of child tags, recursively convert those too."
  (let ((cls (type-of tag)))
    (cons cls
          (loop for slot in (closer-mop:class-slots (find-class cls))
                for initargs = (closer-mop:slot-definition-initargs slot)
                for name = (closer-mop:slot-definition-name slot)
                when (and initargs (slot-boundp tag name))
                  nconc (let ((value (slot-value tag name))
                              (initarg (first initargs)))
                          (list initarg
                                (if (and (eq initarg :contains) (listp value))
                                    (mapcar #'tag->sexp value)
                                    value)))))))

(defun write-sif-file (tags output)
  (let ((*package*        (find-package :tags))
        (*print-readably* t)
        (*print-pretty*   nil)
        (*print-case*     :downcase)
        (*print-circle*   nil)
        (*print-length*   nil)
        (*print-level*    nil))
    (ninja:with-timestamp-preserving-stream (stream output :external-format :utf-8)
      (loop for tag in tags
            do (prin1 (tag->sexp tag) stream)
               (terpri stream)))))

(defun generate-sif-file (input output)
  "* Arguments
- input :: A pathname.
- output :: A pathname.
* Description
Read the input .i file and extract the tags from it and write out a .sif file"
  (write-sif-file (process-all-recognition-elements (read-entire-file input))
                  output))

;;; ---------------------------------------------------------------------------
;;; Reader.  Plain s-expressions, dispatched by tag-class symbol.  Recursively
;;; rebuilds nested :contains lists into child tag instances.
;;; ---------------------------------------------------------------------------

(defun sexp->tag (sexp)
  "Convert (TAG-CLASS :initarg value ...) back into a tag CLOS instance.
   If the form has a :contains initarg whose value is a list of sub-forms,
   recursively reconstruct each."
  (unless (consp sexp)
    (error "Malformed tag form: ~S" sexp))
  (let* ((class-symbol (first sexp))
         (plist (rest sexp))
         (xformed (loop for (k v) on plist by #'cddr
                        nconc (list k
                                    (if (and (eq k :contains) (listp v))
                                        (mapcar #'sexp->tag v)
                                        v)))))
    (apply #'make-instance class-symbol xformed)))

(defun first-non-whitespace-char (path)
  "Return the first non-whitespace character of PATH, or NIL on empty file.
   Used to detect which .sif format we're reading."
  (with-open-file (s path :direction :input :external-format :utf-8)
    (loop for c = (read-char s nil nil)
          while c
          unless (member c '(#\Space #\Tab #\Newline #\Return))
            return c)))

(defun read-sif-file-legacy-brackets (sif-pathname)
  "Read a .sif file written in the legacy {tag :slot value ...} bracket
   syntax.  Each {} form is read as (apply #'make-instance ...).  Inner
   records (fixed-field, variable-*) appear as sibling top-level tags with
   no :contains nesting — the kind-tag's :contains slot defaults to nil
   and interpret-tags' state-cur-kind machinery handles the grouping."
  (let* ((*readtable* (copy-readtable))
         (*package*   (find-package :tags)))
    (set-macro-character
     #\{
     (lambda (stream char)
       (declare (ignore char))
       (apply #'make-instance (read-delimited-list #\} stream t))))
    (set-macro-character #\} (get-macro-character #\) nil))
    (with-open-file (stream sif-pathname :direction :input :external-format :utf-8)
      (loop for tag = (read stream nil stream)
            until (eq tag stream)
            unless tag
              do (error "Encountered a NIL tag in ~s" sif-pathname)
            collect tag))))

(defun read-sif-file (sif-file)
  "* Arguments
- sif-file :: Pathname or namestring.
* Description
Read a list of tags from the sif file.  Auto-detects the file format by
peeking at its first non-whitespace character:
  '(' — modern s-expression format with nested :contains.
  '{' — legacy bracket format (kept readable for transition convenience)."
  (let* ((sif-pathname (pathname sif-file))
         (first-char   (first-non-whitespace-char sif-pathname)))
    (cond
      ((null first-char) '())              ; empty file
      ((char= first-char #\{)              ; legacy bracketed
       (read-sif-file-legacy-brackets sif-pathname))
      (t                                   ; new s-expression
       (let ((*package* (find-package :tags)))
         (with-open-file (stream sif-pathname :direction :input :external-format :utf-8)
           (loop for form = (read stream nil :sif-eof)
                 until (eq form :sif-eof)
                 unless form
                   do (error "Encountered a NIL form in ~s" sif-pathname)
                 collect (sexp->tag form))))))))
