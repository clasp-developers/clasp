
(defvar *documentation-pool* (list (make-hash-table :test #'equal :size 128) "SYS_help.doc"))
(defparameter *keep-documentation* t)


(defun record-cons (record key sub-key)
  (let ((cons (cons key sub-key)))
    (dolist (i record i)
      (when (equalp (car i) cons)
        (return i)))))

(defun record-field (record key sub-key)
  (cdr (record-cons record key sub-key)))

(defun set-record-field (record key sub-key value)
  (let ((field (record-cons record key sub-key)))
    (if field
        (rplacd field value)
        (setq record (list* (cons (cons key sub-key) value) record)))
    record))


(defun rem-record-field (record key sub-key)
  (let ((x (record-cons record key sub-key)))
    (if x
        (let ((output '()))
          (dolist (i record output)
            (when (not (eq i x))
              (setq output (cons i output)))))
        record)))

(defun annotate (object key sub-key value)
  (let ((dict (first *documentation-pool*)))
    (when (hash-table-p dict)
      (let ((record (set-record-field (gethash object dict)
                                      key sub-key value)))
        (setf (gethash object dict) record)))))

(defun remove-annotation (object key sub-key)
  (let ((dict (first *documentation-pool*)))
    (when (hash-table-p dict)
      (let ((record (rem-record-field (gethash object dict)
                                      key sub-key)))
	(if record
            (setf (gethash object dict) record)
            (remhash object dict))))))

(defun get-annotation (object key &optional (sub-key :all))
  (let ((output '()))
    (dolist (dict *documentation-pool* output)
      (let ((record (if (hash-table-p dict)
                        (gethash object dict)
                        (if (stringp dict)
                            (search-help-file object dict)
                            nil))))
        (when record
          (if (eq sub-key :all)
              (dolist (i record)
                (let ((key-sub-key (car i)))
                  (when (equal (car key-sub-key) key)
                    (push (cons (cdr key-sub-key) (cdr i)) output))))
              (if (setq output (record-field record key sub-key))
                  (return output))))))))

(defun dump-documentation (file &optional (merge nil))
  "Args: (filespec &optional (merge nil))
Saves the current hash table for documentation strings to the specificed file.
If MERGE is true, merges the contents of this table with the original values in
the help file."
  (let ((dict (first *documentation-pool*)))
    (when (hash-table-p dict)
      (dump-help-file dict file merge)
      (rplaca *documentation-pool* file))))

(defun get-documentation (object doc-type)
  (when (functionp object)
    (when (null (setq object (compiled-function-name object)))
      (return-from get-documentation nil)))
  (if (and object (listp object) (valid-function-name-p object))
      (get-annotation (second object) 'setf-documentation doc-type)
      (get-annotation object 'documentation doc-type)))

(defun set-documentation (object doc-type string)
  (when (not (or (stringp string) (null string)))
    (error "~S is not a valid documentation string" string))
  (let ((key 'documentation))
    (when (and object (listp object) (valid-function-name-p object))
      (setq object (second object) key 'setf-documentation))
    (if string
        (annotate object key doc-type string)
        (remove-annotation object key doc-type)))
  string)


(defparameter dict (first *documentation-pool*))
(set-documentation 'hello 'function "hello function doc")
(set-documentation 'hello 'setf-function "hello setf-function doc")
(gethash 'hello dict)
