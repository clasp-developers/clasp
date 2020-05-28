(in-package :ext)

;;; file is loaded first, than compiled
(defvar *encoding-data* nil)

(defvar *encoding-cache* (make-hash-table))

(defun enconding-strong-to-encoding-symbol (encoding-string)
  (cond ((STRING= "DOS-CP857" :DOS-CP857))
        ((STRING= "ISO-8859-9" :ISO-8859-9)) 
        ((STRING= "ISO-8859-8" :ISO-8859-8)) 
        ((STRING= "DOS-CP852" :DOS-CP852)) 
        ((STRING= "WINDOWS-CP949" :WINDOWS-CP949)) 
        ((STRING= "WINDOWS-CP932" :WINDOWS-CP932)) 
        ((STRING= "WINDOWS-CP1253" :WINDOWS-CP1253)) 
        ((STRING= "ISO-8859-14" :ISO-8859-14)) 
        ((STRING= "WINDOWS-CP950" :WINDOWS-CP950)) 
        ((STRING= "DOS-CP863" :DOS-CP863)) 
        ((STRING= "WINDOWS-CP1250" :WINDOWS-CP1250)) 
        ((STRING= "ISO-8859-3" :ISO-8859-3)) 
        ((STRING= "WINDOWS-CP1258" :WINDOWS-CP1258)) 
        ((STRING= "DOS-CP862" :DOS-CP862)) 
        ((STRING= "ISO-8859-2" :ISO-8859-2)) 
        ((STRING= "KOI8-R" :KOI8-R)) 
        ((STRING= "ISO-8859-6" :ISO-8859-6)) 
        ((STRING= "WINDOWS-CP1252" :WINDOWS-CP1252)) 
        ((STRING= "DOS-CP860" :DOS-CP860)) 
        ((STRING= "DOS-CP864" :DOS-CP864)) 
        ((STRING= "ISO-8859-5" :ISO-8859-5)) 
        ((STRING= "ISO-8859-4" :ISO-8859-4)) 
        ((STRING= "DOS-CP850" :DOS-CP850)) 
        ((STRING= "DOS-CP855" :DOS-CP855)) 
        ((STRING= "WINDOWS-CP1257" :WINDOWS-CP1257)) 
        ((STRING= "DOS-CP869" :DOS-CP869)) 
        ((STRING= "DOS-CP866" :DOS-CP866)) 
        ((STRING= "WINDOWS-CP1256" :WINDOWS-CP1256)) 
        ((STRING= "WINDOWS-CP1251" :WINDOWS-CP1251)) 
        ((STRING= "ISO-8859-10" :ISO-8859-10)) 
        ((STRING= "ISO-8859-13" :ISO-8859-13)) 
        ((STRING= "DOS-CP861" :DOS-CP861)) 
        ((STRING= "DOS-CP865" :DOS-CP865)) 
        ((STRING= "ISO-8859-7" :ISO-8859-7)) 
        ((STRING= "WINDOWS-CP1255" :WINDOWS-CP1255)) 
        ((STRING= "WINDOWS-CP936" :WINDOWS-CP936)) 
        ((STRING= "DOS-CP437" :DOS-CP437)) 
        ((STRING= "ISO-8859-15" :ISO-8859-15))
        (T :unknown-encoding)))  

(defun generate-encoding-hashtable (encoding)
  (let ((hash (gethash encoding *encoding-cache*)))
    (if hash
        hash
        (let ((spec (assoc encoding *encoding-data*)))
          (when spec
            (let ((table (make-hash-table)))
              (dolist (pair (rest spec))
                (let ((key (first pair))
                      (value (rest pair)))
                  (setf (gethash key table) value)
                  (setf (gethash value table) key)))
              (setf (gethash encoding *encoding-cache*) table)
              table))))))

;;; format per line :ISO-8859-2;0;0;
(defun process-encodings-file ()
  (setq *encoding-data* nil)
  (let ((file (translate-logical-pathname #P"SOURCE-DIR:tools-for-build;encodingdata.txt"))
        (old-encoding nil)
        (alist nil))
    (with-open-file (stream file :element-type 'character :direction :input :external-format :utf-8)
      (loop
        (let ((line (read-line stream nil :end)))
          (when (eq line :end)
            ;;; store the last encodings
            (when old-encoding
              (push (cons old-encoding (nreverse alist)) *encoding-data*))
            
            (return))
          (let* ((pos-semicolon-1 (position #\; line :test #'char=))
                 (encoding-string (subseq line 0 pos-semicolon-1))
                 (encoding (enconding-strong-to-encoding-symbol encoding-string))
                 (pos-semicolon-2 (position #\; line :test #'char= :start (1+ pos-semicolon-1)))
                 (index (parse-integer (subseq line (1+ pos-semicolon-1) pos-semicolon-2)))
                 (pos-semicolon-3 (position #\; line :test #'char= :start (1+ pos-semicolon-2)))
                 (char-code (parse-integer (subseq line (1+ pos-semicolon-2) pos-semicolon-3))))
             ;;; Read a line
            (cond ((null old-encoding)
                   (setq old-encoding encoding)
                   (push (cons index (code-char char-code)) alist))
                  ((not (eq old-encoding encoding))
                    ;;; Store current data
                   (push (cons old-encoding (nreverse alist)) *encoding-data*)
                   (setq old-encoding encoding)
                    ;;; reset alist
                   (setq alist (list (cons index (code-char char-code)))))
                  (t
                   (push (cons index (code-char char-code)) alist)))))))
    (mapcar #'generate-encoding-hashtable (mapcar #'first *encoding-data*)))
  (values))

(eval-when (:compile-toplevel)
  (process-encodings-file))

(setq *encoding-cache* #.*encoding-cache*)
