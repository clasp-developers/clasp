(in-package :ext)

(let* ((basic-encodings
        #+unicode
         '(:UTF-8 :UCS-2 :UCS-2BE :UCS-2LE :UCS-4 :UCS-4BE :UCS-4LE
           :ISO-8859-1 :LATIN-1 :US-ASCII :DEFAULT)
         #-unicode
         '(:DEFAULT))
       (all-encodings nil))
  (defun all-encodings ()
    (or all-encodings
        (progn
          (setf all-encodings basic-encodings)
          #+unicode
          (let ((unicode-encodings
                 '(:iso-8859-2 :iso-8859-3 :iso-8859-4 :iso-8859-5
                   :iso-8859-6 :iso-8859-7 :iso-8859-8 :iso-8859-9
                   :iso-8859-10 :iso-8859-13 :iso-8859-14 :iso-8859-15
                   :koi8-r
                   :dos-cp437
                   :dos-cp850
                   :dos-cp852
                   :dos-cp855
                   :dos-cp857
                   :dos-cp860
                   :dos-cp861
                   :dos-cp862
                   :dos-cp863
                   :dos-cp864
                   :dos-cp865
                   :dos-cp866
                   :dos-cp869
                   :windows-cp1250
                   :windows-cp1251
                   :windows-cp1252
                   :windows-cp1253
                   :windows-cp1254
                   :windows-cp1255
                   :windows-cp1256
                   :windows-cp1257
                   :windows-cp1258
                   :latin-2
                   :latin-3
                   :latin-4
                   :latin-5
                   :latin-6
                   :latin-7
                   :latin-8
                   :latin-9
                   :cyrillic :arabic :asmo-708 :ecma-114 :greek :greek8 :ecma-118
                   :hebrew :ibm437 :ibm850 :cp850 :ibm852 :ibm855
                   :ibm857 
                   :ibm860 
                   :ibm861 
                   :ibm862 :cp862
                   :ibm863 
                   :ibm864 
                   :ibm865 
                   :ibm866 :cp866
                   :ibm869
                   
                   :windows-1250 :ms-ee
                   :windows-1251 :ms-cyrl
                   :windows-1252 :ms-ansi
                   :windows-1253 :ms-greek
                   :windows-1254 :ms-turk
                   :windows-1255 :ms-hebr
                   :windows-1256 :ms-arab
                   :windows-1257 :winbaltrim
                   :windows-1258)
                  ))
            (setf all-encodings (append all-encodings unicode-encodings)))
          all-encodings))))

(export 'ext:all-encodings :ext)

#+(or)
(defun ext:load-encoding (name)
  #-unicode
  (warn "EXT:LOAD-ENCODING not available when clasp is built without support for Unicode")
  #+unicode
  (let ((filename (make-pathname :name (symbol-name name) :defaults "sys:encodings;")))
    (cond ((probe-file filename)
	   (load filename :verbose nil)
	   name)
	  ((probe-file (setf filename (make-pathname :type "BIN" :defaults filename)))
	   (with-open-file (in filename :element-type '(unsigned-byte 16)
			       :external-format :big-endian)
	     (let* ((l (read-byte in))
		    (s (make-array l :element-type '(unsigned-byte 16) :initial-element 0)))
	       (read-sequence s in)
	       s)))
	  (t
	   (error "Unable to find mapping file ~A for encoding ~A" filename name)))))

(defun ext:make-encoding (encoding)
  (ecase encoding
    ((:US-ASCII
      :UTF-8 :UCS-2 :UCS-2LE :UCS-2BE
      :UCS-4 :UCS-4BE :UCS-4LE
      :ISO-8859-1 :LATIN-1 :DEFAULT)
     encoding)
    ((:iso-8859-2 :iso-8859-3 :iso-8859-4 :iso-8859-5
                  :iso-8859-6 :iso-8859-7 :iso-8859-8 :iso-8859-9
                  :iso-8859-10 :iso-8859-13 :iso-8859-14 :iso-8859-15
                  :koi8-r
                  :dos-cp437
                  :dos-cp850
                  :dos-cp852
                  :dos-cp855
                  :dos-cp857
                  :dos-cp860
                  :dos-cp861
                  :dos-cp862
                  :dos-cp863
                  :dos-cp864
                  :dos-cp865
                  :dos-cp866
                  :dos-cp869
                  :windows-cp1250
                  :windows-cp1251
                  :windows-cp1252
                  :windows-cp1253
                  :windows-cp1254
                  :windows-cp1255
                  :windows-cp1256
                  :windows-cp1257
                  :windows-cp1258)
     (ext:generate-encoding-hashtable encoding))
    (:latin-2 (ext:generate-encoding-hashtable :iso-8859-2))
    (:latin-3 (ext:generate-encoding-hashtable :iso-8859-3))
    (:latin-4 (ext:generate-encoding-hashtable :iso-8859-4))
    (:latin-5 (ext:generate-encoding-hashtable :iso-8859-9))
    (:latin-6 (ext:generate-encoding-hashtable :iso-8859-10))
    (:latin-7 (ext:generate-encoding-hashtable :iso-8859-13))
    (:latin-8 (ext:generate-encoding-hashtable :iso-8859-14))
    (:latin-9 (ext:generate-encoding-hashtable :iso-8859-15))

    (:cyrillic (ext:generate-encoding-hashtable :iso-8859-5))
    ((:arabic :asmo-708 :ecma-114) (ext:generate-encoding-hashtable :iso-8859-6))
    ((:greek :greek8 :ecma-118) (ext:generate-encoding-hashtable :iso-8859-7))
    (:hebrew (ext:generate-encoding-hashtable :iso-8859-8))

    (:ibm437 (ext:generate-encoding-hashtable :dos-cp437))
    ((:ibm850 :cp850) (ext:generate-encoding-hashtable :dos-cp850))
    (:ibm852 (ext:generate-encoding-hashtable :dos-cp852))
    (:ibm855 (ext:generate-encoding-hashtable :dos-cp855))
    (:ibm857 (ext:generate-encoding-hashtable :dos-cp857))
    (:ibm860 (ext:generate-encoding-hashtable :dos-cp860))
    (:ibm861 (ext:generate-encoding-hashtable :dos-cp861))
    ((:ibm862 :cp862) (ext:generate-encoding-hashtable :dos-cp862))
    (:ibm863 (ext:generate-encoding-hashtable :dos-cp863))
    (:ibm864 (ext:generate-encoding-hashtable :dos-cp864))
    (:ibm865 (ext:generate-encoding-hashtable :dos-cp865))
    ((:ibm866 :cp866) (ext:generate-encoding-hashtable :dos-cp866))
    (:ibm869 (ext:generate-encoding-hashtable :dos-cp869))
    
    ((:windows-1250 :ms-ee)(ext:generate-encoding-hashtable :windows-cp1250))
    ((:windows-1251 :ms-cyrl)(ext:generate-encoding-hashtable :windows-cp1251))
    ((:windows-1252 :ms-ansi)(ext:generate-encoding-hashtable :windows-cp1252))
    ((:windows-1253 :ms-greek)(ext:generate-encoding-hashtable :windows-cp1253))
    ((:windows-1254 :ms-turk)(ext:generate-encoding-hashtable :windows-cp1254) )
    ((:windows-1255 :ms-hebr)(ext:generate-encoding-hashtable :windows-cp1255))
    ((:windows-1256 :ms-arab)(ext:generate-encoding-hashtable :windows-cp1256))
    ((:windows-1257 :winbaltrim)(ext:generate-encoding-hashtable :windows-cp1257) )
    (:windows-1258 (ext:generate-encoding-hashtable :windows-cp1258))))

#+(or)
(defun ext:make-encoding (mapping)
  #-unicode
  (error "Not a valid external format ~A" mapping)
  #+unicode
  (cond
    ((symbolp mapping)
     (let ((var (intern (symbol-name mapping) (find-package "EXT"))))
       (unless (boundp var)
         (setf (symbol-value var) (ext:make-encoding (ext:load-encoding mapping))))
       (symbol-value var)))
    ((consp mapping)
     (let ((output (make-hash-table :size 512 :test 'eq)))
       (dolist (record mapping output)
	 (let* ((byte (car record))
		(unicode (cdr record))
		(unicode-char (code-char unicode)))
	   (when (> byte #xFF)
	     (setf (gethash (ash byte -8) output) t))
	   (setf (gethash byte output) unicode-char)
	   (setf (gethash unicode-char output) byte)))))
    ((arrayp mapping)
     (do* ((l (array-total-size mapping))
           (output (make-hash-table :size (floor (* 1.5 l)) :test 'eq))
           (i 0 (+ 2 i)))
          ((>= i l) output)
       (let* ((byte (aref mapping i))
              (unicode (aref mapping (1+ i)))
              (unicode-char (code-char unicode)))
         (when (> byte #xFF)
           (setf (gethash (ash byte -8) output) t))
         (setf (gethash byte output) unicode-char)
         (setf (gethash unicode-char output) byte))))
    (t
     (error "Not a valid external format ~A" mapping))))

;;; load this in ecl to generate generated-encodings.lsp
;;; e.g. (create-encodings-from-ecl "~/lisp/compiler/clasp-karsten/src/lisp/kernel/lsp/generated-encodings.lsp")
#+ecl
(defun create-encodings-from-ecl (path)
  (let ((encodings
         (list :iso-8859-2 :iso-8859-3 :iso-8859-4 :iso-8859-5
               :iso-8859-6 :iso-8859-7 :iso-8859-8 :iso-8859-9
               :iso-8859-10 :iso-8859-13 :iso-8859-14 :iso-8859-15

               :koi8-r

               :dos-cp437
               :dos-cp850
               :dos-cp852
               :dos-cp855
               :dos-cp857
               :dos-cp860
               :dos-cp861
               :dos-cp862
               :dos-cp863
               :dos-cp864
               :dos-cp865
               :dos-cp866
               :dos-cp869

               ;;; :windows-cp932
               ;;; :windows-cp936
               ;;; :windows-cp949
               ;;; :windows-cp950

               :windows-cp1250
               :windows-cp1251
               :windows-cp1252
               :windows-cp1253
               :windows-cp1254
               :windows-cp1255
               :windows-cp1256
               :windows-cp1257
               :windows-cp1258
               ))
        (result-alist nil))
    (dolist (name encodings)
      (let ((table (ext:make-encoding name))
            (mappings nil))
        (maphash #'(lambda(key value)
                     (when (and (numberp key)(characterp value))
                       (push (list key value) mappings)))
                 table)
        ;;; note the result table
        (push (list name mappings) result-alist)))
    ;;; note generate the mapping function
    (let ((file path))
      (when (probe-file file)
        (delete-file file))
      (with-open-file (stream file
                              :direction :output
                              :if-does-not-exist :create)
        (format stream "(in-package :ext)~2%")
        (format stream
                "(defun generate-encoding-hashtable (encoding)
  (let ((mappings
          (load-time-value
           (list~%")
        (dolist (specs (reverse result-alist))
          (let ((name (first specs))
                (mappings (second specs)))
            (format stream "            (list ~s ~%            (list ~%" name)
            (dolist (mapping (reverse mappings))
              (format stream "                   (list ~s (code-char ~s))~%" (first mapping)(char-code (second mapping))))
            (format stream "                   ))~%")))
        (format stream "            ))))
    (let ((spec (assoc encoding mappings)))
      (when spec
        (let ((table (make-hash-table)))
          (dolist (pair (second spec))
            (let ((key (first pair))
                  (value (second pair)))
              (setf (gethash key table) value)
              (setf (gethash value table) key)))
          table)))))")))))
