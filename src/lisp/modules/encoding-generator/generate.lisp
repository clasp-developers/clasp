(in-package #:encoding-generator)

;;;; process-encodings-file is the main entry point.

(defun encoding-string-to-encoding-symbol (encoding-string)
  ;; encodingdata.txt is assumed to only hold valid encoding names.
  (intern encoding-string "KEYWORD"))  

;;; format per line :ISO-8859-2;0;0;
;;; name of the encoding, the encoding's char-code, and unicode code point.
;;; So, :ISO-8859-5;161;1025; expresses that in the ISO-8859-5 encoding,
;;; 161 maps to Unicode codepoint 1025 (CYRILLIC CAPITAL LETTER IO).
(defun parse-line (line)
  (let* ((pos-colon (position #\: line :test #'char=))
         (pos-semicolon-1 (position #\; line :test #'char=))
         (encoding-string (subseq line (1+ pos-colon) pos-semicolon-1))
         (pos-semicolon-2 (position #\; line :test #'char= :start (1+ pos-semicolon-1)))
         (encoding-point (parse-integer (subseq line (1+ pos-semicolon-1) pos-semicolon-2)))
         (pos-semicolon-3 (position #\; line :test #'char= :start (1+ pos-semicolon-2)))
         (unicode-codepoint (parse-integer (subseq line (1+ pos-semicolon-2) pos-semicolon-3))))
    (values (encoding-string-to-encoding-symbol encoding-string)
            encoding-point
            unicode-codepoint)))

;;; Creates and returns an encoding cache based on an input encodingdata.txt.
;;; An encoding cache is an alist mapping encoding names, i.e. keywords
;;; returned from encoding-string-to-encoding-symbol, to encoding tables.
;;; An encoding table is an alist mapping encoding code points to
;;; characters.
(defun process-encodings-file (file)
  (with-open-file (stream file :element-type 'character :direction :input
                               :external-format :utf-8)
    (loop with encodings = ()
          for line = (read-line stream nil :end)
          until (eq line :end)
          do (multiple-value-bind (encoding point unipoint)
                 (parse-line line)
               (setf (alexandria:assoc-value
                      (alexandria:assoc-value encodings encoding)
                      point)
                     unipoint))
          finally (return encodings))))
