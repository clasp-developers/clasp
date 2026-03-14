(in-package :ext)

(export '(all-encodings))

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

(defun lookup-encoding (encoding)
  (or (gethash encoding *encoding-data*)
    (error "BUG: Unknown encoding ~a" encoding)))

;;; Called from C++ when an external format is used.
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
      :windows-cp1258
      :windows-cp932
      :windows-cp936
      :windows-cp949
      :windows-cp950)
     (lookup-encoding encoding))
    (:latin-2 (lookup-encoding :iso-8859-2))
    (:latin-3 (lookup-encoding :iso-8859-3))
    (:latin-4 (lookup-encoding :iso-8859-4))
    (:latin-5 (lookup-encoding :iso-8859-9))
    (:latin-6 (lookup-encoding :iso-8859-10))
    (:latin-7 (lookup-encoding :iso-8859-13))
    (:latin-8 (lookup-encoding :iso-8859-14))
    (:latin-9 (lookup-encoding :iso-8859-15))

    (:cyrillic (lookup-encoding :iso-8859-5))
    ((:arabic :asmo-708 :ecma-114) (lookup-encoding :iso-8859-6))
    ((:greek :greek8 :ecma-118) (lookup-encoding :iso-8859-7))
    (:hebrew (lookup-encoding :iso-8859-8))

    (:ibm437 (lookup-encoding :dos-cp437))
    ((:ibm850 :cp850) (lookup-encoding :dos-cp850))
    (:ibm852 (lookup-encoding :dos-cp852))
    (:ibm855 (lookup-encoding :dos-cp855))
    (:ibm857 (lookup-encoding :dos-cp857))
    (:ibm860 (lookup-encoding :dos-cp860))
    (:ibm861 (lookup-encoding :dos-cp861))
    ((:ibm862 :cp862) (lookup-encoding :dos-cp862))
    (:ibm863 (lookup-encoding :dos-cp863))
    (:ibm864 (lookup-encoding :dos-cp864))
    (:ibm865 (lookup-encoding :dos-cp865))
    ((:ibm866 :cp866) (lookup-encoding :dos-cp866))
    (:ibm869 (lookup-encoding :dos-cp869))
    
    ((:windows-1250 :ms-ee)(lookup-encoding :windows-cp1250))
    ((:windows-1251 :ms-cyrl)(lookup-encoding :windows-cp1251))
    ((:windows-1252 :ms-ansi)(lookup-encoding :windows-cp1252))
    ((:windows-1253 :ms-greek)(lookup-encoding :windows-cp1253))
    ((:windows-1254 :ms-turk)(lookup-encoding :windows-cp1254) )
    ((:windows-1255 :ms-hebr)(lookup-encoding :windows-cp1255))
    ((:windows-1256 :ms-arab)(lookup-encoding :windows-cp1256))
    ((:windows-1257 :winbaltrim)(lookup-encoding :windows-cp1257) )
    (:windows-1258 (lookup-encoding :windows-cp1258))))
