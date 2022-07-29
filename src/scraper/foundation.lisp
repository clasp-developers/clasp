(in-package :cscrape)

(defparameter *clasp-sys* #P"")
(defparameter *clasp-code* #P"")

(define-constant +begin-tag+ "BEGIN_TAG_bfc54f90bafadf5" :test 'equal)
(define-constant +end-tag+ "END_TAG_bfc54f90bafadf5" :test 'equal)

(defun concat-ds (short long)
  (cond
    ((and short (null long))
     short)
    (long
     (break "short: ~s long: ~s" short long))))

(defun fill-config (config line)
  (let* ((trimmed (string-trim " " line))
         (var-start (position #\space trimmed))
         (data-start (position #\< trimmed :start var-start))
         (var (string-trim " " (subseq trimmed var-start data-start)))
         (data (string-trim " <>" (subseq trimmed data-start))))
    (setf (gethash (intern var :keyword) config) data)))

(defun read-application-config (filename)
  (let ((config (make-hash-table :test #'equal)))
    (with-open-file (fin filename :direction :input :external-format :utf-8)
      (loop for l = (read-line fin nil 'eof)
         until (eq l 'eof)
         for tl = (string-trim '(#\space #\tab) l)
         do (cond
              ((string= (subseq tl 0 7) "#define")
               (fill-config config tl))
              (t (error "Illegal application.config line: ~a" l)))))
    config))




(defun lispify-match (cur name match &optional (next-char-text :ignore))
  (when (<= (length match) (- (length name) cur))
    ;; There are no enough characters in name to match match
    (unless (string= name match :start1 cur :end1 (+ cur (length match)))
      (return-from lispify-match nil))
    (case next-char-text
      (:ignore
       (+ cur (length match)))
      (:upper-case-alpha
       (error "Handle :upper-case-alpha"))
      (otherwise (error "Unknown option for next-char-text ~a" next-char-text)))
    ))

(defun maybe-lispify-substitute (sout cur name match subst)
  "If match is in name at cur then princ subst into sout. Always return new-cur"
  (let ((new-cur (lispify-match cur name match)))
    (when new-cur
      (princ subst sout))
    new-cur))

(defun lispify-symbol-name (name)
  "This needs to generate exactly the same result as lispify_symbol_name
   in foundation.cc"
  (when (string= name "Atom/getNeighborsForAbsoluteConfiguration")
    (break "Check backtrace"))
  (let ((subst-name
          (with-output-to-string (sout)
            (loop named subst
                  for cur = 0
                    then (or
                          (maybe-lispify-substitute sout cur name "_SHARP_" "#")
                          (maybe-lispify-substitute sout cur name "_BANG_" "!")
                          (maybe-lispify-substitute sout cur name "_ATSIGN_" "@" )
                          (maybe-lispify-substitute sout cur name "_COMMA_" "," )
                          (maybe-lispify-substitute sout cur name "_DIVIDE_" "/" )
                          (maybe-lispify-substitute sout cur name "_MINUS_" "-" )
                          (maybe-lispify-substitute sout cur name "_TIMES_" "*" )
                          (maybe-lispify-substitute sout cur name "_SLASH_" "/" )
                          (maybe-lispify-substitute sout cur name "PERCENT" "%" )
                          (maybe-lispify-substitute sout cur name "_PLUS_" "+" )
                          (maybe-lispify-substitute sout cur name "_DOT_" "." )
                          (maybe-lispify-substitute sout cur name "_EQ_" "=" )
                          (maybe-lispify-substitute sout cur name "_NE_" "/=" )
                          (maybe-lispify-substitute sout cur name "_LT_" "<" )
                          (maybe-lispify-substitute sout cur name "_GT_" ">" )
                          (maybe-lispify-substitute sout cur name "_LE_" "<=" )
                          (maybe-lispify-substitute sout cur name "_GE_" ">=" )
                          (maybe-lispify-substitute sout cur name "_UNDERSCORE_" "_" )
                          (maybe-lispify-substitute sout cur name "STAR" "*" )
                          (maybe-lispify-substitute sout cur name "AMP" "&" )
                          (maybe-lispify-substitute sout cur name "_" "-" )
                          (progn
                            (princ (elt name cur) sout)
                            (1+ cur)))
                  when (>= cur (length name))
                    do (return-from subst nil)
                  ))))
    ;; Now convert transitions in case in camel case to hyphens
    (let ((result (with-output-to-string (sout)
                    (loop named case
                          with length-subst-name = (length subst-name)
                          for cur below length-subst-name
                          for cc = (elt subst-name cur)
                          for ccn = (and (< (1+ cur) length-subst-name)
                                         (elt subst-name (1+ cur)))
                          if (and ccn
                                  (lower-case-p cc)
                                  (and (alpha-char-p ccn)
                                       (upper-case-p ccn)))
                            do (let ((ucc (char-upcase cc)))
                                 (princ ucc sout)
                                 (princ #\- sout))
                          else
                            do (let ((ucc (char-upcase cc)))
                                 (princ ucc sout))))))
      result)))



(defun magic-name (name &optional package-name)
  (let ((found (position #\: name)))
    (when found
      (unless (= (length package-name) 0)
        (error "Cannot convert ~s into a symbol name because package-name ~s was provided"
               name package-name))
      (let ((pkg-str (subseq name 0 found))
            (symbol-str (lispify-symbol-name (subseq name (1+ found)))))
        (return-from magic-name (format nil "~a:~a" pkg-str symbol-str))))
    (let ((found2 (search "__" name)))
      (when found2
        (unless (= (length package-name) 0)
          (error "Cannot convert ~s into a symbol name because package-name ~s was provided"
                 name package-name))
        (let ((pkg-str (lispify-symbol-name (subseq name 0 found2)))
              (symbol-str (lispify-symbol-name (subseq name (+ 2 found2)))))
          (return-from magic-name (format nil "~a:~a" pkg-str symbol-str)))))
    (when package-name
      (let ((symbol-str (lispify-symbol-name name)))
        (return-from magic-name (format nil "~a:~a" package-name symbol-str))))
    (error "Cannot convert ~s into a package:name form because no package-name was provided" name)))
