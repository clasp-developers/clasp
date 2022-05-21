(in-package #:clasp-tests)

(test unicode-downcase-1
      (char-downcase #\CYRILLIC_CAPITAL_LETTER_IE_WITH_GRAVE)
      (#\CYRILLIC_SMALL_LETTER_IE_WITH_GRAVE))

(test unicode-upcase-1
      (char-upcase #\CYRILLIC_SMALL_LETTER_IE_WITH_GRAVE)
      (#\CYRILLIC_CAPITAL_LETTER_IE_WITH_GRAVE))

(test char-upcase.2
      (loop for i from 0 below char-code-limit
            for x = (code-char i)
            unless (or (not x)
                       (let ((u (char-upcase x)))
                         (and
                          (or (lower-case-p x) (char= u x))
                          (char= u (char-upcase u)))))
              collect (list i (char-name x)))
      (nil))

(test char-upcase.2a
      (loop for i from 0 below char-code-limit
            for x = (code-char i)
            when 
               (and x (lower-case-p x)
                          (char= x (char-upcase x)))
            collect (list i x (char-name x)))
      (nil))

(test char-downcase.2
      (loop for i from 0 below char-code-limit
            for x = (code-char i)
            unless (or (not x)
                       (let ((u (char-downcase x)))
                         (and
                          (or (upper-case-p x) (char= u x))
                          (char= u (char-downcase u)))))
              collect (list i x (char-name x)))
      (nil))

(test char-downcase.2a
      (loop for i from 0 below char-code-limit
            for x = (code-char i)
            when 
               (and x (upper-case-p x)
                          (char= x (char-downcase x)))
            collect (list i x (char-name x)))
      (nil))
