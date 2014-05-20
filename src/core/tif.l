(defvar *f* (FileInStream "zMatrix.cc"))
(defvar *s* (readEntireFile *f*))
(close *f*)
(print *s*)