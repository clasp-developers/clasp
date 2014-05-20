#|#########################################################################
# Copyright (C) 1997-2011 Ufasoft                                         #
# http://ufasoft.com   mailto:support@ufasoft.com                         #
#                                                                         #
# This program is free software; you can redistribute it and/or modify it #
# under the terms of the GNU General Public License as published by the   #
# Free Software Foundation; either version 3, or (at your option) any     #
# later version.                                                          #
#                                                                         #
# This program is distributed in the hope that it will be useful,         #
# but WITHOUT ANY WARRANTY; without even the implied warranty of          #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           #
# GNU General Public License for more details.                            #
#                                                                         #
# You should have received a copy of the GNU General Public License       #
# along with this program; If not, see <http://www.gnu.org/licenses/>     #
########################################################################=|#

(in-package "SYS")


(defun logical-pathname (x)
  (typecase x
    (logical-pathname x)
    (pathname (error 'type-error :datum x :expected-type '(or logical-pathname stream string)))
    (synonym-stream (logical-pathname (symbol-value (synonym-stream-symbol x))))
    (stream (let ((pn (pathname x)))
              (unless (logical-pathname-p pn)
                 (error 'type-error :datum x :expected-type '(or logical-pathname stream string)))
              pn))
    (t
      #+COMPILER
       (handler-case
           (parse-namestring x nil _*empty-logical-pathname*)
         (parse-error (c) (error 'type-error :datum x)))
      #-COMPILER
         (parse-namestring x nil _*empty-logical-pathname*)
     )))


(defun _equal-pathchar (x y)
  (if (member :win32 *features*)
    (char-equal x y)
    (char= x y)))

(defun _trivial-p (x)
  (or (null x) (stringp x)))

(defun _simple-p (x)
  (or (null x) (stringp x) (eq x :wild)))

(defun _host-match (p s)
  (or (null p) (equal p s)))

(defun _host-diff (p s)
  (if p (list :host)
        (list s)))

(defvar _*subst*)

(defun _translate-host (p)
  (let ((a (car _*subst*)))
    (if (or (null p) (eq a :host))
      (pop _*subst*))
    (or p (and (_trivial-p a) a))))

(defun _device-match (p s)
  (cond ((or (null p) (eq p :wild)) t)
        ((eq s :wild) nil)
        (t (if (member :win32 *features*)
              (equalp p s)
              (equal p s)))))

(defun _wild-to-string (x)
  (if (eq x :wild) "*" x))

(defun _device-diff (p s)
  (cond ((or (null p) (eq p :wild))
           (list (_wild-to-string s)))
        (t (list :device))))

(defun _translate-device (p)
  (let ((a (car _*subst*)))
    (if (or (null p) (eq a :device))
      (pop _*subst*))
    (or p (and (_trivial-p a) a))))


(defun _wildcard-match-ab (m i b j)
  (let ((mc (length m))
        (bc (length b))
        c)
   (loop
     (if (eql i mc) (return-from _wildcard-match-ab (eql j bc)))
     (setq c (char m (prog1 i (incf i))))
     (cond ((eql c #\?)
       		(if (eql j bc)
	           (return-from _wildcard-match-ab nil)
       		   (incf j)))
          ((eql c #\*) (return))
          ((eql j bc) (return-from _wildcard-match-ab nil))
          (t (unless (_equal-pathchar c (char b (prog1 j (incf j))))
               (return-from _wildcard-match-ab nil)))))
   (loop
    (if (eql i mc) (return-from _wildcard-match-ab t))
    (setq c (char m (prog1 i (incf i))))
    (cond ((eql c #\?)
       		(if (zerop bc)
	           (return-from _wildcard-match-ab nil)
               (incf j)))
           ((not (eql c #\*)) (return))))
   (loop
    (if (eql j bc) (return-from _wildcard-match-ab nil))
    (and (_equal-pathchar c (char b (prog1 j (incf j))))
         (_wildcard-match-ab m i b j)
         (return-from _wildcard-match-ab t)))))


(defun _wildcard-match (p s)
  (case p
    ((:wild :wild-inferiors) t)
    ((:up :back) nil)
    (t (_wildcard-match-ab p 0 s 0))))

(defun _subdir-match (p s)
  (or (eq p s)
      (eq p :wild)
      (and (stringp p) (stringp s) (_wildcard-match p s))))

(defun _directory-match-ab (m b &aux item)
  (loop
    (if (atom m) (return-from _directory-match-ab (atom b)))
    (setq item (pop m))
    (if (eq item :wild-inferiors)
      (return))
    (if (or (atom b) (not (_subdir-match item (pop b))))
      (return-from _directory-match-ab nil)))
  (loop
    (if (atom m) (return-from _directory-match-ab t))
    (setq item (pop m))
    (if (not (eq item :wild-inferiors))
      (return)))
  (loop
    (if (atom b) (return-from _directory-match-ab nil))
    (if (and (_subdir-match item (pop b))
             (_directory-match-ab m b))
      (return-from _directory-match-ab t))))
    

(defun _directory-match (p s)
  (cond ((or (null p) (null s) (eq s :unspecific)) t)
        ((not (eq (car p) (car s))) nil)
        (t (_directory-match-ab (cdr p) (cdr s)))))


(defun _uni-diff (p s wild wild-inferiors sub-diff conv &optional r)
  (do* ((qi 0)
        (si 0)
        (plen (length p))
        (slen (length s))
        c)        
      ((eql qi plen) (if (eql si slen) r (error "Sequences don't match")))
    (setq c (elt p qi))
    (incf qi)
    (cond ((eql c wild-inferiors)
             (if (> (- plen qi) (- slen si))
               (error "Sequences don't match"))
             (push (subseq s si
                             (setq si (- slen (- plen qi))))
                   r))
               
          ((eql si slen)
             (error "Sequences don't match"))
          (t (let ((d (elt s si)))
               (incf si)
               (if (eql c wild)
                 (push (conv d) r)
                 (setq r (funcall sub-diff c d r))))))))


(defun _directory-diff-ab (p s)
  (let ((r (_uni-diff p s :wild :wild-inferiors #'_nametype-diff #'identity)))
    (mapcar (lambda (x)
              (if (consp x)
                (cons :directory x)
                x))
            r)))

(defun _directory-diff (p s)
  (cond ((null s) (list p))
        ((or (null p) (equal p '(:relative))) (list s))
        (t (_directory-diff-ab (cdr p) (cdr s)))))

(defun _translate-nametype-aux (p)
  (cond ((and (eq p :wild) (consp _*subst*))
           (let ((a (car _*subst*)))
                (if (or (null a) (stringp a)) (pop _*subst*))))
        ((stringp p)
           (do* ((i 0 (1+ i))
                 (len (length p))
                 r)
               ((progn (push (subseq p i
                                       (setq i (or (position #\* p :start i) len)))
                             r)
                       (eql i len))
                  (apply #'ext:string-concat (reverse r)))
             (if (or (null (car _*subst*)) (stringp (null (car _*subst*))))
               (push (pop _*subst*) r)
			   (return-from _translate-nametype-aux nil))))
        (t p)))


(defun _translate-directory (p)
  (cond ((null p)
           (let ((a (pop _*subst*)))
              (if (listp a) (copy-list a))))

        ((and (eq (car p) :absolute)
              (or (null (car _*subst*)) (equal (car _*subst*) '(:relative))))
           (pop _*subst*)
           (copy-list p))
        (t (let ((r (list (pop p))))
             (dolist (x p (reverse r))
			   (if (eq x :wild-inferiors)
                 (if (eq (caar _*subst*) :directory)
                   (setq r (append (reverse (cdr (pop _*subst*))) r))
				   (return-from _translate-directory))
                 (let ((item (_translate-nametype-aux x)))
                   (if item
                     (push item r)
                     (return-from _translate-directory)))))))))



(defun _nametype-match (p s)
  (cond ((or (null p) (eq p :unspecific) (eq p :wild)) t)
        ((or (null s) (eq s :wild)) nil)
        (t (_wildcard-match p s))))

(defun _nametype-diff (p s &optional r)
  (if (or (null p) (eq p :wild))
    (list s)
    (_uni-diff p s #\? #\* 
             (lambda (a b r)
               (if (_equal-pathchar a b)
                 r
                 (error "Sequences don't match")))
             #'string r)))

(defun _translate-nametype (p)
  (if (and (null p) _*subst*)
    (and (_simple-p (car _*subst*)) (pop _*subst*))
    (_translate-nametype-aux p)))


(defun _version-match (p s)
  (cond ((or (eq s :unspecific) (null p) (eq p :wild)) t)
        ((eq s :wild) nil)
        (t (eql p s))))

(defun _version-diff (p s)
  (cond ((or (null p) (eq p :wild))
           (list s))
        (t (list :version))))

(defun _translate-version (p)
  (cond ((and _*subst* (or (null p) (eq p :wild)))
           (let ((a (pop _*subst*)))
             (if (or (null a) (integerp a) (eq a :wild) (eq a :newest) (eq a :unspecific))
               a)))
        (t p)))

(defun pathname-match-p (pathname wildcard)
  (setq pathname (pathname pathname)
        wildcard (pathname wildcard))
  (and (_host-match       (pathname-host wildcard)      (pathname-host pathname))
       (_device-match     (pathname-device wildcard)    (pathname-device pathname))
       (_directory-match  (pathname-directory wildcard) (pathname-directory pathname))
       (_nametype-match   (pathname-name wildcard)      (pathname-name pathname))
       (_nametype-match   (pathname-type wildcard)      (pathname-type pathname))  
       (_version-match    (pathname-version wildcard)   (pathname-version pathname))))
   

(defun _translate-component (fun keyword pattern)
  (let ((r (funcall fun pattern)))
	(cond (r (list keyword r))
	      ((eq keyword :host) nil)
	      (t (error "Replacement pieces don't fit onto ~S" pattern)))))

(defun translate-pathname (source from-wildcard to-wildcard &key)
  (setq source        (pathname source)
        from-wildcard (pathname from-wildcard)
        to-wildcard   (pathname  to-wildcard))
  (unless (pathname-match-p source from-wildcard)
	(error 'type-error :datum source))
  (let* ((host 		(reverse (_host-diff 		(pathname-host from-wildcard)		(pathname-host source)      )))
        (device  	(reverse (_device-diff   	(pathname-device from-wildcard)		(pathname-device source)    )))
		(directory  (reverse (_directory-diff  	(pathname-directory from-wildcard)	(pathname-directory source) )))
		(name		(reverse (_nametype-diff   	(pathname-name from-wildcard)		(pathname-name source)      )))
		(type       (reverse (_nametype-diff   	(pathname-type from-wildcard)		(pathname-type source)      )))
		(version    (reverse (_version-diff    	(pathname-version from-wildcard)	(pathname-version source)   )))
        (logical (logical-pathname-p to-wildcard))
        (_*subst* (append host device directory name type version)))
    (apply #'make-pathname
      (append
        (list :logical logical)
        (_translate-component #'_translate-host :host (pathname-host to-wildcard))
        (unless logical (_translate-component #'_translate-device :device (pathname-device to-wildcard)))
        (_translate-component #'_translate-directory :directory (pathname-directory to-wildcard))
        (_translate-component #'_translate-nametype :name (pathname-name to-wildcard))
        (_translate-component #'_translate-nametype :type (pathname-type to-wildcard))
        (_translate-component #'_translate-version :version (pathname-version to-wildcard))))))

(defun translate-logical-pathname (pathname &key)
  (setq pathname (if (stringp pathname)
                   (logical-pathname pathname)
                   (pathname pathname)))
  (if (logical-pathname-p pathname)
    (let ((ht (make-hash-table :key-type 'logical-pathname :value-type '(eql t)
                                :test #'equal)))
       (loop
         (when (gethash pathname ht) (error "Translation loop"))
         (setf (gethash pathname ht) t)
         (let ((host (or (pathname-host pathname) "SYS")))
           (unless (logical-host-p host) (error "No translation for host"))
           (let* ((translations
                   (gethash host sys::*logical-pathname-translations*))
                  (translation
                   (assoc pathname translations :test #'pathname-match-p)))
             (unless (and translation (consp translation)
                          (consp (cdr translation)))
               (error "No translation for pathname"))
             (setq pathname (translate-pathname pathname (first translation)
                                                (second translation)))))
         (unless (logical-pathname-p pathname) (return)))))
  pathname)


(defun enough-namestring (pathname &optional (defaults *default-pathname-defaults*))
   (setq pathname (pathname pathname))
   (setq defaults (pathname defaults))
   (namestring
     (multiple-value-call #'make-pathname
       (if (equal (pathname-device pathname) (pathname-device defaults))
         (values
           :device nil
           :directory
             (let ((pathname-dir (pathname-directory pathname))
                   (defaults-dir (pathname-directory defaults)))
               (if (equal pathname-dir defaults-dir)
                 (list ':RELATIVE)
                 (if (and (not (eq (car pathname-dir) ':RELATIVE))
                          (not (eq (car defaults-dir) ':RELATIVE))
                          (equal (subseq pathname-dir 0 (min (length pathname-dir) (length defaults-dir)))
                                 defaults-dir
                     )    )
                   (cons ':RELATIVE (nthcdr (length defaults-dir) pathname-dir))
                   pathname-dir
             ) ) )
         )
         (values
           :device (pathname-device pathname)
           :directory (pathname-directory pathname)))
       :name (if (equal (pathname-name pathname) (pathname-name defaults))
               nil
               (pathname-name pathname))
       :type (if (equal (pathname-type pathname) (pathname-type defaults))
               nil
               (pathname-type pathname)))))


