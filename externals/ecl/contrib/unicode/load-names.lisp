(defun split-words (text &key (set '(#\Space)) (exclude t))
  (loop with start = 0
     with output = '()
     with elt-type = (array-element-type text)
     for i from 0 below (length text)
     for c across text
     when (member c set)
     do (setf output (list* (make-array (+ (- i start) (if exclude 0 1))
				       :element-type elt-type
				       :displaced-to text
				       :displaced-index-offset start)
			    output)
	      start (1+ i))
     finally (return (nreverse (list* (make-array (- i start)
						 :element-type elt-type
						 :displaced-to text
						 :displaced-index-offset start)
				      output)))))

(defun encode-words (words hash)
  (loop for word in words
     collect (or (gethash word hash)
		 (let* ((word (copy-seq word))
			(ndx (hash-table-count hash)))
		   (setf (gethash word hash) (1+ ndx))))))

(defun fixup-hangul-syllables (dictionary)
  ;; "Hangul Syllable Composition, Unicode 5.1 section 3-12"
  (let* ((sbase #xac00)
         (lbase #x1100)
         (vbase #x1161)
         (tbase #x11a7)
         (scount 11172)
         (lcount 19)
         (vcount 21)
         (tcount 28)
         (ncount (* vcount tcount))
         (table (make-hash-table)))
    (with-open-file (*standard-input*
                     (make-pathname :name "Jamo" :type "txt"))
      (loop for line = (read-line nil nil)
            while line
            if (position #\; line)
            do (add-jamo-information line table)))
    (loop for sindex from 0 below scount
       for l = (+ lbase (floor sindex ncount))
       for v = (+ vbase (floor (mod sindex ncount) tcount))
       for tee = (+ tbase (mod sindex tcount))
       for name = (list* "HANGUL_" "SYLLABLE_"
			 (gethash l table) (gethash v table)
			 (unless (= tee tbase) (list (gethash tee table))))
       for code = (+ sbase sindex)
       collect (list* code (apply #'concatenate 'string name)
		      (encode-words name dictionary)))))

(defun add-jamo-information (line table)
  (let* ((split (split-words line :set '(#\;) :exclude t))
         (code (parse-integer (first split) :radix 16))
         (syllable (string-trim '(#\Space)
                                (subseq (second split) 0 (position #\# (second split))))))
    (setf (gethash code table) syllable)))

(defvar *words*)

(defparameter *data*
  (with-open-file (in "~/src/sbcl/tools-for-build/UnicodeData.txt" :direction :input)
    (loop with words = (setf *words* (make-hash-table :size 1024 :test #'equal))
       for ucd-line = (read-line in nil nil nil)
       while ucd-line
       nconc (let* ((ucd-data (split-words ucd-line :set '(#\;)))
		    (code (first ucd-data))
		    (name (second ucd-data)))
	       (unless (eql (char name 0) #\<)
		 (setf name (substitute #\_ #\Space name))
		 (list (list* (parse-integer code :radix 16)
			      name
			      (encode-words (split-words
					     name
					     :set '(#\Space #\_ #\-)
					     :exclude nil)
					    words))))))))

(print (length *data*))
(print (first (last *data*)))

;#+(or)
(progn
  (setf *data*
	(sort (nconc (fixup-hangul-syllables *words*) *data*)
	      #'<
	      :key #'car))
  (print (length *data*))
  (print (first (last *data*))))

(defparameter *words-array*
  (loop with array = (make-array (1+ (hash-table-count *words*)))
     for k being the hash-key in *words* using (hash-value v)
     do (setf (aref array v) k)
     finally (return array)))

(defparameter *last-word-index* (1- (length *words-array*)))

(defparameter *words-array-bytes*
  (loop for c across *words-array*
     sum (1+ (length c))))

(defun code-to-string (code)
  (aref *words-array* code))

(defparameter *flattened-data*
  (loop for (code name . rest) in *data*
     nconc (append rest (list 0))))

(defparameter *group-names*
  (loop with output = '()
     with start = (first (first *data*))
     with last = start
     for (code name . rest) in *data*
     do (when (>= (- code last) 2)
	  (setf output (cons (list start last) output)
		start code))
       (setf last code)
     finally (return (nreverse (cons (list start code) output)))))
