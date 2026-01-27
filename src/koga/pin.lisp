(in-package #:koga)

(defvar +head-branch-text+ "HEAD branch: ")

(defun pin-repo (new-level
                 &key name extension directory repository branch commit ((:pin level) 1))
  (when (and (not branch)
             (probe-file directory))
    (let* ((output (run-program-capture "git remote show origin" :directory directory))
           (pos (search +head-branch-text+ output)))
      (when pos
        (setf branch (subseq output
                             (+ pos (length +head-branch-text+))
                             (position #\newline output :start pos))))))
  (loop for (key value) on `(:name ,name
                             :extension ,extension
                             :directory ,directory
                             :repository ,repository
                             :branch ,branch
                             :commit ,(cond ((or (not branch)
                                                 (not (probe-file directory)))
                                             commit)
                                            ((<= new-level level)
                                             (run-program-capture "git rev-parse HEAD"
                                                                  :directory directory)))
                             :pin ,level)
          by #'cddr
        when value
          collect key and collect value))

(defun pin (&key ((:pin level) 0) &allow-other-keys)
  (loop for path in (directory #P"repos.sexp")
        for repos = (sort (uiop:read-file-form path)
                          #'string-lessp
                          :key (lambda (x)
                                 (symbol-name (getf x :name))))
        do (with-open-file (stream path :direction :output :if-exists :supersede)
             (pprint-logical-block (stream repos :prefix "(" :suffix ")")
               (pprint-exit-if-list-exhausted)
               (loop (pprint-logical-block (stream (apply #'pin-repo level (pprint-pop))
                                            :prefix "(" :suffix ")")
                       (pprint-exit-if-list-exhausted)
                       (loop (write (pprint-pop) :stream stream :case :downcase)
                             (write-char #\space stream)
                             (write (pprint-pop) :stream stream :case :downcase)
                             (pprint-exit-if-list-exhausted)
                             (write-char #\space stream)
                             (pprint-newline :mandatory stream)))
                     (pprint-exit-if-list-exhausted)
                     (write-char #\space stream)
                     (pprint-newline :mandatory stream))))))
