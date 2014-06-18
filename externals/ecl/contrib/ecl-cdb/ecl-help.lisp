;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 2011, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;

(in-package "SYSTEM")

(defun to-cdb-vector (object)
  (let* ((vector (make-array 128 :adjustable t
			     :fill-pointer 0
			     :element-type '(unsigned-byte 8)
			     :initial-element 0))
	 (stream (ext:make-sequence-output-stream
		  vector :external-format #+unicode :utf-8 #-unicode :default)))
    (with-standard-io-syntax
      (let ((si::*print-package* (find-package "CL")))
	(write object :stream stream :pretty nil
	       :readably nil :escape t)))
    vector))

(defun from-cdb-vector (vector)
  (let* ((stream (ext:make-sequence-input-stream
		  vector :external-format #+unicode :utf-8 #-unicode :default)))
    (read stream nil nil nil)))

(defun search-help-file (string path)
  (let* ((key (to-cdb-vector string))
	 (value (ecl-cdb:lookup-cdb key path)))
    (when value
      (from-cdb-vector value))))

(defun dump-help-file (hash-table path &optional merge test)
  (when merge
    (error "DUMP-HELP-FILE does not suport yet the third argument"))
  (ecl-cdb:with-output-to-cdb (cdb nil path)
    (loop for k being the hash-key of hash-table
       using (hash-value v)
       do (ecl-cdb:add-record (to-cdb-vector k)
			      (to-cdb-vector v)
			      cdb)))
  ;; Testing the consistency of the output
  (when test
    (loop for k being the hash-key of hash-table
       using (hash-value v)
       for other-value = (search-help-file k path)
       unless (and other-value (equalp other-value v))
       do (error "Symbol ~A not found in database ~A" k path))))
