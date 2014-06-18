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
;;;; This file implements the CDB specification, which you find here
;;;; http://cr.yp.to/cdb/cdb.txt and is reproduced below, replicating
;;;; the interface developed by Zach Beane under the name ZCDB.
;;;;
;;;; A structure for constant databases
;;;; 19960914
;;;; Copyright 1996
;;;; D. J. Bernstein, djb@pobox.com
;;;;
;;;; A cdb is an associative array: it maps strings (``keys'') to strings
;;;; (``data'').
;;;;
;;;; A cdb contains 256 pointers to linearly probed open hash tables. The
;;;; hash tables contain pointers to (key,data) pairs. A cdb is stored in
;;;; a single file on disk:
;;;;
;;;;     +----------------+---------+-------+-------+-----+---------+
;;;;     | p0 p1 ... p255 | records | hash0 | hash1 | ... | hash255 |
;;;;     +----------------+---------+-------+-------+-----+---------+
;;;;
;;;; Each of the 256 initial pointers states a position and a length. The
;;;; position is the starting byte position of the hash table. The length
;;;; is the number of slots in the hash table.
;;;;
;;;; Records are stored sequentially, without special alignment. A record
;;;; states a key length, a data length, the key, and the data.
;;;;
;;;; Each hash table slot states a hash value and a byte position. If the
;;;; byte position is 0, the slot is empty. Otherwise, the slot points to
;;;; a record whose key has that hash value.
;;;;
;;;; Positions, lengths, and hash values are 32-bit quantities, stored in
;;;; little-endian form in 4 bytes. Thus a cdb must fit into 4 gigabytes.
;;;;
;;;; A record is located as follows. Compute the hash value of the key in
;;;; the record. The hash value modulo 256 is the number of a hash table.
;;;; The hash value divided by 256, modulo the length of that table, is a
;;;; slot number. Probe that slot, the next higher slot, and so on, until
;;;; you find the record or run into an empty slot.
;;;;
;;;; The cdb hash function is ``h = ((h << 5) + h) ^ c'', with a starting
;;;; hash of 5381.
;;;;

(defpackage "ECL-CDB"
  (:use "CL")
  (:export "WITH-OUTPUT-TO-CDB" "ADD-RECORD" "LOOKUP-CDB"))

(in-package "ECL-CDB")

(defstruct cdb
  stream
  pathname
  word-buffer
  tables
  temporary-pathname)

(defun read-word (stream)
  (logior (read-byte stream)
	  (ash (read-byte stream) 8)
	  (ash (read-byte stream) 16)
	  (ash (read-byte stream) 24)))

(defun write-word (byte stream)
  (declare (type (unsigned-byte 32) byte)
	   (stream stream)
	   (optimize speed (safety 0)))
  (write-byte (logand #xff byte) stream)
  (write-byte (logand #xff (ash byte -8)) stream)
  (write-byte (logand #xff (ash byte -16)) stream)
  (write-byte (logand #xff (ash byte -24)) stream))

(defun write-vector (vector stream)
  (declare (type (array (unsigned-byte 8) (*)) vector))
  (loop for v across vector
     do (write-word v stream)))

(defun to-cdb-hash (key-vector)
  (declare (type (array (unsigned-byte 8) (*)) vector))
  (loop with h of-type (unsigned-integer 32) = 5381
     for byte of-type (unsigned-byte 8) across key-vector
     do (setf h (logxor (logand #xffffffff
				(+ (ash (logand #.(ash #xffffffff -5) h)
					5)
				   h))
			byte))
     finally (return h)))

(defun %make-cdb (cdb-pathname temporary-pathname)
  (let ((stream (open temporary-pathname
		      :direction :output
		      :if-exists :supersede
		      :if-does-not-exist :create
		      :element-type '(unsigned-byte 8))))
    (if stream
	(progn
	  (file-position stream 0)
	  (dotimes (i (* 256 2))
	    (write-word 0 stream))
	  (make-cdb :stream stream
		    :pathname cdb-pathname
		    :tables (make-array 256 :initial-element nil)
		    :temporary-pathname temporary-pathname))
	(error "Unable to create CDB at filename ~A" temporary-pathname))))

(defmacro with-output-to-cdb ((cdb cdb-pathname temporary-pathname) &body body)
  `(let (,cdb)
     (unwind-protect
	  (progn
	    (setf ,cdb (%make-cdb ,cdb-pathname ,temporary-pathname))
	    ,@body)
       (close-cdb ,cdb))))

(defun add-record (key value cdb)
  ;; This routine dumps the data of this record, storing a small
  ;; reference in the CDB structure itself. This reference will be
  ;; used to create the hash.
  (let* ((hash-key (to-cdb-hash key))
	 (table-index (logand #xff hash-key))
	 (stream (cdb-stream cdb)))
    (push (cons hash-key (file-position stream))
	  (aref (cdb-tables cdb) table-index))
    (write-word (length key) stream)
    (write-word (length value) stream)
    (write-sequence key stream)
    (write-sequence value stream)))

(defun dump-table (table stream)
  (declare (optimize speed (safety 0)))
  ;; TABLE is an association list of (HASH-KEY . FILE-POSITION)
  ;; We dump it at the end of the file. The length of the actual
  ;; file table can be a bit larger, to avoid many coincidences.
  ;; Here we use a factor 2.
  (loop with length = (* 2 (length table))
     with vector = (make-array (* 2 length) :initial-element 0
			       :element-type '(unsigned-byte 32))
     for (hash-key . pos) in table
     for index = (mod (ash hash-key -8) length)
     do (loop for disp from 0 below length
	   for i = (* 2 (mod (+ disp index) length))
	   for record-pos = (aref vector (1+ i))
	   until (zerop record-pos)
	   finally (setf (aref vector i) hash-key (aref vector (1+ i)) pos))
     finally (progn (write-vector vector stream)
		    (return length))))

(defun dump-cdb (cdb)
  ;; After we have dumped all the records in the file, we append the
  ;; hash tables and recreate the index table at the beginning.
  (let* ((stream (cdb-stream cdb))
	 (index (make-array (* 2 256) :element-type '(unsigned-byte 32))))
    (loop for table across (cdb-tables cdb)
       for i of-type fixnum from 0 by 2
       do (setf (aref index i) (file-position stream)
		(aref index (1+ i)) (dump-table table stream)))
    (file-position stream 0)
    (write-vector index stream)))

(defun close-cdb (cdb)
  (let ((stream (cdb-stream cdb)))
    (when (open-stream-p stream)
      (dump-cdb cdb)
      (close stream)
      (when (cdb-pathname cdb)
	(rename-file (cdb-temporary-pathname cdb)
		     (cdb-pathname cdb))))))

(defun cdb-error (stream)
  (error "Error when reading CDB database ~A" stream))

(defun values-coincide (position key-vector stream return-position-p)
  (unless (file-position stream position)
    (cdb-error stream))
  (let ((key-length (read-word stream)))
    (when (= key-length (length key-vector))
      (let* ((value-length (read-word stream))
	     (other-key (make-array key-length :element-type '(unsigned-byte 8))))
	(read-sequence other-key stream)
	(when (equalp other-key key-vector)
	  (if return-position-p
	      (file-position stream)
	      (let ((value (make-array value-length :element-type '(unsigned-byte 8))))
		(read-sequence value stream)
		value)
	      ))))))

(defun lookup-cdb (key stream &optional return-position-p)
  (if (streamp stream)
      (let* ((hash (to-cdb-hash key))
	     (table (logand #xFF hash)))
	(unless (file-position stream (* table 8))
	  (cdb-error stream))
	(let* ((start (read-word stream))
	       (length (read-word stream))
	       (index (mod (ash hash -8) length)))
	  (loop for reset = t
	     for i from 0 below length
	     for rounded-i = (mod (+ index i) length)
	     for position = (+ start (* 8 rounded-i))
	     do (progn
		  (when reset
		    (unless (file-position stream position)
		      (cdb-error stream))
		    (setf reset nil))
		  (let* ((other-hash (read-word stream))
			 (record-position (read-word stream)))
		    (when (zerop record-position)
		      (return nil))
		    (when (= other-hash hash)
		      (let ((output (values-coincide record-position key stream
						     return-position-p)))
			(if output
			    (return output)
			    (setf reset t)))))))))
      (with-open-file (s stream :direction :input
			 :element-type '(unsigned-byte 8))
	(lookup-cdb key s return-position-p))))

(defun map-cdb (function stream)
  (if (streamp stream)
      (let* ((index (make-array (* 256 2) :element-type '(unsigned-byte 32))))
	(unless (file-position stream 0)
	  (cdb-error stream))
	(unless (= (read-sequence index stream) (length index))
	  (cdb-error stream))
	(loop for i from 0 by 2 below (length index)
	   for table-position = (aref index i)
	   for table-length = (aref index (1+ i))
	   do (progn
		(unless (file-position stream table-position)
		  (cdb-error stream))
		(loop for i from 0 below table-length
		   for position from table-position by 8
		   for record-hash = (read-word stream)
		   for record-position = (read-word stream)
		   unless (zerop record-position)
		   do (progn
			(unless (file-position stream record-position)
			  (cdb-error stream))
			(let* ((key-length (read-word stream))
			       (value-length (read-word stream))
			       (key (make-array key-length
						:element-type '(unsigned-byte 8)))
			       (value (make-array value-length
						  :element-type '(unsigned-byte 8))))
			  (unless (and (= (read-sequence key stream)
					  key-length)
				       (= (read-sequence value stream)
					  value-length))
			    (cdb-error stream))
			  (funcall function key value)))))))
      (with-open-file (s stream :direction :input :element-type '(unsigned-byte 8))
	(map-cdb function s))))

(provide :ecl-cdb)
