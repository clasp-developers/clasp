;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;
;;; ecl-curl.lisp - download of files via http
;;;
;;; (c) 2011, Juan Jose Garcia-Ripoll 
;;;
;;; THIS CODE IS BASED ON ASDF-INSTALL. AS SUCH IT RETAINS THE FOLLOWING
;;; COPYRIGHT NOTICE
;;;
;;; The original ASDF-INSTALL code (the files Makefile, README,
;;; asdf-install.asd, defpackage.lisp, and installer.lisp) was written by
;;; Daniel Barlow <dan@telent.net> and is distributed with SBCL and
;;; therefore in the public domain.  The SBCL Common Lisp implementation
;;; can be obtained from Sourceforge: <http://sbcl.sf.net/>.
;;;
;;; The initial port of ASDF-INSTALL to other Lisps was done by Dr. Edmund
;;; Weitz <edi@agharta.de> and included the file port.lisp and some
;;; changes to the files mentioned above.  More code was provided by Marco
;;; Baringer <mb@bese.it> (OpenMCL port), James Anderson
;;; <james.anderson@setf.de> (MCL port, including the file digitool.lisp),
;;; Kiyoshi Mizumaru <maru@krc.sony.co.jp>, Robert P. Goldman
;;; <rpgoldman@sift.info>, and Raymond Toy <toy@rtp.ericsson.se>
;;; (bugfixes).  Marco Antoniotti <marcoxa@cs.nyu.edu> added support for
;;; MK:DEFSYSTEM which includes the files load-asdf-install.lisp,
;;; loader.lisp, and finally split-sequence.lisp which has its own
;;; copyright notice. ASDF-Install is currently maintained by Gary King
;;; <gwking@metabang.com> and is hosted on Common-Lisp.net.
;;;
;;; The complete code distributed with this archive (asdf-install.tar.gz)
;;; is copyrighted by the above-mentioned authors and governed by the
;;; following license.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are
;;; met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS 'AS IS' AND ANY EXPRESSED OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT,
;;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
;;; IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(require :sockets)

(defpackage #:ecl-curl
  (:use #:sb-bsd-sockets #:cl)
  (:export #:download-url-to-file
	   #:download-error
	   #:download-url
	   #:download-response))

(in-package "ECL-CURL")

;;;---------------------------------------------------------------------------
;;; CONDITIONS
;;;

(define-condition http-transfer-error (error)
   ((url :initarg :url :reader download-url)))

(define-condition download-error (http-transfer-error)
  ((response :initarg :response :reader download-response))
  (:report (lambda (c s)
             (format s "Server responded ~A for GET ~A"
                     (download-response c) (download-url c)))))

;;;---------------------------------------------------------------------------
;;; PORTABILITY LAYER
;;;

(defvar *stream-buffer-size* 8192)

(defun copy-stream (from to)
  "Copy into TO from FROM until end of the input stream, in blocks of
*stream-buffer-size*.  The streams should have the same element type."
  (unless (subtypep (stream-element-type to) (stream-element-type from))
    (error "Incompatible streams ~A and ~A." from to))
  (let ((buf (make-array *stream-buffer-size*
                         :element-type (stream-element-type from))))
    (loop
      (let ((pos #-(or :clisp :cmu) (read-sequence buf from)
                 #+:clisp (ext:read-byte-sequence buf from :no-hang nil)
                 #+:cmu (sys:read-n-bytes from buf 0 *stream-buffer-size* nil)))
        (when (zerop pos) (return))
        (write-sequence buf to :end pos)))))

(defun make-stream-from-url (url)
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket
             :type :stream
             :protocol :tcp)))
    (sb-bsd-sockets:socket-connect
     s (car (sb-bsd-sockets:host-ent-addresses
             (sb-bsd-sockets:get-host-by-name (url-host url))))
     (url-port url))
    (sb-bsd-sockets:socket-make-stream 
     s
     :input t 
     :output t
     :buffering :full
     :external-format #+unicode :iso-8859-1 #-unicode :default)))

;;;---------------------------------------------------------------------------
;;; URL handling.
;;;

(defun url-host (url)
  (assert (string-equal url "http://" :end1 7))
  (let* ((port-start (position #\: url :start 7))
         (host-end (min (or (position #\/ url :start 7) (length url))
                        (or port-start (length url)))))
    (subseq url 7 host-end)))

(defun url-port (url)
  (assert (string-equal url "http://" :end1 7))
  (let ((port-start (position #\: url :start 7)))
    (if port-start 
        (parse-integer url :start (1+ port-start) :junk-allowed t) 80)))

; This is from Juri Pakaste's <juri@iki.fi> base64.lisp
(defparameter *encode-table*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=")

(defun base64-encode (string)
  (let ((result (make-array
                 (list (* 4 (truncate (/ (+ 2 (length string)) 3))))
                 :element-type 'base-char)))
    (do ((sidx 0 (+ sidx 3))
         (didx 0 (+ didx 4))
         (chars 2 2)
         (value nil nil))
        ((>= sidx (length string)) t)
      (setf value (ash (logand #xFF (char-code (char string sidx))) 8))
      (dotimes (n 2)
        (when (< (+ sidx n 1) (length string))
          (setf value
                (logior value
                        (logand #xFF (char-code (char string (+ sidx n 1))))))
          (incf chars))
        (when (= n 0)
          (setf value (ash value 8))))
      (setf (elt result (+ didx 3))
            (elt *encode-table* (if (> chars 3) (logand value #x3F) 64)))
      (setf value (ash value -6))
      (setf (elt result (+ didx 2))
            (elt *encode-table* (if (> chars 2) (logand value #x3F) 64)))
      (setf value (ash value -6))
      (setf (elt result (+ didx 1))
            (elt *encode-table* (logand value #x3F)))
      (setf value (ash value -6))
      (setf (elt result didx)
            (elt *encode-table* (logand value #x3F))))
    result))

(defvar *proxy* (ext:getenv "http_proxy"))

(defvar *proxy-user* nil)

(defvar *proxy-passwd* nil)

(defun request-uri (url)
  (assert (string-equal url "http://" :end1 7))
  (if *proxy*
      url
      (let ((path-start (position #\/ url :start 7)))
	(if path-start
	    (subseq url path-start)
	    "/index.html"))))

;;;---------------------------------------------------------------------------
;;; CONNECTION & HEADRE
;;;

(defun header-pair (name headers)
  "Searchers headers for name _without_ case sensitivity. Headers should be an alist mapping symbols to values; name a symbol. Returns the \(name value\) pair if name is found or nil if it is not."
  (assoc name headers 
         :test (lambda (a b) 
                 (string-equal (symbol-name a) (symbol-name b)))))

(defun header-value (name headers)
  "Searchers headers for name _without_ case sensitivity. Headers should be an alist mapping symbols to values; name a symbol. Returns the value if name is found or nil if it is not."
  (cdr (header-pair name headers)))

(defun url-connection (url)
  (let ((stream (make-stream-from-url (or *proxy* url)))
        (host (url-host url)))
    (format stream "GET ~A HTTP/1.0~C~CHost: ~A~C~C"
            (request-uri url) #\Return #\Linefeed
            host #\Return #\Linefeed)
    (when (and *proxy-passwd* *proxy-user*)
      (format stream "Proxy-Authorization: Basic ~A~C~C"
              (base64-encode (format nil "~A:~A" *proxy-user* *proxy-passwd*))
              #\Return #\Linefeed))
    (format stream "~C~C" #\Return #\Linefeed)
    (force-output stream)
    (values
     (let* ((l (read-line stream))
            (space (position #\Space l)))
       (parse-integer l :start (1+ space) :junk-allowed t))
     (loop for line = (read-line stream)
           until (or (null line)
                     (zerop (length line))
                     (eql (elt line 0) (code-char 13)))
           collect
           (let ((colon (position #\: line)))
             (cons (intern (string-upcase (subseq line 0 colon)) :keyword)
                   (string-trim (list #\Space (code-char 13))
                                (subseq line (1+ colon))))))
     stream)))

;;;---------------------------------------------------------------------------
;;; DOWNLOAD
;;;

(defun download-url-to-file (url file-name &key quiet)
  "Resolves url and then downloads it to file-name; returns the url actually used."
  (multiple-value-bind (response headers stream)
      (loop
       (multiple-value-bind (response headers stream)
           (url-connection url)
         (unless (member response '(301 302))
           (return (values response headers stream)))
         (close stream)
         (setf url (header-value :location headers))))
    (when (>= response 400)
      (error 'download-error :url url :response response))
    (let ((length (parse-integer (or (header-value :content-length headers) "")
                                 :junk-allowed t)))
      (unless quiet
	(format t "~&;;; Downloading ~A bytes from ~A to ~A ...~%"
		(or length "some unknown number of")
		url
		file-name))
      (force-output)
      (let ((ok? nil) (o nil))
        (unwind-protect
             (progn
               (setf o (open file-name 
                              :direction :output :if-exists :supersede
                              :external-format
			      #-unicode :default
			      #+unicode :latin-1))
               (if length
                   (let ((buf (make-array length
                                          :element-type
                                          (stream-element-type stream))))
                     (read-sequence buf stream)
                     (write-sequence buf o))
                   (copy-stream stream o))
               (setf ok? t))
          (when o (close o :abort (null ok?))))))
    (close stream))
  (values url))
