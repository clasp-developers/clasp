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
;;;;  unicode.lsp -- encoding and decoding
;;;;

(in-package "SYSTEM")

;;;;
;;;; ENCODING / DECODING ERRORS
;;;;

(define-condition character-coding-error (error)
  ((external-format :initarg :external-format :reader character-coding-error-external-format)))

(define-condition character-encoding-error (character-coding-error)
  ((code :initarg :code :reader character-encoding-error-code)))

(define-condition character-decoding-error (character-coding-error)
  ((octets :initarg :octets :reader character-decoding-error-octets)))

(define-condition stream-encoding-error (stream-error character-encoding-error)
  ()
  (:report
   (lambda (c s)
     (let ((stream (stream-error-stream c))
           (code (character-encoding-error-code c)))
       (format s "~@<encoding error on stream ~S (~S ~S): ~2I~_~
                  the character with code ~D cannot be encoded.~@:>"
               stream ':external-format
               (character-coding-error-external-format c)
               code)))))

(define-condition stream-decoding-error (stream-error character-decoding-error)
  ()
  (:report
   (lambda (c s)
     (let ((stream (stream-error-stream c))
           (octets (character-decoding-error-octets c)))
       (format s "~@<decoding error on stream ~S (~S ~S): ~2I~_~
                  the octet sequence ~S cannot be decoded.~@:>"
               stream ':external-format
               (character-coding-error-external-format c)
               octets)))))

(defun encoding-error (stream external-format code)
  (restart-case (error 'stream-encoding-error
                       :stream stream
                       :external-format external-format
                       :code code)
    (continue ()
      :report "Ignore character"
      nil)
    (use-value (c)
      :report "Store a different character code."
      (if (characterp c) c (code-char c)))))

(defun decoding-error (stream external-format octets)
  (restart-case (error 'stream-decoding-error
                       :stream stream
                       :external-format external-format
                       :octets octets)
    (continue ()
      :report "Read next character"
      nil)
    (use-value (c)
      :report "Replace the bogus sequence with a character"
      (if (characterp c) c (code-char c)))))
