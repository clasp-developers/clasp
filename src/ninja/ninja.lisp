(in-package #:ninja)

(defparameter *indent* nil)

(defparameter *ignored-bindings* nil)

(defun escape (stream arg colonp atsignp &rest rest)
  "Escape the characters #\$, #\: and #\Space. This is needed mostly
in file paths so Ninja does not get confused about the breaks inbetween
inputs, outputs, etc."
  (declare (ignore colonp atsignp rest))
  (loop for char across (princ-to-string arg)
        when (position char "$: ")
          do (write-char #\$ stream)
        do (write-char char stream)))

(defun write-comment (output-stream text)
  "Write a comment into a Ninja build file."
  (let ((*line-end* "")
        (*line-start* (format nil "~:[~;  ~]# " *indent*)))
    (format output-stream "~:[~;  ~]# ~a~%"
            *indent* text)))

(defun write-bindings (output-stream &rest bindings)
  "Write a collection of binding instructions into a Ninja build file. The bindings are
assumed to be in a key-value pattern. The binding names are downcased."
  (let ((*line-end* " $")
        (*line-start* (format nil "~:[~;  ~]  " *indent*)))
    (loop for (name value) on bindings by #'cddr
          unless (or (null value)
                     (member name *ignored-bindings*))
            do (format output-stream "~:[~;  ~]~(~a~) = ~a~%"
                       *indent* name value))))

(defun write-rule (output-stream name
                   &rest bindings
                   &key command description depfile generator pool restat
                        rspfile rspfile-content deps
                   &allow-other-keys)
  "Write a rule instruction into a Ninja build file. In addition to the standard bindings
additional ones can passed by using the usual key value pattern."
  (declare (ignore command description depfile generator pool restat rspfile rspfile-content deps))
  (let ((*line-end* " $")
        (*line-start* "    "))
    (format output-stream "rule ~(~a~)~%" name))
  (let ((*indent* t))
    (apply #'write-bindings output-stream bindings))
  (terpri output-stream))

(defun write-build (output-stream name
                    &rest bindings
                    &key outputs implicit-outputs
                         inputs implicit-inputs order-only-inputs
                    &allow-other-keys)
  "Write a build instruction into a Ninja build file. The special key-values :outputs,
:implicit-outputs, :inputs, :implicit-inputs, and :order-only-inputs should be lists of
paths. These paths will be escaped using Ninja's character escaping rules."
  (let ((*line-end* " $")
        (*line-start* "    "))
    (format output-stream "build~{ ~/ninja:escape/~}~@[ |~{ ~/ninja:escape/~}~]: ~
~(~/ninja:escape/~)~{ ~/ninja:escape/~}~@[ |~{ ~/ninja:escape/~}~]~@[ ||~{ ~/ninja:escape/~}~]~%"
            outputs implicit-outputs name
            inputs implicit-inputs order-only-inputs))
  (let ((*indent* t)
        (*ignored-bindings* '(:implicit-outputs :outputs :implicit-inputs
                              :inputs :order-only-inputs)))
    (apply #'write-bindings output-stream bindings))
  (terpri output-stream))

(defun write-default (output-stream &rest targets)
  "Write a default instruction into a Ninja build file."
  (let ((*line-end* " $")
        (*line-start* "  "))
    (format output-stream "default~{ ~/ninja:escape/~}~%" targets))
  (terpri output-stream))

(defun write-pool (output-stream name
                   &rest bindings
                   &key depth
                   &allow-other-keys)
  "Write a pool instruction into a Ninja build file."
  (declare (ignore depth))
  (let ((*line-end* " $")
        (*line-start* "    "))
    (format output-stream "pool ~(~a~)~%" name))
  (let ((*indent* t))
    (apply #'write-bindings output-stream bindings))
  (terpri output-stream))

(defun write-include (output-stream path &key new-scope)
  "Write a include instruction into a Ninja build file."
  (let ((*line-end* " $")
        (*line-start* "  "))
    (format output-stream "~:[include~;subninja~] ~/ninja:escape/~%" new-scope path))
  (terpri output-stream))

