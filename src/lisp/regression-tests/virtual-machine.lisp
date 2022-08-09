(in-package #:clasp-tests)

(test int16-positive-values
      (let ((bad nil))
        (loop for bit below 15
              for value = (expt 2 bit)
              do (let ((buf (make-array 1024 :element-type 'ext:byte8 :initial-element 0 :adjustable t :fill-pointer 0)))
                   (core:write-unsigned-short buf value)
                   (let ((read (core:read-unsigned-short buf 0)))
                     (unless (= read value)
                       (setf bad t)
                       (format t "!!!!!!!!! NOT MATCHING ~a~%" value)))))
        bad)
      (nil) :test 'eq)

(test int-positive-values
      (let ((bad nil))
        (loop for bit below 20
              for value = (expt 2 bit)
              do (let ((buf (make-array 1024 :element-type 'ext:byte8 :initial-element 0 :adjustable t :fill-pointer 0)))
                   (core:write-int buf value)
                   (let ((read (core:read-int buf 0)))
                     (unless (= read value)
                       (setf bad t)
                       (format t "!!!!!!!!! NOT MATCHING ~a~%" value)))))
        bad)
      (nil) :test 'eq)

(test int-negative-values
      (let ((bad nil))
        (loop for bit below 20
              for value = (- (expt 2 bit))
              do (let ((buf (make-array 1024 :element-type 'ext:byte8 :initial-element 0 :adjustable t :fill-pointer 0)))
                   (core:write-int buf value)
                   (let ((read (core:read-int buf 0)))
                     (unless (= read value)
                       (setf bad t)
                       (format t "!!!!!!!!! NOT MATCHING ~a~%" value)))))
        bad)
      (nil) :test 'eq)

