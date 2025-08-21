(in-package #:cmp)

;;; Used by debugger - see clasp-debug:disassemble-frame
(defun disassemble-assembly (start end)
  (format t "~&; disassemble-assembly Size: ~s Origin: ~s~%" (- (core:pointer-integer end) (core:pointer-integer start)) start)
  (llvm-sys:disassemble-instructions (get-builtin-target-triple-and-data-layout)
                                     start end))

(defun disassemble-function-to-asm (function)
  (let ((function-pointers (core:function-pointer-alist function)))
    (dolist (fp function-pointers)
      (let ((entry-point-name (car fp))
            (address (cdr fp)))
        (when address
          (multiple-value-bind (symbol start end)
              (core:lookup-address address)
            (if symbol
                (progn
                  (format t "Entry point ~a~%" (if (fixnump entry-point-name)
                                                   (format nil "xep~a" entry-point-name)
                                                   (string entry-point-name)))
                  (disassemble-assembly start end))
                (format t "; could not locate code object (bug?)~%"))))))))

;;; should work for both lambda expressions and bytecode functions.
(defun disassemble-to-ir (thing)
  (let* ((*save-module-for-disassemble* t)
         (*saved-module-from-clasp-jit* nil))
    (compile nil thing)
    (if *saved-module-from-clasp-jit*
        (format t "~&Disassembly: ~a~%" *saved-module-from-clasp-jit*)
        (error "Could not recover jitted module for ~a" thing)))
  (values))
