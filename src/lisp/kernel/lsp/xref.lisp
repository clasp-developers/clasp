(in-package #:core)

(defmacro do-bytecode-modules ((modname &optional result) &body body)
  `(block nil
     (core:map-bytecode-modules (lambda (,modname) ,@body))
     ,result))

(defun function-at-ip (ip module)
  (map nil (lambda (dinfo)
             (when (and (typep dinfo 'core:global-bytecode-simple-fun)
                        (<= (core:bytecode-debug-info/start dinfo) ip)
                        (< ip (core:bytecode-debug-info/end dinfo)))
               (return-from function-at-ip dinfo)))
       (core:bytecode-module/debug-info module)))

(defun spi-at-ip (ip module)
  (let ((cur nil)
        (cur-start -1) (cur-end -1))
    (map nil (lambda (dinfo)
               (let ((start (core:bytecode-debug-info/start dinfo))
                     (end (core:bytecode-debug-info/end dinfo)))
                 ;; cut off early - debug infos are stored in order
                 (when (> start ip) (return-from spi-at-ip cur))
                 (when (and (typep dinfo 'core:bytecode-debug-location)
                            (< ip end)
                            (or (null cur)
                                ;; more precise
                                (> start cur-start)
                                (< end cur-end)))
                   (setf cur (core:bytecode-debug-location/location dinfo)
                         cur-start start cur-end end))))
         (core:bytecode-module/debug-info module))
    cur))

;;; Return (function-name . spi) for an IP, or NIL if not available.
(defun xref-at-ip (ip module)
  (let ((fun (function-at-ip ip module))
        (spi (spi-at-ip ip module)))
    (if fun
        (cons (core:function-name fun) spi)
        nil)))

;;; Iterate through a module's instructions and return xrefs for all
;;; IPs for which the instruction is MNEMONIC, and which have a single
;;; constant argument that is the cell.
;;; (all of the instructions we're interested in for xref have this
;;;  format, so it works out ok to be specific)
(defun module-find-by-mnemonic (mnemonic cell module)
  (let ((results nil)
        ;; do-module-instructions has something like (:CONSTANT . n)
        ;; where n is a literal index, so get that index ahead of time.
        (cell-pos
          (position cell (core:bytecode-module/literals module))))
    (do-module-instructions (mnem args opip ip)
        (module)
      (when (eql mnem mnemonic)
        (let ((arg (first args)))
          (when (eql (cdr arg) cell-pos)
            (let ((xref (xref-at-ip opip module)))
              (when xref (push xref results)))))))
    results))

(defun module-callers (fcell module)
  (module-find-by-mnemonic :called-fdefinition fcell module))
(defun module-binders (vcell module)
  (module-find-by-mnemonic :special-bind vcell module))
(defun module-referencers (vcell module)
  (module-find-by-mnemonic :symbol-value vcell module))
(defun module-setters (vcell module)
  (module-find-by-mnemonic :symbol-value-set vcell module))

;;; If the cell is NIL, return NIL. Otherwise accumulate results.
;;; Search through all modules to find ones that reference the cell.
;;; If they do, call SEARCHER on the cell and module to get results.
;;; Return accumulated results.
(defun who-whats (cell searcher)
  (if cell
      (let ((results nil))
        (do-bytecode-modules (module results)
          (when (find cell (core:bytecode-module/literals module))
            (setf results (nconc results (funcall searcher cell module))))))
      nil))

(defun ext:who-calls (function-name)
  (who-whats (core:function-cell function-name) #'module-callers))
(defun ext:who-binds (variable-name)
  (who-whats (core:variable-cell variable-name) #'module-binders))
(defun ext:who-references (variable-name)
  (who-whats (core:variable-cell variable-name) #'module-referencers))
(defun ext:who-sets (variable-name)
  (who-whats (core:variable-cell variable-name) #'module-setters))

(defun ext:who-macroexpands (macro-name)
  ;; TODO: implement me.
  (declare (ignore macro-name))
  nil)

;;; this is supposed to be "lower level", but we don't need to.
(setf (fdefinition 'ext:list-callers) #'ext:who-calls)

;;; Kind of a reverse version of all the above stuff.
(defun bc-list-callees (fun)
  (let* ((module (core:simple-fun-code fun))
         (literals (core:bytecode-module/literals module))
         (start (core:bytecode-debug-info/start fun))
         (end (core:bytecode-debug-info/end fun))
         (result nil))
    (do-module-instructions (mnem args opip ip) (module)
      (when (eql mnem :called-fdefinition)
        (let* ((arg (first args))
               (pos (cdr arg))
               (cell (aref literals pos))
               (fname (core:function-name cell))
               (spi (spi-at-ip opip module))
               (xref (cons fname spi)))
          (push xref result))))
    result))

(defun ext:list-callees (function-name)
  (if (fboundp function-name)
      (let* ((function (fdefinition function-name))
             (simple (core:function/entry-point function)))
        (if (typep simple 'core:global-bytecode-simple-fun)
            (bc-list-callees simple)
            nil))
      nil))
