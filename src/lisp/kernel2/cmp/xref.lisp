(in-package #:cmp)

(defmacro do-bytecode-modules ((modname &optional result) &body body)
  `(block nil
     (core:map-bytecode-modules (lambda (,modname) ,@body))
     ,result))

(defun function-at-ip (ip module)
  (map nil (lambda (dinfo)
             (when (and (typep dinfo 'core:bytecode-simple-fun)
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
  "Find callers of a given function. This includes only direct calls, so not e.g. using the function as an argument to MAPCAR. Results may vary based on debug level and other factors and are not comprehensive.
Returns an alist. Each CAR is the name of the calling function, and the CDR is a source position for the call if available, or else NIL."
  (who-whats (core:function-cell function-name) #'module-callers))
(defun ext:who-binds (variable-name)
  "Find code that binds a given special variable. This includes only literal bindings, so not e.g. PROGV. Results may vary based on debug level and other factors and are not comprehensive.
Returns an alist. Each CAR is the name of the binding function, and the CDR is a source position for the bind if available, or else NIL."
  (who-whats (core:variable-cell variable-name) #'module-binders))
(defun ext:who-references (variable-name)
  "Find code that reads a given special variable. This includes only literal reads, so not necessarily e.g. SYMBOL-VALUE. Results may vary based on debug level and other factors and are not comprehensive.
Returns an alist. Each CAR is the name of the referencing function, and the CDR is a source position for the reference if available, or else NIL."
  (who-whats (core:variable-cell variable-name) #'module-referencers))
(defun ext:who-sets (variable-name)
  "Find code that modifies a given special variable. This includes only literal sets, so not necessarily e.g. SET. Results may vary based on debug level and other factors and are not comprehensive.
Returns an alist. Each CAR is the name of the modifying function, and the CDR is a source position for the set if available, or else NIL."
  (who-whats (core:variable-cell variable-name) #'module-setters))

(defun ext:who-macroexpands (macro-name)
  "Find code that includes use of a given macro. This includes only literal macro usage, so not e.g. EVAL or MACROEXPAND. Results may vary based on debug level and other factors and are not comprehensive.
Returns an alist. Each CAR is name of the function using the macro, and the CDR is a source position for the macro form if available, or else NIL."
  (let ((result nil))
    (do-bytecode-modules (module result)
      (map nil
           (lambda (dinfo)
             (when (and (typep dinfo 'core:bytecode-debug-macroexpansion)
                        (eql macro-name (core:bytecode-debug-macroexpansion/macro-name dinfo)))
               (let ((xref (xref-at-ip (core:bytecode-debug-info/start dinfo) module)))
                 (when xref
                   (push xref result)))))
           (core:bytecode-module/debug-info module)))))

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
      (when (and (<= start opip end) (eql mnem :called-fdefinition))
        (let* ((arg (first args))
               (pos (cdr arg))
               (cell (aref literals pos))
               (fname (core:function-name cell))
               (spi (spi-at-ip opip module))
               (xref (cons fname spi)))
          (push xref result))))
    result))

(defun ext:list-callees (function-name)
  "List functions called by a given function. This includes only direct calls, so not e.g. using the function as an argument to MAPCAR. Results may vary based on debug level and other factors and are not comprehensive.
Returns an alist. Each CAR is the name of the called function, and the CDR is a source position for the call if available, or else NIL."
  (if (fboundp function-name)
      (let* ((function (fdefinition function-name))
             (simple (core:function/entry-point function)))
        (if (typep simple 'core:bytecode-simple-fun)
            (bc-list-callees simple)
            nil))
      nil))

(defun %function-spi (function)
  (multiple-value-bind (file pos line col)
      (core:function-source-pos function)
    (core:make-source-pos-info :filepos pos :lineno line :column col
                               :filename (namestring file))))

(defun %method-spi (method)
  (or (clos::method-source-position method)
      (let ((mf (clos:method-function method)))
        (or (%function-spi (clos::fmf method))
            (%function-spi (clos::contf method))
            (%function-spi mf)))))

(defun ext:who-specializes-directly (class-designator)
  "Find methods that directly specialize a given class.
Returns an alist. Each CAR is the name of a method specialized on the class, and each CDR is the source location of the method definition if available, and otherwise NIL."
  (let ((class (typecase class-designator
                 (class class-designator)
                 (symbol (find-class class-designator nil))
                 (t nil))))
    (unless class (return-from ext:who-specializes-directly nil))
    (let ((methods (clos:specializer-direct-methods class)))
      (loop for method in methods
            for fname = (core:function-name
                         (clos:method-generic-function method))
            for spi = (%method-spi method)
            collect (cons `(method ,fname) spi)))))
