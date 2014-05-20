(require 'cl)

(defun replace-in-files (matches files)
  (save-excursion
    (mapc (lambda (file)
	    (switch-to-buffer (or (find-buffer-visiting file) (find-file file)))
	    (mapc (lambda (x)
		    (beginning-of-buffer)
		    (let ((case-fold-search nil))
		      (print x)
		      (while (search-forward-regexp (car x) nil t)
			(replace-match (cdr x) nil t)))
		    (save-buffer)
		    )
		  matches))
	  files)))

(defun ecl-load-symbols ()
  (interactive)
  (beginning-of-buffer)
  (while (re-search-forward ";;; Address = \\([0-9a-f]*\\)" nil t)
    (let ((address (buffer-substring (match-beginning 1)
				     (match-end 1))))
      (re-search-backward ";;; Loading \\(/.*\.o\\)$")
      (let ((file (buffer-substring (match-beginning 1)
				    (match-end 1))))
	(print file) (print address)
	(save-excursion
	  (gud-call (format "add-symbol-file %s 0x%s" file address))))
      (next-line 2))))

(defvar ecl-search-string)

(defun query-replace-ecl (from-string to-string &optional delimited start end)
  (interactive (query-replace-read-args "Query replace" nil))
  (setq ecl-search-string from-string)
  (let ((remaining (member (buffer-file-name (current-buffer)) ecl-files)))
    (dolist (i (or remaining ecl-files))
      (let ((b (find-buffer-visiting i)))
	(unless (equal b (current-buffer))
	  (switch-to-buffer b)
	  (beginning-of-buffer)))
      (perform-replace from-string to-string t nil delimited nil nil
		       start end))))

(defun query-replace-regexp-ecl (from-string to-string &optional delimited start end)
  (interactive (query-replace-read-args "Query replace" nil))
  (setq ecl-search-string from-string)
  (let ((remaining (member (buffer-file-name (current-buffer)) ecl-files)))
    (dolist (i (or remaining ecl-files))
      (let ((b (find-buffer-visiting i)))
	(unless (equal b (current-buffer))
	  (switch-to-buffer b)
	  (beginning-of-buffer)))
      (query-replace-regexp from-string to-string delimited start end))))

(defun search-ecl (string)
  (interactive "sString: ")
  (setq ecl-search-string string)
  (let ((remaining (member (buffer-file-name (current-buffer)) ecl-files)))
    (dolist (i (or remaining ecl-files))
      (let ((b (find-buffer-visiting i)))
	(unless (equal b (current-buffer))
	  (print b)
	  (switch-to-buffer b)
	  (beginning-of-buffer)))
      (print '*)
      (setq case-fold-search t)
      (if (search-forward string nil t)
	  (return)))))

(defun search-next-ecl ()
  (interactive)
  (search-ecl ecl-search-string))

(defun back-to-emacs ()
  (interactive)
  (switch-to-buffer "emacs.el"))

(defun next-ecl ()
  (interactive)
  (let ((remaining (member (buffer-file-name (current-buffer)) ecl-files)))
    (when (cdr remaining)
      (switch-to-buffer (find-buffer-visiting (cadr remaining))))))

(global-set-key [?\M-p ?\C-i] 'back-to-emacs)
(global-set-key [?\M-p ?\C-s] 'search-ecl)
(global-set-key [?\M-p ?\C-n] 'search-next-ecl)
(global-set-key [?\M-p ?\C-m] 'next-ecl)
(global-set-key [?\M-p ?\C-p] 'ecl-load-symbols)

(setq auto-mode-alist (acons "\\.d\\'" 'c-mode auto-mode-alist))

(setq ecl-files
      (mapcar (lambda (x)
		(set-buffer "emacs.el")
		(concat (subseq (buffer-file-name (current-buffer)) 0 -13) x))
	      '(
"c/all_symbols.d"
"c/alloc.d"
"c/alloc_2.d"
"c/apply.d"
"c/array.d"
"c/assignment.d"
"c/backq.d"
"c/big.d"
"c/big_ll.d"
"c/cfun.d"
"c/cfun_dispatch.d"
"c/char_ctype.d"
"c/character.d"
"c/cinit.d"
"c/clos/accessor.d"
"c/clos/cache.d"
"c/cmpaux.d"
"c/compiler.d"
"c/cons.d"
"c/disassembler.d"
"c/dosdummy.d"
"c/dostimes.d"
"c/earith.d"
"c/ecl_constants.h"
"c/ecl_features.h"
"c/error.d"
"c/eval.d"
"c/ffi/backtrace.d"
"c/ffi/cdata.d"
"c/ffi/libraries.d"
"c/ffi/mmap.d"
"c/ffi.d"
"c/file.d"
"c/format.d"
"c/gbc-new.d"
"c/gbc.d"
"c/gfun.d"
"c/hash.d"
"c/instance.d"
"c/interpreter.d"
"c/iso_latin_names.h"
"c/list.d"
"c/load.d"
"c/macros.d"
"c/main.d"
"c/Makefile.in"
"c/mapfun.d"
"c/multival.d"
"c/newhash.h"
"c/num_arith.d"
"c/num_co.d"
"c/num_log.d"
"c/num_pred.d"
"c/num_rand.d"
"c/number.d"
"c/numbers/abs.d"
"c/numbers/atan.d"
"c/numbers/conjugate.d"
"c/numbers/cos.d"
"c/numbers/cosh.d"
"c/numbers/divide.d"
"c/numbers/exp.d"
"c/numbers/expt.d"
"c/numbers/float_fix_compare.d"
"c/numbers/log.d"
"c/numbers/minmax.d"
"c/numbers/minus.d"
"c/numbers/minusp.d"
"c/numbers/negate.d"
"c/numbers/number_compare.d"
"c/numbers/number_equalp.d"
"c/numbers/one_minus.d"
"c/numbers/one_plus.d"
"c/numbers/plus.d"
"c/numbers/plusp.d"
"c/numbers/sin.d"
"c/numbers/sinh.d"
"c/numbers/sqrt.d"
"c/numbers/tan.d"
"c/numbers/tanh.d"
"c/numbers/times.d"
"c/numbers/zerop.d"
"c/package.d"
"c/pathname.d"
"c/predicate.d"
"c/print.d"
"c/printer/float_string_old.d"
"c/printer/float_to_digits.d"
"c/printer/float_to_string.d"
"c/printer/integer_to_string.d"
"c/printer/print_unreadable.d"
"c/printer/write_array.d"
"c/printer/write_code.d"
"c/printer/write_list.d"
"c/printer/write_object.d"
"c/printer/write_sse.d"
"c/printer/write_symbol.d"
"c/printer/write_ugly.d"
"c/read.d"
"c/reader/parse_integer.d"
"c/reader/parse_number.d"
"c/reference.d"
"c/sequence.d"
"c/serialize.d"
"c/sse2.d"
"c/stacks.d"
"c/string.d"
"c/structure.d"
"c/symbol.d"
"c/symbols_list.h"
"c/symbols_list2.h"
"c/tcp.d"
"c/threads/atomic.d"
"c/threads/barrier.d"
"c/threads/condition_variable.d"
"c/threads/ecl_atomics.h"
"c/threads/mailbox.d"
"c/threads/mutex.d"
"c/threads/process.d"
"c/threads/queue.d"
"c/threads/rwlock.d"
"c/threads/semaphore.d"
"c/time.d"
"c/typespec.d"
"c/unify.d"
"c/unixfsys.d"
"c/unixint.d"
"c/unixsys.d"
"c/vector_push.d"
"h/bytecodes.h"
"h/cache.h"
"h/config.h.in"
"h/cons.h"
"h/cs.h"
"h/ecl-cmp.h"
"h/ecl-inl.h"
"h/ecl.h"
"h/external.h"
"h/impl/math_dispatch.h"
"h/impl/math_dispatch2.h"
"h/impl/math_fenv.h"
"h/impl/math_fenv_msvc.h"
"h/internal.h"
"h/legacy.h"
"h/number.h"
"h/object.h"
"h/page.h"
"h/stacks.h"
"h/unify.h"
"lsp/arraylib.lsp"
"lsp/assert.lsp"
"lsp/autoload.lsp"
"lsp/cdr-5.lsp"
"lsp/cmdline.lsp"
"lsp/cmpinit.lsp"
"lsp/cmuutil.lsp"
"lsp/config.lsp.in"
"lsp/defmacro.lsp"
"lsp/defpackage.lsp"
"lsp/defstruct.lsp"
"lsp/describe.lsp"
"lsp/evalmacros.lsp"
"lsp/export.lsp"
"lsp/ffi.lsp"
"lsp/format.lsp"
"lsp/helpfile.lsp"
"lsp/init.lsp"
"lsp/iolib.lsp"
"lsp/listlib.lsp"
"lsp/load.lsp.in"
"lsp/loop.lsp"
"lsp/loop2.lsp"
"lsp/mislib.lsp"
"lsp/module.lsp"
"lsp/mp.lsp"
"lsp/numlib.lsp"
"lsp/packlib.lsp"
"lsp/pprint.lsp"
"lsp/predlib.lsp"
"lsp/process.lsp"
"lsp/proclaim.lsp"
"lsp/seq.lsp"
"lsp/seqlib.lsp"
"lsp/seqmacros.lsp"
"lsp/setf.lsp"
"lsp/top.lsp"
"lsp/trace.lsp"
"lsp/unicode.lsp"
"lsp/util.lsp"
"clos/boot.lsp"
"clos/builtin.lsp"
"clos/change.lsp"
"clos/cmpinit.lsp"
"clos/combin.lsp"
"clos/conditions.lsp"
"clos/cpl.lsp"
"clos/defclass.lsp"
"clos/fixup.lsp"
"clos/generic.lsp"
"clos/hierarchy.lsp"
"clos/init.lsp"
"clos/inspect.lsp"
"clos/kernel.lsp"
"clos/load.lsp.in"
"clos/macros.lsp"
"clos/method.lsp"
"clos/package.lsp"
"clos/print.lsp"
"clos/slot.lsp"
"clos/slotvalue.lsp"
"clos/standard.lsp"
"clos/std-accessors.lsp"
"clos/std-slot-value.lsp"
"clos/stdmethod.lsp"
"clos/streams.lsp"
"clos/walk.lsp"
"cmp/cmparray.lsp"
"cmp/cmpbind.lsp"
"cmp/cmpblock.lsp"
"cmp/cmpc-inliner.lsp"
"cmp/cmpc-wt.lsp"
"cmp/cmpcall.lsp"
"cmp/cmpcatch.lsp"
"cmp/cmpcbk.lsp"
"cmp/cmpclos.lsp"
"cmp/cmpct.lsp"
"cmp/cmpdefs.lsp"
"cmp/cmpenv-api.lsp"
"cmp/cmpenv-declaim.lsp"
"cmp/cmpenv-declare.lsp"
"cmp/cmpenv-fun.lsp"
"cmp/cmpenv-proclaim.lsp"
"cmp/cmpeval.lsp"
"cmp/cmpexit.lsp"
"cmp/cmpffi.lsp"
"cmp/cmpflet.lsp"
"cmp/cmpform.lsp"
"cmp/cmpfun.lsp"
"cmp/cmpglobals.lsp"
"cmp/cmpif.lsp"
"cmp/cmpinline.lsp"
"cmp/cmplam.lsp"
"cmp/cmplet.lsp"
"cmp/cmploc.lsp"
"cmp/cmpmac.lsp"
"cmp/cmpmain.lsp"
"cmp/cmpmap.lsp"
"cmp/cmpmulti.lsp"
"cmp/cmpname.lsp"
"cmp/cmpnum.lsp"
"cmp/cmpopt-bits.lsp"
"cmp/cmpopt-clos.lsp"
"cmp/cmpopt-cons.lsp"
"cmp/cmpopt-constant.lsp"
"cmp/cmpopt-printer.lsp"
"cmp/cmpopt-sequence.lsp"
"cmp/cmpopt-type.lsp"
"cmp/cmpopt.lsp"
"cmp/cmpos-features.lsp"
"cmp/cmpos-run.lsp"
"cmp/cmppackage.lsp"
"cmp/cmppolicy.lsp"
"cmp/cmpprop.lsp"
"cmp/cmpspecial.lsp"
"cmp/cmpstack.lsp"
"cmp/cmpstructures.lsp"
"cmp/cmptables.lsp"
"cmp/cmptag.lsp"
"cmp/cmptest.lsp"
"cmp/cmptop.lsp"
"cmp/cmptype-arith.lsp"
"cmp/cmptype-assert.lsp"
"cmp/cmptype-prop.lsp"
"cmp/cmptype.lsp"
"cmp/cmptypes.lsp"
"cmp/cmputil.lsp"
"cmp/cmpvar.lsp"
"cmp/cmpwt.lsp"
"cmp/load.lsp.in"
"cmp/proclamations.lsp"
"cmp/sysfun.lsp"
"../msvc/ecl/config.h.msvc6"
                )))

(mapcar 'find-file ecl-files)

(defun ecl-revert ()
  (interactive)
  (mapcar '(lambda (x) (let ((a (find-buffer-visiting x)))
			 (and a (switch-to-buffer a)
			      (revert-buffer t t))))
	  ecl-files))

(defun ecl-save ()
  (interactive)
  (mapcar '(lambda (x) (let ((a (find-buffer-visiting x)))
			 (and a (switch-to-buffer a)
			      (save-buffer 0))))
	  ecl-files))
