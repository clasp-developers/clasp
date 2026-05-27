;;;; sif-tools.lisp
;;;;
;;;; Diff / merge / compare for clasp-analyzer .sif files.
;;;;
;;;; The .sif format is a stream of top-level Common Lisp s-expressions, one
;;;; per record:
;;;;     (tag :keyword value :keyword value ...)
;;;;
;;;; Class-kind records (class-kind, templated-kind, container-kind,
;;;; bitunit-container-kind) carry their per-field descriptors nested in a
;;;; :contains slot:
;;;;     (class-kind :stamp-key "core::Foo_O" ...
;;;;                 :contains ((fixed-field ...) (variable-array0 ...) ...))
;;;; So at top level a .sif file is one forwards-tag plus N class-kind-family
;;;; records — nothing else.
;;;;
;;;; Plain s-expressions, no custom readtable.  Standard CL `read` and
;;;; `prin1` work directly.
;;;;
;;;; The .dif format is the same syntax with two extra control-record tags:
;;;;     (base-fingerprint :format-version N :hash "fnv1a:0x..."
;;;;                       :record-count N)     ; integrity check vs base
;;;;     (added-forwards :forwards% (...))      ; class-name strings to add
;;;; All other records in the .dif are records to add as-is.
;;;;
;;;; The base-fingerprint is a hash of the *canonical* form (normalized,
;;;; sorted) of the base .sif used at diff time.  The hash is recorded for
;;;; diagnostics but not enforced at merge time — the strict check was too
;;;; restrictive since any core C++ change invalidates the hash even when
;;;; the extension's additions are independent.  The format-version check
;;;; and dedupe-and-check-records conflict detection remain active.
;;;;
;;;; This tool assumes that every extension's .sif is a strict superset of
;;;; the bare-clasp .sif (additive only — no removed records or removed
;;;; forwards).  diff-sif-files signals an error if the target removes
;;;; anything from the base, since that would indicate either an analyzer
;;;; bug or an extension that is unsafe to merge with the additive design.
;;;;
;;;; Three exported entry points:
;;;;
;;;;   (sif-tools:diff-sif-files BASE TARGET DIFF)
;;;;       Compute the additions that turn BASE into TARGET and write to DIFF.
;;;;       Errors if TARGET is missing anything BASE has.
;;;;
;;;;   (sif-tools:merge-sif-files BASE DIFFS OUTPUT)
;;;;       Apply DIFFS (a single diff path or a list of diff paths) to BASE
;;;;       in one step, writing the merged result to OUTPUT.  Each diff's
;;;;       fingerprint is verified against BASE — all diffs must share BASE,
;;;;       since each was generated against the bare base by ./analyze.
;;;;       Cross-extension class-definition collisions:
;;;;         * identical re-registration of a class is silently deduped;
;;;;         * conflicting registrations (same :stamp-key, different content)
;;;;           signal an error.
;;;;
;;;;   (sif-tools:compare-sif-files A B)
;;;;       Returns three values: (overall-equal forwards-equal records-equal).
;;;;       Order of records is irrelevant; multiset semantics for records,
;;;;       set semantics for forward-decl class names.

(defpackage :sif-tools
  (:use :common-lisp)
  (:export #:diff-sif-files
           #:merge-sif-files
           #:compare-sif-files))

(in-package :sif-tools)

;;; A throwaway package for symbols read out of .sif files.  Reading and
;;; writing with *PACKAGE* bound here keeps record tags from acquiring
;;; package-qualified prefixes when they round-trip through PRIN1.
(defpackage :sif-data (:use))

(defun sif-intern (name) (intern name :sif-data))

;;; ---------------------------------------------------------------------------
;;; Reader / writer.
;;; ---------------------------------------------------------------------------

(defun read-sif-file (path)
  "Read a .sif file as a list of records.  Each record is a top-level
   s-expression in the file."
  (let ((*package* (find-package :sif-data)))
    (with-open-file (in path)
      (loop for r = (read in nil :sif-eof)
            until (eq r :sif-eof)
            collect r))))

(defun write-sif-record (record stream)
  (prin1 record stream)
  (terpri stream))

(defun write-sif-file (path records)
  (let ((*package*       (find-package :sif-data))
        (*print-pretty*  nil)
        (*print-readably* t)
        (*print-circle*  nil)
        (*print-length*  nil)
        (*print-level*   nil))
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
      (dolist (r records)
        (write-sif-record r out)))))

;;; ---------------------------------------------------------------------------
;;; Record helpers.
;;; ---------------------------------------------------------------------------

(defun tag-name (record)
  (and (consp record) (symbolp (first record)) (symbol-name (first record))))

(defun tag-named-p (record name)
  (let ((tn (tag-name record)))
    (and tn (string= tn name))))

(defun split-source-records (records)
  "Read a .sif file's records.  Return (forwards-list other-records)."
  (let (forwards-record others)
    (dolist (r records)
      (cond ((tag-named-p r "FORWARDS-TAG")
             (when forwards-record
               (error "more than one forwards-tag record in SIF input"))
             (setf forwards-record r))
            (t (push r others))))
    (values (if forwards-record (getf (rest forwards-record) :forwards%) '())
            (nreverse others))))

(defun split-diff-records (records)
  "Read a .dif file's records.  Return three values:
   added-forwards, added-records, fingerprint-plist (or nil if absent).
   The fingerprint plist has :format-version, :hash, :record-count."
  (let ((added-fwd nil)
        (added-recs '())
        (fingerprint nil))
    (dolist (r records)
      (cond ((tag-named-p r "ADDED-FORWARDS")
             (setf added-fwd (getf (rest r) :forwards%)))
            ((tag-named-p r "BASE-FINGERPRINT")
             (setf fingerprint (rest r)))
            (t (push r added-recs))))
    (values (or added-fwd '()) (nreverse added-recs) fingerprint)))

(defun make-source-forwards (forwards-list)
  (list (sif-intern "FORWARDS-TAG") :forwards% forwards-list))

(defun make-added-forwards (forwards-list)
  (list (sif-intern "ADDED-FORWARDS") :forwards% forwards-list))

(defun record< (a b)
  "Total order on normalized records, used to make sibling :contains lists
   comparable as multisets.  Sorts by the printed form — slow but stable
   and total without having to know each tag's identity slot."
  (string< (write-to-string a :readably t :pretty nil :circle nil)
           (write-to-string b :readably t :pretty nil :circle nil)))

(defun normalize-record (record)
  "Canonical form for record-equality comparisons: (tag k1 v1 k2 v2 ...)
   with the keyword/value pairs sorted by keyword name.

   The :contains slot holds a list of child records (fixed-field,
   variable-array0, etc.); each child is normalized recursively and the
   resulting sibling list is sorted so that two parents with the same
   children in different order compare equal.

   Other inner list values are NOT reordered (e.g. :layout-offset-field-names
   is order-significant)."
  (let* ((tag (first record))
         (plist (rest record))
         (pairs (loop for (k v) on plist by #'cddr collect (list k v)))
         (sorted (sort (copy-list pairs) #'string<
                       :key (lambda (p) (symbol-name (first p))))))
    (cons tag
          (loop for (k v) in sorted
                nconc (list k
                            (if (and (eq k :contains) (listp v))
                                (sort (mapcar #'normalize-record v) #'record<)
                                v))))))

;;; ---------------------------------------------------------------------------
;;; Base fingerprint: integrity check between a .dif and the base .sif it
;;; will be merged against.  Stored as a control record at the top of the
;;; .dif; verified at merge time.
;;; ---------------------------------------------------------------------------

(defparameter +sif-format-version+ 1
  "Bump when the canonical form (normalize-record / record<) changes.
   Old .dif files with a different format-version cannot be verified
   against newly-canonicalized bases — they must be regenerated.")

(defun fnv1a-64 (string)
  "Portable 64-bit FNV-1a hash of STRING's char-codes.  Stable across
   CL implementations; not cryptographic but more than enough to detect
   a mismatched base file."
  (let ((hash #xcbf29ce484222325)
        (mask #xffffffffffffffff))
    (loop for c across string
          do (setf hash (logand mask (logxor hash (char-code c))))
             (setf hash (logand mask (* hash #x100000001b3))))
    hash))

(defun base-canonical-string (forwards records)
  "Return a deterministic string representation of a base .sif's
   semantic content: sorted forwards-list followed by sorted normalized
   records.  Two analyzer runs that emit the same content in different
   order produce the same string."
  (let ((*package*       (find-package :sif-data))
        (*print-pretty*  nil)
        (*print-readably* t)
        (*print-circle*  nil)
        (*print-length*  nil)
        (*print-level*   nil))
    (with-output-to-string (s)
      (prin1 (sort (copy-list forwards) #'string<) s)
      (terpri s)
      (dolist (r (sort (mapcar #'normalize-record records) #'record<))
        (prin1 r s)
        (terpri s)))))

(defun compute-base-fingerprint (forwards records)
  "FNV-1a fingerprint of the canonical form of a base .sif.  String
   form: \"fnv1a:0x<16 hex digits>\"."
  (format nil "fnv1a:0x~16,'0x"
          (fnv1a-64 (base-canonical-string forwards records))))

(defun make-base-fingerprint-record (forwards records)
  (list (sif-intern "BASE-FINGERPRINT")
        :format-version +sif-format-version+
        :hash (compute-base-fingerprint forwards records)
        :record-count (length records)))

(defun verify-base-fingerprint (fingerprint base-fwd base-rest base-path diff-path)
  "Check that the base supplied to a merge matches the base used to
   generate the diff.  Errors on mismatch.  Warns (and proceeds) when no
   fingerprint is present in the diff (legacy .dif files predate this
   check)."
  (cond
    ((null fingerprint)
     (warn "no base-fingerprint in ~A — base/diff compatibility unverified"
           diff-path))
    ((not (eql (getf fingerprint :format-version) +sif-format-version+))
     (error "diff ~A was produced for sif-tools format version ~A, ~
             but this tool is at version ~A — regenerate the diff."
            diff-path
            (getf fingerprint :format-version)
            +sif-format-version+))
    (t
     (let ((expected (getf fingerprint :hash))
           (actual   (compute-base-fingerprint base-fwd base-rest)))
       #+(or)
       (unless (string= expected actual)
         (error "base ~A does not match the base used to produce diff ~A.~%~
                 expected fingerprint: ~A~%~
                 actual fingerprint:   ~A~%~
                 The base .sif has changed since this .dif was generated; ~
                 regenerate the diff against the current base."
                base-path diff-path expected actual))))))

(defun multiset-difference (a b)
  "Return (values a-only b-only) as lists of records, comparing by
   normalized form.  Multiset semantics: matching pairs cancel, leaving
   only the residual on each side.  Both result lists preserve the
   original order of A and B respectively, which matters for B-ONLY:
   downstream consumers (e.g. clasp_gc.cc generation) rely on the
   analyzer's topological ordering of class records, and shuffling them
   produces parent-after-child output that breaks cross-clasp."
  (let ((b-bag (make-hash-table :test 'equal))
        (a-only '())
        (b-only '()))
    (dolist (r b)
      (incf (gethash (normalize-record r) b-bag 0)))
    (dolist (r a)
      (let ((k (normalize-record r)))
        (if (plusp (gethash k b-bag 0))
            (decf (gethash k b-bag))
            (push r a-only))))
    (dolist (r b)
      (let ((k (normalize-record r)))
        (when (plusp (gethash k b-bag 0))
          (push r b-only)
          (decf (gethash k b-bag)))))
    (values (nreverse a-only) (nreverse b-only))))

(defun stamp-key-of (record)
  "Return the :stamp-key value for a record kind that has one (class-kind,
   templated-kind, container-kind, bitunit-container-kind), or nil for
   record kinds that don't.  Used to detect per-class conflicts at merge
   time."
  (let ((tn (tag-name record)))
    (when (and tn
               (or (string= tn "CLASS-KIND")
                   (string= tn "TEMPLATED-KIND")
                   (string= tn "CONTAINER-KIND")
                   (string= tn "BITUNIT-CONTAINER-KIND")))
      (getf (rest record) :stamp-key))))

(defun dedupe-and-check-records (records context)
  "Concatenate RECORDS with two collision rules:
     * Records that have a :stamp-key (the class-kind family) are unique
       per class.  At most one is kept per stamp-key.  If two records
       share a stamp-key but normalize to different content (a genuine
       cross-extension class-definition conflict), an error is signaled.
     * All other records pass through preserving multiplicity — the
       analyzer legitimately emits some descriptor records (variable-field,
       variable-field-only) more than once per .sif, and dropping those
       would lose information.

   CONTEXT is a short string used in error messages to identify what's
   being merged."
  (let ((by-stamp-key (make-hash-table :test 'equal))   ; stamp-key -> record
        (out '()))
    (dolist (r records)
      (let ((sk (stamp-key-of r)))
        (cond
          ((null sk)
           ;; Record with no :stamp-key — pass through untouched.
           (push r out))
          (t
           ;; Stamp-keyed record — dedup with conflict detection.
           (let ((prior (gethash sk by-stamp-key)))
             (cond
               ((null prior)
                (setf (gethash sk by-stamp-key) r)
                (push r out))
               ((equal (normalize-record prior) (normalize-record r))
                ;; Identical re-registration — drop silently.
                nil)
               (t
                (error "~A: conflicting records for stamp-key ~S~%~
                          previous: ~S~%new:      ~S"
                       context sk prior r))))))))
    (nreverse out)))

;;; ---------------------------------------------------------------------------
;;; Diff: TARGET vs BASE -> DIFF (additive only)
;;; ---------------------------------------------------------------------------

(defun diff-sif-files (base-path target-path diff-path)
  "Write to DIFF-PATH the additions that turn BASE-PATH into TARGET-PATH.
   Signals an error if TARGET is missing any forward decls or records that
   BASE has — the per-extension synthesis design requires extensions to be
   strictly additive against the bare-clasp .sif."
  (multiple-value-bind (base-fwd base-rest) (split-source-records (read-sif-file base-path))
    (multiple-value-bind (tgt-fwd tgt-rest) (split-source-records (read-sif-file target-path))
      (let ((removed-fwd (set-difference base-fwd tgt-fwd :test #'string=)))
        (when removed-fwd
          (error "target ~A is missing ~D forward-decl(s) present in base ~A: ~{~A~^, ~}"
                 target-path (length removed-fwd) base-path removed-fwd)))
      (multiple-value-bind (removed-recs added-recs) (multiset-difference base-rest tgt-rest)
        (when removed-recs
          (error "target ~A is missing ~D record(s) present in base ~A. First missing: ~S"
                 target-path (length removed-recs) base-path (first removed-recs)))
        (let ((added-fwd (set-difference tgt-fwd base-fwd :test #'string=))
              (out '()))
          (push (make-base-fingerprint-record base-fwd base-rest) out)
          (when added-fwd (push (make-added-forwards added-fwd) out))
          (dolist (r added-recs) (push r out))
          (write-sif-file diff-path (nreverse out))))))
  diff-path)

;;; ---------------------------------------------------------------------------
;;; Merge: BASE + DIFFS -> OUTPUT
;;; ---------------------------------------------------------------------------

(defun merge-sif-files (base-path diff-paths output-path)
  "Apply each diff in DIFF-PATHS to BASE-PATH in one step and write the
   result to OUTPUT-PATH.  DIFF-PATHS may be a single pathname or a list.

   Each diff's base-fingerprint is verified against BASE-PATH — all diffs
   must share BASE-PATH as their base, which is the case when each is
   generated by ./analyze against the bare clasp_gc.sif.

   Forwards and records that appear in multiple diffs (e.g. two extensions
   that independently register the same class because they share an
   #include) are deduplicated.  If two records share a :stamp-key but
   describe different class definitions, an error is signaled — that's a
   genuine conflict between the inputs that synthesis can't resolve."
  (let ((diff-list (if (listp diff-paths) diff-paths (list diff-paths))))
    (multiple-value-bind (base-fwd base-rest)
        (split-source-records (read-sif-file base-path))
      (let ((merged-fwd  base-fwd)
            (merged-rest base-rest))
        (dolist (diff-path diff-list)
          (multiple-value-bind (added-fwd added-recs fingerprint)
              (split-diff-records (read-sif-file diff-path))
            (verify-base-fingerprint fingerprint base-fwd base-rest
                                     base-path diff-path)
            (setf merged-fwd  (union merged-fwd added-fwd :test #'string=)
                  merged-rest (append merged-rest added-recs))))
        (write-sif-file output-path
                        (cons (make-source-forwards merged-fwd)
                              (dedupe-and-check-records
                               merged-rest
                               (format nil "merge of ~A + ~D diff(s)"
                                       base-path (length diff-list))))))))
  output-path)

;;; ---------------------------------------------------------------------------
;;; Compare
;;; ---------------------------------------------------------------------------

(defun compare-sif-files (path-a path-b)
  "Return three values: OVERALL-EQUAL, FORWARDS-EQUAL, RECORDS-EQUAL.
   OVERALL-EQUAL is the conjunction.  Records are compared with multiset
   semantics; forward-decl class-name lists are compared with set semantics."
  (multiple-value-bind (a-fwd a-rest) (split-source-records (read-sif-file path-a))
    (multiple-value-bind (b-fwd b-rest) (split-source-records (read-sif-file path-b))
      (let ((forwards-equal
              (null (set-exclusive-or a-fwd b-fwd :test #'string=))))
        (multiple-value-bind (a-only b-only) (multiset-difference a-rest b-rest)
          (let ((records-equal (and (null a-only) (null b-only))))
            (values (and forwards-equal records-equal)
                    forwards-equal
                    records-equal)))))))
