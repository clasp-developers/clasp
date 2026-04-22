;;; flamegraph.lisp — Common Lisp port of Brendan Gregg's flamegraph.pl.
;;;
;;; Derived from https://github.com/brendangregg/FlameGraph
;;; Original copyright: 2011 Joyent, 2011 Brendan Gregg, 2016 Netflix.
;;; License: CDDL 1.0 (same as the Perl original).
;;;
;;; Updated by Christian Schafmeister (April 2026) to
;;;   support clasp colors and frame filters
;;;
;;; Usage from a shell (SBCL):
;;;   sbcl --script flamegraph.lisp [options] input.txt > graph.svg
;;; From a Lisp REPL:
;;;   (load "flamegraph.lisp")
;;;   (flamegraph:main '("--title" "My Profile" "/tmp/prof.collapsed"))
;;;
;;; Input format: one line per sample, semicolon-separated frames then a
;;; space and an integer count:
;;;
;;;   main;foo;bar 42
;;;   main;foo;baz 17
;;;
;;; Output: SVG with interactive zoom/search JavaScript on stdout.
;;;
;;; Scope differences vs the Perl original:
;;;   * Integer counts only. Fractional counts in the input are dropped.
;;;   * No differential flame graphs (no second samples column).
;;;   * No --nameattr (per-function attribute file).
;;;   * No --cp / --pal (consistent palette persistence).
;;;   * No "chain" waker annotation.
;;; Everything else (palettes, --reverse, --inverted, --hash, --negate,
;;; --title, --subtitle, --minwidth, zoom/search JS) ported faithfully.
;;;
;;; Only uses ANSI Common Lisp. No external libraries.

(defpackage #:flamegraph
  (:use #:cl)
  (:export #:main #:flamegraph))

(in-package #:flamegraph)

;;; ---------------------------------------------------------------------------
;;; Options
;;; ---------------------------------------------------------------------------

(defstruct opts
  (title "")
  (subtitle "")
  (image-width 1200)
  (frame-height 16)
  (font-type "Verdana")
  (font-size 12)
  (font-width 0.59)
  (min-width 0.1)
  (name-type "Function:")
  (count-name "samples")
  (colors "hot")
  (bg-color1 "#eeeeee")
  (bg-color2 "#eeeeb0")
  (total nil)
  (factor 1.0)
  (hash nil)
  (reverse-stack nil)
  (inverted nil)
  (negate nil)
  (notes "")
  (encoding nil)
  (search-color "rgb(230,0,230)")
  (input-file nil))

;;; ---------------------------------------------------------------------------
;;; Command-line
;;; ---------------------------------------------------------------------------

(defun cmdline-args ()
  #+sbcl (cdr sb-ext:*posix-argv*)
  #+ccl (cdr ccl:*command-line-argument-list*)
  #+ecl (cdr (si:command-args))
  #+clisp ext:*args*
  #+clasp (cdr core:*command-line-arguments*)
  #+abcl (cdr ext:*command-line-argument-list*)
  #-(or sbcl ccl ecl clisp clasp abcl) nil)

(defun usage ()
  (format *error-output* "~%USAGE: flamegraph.lisp [options] [input.txt] > out.svg~%~%~
   --title TEXT     change title text~%~
   --subtitle TEXT  second-level title~%~
   --width NUM      image width (default 1200)~%~
   --height NUM     frame height (default 16)~%~
   --minwidth NUM   omit smaller functions (default 0.1 px)~%~
   --fonttype FONT  font type (default Verdana)~%~
   --fontsize NUM   font size (default 12)~%~
   --fontwidth NUM  avg width relative to fontsize (default 0.59)~%~
   --countname TEXT count label (default samples)~%~
   --nametype TEXT  name label (default Function:)~%~
   --colors NAME    palette: hot mem io wakeup chain java js perl clasp~%~
                    red green blue aqua yellow purple orange~%~
   --hash           color by function-name hash (stable across runs)~%~
   --reverse        reverse each stack before merging~%~
   --inverted       icicle (root at top)~%~
   --negate         switch differential hues~%~
   --notes TEXT     embed a note in the SVG~%~
   --total NUM      override total (max) count~%~
   --factor NUM     scale counts~%~
   --encoding ENC   SVG XML encoding attribute~%~
   --help           this message~%"))

(defun parse-args (args)
  (let ((o (make-opts))
        (remaining (copy-list args)))
    (loop while remaining
          for a = (pop remaining)
          do (cond
               ((string= a "--title")     (setf (opts-title o) (pop remaining)))
               ((string= a "--subtitle")  (setf (opts-subtitle o) (pop remaining)))
               ((string= a "--width")     (setf (opts-image-width o) (parse-integer (pop remaining))))
               ((string= a "--height")    (setf (opts-frame-height o) (parse-integer (pop remaining))))
               ((string= a "--minwidth")  (setf (opts-min-width o) (read-from-string (pop remaining))))
               ((string= a "--fonttype")  (setf (opts-font-type o) (pop remaining)))
               ((string= a "--fontsize")  (setf (opts-font-size o) (read-from-string (pop remaining))))
               ((string= a "--fontwidth") (setf (opts-font-width o) (read-from-string (pop remaining))))
               ((string= a "--countname") (setf (opts-count-name o) (pop remaining)))
               ((string= a "--nametype")  (setf (opts-name-type o) (pop remaining)))
               ((string= a "--colors")    (setf (opts-colors o) (pop remaining)))
               ((string= a "--total")     (setf (opts-total o) (parse-integer (pop remaining))))
               ((string= a "--factor")    (setf (opts-factor o) (read-from-string (pop remaining))))
               ((string= a "--notes")     (setf (opts-notes o) (pop remaining)))
               ((string= a "--encoding")  (setf (opts-encoding o) (pop remaining)))
               ((string= a "--hash")      (setf (opts-hash o) t))
               ((string= a "--reverse")   (setf (opts-reverse-stack o) t))
               ((string= a "--inverted")  (setf (opts-inverted o) t))
               ((string= a "--negate")    (setf (opts-negate o) t))
               ((or (string= a "--help") (string= a "-h"))
                (usage)
                (return-from parse-args nil))
               ((and (plusp (length a)) (char= (schar a 0) #\-))
                (format *error-output* "Unknown option: ~A~%" a)
                (usage)
                (return-from parse-args nil))
               (t
                (setf (opts-input-file o) a))))
    ;; Background color selection driven by palette.
    (cond
      ((or (string= (opts-colors o) "mem") (string= (opts-colors o) "chain"))
       (setf (opts-bg-color1 o) "#eeeeee"
             (opts-bg-color2 o) "#e0e0ff"))
      ((member (opts-colors o)
               '("io" "wakeup" "red" "green" "blue" "aqua" "yellow" "purple" "orange")
               :test #'string=)
       (setf (opts-bg-color1 o) "#f8f8f8"
             (opts-bg-color2 o) "#e8e8e8")))
    (when (and (plusp (length (opts-notes o)))
               (or (find #\< (opts-notes o)) (find #\> (opts-notes o))))
      (error "Notes string can't contain < or >"))
    (when (zerop (length (opts-title o)))
      (setf (opts-title o)
            (if (opts-inverted o) "Icicle Graph" "Flame Graph")))
    o))

;;; ---------------------------------------------------------------------------
;;; Utilities
;;; ---------------------------------------------------------------------------

(defun xml-escape (s)
  (with-output-to-string (out)
    (loop for c across s
          do (case c
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (t (write-char c out))))))

(defun split-string (s sep)
  "Split S at every occurrence of SEP (a character). Returns a list of
substrings — empty substrings are preserved."
  (loop with start = 0
        with len = (length s)
        for pos = (position sep s :start start)
        if pos
          collect (subseq s start pos) into parts
          and do (setf start (1+ pos))
        else
          return (nconc parts (list (subseq s start len)))))

(defun join (list sep)
  (with-output-to-string (out)
    (loop for cell on list
          do (write-string (car cell) out)
             (when (cdr cell) (write-char sep out)))))

(defun list-string< (a b)
  "Lexicographic comparison of two lists of strings."
  (loop
    (cond ((null a) (return (not (null b))))
          ((null b) (return nil))
          ((string< (car a) (car b)) (return t))
          ((string< (car b) (car a)) (return nil))
          (t (pop a) (pop b)))))

(defun format-with-commas (n)
  "Format integer N with thousands-separator commas."
  (let* ((s (princ-to-string n))
         (neg (and (plusp (length s)) (char= (schar s 0) #\-)))
         (digits (if neg (subseq s 1) s))
         (dlen (length digits))
         (first-group (or (mod dlen 3) 0)))
    (when (zerop first-group) (setf first-group 3))
    (with-output-to-string (out)
      (when neg (write-char #\- out))
      (loop for i below dlen
            do (when (and (plusp i) (zerop (mod (- i first-group) 3)))
                 (write-char #\, out))
               (write-char (schar digits i) out)))))

(defun suffix-p (s suffix)
  (and (>= (length s) (length suffix))
       (string= s suffix :start1 (- (length s) (length suffix)))))

(defun starts-with-p (s prefix)
  (and (>= (length s) (length prefix))
       (string= s prefix :end1 (length prefix))))

(defun strip-frame-annotation (name)
  "Strip trailing _[k], _[w], _[i], _[j] annotation if present."
  (let ((len (length name)))
    (if (and (>= len 4)
             (char= (schar name (- len 4)) #\_)
             (char= (schar name (- len 3)) #\[)
             (find (schar name (- len 2)) "kwij")
             (char= (schar name (- len 1)) #\]))
        (subseq name 0 (- len 4))
        name)))

;;; ---------------------------------------------------------------------------
;;; Color palettes
;;; ---------------------------------------------------------------------------

(defun namehash (name)
  "Produce a [0,1) float from NAME, weighting early characters heavier
so semantically-related names get similar colors."
  (let* ((tick (position #\` name))
         (name (if tick (subseq name (1+ tick)) name))
         (vector 0.0)
         (weight 1.0)
         (maxw 1.0)
         (modulus 10))
    (loop for c across name
          while (<= modulus 12)
          do (let ((i (mod (char-code c) modulus)))
               (incf vector (* (/ (float i) (float (1- (incf modulus)))) weight))
               (incf maxw weight)
               (setf weight (* weight 0.7))))
    (- 1.0 (/ vector maxw))))

(defun rand-float () (random 1.0))

(defun rgb (r g b) (format nil "rgb(~D,~D,~D)" r g b))

(defun color (type hash-p name)
  (multiple-value-bind (v1 v2 v3)
      (if hash-p
          (let ((h1 (namehash name))
                (h2 (namehash (reverse name))))
            (values h1 h2 h2))
          (values (rand-float) (rand-float) (rand-float)))
    ;; "Multi" palettes remap by inspecting the frame name, then fall
    ;; through to the single-color palettes.
    (setf type (remap-type type name))
    (cond
      ((equal type "hot")    (rgb (+ 205 (floor (* 50 v3)))
                                  (floor (* 230 v1))
                                  (floor (* 55 v2))))
      ((equal type "mem")    (rgb 0
                                  (+ 190 (floor (* 50 v2)))
                                  (floor (* 210 v1))))
      ((equal type "io")     (let ((r (+ 80 (floor (* 60 v1)))))
                               (rgb r r (+ 190 (floor (* 55 v2))))))
      ((equal type "red")    (let ((r (+ 200 (floor (* 55 v1))))
                                   (x (+ 50 (floor (* 80 v1)))))
                               (rgb r x x)))
      ((equal type "green")  (let ((g (+ 200 (floor (* 55 v1))))
                                   (x (+ 50 (floor (* 60 v1)))))
                               (rgb x g x)))
      ((equal type "blue")   (let ((b (+ 205 (floor (* 50 v1))))
                                   (x (+ 80 (floor (* 60 v1)))))
                               (rgb x x b)))
      ((equal type "yellow") (let ((x (+ 175 (floor (* 55 v1))))
                                   (b (+ 50 (floor (* 20 v1)))))
                               (rgb x x b)))
      ((equal type "purple") (let ((x (+ 190 (floor (* 65 v1))))
                                   (g (+ 80 (floor (* 60 v1)))))
                               (rgb x g x)))
      ((equal type "aqua")   (rgb (+ 50 (floor (* 60 v1)))
                                  (+ 165 (floor (* 55 v1)))
                                  (+ 165 (floor (* 55 v1)))))
      ((equal type "orange") (rgb (+ 190 (floor (* 65 v1)))
                                  (+ 90 (floor (* 65 v1)))
                                  0))
      (t "rgb(0,0,0)"))))

(defun remap-type (type name)
  (cond
    ((equal type "clasp")
     (cond
       ((search "_bct" name) "blue")
       ((starts-with-p name "bytecode_call") "blue")
       ((search "::" name) "green")
       ((search "bytecode_vm" name) "green")
       ((starts-with-p name "`GC_") "red")
       ((starts-with-p name "GC_") "red")
       ((suffix-p name "^^") "green")
       ((find #\^ name) "green")
       ((suffix-p name "_[k]") "orange")
       (t "yellow")))
    ((equal type "java")
     (cond ((suffix-p name "_[j]") "green")
           ((suffix-p name "_[i]") "aqua")
           ((some (lambda (p) (starts-with-p name p))
                  '("java/" "org/" "com/" "io/" "sun/"
                    "Ljava/" "Lorg/" "Lcom/" "Lio/" "Lsun/"))
            "green")
           ((suffix-p name "_[k]") "orange")
           ((search "::" name) "yellow")
           (t "red")))
    ((equal type "perl")
     (cond ((search "::" name) "yellow")
           ((or (search "Perl" name) (search ".pl" name)) "green")
           ((suffix-p name "_[k]") "orange")
           (t "red")))
    ((equal type "js")
     (cond ((suffix-p name "_[j]")
            (if (find #\/ name) "green" "aqua"))
           ((search "::" name) "yellow")
           ((and (find #\/ name) (search ".js" name)) "green")
           ((find #\: name) "aqua")
           ((string= name " ") "green")
           ((search "_[k]" name) "orange")
           (t "red")))
    ((equal type "wakeup") "aqua")
    ((equal type "chain")
     (if (search "_[w]" name) "aqua" "blue"))
    (t type)))

(defun color-scale (value maxv negate-p)
  "Differential color: blue for negative, red for positive, scaled by MAXV."
  (let ((r 255) (g 255) (b 255)
        (v (if negate-p (- value) value)))
    (cond ((> v 0) (setf g (floor (* 210 (/ (- maxv v) (float maxv))))
                         b g))
          ((< v 0) (setf r (floor (* 210 (/ (+ maxv v) (float maxv))))
                         g r)))
    (rgb r g b)))

;;; ---------------------------------------------------------------------------
;;; Input
;;; ---------------------------------------------------------------------------

(defun parse-folded-line (line)
  "Return (values FRAMES COUNT) from a collapsed-stacks line, where
FRAMES is a list of frame-name strings (outermost-first), or
(values NIL NIL) if the line can't be parsed."
  (let ((line (string-right-trim '(#\Space #\Tab #\Return) line)))
    (when (plusp (length line))
      (let ((sp (position #\Space line :from-end t)))
        (when sp
          (let ((num-str (subseq line (1+ sp)))
                (stack (string-right-trim '(#\Space #\Tab)
                                          (subseq line 0 sp))))
            (when (and (plusp (length num-str))
                       (every #'digit-char-p num-str))
              (values (split-string stack #\;) (parse-integer num-str)))))))))

(defun read-input (stream)
  "Return a list of (STACK . COUNT) pairs from STREAM."
  (let ((out '())
        (ignored 0))
    (loop for line = (read-line stream nil nil)
          while line
          do (multiple-value-bind (stack count) (parse-folded-line line)
               (if (and stack count)
                   (push (cons stack count) out)
                   (incf ignored))))
    (when (plusp ignored)
      (format *error-output* "Ignored ~D lines with invalid format~%" ignored))
    (nreverse out)))

(defun transform-sample (sym-sample)
  ;; sym-sample layout: #(thread-id sample-count #(frame ...))
  (values (coerce (aref sym-sample 2) 'list)
          (aref sym-sample 1)))

(defun transform-symbolicated-samples (sym-samples)
  (let ((out nil))
    (loop for sym-sample across sym-samples
          do (multiple-value-bind (stack count) (transform-sample sym-sample)
               (push (cons stack count) out)))
    (nreverse out)))

(defun reverse-stack-frames (stack)
  (reverse stack))

;;; ---------------------------------------------------------------------------
;;; Flow / merge
;;; ---------------------------------------------------------------------------

(defstruct node
  func
  depth
  stime
  etime
  delta)

(defun flow (last this v nodes tmp delta)
  "Merge stack vectors LAST and THIS at time V. Closes frames from LAST
that differ from THIS (recording them in NODES keyed by func;depth;etime)
and opens new frames from THIS in TMP. Returns THIS."
  (let* ((len-a (1- (length last)))
         (len-b (1- (length this)))
         (len-same 0))
    (loop for i from 0
          while (and (<= i len-a) (<= i len-b)
                     (string= (aref last i) (aref this i)))
          do (incf len-same))
    (loop for i from len-a downto len-same
          do (let* ((func (aref last i))
                    (k (format nil "~A;~D" func i))
                    (partial (gethash k tmp)))
               (when partial
                 (setf (gethash (format nil "~A;~D" k v) nodes)
                       (make-node :func func :depth i
                                  :stime (node-stime partial)
                                  :etime v
                                  :delta (node-delta partial)))
                 (remhash k tmp))))
    (loop for i from len-same to len-b
          do (let* ((func (aref this i))
                    (k (format nil "~A;~D" func i)))
               (setf (gethash k tmp)
                     (make-node :func func :depth i :stime v
                                :delta (when delta
                                         (if (= i len-b) delta 0))))))
    this))

;;; ---------------------------------------------------------------------------
;;; Embedded JavaScript (zoom/search) — ported verbatim from flamegraph.pl
;;; with placeholders for $xpad, $fontsize, $fontwidth, $inverted, $nametype,
;;; $searchcolor substituted at emission time.
;;; ---------------------------------------------------------------------------

(defun embedded-js (opts)
  (let ((xpad 10)
        (fontsize (opts-font-size opts))
        (fontwidth (opts-font-width opts))
        (inverted (if (opts-inverted opts) 1 0))
        (nametype (opts-name-type opts))
        (searchcolor (opts-search-color opts)))
    (format nil "~
<style type=\"text/css\">
.func_g:hover { stroke:black; stroke-width:0.5; cursor:pointer; }
</style>
<script type=\"text/ecmascript\">
<![CDATA[
        var details, searchbtn, matchedtxt, svg;
        function init(evt) {
                details = document.getElementById(\"details\").firstChild;
                searchbtn = document.getElementById(\"search\");
                matchedtxt = document.getElementById(\"matched\");
                svg = document.getElementsByTagName(\"svg\")[0];
                searching = 0;
        }

        function s(node) {
                info = g_to_text(node);
                details.nodeValue = \"~A \" + info;
        }
        function c() { details.nodeValue = ' '; }

        window.addEventListener(\"keydown\",function (e) {
                if (e.keyCode === 114 || (e.ctrlKey && e.keyCode === 70)) {
                        e.preventDefault();
                        search_prompt();
                }
        })

        function find_child(parent, name, attr) {
                var children = parent.childNodes;
                for (var i=0; i<children.length;i++) {
                        if (children[i].tagName == name)
                                return (attr != undefined) ? children[i].attributes[attr].value : children[i];
                }
                return;
        }
        function orig_save(e, attr, val) {
                if (e.attributes[\"_orig_\"+attr] != undefined) return;
                if (e.attributes[attr] == undefined) return;
                if (val == undefined) val = e.attributes[attr].value;
                e.setAttribute(\"_orig_\"+attr, val);
        }
        function orig_load(e, attr) {
                if (e.attributes[\"_orig_\"+attr] == undefined) return;
                e.attributes[attr].value = e.attributes[\"_orig_\"+attr].value;
                e.removeAttribute(\"_orig_\"+attr);
        }
        function g_to_text(e) {
                var text = find_child(e, \"title\").firstChild.nodeValue;
                return (text)
        }
        function g_to_func(e) {
                var func = g_to_text(e);
                return (func);
        }
        function update_text(e) {
                var r = find_child(e, \"rect\");
                var t = find_child(e, \"text\");
                var w = parseFloat(r.attributes[\"width\"].value) -3;
                var txt = find_child(e, \"title\").textContent.replace(/\\\\([^(]*\\\\)$/,\"\");
                t.attributes[\"x\"].value = parseFloat(r.attributes[\"x\"].value) +3;
                if (w < 2*~D*~F) { t.textContent = \"\"; return; }
                t.textContent = txt;
                if (/^ *$/.test(txt) || t.getSubStringLength(0, txt.length) < w) return;
                for (var x=txt.length-2; x>0; x--) {
                        if (t.getSubStringLength(0, x+2) <= w) {
                                t.textContent = txt.substring(0,x) + \"..\";
                                return;
                        }
                }
                t.textContent = \"\";
        }

        function zoom_reset(e) {
                if (e.attributes != undefined) {
                        orig_load(e, \"x\");
                        orig_load(e, \"width\");
                }
                if (e.childNodes == undefined) return;
                for(var i=0, c=e.childNodes; i<c.length; i++) { zoom_reset(c[i]); }
        }
        function zoom_child(e, x, ratio) {
                if (e.attributes != undefined) {
                        if (e.attributes[\"x\"] != undefined) {
                                orig_save(e, \"x\");
                                e.attributes[\"x\"].value = (parseFloat(e.attributes[\"x\"].value) - x - ~D) * ratio + ~D;
                                if(e.tagName == \"text\") e.attributes[\"x\"].value = find_child(e.parentNode, \"rect\", \"x\") + 3;
                        }
                        if (e.attributes[\"width\"] != undefined) {
                                orig_save(e, \"width\");
                                e.attributes[\"width\"].value = parseFloat(e.attributes[\"width\"].value) * ratio;
                        }
                }
                if (e.childNodes == undefined) return;
                for(var i=0, c=e.childNodes; i<c.length; i++) { zoom_child(c[i], x-~D, ratio); }
        }
        function zoom_parent(e) {
                if (e.attributes) {
                        if (e.attributes[\"x\"] != undefined) {
                                orig_save(e, \"x\");
                                e.attributes[\"x\"].value = ~D;
                        }
                        if (e.attributes[\"width\"] != undefined) {
                                orig_save(e, \"width\");
                                e.attributes[\"width\"].value = parseInt(svg.width.baseVal.value) - (~D*2);
                        }
                }
                if (e.childNodes == undefined) return;
                for(var i=0, c=e.childNodes; i<c.length; i++) { zoom_parent(c[i]); }
        }
        function zoom(node) {
                var attr = find_child(node, \"rect\").attributes;
                var width = parseFloat(attr[\"width\"].value);
                var xmin = parseFloat(attr[\"x\"].value);
                var xmax = parseFloat(xmin + width);
                var ymin = parseFloat(attr[\"y\"].value);
                var ratio = (svg.width.baseVal.value - 2*~D) / width;
                var fudge = 0.0001;
                var unzoombtn = document.getElementById(\"unzoom\");
                unzoombtn.style[\"opacity\"] = \"1.0\";
                var el = document.getElementsByTagName(\"g\");
                for(var i=0;i<el.length;i++){
                        var e = el[i];
                        var a = find_child(e, \"rect\").attributes;
                        var ex = parseFloat(a[\"x\"].value);
                        var ew = parseFloat(a[\"width\"].value);
                        if (~D == 0) {
                                var upstack = parseFloat(a[\"y\"].value) > ymin;
                        } else {
                                var upstack = parseFloat(a[\"y\"].value) < ymin;
                        }
                        if (upstack) {
                                if (ex <= xmin && (ex+ew+fudge) >= xmax) {
                                        e.style[\"opacity\"] = \"0.5\";
                                        zoom_parent(e);
                                        e.onclick = function(e){unzoom(); zoom(this);};
                                        update_text(e);
                                } else { e.style[\"display\"] = \"none\"; }
                        } else {
                                if (ex < xmin || ex + fudge >= xmax) { e.style[\"display\"] = \"none\"; }
                                else { zoom_child(e, xmin, ratio); e.onclick = function(e){zoom(this);}; update_text(e); }
                        }
                }
        }
        function unzoom() {
                var unzoombtn = document.getElementById(\"unzoom\");
                unzoombtn.style[\"opacity\"] = \"0.0\";
                var el = document.getElementsByTagName(\"g\");
                for(i=0;i<el.length;i++) {
                        el[i].style[\"display\"] = \"block\";
                        el[i].style[\"opacity\"] = \"1\";
                        zoom_reset(el[i]);
                        update_text(el[i]);
                }
        }

        function reset_search() {
                var el = document.getElementsByTagName(\"rect\");
                for (var i=0; i < el.length; i++) { orig_load(el[i], \"fill\") }
        }
        function search_prompt() {
                if (!searching) {
                        var term = prompt(\"Enter a search term (regexp allowed, eg: ^ext4_)\", \"\");
                        if (term != null) { search(term) }
                } else {
                        reset_search();
                        searching = 0;
                        searchbtn.style[\"opacity\"] = \"0.1\";
                        searchbtn.firstChild.nodeValue = \"Search\";
                        matchedtxt.style[\"opacity\"] = \"0.0\";
                        matchedtxt.firstChild.nodeValue = \"\";
                }
        }
        function search(term) {
                var re = new RegExp(term);
                var el = document.getElementsByTagName(\"g\");
                var matches = new Object();
                var maxwidth = 0;
                for (var i = 0; i < el.length; i++) {
                        var e = el[i];
                        if (e.attributes[\"class\"].value != \"func_g\") continue;
                        var func = g_to_func(e);
                        var rect = find_child(e, \"rect\");
                        if (rect == null) { if (rect = find_child(e, \"a\")) { rect = find_child(r, \"rect\"); } }
                        if (func == null || rect == null) continue;
                        var w = parseFloat(rect.attributes[\"width\"].value);
                        if (w > maxwidth) maxwidth = w;
                        if (func.match(re)) {
                                var x = parseFloat(rect.attributes[\"x\"].value);
                                orig_save(rect, \"fill\");
                                rect.attributes[\"fill\"].value = \"~A\";
                                if (matches[x] == undefined) { matches[x] = w; }
                                else { if (w > matches[x]) { matches[x] = w; } }
                                searching = 1;
                        }
                }
                if (!searching) return;
                searchbtn.style[\"opacity\"] = \"1.0\";
                searchbtn.firstChild.nodeValue = \"Reset Search\";
                var count = 0; var lastx = -1; var lastw = 0; var keys = Array();
                for (k in matches) { if (matches.hasOwnProperty(k)) keys.push(k); }
                keys.sort(function(a, b){ return a - b; });
                var fudge = 0.0001;
                for (var k in keys) {
                        var x = parseFloat(keys[k]); var w = matches[keys[k]];
                        if (x >= lastx + lastw - fudge) { count += w; lastx = x; lastw = w; }
                }
                matchedtxt.style[\"opacity\"] = \"1.0\";
                pct = 100 * count / maxwidth;
                if (pct == 100) pct = \"100\"; else pct = pct.toFixed(1);
                matchedtxt.firstChild.nodeValue = \"Matched: \" + pct + \"%\";
        }
        function searchover(e) { searchbtn.style[\"opacity\"] = \"1.0\"; }
        function searchout(e) {
                if (searching) { searchbtn.style[\"opacity\"] = \"1.0\"; }
                else { searchbtn.style[\"opacity\"] = \"0.1\"; }
        }
]]>
</script>
"
            nametype fontsize fontwidth xpad xpad xpad xpad xpad xpad inverted searchcolor)))

;;; ---------------------------------------------------------------------------
;;; SVG emission primitives
;;; ---------------------------------------------------------------------------

(defun emit-svg-header (out width height opts)
  (format out "<?xml version=\"1.0\"~@[ encoding=\"~A\"~] standalone=\"no\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
<svg version=\"1.1\" width=\"~D\" height=\"~D\" onload=\"init(evt)\" viewBox=\"0 0 ~D ~D\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">
<!-- Flame graph stack visualization -->
<!-- NOTES: ~A -->
"
          (opts-encoding opts) width height width height (opts-notes opts)))

(defun emit-rect (out x1 y1 x2 y2 fill &optional (extra ""))
  (format out "<rect x=\"~,1F\" y=\"~,1F\" width=\"~,1F\" height=\"~,1F\" fill=\"~A\" ~A/>~%"
          (float x1) (float y1) (float (- x2 x1)) (float (- y2 y1)) fill extra))

(defun emit-text (out color font size x y str &key (anchor "left") (extra ""))
  (format out "<text text-anchor=\"~A\" x=\"~,2F\" y=\"~D\" font-size=\"~D\" font-family=\"~A\" fill=\"~A\" ~A>~A</text>~%"
          anchor (float x) y size font color extra str))

;;; ---------------------------------------------------------------------------
;;; Top-level drawing
;;; ---------------------------------------------------------------------------

(defun default-frame-filter (name)
  (cond
    ((search "bytecode_vm" name) nil)
    ((search "bytecode_call" name) nil)
    ((search "entry_point_" name) nil)
    ((search "default_bytecode_" name) nil)
    ((search "apply_inner_" name) nil)
    (t t)))

(defun flamegraph (&key (data (ext:profile-symbolicated-samples))
                     (output (make-string-output-stream))
                     (title "") (subtitle "")
                     (image-width 1200) (frame-height 16)
                     (font-type "Verdana") (font-size 12) (font-width 0.59)
                     (min-width 0.1)
                     (name-type "Function:") (count-name "samples")
                     (colors "clasp")
                     (bg-color1 "#eeeeee" bg-color1-p)
                     (bg-color2 "#eeeeb0" bg-color2-p)
                     total (factor 1.0)
                     hash reverse-stack inverted negate
                     (notes "") encoding
                     (search-color "rgb(230,0,230)")
                     (frame-filter #'default-frame-filter))
  "Render a flamegraph from DATA to OUTPUT (a stream).
DATA is a list of (FRAMES . COUNT) pairs where FRAMES is a list of
frame-name strings, outermost-first (index 0 = root, last = leaf).
Keyword arguments correspond to fields of the `opts' struct.

Certain :colors palettes (mem, chain, io, wakeup, red, green, blue, aqua,
yellow, purple, orange) imply specific :bg-color1 / :bg-color2 defaults.
If you pass an explicit :bg-color1 or :bg-color2 alongside one of those
palettes, your value is honored and a warning is printed to
*error-output* noting which palette default was skipped."
  (let ((o (make-opts :title title :subtitle subtitle
                      :image-width image-width :frame-height frame-height
                      :font-type font-type :font-size font-size
                      :font-width font-width :min-width min-width
                      :name-type name-type :count-name count-name
                      :colors colors
                      :bg-color1 bg-color1 :bg-color2 bg-color2
                      :total total :factor factor
                      :hash hash :reverse-stack reverse-stack
                      :inverted inverted :negate negate
                      :notes notes :encoding encoding
                      :search-color search-color)))
    ;; Accept either the pre-built (frames . count) list or the raw
    ;; vector returned by ext:profile-symbolicated-samples.
    (when (vectorp data)
      (setf data (transform-symbolicated-samples data)))
    ;; Drop frames the caller doesn't want. Samples that become empty
    ;; (all frames filtered out) are removed entirely.
    (when frame-filter
      (setf data
            (loop for (frames . count) in data
                  for kept = (remove-if-not frame-filter frames)
                  when kept
                    collect (cons kept count))))
    ;; Palette-driven bg defaults — applied only when the caller didn't
    ;; supply the corresponding keyword.
    (multiple-value-bind (palette-bg1 palette-bg2)
        (cond ((or (string= colors "mem") (string= colors "chain"))
               (values "#eeeeee" "#e0e0ff"))
              ((member colors
                       '("io" "wakeup" "red" "green" "blue"
                         "aqua" "yellow" "purple" "orange")
                       :test #'string=)
               (values "#f8f8f8" "#e8e8e8"))
              (t (values nil nil)))
      (when (and palette-bg1 (or bg-color1-p bg-color2-p))
        (format *error-output*
                "flamegraph: :colors ~S has palette-default backgrounds ~
                 ~A / ~A but you passed explicit~:[~; :bg-color1~]~
                 ~:[~; :bg-color2~]; keeping your values.~%"
                colors palette-bg1 palette-bg2 bg-color1-p bg-color2-p))
      (unless bg-color1-p
        (when palette-bg1 (setf (opts-bg-color1 o) palette-bg1)))
      (unless bg-color2-p
        (when palette-bg2 (setf (opts-bg-color2 o) palette-bg2))))
    (when (zerop (length (opts-title o)))
      (setf (opts-title o)
            (if (opts-inverted o) "Icicle Graph" "Flame Graph")))
    (do-flamegraph o data output)
    output))

(defun do-flamegraph (opts data output)
    (when (opts-reverse-stack opts)
      (setf data (mapcar (lambda (p)
                           (cons (reverse-stack-frames (car p)) (cdr p)))
                         data)))
    (setf data (sort data #'list-string< :key #'car))

    (let* ((nodes (make-hash-table :test 'equal))
           (tmp (make-hash-table :test 'equal))
           (last (vector ""))
           (time 0))
      (dolist (pair data)
        (destructuring-bind (stack . samples) pair
          (let ((frames (coerce (cons "" stack) 'vector)))
            (setf last (flow last frames time nodes tmp nil))
            (incf time samples))))
      (flow last (vector) time nodes tmp nil)

      (when (zerop time)
        (format *error-output* "ERROR: No stack counts found~%")
        (let* ((iw (opts-image-width opts))
               (ih (* (opts-font-size opts) 5)))
          (emit-svg-header output iw ih opts)
          (emit-text output "rgb(0,0,0)" (opts-font-type opts)
                     (+ (opts-font-size opts) 2)
                     (floor iw 2) (* (opts-font-size opts) 2)
                     "ERROR: No valid input provided to flamegraph.lisp."
                     :anchor "middle")
          (format output "</svg>~%"))
        (return-from do-flamegraph nil))

      (let* ((timemax (let ((tm (opts-total opts)))
                        (cond ((and tm (< tm time))
                               (format *error-output* "Specified --total ~D < actual ~D, ignoring~%" tm time)
                               time)
                              (tm tm)
                              (t time))))
             (imagewidth (opts-image-width opts))
             (frameheight (opts-frame-height opts))
             (fontsize (opts-font-size opts))
             (ypad1 (* fontsize 3))
             (ypad2 (+ (* fontsize 2) 10))
             (ypad3 (* fontsize 2))
             (xpad 10)
             (framepad 1)
             (widthpertime (/ (- imagewidth (* 2 xpad)) (float timemax)))
             (minwidth-time (/ (opts-min-width opts) widthpertime))
             (depthmax 0)
             (has-subtitle (plusp (length (opts-subtitle opts)))))

        ;; Prune narrow nodes and record depthmax.
        (let ((to-delete '()))
          (maphash (lambda (id n)
                     (if (< (- (node-etime n) (node-stime n)) minwidth-time)
                         (push id to-delete)
                         (when (> (node-depth n) depthmax)
                           (setf depthmax (node-depth n)))))
                   nodes)
          (dolist (id to-delete) (remhash id nodes)))

        (let ((imageheight (+ (* (1+ depthmax) frameheight) ypad1 ypad2
                              (if has-subtitle ypad3 0))))
          (emit-svg-header output imagewidth imageheight opts)

          ;; <defs> for background gradient + the interactive JS.
          (format output "<defs>
  <linearGradient id=\"background\" y1=\"0\" y2=\"1\" x1=\"0\" x2=\"0\">
    <stop stop-color=\"~A\" offset=\"5%\"/>
    <stop stop-color=\"~A\" offset=\"95%\"/>
  </linearGradient>
</defs>
"
                  (opts-bg-color1 opts) (opts-bg-color2 opts))
          (write-string (embedded-js opts) output)

          ;; Background.
          (emit-rect output 0 0 imagewidth imageheight "url(#background)")

          ;; Title + subtitle.
          (emit-text output "rgb(0,0,0)" (opts-font-type opts) (+ fontsize 5)
                     (floor imagewidth 2) (* fontsize 2)
                     (xml-escape (opts-title opts))
                     :anchor "middle")
          (when has-subtitle
            (emit-text output "rgb(160,160,160)" (opts-font-type opts) fontsize
                       (floor imagewidth 2) (* fontsize 4)
                       (xml-escape (opts-subtitle opts))
                       :anchor "middle"))

          ;; Details / Reset Zoom / Search / Matched labels.
          (emit-text output "rgb(0,0,0)" (opts-font-type opts) fontsize
                     xpad (- imageheight (floor ypad2 2))
                     " " :extra "id=\"details\"")
          (emit-text output "rgb(0,0,0)" (opts-font-type opts) fontsize
                     xpad (* fontsize 2)
                     "Reset Zoom"
                     :extra "id=\"unzoom\" onclick=\"unzoom()\" style=\"opacity:0.0;cursor:pointer\"")
          (emit-text output "rgb(0,0,0)" (opts-font-type opts) fontsize
                     (- imagewidth xpad 100) (* fontsize 2)
                     "Search"
                     :extra "id=\"search\" onmouseover=\"searchover()\" onmouseout=\"searchout()\" onclick=\"search_prompt()\" style=\"opacity:0.1;cursor:pointer\"")
          (emit-text output "rgb(0,0,0)" (opts-font-type opts) fontsize
                     (- imagewidth xpad 100) (- imageheight (floor ypad2 2))
                     " " :extra "id=\"matched\"")

          ;; Each merged node → a <g> with a <title> tooltip, <rect> and
          ;; <text>.
          (maphash
           (lambda (id n)
             (declare (ignore id))
             (let* ((func (node-func n))
                    (depth (node-depth n))
                    (stime (node-stime n))
                    (etime (if (and (string= func "") (zerop depth))
                               timemax
                               (node-etime n)))
                    (x1 (+ xpad (* stime widthpertime)))
                    (x2 (+ xpad (* etime widthpertime)))
                    y1 y2)
               (if (opts-inverted opts)
                   (setf y1 (+ ypad1 (* depth frameheight))
                         y2 (+ ypad1 (* (1+ depth) frameheight) (- framepad)))
                   (setf y1 (- imageheight ypad2 (* (1+ depth) frameheight) (- framepad))
                         y2 (- imageheight ypad2 (* depth frameheight))))
               (let* ((samples (round (* (- etime stime) (opts-factor opts))))
                      (samples-txt (format-with-commas samples))
                      (info
                        (if (and (string= func "") (zerop depth))
                            (format nil "all (~A ~A, 100%)"
                                    samples-txt (opts-count-name opts))
                            (let* ((pct (* 100.0 (/ samples
                                                    (* timemax (opts-factor opts)))))
                                   (esc (xml-escape (strip-frame-annotation func))))
                              (format nil "~A (~A ~A, ~,2F%)"
                                      esc samples-txt (opts-count-name opts) pct))))
                      (fill (cond
                              ((string= func "--") "rgb(160,160,160)")
                              ((string= func "-")  "rgb(200,200,200)")
                              ((node-delta n)
                               (color-scale (node-delta n) 1
                                            (opts-negate opts)))
                              (t
                               (color (opts-colors opts)
                                      (opts-hash opts) func)))))
                 ;; <g>
                 (format output "<g class=\"func_g\" onmouseover=\"s(this)\" onmouseout=\"c()\" onclick=\"zoom(this)\">~%")
                 (format output "<title>~A</title>" info)
                 (emit-rect output x1 y1 x2 y2 fill "rx=\"2\" ry=\"2\"")
                 (let* ((chars (floor (/ (- x2 x1)
                                         (* fontsize (opts-font-width opts)))))
                        (text (if (< chars 3)
                                  ""
                                  (let ((stripped (strip-frame-annotation func)))
                                    (if (< chars (length stripped))
                                        (concatenate 'string
                                                     (subseq stripped 0 (max 0 (- chars 2)))
                                                     "..")
                                        stripped)))))
                   (emit-text output "rgb(0,0,0)" (opts-font-type opts) fontsize
                              (+ x1 3) (+ 3 (floor (+ y1 y2) 2))
                              (xml-escape text)))
                 (format output "</g>~%"))))
           nodes)

          (format output "</svg>~%")))))

(defun main (&optional (args (cmdline-args)))
(let ((opts (parse-args args)))
  (unless opts (return-from main nil))
  (let* ((in-path (opts-input-file opts))
         (input (if in-path
                    (open in-path :direction :input :if-does-not-exist :error)
                    *standard-input*)))
    (let ((data (read-input input)))
      (unwind-protect (do-flamegraph opts data *standard-output*)
        (when in-path (close input)))
      t))))

;;; Auto-invoke MAIN when loaded as an SBCL script
;;; (`sbcl --script flamegraph.lisp ARGS`). SBCL consumes the script path
;;; from argv, leaving *posix-argv* = ("sbcl" "arg1" "arg2" ...), which
;;; CMDLINE-ARGS reads with (cdr). We detect script mode via the
;;; *script-args*-isn't-bound-in-REPL heuristic: when LOAD is invoked
;;; from the REPL, `*load-truename*` is set *and* the user will call
;;; MAIN explicitly. When we're a --script invocation, SBCL runs the
;;; file's top-level forms and exits.
;;;
;;; Simplest portable approach: always call main when this file is
;;; evaluated at top level. From the REPL the user can just LOAD +
;;; invoke (flamegraph:main '(...args...)) themselves.
;;; ---------------------------------------------------------------------------
;;; SIGUSR2 snapshot
;;;
;;; When CLASP_FLAME_PROFILE is set at load time, sending SIGUSR2 to the
;;; process starts a sampling profile on a background thread and writes
;;; the flame graph SVG to the configured path. Useful for capturing a
;;; profile of a long-running process without a REPL.
;;;
;;; Syntax of CLASP_FLAME_PROFILE:
;;;   unset / "" / 0 / off / no / false    -> disabled
;;;   1 / on / yes / true                  -> enabled with defaults
;;;                                           (path=/tmp/clasp-PID.svg,
;;;                                            duration=10s, rate=97Hz)
;;;   key=value:key=value:...              -> enabled with overrides
;;;
;;; Known keys: path, duration (seconds), rate (Hz). Unknown keys emit
;;; a warning but do not disable the profiler.
;;;
;;; Examples:
;;;   CLASP_FLAME_PROFILE=1 cando ...
;;;   CLASP_FLAME_PROFILE=path=/tmp/foo.svg:duration=5:rate=499 cando ...
;;;   kill -USR2 <pid>
;;; ---------------------------------------------------------------------------

#+clasp
(progn
  (defparameter *snapshot-duration* 10
    "Seconds of sampling per SIGUSR2 snapshot.")
  (defparameter *snapshot-rate* 97
    "Sampling rate (Hz) for SIGUSR2 snapshots.")
  (defparameter *snapshot-path* nil
    "SIGUSR2 writes a flame graph SVG to this pathname.")

  (defun parse-flame-profile-env (value)
    "Parse the value of CLASP_FLAME_PROFILE. Returns three values:
ENABLED-P, a plist of (:path :duration :rate) overrides, and a list
of error-message strings describing malformed fields.

Syntax:
  unset / \"\" / 0 / off / no / false   -> disabled
  1 / on / yes / true                   -> enabled, all defaults
  key=value:key=value:...               -> enabled with overrides
Known keys: path, duration, rate."
    (when (or (null value) (zerop (length value)))
      (return-from parse-flame-profile-env (values nil nil nil)))
    (let ((trimmed (string-trim '(#\Space #\Tab) value)))
      (cond
        ((zerop (length trimmed)) (values nil nil nil))
        ((member trimmed '("0" "off" "no" "false") :test #'string-equal)
         (values nil nil nil))
        ((member trimmed '("1" "on" "yes" "true") :test #'string-equal)
         (values t nil nil))
        (t
         (let ((plist '())
               (errors '()))
           (dolist (pair (split-string trimmed #\:))
             (let* ((p (string-trim '(#\Space #\Tab) pair))
                    (eq (position #\= p)))
               (cond
                 ((zerop (length p)))
                 ((null eq)
                  (push (format nil "expected key=value, got ~S" p) errors))
                 (t
                  (let ((key (string-trim '(#\Space #\Tab) (subseq p 0 eq)))
                        (val (string-trim '(#\Space #\Tab) (subseq p (1+ eq)))))
                    (cond
                      ((string-equal key "path")
                       (if (zerop (length val))
                           (push "path= must not be empty" errors)
                           (setf (getf plist :path) val)))
                      ((string-equal key "duration")
                       (multiple-value-bind (n pos)
                           (parse-integer val :junk-allowed t)
                         (if (and n (= pos (length val)) (plusp n))
                             (setf (getf plist :duration) n)
                             (push (format nil "duration must be a positive integer, got ~S" val)
                                   errors))))
                      ((string-equal key "rate")
                       (multiple-value-bind (n pos)
                           (parse-integer val :junk-allowed t)
                         (if (and n (= pos (length val)) (plusp n))
                             (setf (getf plist :rate) n)
                             (push (format nil "rate must be a positive integer, got ~S" val)
                                   errors))))
                      (t
                       (push (format nil "unknown key ~S (known: path, duration, rate)" key)
                             errors))))))))
           (values t plist (nreverse errors)))))))

  (defun snapshot (path &key (duration *snapshot-duration*))
    "Run DURATION seconds of sampling profiling on a background thread
and write the flame graph SVG to PATH. Returns T if a snapshot was
spawned, NIL if the profiler was already running."
    (when (ext:profile-running-p)
      (format *error-output* "flamegraph: profiler already running~%")
      (return-from snapshot nil))
    (mp:process-run-function
     "flamegraph-snapshot"
     (lambda ()
       (handler-case
           (if (ext:profile-start :rate *snapshot-rate*)
               (unwind-protect
                    (progn
                      (sleep duration)
                      (ext:profile-stop)
                      (with-open-file (out path :direction :output
                                                :if-exists :supersede
                                                :if-does-not-exist :create)
                        (flamegraph :data (ext:profile-symbolicated-samples)
                                    :output out
                                    :title (format nil "clasp ~A (~Ds)"
                                                   (core:getpid) duration)))
                      (core:chmod path #o664)
                      (ext:profile-reset)
                      (format *error-output* "flamegraph: wrote ~A~%" path))
                 ;; Make sure the profiler is stopped even on non-local exit.
                 (when (ext:profile-running-p) (ext:profile-stop)))
               (format *error-output*
                       "flamegraph: profile-start failed (already running?)~%"))
         (error (c)
           (format *error-output* "flamegraph: snapshot failed: ~A~%" c)))))
    t)

  ;; CLASP_FLAME_PROFILE controls SIGUSR2 snapshots. See parse-flame-profile-env
  ;; for the full syntax.
  (multiple-value-bind (enabled overrides errors)
      (parse-flame-profile-env (ext:getenv "CLASP_FLAME_PROFILE"))
    (dolist (e errors)
      (format *error-output* "flamegraph: CLASP_FLAME_PROFILE: ~A~%" e))
    (when enabled
      (setf *snapshot-path*
            (or (getf overrides :path)
                (format nil "/tmp/clasp-~d.svg" (core:getpid))))
      (let ((d (getf overrides :duration))) (when d (setf *snapshot-duration* d)))
      (let ((r (getf overrides :rate)))     (when r (setf *snapshot-rate* r)))
      (let ((dir (directory-namestring *snapshot-path*)))
        (unless (probe-file dir)
          (format *error-output*
                  "flamegraph: warning: output directory ~A does not exist~%" dir)))
      (defmethod mp:service-interrupt ((i core:sigusr2))
        (snapshot *snapshot-path*))
      (format *error-output*
              "flamegraph: SIGUSR2 will write flame graph to ~A (~Ds @ ~DHz)~%"
              *snapshot-path* *snapshot-duration* *snapshot-rate*))))

#+sbcl (flamegraph:main)
