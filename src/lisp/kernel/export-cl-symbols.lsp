;;(make-package "CLSYMBOLS" :use '(:core :clos))
(select-package :cl)

(defvar *all-cl-symbols* '(&allow-other-keys            *print-miser-width*          
			   &aux                         *print-pprint-dispatch*      
			   &body                        *print-pretty*               
			   &environment                 *print-radix*                
			   &key                         *print-readably*             
			   &optional                    *print-right-margin*         
			   &rest                        *query-io*                   
			   &whole                       *random-state*               
			   *                            *read-base*                  
			   **                           *read-default-float-format*  
			   ***                          *read-eval*                  
			   *break-on-signals*           *read-suppress*              
			   *compile-file-pathname*      *readtable*                  
			   *compile-file-truename*      *standard-input*             
			   *compile-print*              *standard-output*            
			   *compile-verbose*            *terminal-io*                
			   *debug-io*                   *trace-output*               
			   *debugger-hook*              +                            
			   *default-pathname-defaults*  ++                           
			   *error-output*               +++                          
			   *features*                   -                            
			   *gensym-counter*             /                            
			   *load-pathname*              //                           
			   *load-print*                 ///                          
			   *load-truename*              /=                           
			   *load-verbose*               1+                           
			   *macroexpand-hook*           1-                           
			   *modules*                    <                            
			   *package*                    <=                           
			   *print-array*                =                            
			   *print-base*                 >                            
			   *print-case*                 >=                           
			   *print-circle*               abort                        
			   *print-escape*               abs                          
			   *print-gensym*               acons                        
			   *print-length*               acos                         
			   *print-level*                acosh                        
			   *print-lines*                add-method                   
			   adjoin                      atom          boundp                    
			   adjust-array                base-char     break                     
			   adjustable-array-p          base-string   broadcast-stream          
			   allocate-instance           bignum        broadcast-stream-streams  
			   alpha-char-p                bit           built-in-class            
			   alphanumericp               bit-and       butlast                   
			   and                         bit-andc1     byte                      
			   append                      bit-andc2     byte-position             
			   apply                       bit-eqv       byte-size                 
			   apropos                     bit-ior       caaaar                    
			   apropos-list                bit-nand      caaadr                    
			   aref                        bit-nor       caaar                     
			   arithmetic-error            bit-not       caadar                    
			   arithmetic-error-operands   bit-orc1      caaddr                    
			   arithmetic-error-operation  bit-orc2      caadr                     
			   array                       bit-vector    caar                      
			   array-dimension             bit-vector-p  cadaar                    
			   array-dimension-limit       bit-xor       cadadr                    
			   array-dimensions            block         cadar                     
			   array-displacement          boole         caddar                    
			   array-element-type          boole-1       cadddr                    
			   array-has-fill-pointer-p    boole-2       caddr                     
			   array-in-bounds-p           boole-and     cadr                      
			   array-rank                  boole-andc1   call-arguments-limit      
			   array-rank-limit            boole-andc2   call-method               
			   array-row-major-index       boole-c1      call-next-method          
			   array-total-size            boole-c2      car                       
			   array-total-size-limit      boole-clr     case                      
			   arrayp                      boole-eqv     catch                     
			   ash                         boole-ior     ccase                     
			   asin                        boole-nand    cdaaar                    
			   asinh                       boole-nor     cdaadr                    
			   assert                      boole-orc1    cdaar                     
			   assoc                       boole-orc2    cdadar                    
			   assoc-if                    boole-set     cdaddr                    
			   assoc-if-not                boole-xor     cdadr                     
			   atan                        boolean       cdar                      
			   atanh                       both-case-p   cddaar                    
			   cddadr             clear-input                  copy-tree                  
			   cddar              clear-output                 cos                        
			   cdddar             close                        cosh                       
			   cddddr             clrhash                      count                      
			   cdddr              code-char                    count-if                   
			   cddr               coerce                       count-if-not               
			   cdr                compilation-speed            ctypecase                  
			   ceiling            compile                      debug                      
			   cell-error         compile-file                 decf                       
			   cell-error-name    compile-file-pathname        declaim                    
			   cerror             compiled-function            declaration                
			   change-class       compiled-function-p          declare                    
			   char               compiler-macro               decode-float               
			   char-code          compiler-macro-function      decode-universal-time      
			   char-code-limit    complement                   defclass                   
			   char-downcase      complex                      defconstant                
			   char-equal         complexp                     defgeneric                 
			   char-greaterp      compute-applicable-methods   define-compiler-macro      
			   char-int           compute-restarts             define-condition           
			   char-lessp         concatenate                  define-method-combination  
			   char-name          concatenated-stream          define-modify-macro        
			   char-not-equal     concatenated-stream-streams  define-setf-expander       
			   char-not-greaterp  cond                         define-symbol-macro        
			   char-not-lessp     condition                    defmacro                   
			   char-upcase        conjugate                    defmethod                  
			   char/=             cons                         defpackage                 
			   char<              consp                        defparameter               
			   char<=             constantly                   defsetf                    
			   char=              constantp                    defstruct                  
			   char>              continue                     deftype                    
			   char>=             control-error                defun                      
			   character          copy-alist                   defvar                     
			   characterp         copy-list                    delete                     
			   check-type         copy-pprint-dispatch         delete-duplicates          
			   cis                copy-readtable               delete-file                
			   class              copy-seq                     delete-if                  
			   class-name         copy-structure               delete-if-not              
			   class-of           copy-symbol                  delete-package             
			   denominator                    eq                   
			   deposit-field                  eql                  
			   describe                       equal                
			   describe-object                equalp               
			   destructuring-bind             error                
			   digit-char                     etypecase            
			   digit-char-p                   eval                 
			   directory                      eval-when            
			   directory-namestring           evenp                
			   disassemble                    every                
			   division-by-zero               exp                  
			   do                             export               
			   do*                            expt                 
			   do-all-symbols                 extended-char        
			   do-external-symbols            fboundp              
			   do-symbols                     fceiling             
			   documentation                  fdefinition          
			   dolist                         ffloor               
			   dotimes                        fifth                
			   double-float                   file-author          
			   double-float-epsilon           file-error           
			   double-float-negative-epsilon  file-error-pathname  
			   dpb                            file-length          
			   dribble                        file-namestring      
			   dynamic-extent                 file-position        
			   ecase                          file-stream          
			   echo-stream                    file-string-length   
			   echo-stream-input-stream       file-write-date      
			   echo-stream-output-stream      fill                 
			   ed                             fill-pointer         
			   eighth                         find                 
			   elt                            find-all-symbols     
			   encode-universal-time          find-class           
			   end-of-file                    find-if              
			   endp                           find-if-not          
			   enough-namestring              find-method          
			   ensure-directories-exist       find-package         
			   ensure-generic-function        find-restart         
			   find-symbol                       get-internal-run-time        
			   finish-output                     get-macro-character          
			   first                             get-output-stream-string     
			   fixnum                            get-properties               
			   flet                              get-setf-expansion           
			   float                             get-universal-time           
			   float-digits                      getf                         
			   float-precision                   gethash                      
			   float-radix                       go                           
			   float-sign                        graphic-char-p               
			   floating-point-inexact            handler-bind                 
			   floating-point-invalid-operation  handler-case                 
			   floating-point-overflow           hash-table                   
			   floating-point-underflow          hash-table-count             
			   floatp                            hash-table-p                 
			   floor                             hash-table-rehash-size       
			   fmakunbound                       hash-table-rehash-threshold  
			   force-output                      hash-table-size              
			   format                            hash-table-test              
			   formatter                         host-namestring              
			   fourth                            identity                     
			   fresh-line                        if                           
			   fround                            ignorable                    
			   ftruncate                         ignore                       
			   ftype                             ignore-errors                
			   funcall                           imagpart                     
			   function                          import                       
			   function-keywords                 in-package                   
			   function-lambda-expression        incf                         
			   functionp                         initialize-instance          
			   gcd                               inline                       
			   generic-function                  input-stream-p               
			   gensym                            inspect                      
			   gentemp                           integer                      
			   get                               integer-decode-float         
			   get-decoded-time                  integer-length               
			   get-dispatch-macro-character      integerp                     
			   get-internal-real-time            interactive-stream-p         
			   intern                                  lisp-implementation-type            
			   internal-time-units-per-second          lisp-implementation-version         
			   intersection                            list                                
			   invalid-method-error                    list*                               
			   invoke-debugger                         list-all-packages                   
			   invoke-restart                          list-length                         
			   invoke-restart-interactively            listen                              
			   isqrt                                   listp                               
			   keyword                                 load                                
			   keywordp                                load-logical-pathname-translations  
			   labels                                  load-time-value                     
			   lambda                                  locally                             
			   lambda-list-keywords                    log                                 
			   lambda-parameters-limit                 logand                              
			   last                                    logandc1                            
			   lcm                                     logandc2                            
			   ldb                                     logbitp                             
			   ldb-test                                logcount                            
			   ldiff                                   logeqv                              
			   least-negative-double-float             logical-pathname                    
			   least-negative-long-float               logical-pathname-translations       
			   least-negative-normalized-double-float  logior                              
			   least-negative-normalized-long-float    lognand                             
			   least-negative-normalized-short-float   lognor                              
			   least-negative-normalized-single-float  lognot                              
			   least-negative-short-float              logorc1                             
			   least-negative-single-float             logorc2                             
			   least-positive-double-float             logtest                             
			   least-positive-long-float               logxor                              
			   least-positive-normalized-double-float  long-float                          
			   least-positive-normalized-long-float    long-float-epsilon                  
			   least-positive-normalized-short-float   long-float-negative-epsilon         
			   least-positive-normalized-single-float  long-site-name                      
			   least-positive-short-float              loop                                
			   least-positive-single-float             loop-finish                         
			   length                                  lower-case-p                        
			   let                                     machine-instance                    
			   let*                                    machine-type                        
			   machine-version                mask-field                  
			   macro-function                 max                         
			   macroexpand                    member                      
			   macroexpand-1                  member-if                   
			   macrolet                       member-if-not               
			   make-array                     merge                       
			   make-broadcast-stream          merge-pathnames             
			   make-concatenated-stream       method                      
			   make-condition                 method-combination          
			   make-dispatch-macro-character  method-combination-error    
			   make-echo-stream               method-qualifiers           
			   make-hash-table                min                         
			   make-instance                  minusp                      
			   make-instances-obsolete        mismatch                    
			   make-list                      mod                         
			   make-load-form                 most-negative-double-float  
			   make-load-form-saving-slots    most-negative-fixnum        
			   make-method                    most-negative-long-float    
			   make-package                   most-negative-short-float   
			   make-pathname                  most-negative-single-float  
			   make-random-state              most-positive-double-float  
			   make-sequence                  most-positive-fixnum        
			   make-string                    most-positive-long-float    
			   make-string-input-stream       most-positive-short-float   
			   make-string-output-stream      most-positive-single-float  
			   make-symbol                    muffle-warning              
			   make-synonym-stream            multiple-value-bind         
			   make-two-way-stream            multiple-value-call         
			   makunbound                     multiple-value-list         
			   map                            multiple-value-prog1        
			   map-into                       multiple-value-setq         
			   mapc                           multiple-values-limit       
			   mapcan                         name-char                   
			   mapcar                         namestring                  
			   mapcon                         nbutlast                    
			   maphash                        nconc                       
			   mapl                           next-method-p               
			   maplist                        nil                         
			   nintersection         package-error                  
			   ninth                 package-error-package          
			   no-applicable-method  package-name                   
			   no-next-method        package-nicknames              
			   not                   package-shadowing-symbols      
			   notany                package-use-list               
			   notevery              package-used-by-list           
			   notinline             packagep                       
			   nreconc               pairlis                        
			   nreverse              parse-error                    
			   nset-difference       parse-integer                  
			   nset-exclusive-or     parse-namestring               
			   nstring-capitalize    pathname                       
			   nstring-downcase      pathname-device                
			   nstring-upcase        pathname-directory             
			   nsublis               pathname-host                  
			   nsubst                pathname-match-p               
			   nsubst-if             pathname-name                  
			   nsubst-if-not         pathname-type                  
			   nsubstitute           pathname-version               
			   nsubstitute-if        pathnamep                      
			   nsubstitute-if-not    peek-char                      
			   nth                   phase                          
			   nth-value             pi                             
			   nthcdr                plusp                          
			   null                  pop                            
			   number                position                       
			   numberp               position-if                    
			   numerator             position-if-not                
			   nunion                pprint                         
			   oddp                  pprint-dispatch                
			   open                  pprint-exit-if-list-exhausted  
			   open-stream-p         pprint-fill                    
			   optimize              pprint-indent                  
			   or                    pprint-linear                  
			   otherwise             pprint-logical-block           
			   output-stream-p       pprint-newline                 
			   package               pprint-pop                     
			   pprint-tab                 read-char                   
			   pprint-tabular             read-char-no-hang           
			   prin1                      read-delimited-list         
			   prin1-to-string            read-from-string            
			   princ                      read-line                   
			   princ-to-string            read-preserving-whitespace  
			   print                      read-sequence               
			   print-not-readable         reader-error                
			   print-not-readable-object  readtable                   
			   print-object               readtable-case              
			   print-unreadable-object    readtablep                  
			   probe-file                 real                        
			   proclaim                   realp                       
			   prog                       realpart                    
			   prog*                      reduce                      
			   prog1                      reinitialize-instance       
			   prog2                      rem                         
			   progn                      remf                        
			   program-error              remhash                     
			   progv                      remove                      
			   provide                    remove-duplicates           
			   psetf                      remove-if                   
			   psetq                      remove-if-not               
			   push                       remove-method               
			   pushnew                    remprop                     
			   quote                      rename-file                 
			   random                     rename-package              
			   random-state               replace                     
			   random-state-p             require                     
			   rassoc                     rest                        
			   rassoc-if                  restart                     
			   rassoc-if-not              restart-bind                
			   ratio                      restart-case                
			   rational                   restart-name                
			   rationalize                return                      
			   rationalp                  return-from                 
			   read                       revappend                   
			   read-byte                  reverse                     
			   room                          simple-bit-vector                  
			   rotatef                       simple-bit-vector-p                
			   round                         simple-condition                   
			   row-major-aref                simple-condition-format-arguments  
			   rplaca                        simple-condition-format-control    
			   rplacd                        simple-error                       
			   safety                        simple-string                      
			   satisfies                     simple-string-p                    
			   sbit                          simple-type-error                  
			   scale-float                   simple-vector                      
			   schar                         simple-vector-p                    
			   search                        simple-warning                     
			   second                        sin                                
			   sequence                      single-float                       
			   serious-condition             single-float-epsilon               
			   set                           single-float-negative-epsilon      
			   set-difference                sinh                               
			   set-dispatch-macro-character  sixth                              
			   set-exclusive-or              sleep                              
			   set-macro-character           slot-boundp                        
			   set-pprint-dispatch           slot-exists-p                      
			   set-syntax-from-char          slot-makunbound                    
			   setf                          slot-missing                       
			   setq                          slot-unbound                       
			   seventh                       slot-value                         
			   shadow                        software-type                      
			   shadowing-import              software-version                   
			   shared-initialize             some                               
			   shiftf                        sort                               
			   short-float                   space                              
			   short-float-epsilon           special                            
			   short-float-negative-epsilon  special-operator-p                 
			   short-site-name               speed                              
			   signal                        sqrt                               
			   signed-byte                   stable-sort                        
			   signum                        standard                           
			   simple-array                  standard-char                      
			   simple-base-string            standard-char-p                    
			   standard-class             sublis                      
			   standard-generic-function  subseq                      
			   standard-method            subsetp                     
			   standard-object            subst                       
			   step                       subst-if                    
			   storage-condition          subst-if-not                
			   store-value                substitute                  
			   stream                     substitute-if               
			   stream-element-type        substitute-if-not           
			   stream-error               subtypep                    
			   stream-error-stream        svref                       
			   stream-external-format     sxhash                      
			   streamp                    symbol                      
			   string                     symbol-function             
			   string-capitalize          symbol-macrolet             
			   string-downcase            symbol-name                 
			   string-equal               symbol-package              
			   string-greaterp            symbol-plist                
			   string-left-trim           symbol-value                
			   string-lessp               symbolp                     
			   string-not-equal           synonym-stream              
			   string-not-greaterp        synonym-stream-symbol       
			   string-not-lessp           t                           
			   string-right-trim          tagbody                     
			   string-stream              tailp                       
			   string-trim                tan                         
			   string-upcase              tanh                        
			   string/=                   tenth                       
			   string<                    terpri                      
			   string<=                   the                         
			   string=                    third                       
			   string>                    throw                       
			   string>=                   time                        
			   stringp                    trace                       
			   structure                  translate-logical-pathname  
			   structure-class            translate-pathname          
			   structure-object           tree-equal                  
			   style-warning              truename                    
			   truncate                             values-list               
			   two-way-stream                       variable                  
			   two-way-stream-input-stream          vector                    
			   two-way-stream-output-stream         vector-pop                
			   type                                 vector-push               
			   type-error                           vector-push-extend        
			   type-error-datum                     vectorp                   
			   type-error-expected-type             warn                      
			   type-of                              warning                   
			   typecase                             when                      
			   typep                                wild-pathname-p           
			   unbound-slot                         with-accessors            
			   unbound-slot-instance                with-compilation-unit     
			   unbound-variable                     with-condition-restarts   
			   undefined-function                   with-hash-table-iterator  
			   unexport                             with-input-from-string    
			   unintern                             with-open-file            
			   union                                with-open-stream          
			   unless                               with-output-to-string     
			   unread-char                          with-package-iterator     
			   unsigned-byte                        with-simple-restart       
			   untrace                              with-slots                
			   unuse-package                        with-standard-io-syntax   
			   unwind-protect                       write                     
			   update-instance-for-different-class  write-byte                
			   update-instance-for-redefined-class  write-char                
			   upgraded-array-element-type          write-line                
			   upgraded-complex-part-type           write-sequence            
			   upper-case-p                         write-string              
			   use-package                          write-to-string           
			   use-value                            y-or-n-p                  
			   user-homedir-pathname                yes-or-no-p               
			   values                               zerop                     
			   ))

#||
(defvar *cl-symbol-ht* (make-hash-table :test #'equal))
(dolist (s *all-cl-symbols*)
  (setf (gethash (symbol-name s) *cl-symbol-ht*) s))


(defun identify-symbols-for-export (pkg-name)
  (let ((pkg (find-package pkg-name))
	symbols)
    (do-symbols (s pkg)
      (when (eq (symbol-package s) pkg)
	(when (gethash (symbol-name s) *cl-symbol-ht*)
	  (push s symbols))))
    symbols))


(defun create-export-file (file-name pkg-name)
  (let ((symbols (identify-symbols-for-export pkg-name)))
    (with-open-file (fout file-name :direction :output :if-exists :supersede)
      (format fout "(in-package ~A)~%" pkg-name)
      (format fout "(export '(~%")
;;      (format nil "(export ~{~<~%~1,40:;~A~>  ~}" symbols)
      (dolist (sym symbols)
	(format fout "   ~A~%" sym))
      (format fout ")~%")
      )))




(export '(identify-symbols-for-export create-export-file))


(create-export-file (bformat nil "%s/exportcl.lsp" ( :core)

||#
