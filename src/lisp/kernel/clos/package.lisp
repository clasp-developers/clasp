;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "CLOS")

(eval-when (:compile-toplevel :load-toplevel :execute)
(export '(WITH-SLOTS WITH-ACCESSORS UPDATE-INSTANCE-FOR-REDEFINED-CLASS
	  UPDATE-INSTANCE-FOR-DIFFERENT-CLASS STANDARD-METHOD
	  STANDARD SLOT-UNBOUND SLOT-MISSING SLOT-MAKUNBOUND
	  SLOT-EXISTS-P SLOT-BOUNDP SHARED-INITIALIZE REMOVE-METHOD
	  REINITIALIZE-INSTANCE NO-NEXT-METHOD METHOD-QUALIFIERS
	  METHOD-COMBINATION-ERROR METHOD-COMBINATION MAKE-METHOD MAKE-INSTANCE
	  MAKE-LOAD-FORM-SAVING-SLOTS MAKE-LOAD-FORM MAKE-INSTANCES-OBSOLETE
	  INVALID-METHOD-ERROR INITIALIZE-INSTANCE FUNCTION-KEYWORDS
	  FIND-METHOD ENSURE-GENERIC-FUNCTION DESCRIBE-OBJECT
	  CHANGE-CLASS CALL-METHOD ALLOCATE-INSTANCE
	  ADD-METHOD ))

(export '(metaobject
          specializer
          UPDATE-DEPENDENT 
          SLOT-DEFINITION-LOCATION 
          CLASS-PRECEDENCE-LIST 
          CLASS-SLOTS 
          SPECIALIZER 
          MAKE-METHOD-LAMBDA 
          SLOT-DEFINITION-ALLOCATION 
          STANDARD-DIRECT-SLOT-DEFINITION 
          STANDARD-WRITER-METHOD 
          EFFECTIVE-SLOT-DEFINITION-CLASS 
          MAP-DEPENDENTS 
          STANDARD-READER-METHOD 
          SLOT-DEFINITION-TYPE 
          GENERIC-FUNCTION-ARGUMENT-PRECEDENCE-ORDER 
          EQL-SPECIALIZER 
          COMPUTE-EFFECTIVE-SLOT-DEFINITION 
          WRITER-METHOD-CLASS 
          CLASS-DEFAULT-INITARGS 
          METHOD-SPECIALIZERS 
          ENSURE-GENERIC-FUNCTION-USING-CLASS 
          SLOT-DEFINITION-INITARGS 
          METAOBJECT 
          COMPUTE-SLOTS 
          GENERIC-FUNCTION-METHOD-COMBINATION 
          CLASS-PROTOTYPE 
          FUNCALLABLE-STANDARD-CLASS 
          SLOT-DEFINITION 
          SET-FUNCALLABLE-INSTANCE-FUNCTION 
          ACCESSOR-METHOD-SLOT-DEFINITION 
          CLASS-DIRECT-SLOTS 
          FIND-METHOD-COMBINATION 
          GENERIC-FUNCTION-NAME 
          CLASS-DIRECT-SUPERCLASSES 
          EQL-SPECIALIZER-OBJECT 
          ENSURE-CLASS-USING-CLASS 
          EXTRACT-SPECIALIZER-NAMES 
          SPECIALIZER-DIRECT-GENERIC-FUNCTIONS 
          ADD-DIRECT-SUBCLASS 
          METHOD-FUNCTION 
          COMPUTE-DEFAULT-INITARGS 
          FUNCALLABLE-STANDARD-INSTANCE-ACCESS 
          READER-METHOD-CLASS 
          SPECIALIZER-DIRECT-METHODS 
          REMOVE-DEPENDENT 
          DIRECT-SLOT-DEFINITION 
          GENERIC-FUNCTION-METHODS 
          SLOT-MAKUNBOUND-USING-CLASS 
          STANDARD-ACCESSOR-METHOD 
          SLOT-DEFINITION-WRITERS 
          METHOD-LAMBDA-LIST 
          SLOT-VALUE-USING-CLASS 
          GENERIC-FUNCTION-LAMBDA-LIST
          generic-function-min-max-args
          FUNCALLABLE-STANDARD-OBJECT 
          COMPUTE-EFFECTIVE-METHOD-FUNCTION 
          VALIDATE-SUPERCLASS 
          ADD-DIRECT-METHOD 
          ENSURE-CLASS 
          SLOT-DEFINITION-NAME 
          FINALIZE-INHERITANCE 
          UPDATE-INSTANCE 
          CLASS-FINALIZED-P 
          COMPUTE-DISCRIMINATING-FUNCTION 
          SLOT-DEFINITION-READERS 
          ADD-DEPENDENT 
          EXTRACT-LAMBDA-LIST 
          COMPUTE-CLASS-PRECEDENCE-LIST 
          DIRECT-SLOT-DEFINITION-CLASS 
          EFFECTIVE-SLOT-DEFINITION 
          REMOVE-DIRECT-SUBCLASS 
          STANDARD-EFFECTIVE-SLOT-DEFINITION 
          CLASS-DIRECT-SUBCLASSES 
          GENERIC-FUNCTION-DECLARATIONS 
          STANDARD-INSTANCE-ACCESS 
          COMPUTE-APPLICABLE-METHODS-USING-CLASSES 
          GENERIC-FUNCTION-METHOD-CLASS 
          FORWARD-REFERENCED-CLASS 
          SLOT-BOUNDP-USING-CLASS 
          INTERN-EQL-SPECIALIZER 
          METHOD-GENERIC-FUNCTION 
          COMPUTE-EFFECTIVE-METHOD 
          DOCSTRING 
          STANDARD-SLOT-DEFINITION 
          REMOVE-DIRECT-METHOD 
          CLASS-DIRECT-DEFAULT-INITARGS 
          SLOT-DEFINITION-INITFUNCTION 
          SLOT-DEFINITION-INITFORM
          class-source-position)
        )

(export '(satiate
          satiate-initialization
          apply-method
          ))

(export '(no-applicable-method-error))

(export '(disassemble-discriminator
          compilediscriminating-function ; also exported by runtime
          compile-all-generic-functions))

(export '(start-profiling stop-profiling
          report-profiling profiling-data
          with-profiling))
) ; eval-when
