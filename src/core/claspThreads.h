

struct BrclThread {

// Objects from Lisp object
	/*! Store the dynamic variable binding stack */
	DynamicBindingStack	_Bindings;

	/*! Store Catch info */
	Cons_sp 	_CatchInfo;
 
	/*! Generic functions method cache */
	Cache* 			_MethodCachePtr;

	/*! Generic functions slot cache */
	Cache*			_SlotCachePtr;


// InvocationStack needs to be here as well

};
