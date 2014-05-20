// start
 // class MpiLispHolder : public LispHolder
#ifdef Use_ClPackage
extern void Register_Object_O(Lisp_sp); // base: 
{_BLOCK_TRACE("initializing Register_Object_O");
	Register_Object_O(lisp); // base: 
}
#endif // ifdef Use_ClPackage
#ifdef Use_ClPackage
extern void Register_Model_O(Lisp_sp); // base: Object_O
{_BLOCK_TRACE("initializing Register_Model_O");
	Register_Model_O(lisp); // base: Object_O
}
#endif // ifdef Use_ClPackage
#ifdef Use_ClPackage
extern void Register_CandoObject_O(Lisp_sp); // base: Object_O
{_BLOCK_TRACE("initializing Register_CandoObject_O");
	Register_CandoObject_O(lisp); // base: Object_O
}
#endif // ifdef Use_ClPackage
#ifdef Use_MpiPackage
extern void Register_Mpi_O(Lisp_sp); // base: Object_O
{_BLOCK_TRACE("initializing Register_Mpi_O");
	Register_Mpi_O(lisp); // base: Object_O
}
#endif // ifdef Use_MpiPackage
#ifdef Use_ClPackage
extern void Register_ExternalObject_O(Lisp_sp); // base: CandoObject_O
{_BLOCK_TRACE("initializing Register_ExternalObject_O");
	Register_ExternalObject_O(lisp); // base: CandoObject_O
}
#endif // ifdef Use_ClPackage
