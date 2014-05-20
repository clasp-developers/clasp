

#if defined(USE_MPS)
mps_res_t mps_scan_LispRoots(mps_ss_t ss, mps_thr_t thr, void *p, size_t s, Lisp_O::GCRoots& lispRoots)
    {
	mps_scan_InvocationHistoryStack(ss,thr,p,s,this->_InvocationHistoryStack);
	mps_scan_MultipleValues(ss,thr,p,s,this->_MultipleValues);
	mps_scan_DynamicBindingStack(ss,thr,p,s,this->_Bindings);
	mps_scan_Cache(ss,thr,p,s,this->_MethodCachePtr);
	mps_scan_Cache(ss,thr,p,s,this->_SlotCachePtr);
	STLMAP_SMART_SECOND_FIX(this->_SourceFiles)
	GCHOLDER_UNORDEREDSET_FIX(this->_TraceFunctions);
	SMART_PTR_FIX(this->_SystemProperties);
	SMART_PTR_FIX(this->_CatchInfo);
	GCHOLDER_SYMBOLMAP_FIX(this->_BootClassTable);
	STLMAP_SMART_SECOND_FIX(this->_LoadTimeValueArrays);
	SMART_PTR_FIX(this->_Packages);
	GCHOLDER_SYMBOLMAP_FIX(this->_SetfDefinitions);
	SMART_PTR_FIX(this->_CorePackage);
	SMART_PTR_FIX(this->_KeywordPackage);
	SMART_PTR_FIX(this->_HiddenBinder);
	GCHOLDER_SYMBOLMAP_FIX(this->_SpecialForms);
	GCHOLDER_SYMBOLMAP_FIX(this->_SingleDispatchGenericFunctionTable);
	SMART_PTR_FIX(this->_TrueObject);
	SMART_PTR_FIX(this->_RehashSize);
	SMART_PTR_FIX(this->_RehashThreshold);
	SMART_PTR_FIX(this->_NullStream);
    };







