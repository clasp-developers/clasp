/*
    File: lisp_gc.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */

#if defined(USE_MPS)
mps_res_t mps_scan_LispRoots(mps_ss_t ss, mps_thr_t thr, void *p, size_t s, Lisp_O::GCRoots &lispRoots) {
  mps_scan_InvocationHistoryStack(ss, thr, p, s, this->_InvocationHistoryStack);
  mps_scan_MultipleValues(ss, thr, p, s, this->_MultipleValues);
  mps_scan_DynamicBindingStack(ss, thr, p, s, this->_Bindings);
  // this->_SourceFiles is a Vec0
//  STLMAP_SMART_SECOND_FIX(this->_SourceFiles)
  GCHOLDER_UNORDEREDSET_FIX(this->_TraceFunctions);
  SMART_PTR_FIX(this->_SystemProperties);
  SMART_PTR_FIX(this->_CatchInfo);
  GCHOLDER_SYMBOLMAP_FIX(this->_BootClassTable);
  STLMAP_SMART_SECOND_FIX(this->_LoadTimeValueArrays);
  SMART_PTR_FIX(this->_Packages);
//  GCHOLDER_SYMBOLMAP_FIX(this->_SetfDefinitions);
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
