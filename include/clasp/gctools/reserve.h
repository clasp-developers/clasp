/*
    File: reserve.h
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
#define _GC_RESERVE_BEGIN_BASE(_class_, _obj_) \
  gctools::smart_ptr<_class_> _obj_;           \
  {                                            \
    mps_addr_t __reserve_addr(0);              \
    do {

#define _GC_RESERVE_GET_BASE(_class_, _kind_, _obj_)                                                                       \
  mps_res_t __gc_res = mps_reserve(&__reserve_addr, gctools::_global_obj_allocation_point, sizeof_with_header<_class_>()); \
  if (__gc_res != MPS_RES_OK)                                                                                              \
    THROW_HARD_ERROR(BF("Could not allocate %s") % #_class_);                                                              \
  gctools::Header_s *header = reinterpret_cast<gctools::Header_s *>(__reserve_addr);                                       \
  header->kind._Kind = (gctools::GcKindEnum)(_kind_);                                                                      \
  void *__obj_addr = BASE_TO_OBJ_PTR(__reserve_addr);                                                                      \
  DEBUG_MPS_ALLOCATION(__reserve_addr, __obj_addr, sizeof_with_header<_class_>(), _kind_);                                 \
  _obj_ = new (__obj_addr) /* Followed by the class in EVERY usage of GC_RESERVE_GET_BASE */

#define _GC_RESERVE_END_BASE(_class_)                                                                       \
  }                                                                                                         \
  while (!mps_commit(gctools::_global_obj_allocation_point, __reserve_addr, sizeof_with_header<_class_>())) \
    ;

#define _GC_RESERVE_FINALIZE() mps_finalize(global_arena, &__reserve_addr)

#define _GC_RESERVE_END_FINALyes_INITno(_class_, _obj_) \
  _GC_RESERVE_END_BASE(_class_);                        \
  _GC_RESERVE_FINALIZE();                               \
  }                                                     \
  poll_signals()
#define _GC_RESERVE_END_FINALyes_INITyes(_class_, _obj_) \
  _GC_RESERVE_END_BASE(_class_);                         \
  _GC_RESERVE_FINALIZE();                                \
  _obj_->initialize();                                   \
  }                                                      \
  poll_signals()

#define GC_RESERVE_BEGIN(_class_, _obj_) _GC_RESERVE_BEGIN_BASE(_class_, _obj_)
#define GC_RESERVE_GET(_class_, _obj_) _GC_RESERVE_GET_BASE(_class_, _class_::static_StampWtagMtag, _obj_) _class_();
#define GC_RESERVE_GET_VARIADIC(_class_, _obj_, ...) _GC_RESERVE_GET_BASE(_class_, _class_::static_StampWtagMtag, _obj_) _class_(__VA_ARGS__);
#define GC_RESERVE_END(_class_, _obj_) _GC_RESERVE_END_FINALyes_INITyes(_class_, _obj_)

#define GC_RESERVE(_class_, _obj_) GC_RESERVE_BEGIN(_class_, _obj_) GC_RESERVE_GET(_class_, _obj_) GC_RESERVE_END(_class_, _obj_)
#define GC_RESERVE_VARIADIC(_class_, _obj_, ...) GC_RESERVE_BEGIN(_class_, _obj_) GC_RESERVE_GET_VARIADIC(_class_, _obj_, __VA_ARGS__) GC_RESERVE_END(_class_, _obj_)

#define GC_COPY_BEGIN(_class_, _obj_) _GC_RESERVE_BEGIN_BASE(_class_, _obj_)
#define GC_COPY_GET(_class_, _obj_, _orig_) _GC_RESERVE_GET_VARIADIC(_class_, _obj_, _orig_)
#define GC_COPY_END(_class_, _obj_) _GC_RESERVE_END_FINALyes_INITno(_class_, _obj_)
#define GC_COPY(_class_, _obj_, _orig_) GC_COPY_BEGIN(_class_, _obj_) GC_COPY_GET(_class_, _obj_, _orig_) GC_COPY_END(_class_, _obj_)
