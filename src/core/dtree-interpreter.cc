// This file is #included in funcallableInstance.cc four times -
// twice in each of entry_point_n and entry_point_fixed.
// Within each, it's #included with MAYBE_LONG_ADD defined as 0 or 1.

case MAYBE_LONG_ADD + DTREE_OP_MISS:
goto DISPATCH_MISS;
case MAYBE_LONG_ADD + DTREE_OP_ARGN: {
  size_t idx = ReadArg<MAYBE_LONG_MUL>::read(ip, (DTREE_ARGN_OFFSET));
  DTILOG("About to read arg %lu\n", idx);
#if defined(GENERAL_ENTRY)
  if (lcc_nargs <= idx) {
  // we use an intermediate function, in lisp, to get a nice error message.
    Function_sp generic_function = gfep->_GenericFunction;
    return core::eval::funcall(clos::_sym_interp_wrong_nargs, generic_function, make_fixnum(lcc_nargs));
  }
  arg = T_sp((gctools::Tagged)(lcc_args[idx]));
#else
  arg = T_sp((gctools::Tagged)fargn(lcc_closure, idx, args));
#endif
  ip += ReadArg<MAYBE_LONG_MUL>::offset(DTREE_ARGN_NEXT_OFFSET);
  DTILOG("Got arg@%p %s new ip %p\n", (void*)arg.raw_(), _safe_rep_(arg), (void*)ip);
} break;
case MAYBE_LONG_ADD + DTREE_OP_TAG_TEST:
if (arg.fixnump()) {
  ip += ReadArg<MAYBE_LONG_MUL>::read(ip, (DTREE_FIXNUM_TAG_OFFSET));
  DTILOG("DTREE_OP_TAG_TEST: fixnum | ip <= %p\n", (void*)ip);
  break;
} else if (arg.consp()) {
  ip += ReadArg<MAYBE_LONG_MUL>::read(ip, (DTREE_CONS_TAG_OFFSET));
  DTILOG("DTREE_OP_TAG_TEST: cons | ip <= %p\n", (void*)ip);
  break;
} else if (arg.single_floatp()) {
  ip += ReadArg<MAYBE_LONG_MUL>::read(ip, (DTREE_SINGLE_FLOAT_TAG_OFFSET));
  DTILOG("DTREE_OP_TAG_TEST: single-float | ip <= %p\n", (void*)ip);
  break;
} else if (arg.characterp()) {
  DTILOG("character\n");
  ip += ReadArg<MAYBE_LONG_MUL>::read(ip, (DTREE_CHARACTER_TAG_OFFSET));
  DTILOG("DTREE_OP_TAG_TEST: character | ip <= %p\n", (void*)ip);
  break;
} else if (arg.generalp()) {
  ip += ReadArg<MAYBE_LONG_MUL>::offset(DTREE_GENERAL_TAG_OFFSET);
  DTILOG("DTREE_OP_TAG_TEST: general | ip <= %p\n", (void*)ip);
  break;
}
DTILOG("DTREE_OP_TAG_TEST: unknown\n");
// FIXME: We should be able to specialize on class valist and stuff.
SIMPLE_ERROR("unknown tag for arg {}", arg);
goto DISPATCH_MISS;
case MAYBE_LONG_ADD + DTREE_OP_STAMP_READ: {
  General_O* client_ptr = gctools::untag_general<General_O*>((General_O*)arg.raw_());
  stamp = (uintptr_t)(llvmo::template_read_general_stamp(client_ptr));
  uintptr_t where = stamp & gctools::Header_s::where_mask;
  switch (where) {
  case gctools::Header_s::header_wtag:
    ip += ReadArg<MAYBE_LONG_MUL>::read(ip, (DTREE_READ_HEADER_OFFSET));
    break;
  case gctools::Header_s::rack_wtag:
    stamp = (uintptr_t)(llvmo::template_read_rack_stamp(client_ptr));
    ip += ReadArg<MAYBE_LONG_MUL>::offset(DTREE_READ_OTHER_OFFSET);
    break;
  case gctools::Header_s::wrapped_wtag:
    stamp = (uintptr_t)(llvmo::template_read_wrapped_stamp(client_ptr));
    ip += ReadArg<MAYBE_LONG_MUL>::offset(DTREE_READ_OTHER_OFFSET);
    break;
  case gctools::Header_s::derivable_wtag:
    stamp = (uintptr_t)(llvmo::template_read_derived_stamp(client_ptr));
    ip += ReadArg<MAYBE_LONG_MUL>::offset(DTREE_READ_OTHER_OFFSET);
    break;
  }
  DTILOG(" stamp read: %lu\n", stamp);
  break;
}
case MAYBE_LONG_ADD + DTREE_OP_LT_BRANCH: {
  // The stamps are from Common Lisp, so they're tagged fixnums. Don't untag.
  uintptr_t pivot = ReadArg<MAYBE_LONG_MUL>::read_literal_tagged(ip, (DTREE_LT_PIVOT_OFFSET), literals);
  DTILOG("testing stamp %lu < pivot %lu\n", stamp, pivot);
  if (stamp < pivot) {
    ip += ReadArg<MAYBE_LONG_MUL>::read(ip, (DTREE_LT_LEFT_OFFSET));
    DTILOG("  TRUE - ip <- %lu @ %p\n", (ip - ip0), (void*)ip);
  } else {
    ip += ReadArg<MAYBE_LONG_MUL>::offset(DTREE_LT_RIGHT_OFFSET);
    DTILOG("  FALSE - ip <- %lu @ %p\n", (ip - ip0), (void*)ip);
  }
  break;
}
case MAYBE_LONG_ADD + DTREE_OP_EQ_CHECK: {
  uintptr_t pivot = ReadArg<MAYBE_LONG_MUL>::read_literal_tagged(ip, (DTREE_EQ_PIVOT_OFFSET), literals);
  DTILOG("testing - pivot %lu  stamp: %lu  EQ -> %d\n", pivot, stamp, (stamp == pivot));
  if (stamp != pivot)
    goto DISPATCH_MISS;
  ip += ReadArg<MAYBE_LONG_MUL>::offset(DTREE_EQ_NEXT_OFFSET);
  break;
}
case MAYBE_LONG_ADD + DTREE_OP_RANGE_CHECK: {
  uintptr_t min = ReadArg<MAYBE_LONG_MUL>::read_literal_tagged(ip, (DTREE_RANGE_MIN_OFFSET), literals);
  uintptr_t max = ReadArg<MAYBE_LONG_MUL>::read_literal_tagged(ip, (DTREE_RANGE_MAX_OFFSET), literals);
  DTILOG("testing > %lu and < %lu\n", min, max);
  if (stamp < min || stamp > max)
    goto DISPATCH_MISS;
  ip += ReadArg<MAYBE_LONG_MUL>::offset(DTREE_RANGE_NEXT_OFFSET);
  break;
}
case MAYBE_LONG_ADD + DTREE_OP_EQL: {
  T_sp object = ReadArg<MAYBE_LONG_MUL>::read_literal(ip, (DTREE_EQL_OBJECT_OFFSET), literals);
  if (cl__eql(arg, object))
    ip += ReadArg<MAYBE_LONG_MUL>::read(ip, (DTREE_EQL_BRANCH_OFFSET));
  else
    ip += ReadArg<MAYBE_LONG_MUL>::offset(DTREE_EQL_NEXT_OFFSET);
  break;
}
case MAYBE_LONG_ADD + DTREE_OP_SLOT_READ: {
  DTILOG("reading slot: ");
#if defined(GENERAL_ENTRY)
  if (lcc_nargs != 1)
    wrongNumberOfArgumentsForGenericFunction(lcc_closure, lcc_nargs);
  arg = T_sp((gctools::Tagged)(lcc_args[0]));
#else
  if constexpr(fixed_nargs == 1) {
    arg = T_sp((gc::Tagged)(std::get<0>(args)));
  } else
    wrongNumberOfArgumentsForGenericFunction(lcc_closure, fixed_nargs);
#endif
  T_sp location = ReadArg<MAYBE_LONG_MUL>::read_literal(ip, (DTREE_SLOT_READER_INDEX_OFFSET), literals);
  T_sp slot_name = ReadArg<MAYBE_LONG_MUL>::read_literal(ip, (DTREE_SLOT_READER_SLOT_NAME_OFFSET), literals);
  DTILOG(" location: %s  name: %s\n", _safe_rep_(location), _safe_rep_(slot_name));
  size_t index = location.unsafe_fixnum();
  DTILOG("Got tinstance@%p %s\n", (void*)arg.raw_(), _safe_rep_(arg));

#ifdef DEBUG_ASSERT
  if (!(gc::IsA<Instance_sp>(arg) || gc::IsA<FuncallableInstance_sp>(arg))) {
    THROW_HARD_ERROR("arg = {} must be an Instance_sp or FuncallableInstance_sp",(void*)arg.raw_());
  }
#endif
  Instance_sp instance = gc::As_unsafe<Instance_sp>(arg);
  DTILOG("instance %p index %lu\n", (void*)instance.raw_(), index);
  T_sp value = instance->instanceRef(index);
  if (value.unboundp()) {
    DTILOG("Slot was unbound\n");
    return core::eval::funcall(cl::_sym_slot_unbound, lisp_instance_class(arg), instance, slot_name);
  }
  DTILOG("Slot was read with value: %s\n", _safe_rep_(value));
  return gctools::return_type(value.raw_(), 1);
}
case MAYBE_LONG_ADD + DTREE_OP_CAR: {
  DTILOG("class cell\n");
#if defined(GENERAL_ENTRY)
  if (lcc_nargs != 1)
    wrongNumberOfArgumentsForGenericFunction(lcc_closure, lcc_nargs);
  arg = T_sp((gctools::Tagged)(lcc_args[0]));
#else
  if constexpr(fixed_nargs == 1) {
    arg = T_sp((gctools::Tagged)(std::get<0>(args)));
  } else
    wrongNumberOfArgumentsForGenericFunction(lcc_closure, fixed_nargs);
#endif
  T_sp location = ReadArg<MAYBE_LONG_MUL>::read_literal(ip, (DTREE_SLOT_READER_INDEX_OFFSET), literals);
  T_sp slot_name = ReadArg<MAYBE_LONG_MUL>::read_literal(ip, (DTREE_SLOT_READER_SLOT_NAME_OFFSET), literals);
  // As_unsafe is ok since we couldn't be here unless we've already tested it.
  Instance_sp instance = gc::As_unsafe<Instance_sp>(arg);
  DTILOG("Got instance@%p %s\n", (void*)instance.raw_(), _safe_rep_(instance));
  Cons_sp cell = gc::As_unsafe<Cons_sp>(location);
  T_sp value = CONS_CAR(cell);
  if (value.unboundp())
    return core::eval::funcall(cl::_sym_slot_unbound, lisp_instance_class(instance), instance, slot_name);
  DTILOG("read value: %s\n", _safe_rep_(value));
  return gctools::return_type(value.raw_(), 1);
}
case MAYBE_LONG_ADD + DTREE_OP_SLOT_WRITE: {
  DTILOG("writing slot: ");
  // initializations maybe unnecessary?
#if defined(GENERAL_ENTRY)
  if (lcc_nargs != 2)
    wrongNumberOfArgumentsForGenericFunction(lcc_closure, lcc_nargs);
  T_sp value = T_sp((gc::Tagged)(lcc_args[0]));
  T_sp tinstance = T_sp((gc::Tagged)(lcc_args[1]));
#else
  T_sp value, tinstance;
  if constexpr(fixed_nargs == 2) {
    value = T_sp((gc::Tagged)(std::get<0>(args)));
    tinstance = T_sp((gc::Tagged)(std::get<1>(args)));
  } else
    wrongNumberOfArgumentsForGenericFunction(lcc_closure, fixed_nargs);
#endif
  T_sp location = ReadArg<MAYBE_LONG_MUL>::read_literal(ip, (DTREE_SLOT_WRITER_INDEX_OFFSET), literals);
  size_t index = location.unsafe_fixnum();
  DTILOG("index %lu\n", index);
  DTILOG("Got tinstance@%p %s\n", (void*)tinstance.raw_(), _safe_rep_(tinstance));
  Instance_sp instance((gc::Tagged)tinstance.raw_());
  instance->instanceSet(index, value);
  DTILOG("Set to value: %s\n", _safe_rep_(value));
  return gctools::return_type(value.raw_(), 1);
}
case MAYBE_LONG_ADD + DTREE_OP_RPLACA: {
  DTILOG("class cell\n");
#if defined(GENERAL_ENTRY)
  if (lcc_nargs != 2)
    wrongNumberOfArgumentsForGenericFunction(lcc_closure, lcc_nargs);
  arg = T_sp((gctools::Tagged)(lcc_args[0]));
#else
  if constexpr(fixed_nargs == 2) {
    arg = T_sp((gc::Tagged)(std::get<0>(args)));
  } else
    wrongNumberOfArgumentsForGenericFunction(lcc_closure, fixed_nargs);
#endif
  T_sp location = ReadArg<MAYBE_LONG_MUL>::read_literal(ip, (DTREE_SLOT_WRITER_INDEX_OFFSET), literals);
  Cons_sp cell = gc::As_unsafe<Cons_sp>(location);
  DTILOG("Got value@%p %s\n", (void*)arg.raw_(), _safe_rep_(arg));
  cell->rplaca(arg);
  DTILOG("Set to value: %s\n", _safe_rep_(arg));
  return gctools::return_type(arg.raw_(), 1);
}
case MAYBE_LONG_ADD + DTREE_OP_EFFECTIVE_METHOD: {
  DTILOG("effective method call\n");
  T_sp tfunc = ReadArg<MAYBE_LONG_MUL>::read_literal(ip, (DTREE_EFFECTIVE_METHOD_OFFSET), literals);
  Function_sp func = gc::As_unsafe<Function_sp>(tfunc);
  DTILOG(">>>>>>> DTREE_OP_EFFECTIVE_METHOD: Invoking effective method\n");
  DTILOG("DTREE_OP_EFFECTIVE_METHOD: About to dump args\n");
#if defined(GENERAL_ENTRY)
  DTIDO(dump_lcc_args(monitor_file("dtree-interp"), lcc_nargs, lcc_args));
  return func->apply_raw(lcc_nargs, lcc_args);
#else
  DTIDO_ALWAYS((fprintf(DTILOG_fout, " arg = %s\n", _safe_rep_(T_sp((gctools::Tagged)lcc_args))), ...););
  return func->funcall_raw(lcc_args...);
#endif
}
case MAYBE_LONG_ADD + DTREE_OP_FARG0: {
#if defined(GENERAL_ENTRY)
  // We don't need to check argcount since entry_point_n does.
  // (We shouldn't need to for SLOT_READ etc. either, but there's
  //  a bug in FuncallableInstance - it tracks the number of specialized
  //  parameters, but what we really want here is required parameters.
  //  FIXME)
  arg = T_sp((gctools::Tagged)(lcc_args[0]));
#else
  if constexpr(fixed_nargs > 0) {
    arg = T_sp((gctools::Tagged)(std::get<0>(args)));
  } else
    wrongNumberOfArgumentsForGenericFunction(lcc_closure, fixed_nargs);
#endif
  ++ip;
  DTILOG("Got arg@%p %s new ip %p\n", (void*)arg.raw_(), _safe_rep_(arg), (void*)ip);
} break;
case MAYBE_LONG_ADD + DTREE_OP_FARG1: {
#if defined(GENERAL_ENTRY)
  arg = T_sp((gctools::Tagged)(lcc_args[1]));
#else
  if constexpr(fixed_nargs > 1) {
    arg = T_sp((gctools::Tagged)(std::get<1>(args)));
  } else
    wrongNumberOfArgumentsForGenericFunction(lcc_closure, fixed_nargs);
#endif
  ++ip;
  DTILOG("Got arg@%p %s new ip %p\n", (void*)arg.raw_(), _safe_rep_(arg), (void*)ip);
} break;
case MAYBE_LONG_ADD + DTREE_OP_FARG2: {
#if defined(GENERAL_ENTRY)
  arg = T_sp((gctools::Tagged)(lcc_args[2]));
#else
  if constexpr(fixed_nargs > 2) {
    arg = T_sp((gctools::Tagged)(std::get<2>(args)));
  } else
    wrongNumberOfArgumentsForGenericFunction(lcc_closure, fixed_nargs);
#endif
  ++ip;
  DTILOG("Got arg@%p %s new ip %p\n", (void*)arg.raw_(), _safe_rep_(arg), (void*)ip);
} break;
case MAYBE_LONG_ADD + DTREE_OP_FARG3: {
#if defined(GENERAL_ENTRY)
  arg = T_sp((gctools::Tagged)(lcc_args[3]));
#else
  if constexpr(fixed_nargs > 3) {
    arg = T_sp((gctools::Tagged)(std::get<3>(args)));
  } else
    wrongNumberOfArgumentsForGenericFunction(lcc_closure, fixed_nargs);
#endif
  ++ip;
  DTILOG("Got arg@%p %s new ip %p\n", (void*)arg.raw_(), _safe_rep_(arg), (void*)ip);
} break;
case MAYBE_LONG_ADD + DTREE_OP_FARG4: {
#if defined(GENERAL_ENTRY)
  arg = T_sp((gctools::Tagged)(lcc_args[4]));
#else
  if constexpr(fixed_nargs > 4) {
    arg = T_sp((gctools::Tagged)(std::get<4>(args)));
  } else
    wrongNumberOfArgumentsForGenericFunction(lcc_closure, fixed_nargs);
#endif
  ++ip;
  DTILOG("Got arg@%p %s new ip %p\n", (void*)arg.raw_(), _safe_rep_(arg), (void*)ip);
} break;
case MAYBE_LONG_ADD + DTREE_OP_SD_EQ_BRANCH: {
  uintptr_t pivot = ReadArg<MAYBE_LONG_MUL>::read_literal_tagged(ip, (DTREE_SD_STAMP_OFFSET), literals);
  DTILOG("testing - pivot %lu  stamp: %lu  EQ -> %d\n", pivot, stamp, (stamp == pivot));
  if (stamp != pivot) {
    ip += ReadArg<MAYBE_LONG_MUL>::offset(DTREE_SD_FAIL_OFFSET);
  } else {
    ip += ReadArg<MAYBE_LONG_MUL>::offset(DTREE_SD_NEXT_OFFSET);
  }
  break;
}
case MAYBE_LONG_ADD + DTREE_OP_SINGLE_DISPATCH_MISS:
goto SINGLE_DISPATCH_MISS;
