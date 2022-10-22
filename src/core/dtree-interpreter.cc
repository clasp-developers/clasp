case MAYBE_LONG_ADD + DTREE_OP_MISS:
goto DISPATCH_MISS;
#if defined(GENERAL_ARITY_CALL)
case MAYBE_LONG_ADD + DTREE_OP_ARGN: {
  size_t idx = ReadArg<MAYBE_LONG_MUL>::read(ip, (DTREE_ARGN_OFFSET));
  DTILOG("About to read arg %lu\n", idx);
  if (pass_args.nargs() <= idx) {
    // we use an intermediate function, in lisp, to get a nice error message.
    Function_sp generic_function = gfep->_GenericFunction;
    return core::eval::funcall(clos::_sym_interp_wrong_nargs, generic_function, make_fixnum(pass_args.nargs()));
  }
  arg = pass_args.iarg(idx);
  ip += ReadArg<MAYBE_LONG_MUL>::offset(DTREE_ARGN_NEXT_OFFSET);
  DTILOG("Got arg@%p %s new ip %p\n", (void *)arg.raw_(), _safe_rep_(arg), (void *)ip);
} break;
#endif
case MAYBE_LONG_ADD + DTREE_OP_TAG_TEST:
if (arg.fixnump()) {
  ip += ReadArg<MAYBE_LONG_MUL>::read(ip, (DTREE_FIXNUM_TAG_OFFSET));
  DTILOG("DTREE_OP_TAG_TEST: fixnum | ip <= %p\n", (void *)ip);
  break;
} else if (arg.consp()) {
  ip += ReadArg<MAYBE_LONG_MUL>::read(ip, (DTREE_CONS_TAG_OFFSET));
  DTILOG("DTREE_OP_TAG_TEST: cons | ip <= %p\n", (void *)ip);
  break;
} else if (arg.single_floatp()) {
  ip += ReadArg<MAYBE_LONG_MUL>::read(ip, (DTREE_SINGLE_FLOAT_TAG_OFFSET));
  DTILOG("DTREE_OP_TAG_TEST: single-float | ip <= %p\n", (void *)ip);
  break;
} else if (arg.characterp()) {
  DTILOG("character\n");
  ip += ReadArg<MAYBE_LONG_MUL>::read(ip, (DTREE_CHARACTER_TAG_OFFSET));
  DTILOG("DTREE_OP_TAG_TEST: character | ip <= %p\n", (void *)ip);
  break;
} else if (arg.generalp()) {
  ip += ReadArg<MAYBE_LONG_MUL>::offset(DTREE_GENERAL_TAG_OFFSET);
  DTILOG("DTREE_OP_TAG_TEST: general | ip <= %p\n", (void *)ip);
  break;
}
DTILOG("DTREE_OP_TAG_TEST: unknown\n");
// FIXME: We should be able to specialize on class valist and stuff.
SIMPLE_ERROR(("unknown tag for arg %s"), arg);
goto DISPATCH_MISS;
case MAYBE_LONG_ADD + DTREE_OP_STAMP_READ: {
  General_O *client_ptr = gctools::untag_general<General_O *>((General_O *)arg.raw_());
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
    DTILOG("  TRUE - ip <- %lu @ %p\n", (ip - ip0), (void *)ip);
  } else {
    ip += ReadArg<MAYBE_LONG_MUL>::offset(DTREE_LT_RIGHT_OFFSET);
    DTILOG("  FALSE - ip <- %lu @ %p\n", (ip - ip0), (void *)ip);
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
#if defined(GENERAL_ARITY_CALL)
case MAYBE_LONG_ADD + DTREE_OP_SLOT_READ: {
  DTILOG("reading slot: ");
  T_sp location = ReadArg<MAYBE_LONG_MUL>::read_literal(ip, (DTREE_SLOT_READER_INDEX_OFFSET), literals);
  T_sp slot_name = ReadArg<MAYBE_LONG_MUL>::read_literal(ip, (DTREE_SLOT_READER_SLOT_NAME_OFFSET), literals);
  DTILOG(" location: %s  name: %s\n", _safe_rep_(location), _safe_rep_(slot_name));
  size_t index = location.unsafe_fixnum();
  T_sp tinstance = pass_args.iarg(0);
  DTILOG("Got tinstance@%p %s\n", (void *)tinstance.raw_(), _safe_rep_(tinstance));
  Instance_sp instance((gc::Tagged)tinstance.raw_());
  DTILOG("instance %p index %lu\n", (void *)instance.raw_(), index);
  T_sp value = instance->instanceRef(index);
  if (value.unboundp()) {
    DTILOG("Slot was unbound\n");
    return core::eval::funcall(cl::_sym_slot_unbound, lisp_instance_class(tinstance), instance, slot_name);
  }
  DTILOG("Slot was read with value: %s\n", _safe_rep_(value));
  return gctools::return_type(value.raw_(), 1);
}
case MAYBE_LONG_ADD + DTREE_OP_CAR: {
  DTILOG("class cell\n");
  T_sp location = ReadArg<MAYBE_LONG_MUL>::read_literal(ip, (DTREE_SLOT_READER_INDEX_OFFSET), literals);
  T_sp slot_name = ReadArg<MAYBE_LONG_MUL>::read_literal(ip, (DTREE_SLOT_READER_SLOT_NAME_OFFSET), literals);
  Instance_sp instance = gc::As_unsafe<Instance_sp>(pass_args.iarg(0));
  DTILOG("Got instance@%p %s\n", (void *)instance.raw_(), _safe_rep_(instance));
  Cons_sp cell = gc::As_unsafe<Cons_sp>(location);
  T_sp value = CONS_CAR(cell);
  if (value.unboundp())
    return core::eval::funcall(cl::_sym_slot_unbound, lisp_instance_class(instance), instance, slot_name);
  DTILOG("read value: %s\n", _safe_rep_(value));
  return gctools::return_type(value.raw_(), 1);
}
case MAYBE_LONG_ADD + DTREE_OP_SLOT_WRITE: {
  DTILOG("writing slot: ");
  T_sp location = ReadArg<MAYBE_LONG_MUL>::read_literal(ip, (DTREE_SLOT_WRITER_INDEX_OFFSET), literals);
  size_t index = location.unsafe_fixnum();
  DTILOG("index %lu\n", index);
  T_sp value = pass_args.iarg(0);
  T_sp tinstance = pass_args.iarg(1);
  DTILOG("Got tinstance@%p %s\n", (void *)tinstance.raw_(), _safe_rep_(tinstance));
  Instance_sp instance((gc::Tagged)tinstance.raw_());
  instance->instanceSet(index, value);
  DTILOG("Set to value: %s\n", _safe_rep_(value));
  return gctools::return_type(value.raw_(), 1);
}
case MAYBE_LONG_ADD + DTREE_OP_RPLACA: {
  DTILOG("class cell\n");
  T_sp location = ReadArg<MAYBE_LONG_MUL>::read_literal(ip, (DTREE_SLOT_WRITER_INDEX_OFFSET), literals);
  size_t index = location.unsafe_fixnum();
  Cons_sp cell = gc::As_unsafe<Cons_sp>(location);
  T_sp value = pass_args.iarg(0);
  DTILOG("Got value@%p %s\n", (void *)value.raw_(), _safe_rep_(value));
  cell->rplaca(value);
  DTILOG("Set to value: %s\n", _safe_rep_(value));
  return gctools::return_type(value.raw_(), 1);
}
#else
#if ENABLE_REGISTER >= 0
case MAYBE_LONG_ADD + DTREE_OP_SLOT_READ: {
  DTILOG("reading slot: ");
  T_sp location = ReadArg<MAYBE_LONG_MUL>::read_literal(ip, (DTREE_SLOT_READER_INDEX_OFFSET), literals);
  T_sp slot_name = ReadArg<MAYBE_LONG_MUL>::read_literal(ip, (DTREE_SLOT_READER_SLOT_NAME_OFFSET), literals);
  DTILOG(" location: %s  name: %s\n", _safe_rep_(location), _safe_rep_(slot_name));
  size_t index = location.unsafe_fixnum();
  T_sp tinstance((gctools::Tagged)lcc_farg0);
  // Do I need to check if it's an Instance_sp here????
  Instance_sp instance = gc::As_unsafe<Instance_sp>(tinstance);
  DTILOG("instance %p index %lu\n", (void *)instance.raw_(), index);
  T_sp value = instance->instanceRef(index);
  if (value.unboundp()) {
    DTILOG("Slot was unbound\n");
    return core::eval::funcall(cl::_sym_slot_unbound, lisp_instance_class(tinstance), instance, slot_name);
  }
  DTILOG("Slot was read with value: %s\n", _safe_rep_(value));
  return gctools::return_type(value.raw_(), 1);
}
case MAYBE_LONG_ADD + DTREE_OP_CAR: {
  DTILOG("class cell\n");
  T_sp location = ReadArg<MAYBE_LONG_MUL>::read_literal(ip, (DTREE_SLOT_READER_INDEX_OFFSET), literals);
  T_sp slot_name = ReadArg<MAYBE_LONG_MUL>::read_literal(ip, (DTREE_SLOT_READER_SLOT_NAME_OFFSET), literals);
  T_sp tinstance((gctools::Tagged)lcc_farg0);
  // Do I need to check if it's an Instance_sp here????
  Instance_sp instance = gc::As_unsafe<Instance_sp>(tinstance);
  DTILOG("Got instance@%p %s\n", (void *)instance.raw_(), _safe_rep_(instance));
  Cons_sp cell = gc::As_unsafe<Cons_sp>(location);
  T_sp value = CONS_CAR(cell);
  if (value.unboundp())
    return core::eval::funcall(cl::_sym_slot_unbound, lisp_instance_class(instance), instance, slot_name);
  DTILOG("read value: %s\n", _safe_rep_(value));
  return gctools::return_type(value.raw_(), 1);
}
#endif
#if ENABLE_REGISTER >= 1
case MAYBE_LONG_ADD + DTREE_OP_SLOT_WRITE: {
  DTILOG("writing slot: ");
  T_sp location = ReadArg<MAYBE_LONG_MUL>::read_literal(ip, (DTREE_SLOT_WRITER_INDEX_OFFSET), literals);
  size_t index = location.unsafe_fixnum();
  DTILOG("index %lu\n", index);
  T_sp value((gc::Tagged)lcc_farg0);
  T_sp tinstance((gctools::Tagged)lcc_farg1);
  // Do I need to check if it's an Instance_sp here????
  Instance_sp instance = gc::As_unsafe<Instance_sp>(tinstance);
  instance->instanceSet(index, value);
  DTILOG("Set to value: %s\n", _safe_rep_(value));
  return gctools::return_type(value.raw_(), 1);
}
#endif
#if ENABLE_REGISTER >= 0
case MAYBE_LONG_ADD + DTREE_OP_RPLACA: {
  DTILOG("class cell\n");
  T_sp location = ReadArg<MAYBE_LONG_MUL>::read_literal(ip, (DTREE_SLOT_WRITER_INDEX_OFFSET), literals);
  size_t index = location.unsafe_fixnum();
  Cons_sp cell = gc::As_unsafe<Cons_sp>(location);
  T_sp value((gc::Tagged)lcc_farg0);
  DTILOG("Got value@%p %s\n", (void *)value.raw_(), _safe_rep_(value));
  cell->rplaca(value);
  DTILOG("Set to value: %s\n", _safe_rep_(value));
  return gctools::return_type(value.raw_(), 1);
}
#endif
#endif
case MAYBE_LONG_ADD + DTREE_OP_EFFECTIVE_METHOD: {
  DTILOG("effective method call\n");
  T_sp tfunc = ReadArg<MAYBE_LONG_MUL>::read_literal(ip, (DTREE_EFFECTIVE_METHOD_OFFSET), literals);
  Function_sp func = gc::As_unsafe<Function_sp>(tfunc);
  // Use the pass_args here because it points to the original arguments
  DTILOG(">>>>>>> DTREE_OP_EFFECTIVE_METHOD: Invoking effective method\n");
  ClaspXepFunction &xep = gc::As_assert<GlobalSimpleFunBase_sp>(func->_TheSimpleFun.load())->_EntryPoints;
  DTILOG("DTREE_OP_EFFECTIVE_METHOD: About to dump args\n");
#if defined(GENERAL_ARITY_CALL)
  DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"), &pass_args));
  return xep.invoke_n(func.raw_(), pass_args.nargs(), pass_args.args());
#elif (ENABLE_REGISTER == -1)
  DTILOG(" ---- done\n");
  return xep.invoke_0(func.raw_());
#elif (ENABLE_REGISTER == 0)
  DTILOG(" arg0 = %s\n", _safe_rep_(T_sp((gctools::Tagged)lcc_farg0)));
  DTILOG(" ---- done\n");
  return xep.invoke_1(func.raw_(), lcc_farg0);
#elif (ENABLE_REGISTER == 1)
  DTILOG(" arg0 = %s\n", _safe_rep_(T_sp((gctools::Tagged)lcc_farg0)));
  DTILOG(" arg1 = %s\n", _safe_rep_(T_sp((gctools::Tagged)lcc_farg1)));
  DTILOG(" ---- done\n");
  return xep.invoke_2(func.raw_(), lcc_farg0, lcc_farg1);
#elif (ENABLE_REGISTER == 2)
  DTILOG(" arg0 = %s\n", _safe_rep_(T_sp((gctools::Tagged)lcc_farg0)));
  DTILOG(" arg1 = %s\n", _safe_rep_(T_sp((gctools::Tagged)lcc_farg1)));
  DTILOG(" arg2 = %s\n", _safe_rep_(T_sp((gctools::Tagged)lcc_farg2)));
  DTILOG(" ---- done\n");
  return xep.invoke_3(func.raw_(), lcc_farg0, lcc_farg1, lcc_farg2);
#elif (ENABLE_REGISTER == 3)
  DTILOG(" arg0 = %s\n", _safe_rep_(T_sp((gctools::Tagged)lcc_farg0)));
  DTILOG(" arg1 = %s\n", _safe_rep_(T_sp((gctools::Tagged)lcc_farg1)));
  DTILOG(" arg2 = %s\n", _safe_rep_(T_sp((gctools::Tagged)lcc_farg2)));
  DTILOG(" arg3 = %s\n", _safe_rep_(T_sp((gctools::Tagged)lcc_farg3)));
  DTILOG(" ---- done\n");
  return xep.invoke_4(func.raw_(), lcc_farg0, lcc_farg1, lcc_farg2, lcc_farg3);
#elif (ENABLE_REGISTER == 4)
  DTILOG("DTREE_OP_EFFECTIVE_METHOD: About to args\n");
  DTILOG(" arg0 = %s\n", _safe_rep_(T_sp((gctools::Tagged)lcc_farg0)));
  DTILOG(" arg1 = %s\n", _safe_rep_(T_sp((gctools::Tagged)lcc_farg1)));
  DTILOG(" arg2 = %s\n", _safe_rep_(T_sp((gctools::Tagged)lcc_farg2)));
  DTILOG(" arg3 = %s\n", _safe_rep_(T_sp((gctools::Tagged)lcc_farg3)));
  DTILOG(" arg4 = %s\n", _safe_rep_(T_sp((gctools::Tagged)lcc_farg4)));
  DTILOG(" ---- done\n");
  return xep.invoke_5(func.raw_(), lcc_farg0, lcc_farg1, lcc_farg2, lcc_farg3, lcc_farg4);
#endif
}
#if !defined(GENERAL_ARITY_CALL)
#if (ENABLE_REGISTER >= 0)
case MAYBE_LONG_ADD + DTREE_OP_FARG0: {
  arg = T_sp((gctools::Tagged)lcc_farg0);
  ++ip;
  DTILOG("Got arg@%p %s new ip %p\n", (void *)arg.raw_(), _safe_rep_(arg), (void *)ip);
} break;
#if (ENABLE_REGISTER >= 1)
case MAYBE_LONG_ADD + DTREE_OP_FARG1: {
  arg = T_sp((gctools::Tagged)lcc_farg1);
  ++ip;
  DTILOG("Got arg@%p %s new ip %p\n", (void *)arg.raw_(), _safe_rep_(arg), (void *)ip);
} break;
#if (ENABLE_REGISTER >= 2)
case MAYBE_LONG_ADD + DTREE_OP_FARG2: {
  arg = T_sp((gctools::Tagged)lcc_farg2);
  ++ip;
  DTILOG("Got arg@%p %s new ip %p\n", (void *)arg.raw_(), _safe_rep_(arg), (void *)ip);
} break;
#if (ENABLE_REGISTER >= 3)
case MAYBE_LONG_ADD + DTREE_OP_FARG3: {
  arg = T_sp((gctools::Tagged)lcc_farg3);
  ++ip;
  DTILOG("Got arg@%p %s new ip %p\n", (void *)arg.raw_(), _safe_rep_(arg), (void *)ip);
} break;
#if (ENABLE_REGISTER >= 4)
case MAYBE_LONG_ADD + DTREE_OP_FARG4: {
  arg = T_sp((gctools::Tagged)lcc_farg4);
  ++ip;
  DTILOG("Got arg@%p %s new ip %p\n", (void *)arg.raw_(), _safe_rep_(arg), (void *)ip);
} break;
#endif // ENABLE_REGISTER >= 4
#endif // ENABLE_REGISTER >= 3
#endif // ENABLE_REGISTER >= 2
#endif // ENABLE_REGISTER >= 1
#endif // ENABLE_REGISTER >= 0
#else  // GENERAL_ARITY_CALL

case MAYBE_LONG_ADD + DTREE_OP_FARG0: {
  arg = pass_args.iarg(0);
  ++ip;
  DTILOG("Got arg@%p %s new ip %p\n", (void *)arg.raw_(), _safe_rep_(arg), (void *)ip);
} break;
case MAYBE_LONG_ADD + DTREE_OP_FARG1: {
  arg = pass_args.iarg(1);
  ++ip;
  DTILOG("Got arg@%p %s new ip %p\n", (void *)arg.raw_(), _safe_rep_(arg), (void *)ip);
} break;
case MAYBE_LONG_ADD + DTREE_OP_FARG2: {
  arg = pass_args.iarg(2);
  ++ip;
  DTILOG("Got arg@%p %s new ip %p\n", (void *)arg.raw_(), _safe_rep_(arg), (void *)ip);
} break;
case MAYBE_LONG_ADD + DTREE_OP_FARG3: {
  arg = pass_args.iarg(3);
  ++ip;
  DTILOG("Got arg@%p %s new ip %p\n", (void *)arg.raw_(), _safe_rep_(arg), (void *)ip);
} break;
case MAYBE_LONG_ADD + DTREE_OP_FARG4: {
  arg = pass_args.iarg(4);
  ++ip;
  DTILOG("Got arg@%p %s new ip %p\n", (void *)arg.raw_(), _safe_rep_(arg), (void *)ip);
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

#endif
