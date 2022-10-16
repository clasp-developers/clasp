    case MAYBE_LONG_ADD+DTREE_OP_MISS:
        goto DISPATCH_MISS;
    case MAYBE_LONG_ADD+DTREE_OP_ADVANCE: {
      DTILOG("About to read arg dispatch_args-> %p\n" , (void*)dispatch_args.raw_());
      DTILOG("About to dump dispatch_args Vaslist\n");
      DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*dispatch_args));
      if (dispatch_args->nargs_zero())
          // we use an intermediate function, in lisp, to get a nice error message.
        return core::eval::funcall(clos::_sym_interp_wrong_nargs,
                                   generic_function, make_fixnum(nargs));
      arg = dispatch_args->next_arg();
      ++ip;
      DTILOG("Got arg@%p %s new ip %p\n" , (void*)arg.raw_() , _safe_rep_(arg), (void*)ip);
    }
        break;
    case MAYBE_LONG_ADD+DTREE_OP_TAG_TEST:
        if (arg.fixnump()) {
          ip += ReadArg<MAYBE_LONG_MUL>::read(ip,(DTREE_FIXNUM_TAG_OFFSET));
          DTILOG("DTREE_OP_TAG_TEST: fixnum | ip <= %p\n", (void*)ip);
          break;
        } else if (arg.consp()) {
          ip += ReadArg<MAYBE_LONG_MUL>::read(ip,(DTREE_CONS_TAG_OFFSET));
          DTILOG("DTREE_OP_TAG_TEST: cons | ip <= %p\n", (void*)ip);
          break;
        } else if (arg.single_floatp()) {
          ip += ReadArg<MAYBE_LONG_MUL>::read(ip,(DTREE_SINGLE_FLOAT_TAG_OFFSET));
          DTILOG("DTREE_OP_TAG_TEST: single-float | ip <= %p\n", (void*)ip);
          break;
        } else if (arg.characterp()) {
          DTILOG("character\n");
          ip += ReadArg<MAYBE_LONG_MUL>::read(ip,(DTREE_CHARACTER_TAG_OFFSET));
          DTILOG("DTREE_OP_TAG_TEST: character | ip <= %p\n", (void*)ip);
          break;
        } else if (arg.generalp()) {
          ip += ReadArg<MAYBE_LONG_MUL>::offset(DTREE_GENERAL_TAG_OFFSET);
          DTILOG("DTREE_OP_TAG_TEST: general | ip <= %p\n", (void*)ip);
          break;
        }
        DTILOG("DTREE_OP_TAG_TEST: unknown\n");
        // FIXME: We should be able to specialize on class valist and stuff.
        SIMPLE_ERROR(("unknown tag for arg %s") , arg);
        goto DISPATCH_MISS;
    case MAYBE_LONG_ADD+DTREE_OP_STAMP_READ:
      {
        General_O* client_ptr = gctools::untag_general<General_O*>((General_O*)arg.raw_());
        stamp = (uintptr_t)(llvmo::template_read_general_stamp(client_ptr));
        uintptr_t where = stamp & gctools::Header_s::where_mask;
        switch (where) {
        case gctools::Header_s::header_wtag:
            ip += ReadArg<MAYBE_LONG_MUL>::read(ip,(DTREE_READ_HEADER_OFFSET));
            break;
        case gctools::Header_s::rack_wtag:
            stamp = (uintptr_t)(llvmo::template_read_rack_stamp(client_ptr));
            ip += ReadArg<MAYBE_LONG_MUL>::offset(DTREE_READ_OTHER_OFFSET); break;
        case gctools::Header_s::wrapped_wtag:
            stamp = (uintptr_t)(llvmo::template_read_wrapped_stamp(client_ptr));
            ip += ReadArg<MAYBE_LONG_MUL>::offset(DTREE_READ_OTHER_OFFSET); break;
        case gctools::Header_s::derivable_wtag:
            stamp = (uintptr_t)(llvmo::template_read_derived_stamp(client_ptr));
            ip += ReadArg<MAYBE_LONG_MUL>::offset(DTREE_READ_OTHER_OFFSET); break;
        }
        DTILOG(" stamp read: %lu\n" , stamp );
        break;
      }
    case MAYBE_LONG_ADD+DTREE_OP_LT_BRANCH:
      {
        // The stamps are from Common Lisp, so they're tagged fixnums. Don't untag.
        uintptr_t pivot = ReadArg<MAYBE_LONG_MUL>::read_literal_tagged(ip,(DTREE_LT_PIVOT_OFFSET),literals);
        DTILOG("testing stamp %lu < pivot %lu\n" , stamp, pivot);
        if (stamp < pivot) {
          ip += ReadArg<MAYBE_LONG_MUL>::read(ip,(DTREE_LT_LEFT_OFFSET));
          DTILOG("  TRUE - ip <- %lu @ %p\n" , (ip-ip0), (void*)ip );
        } else {
          ip += ReadArg<MAYBE_LONG_MUL>::offset(DTREE_LT_RIGHT_OFFSET);
          DTILOG("  FALSE - ip <- %lu @ %p\n" , (ip-ip0), (void*)ip );
        }
        break;
      }
    case MAYBE_LONG_ADD+DTREE_OP_EQ_CHECK:
      {
        uintptr_t pivot = ReadArg<MAYBE_LONG_MUL>::read_literal_tagged(ip,(DTREE_EQ_PIVOT_OFFSET),literals);
        DTILOG("testing - pivot %lu  stamp: %lu  EQ -> %d\n" , pivot , stamp , (stamp == pivot) );
        if (stamp != pivot) goto DISPATCH_MISS;
        ip += ReadArg<MAYBE_LONG_MUL>::offset(DTREE_EQ_NEXT_OFFSET);
        break;
      }
    case MAYBE_LONG_ADD+DTREE_OP_RANGE_CHECK:
      {
        uintptr_t min = ReadArg<MAYBE_LONG_MUL>::read_literal_tagged(ip,(DTREE_RANGE_MIN_OFFSET),literals);
        uintptr_t max = ReadArg<MAYBE_LONG_MUL>::read_literal_tagged(ip,(DTREE_RANGE_MAX_OFFSET),literals);
        DTILOG("testing > %lu and < %lu\n" , min , max);
        if (stamp < min || stamp > max) goto DISPATCH_MISS;
        ip += ReadArg<MAYBE_LONG_MUL>::offset(DTREE_RANGE_NEXT_OFFSET);
        break;
      }
    case MAYBE_LONG_ADD+DTREE_OP_EQL:
      {
        T_sp object = ReadArg<MAYBE_LONG_MUL>::read_literal(ip,(DTREE_EQL_OBJECT_OFFSET),literals);
        if (cl__eql(arg, object))
          ip += ReadArg<MAYBE_LONG_MUL>::read(ip,(DTREE_EQL_BRANCH_OFFSET));
        else ip += ReadArg<MAYBE_LONG_MUL>::offset(DTREE_EQL_NEXT_OFFSET);
        break;
      }
    case MAYBE_LONG_ADD+DTREE_OP_SLOT_READ:
      {
        DTILOG("reading slot: ");
        T_sp location = ReadArg<MAYBE_LONG_MUL>::read_literal(ip,(DTREE_SLOT_READER_INDEX_OFFSET),literals);
        T_sp slot_name = ReadArg<MAYBE_LONG_MUL>::read_literal(ip,(DTREE_SLOT_READER_SLOT_NAME_OFFSET),literals);
        DTILOG(" location: %s  name: %s\n", _safe_rep_(location), _safe_rep_(slot_name));
        size_t index = location.unsafe_fixnum();
        DTILOG("DTREE_OP_SLOT_READ: About to dump pass_args Vaslist\n");
        DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*pass_args));
        T_sp tinstance = pass_args->next_arg();
        DTILOG("Got tinstance@%p %s\n" , (void*)tinstance.raw_() , _safe_rep_(tinstance));
        DTILOG("DTREE_OP_SLOT_READ: About to dump pass_args Vaslist AFTER next_arg\n");
        DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*pass_args));
        Instance_sp instance((gc::Tagged)tinstance.raw_());
        DTILOG("instance %p index %lu\n" , (void*)instance.raw_() , index);
        T_sp value = instance->instanceRef(index);
        if (value.unboundp()) {
          DTILOG("Slot was unbound\n");
          return core::eval::funcall(cl::_sym_slot_unbound,
                                     lisp_instance_class(tinstance),
                                     instance,slot_name);
        }
        DTILOG("Slot was read with value: %s\n", _safe_rep_(value));
        return gctools::return_type(value.raw_(),1);
      }
    case MAYBE_LONG_ADD+DTREE_OP_CAR:
      {
        DTILOG("class cell\n");
        T_sp location = ReadArg<MAYBE_LONG_MUL>::read_literal(ip,(DTREE_SLOT_READER_INDEX_OFFSET),literals);
        T_sp slot_name = ReadArg<MAYBE_LONG_MUL>::read_literal(ip,(DTREE_SLOT_READER_SLOT_NAME_OFFSET),literals);
        DTILOG("DTREE_OP_CAR: About to dump pass_args Vaslist\n");
        DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*pass_args));
        Instance_sp instance = gc::As_unsafe<Instance_sp>(pass_args->next_arg());
        DTILOG("Got instance@%p %s\n" , (void*)instance.raw_() , _safe_rep_(instance));
        Cons_sp cell = gc::As_unsafe<Cons_sp>(location);
        T_sp value = CONS_CAR(cell);
        if (value.unboundp())
          return core::eval::funcall(cl::_sym_slot_unbound,
                                     lisp_instance_class(instance),
                                     instance,slot_name);
        return gctools::return_type(value.raw_(),1);
      }
    case MAYBE_LONG_ADD+DTREE_OP_SLOT_WRITE:
      {
        DTILOG("writing slot: ");
        T_sp location = ReadArg<MAYBE_LONG_MUL>::read_literal(ip,(DTREE_SLOT_WRITER_INDEX_OFFSET),literals);
        size_t index = location.unsafe_fixnum();
        DTILOG("index %lu\n" , index);
        DTILOG("DTREE_OP_SLOT_WRITE: About to dump pass_args Vaslist\n");
        DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*pass_args));
        T_sp value((gc::Tagged)pass_args->next_arg_raw());
        DTILOG("DTREE_OP_SLOT_WRITE: About to dump pass_args Vaslist\n");
        DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*pass_args));
        T_sp tinstance = pass_args->next_arg();
        DTILOG("Got tinstance@%p %s\n" , (void*)tinstance.raw_() , _safe_rep_(tinstance));
        Instance_sp instance((gc::Tagged)tinstance.raw_());
        instance->instanceSet(index,value);
        return gctools::return_type(value.raw_(),1);
      }
    case MAYBE_LONG_ADD+DTREE_OP_RPLACA:
      {
        DTILOG("class cell\n");
        T_sp location = ReadArg<MAYBE_LONG_MUL>::read_literal(ip,(DTREE_SLOT_WRITER_INDEX_OFFSET),literals);
        size_t index = location.unsafe_fixnum();
        Cons_sp cell = gc::As_unsafe<Cons_sp>(location);
        DTILOG("DTREE_OP_RPLACA: About to dump pass_args Vaslist\n");
        DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*pass_args));
        T_sp value((gc::Tagged)pass_args->next_arg());
        DTILOG("Got value@%p %s\n" , (void*)value.raw_() , _safe_rep_(value));
        cell->rplaca(value);
        return gctools::return_type(value.raw_(),1);
      }
    case MAYBE_LONG_ADD+DTREE_OP_EFFECTIVE_METHOD:
      {
        DTILOG("effective method call\n");
        T_sp tfunc = ReadArg<MAYBE_LONG_MUL>::read_literal(ip,(DTREE_EFFECTIVE_METHOD_OFFSET),literals);
        Function_sp func = gc::As_unsafe<Function_sp>(tfunc);
        // Use the pass_args here because it points to the original arguments
        DTILOG("DTREE_OP_EFFECTIVE_METHOD: About to dump pass_args Vaslist\n");
        DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*pass_args));
        DTILOG(">>>>>>> DTREE_OP_EFFECTIVE_METHOD: Invoking effective method\n");
        return func->entry()(func.raw_(), pass_args->nargs(), pass_args->args());
      }
#if (ENABLE_REGISTER>=0)
    case MAYBE_LONG_ADD+DTREE_OP_REGISTER0: {
      arg = lcc_reg0;
      ++ip;
      DTILOG("Got arg@%p %s new ip %p\n" , (void*)arg.raw_() , _safe_rep_(arg), (void*)ip);
    }
        break;
#if (ENABLE_REGISTER>=1)
    case MAYBE_LONG_ADD+DTREE_OP_REGISTER1: {
      arg = lcc_reg1;
      ++ip;
      DTILOG("Got arg@%p %s new ip %p\n" , (void*)arg.raw_() , _safe_rep_(arg), (void*)ip);
    }
        break;
#if (ENABLE_REGISTER>=2)
    case MAYBE_LONG_ADD+DTREE_OP_REGISTER2: {
      arg = lcc_reg2;
      ++ip;
      DTILOG("Got arg@%p %s new ip %p\n" , (void*)arg.raw_() , _safe_rep_(arg), (void*)ip);
    }
        break;
#if (ENABLE_REGISTER>=3)
    case MAYBE_LONG_ADD+DTREE_OP_REGISTER3: {
      arg = lcc_reg3;
      ++ip;
      DTILOG("Got arg@%p %s new ip %p\n" , (void*)arg.raw_() , _safe_rep_(arg), (void*)ip);
    }
        break;
#if (ENABLE_REGISTER>=4)
    case MAYBE_LONG_ADD+DTREE_OP_REGISTER4: {
      arg = lcc_reg4;
      ++ip;
      DTILOG("Got arg@%p %s new ip %p\n" , (void*)arg.raw_() , _safe_rep_(arg), (void*)ip);
    }
        break;
#if (ENABLE_REGISTER>=5)
    case MAYBE_LONG_ADD+DTREE_OP_REGISTER5: {
      arg = lcc_reg5;
      ++ip;
      DTILOG("Got arg@%p %s new ip %p\n" , (void*)arg.raw_() , _safe_rep_(arg), (void*)ip);
    }
        break;
#if (ENABLE_REGISTER>=6)
    case MAYBE_LONG_ADD+DTREE_OP_REGISTER6: {
      arg = lcc_reg6;
      ++ip;
      DTILOG("Got arg@%p %s new ip %p\n" , (void*)arg.raw_() , _safe_rep_(arg), (void*)ip);
    }
        break;
#if (ENABLE_REGISTER>=7)
    case MAYBE_LONG_ADD+DTREE_OP_REGISTER7: {
      arg = lcc_reg7;
      ++ip;
      DTILOG("Got arg@%p %s new ip %p\n" , (void*)arg.raw_() , _safe_rep_(arg), (void*)ip);
    }
        break;
#endif // ENABLE_REGISTER >= 7
#endif // ENABLE_REGISTER >= 6
#endif // ENABLE_REGISTER >= 5
#endif // ENABLE_REGISTER >= 4
#endif // ENABLE_REGISTER >= 3
#endif // ENABLE_REGISTER >= 2
#endif // ENABLE_REGISTER >= 1
#endif // ENABLE_REGISTER >= 0
