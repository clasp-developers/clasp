
namespace translate {
/*! Translate a Common Lisp VECTOR or CONS containing integers 
      (must be within the domain of C++ int(s)
    */
template <>
struct from_object<const vector<int> &> {
  typedef vector<int> DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o.nilp()) {
      _v.clear();
      return;
    } else if (core::Vector_sp vo = o.asOrNull<core::Vector_O>()) {
      _v.resize(vo->length());
      for (cl_index i(0), iEnd(vo->length()); i < iEnd; ++i) {
        core::Integer_sp io = vo->elt(i).as<core::Integer_O>();
        _v[i] = io->as_int();
      }
      return;
    } else if (core::Cons_sp lo = o.asOrNull<core::Cons_O>()) {
      _v.resize(lo->length());
      int i = 0;
      for (auto cur : lo) {
        core::Integer_sp io = oCar(cur).as<core::Integer_O>();
        _v[i] = io->as_int();
        ++i;
      }
      return;
    }
    SIMPLE_ERROR_SPRINTF("Add support to convert other types to vector<int>");
  }
};

template <>
struct to_object<std::vector<int>> {
  static core::T_sp convert(std::vector<int> x) {
    core::VectorObjects_sp vo = core::VectorObjects_O::create(_Nil<core::T_O>(), x.size(), cl::_sym_T_O);
    int i(0);
    for (auto ai : x) {
      vo->setf_elt(i++, core::Integer_O::create(ai));
    }
    return vo;
  }
};
};
