#ifndef gctools_cast_H
#define gctools_cast_H



namespace cast {
  template <typename TOPTR, typename FROMPTR>
    struct Cast {
      typedef TOPTR ToType;
      typedef FROMPTR FromType;
  // Very few Cast's should default back to this one.
  // Maybe keep a count of how often it gets called?
      inline static bool isA(FromType client) {
        printf("%s:%d Add support for Cast::isA for this type\n", __FILE__, __LINE__);
        return false;
    //return (dynamic_cast<ToType>(client) != NULL);
      }
    };
};

  namespace core {
    class T_O;
    class Sequence_V;
    class List_V;
    class WrappedPointer_O;
    class Function_O;
    class Creator_O;
    class Iterator_O;
  };
  namespace clbind {
    class ConstructorCreator_O;
  };

#ifdef USE_BOEHM
//----------------------------------------------------------------------
#ifndef USE_CXX_DYNAMIC_CAST
#define DECLARE_FORWARDS
#include "clasp_gc.cc"
#undef DECLARE_FORWARDS
#endif
namespace cast {
#ifndef USE_CXX_DYNAMIC_CAST
#define GC_DYNAMIC_CAST
#include "clasp_gc.cc" // "main/clasp_gc.cc"
#undef GC_DYNAMIC_CAST
#endif
};
//----------------------------------------------------------------------
#endif // #ifdef USE_BOEHM


#ifdef USE_MPS
//----------------------------------------------------------------------
#ifndef RUNNING_GC_BUILDER
#define DECLARE_FORWARDS
#include "clasp_gc.cc"
#undef DECLARE_FORWARDS
#endif
namespace cast {
#if !defined(RUNNING_GC_BUILDER)
#define GC_DYNAMIC_CAST
#include "clasp_gc.cc" // "main/clasp_gc.cc"
#undef GC_DYNAMIC_CAST
#endif
};
#endif // #ifdef USE_MPS



// Cast assumes that the client pointer is untagged already
#ifdef USE_BOEHM
#ifdef USE_CXX_DYNAMIC_CAST
namespace gctools {
    template <typename TOPTR>
    struct FromGeneralCast {
    typedef TOPTR ToType;
    inline static bool isA(core::General_O* client) {
      return (dynamic_cast<ToType>(client) != NULL);
    }
  };
};

#else
namespace gctools {
    template <typename TOPTR>
    struct FromGeneralCast {
    typedef TOPTR ToType;
    inline static bool isA(core::General_O* client) {
      return cast::Cast<ToType,core::General_O*>::isA(client);
    }
  };
};
#endif // USE_CXX_DYNAMIC_CAST
#endif // USE_BOEHM




#ifdef USE_MPS
namespace gctools {
  template <typename TOPTR>
    struct FromGeneralCast {
      typedef TOPTR ToType;
      inline static bool isA(core::General_O* client) {
        return cast::Cast<ToType,core::General_O*>::isA(client);
      }
    };

};
#endif // USE_MPS



namespace gctools {
  template <>
    struct FromGeneralCast<core::T_O*> {
    typedef core::T_O* ToType;
      inline static bool isA(core::General_O* client) {
        return true;
      }
    };
  template <>
    struct FromGeneralCast<core::Sequence_V*> {
    typedef core::Sequence_V* ToType;
      inline static bool isA(core::General_O* client) {
        return false;
      }
    };
  template <>
    struct FromGeneralCast<core::List_V*> {
    typedef core::List_V* ToType;
      inline static bool isA(core::General_O* client) {
        return false;
      }
    };
  template <>
    struct FromGeneralCast<core::Cons_O*> {
    typedef core::Cons_O* ToType;
      inline static bool isA(core::General_O* client) {
        return false;
      }
    };

};

namespace gctools {
  template <typename TOPTR>
    struct FromConsCast {
    typedef TOPTR ToType;
    inline static bool isA(core::Cons_O* client) {
      return false;
    }
  };
  template <>
    struct FromConsCast<core::T_O*> {
    typedef core::T_O* ToType;
    inline static bool isA(core::Cons_O* client) {
      return true;
    }
  };

  template <>
    struct FromConsCast<core::Sequence_V*> {
    typedef core::Sequence_V* ToType;
    inline static bool isA(core::Cons_O* client) {
      return true;
    }
  };

  template <>
    struct FromConsCast<core::List_V*> {
    typedef core::List_V* ToType;
    inline static bool isA(core::Cons_O* client) {
      return true;
    }
  };

  template <>
    struct FromConsCast<core::Cons_O*> {
    typedef core::Cons_O* ToType;
    inline static bool isA(core::Cons_O* client) {
      return true;
    }
  };

};



#endif // #ifndef gctools_cast_H
