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
        return (dynamic_cast<ToType>(client) != NULL);
//        printf("%s:%d Add support for Cast::isA to type %s from type %s\n", __FILE__, __LINE__, typeid(ToType).name(), typeid(FromType).name());
//        return false;
    //return (dynamic_cast<ToType>(client) != NULL);
      }
    };
};

// A few classes need to be forward declared for clasp_gc.cc

namespace gctools {
  template <class T>
    class GCArray_moveable;
  template <class T>
    class GCArray_atomic;
  template <class T>
    class GCArraySignedLength_moveable;
  template <class T>
    class GCVector_moveable;
  template <class T>
    class Vec0;
  template <int N, int SignedP>
    class GCBitUnitArray_moveable;
  template <class K>
    class SmallOrderedSet;
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

#if ((defined(USE_MMTK)||defined(USE_BOEHM)) && !defined(USE_PRECISE_GC)) || defined(RUNNING_PRECISEPREP)
//----------------------------------------------------------------------
# ifndef SCRAPING
#  define DECLARE_FORWARDS
#  include INIT_CLASSES_INC_H // REPLACED CLASP_GC_CC // "main/clasp_gc.cc"
#  undef DECLARE_FORWARDS
# endif
namespace cast {
#ifndef SCRAPING
# define GC_DYNAMIC_CAST
# include INIT_CLASSES_INC_H // REPLACED CLASP_GC_CC // "main/clasp_gc.cc"
# undef GC_DYNAMIC_CAST
#endif
};
//----------------------------------------------------------------------
#endif // #ifdef USE_BOEHM

#if !defined(SCRAPING)
 #if defined(USE_PRECISE_GC)
//----------------------------------------------------------------------
  #define DECLARE_FORWARDS
   #include CLASP_GC_CC
  #undef DECLARE_FORWARDS
namespace cast {
  #define GC_DYNAMIC_CAST
   #include CLASP_GC_CC
  #undef GC_DYNAMIC_CAST
};
 #endif // #if defined(USE_PRECISE_GC)
#endif // #if !defined(SCRAPING)



// Cast assumes that the client pointer is untagged already
#if !defined(USE_PRECISE_GC)
namespace gctools {
    template <typename TOPTR>
    struct FromGeneralCast {
    typedef TOPTR ToType;
    inline static bool isA(core::General_O* client) {
      return cast::Cast<ToType,core::General_O*>::isA(client);
    }
  };
};
#endif // !USE_PRECISE_GC




#if defined(USE_PRECISE_GC)
namespace gctools {
  template <typename TOPTR>
    struct FromGeneralCast {
      typedef TOPTR ToType;
      inline static bool isA(core::General_O* client) {
        return cast::Cast<ToType,core::General_O*>::isA(client);
      }
    };

};
#endif // USE_PRECISE_GC



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
