namespace gctools {
    template <typename TOPTR, typename FROMPTR>
    struct TaggedCast {
	typedef TOPTR   ToType;
	typedef FROMPTR FromType;
        static bool isA(FromType client) {
	    if ( tagged_otherp(client) ) {
		return (dynamic_cast<ToType>(untag_other(client))!=NULL);
	    } else if ( tagged_consp(client) ) {
		return (dynamic_cast<ToType>(untag_cons(client))!=NULL);
	    }
	    return false;//THROW_HARD_ERROR(BF("An immediate should never be isA tested by this function - it should have a specialized version")); // Must be specialized
        }
        static ToType castOrNULL(FromType client) {
	    if ( tagged_otherp(client) ) {
		ToType ptr = dynamic_cast<ToType>(untag_other(client));
		if (ptr) return tag_other<ToType>(ptr);
		return NULL;
	    } else if ( tagged_consp(client) ) {
		ToType ptr = dynamic_cast<ToType>(untag_cons(client));
		if (ptr) return tag_cons<ToType>(ptr);
		return NULL;
	    }
	    return NULL; // handle with specializations
        }
    };
};



namespace core {
    class Fixnum_I {};
    class Integer_O;
    class Rational_O;
    class Real_O;
    class Number_O;
    class T_O;
     typedef Fixnum_I Fixnum_O;
};

////////////////////////////////////////////////////////////////////////
//
// Downcast from a supertype to a subtype
//
// Every possible downcast needs an "As" template function
namespace gctools {
    template <> struct TaggedCast<core::Fixnum_I*,core::Fixnum_I*> {
	typedef core::Fixnum_I* ToType;
	typedef core::Fixnum_I* FromType;
	static bool isA(FromType ptr) { return true;}
	static ToType castOrNULL(FromType client) { return client;}
    };
    template <typename FROM> struct TaggedCast<core::Fixnum_I*,FROM> {
	typedef core::Fixnum_I* ToType;
	typedef FROM FromType;
	static bool isA(FromType ptr) {
	    return tagged_fixnump(ptr);
	}
	static ToType castOrNULL(FromType client) { return reinterpret_cast<ToType>(client);}
    };

    template <> struct TaggedCast<core::Integer_O*,core::Integer_O*> {
	typedef core::Integer_O* ToType;
	typedef core::Integer_O* FromType;
	static bool isA(FromType ptr) { return true;}
	static ToType castOrNULL(FromType client) { return client;}
    };
    template <> struct TaggedCast<core::Integer_O*,core::Fixnum_I*> {
	typedef core::Integer_O* ToType;
	typedef core::Fixnum_I* FromType;
	static bool isA(FromType ptr) {return true;};
	static ToType castOrNULL(FromType client) { return reinterpret_cast<ToType>(client);}
    };
    template <typename FROM> struct TaggedCast<core::Integer_O*,FROM> {
	typedef core::Integer_O* ToType;
	typedef FROM FromType;
	static bool isA(FromType ptr) {
	    return tagged_fixnump(ptr) 
		|| (tagged_otherp(ptr) && (dynamic_cast<ToType>(untag_other(ptr))!=NULL));
	}
	static ToType castOrNULL(FromType client) { return reinterpret_cast<ToType>(client);}
    };

    template <> struct TaggedCast<core::Rational_O*,core::Rational_O*> {
	typedef core::Rational_O* ToType;
	typedef core::Rational_O* FromType;
	static bool isA(FromType ptr) { return true;}
	static ToType castOrNULL(FromType client) { return client;}
    };
    template <typename FROM> struct TaggedCast<core::Rational_O*,FROM> {
	typedef core::Rational_O* ToType;
	typedef FROM FromType;
	static bool isA(FromType ptr) {
	    return tagged_fixnump(ptr) 
		|| (tagged_otherp(ptr) && (dynamic_cast<ToType>(untag_other(ptr))!=NULL));
	}
	static ToType castOrNULL(FromType client) { return reinterpret_cast<ToType>(client);}
    };


    template <> struct TaggedCast<core::Real_O*,core::Real_O*> {
	typedef core::Real_O* ToType;
	typedef core::Real_O* FromType;
	static bool isA(FromType ptr) { return true;}
	static ToType castOrNULL(FromType client) { return client;}
    };
    template <> struct TaggedCast<core::Real_O*,core::Fixnum_I*> {
	typedef core::Real_O* ToType;
	typedef core::Fixnum_I* FromType;
	static bool isA(FromType ptr) {return true;}
	static ToType castOrNULL(FromType client) { return reinterpret_cast<ToType>(client);}
    };
    template <typename FROM> struct TaggedCast<core::Real_O*,FROM> {
	typedef core::Real_O* ToType;
	typedef FROM FromType;
	static bool isA(FromType ptr) {
	    return tagged_fixnump(ptr) 
		|| tagged_single_floatp(ptr) 
		|| (tagged_otherp(ptr) && (dynamic_cast<ToType>(untag_other(ptr))!=NULL));
	}
	static ToType castOrNULL(FromType client) { return reinterpret_cast<ToType>(client);}
    };

    template <> struct TaggedCast<core::Number_O*,core::Number_O*> {
	typedef core::Number_O* ToType;
	typedef core::Number_O* FromType;
	static bool isA(FromType ptr) { return true;}
	static ToType castOrNULL(FromType client) { return client;}
    };
    template <> struct TaggedCast<core::Number_O*,core::Fixnum_I*> {
	typedef core::Number_O* ToType;
	typedef core::Fixnum_I* FromType;
	static bool isA(FromType ptr) {return true;}
	static ToType castOrNULL(FromType client) { return reinterpret_cast<ToType>(client);}
    };
    template <typename FROM> struct TaggedCast<core::Number_O*,FROM> {
	typedef core::Number_O* ToType;
	typedef FROM FromType;
	static bool isA(FromType ptr) {
	    return tagged_fixnump(ptr) 
		|| tagged_single_floatp(ptr) 
		|| (tagged_otherp(ptr) && (dynamic_cast<ToType>(untag_other(ptr))!=NULL));
	}
	static ToType castOrNULL(FromType client) { return reinterpret_cast<ToType>(client);}
    };


    template <> struct TaggedCast<core::T_O*,core::T_O*> {
	typedef core::T_O* ToType;
	typedef core::T_O* FromType;
	static bool isA(FromType ptr) { return true;}
	static ToType castOrNULL(FromType client) { return client;}
    };
    template <> struct TaggedCast<core::T_O*,core::Fixnum_I*> {
	typedef core::T_O* ToType;
	typedef core::Fixnum_I* FromType;
	static bool isA(FromType ptr) {return true;}
	static ToType castOrNULL(FromType client) { return reinterpret_cast<ToType>(client);}
    };
    template <typename FROM> struct TaggedCast<core::T_O*,FROM> {
	typedef core::T_O* ToType;
	typedef FROM FromType;
	static bool isA(FromType ptr) {return true;};
	static ToType castOrNULL(FromType client) { return reinterpret_cast<ToType>(client);}
    };


};


#if 0
/// Do I need the following?

namespace gctools {
    ////////////////////////////////////////////////////////////////////////
    // Cast to Integer_O* from Fixnum_I*
    template <>
	struct TaggedCast<core::Integer_O*,core::Fixnum_I*> {
	typedef core::Integer_O* ToType;
	typedef core::Fixnum_I* FromType;
        static bool isA(FromType ptr) {
            return (tagged_fixnump<FromType>(ptr));
        }
        static ToType castOrNULL(FromType client) {
	    if ( TaggedCast<ToType,FromType>::isA(client) ) return reinterpret_cast<ToType>(client);
	    return NULL;
        }
    };
    ////////////////////////////////////////////////////////////////////////
    // Cast to Rational_O* from Fixnum_I*
    template <>
	struct TaggedCast<core::Rational_O*,core::Fixnum_I*> {
	typedef core::Rational_O* ToType;
	typedef core::Fixnum_I* FromType;
        static bool isA(FromType ptr) {
            return (tagged_fixnump<FromType>(ptr));
        }
        static ToType castOrNULL(FromType client) {
	    if ( TaggedCast<ToType,FromType>::isA(client) ) return reinterpret_cast<ToType>(client);
	    return NULL;
        }
    };
    ////////////////////////////////////////////////////////////////////////
    // Cast to Real_O* from Fixnum_I*
    template <>
	struct TaggedCast<core::Real_O*,core::Fixnum_I*> {
	typedef core::Real_O* ToType;
	typedef core::Fixnum_I* FromType;
        static bool isA(FromType ptr) {
            return (tagged_fixnump<FromType>(ptr));
        }
        static ToType castOrNULL(FromType client) {
	    if ( TaggedCast<ToType,FromType>::isA(client) ) return reinterpret_cast<ToType>(client);
	    return NULL;
        }
    };
    ////////////////////////////////////////////////////////////////////////
    // Cast to Number_O* from Fixnum_I*
    template <>
	struct TaggedCast<core::Number_O*,core::Fixnum_I*> {
	typedef core::Number_O* ToType;
	typedef core::Fixnum_I* FromType;
        static bool isA(FromType ptr) {
            return (tagged_fixnump<FromType>(ptr));
        }
        static ToType castOrNULL(FromType client) {
	    if ( TaggedCast<ToType,FromType>::isA(client) ) return reinterpret_cast<ToType>(client);
	    return NULL;
        }
    };
    ////////////////////////////////////////////////////////////////////////
    // Cast to T_O* from Fixnum_I*
    template <>
	struct TaggedCast<core::T_O*,core::Fixnum_I*> {
	typedef core::T_O* ToType;
	typedef core::Fixnum_I* FromType;
        static bool isA(FromType ptr) {
            return (tagged_fixnump<FromType>(ptr));
        }
        static ToType castOrNULL(FromType client) {
	    if ( TaggedCast<ToType,FromType>::isA(client) ) return reinterpret_cast<ToType>(client);
	    return NULL;
        }
    };
};

#endif
