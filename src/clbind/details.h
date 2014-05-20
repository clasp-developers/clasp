#ifndef clbind_details_H
#define clbind_details_H

#include "policies.h"


namespace clbind {


    namespace detail {



        template <int>
        struct Int2Type {};


        template <typename POL, typename T> 
        struct IndexOf;

        template <typename T>
        struct IndexOf< policies<>, T>
        {
            enum { value = -1 };
        };

        template <typename Head, typename... Tail>
        struct IndexOf< policies<Head,Tail...>, Head>
        {
            enum { value = 0 };
        };

        template <typename Head, typename...Tail, typename T>
        struct IndexOf< policies<Head,Tail...>, T> 
        {
        private:
            enum { temp = IndexOf<policies<Tail...>,T>::value };
        public:
            enum { value = temp == -1 ? -1 : 1 + temp };
        };
    };
    template <typename Pol, typename T>
    using IndexOf = typename detail::IndexOf<Pol,T>;





    template<typename Cond>
    struct IncWhen {
        static void go(ArgArray& args)
        {
            args++;
        }
    };

    template <>
    struct IncWhen<std::false_type> {
        static void go(ArgArray& args)
        {
            // Do nothing
        }
    };


    template<typename OutValue, typename Adopt, typename ARG>
    void ReturnValueWhen(core::MultipleValues& mv, int& idx, OutValue, Adopt, ARG&& a)
    {
        mv.valueSet(idx++,translate::to_object<ARG,Adopt>::convert(a));
    }

    template<typename Adopt, typename ARG>
    void ReturnValueWhen(core::MultipleValues& mv, int& idx, std::false_type, Adopt, ARG&& a)
    {
        // Do nothing
    }




    template<bool B, typename X, typename Y>
    struct if_c { typedef X type; };
    template<typename X, typename Y>
    struct if_c<false, X, Y> { typedef Y type; };


    template<typename Set, typename T>
    struct Contains_ {
        typedef typename if_c<(IndexOf<Set, T>::value >= 0),
            std::true_type,
            std::false_type>::type type;
    };

    template<typename Set, typename T>
    struct DoesNotContain_ {
        typedef typename if_c<(IndexOf<Set, T>::value >= 0),
            std::false_type,
            std::true_type>::type type;
    };







    template <typename... Tail>
    struct or_ {
        typedef std::true_type   type;
    };

    template <typename...Tail>
    struct or_<std::false_type,Tail...>
    {
        typedef typename or_<Tail...>::type   type;
    };

    template <>
    struct or_<>
    {
        typedef std::false_type  type;
    };






    template<typename Policies, int N>
    struct is_outValue {
        typedef typename or_<typename Contains_<Policies,pureOutValue<N> >::type
                             , typename Contains_<Policies,outValue<N> >::type>::type type;
    };


    template<typename Policies, int N>
    struct AdoptPointer {
        typedef typename boost::mpl::if_<typename Contains_<Policies,adopt<N> >::type
                                         , translate::adopt_pointer
                                         , translate::dont_adopt_pointer >::type type;
    };





//
// Counts instances of pureOutValue<N> where N is an int in the list of policies<...>
//
    namespace detail {
        template <typename POL> 
        struct CountPureOutValues;

        template <>
        struct CountPureOutValues< policies<> >
        {
            enum { value = 0 };
        };

        template <typename... Tail, int N>
        struct CountPureOutValues< policies<pureOutValue<N>,Tail...> >
        {
            enum { value = CountPureOutValues<policies<Tail...> >::value + 1 };
        };

        template <typename Head, typename... Tail>
        struct CountPureOutValues< policies<Head,Tail...> >
        {
            enum { value = CountPureOutValues<policies<Tail...> >::value };
        };

    };
    template <typename Policies>
    using CountPureOutValues = typename detail::CountPureOutValues<Policies>;





};
#endif
