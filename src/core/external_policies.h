/*
    File: external_policies.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
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
//----------------------------------------------------------------------
//
// Policies
//

namespace policies {

    template <typename... POLS>
    struct policies_ {};


    const int _return = 32767;
    const int _this = 32768;

    // Declares that a parameter is to be added to the multiple-value-return
    // This should only be used for pass-by-reference parameters that the
    // function modifies
    template <int N>
    struct outValue_ {};

    // Declare that a (pass-by-reference) parameter is a pure out value and
    // should not be passed in from Lisp
    template <int N>
    struct pureOutValue_ {};

    template <int N>
    struct adopt_;




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


    template<typename Cond, typename ARG>
    void ReturnValueWhen(core::MultipleValues& mv, int& idx, Cond, ARG&& a)
    {
        mv.emplace_back(translate::to_object<ARG>::convert(a));
        ++idx;
//        mv.valueSet(idx++,translate::to_object<ARG>::convert(a));
    }

    template<typename ARG>
    void ReturnValueWhen(core::MultipleValues& mv, int& idx, std::false_type, ARG&& a)
    {
        // Do nothing
    }


    namespace detail {
        template <int>
        struct Int2Type {};


        template <typename POL, typename T> 
        struct IndexOf;

        template <typename T>
        struct IndexOf< policies_<>, T>
        {
            enum { value = -1 };
        };

        template <typename Head, typename... Tail>
        struct IndexOf< policies_<Head,Tail...>, Head>
        {
            enum { value = 0 };
        };

        template <typename Head, typename...Tail, typename T>
        struct IndexOf< policies_<Head,Tail...>, T> 
        {
        private:
            enum { temp = IndexOf<policies_<Tail...>,T>::value };
        public:
            enum { value = temp == -1 ? -1 : 1 + temp };
        };
    };
    template <typename Pol, typename T>
    using IndexOf = typename detail::IndexOf<Pol,T>;






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







    namespace detail {
        template <typename POL> 
        struct CountPureOutValues;

        template <>
        struct CountPureOutValues< policies_<> >
        {
            enum { value = 0 };
        };

        template <typename... Tail, int N>
        struct CountPureOutValues< policies_<pureOutValue_<N>,Tail...> >
        {
            enum { value = CountPureOutValues<policies_<Tail...> >::value + 1 };
        };

        template <typename Head, typename... Tail>
        struct CountPureOutValues< policies_<Head,Tail...> >
        {
            enum { value = CountPureOutValues<policies_<Tail...> >::value };
        };

    };
    template <typename Policies>
    using CountPureOutValues = typename detail::CountPureOutValues<Policies>;





};
