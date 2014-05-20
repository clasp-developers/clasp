#ifndef clbind_policies_H
#define clbind_policies_H

//----------------------------------------------------------------------
//
// Policies
//

namespace clbind {

    template <typename... POLS>
    struct policies {};


    const int result = 32767;
    const int this_ = 32768;

    // Declares that a parameter is to be added to the multiple-value-return
    // This should only be used for pass-by-reference parameters that the
    // function modifies
    // The first argument is <1>
    template <int N>
    struct outValue {};

    // Declare that a (pass-by-reference) parameter is a pure out value and
    // should not be passed in from Lisp
    // For functions the first argument is <1>

    template <int N>
    struct pureOutValue {};

    template <int N>
    struct adopt;



    template <typename Policies>
    struct is_policy_list {
        typedef boost::mpl::false_ type;
    };

    template <typename... Pols>
    struct is_policy_list <policies<Pols...> > {
        typedef boost::mpl::true_ type;
    };

};
#endif
