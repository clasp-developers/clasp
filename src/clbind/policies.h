/*
    File: policies.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
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
