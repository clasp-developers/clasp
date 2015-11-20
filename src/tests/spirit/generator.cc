/*
    File: generator.cc
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
/*=============================================================================
    Copyright (c) 2002-2010 Hartmut Kaiser
    Copyright (c) 2002-2010 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
///////////////////////////////////////////////////////////////////////////////
//
//  This sample demonstrates a generator for a comma separated list of numbers.
//  No actions. It is based on the example qi/num_lists.cpp for reading in
//  some numbers to generate.
//
///////////////////////////////////////////////////////////////////////////////

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/karma.hpp>

#include <iostream>
#include <string>
#include <list>
#include <boost/array.hpp>

namespace qi = boost::spirit::qi;
namespace karma = boost::spirit::karma;
namespace ascii = boost::spirit::ascii;
namespace client {

///////////////////////////////////////////////////////////////////////////
//  Our number list parser, please see the example qi/numlist1.cpp for
//  more information
///////////////////////////////////////////////////////////////////////////
template <typename Iterator>
bool parse_numbers(Iterator first, Iterator last, std::list<double> &v) {
  using qi::double_;
  using qi::phrase_parse;
  using ascii::space;

  bool r = phrase_parse(first, last, *double_, space, v);
  if (first != last)
    return false;
  return r;
}

///////////////////////////////////////////////////////////////////////////
//  Our number list generator
///////////////////////////////////////////////////////////////////////////
//[tutorial_karma_numlist1
template <typename OutputIterator>
bool generate_numbers(OutputIterator &sink, std::list<double> const &v) {
  using karma::double_;
  using karma::generate_delimited;
  using ascii::space;

  bool r = generate_delimited(
      sink,                         // destination: output iterator
      double_ << *(',' << double_), // the generator
      space,                        // the delimiter-generator
      v                             // the data to output
      );
  return r;
}
//]
}

////////////////////////////////////////////////////////////////////////////
//  Main program
////////////////////////////////////////////////////////////////////////////
int main() {
  printf("Size of karma::double_ = %ld\n", sizeof(karma::double_));
  printf("typeid().name of karma::double_ = %s\n", typeid(karma::double_).name());
  std::cout << "/////////////////////////////////////////////////////////\n\n";
  std::cout << "\t\tA comma separated list generator for Spirit...\n\n";
  std::cout << "/////////////////////////////////////////////////////////\n\n";

  std::cout << "Give me a comma separated list of numbers.\n";
  std::cout << "Type [q or Q] to quit\n\n";

  std::string str;
  while (getline(std::cin, str)) {
    if (str.empty() || str[0] == 'q' || str[0] == 'Q')
      break;

    std::list<double> v; // here we put the data to generate
    if (client::parse_numbers(str.begin(), str.end(), v)) {
      // ok, we got some numbers, now print them back out
      std::cout << "-------------------------\n";

      std::string generated;
      std::back_insert_iterator<std::string> sink(generated);
      if (!client::generate_numbers(sink, v)) {
        std::cout << "-------------------------\n";
        std::cout << "Generating failed\n";
        std::cout << "-------------------------\n";
      } else {
        std::cout << "-------------------------\n";
        std::cout << "Generated: " << generated << "\n";
        std::cout << "-------------------------\n";
      }
    } else {
      std::cout << "-------------------------\n";
      std::cout << "Parsing failed\n";
      std::cout << "-------------------------\n";
    }
  }

  std::cout << "Bye... :-) \n\n";
  return 0;
}
