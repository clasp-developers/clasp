/*
    File: myReadLine.cc
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
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/myReadLine.h>

#ifdef READLINE
extern "C" char *readline(const char *prompt);
extern "C" void add_history(char *line);
#endif

namespace core {

string myReadLine(const string &prompt, bool &end_of_transmission) {
  end_of_transmission = false;
  string res;
#ifdef READLINE
  char *line_read;
  /* Get a line from the user. */
  //      lisp->print(BF("%s")%prompt);
  stringstream ss;
  ss << std::endl
     << prompt;
  line_read = ::readline(ss.str().c_str()); // prompt.c_str());
  if (line_read != NULL) {
    if (*line_read)
      ::add_history(line_read);
    res = line_read;
    free(line_read);
  } else {
    end_of_transmission = true;
  }
#else
  if (prompt != "") {
    _lisp->print(BF("%s ") % prompt);
  }
  getline(std::cin, res);
#endif
  return res;
}
};
