/*
    File: testTemplate.cc
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
#include <stdio.h>




template <typename 



template <int X, typename P, typename TUPLE, class... ARGS>
void select(ARGS...args)
{
    select<X+1,Y,TUPLE,ARGS...>(args...);
}


template <unsigned X, int X, typename TUPLE, class HEAD, class... ARGS>
void select(HEAD head, ARGS...args)
{
    printf("Selected %d\n", head);
    select<X+1,TUPLE::Head, TUPLE::Tail, ARGS...>(args...);
}


int main(int argc, char* argv[])
{
    select<1,1,mv_<1,3,5,9> >(1,2,3,4,5,6,7,8,9,10,11);
}
