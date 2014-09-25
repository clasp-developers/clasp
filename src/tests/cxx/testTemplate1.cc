/*
    File: testTemplate1.cc
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
#include <iostream>

template <int...>
struct Indices
{};

template <int>
struct Int2Type
{};

template <typename F, typename Head, typename... Tail, int N, int... TailI>
void applyImpl(F f, Indices<N, TailI...>, Int2Type<N>, Head head, Tail... tail)
{
	std::cout << f(head) << '\n';
	applyImpl(f, Indices<TailI...>(), Int2Type<N + 1>(), tail...);
}

template <typename F, typename Head, typename... Tail, int HeadI, int... TailI, int N>
void applyImpl(F f, Indices<HeadI, TailI...>, Int2Type<N>, Head head, Tail... tail)
{
	std::cout << head << '\n';
	applyImpl(f, Indices<HeadI, TailI...>(), Int2Type<N + 1>(), tail...);
}

template <typename F, typename Head, typename... Tail, int N>
void applyImpl(F f, Indices<>, Int2Type<N>, Head head, Tail... tail)
{
	std::cout << head << '\n';
	applyImpl(f, Indices<>(), Int2Type<N>(), tail...);
}

template <typename F, int N>
void applyImpl(F f, Indices<>, Int2Type<N>)
{}

template <typename TT, typename F, typename... Args, int... I>
void apply(F f, Args... args)
{
	applyImpl(f, TT(), Int2Type<0>(), args...);
}

int times10(int x)
{
	return x * 10;
}

int main()
{
    apply<Indices<0,2> >(times10, 5, 6, 7, 8);
}
