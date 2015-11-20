/*
    File: wrapfun.cc
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
#include <functional>

typedef void *(*funcT)(void *arg);

void *call_with_alloc_lock(funcT fn, void *data) {
  printf("call_with_alloc_lock LOCKED\n");
  fn(data);
  printf("call_with_alloc_lock UNLOCKED\n");
  return NULL;
}

template <class Proto>
void *wrapRun(void *wrappedFn) {
  std::function<Proto> *fn = reinterpret_cast<std::function<Proto> *>(wrappedFn);
  (*fn)();
  return NULL;
}

template <class Proto>
void safeRun(std::function<Proto> f) {
  printf("Entered safeRun\n");
  call_with_alloc_lock(wrapRun<Proto>, reinterpret_cast<void *>(&f));
  printf("Leaving safeRun\n");
};

struct A {
  A(int z) : val(z){};
  int val;
  int foo(int x, int y) {
    int result;
    safeRun<void()>([&result, x, y, this]() -> void {
                printf("In lambda for foo\n");
                result = x + y + val;
    });
    return result;
  };

  int bar(int x, int y, int z) {
    int result;
    safeRun<void()>([&result, x, y, z, this]() -> void {
                printf("In lambda for bar\n");
                result = x + y + z + val;
    });
    return result;
  };
};

int main(int argc, const char *argv[]) {
  A a(3);
  printf("Calling foo %d\n", a.foo(1, 2));
  printf("Calling bar %d\n", a.bar(1, 2, 3));
  return 0;
}
