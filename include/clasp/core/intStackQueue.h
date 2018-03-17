/*
    File: intStackQueue.h
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
#ifndef IntStackQueue_H
#define IntStackQueue_H

#include <clasp/core/exceptions.h>

#define INT_QUEUE_SIZE 256
#define INT_STACK_SIZE 256

namespace core {
class IntQueue {
protected:
  int _Queue[INT_QUEUE_SIZE];
  uint _QueueFront;
  uint _QueueBack;

protected:
  void advanceQueueIndex(uint &idx);

public:
  IntQueue() {
    this->_QueueFront = 0;
    this->_QueueBack = 0;
  };
  virtual ~IntQueue(){};

  bool empty() { return this->_QueueFront == this->_QueueBack; };
  bool notEmpty() { return this->_QueueFront != this->_QueueBack; };
  void reset();
  int front() {
    HARD_ASSERT(this->notEmpty());
    return this->_Queue[this->_QueueFront];
  };
  int dequeue();
  int enqueue(int v);
  int enqueue(int val, uint count);
  uint size() { return (this->_QueueBack - this->_QueueFront); };
};

inline void IntQueue::reset() {
  this->_QueueBack = 0;
  this->_QueueFront = 0;
}

inline void IntQueue::advanceQueueIndex(uint &idx) {
  idx++;
  if (idx >= INT_QUEUE_SIZE)
    idx = 0;
}

inline int IntQueue::dequeue() {
  _OF();
  ASSERT(!this->empty());
  int ret = this->_Queue[this->_QueueFront];
  this->advanceQueueIndex(this->_QueueFront);
  return ret;
};

inline int IntQueue::enqueue(int val) {
  this->_Queue[this->_QueueBack] = val;
  this->advanceQueueIndex(this->_QueueBack);
  return val;
};

inline int IntQueue::enqueue(int val, uint count) {
  for (uint zz = 0; zz < count; zz++) {
    this->_Queue[this->_QueueBack] = val;
    this->advanceQueueIndex(this->_QueueBack);
  };
  return val;
}

class IntStack {
private:
  int _Stack[INT_STACK_SIZE];
  uint _Next;

public:
  IntStack() { this->_Next = 0; };
  virtual ~IntStack(){};
  void reset();
  int size() { return this->_Next; };
  bool empty() { return this->_Next == 0; };
  bool notEmpty() { return this->_Next != 0; };
  string asString();
  int top();
  int pop();
  void push(int val);
};

inline void IntStack::reset() {
  this->_Next = 0;
}

inline int IntStack::pop() {
  _OF();
  ASSERT(this->notEmpty());
  return this->_Stack[--this->_Next];
}

inline void IntStack::push(int val) {
  _OF();
  ASSERT(this->_Next < INT_STACK_SIZE);
  this->_Stack[this->_Next++] = val;
};

inline int IntStack::top() {
  _OF();
  ASSERT(this->notEmpty());
  return this->_Stack[this->_Next - 1];
}

inline string IntStack::asString() {
  stringstream ss;
  for (uint i = 0; i < this->_Next; i++) {
    ss << this->_Stack[i] << " ";
  }
  return ss.str();
}
};

#endif
