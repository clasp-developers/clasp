/*
    File: lightProfiler.cc
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

#define TURN_DEBUG_OFF
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lightProfiler.h>
#include <sstream>

namespace core {

LightEventCounter::LightEventCounter() {
  this->_Calls = 0;
  this->_Problems = 0;
}

LightEventCounter::~LightEventCounter() {
}

void LightEventCounter::recordCallAndProblem(bool prob) {
  this->_Calls++;
  if (prob)
    this->_Problems++;
}

LightTimer::LightTimer(LightProfiler *profiler) : _Profiler(profiler), _Id(0), _IsOn(false), _AccumulatedTime(0.0), _Calls(0), _ClockResolutionFails(0), _Parent(UndefinedUnsignedInt), _Sibling(UndefinedUnsignedInt), _Child(UndefinedUnsignedInt), _StartTime(0.0) {
#ifdef DARWIN_CLOCK
  mach_timebase_info_data_t info;
  kern_return_t err = mach_timebase_info(&info);
  if (err == 0) {
    this->_DarwinConversion = 1.0e-9 * (double)info.numer / (double)info.denom;
  } else {
    THROW_HARD_ERROR(BF("Could not determine clock conversion"));
  }
#else
// #error "What initialization needs to be done for LightTimer on non DARWIN systems"
#endif
}

void LightTimer::addChild(uint childIndex) {
  HARD_ASSERT(this->_Profiler != NULL);
  LightTimer &child = this->_Profiler->timer(childIndex);
  child._Sibling = this->_Child;
  this->_Child = childIndex;
}

void LightTimer::start() {
  this->_IsOn = true;
  this->_Calls++;
#ifdef DARWIN_CLOCK
  this->_DarwinStartTime = mach_absolute_time();
#else
  this->_StartTime = clock();
#endif
}

void LightTimer::stop() {
  double cpu_time_used;
#ifdef DARWIN_CLOCK
  uint64_t difference;
#else
  clock_t end;
#endif
  if (!this->_IsOn) {
    THROW_HARD_ERROR(boost::format("Timer %s is not on") % this->_Description);
  }
  this->_IsOn = false;
#ifdef DARWIN_CLOCK
  difference = mach_absolute_time() - this->_DarwinStartTime;
  cpu_time_used = difference * this->_DarwinConversion;
#else
  end = clock();
  cpu_time_used = ((double)(end - this->_StartTime)) / CLOCKS_PER_SEC;
#endif
  if (cpu_time_used == 0.0) {
    this->_ClockResolutionFails++;
  }
  this->_AccumulatedTime += cpu_time_used;
}

uint LightProfiler::createEventCounter(string name) {
  LightEventCounter counter;
  counter.setDescription(name);
  this->_EventCounters.push_back(counter);
  return this->_EventCounters.size() - 1;
}

uint LightProfiler::createTimer(uint parent, const string &name) {
  LightTimer child(this);
  ;
  if (this->_Timers.size() == 0) {
    child.setup(0, "unused timer#0", UndefinedUnsignedInt);
    this->_Timers.push_back(child);
  }
  ASSERT_lessThan(parent, this->_Timers.size());
  child.setup(this->_Timers.size(), name, parent);
  this->_Timers.push_back(child);
  if (parent != UndefinedUnsignedInt) {
    this->_Timers[parent].addChild(child.getId());
  }
  return child.getId();
}

#if 0
LightTimer*	LightProfiler::getTimer(uint id)
{
LightTimer	*timer;
    if ( id >= this->_Timers.size() ) {
	SIMPLE_ERROR(BF("Illegal timer id" ));
    }
    timer = this->_Timers[id];
    if ( timer == NULL ) {
	SIMPLE_ERROR(BF("Undefined timer" ));
    }
    return timer;
}


void	LightProfiler::startTimer(uint id)
{
LightTimer	*timer;
    timer = this->getTimer(id);
    timer->start();
}


void	LightProfiler::stopTimer(uint id)
{
LightTimer	*timer;
    timer = this->getTimer(id);
    timer->stop();
}
#endif

void LightProfiler::createTimers(uint num) {
  for (int i = 0; i < num; ++i) {
    LightTimer child;
    this->_Timers.emplace_back(LightTimer());
  }
}

double LightProfiler::getLongestTime() {
  double max = 0.0;
  for (auto &t : this->_Timers) {
    if (max < t.getAccumulatedTime()) {
      max = t.getAccumulatedTime();
    }
  }
  return max;
}

void LightProfiler::resetAllTimers() {
  for (auto &t : this->_Timers) {
    t.reset();
  }
}

void LightProfiler::stopAllTimers() {
  for (auto &t : this->_Timers) {
    t.stop();
  }
}

void LightProfiler::dumpChildTimers(uint level, uint top) {
  uint child;
  stringstream prefix;
  if (this->_Timers[top].getCalls() == 0)
    return;
  child = this->_Timers[top].getChild();
  while (child != UndefinedUnsignedInt) {
    this->dumpChildTimers(level + 3, this->_Timers[child].getId());
    child = this->_Timers[child].getSibling();
  }
  prefix.str("");
  prefix << "%" << level + 20 << "s";
  printf("\n%s%s", prefix.str().c_str(), this->_Timers[top].getDescription().c_str());
  if (this->_Timers[top].getClockResolutionFails()) {
    printf("\n %10.4lf s  %6d calls  %6d clockResFails",
           this->_Timers[top].getAccumulatedTime(),
           this->_Timers[top].getCalls(),
           this->_Timers[top].getClockResolutionFails());
  } else {
    printf("\n %10.4lf s  %6d calls\n", this->_Timers[top].getAccumulatedTime(), this->_Timers[top].getCalls());
  }
  printf("\n");
}

void LightProfiler::dump() {
  int root;
  if (!this->_MessagesEnabled)
    return;
  //
  // Find a timer and crawl up the parents to find the root
  //

  if (this->_Timers.size() <= 1)
    return;
  GCTOOLS_ASSERTF(this->_Timers.size() >= 2, "There is no timer root");

  root = 1;
  if (!root) {
    THROW_HARD_ERROR(BF("There is no root timer!!!!!"));
  }
  //
  // Find the root timer
  //
  while (this->_Timers[root].getParent() != UndefinedUnsignedInt) {
    //	printf("\n%s:%d  looking for root - this->_Timers[%d]._Parent = %d", __FILE__,__LINE__,root,this->_Timers[root].getParent());
    root = this->_Timers[root].getParent();
  }
#ifdef DARWIN_CLOCK
//    printf( "Profiling with mach clock conversion=%lf\n", this->_Timers[root]._DarwinConversion );
#else
//    stringstream ss;
//    ss << "Profiling  CLOCKS_PER_SEC=" << CLOCKS_PER_SEC;
//    printf( "\n%s\n", ss.str().c_str() );
#endif
  this->dumpChildTimers(0, root);

  for (int i = 0; i < this->_Timers.size(); ++i) {
    printf("Timer %d accumulatedTime %lf\n", i, this->_Timers[i].getAccumulatedTime());
  }

  for (uint i = 0; i < this->_EventCounters.size(); i++) {
    int problems = this->_EventCounters[i].getProblems();
    int calls = this->_EventCounters[i].getCalls();
    if (calls == 0)
      continue;
    printf("\n %40s %10d problems over %10d calls (%lf%% problems)",
           this->_EventCounters[i].getDescription().c_str(),
           problems, calls, (1.0 * problems) / calls);
  }
  printf("\n");
}

LightProfiler::LightProfiler() {
  this->_Timers.clear();
  this->_EventCounters.clear();
  this->_MessagesEnabled = true;
}

LightProfiler::~LightProfiler() {
  for (uint i = 0; i < this->_Timers.size(); i++) {
    if (this->_Timers[i].getIsOn())
      this->_Timers[i].stop();
  }
  this->dump();
}

void LightProfiler::pushTimerStates() {
  vector<bool> states;
  states.resize(this->_Timers.size());
  for (uint i = 0; i < this->_Timers.size(); i++) {
    states[i] = this->_Timers[i].getIsOn();
  }
  this->_TimerStateStack.push_back(states);
}

void LightProfiler::popTimerStates() {
  vector<bool> states;
  if (this->_TimerStateStack.size() == 0)
    return;
  states = this->_TimerStateStack.back();
  for (uint i = 0; i < states.size(); i++) {
    if (states[i] != this->_Timers[i].getIsOn()) {
      if (states[i]) {
        this->_Timers[i].start();
      } else {
        this->_Timers[i].stop();
      }
    }
  }
  this->_TimerStateStack.erase(this->_TimerStateStack.end() - 1);
}

LightEventCounter &LightProfiler::eventCounter(uint c) {
  _OF();
  ASSERT_lessThan(c, this->_EventCounters.size());
  return this->_EventCounters[c];
}

LightTimer &LightProfiler::timer(uint c) {
  _OF();
  ASSERT_lessThan(c, this->_Timers.size());
  return this->_Timers[c];
}

void LightProfiler::disableMessages() {
  this->_MessagesEnabled = false;
}
};
