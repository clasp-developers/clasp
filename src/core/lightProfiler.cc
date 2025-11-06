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
#include <clasp/core/designators.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/unixfsys.h>
#include <unistd.h>

//  Profiler implementation

#include "profiler.cc"


#if 0
#include <chrono>
#include <stack>
#include <string>
#include <unordered_map>
#include <mutex>
#include <iostream>
#include <fstream>
#include <thread>
#include <vector>
#include <memory>
#include <sstream>
#include <locale>

class Profiler {
public:
      // Start timing a code region with a given name
  static void RangePush(const std::string& name) {
    auto now = std::chrono::high_resolution_clock::now();

    if (!rootRegion) {
      rootRegion = std::make_shared<RegionNode>("Root");
      rootRegion->startTime = now; // Record the start time
                  // Do not push rootRegion onto regionStack
    }

    RegionNode* current;
    if (regionStack.empty()) {
                  // If the stack is empty, start from rootRegion
      current = rootRegion.get();
    } else {
      current = regionStack.top();
    }

            // Check if the child already exists
    auto it = current->children.find(name);
    if (it != current->children.end()) {
                  // Child exists; reuse it
      regionStack.push(it->second.get());
    } else {
                  // Create a new child
      auto newRegion = std::make_shared<RegionNode>(name, current);
      newRegion->startTime = now; // Record start time for the region
      current->children[name] = newRegion;
      regionStack.push(newRegion.get());
    }

    getTimingStack().emplace(now);
  }

      // End timing the most recent code region
  static void RangePop() {
    auto now = std::chrono::high_resolution_clock::now();
    if (getTimingStack().empty() || regionStack.empty()) {
      std::cerr << "RangePop called without matching RangePush\n";
      return;
    }
    auto start_time = getTimingStack().top();
    getTimingStack().pop();

    RegionNode* current = regionStack.top();
    regionStack.pop();

    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(now - start_time).count();
    current->totalTime += duration;

            // If the regionStack is empty, we've returned to the root
    if (regionStack.empty()) {
                  // Calculate Root node's totalTime
      auto rootDuration = std::chrono::duration_cast<std::chrono::microseconds>(now - rootRegion->startTime).count();
      rootRegion->totalTime = rootDuration;

      std::lock_guard<std::mutex> lock(dataMutex);
      allThreadRegions.push_back(rootRegion);
      rootRegion.reset();
    }
  }

      // Report the accumulated timing data to the provided ostream
  static void Report(std::ostream& outStream) {
    std::lock_guard<std::mutex> lock(dataMutex);
    for (const auto& root : allThreadRegions) {
      printRegion(outStream, root.get(), 0);
    }
  }

      // Generate an HTML string containing the flame graph
  static std::string ReportFlameGraph() {
    std::lock_guard<std::mutex> lock(dataMutex);

    if (allThreadRegions.empty()) {
      return "<html><body><p>No profiling data available.</p></body></html>";
    }

            // For simplicity, we'll sum up the root times from all threads
    long long totalRootTime = 0;
    for (const auto& root : allThreadRegions) {
      totalRootTime += root->totalTime;
    }

            // SVG content
    std::vector<std::string> svgElements;

    int depth = 0;
    double svgWidth = 1000.0;

    double currentX = 0.0;
    for (const auto& root : allThreadRegions) {
      double rootWidth = svgWidth * (static_cast<double>(root->totalTime) / totalRootTime);
      processRegion(root.get(), currentX, rootWidth, depth, svgElements);
      currentX += rootWidth; // Move to the next root region
    }

            // Calculate SVG height based on maximum depth
    int maxDepth = getMaxDepth();
    int svgHeight = (maxDepth + 1) * 20;

            // Assemble SVG content
    std::ostringstream svgContent;
    svgContent << "<svg width=\"" << svgWidth << "\" height=\"" << svgHeight << "\" "
        "xmlns=\"http://www.w3.org/2000/svg\" "
        "version=\"1.1\">";

    for (const auto& element : svgElements) {
      svgContent << element << "\n";
    }

    svgContent << "</svg>";

            // Embed SVG in HTML
    std::ostringstream htmlContent;
    htmlContent << "<html><body>\n"
                << svgContent.str()
                << "\n</body></html>";

    return htmlContent.str();
  }

      // Reset the Profiler's data structures
  static void Reset() {
    std::lock_guard<std::mutex> lock(dataMutex);
    allThreadRegions.clear();
            // Reset thread-local variables
    rootRegion.reset();
    while (!regionStack.empty()) {
      regionStack.pop();
    }
  }

private:
  using TimePoint = std::chrono::high_resolution_clock::time_point;

  struct RegionNode {
    std::string name;
    long long totalTime = 0; // totalTime in microseconds
    std::unordered_map<std::string, std::shared_ptr<RegionNode>> children;
    RegionNode* parent = nullptr;
    TimePoint startTime; // To record when the region started

    RegionNode(const std::string& name, RegionNode* parent = nullptr)
        : name(name), parent(parent) {}
  };

  static std::stack<TimePoint>& getTimingStack() {
    static thread_local std::stack<TimePoint> timingStack;
    return timingStack;
  }

        // Helper function to print the region tree recursively to the provided ostream
    static void printRegion(std::ostream& outStream, RegionNode* node, int indent) {
      for (int i = 0; i < indent; ++i) {
        outStream << "  ";
      }

      if ( node->totalTime < 1000) {
        outStream << node->name << ": " << node->totalTime << " microseconds\n";
      } else if ( node->totalTime < 1000000 ) {
        outStream << node->name << ": " << node->totalTime/1000 << " milliseconds\n";
      } else {
        outStream << node->name << ": " << node->totalTime/1000000 << " seconds\n";
      }
      for (const auto& child : node->children) {
        printRegion(outStream, child.second.get(), indent + 1);
      }
    }

      // Helper function to process each region and generate SVG elements for the flame graph
  static void processRegion(
      RegionNode* node,
      double rectX,
      double rectWidth,
      int depth,
      std::vector<std::string>& svgElements) {

    double rectY = depth * 20;
    double rectHeight = 20;

            // Generate a rectangle SVG element
    std::ostringstream rectElement;
    rectElement << "<rect x=\"" << rectX << "\" y=\"" << rectY
                << "\" width=\"" << rectWidth << "\" height=\"" << rectHeight
                << "\" style=\"fill:rgb(173,216,230);stroke:black;stroke-width:1\" />";

            // Format the totalTime with commas for thousands
    std::ostringstream timeStream;
    timeStream.imbue(std::locale(""));
    timeStream << std::fixed << node->totalTime;

            // Generate a text SVG element, including the time in microseconds (μs)
    std::ostringstream textElement;
    textElement << "<text x=\"" << rectX + 2 << "\" y=\"" << rectY + 15
                << "\" font-size=\"12\">" << node->name << " (" << timeStream.str() << " μs)" << "</text>";

    svgElements.push_back(rectElement.str());
    svgElements.push_back(textElement.str());

            // If the node has no time or width, we cannot render its children properly
    if (node->totalTime <= 0 || rectWidth <= 0) {
      return;
    }

            // Process child nodes
    double childX = rectX;
    for (const auto& child : node->children) {
      double childWidth = rectWidth * (static_cast<double>(child.second->totalTime) / node->totalTime);
      processRegion(child.second.get(), childX, childWidth, depth + 1, svgElements);
      childX += childWidth;
    }
  }

      // Helper function to get the maximum depth of the region tree
  static int getMaxDepth() {
    int maxDepth = 0;
    for (const auto& root : allThreadRegions) {
      int depth = getDepth(root.get());
      if (depth > maxDepth) {
        maxDepth = depth;
      }
    }
    return maxDepth;
  }

      // Helper function to get the depth of a region node
  static int getDepth(RegionNode* node) {
    int maxChildDepth = 0;
    for (const auto& child : node->children) {
      int childDepth = getDepth(child.second.get());
      if (childDepth > maxChildDepth) {
        maxChildDepth = childDepth;
      }
    }
    return 1 + maxChildDepth;
  }

  static thread_local std::shared_ptr<RegionNode> rootRegion;
  static thread_local std::stack<RegionNode*> regionStack; // Stack of current regions
  static std::vector<std::shared_ptr<RegionNode>> allThreadRegions;
  static std::mutex dataMutex;
};

// Static member definitions
thread_local std::shared_ptr<Profiler::RegionNode> Profiler::rootRegion = nullptr;
thread_local std::stack<Profiler::RegionNode*> Profiler::regionStack;
std::vector<std::shared_ptr<Profiler::RegionNode>> Profiler::allThreadRegions;
std::mutex Profiler::dataMutex;
#endif

namespace core {



CL_DOCSTRING("Reset the profiler.");
CL_DEFUN void core__profiler_reset() {
  Profiler::Reset();
}

CL_DOCSTRING("Push a nested range of profiling. Profiler will start a new timing of this range and report will print a nested time for this range.");
CL_DEFUN void core__profiler_push(const string& name) {
  Profiler::RangePush(name);
}

CL_DOCSTRING("Pop the current level of profiling");
CL_DEFUN void core__profiler_pop() {
  Profiler::RangePop();
}

CL_DOCSTRING("Generate a profiling textual report to OUTPUT-STREAM-DESIGNATOR.")
CL_LAMBDA(&optional (output-stream-designator t));
CL_DEFUN void core__profiler_report(T_sp output_stream_designator) {
  stringstream sout;
  Profiler::Report(sout);
  T_sp stream = coerce::outputStreamDesignator(output_stream_designator);
  clasp_write_string(sout.str(),stream);
}

CL_DOCSTRING("Generate a profiling svg flame chart report to OUTPUT-DESIGNATOR. OUTPUT-DESIGNATOR can be a stream or a filename or a pathname. If it's a filename or pathname then it will be written to and given permissions 644")
CL_LAMBDA(&optional (output-designator t));
CL_DEFUN void core__profiler_flame_graph_report(T_sp output_designator) {
  if (gc::IsA<String_sp>(output_designator)) {
    T_sp stream = core::cl__open(gc::As_unsafe<String_sp>(output_designator), StreamDirection::output, cl::_sym_character, StreamIfExists::supersede, true, StreamIfDoesNotExist::create, true );
    std::string report = Profiler::ReportFlameGraph();
    clasp_write_string(report,stream);
    core::cl__close(stream);
    core__chmod(gc::As_unsafe<String_sp>(output_designator),clasp_make_fixnum(0644));
  } else if (gc::IsA<Pathname_sp>(output_designator)) {
    T_sp stream = core::cl__open(gc::As<Pathname_sp>(output_designator), StreamDirection::output, cl::_sym_character, StreamIfExists::supersede,true, StreamIfDoesNotExist::create, true);
    std::string report = Profiler::ReportFlameGraph();
    clasp_write_string(report,stream);
    core::cl__close(stream);
    core__chmod(gc::As_unsafe<Pathname_sp>(output_designator),clasp_make_fixnum(0644));
  } else {
    T_sp stream = coerce::outputStreamDesignator(output_designator);
    std::string report = Profiler::ReportFlameGraph();
    clasp_write_string(report,stream);
  }
}

CL_DEFUN void core__profiler_testc() {
  Profiler::RangePush("AA");
  for (int i = 0; i<3; i++ ) {
    Profiler::RangePush("B");
    usleep(100);
    for (int j=0; j<4; j++ ) {
      Profiler::RangePush("C");
      usleep(200);
      Profiler::RangePop();
    }
    for (int k=0; k<4; k++ ) {
      Profiler::RangePush("D");
      usleep(50);
      Profiler::RangePop();
    }
    Profiler::RangePop();
  }
  Profiler::RangePop();
}


};

namespace core {

LightEventCounter::LightEventCounter() {
  this->_Calls = 0;
  this->_Problems = 0;
}

LightEventCounter::~LightEventCounter() {}

void LightEventCounter::recordCallAndProblem(bool prob) {
  this->_Calls++;
  if (prob)
    this->_Problems++;
}

LightTimer::LightTimer(LightProfiler* profiler)
    : _Profiler(profiler), _Id(0), _IsOn(false), _AccumulatedTime(0.0), _Calls(0), _ClockResolutionFails(0),
      _Parent(UndefinedUnsignedInt), _Sibling(UndefinedUnsignedInt), _Child(UndefinedUnsignedInt), _StartTime(0.0) {
#ifdef DARWIN_CLOCK
  mach_timebase_info_data_t info;
  kern_return_t err = mach_timebase_info(&info);
  if (err == 0) {
    this->_DarwinConversion = 1.0e-9 * (double)info.numer / (double)info.denom;
  } else {
    THROW_HARD_ERROR("Could not determine clock conversion");
  }
#else
// #error "What initialization needs to be done for LightTimer on non DARWIN systems"
#endif
}

void LightTimer::addChild(uint childIndex) {
  HARD_ASSERT(this->_Profiler != NULL);
  LightTimer& child = this->_Profiler->timer(childIndex);
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
    THROW_HARD_ERROR("Timer {} is not on", this->_Description);
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

uint LightProfiler::createTimer(uint parent, const string& name) {
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
	SIMPLE_ERROR("Illegal timer id");
    }
    timer = this->_Timers[id];
    if ( timer == NULL ) {
	SIMPLE_ERROR("Undefined timer");
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
  for (auto& t : this->_Timers) {
    if (max < t.getAccumulatedTime()) {
      max = t.getAccumulatedTime();
    }
  }
  return max;
}

void LightProfiler::resetAllTimers() {
  for (auto& t : this->_Timers) {
    t.reset();
  }
}

void LightProfiler::stopAllTimers() {
  for (auto& t : this->_Timers) {
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
    printf("\n %10.4lf s  %6d calls  %6d clockResFails", this->_Timers[top].getAccumulatedTime(), this->_Timers[top].getCalls(),
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
    THROW_HARD_ERROR("There is no root timer!!!!!");
  }
  //
  // Find the root timer
  //
  while (this->_Timers[root].getParent() != UndefinedUnsignedInt) {
    //	printf("\n%s:%d  looking for root - this->_Timers[%d]._Parent = %d",
    //__FILE__,__LINE__,root,this->_Timers[root].getParent());
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
    printf("\n %40s %10d problems over %10d calls (%lf%% problems)", this->_EventCounters[i].getDescription().c_str(), problems,
           calls, (1.0 * problems) / calls);
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

LightEventCounter& LightProfiler::eventCounter(uint c) {

  ASSERT_lessThan(c, this->_EventCounters.size());
  return this->_EventCounters[c];
}

LightTimer& LightProfiler::timer(uint c) {

  ASSERT_lessThan(c, this->_Timers.size());
  return this->_Timers[c];
}

void LightProfiler::disableMessages() { this->_MessagesEnabled = false; }
}; // namespace core
