#ifndef CLASP_CORE_PROFILER_EXECUTABLE_RANGES_H
#define CLASP_CORE_PROFILER_EXECUTABLE_RANGES_H

#include <atomic>
#include <cstddef>
#include <cstdint>

namespace core { namespace profiler_detail {

enum class ExecutableRangeRegistration { added, merged, full, invalid };

// A fixed-capacity executable-range table with signal-safe readers. The caller
// must serialize add() calls. Published slots never move, and their endpoints
// may only widen; reset() requires exclusive access with no active readers.
template <std::size_t Capacity>
class DynamicExecutableRanges {
  static_assert(Capacity > 0, "An executable-range table needs at least one slot");
  static_assert(std::atomic<uintptr_t>::is_always_lock_free,
                "Profiler range endpoints must be signal-safe lock-free atomics");
  static_assert(std::atomic<std::size_t>::is_always_lock_free,
                "Profiler range count must be a signal-safe lock-free atomic");

  struct Range {
    std::atomic<uintptr_t> lo{0};
    std::atomic<uintptr_t> hi{0}; // exclusive
  };
  Range _ranges[Capacity];
  std::atomic<std::size_t> _count{0};

public:
  std::size_t size() const noexcept {
    return _count.load(std::memory_order_acquire);
  }

  bool contains(uintptr_t pc) const noexcept {
    const std::size_t count = size();
    for (std::size_t i = 0; i < count; ++i) {
      const uintptr_t lo = _ranges[i].lo.load(std::memory_order_acquire);
      const uintptr_t hi = _ranges[i].hi.load(std::memory_order_acquire);
      if (pc >= lo && pc < hi) return true;
    }
    return false;
  }

  ExecutableRangeRegistration add(uintptr_t lo, uintptr_t hi) noexcept {
    if (lo >= hi) return ExecutableRangeRegistration::invalid;

    const std::size_t count = _count.load(std::memory_order_relaxed);
    if (count != 0) {
      Range& last = _ranges[count - 1];
      const uintptr_t old_lo = last.lo.load(std::memory_order_relaxed);
      const uintptr_t old_hi = last.hi.load(std::memory_order_relaxed);
      // Half-open intervals: equality means exact adjacency, not a gap.
      // Handle downward allocation too (new.hi == last.lo).
      if (lo <= old_hi && hi >= old_lo) {
        // Only widen. A reader observing one old and one new endpoint still
        // sees a subset of the valid union, never an unregistered gap. No
        // seqlock/retry loop: a signal may interrupt this very writer.
        if (lo < old_lo) last.lo.store(lo, std::memory_order_release);
        if (hi > old_hi) last.hi.store(hi, std::memory_order_release);
        return ExecutableRangeRegistration::merged;
      }
    }

    // Even a full table can accept extensions/duplicates of its last entry.
    if (count == Capacity) return ExecutableRangeRegistration::full;
    _ranges[count].lo.store(lo, std::memory_order_relaxed);
    _ranges[count].hi.store(hi, std::memory_order_relaxed);
    _count.store(count + 1, std::memory_order_release);
    return ExecutableRangeRegistration::added;
  }

  // Called between profiling sessions, with readers quiescent and writers
  // excluded. Old slots are initialized again before their next publication.
  void reset() noexcept {
    _count.store(0, std::memory_order_release);
  }
};

}} // namespace core::profiler_detail
#endif
