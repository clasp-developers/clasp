#include <cstdint>
#include <functional>
#include <vector>
#include <stdio.h>
#include <clasp/core/foundation.h>
#include <clasp/core/stackmap.h>

bool is_entry_point_arity(int64_t patchPointId, int64_t& arity_code) {
  if ((patchPointId & STACKMAP_REGISTER_SAVE_AREA_MASK) == STACKMAP_REGISTER_SAVE_AREA_MAGIC_NUMBER) {
    arity_code = (int64_t)(patchPointId & 0xF);
    return true;
  }
  return false;
}

template <typename T> static T read_then_advance(uintptr_t& address) {
  uintptr_t original = address;
  address = address + sizeof(T);
  return *(T*)original;
}

// Return true if the header was read
static bool parse_header(uintptr_t& address, uintptr_t end, smHeader& header, size_t& NumFunctions, size_t& NumConstants,
                         size_t& NumRecords) {
  header.version = read_then_advance<uint8_t>(address);
  header.reserved0 = read_then_advance<uint8_t>(address);
  header.reserved1 = read_then_advance<uint16_t>(address);
  if (address >= end)
    return false;
  NumFunctions = read_then_advance<uint32_t>(address);
  if (address >= end)
    return false;
  NumConstants = read_then_advance<uint32_t>(address);
  if (address >= end)
    return false;
  NumRecords = read_then_advance<uint32_t>(address);
  if (address >= end)
    return false;
  return true;
}

static void parse_function(uintptr_t& address, smStkSizeRecord& function) {
  function.FunctionAddress = read_then_advance<uint64_t>(address);
  function.StackSize = read_then_advance<uint64_t>(address);
  function.RecordCount = read_then_advance<uint64_t>(address);
}

static void parse_constant(uintptr_t& address, uint64_t& constant) { constant = read_then_advance<uint64_t>(address); }

static void parse_record(std::function<void(size_t, const smStkSizeRecord&, int32_t, int64_t)> thunk, uintptr_t& address,
                         size_t functionIndex, const smStkSizeRecord& function, smStkMapRecord& record) {
  [[maybe_unused]] uintptr_t recordAddress = address;
  uint64_t patchPointID = read_then_advance<uint64_t>(address);
  [[maybe_unused]] uint32_t instructionOffset = read_then_advance<uint32_t>(address);
  /* record.Reserved = */ read_then_advance<uint16_t>(address);
  size_t NumLocations = read_then_advance<uint16_t>(address);
  for (size_t index = 0; index < NumLocations; ++index) {
    uintptr_t recordAddress = address;
    /* record.Locations[index].Type = */ read_then_advance<uint8_t>(address);
    uint8_t reserved0 = read_then_advance<uint8_t>(address);
    /* record.Locations[index].LocationSize = */ read_then_advance<uint16_t>(address);
    /* record.Locations[index].DwarfRegNum = */ read_then_advance<uint16_t>(address);
    uint16_t reserved1 = read_then_advance<uint16_t>(address);
    if (reserved0 != 0 || reserved1 != 0) {
      printf("%s:%d:%s stackmap record @%p is out of alignment\n", __FILE__, __LINE__, __FUNCTION__, (void*)recordAddress);
      abort();
    }
    int32_t offsetOrSmallConstant = read_then_advance<int32_t>(address);
    int64_t arity_code;
    if (is_entry_point_arity(patchPointID, arity_code)) {
      thunk(functionIndex, function, offsetOrSmallConstant, patchPointID);
    }
  }
  if (((uintptr_t)address) & 0x7) {
    read_then_advance<uint32_t>(address);
  }
  if (((uintptr_t)address) & 0x7) {
    printf("%s:%d Address %lX is not word aligned - it must be!!!\n", __FILE__, __LINE__, address);
    abort();
  }
  /*Padding*/ read_then_advance<uint16_t>(address);
  size_t NumLiveOuts = read_then_advance<uint16_t>(address);
  for (size_t index = 0; index < NumLiveOuts; ++index) {
    /* record.LiveOuts[index].DwarfRegNum = */ read_then_advance<uint16_t>(address);
    /* record.LiveOuts[index].Reserved = */ read_then_advance<uint8_t>(address);
    /* record.LiveOuts[index].SizeInBytes = */ read_then_advance<uint8_t>(address);
  }
  if (((uintptr_t)address) & 0x7) {
    read_then_advance<uint32_t>(address);
  }
  if (((uintptr_t)address) & 0x7) {
    printf("%s:%d Address %lX is not word aligned - it must be!!!\n", __FILE__, __LINE__, address);
    abort();
  }
}

/* ! Parse an llvm Stackmap
     The format is described here: https://llvm.org/docs/StackMaps.html#stack-map-format
*/

void walk_one_llvm_stackmap(std::function<void(size_t, const smStkSizeRecord&, int32_t, int64_t)> thunk, uintptr_t& address,
                            uintptr_t end) {
  uintptr_t stackMapAddress = address;
  smHeader header;
  size_t NumFunctions;
  size_t NumConstants;
  size_t NumRecords;
  bool read = parse_header(address, end, header, NumFunctions, NumConstants, NumRecords);
  if (!read) {
    printf("%s:%d:%s Walked past the end of stackmaps!!!! address = %p end = %p\n", __FILE__, __LINE__, __FUNCTION__,
           (void*)address, (void*)end);
    abort();
  }
  if (header.version != 3 || header.reserved0 != 0 || header.reserved1 != 0) {
    printf("%s:%d:%s stackmap header @%p is out of alignment header.version=%d header.reserved0=%d header.reserved1=%d\n", __FILE__,
           __LINE__, __FUNCTION__, (void*)stackMapAddress, (int)header.version, (int)header.reserved0, (int)header.reserved1);
    abort();
  }
  uintptr_t functionAddress = address;
  for (size_t index = 0; index < NumFunctions; ++index) {
    smStkSizeRecord function;
    parse_function(address, function); // dummy - used to skip functions
  }
  for (size_t index = 0; index < NumConstants; ++index) {
    uint64_t constant;
    parse_constant(address, constant);
  }
  for (size_t functionIndex = 0; functionIndex < NumFunctions; ++functionIndex) {
    smStkSizeRecord function;
    parse_function(functionAddress, function);
    for (size_t index = 0; index < function.RecordCount; index++) {
      smStkMapRecord record;
      parse_record(thunk, address, functionIndex, function, record);
    }
  }
}
