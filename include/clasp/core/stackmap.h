#ifndef stackmap_H

#include <cstdint>
#include <functional>
#include <vector>

struct smHeader {
  uint8_t  version;
  uint8_t  reserved0;
  uint16_t reserved1;
};

struct smStkSizeRecord {
  uint64_t  FunctionAddress;
  int64_t  StackSize;
  uint64_t  RecordCount;
};

struct smLocation{
  uint8_t  Type;
  uint8_t   Reserved0;
  uint16_t  LocationSize;
  uint16_t  DwarfRegNum;
  uint16_t  Reserved1;
  int32_t   OffsetOrSmallConstant;
};

struct smLiveOut {
  uint16_t DwarfRegNum;
  uint8_t  Reserved;
  uint8_t SizeInBytes;
};

struct smStkMapRecord {
  uint64_t PatchPointID;
  uint32_t InstructionOffset;
  uint16_t Reserved;
  std::vector<smLocation> Locations;
  std::vector<smLiveOut> LiveOuts;
};

void walk_one_llvm_stackmap(std::function<void(size_t,
                                               const smStkSizeRecord&,
                                               int32_t)>,
                            uintptr_t&, uintptr_t);

#endif /* guard */
