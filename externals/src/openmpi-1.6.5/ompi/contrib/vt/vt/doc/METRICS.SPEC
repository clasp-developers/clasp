# VampirTrace metrics specification
#
# measurement definitions complement/extend native (and PAPI preset) definitions
# measured metrics derive from a single measured counter (in principle)
# all measure definitions are considered equivalent
# (though some may be platform-specific)

### generic (PAPI) measurement aliases

### level 1 cache accesses
measure L1_ACCESS   = PAPI_L1_TCA
measure L1_I_ACCESS = PAPI_L1_ICA
measure L1_D_ACCESS = PAPI_L1_DCA

### level 1 cache reads
measure L1_READ     = PAPI_L1_TCR
measure L1_I_READ   = PAPI_L1_ICR # equivalent to PAPI_L1_ICA
measure L1_D_READ   = PAPI_L1_DCR

### level 1 cache writes
measure L1_WRITE    = PAPI_L1_TCW
measure L1_I_WRITE  = PAPI_L1_ICW # never defined
measure L1_D_WRITE  = PAPI_L1_DCW

### level 1 cache hits
measure L1_HIT      = PAPI_L1_TCH
measure L1_I_HIT    = PAPI_L1_ICH
measure L1_D_HIT    = PAPI_L1_DCH

### level 1 cache misses
measure L1_MISS     = PAPI_L1_TCM
measure L1_I_MISS   = PAPI_L1_ICM
measure L1_D_MISS   = PAPI_L1_DCM
measure L1_D_READ_MISS = PAPI_L1_LDM
measure L1_D_WRITE_MISS = PAPI_L1_STM

### alternate level 1 cache representation
measure L1_INST       = PAPI_L1_ICA
measure L1_INST_HIT   = PAPI_L1_ICH
measure L1_INST_MISS  = PAPI_L1_ICM
measure L1_LOAD       = PAPI_L1_DCR
measure L1_LOAD_HIT   = PAPI_L1_LDH # non-standard
measure L1_LOAD_MISS  = PAPI_L1_LDM
measure L1_STORE      = PAPI_L1_DCW
measure L1_STORE_HIT  = PAPI_L1_STH # non-standard
measure L1_STORE_MISS = PAPI_L1_STM

### level 2 cache accesses
measure L2_ACCESS   = PAPI_L2_TCA
measure L2_I_ACCESS = PAPI_L2_ICA
measure L2_D_ACCESS = PAPI_L2_DCA

### level 2 cache reads
measure L2_READ     = PAPI_L2_TCR
measure L2_I_READ   = PAPI_L2_ICR # equivalent to PAPI_L2_ICA
measure L2_D_READ   = PAPI_L2_DCR

### level 2 cache writes
measure L2_WRITE    = PAPI_L2_TCW
measure L2_I_WRITE  = PAPI_L2_ICW # never defined
measure L2_D_WRITE  = PAPI_L2_DCW

### level 2 cache hits
measure L2_HIT      = PAPI_L2_TCH
measure L2_I_HIT    = PAPI_L2_ICH
measure L2_D_HIT    = PAPI_L2_DCH

### level 2 cache misses
measure L2_MISS     = PAPI_L2_TCM
measure L2_I_MISS   = PAPI_L2_ICM
measure L2_D_MISS   = PAPI_L2_DCM
measure L2_D_READ_MISS = PAPI_L2_LDM
measure L2_D_WRITE_MISS = PAPI_L2_STM

### alternate level 2 cache representation
measure L2_INST       = PAPI_L2_ICA
measure L2_INST_HIT   = PAPI_L2_ICH
measure L2_INST_MISS  = PAPI_L2_ICM
measure L2_LOAD       = PAPI_L2_DCR
measure L2_LOAD_HIT   = PAPI_L2_LDH # non-standard
measure L2_LOAD_MISS  = PAPI_L2_LDM
measure L2_STORE      = PAPI_L2_DCW
measure L2_STORE_HIT  = PAPI_L2_STH # non-standard
measure L2_STORE_MISS = PAPI_L2_STM

### level 3 cache accesses
measure L3_ACCESS   = PAPI_L3_TCA
measure L3_I_ACCESS = PAPI_L3_ICA
measure L3_D_ACCESS = PAPI_L3_DCA

### level 3 cache reads
measure L3_READ     = PAPI_L3_TCR
measure L3_I_READ   = PAPI_L3_ICR # equivalent to PAPI_L3_ICA
measure L3_D_READ   = PAPI_L3_DCR

### level 3 cache writes
measure L3_WRITE    = PAPI_L3_TCW
measure L3_I_WRITE  = PAPI_L3_ICW # never defined
measure L3_D_WRITE  = PAPI_L3_DCW

### level 3 cache hits
measure L3_HIT      = PAPI_L3_TCH
measure L3_I_HIT    = PAPI_L3_ICH
measure L3_D_HIT    = PAPI_L3_DCH

### level 3 cache misses
measure L3_MISS     = PAPI_L3_TCM
measure L3_I_MISS   = PAPI_L3_ICM
measure L3_D_MISS   = PAPI_L3_DCM
measure L3_D_READ_MISS = PAPI_L3_LDM
measure L3_D_WRITE_MISS = PAPI_L3_STM

### alternate level 3 cache representation
measure L3_INST       = PAPI_L3_ICA
measure L3_INST_HIT   = PAPI_L3_ICH
measure L3_INST_MISS  = PAPI_L3_ICM
measure L3_LOAD       = PAPI_L3_DCR
measure L3_LOAD_HIT   = PAPI_L3_LDH # non-standard
measure L3_LOAD_MISS  = PAPI_L3_LDM
measure L3_STORE      = PAPI_L3_DCW
measure L3_STORE_HIT  = PAPI_L3_STH # non-standard
measure L3_STORE_MISS = PAPI_L3_STM

### TLB misses
measure TLB_MISS   = PAPI_TLB_TL
measure TLB_I_MISS = PAPI_TLB_IM
measure TLB_D_MISS = PAPI_TLB_DM

### instructions
measure INSTRUCTION    = PAPI_TOT_INS
measure INTEGER        = PAPI_INT_INS
measure FLOATING_POINT = PAPI_FP_INS
measure FP_ADD         = PAPI_FAD_INS
measure FP_MUL         = PAPI_FML_INS
measure FP_FMA         = PAPI_FMA_INS
measure FP_DIV         = PAPI_FDV_INS
measure FP_INV         = PAPI_FNV_INS
measure FP_SQRT        = PAPI_FSQ_INS
measure VECTOR         = PAPI_VEC_INS
measure SYNCH          = PAPI_SYC_INS
measure LOAD_STORE     = PAPI_LST_INS
measure LOAD           = PAPI_LD_INS
measure STORE          = PAPI_SR_INS
measure COND_STORE           = PAPI_CSR_TOT
measure COND_STORE_SUCCESS   = PAPI_CSR_SUC
measure COND_STORE_UNSUCCESS = PAPI_CSR_FAL
measure BRANCH               = PAPI_BR_INS
measure UNCOND_BRANCH        = PAPI_BR_UCN
measure COND_BRANCH          = PAPI_BR_CN
measure COND_BRANCH_TAKEN    = PAPI_BR_TKN
measure COND_BRANCH_NOTTAKEN = PAPI_BR_NTK
measure COND_BRANCH_PRED     = PAPI_BR_PRC
measure COND_BRANCH_MISPRED  = PAPI_BR_MSP

### cycles
measure CYCLES = PAPI_TOT_CYC

### idle units
measure INTEGER_UNIT_IDLE   = PAPI_FXU_IDL
measure FLOAT_UNIT_IDLE     = PAPI_FPU_IDL
measure BRANCH_UNIT_IDLE    = PAPI_BRU_IDL
measure LOADSTORE_UNIT_IDLE = PAPI_LSU_IDL

### stalls
measure STALL_MEMORY_ACCESS = PAPI_MEM_SCY
measure STALL_MEMORY_READ   = PAPI_MEM_RCY
measure STALL_MEMORY_WRITE  = PAPI_MEM_WCY
measure STALL_INST_ISSUE    = PAPI_STL_ICY

# platform-specific measurement aliases
# (complement or redefine generic measurement aliases)
# may need to key to particular platform if ambiguity

### POWER4-specific metrics
measure FP_LOAD  = PM_LSU_LDF
measure FP_STORE = PM_FPU_STF
measure FP_MISC  = PM_FPU_FMOV_FEST

### UltraSPARC-III/IV-specific metrics
measure STALL_L1_MISS    = Re_DC_miss # /1
measure STALL_L2_MISS    = Re_EC_miss # /1
measure STALL_IC_MISS    = Dispatch0_IC_miss # /0
measure STALL_STOREQ     = Rstall_storeQ # /0
measure STALL_IU_USE     = Rstall_IU_use # /0
measure STALL_FP_USE     = Rstall_FP_use # /1
measure STALL_PC_MISS    = Re_PC_miss # /1
measure STALL_RAW_MISS   = Re_RAW_miss # /1
measure STALL_FPU_BYPASS = Re_FPU_bypass # /1
measure STALL_MISPRED    = Dispatch0_mispred # /1
measure STALL_BR_TARGET  = Dispatch0_br_target # /0
measure STALL_2ND_BR     = Dispatch0_2nd_br # /0
measure STALL_L1_MSOVHD  = Re_DC_missovhd # /1

### groupings of metrics for collective measurement

### Opteron groupings (max 4 in group, unrestricted)
aggroup OPTERON_DC1 = DC_ACCESS DC_MISS DC_L2_REFILL_I DC_SYS_REFILL_I
aggroup OPTERON_DC2 = DC_L2_REFILL_M DC_L2_REFILL_O DC_L2_REFILL_E DC_L2_REFILL_S
aggroup OPTERON_DC3 = DC_SYS_REFILL_M DC_SYS_REFILL_O DC_SYS_REFILL_E DC_SYS_REFILL_S
aggroup OPTERON_IC  = IC_FETCH IC_MISS IC_L2_REFILL IC_SYS_REFILL
aggroup OPTERON_TLB = DC_L1_DTLB_MISS_AND_L2_DTLB_MISS DC_L1_DTLB_MISS_AND_L2_DTLB_HIT IC_L1ITLB_MISS_AND_L2ITLB_MISS IC_L1ITLB_MISS_AND_L2ITLB_HIT

aggroup OPTERON_BR  = FR_BR FR_BR_MIS FR_BR_TAKEN FR_BR_TAKEN_MIS
aggroup OPTERON_FP  = FP_ADD_PIPE FP_MULT_PIPE FP_ST_PIPE FP_FAST_FLAG
aggroup OPTERON_FPU = FR_FPU_X87 FR_FPU_MMX_3D FR_FPU_SSE_SSE2_PACKED FR_FPU_SSE_SSE2_SCALAR

aggroup OPTERON_ST1 = IC_FETCH_STALL FR_DECODER_EMPTY FR_DISPATCH_STALLS FR_DISPATCH_STALLS_FULL_FPU
aggroup OPTERON_ST2 = FR_DISPATCH_STALLS_FULL_LS FR_DISPATCH_STALLS_FULL_REORDER FR_DISPATCH_STALLS_FULL_RESERVATION FR_DISPATCH_STALLS_BR
aggroup OPTERON_ST3 = FR_DISPATCH_STALLS_FAR FR_DISPATCH_STALLS_SER FR_DISPATCH_STALLS_SEG FR_DISPATCH_STALLS_QUIET

aggroup OPTERON_ETC = FR_X86_INS CPU_CLK_UNHALTED FR_HW_INTS FP_NONE_RET
aggroup OPTERON_HTM = HT_LL_MEM_XFR HT_LR_MEM_XFR HT_RL_MEM_XFR
aggroup OPTERON_HTI = HT_LL_IO_XFR HT_LR_IO_XFR HT_RL_IO_XFR

### POWER4-specific groupings (max 8 in group, restricted)
aggroup POWER4_DC  = PM_DATA_FROM_L2 PM_DATA_FROM_L25_SHR PM_DATA_FROM_L25_MOD PM_DATA_FROM_L275_SHR PM_DATA_FROM_L275_MOD PM_DATA_FROM_L3 PM_DATA_FROM_L35 PM_DATA_FROM_MEM # 5
aggroup POWER4_IC  = PM_INST_FROM_PREF PM_INST_FROM_L1 PM_INST_FROM_L2 PM_INST_FROM_L25_L275 PM_INST_FROM_L3 PM_INST_FROM_L35 PM_INST_FROM_MEM # 6
aggroup POWER4_L1  = PM_LD_REF_L1 PM_LD_MISS_L1 PM_ST_REF_L1 PM_ST_MISS_L1 # 56
aggroup POWER4_TLB = PM_ITLB_MISS PM_DTLB_MISS
aggroup POWER4_LX  = PM_ITLB_MISS PM_DTLB_MISS PM_LD_REF_L1 PM_LD_MISS_L1 PM_ST_REF_L1 PM_ST_MISS_L1 # 56
aggroup POWER4_BR  = PM_BR_ISSUED PM_BR_MPRED_CR PM_BR_MPRED_TA # 3,55,61
aggroup POWER4_BRT = PM_BR_ISSUED PM_BR_MPRED_CR PM_BR_MPRED_TA PM_BIQ_IDU_FULL_CYC PM_BRQ_FULL_CYC PM_L1_WRITE_CYC PM_INST_CMPL PM_CYC # 55
aggroup POWER4_LSF = PM_FPU_STF PM_LSU_LDF # 15,54,60
aggroup POWER4_STL = PM_CYC PM_FPU_FULL_CYC PM_FPU_STALL3 # 54
aggroup POWER4_LST = PM_INST_CMPL PM_FPU_STF PM_LSU_LDF PM_CYC PM_FPU_FULL_CYC PM_FPU_STALL3 # 54
aggroup POWER4_FP  = PM_FPU_FIN PM_FPU_FMA PM_FPU_FDIV PM_FPU_FSQRT PM_FPU_FMOV_FEST # 53
aggroup POWER4_IFP = PM_FPU_FIN PM_FPU_FMA PM_FPU_FDIV PM_FPU_FSQRT PM_FPU_FMOV_FEST PM_FXU_FIN # 53
aggroup POWER4_II  = PM_INST_DISP PM_INST_CMPL # 1,2,18,20

### MIPS-R1200 groupings (max 32 in group, unrestricted)
aggroup R12000_ALL = TLB_misses primary_data_cache_misses secondary_data_cache_misses primary_instruction_cache_misses secondary_instruction_cache_misses graduated_instructions mispredicted_branches graduated_loads graduated_stores graduated_floating-point_instructions decoded_instructions cycles prefetch_primary_data_cache_misses
aggroup R12000_ALL_PAPI = PAPI_L1_DCM PAPI_L1_ICM PAPI_L2_DCM PAPI_L2_ICM PAPI_TLB_TL PAPI_TOT_INS PAPI_FP_INS PAPI_LD_INS PAPI_SR_INS PAPI_TOT_IIS PAPI_BR_CN PAPI_BR_MSP PAPI_CSR_TOT PAPI_CSR_FAL PAPI_TOT_CYC PAPI_PRF_DM PAPI_CA_INV PAPI_CA_ITV

### UltraSPARC-III/IV groupings (max 2 in group, restricted)
aggroup US3_CPI = Cycle_cnt Instr_cnt                   # duplicates

# cycles/stalls groups
aggroup US3_SMP = Dispatch_rs_mispred Dispatch0_mispred # stall misprediction
aggroup US3_SUS = Rstall_IU_use Rstall_FP_use           # stall IU/FP use
aggroup US3_SST = Rstall_storeQ Re_RAW_miss             # stall store
aggroup US3_SCD = Cycle_cnt Re_DC_miss
aggroup US3_SCO = Dispatch0_br_target Re_DC_missovhd
aggroup US3_SCE = Dispatch0_2nd_br Re_EC_miss
aggroup US3_SCP = Dispatch0_IC_miss Re_PC_miss
aggroup US3_SCX = SI_ciq_flow Re_FPU_bypass       # Re_FPU_bypass always zero?

# instruction and TLB groups
aggroup US3_FPU = FA_pipe_completion FM_pipe_completion
aggroup US3_BMS = IU_Stat_Br_miss_taken IU_Stat_Br_miss_untaken
aggroup US3_BCS = IU_Stat_Br_count_taken IU_Stat_Br_count_untaken
aggroup US3_ITL = Instr_cnt ITLB_miss
aggroup US3_DTL = Cycle_cnt DTLB_miss

# memory and cache groups
aggroup US3_ICH = IC_ref IC_miss
aggroup US3_DCR = DC_rd DC_rd_miss
aggroup US3_DCW = DC_wr DC_wr_miss
aggroup US3_ECI = EC_write_hit_RTO EC_ic_miss
aggroup US3_ECM = EC_rd_miss EC_misses 

# locality/SSM and other miscellaneous groups
aggroup US3_ECL = EC_miss_local EC_miss_remote          # only SF15000/SF25000
aggroup US3_ECX = EC_wb_remote EC_miss_mtag_remote      # only SF15000/SF25000
aggroup US3_ECW = EC_ref EC_wb
aggroup US3_ECS = EC_snoop_inv EC_snoop_cb
aggroup US3_PCR = PC_port0_rd PC_port1_rd
aggroup US3_ETC = SI_snoop PC_MS_misses
aggroup US3_WCM = SI_owned WC_miss

# memory controller groups
aggroup US3_SM1 = MC_stalls_0 MC_stalls_1
aggroup US3_SM2 = MC_stalls_2 MC_stalls_3
aggroup US3_MC0 = MC_reads_0 MC_writes_0
aggroup US3_MC1 = MC_reads_1 MC_writes_1
aggroup US3_MC2 = MC_reads_2 MC_writes_2
aggroup US3_MC3 = MC_reads_3 MC_writes_3

### Itanium2 groupings (max 4 in group, partially restricted)
aggroup ITANIUM2_TLB = ITLB_MISSES_FETCH_L1ITLB ITLB_MISSES_FETCH_L2ITLB L2DTLB_MISSES L1DTLB_TRANSFER
aggroup ITANIUM2_BR  = BRANCH_EVENT BR_MISPRED_DETAIL_ALL_CORRECT_PRED BR_MISPRED_DETAIL_ALL_WRONG_PATH BR_MISPRED_DETAIL_ALL_WRONG_TARGET
aggroup ITANIUM2_STL = DISP_STALLED BACK_END_BUBBLE_ALL BE_EXE_BUBBLE_ALL BE_EXE_BUBBLE_FRALL

aggroup ITANIUM2_L1D = DATA_REFERENCES_SET1 L1D_READS_SET1 L1D_READ_MISSES_ALL L2_DATA_REFERENCES_L2_ALL
aggroup ITANIUM2_L2D = L2_DATA_REFERENCES_L2_DATA_READS L2_DATA_REFERENCES_L2_DATA_WRITES L3_READS_DATA_READ_ALL L3_WRITES_DATA_WRITE_ALL
aggroup ITANIUM2_L3D = L3_READS_DATA_READ_HIT L3_READS_DATA_READ_MISS L3_WRITES_DATA_WRITE_HIT L3_WRITES_DATA_WRITE_MISS
aggroup ITANIUM2_LXD = L2_MISSES L3_REFERENCES L3_READS_ALL_MISS L3_WRITES_ALL_MISS
aggroup ITANIUM2_LXX = L3_MISSES L3_WRITES_L2_WB_HIT L3_WRITES_L2_WB_MISS

aggroup ITANIUM2_ICD = L1I_READS L2_INST_DEMAND_READS L3_READS_DINST_FETCH_HIT L3_READS_DINST_FETCH_MISS # instruction cache (demand-load only)
aggroup ITANIUM2_ICP = L1I_PREFETCHES L2_INST_PREFETCHES L3_READS_INST_FETCH_HIT L3_READS_INST_FETCH_MISS # instruction cache (incl. prefetch)

aggroup ITANIUM2_IN1 = INST_DISPERSED IA32_INST_RETIRED IA64_INST_RETIRED LOADS_RETIRED
aggroup ITANIUM2_IN2 = FP_OPS_RETIRED LOADS_RETIRED CPU_CYCLES ISA_TRANSITIONS
aggroup ITANIUM2_ISA = IA32_INST_RETIRED IA64_INST_RETIRED IA32_ISA_TRANSITIONS STORES_RETIRED
aggroup ITANIUM2_FLP = CPU_CYCLES FP_OPS_RETIRED INST_DISPERSED LOADS_RETIRED

### compositions are derived by combining measurements and create hierarchies
### **** generic hierarchy ****

### cycles (including stalls)
compose CYCLES = BUSY + STALL + IDLE
compose STALL = DISPATCH + UNIT_USE + RECIRCULATE

### instructions
compose INSTRUCTION = BRANCH + INTEGER + FLOATING_POINT + MEMORY
compose BRANCH = BRANCH_PRED + BRANCH_MISP
compose FLOATING_POINT = FP_ADD + FP_MUL + FP_FMA + FP_DIV + FP_INV + FP_SQRT + FP_MISC
compose MEMORY = LOAD + STORE + SYNCH

### data accesses (to cache hierarchy & memory)
compose DATA_ACCESS = DATA_HIT_L1$ + DATA_HIT_L2$ + DATA_HIT_L3$ + DATA_HIT_MEM
compose DATA_HIT_L1$ = DATA_STORE_INTO_L1$ + DATA_LOAD_FROM_L1$
compose DATA_HIT_L2$ = DATA_STORE_INTO_L2$ + DATA_LOAD_FROM_L2$
compose DATA_HIT_L3$ = DATA_STORE_INTO_L3$ + DATA_LOAD_FROM_L3$
compose DATA_HIT_MEM = DATA_STORE_INTO_MEM + DATA_LOAD_FROM_MEM

### instruction accesses (to cache hierarchy & memory)
compose INST_ACCESS = INST_HIT_PREF + INST_HIT_L1$ + INST_HIT_L2$ + INST_HIT_L3$ + INST_HIT_MEM

### TLB accesses (instruction & data)
compose TLB_ACCESS = DATA_TLB_ACCESS + INST_TLB_ACCESS
compose DATA_TLB_ACCESS = DATA_TLB_HIT + DATA_TLB_MISS
compose INST_TLB_ACCESS = INST_TLB_HIT + INST_TLB_MISS

