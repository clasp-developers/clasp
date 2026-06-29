// Hardcoded AArch64 trampoline templates.
// One shared CIE, separate code+FDE per kind (bytecode vs GF).
//
// At init time: copy the code array, patch the 8-byte target address at the
// known offset (a literal pool quad after the ret), then hand code+CIE+FDE
// to the arena's install_template(). No LLVM JIT compilation needed.
//
// Works on both Linux aarch64 and Apple Silicon (same ISA). Platform
// differences (MAP_JIT, W^X toggling) live in ExecutableArena, not here.

#pragma once
#include <cstdint>
#include <cstddef>

#if defined(__aarch64__)

namespace llvmo {
namespace trampoline_aarch64 {

// =========================================================================
// Bytecode trampoline (36 bytes: 28 code + 8 literal pool)
// Signature: (i64 pc, ptr closure, i64 nargs, ptr args) -> {ptr, i64}
//            x0=pc   x1=closure   x2=nargs   x3=args
// Passes all args through; saves pc (x0) on stack for debugger visibility.
// Target address lives in a literal pool quad at offset 28, loaded via
// LDR (literal) from offset 12.
//
//   0: stp  x29, x30, [sp, #-32]!    save fp/lr, sp -= 32
//   4: mov  x29, sp                   frame pointer
//   8: str  x0, [x29, #16]           save bytecode PC for debugger
//  12: ldr  x16, #16                 load target from literal pool (+16 bytes)
//  16: blr  x16                      call target
//  20: ldp  x29, x30, [sp], #32      restore fp/lr, sp += 32
//  24: ret
//  28: .quad <target>                 8-byte absolute address (PATCH HERE)
// =========================================================================
static constexpr uint8_t bytecode_code[] = {
    0xfd, 0x7b, 0xbe, 0xa9,  // stp  x29, x30, [sp, #-32]!
    0xfd, 0x03, 0x00, 0x91,  // mov  x29, sp
    0xa0, 0x0b, 0x00, 0xf9,  // str  x0, [x29, #16]
    0x90, 0x00, 0x00, 0x58,  // ldr  x16, #+16  (literal at offset 28)
    0x00, 0x02, 0x3f, 0xd6,  // blr  x16
    0xfd, 0x7b, 0xc2, 0xa8,  // ldp  x29, x30, [sp], #32
    0xc0, 0x03, 0x5f, 0xd6,  // ret
    0x00, 0x00, 0x00, 0x00,  //   <-- 8-byte target address (low word)
    0x00, 0x00, 0x00, 0x00,  //       (high word) — patch here
};
static constexpr size_t bytecode_code_size = sizeof(bytecode_code);  // 36
static constexpr size_t bytecode_target_offset = 28;

// =========================================================================
// GF trampoline (32 bytes: 24 code + 8 literal pool)
// Signature: (ptr closure, i64 nargs, ptr args) -> {ptr, i64}
//            x0=closure   x1=nargs   x2=args
// Pure passthrough, no extra saves.
// Target address lives in a literal pool quad at offset 24, loaded via
// LDR (literal) from offset 8.
//
//   0: stp  x29, x30, [sp, #-16]!    save fp/lr, sp -= 16
//   4: mov  x29, sp                   frame pointer
//   8: ldr  x16, #16                 load target from literal pool (+16 bytes)
//  12: blr  x16                      call target
//  16: ldp  x29, x30, [sp], #16      restore fp/lr, sp += 16
//  20: ret
//  24: .quad <target>                 8-byte absolute address (PATCH HERE)
// =========================================================================
static constexpr uint8_t gf_code[] = {
    0xfd, 0x7b, 0xbf, 0xa9,  // stp  x29, x30, [sp, #-16]!
    0xfd, 0x03, 0x00, 0x91,  // mov  x29, sp
    0x90, 0x00, 0x00, 0x58,  // ldr  x16, #+16  (literal at offset 24)
    0x00, 0x02, 0x3f, 0xd6,  // blr  x16
    0xfd, 0x7b, 0xc1, 0xa8,  // ldp  x29, x30, [sp], #16
    0xc0, 0x03, 0x5f, 0xd6,  // ret
    0x00, 0x00, 0x00, 0x00,  //   <-- 8-byte target address (low word)
    0x00, 0x00, 0x00, 0x00,  //       (high word) — patch here
};
static constexpr size_t gf_code_size = sizeof(gf_code);  // 32
static constexpr size_t gf_target_offset = 24;

// =========================================================================
// CIE (24 bytes) — shared between both trampoline kinds.
//
// Augmentation "zR": FDE pointers use pcrel|sdata4 encoding (0x1b).
// No personality, no LSDA — bytes are fully position-independent.
// Initial rules: CFA = sp (x31) + 0, return address register = x30 (LR).
// Code alignment factor = 4 (AArch64 fixed-width instructions).
// Data alignment factor = -8 (64-bit pointers).
// =========================================================================
static constexpr uint8_t cie[] = {
    0x14, 0x00, 0x00, 0x00,  // Length = 20
    0x00, 0x00, 0x00, 0x00,  // CIE ID = 0 (marks this as a CIE)
    0x01,                    // Version = 1
    0x7a, 0x52, 0x00,        // Augmentation string "zR\0"
    0x04,                    // Code alignment factor = 4
    0x78,                    // Data alignment factor = -8 (SLEB128)
    0x1e,                    // Return address register = 30 (x30/LR)
    0x01,                    // Augmentation data length = 1
    0x1b,                    // FDE encoding: DW_EH_PE_pcrel | DW_EH_PE_sdata4
    0x0c, 0x1f, 0x00,        // DW_CFA_def_cfa: r31 (sp), offset 0
    0x00, 0x00, 0x00, 0x00,  // DW_CFA_nop x4 (pad to 8-byte aligned entry)
};
static constexpr size_t cie_size = sizeof(cie);  // 24

// =========================================================================
// Bytecode FDE (36 bytes)
//
// Covers 28 bytes of instructions (7 x 4-byte AArch64 insns; the 8-byte
// literal pool at offset 28 is data, not executable, so PC range = 28).
//
// Slot layout: [code(36) | CIE(24) | FDE(36) | terminator(4)]
//   CIE pointer = 28  (cie_size + 4 = distance from this field to CIE start)
//   PC begin    = -68  (pcrel from field at slot+68 back to code at slot+0)
//   PC range    = 28   (instructions only)
//
// CFI describes (code_alignment = 4, data_alignment = -8):
//   offset  0:  stp x29,x30,[sp,#-32]! -> CFA=sp+32, x29 at CFA-32, x30 at CFA-24
//   offset  4:  mov x29,sp             -> CFA=x29+32
//   offset  8-16: body (str, ldr, blr) — CFA=x29+32 unchanged
//   offset 20: ldp x29,x30,[sp],#32    -> (restores, but rule change at ret)
//   offset 24: ret                      -> CFA=sp+0, x29/x30 restored
// =========================================================================
static constexpr uint8_t bytecode_fde[] = {
    0x20, 0x00, 0x00, 0x00,  // Length = 32
    0x1c, 0x00, 0x00, 0x00,  // CIE pointer = 28
    0xbc, 0xff, 0xff, 0xff,  // PC begin = -68 (sdata4, little-endian)
    0x1c, 0x00, 0x00, 0x00,  // PC range = 28
    0x00,                    // Augmentation data length = 0
    0x41,                    // DW_CFA_advance_loc: 1  (-> offset 4, after stp)
    0x0e, 0x20,              // DW_CFA_def_cfa_offset: 32
    0x9d, 0x04,              // DW_CFA_offset: r29 (x29/fp), factored 4 -> at CFA-32
    0x9e, 0x03,              // DW_CFA_offset: r30 (x30/lr), factored 3 -> at CFA-24
    0x41,                    // DW_CFA_advance_loc: 1  (-> offset 8, after mov x29,sp)
    0x0d, 0x1d,              // DW_CFA_def_cfa_register: r29 (x29)
    0x44,                    // DW_CFA_advance_loc: 4  (-> offset 24, after ldp+ret)
    0x0c, 0x1f, 0x00,        // DW_CFA_def_cfa: r31 (sp), offset 0
    0xdd,                    // DW_CFA_restore: r29
    0xde,                    // DW_CFA_restore: r30
    0x00, 0x00, 0x00,        // DW_CFA_nop x3 (pad to 4-byte alignment)
};
static constexpr size_t bytecode_fde_size = sizeof(bytecode_fde);  // 36

// =========================================================================
// GF FDE (36 bytes)
//
// Covers 24 bytes of instructions (6 x 4-byte AArch64 insns; the 8-byte
// literal pool at offset 24 is data, not executable, so PC range = 24).
//
// Slot layout: [code(32) | CIE(24) | FDE(36) | terminator(4)]
//   CIE pointer = 28  (same formula: cie_size + 4)
//   PC begin    = -64  (pcrel from field at slot+64 back to code at slot+0)
//   PC range    = 24
//
// CFI describes:
//   offset  0:  stp x29,x30,[sp,#-16]! -> CFA=sp+16, x29 at CFA-16, x30 at CFA-8
//   offset  4:  mov x29,sp             -> CFA=x29+16
//   offset  8-12: body (ldr, blr)      — CFA=x29+16 unchanged
//   offset 16: ldp x29,x30,[sp],#16    -> (restores, but rule change at ret)
//   offset 20: ret                      -> CFA=sp+0, x29/x30 restored
// =========================================================================
static constexpr uint8_t gf_fde[] = {
    0x20, 0x00, 0x00, 0x00,  // Length = 32
    0x1c, 0x00, 0x00, 0x00,  // CIE pointer = 28
    0xc0, 0xff, 0xff, 0xff,  // PC begin = -64 (sdata4, little-endian)
    0x18, 0x00, 0x00, 0x00,  // PC range = 24
    0x00,                    // Augmentation data length = 0
    0x41,                    // DW_CFA_advance_loc: 1  (-> offset 4, after stp)
    0x0e, 0x10,              // DW_CFA_def_cfa_offset: 16
    0x9d, 0x02,              // DW_CFA_offset: r29, factored 2 -> at CFA-16
    0x9e, 0x01,              // DW_CFA_offset: r30, factored 1 -> at CFA-8
    0x41,                    // DW_CFA_advance_loc: 1  (-> offset 8, after mov x29,sp)
    0x0d, 0x1d,              // DW_CFA_def_cfa_register: r29
    0x43,                    // DW_CFA_advance_loc: 3  (-> offset 20, after ldp+ret)
    0x0c, 0x1f, 0x00,        // DW_CFA_def_cfa: r31 (sp), offset 0
    0xdd,                    // DW_CFA_restore: r29
    0xde,                    // DW_CFA_restore: r30
    0x00, 0x00, 0x00,        // DW_CFA_nop x3 (pad to 4-byte alignment)
};
static constexpr size_t gf_fde_size = sizeof(gf_fde);  // 36

// Slot total sizes (code + CIE + FDE + 4-byte zero terminator, padded to 16):
//   Bytecode: 36 + 24 + 36 + 4 = 100 -> padded to 112
//   GF:       32 + 24 + 36 + 4 =  96 -> already aligned

}  // namespace trampoline_aarch64
}  // namespace llvmo

#endif  // __aarch64__
