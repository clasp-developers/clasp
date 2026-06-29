// Hardcoded x86_64 trampoline templates.
// One shared CIE, separate code+FDE per kind (bytecode vs GF).
//
// At init time: copy the code array, patch the 8-byte target address at the
// known offset, then hand code+CIE+FDE to the arena's install_template().

#pragma once
#include <cstdint>
#include <cstddef>

#if defined(__x86_64__)

namespace llvmo {
namespace trampoline_x86_64 {

// =========================================================================
// Bytecode trampoline (26 bytes)
// Signature: (i64 pc, ptr closure, i64 nargs, ptr args) -> {ptr, i64}
// Passes all args through; saves pc (%rdi) on stack for debugger visibility.
// Target address patched at offset 14 (8-byte immediate in movabs).
// =========================================================================
static constexpr uint8_t bytecode_code[] = {
    0x55,                                             // push   %rbp
    0x48, 0x89, 0xe5,                                 // mov    %rsp, %rbp
    0x48, 0x83, 0xec, 0x10,                           // sub    $16, %rsp        (maintain 16-byte alignment)
    0x48, 0x89, 0x7d, 0xf8,                           // mov    %rdi, -8(%rbp)   (save bytecode PC)
    0x48, 0xb8,                                       // movabs $imm64, %rax     (opcode prefix)
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  //   <-- 8-byte target address (patch here)
    0xff, 0xd0,                                       // call   *%rax
    0xc9,                                             // leave
    0xc3,                                             // ret
};
static constexpr size_t bytecode_code_size = sizeof(bytecode_code);  // 26
static constexpr size_t bytecode_target_offset = 14;

// =========================================================================
// GF trampoline (18 bytes)
// Signature: (ptr closure, i64 nargs, ptr args) -> {ptr, i64}
// Pure passthrough, no extra saves.
// Target address patched at offset 6 (8-byte immediate in movabs).
// =========================================================================
static constexpr uint8_t gf_code[] = {
    0x55,                                             // push   %rbp
    0x48, 0x89, 0xe5,                                 // mov    %rsp, %rbp
    0x48, 0xb8,                                       // movabs $imm64, %rax     (opcode prefix)
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  //   <-- 8-byte target address (patch here)
    0xff, 0xd0,                                       // call   *%rax
    0x5d,                                             // pop    %rbp
    0xc3,                                             // ret
};
static constexpr size_t gf_code_size = sizeof(gf_code);  // 18
static constexpr size_t gf_target_offset = 6;

// =========================================================================
// CIE (24 bytes) — shared between both trampoline kinds.
//
// Augmentation "zR": FDE pointers use pcrel|sdata4 encoding (0x1b).
// No personality, no LSDA — bytes are fully position-independent.
// Initial rules: CFA = rsp+8, return address (r16/rip) at CFA-8.
// =========================================================================
static constexpr uint8_t cie[] = {
    0x14, 0x00, 0x00, 0x00,  // Length = 20
    0x00, 0x00, 0x00, 0x00,  // CIE ID = 0 (marks this as a CIE)
    0x01,                    // Version = 1
    0x7a, 0x52, 0x00,        // Augmentation string "zR\0"
    0x01,                    // Code alignment factor = 1
    0x78,                    // Data alignment factor = -8 (SLEB128)
    0x10,                    // Return address register = 16 (rip)
    0x01,                    // Augmentation data length = 1
    0x1b,                    // FDE encoding: DW_EH_PE_pcrel | DW_EH_PE_sdata4
    0x0c, 0x07, 0x08,        // DW_CFA_def_cfa: r7 (rsp), offset 8
    0x90, 0x01,              // DW_CFA_offset: r16 (rip), factored offset 1 → at CFA-8
    0x00, 0x00,              // DW_CFA_nop × 2 (pad to 8-byte aligned entry)
};
static constexpr size_t cie_size = sizeof(cie);  // 24

// =========================================================================
// Bytecode FDE (32 bytes)
//
// Covers 26 bytes of code.
// Slot layout: [code(26) | CIE(24) | FDE(32) | terminator(4)]
//   CIE pointer = 28  (cie_size + 4 = distance from this field to CIE start)
//   PC begin    = -58  (pcrel from field at slot+58 back to code at slot+0)
//   PC range    = 26
//
// CFI describes:
//   offset 0:  push %rbp        → CFA=rsp+16, rbp at CFA-16
//   offset 1:  mov %rsp,%rbp    → CFA=rbp+16
//   offset 4–24: body (CFA=rbp+16 unchanged; sub+mov don't change CFA)
//   offset 25: ret (after leave) → CFA=rsp+8, rbp restored
// =========================================================================
static constexpr uint8_t bytecode_fde[] = {
    0x1c, 0x00, 0x00, 0x00,  // Length = 28
    0x1c, 0x00, 0x00, 0x00,  // CIE pointer = 28
    0xc6, 0xff, 0xff, 0xff,  // PC begin = -58 (sdata4, little-endian)
    0x1a, 0x00, 0x00, 0x00,  // PC range = 26
    0x00,                    // Augmentation data length = 0
    0x41,                    // DW_CFA_advance_loc: 1   (after push %rbp)
    0x0e, 0x10,              // DW_CFA_def_cfa_offset: 16
    0x86, 0x02,              // DW_CFA_offset: r6 (rbp), factored offset 2 → at CFA-16
    0x43,                    // DW_CFA_advance_loc: 3   (after mov %rsp, %rbp)
    0x0d, 0x06,              // DW_CFA_def_cfa_register: r6 (rbp)
    0x55,                    // DW_CFA_advance_loc: 21  (to ret, after leave)
    0x0c, 0x07, 0x08,        // DW_CFA_def_cfa: r7 (rsp), offset 8
    0xc6,                    // DW_CFA_restore: r6 (rbp)
    0x00, 0x00,              // DW_CFA_nop × 2 (padding)
};
static constexpr size_t bytecode_fde_size = sizeof(bytecode_fde);  // 32

// =========================================================================
// GF FDE (32 bytes)
//
// Covers 18 bytes of code.
// Slot layout: [code(18) | CIE(24) | FDE(32) | terminator(4)]
//   CIE pointer = 28  (same formula: cie_size + 4)
//   PC begin    = -50  (pcrel from field at slot+50 back to code at slot+0)
//   PC range    = 18
//
// CFI describes:
//   offset 0:  push %rbp        → CFA=rsp+16, rbp at CFA-16
//   offset 1:  mov %rsp,%rbp    → CFA=rbp+16
//   offset 4–16: body (CFA=rbp+16 unchanged)
//   offset 17: ret (after pop %rbp) → CFA=rsp+8, rbp restored
// =========================================================================
static constexpr uint8_t gf_fde[] = {
    0x1c, 0x00, 0x00, 0x00,  // Length = 28
    0x1c, 0x00, 0x00, 0x00,  // CIE pointer = 28
    0xce, 0xff, 0xff, 0xff,  // PC begin = -50 (sdata4, little-endian)
    0x12, 0x00, 0x00, 0x00,  // PC range = 18
    0x00,                    // Augmentation data length = 0
    0x41,                    // DW_CFA_advance_loc: 1   (after push %rbp)
    0x0e, 0x10,              // DW_CFA_def_cfa_offset: 16
    0x86, 0x02,              // DW_CFA_offset: r6 (rbp), factored offset 2 → at CFA-16
    0x43,                    // DW_CFA_advance_loc: 3   (after mov %rsp, %rbp)
    0x0d, 0x06,              // DW_CFA_def_cfa_register: r6 (rbp)
    0x4d,                    // DW_CFA_advance_loc: 13  (to ret, after pop %rbp)
    0x0c, 0x07, 0x08,        // DW_CFA_def_cfa: r7 (rsp), offset 8
    0xc6,                    // DW_CFA_restore: r6 (rbp)
    0x00, 0x00,              // DW_CFA_nop × 2 (padding)
};
static constexpr size_t gf_fde_size = sizeof(gf_fde);  // 32

// Slot total sizes (code + CIE + FDE + 4-byte zero terminator, padded to 16):
//   Bytecode: 26 + 24 + 32 + 4 = 86 → padded to 96
//   GF:       18 + 24 + 32 + 4 = 78 → padded to 80

}  // namespace trampoline_x86_64
}  // namespace llvmo

#endif  // __x86_64__
