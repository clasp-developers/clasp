/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    cfun_dispatch.c -- Trampolines for functions
*/

static cl_object dispatch0 (cl_narg narg) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 0)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed();
}

static cl_object dispatch1 (cl_narg narg, cl_object x0) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 1)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0);
}

static cl_object dispatch2 (cl_narg narg, cl_object x0, cl_object x1) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 2)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1);
}

static cl_object dispatch3 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 3)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2);
}

static cl_object dispatch4 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 4)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3);
}

static cl_object dispatch5 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 5)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4);
}

static cl_object dispatch6 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 6)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5);
}

static cl_object dispatch7 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 7)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6);
}

static cl_object dispatch8 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 8)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7);
}

static cl_object dispatch9 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 9)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8);
}

static cl_object dispatch10 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 10)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9);
}

static cl_object dispatch11 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 11)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10);
}

static cl_object dispatch12 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 12)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11);
}

static cl_object dispatch13 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 13)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12);
}

static cl_object dispatch14 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 14)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13);
}

static cl_object dispatch15 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 15)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14);
}

static cl_object dispatch16 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 16)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15);
}

static cl_object dispatch17 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 17)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16);
}

static cl_object dispatch18 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 18)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17);
}

static cl_object dispatch19 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 19)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18);
}

static cl_object dispatch20 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 20)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19);
}

static cl_object dispatch21 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 21)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20);
}

static cl_object dispatch22 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 22)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21);
}

static cl_object dispatch23 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 23)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22);
}

static cl_object dispatch24 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 24)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23);
}

static cl_object dispatch25 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 25)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24);
}

static cl_object dispatch26 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 26)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25);
}

static cl_object dispatch27 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 27)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26);
}

static cl_object dispatch28 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 28)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27);
}

static cl_object dispatch29 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 29)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28);
}

static cl_object dispatch30 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 30)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29);
}

static cl_object dispatch31 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 31)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30);
}

static cl_object dispatch32 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 32)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31);
}

static cl_object dispatch33 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 33)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32);
}

static cl_object dispatch34 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 34)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33);
}

static cl_object dispatch35 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 35)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34);
}

static cl_object dispatch36 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 36)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35);
}

static cl_object dispatch37 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 37)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36);
}

static cl_object dispatch38 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 38)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37);
}

static cl_object dispatch39 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 39)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38);
}

static cl_object dispatch40 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 40)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39);
}

static cl_object dispatch41 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 41)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40);
}

static cl_object dispatch42 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 42)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41);
}

static cl_object dispatch43 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 43)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42);
}

static cl_object dispatch44 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 44)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43);
}

static cl_object dispatch45 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 45)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44);
}

static cl_object dispatch46 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 46)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45);
}

static cl_object dispatch47 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 47)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46);
}

static cl_object dispatch48 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 48)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47);
}

static cl_object dispatch49 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 49)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48);
}

static cl_object dispatch50 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 50)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49);
}

static cl_object dispatch51 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 51)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50);
}

static cl_object dispatch52 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 52)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51);
}

static cl_object dispatch53 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 53)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52);
}

static cl_object dispatch54 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52, cl_object x53) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 54)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53);
}

static cl_object dispatch55 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52, cl_object x53, cl_object x54) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 55)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54);
}

static cl_object dispatch56 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52, cl_object x53, cl_object x54, cl_object x55) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 56)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55);
}

static cl_object dispatch57 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52, cl_object x53, cl_object x54, cl_object x55, cl_object x56) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 57)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56);
}

static cl_object dispatch58 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52, cl_object x53, cl_object x54, cl_object x55, cl_object x56, cl_object x57) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 58)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57);
}

static cl_object dispatch59 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52, cl_object x53, cl_object x54, cl_object x55, cl_object x56, cl_object x57, cl_object x58) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 59)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58);
}

static cl_object dispatch60 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52, cl_object x53, cl_object x54, cl_object x55, cl_object x56, cl_object x57, cl_object x58, cl_object x59) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 60)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59);
}

static cl_object dispatch61 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52, cl_object x53, cl_object x54, cl_object x55, cl_object x56, cl_object x57, cl_object x58, cl_object x59, cl_object x60) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 61)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60);
}

static cl_object dispatch62 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52, cl_object x53, cl_object x54, cl_object x55, cl_object x56, cl_object x57, cl_object x58, cl_object x59, cl_object x60, cl_object x61) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 62)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61);
}

static cl_object dispatch63 (cl_narg narg, cl_object x0, cl_object x1, cl_object x2, cl_object x3, cl_object x4, cl_object x5, cl_object x6, cl_object x7, cl_object x8, cl_object x9, cl_object x10, cl_object x11, cl_object x12, cl_object x13, cl_object x14, cl_object x15, cl_object x16, cl_object x17, cl_object x18, cl_object x19, cl_object x20, cl_object x21, cl_object x22, cl_object x23, cl_object x24, cl_object x25, cl_object x26, cl_object x27, cl_object x28, cl_object x29, cl_object x30, cl_object x31, cl_object x32, cl_object x33, cl_object x34, cl_object x35, cl_object x36, cl_object x37, cl_object x38, cl_object x39, cl_object x40, cl_object x41, cl_object x42, cl_object x43, cl_object x44, cl_object x45, cl_object x46, cl_object x47, cl_object x48, cl_object x49, cl_object x50, cl_object x51, cl_object x52, cl_object x53, cl_object x54, cl_object x55, cl_object x56, cl_object x57, cl_object x58, cl_object x59, cl_object x60, cl_object x61, cl_object x62) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 63)) FEwrong_num_arguments(fun);
  return fun->cfunfixed.entry_fixed(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62);
}

static cl_objectfn dispatch_table[ECL_C_ARGUMENTS_LIMIT+1] = {
(cl_objectfn)dispatch0, 
(cl_objectfn)dispatch1, 
(cl_objectfn)dispatch2, 
(cl_objectfn)dispatch3, 
(cl_objectfn)dispatch4, 
(cl_objectfn)dispatch5, 
(cl_objectfn)dispatch6, 
(cl_objectfn)dispatch7, 
(cl_objectfn)dispatch8, 
(cl_objectfn)dispatch9, 
(cl_objectfn)dispatch10, 
(cl_objectfn)dispatch11, 
(cl_objectfn)dispatch12, 
(cl_objectfn)dispatch13, 
(cl_objectfn)dispatch14, 
(cl_objectfn)dispatch15, 
(cl_objectfn)dispatch16, 
(cl_objectfn)dispatch17, 
(cl_objectfn)dispatch18, 
(cl_objectfn)dispatch19, 
(cl_objectfn)dispatch20, 
(cl_objectfn)dispatch21, 
(cl_objectfn)dispatch22, 
(cl_objectfn)dispatch23, 
(cl_objectfn)dispatch24, 
(cl_objectfn)dispatch25, 
(cl_objectfn)dispatch26, 
(cl_objectfn)dispatch27, 
(cl_objectfn)dispatch28, 
(cl_objectfn)dispatch29, 
(cl_objectfn)dispatch30, 
(cl_objectfn)dispatch31, 
(cl_objectfn)dispatch32, 
(cl_objectfn)dispatch33, 
(cl_objectfn)dispatch34, 
(cl_objectfn)dispatch35, 
(cl_objectfn)dispatch36, 
(cl_objectfn)dispatch37, 
(cl_objectfn)dispatch38, 
(cl_objectfn)dispatch39, 
(cl_objectfn)dispatch40, 
(cl_objectfn)dispatch41, 
(cl_objectfn)dispatch42, 
(cl_objectfn)dispatch43, 
(cl_objectfn)dispatch44, 
(cl_objectfn)dispatch45, 
(cl_objectfn)dispatch46, 
(cl_objectfn)dispatch47, 
(cl_objectfn)dispatch48, 
(cl_objectfn)dispatch49, 
(cl_objectfn)dispatch50, 
(cl_objectfn)dispatch51, 
(cl_objectfn)dispatch52, 
(cl_objectfn)dispatch53, 
(cl_objectfn)dispatch54, 
(cl_objectfn)dispatch55, 
(cl_objectfn)dispatch56, 
(cl_objectfn)dispatch57, 
(cl_objectfn)dispatch58, 
(cl_objectfn)dispatch59, 
(cl_objectfn)dispatch60, 
(cl_objectfn)dispatch61, 
(cl_objectfn)dispatch62, 
(cl_objectfn)dispatch63};
