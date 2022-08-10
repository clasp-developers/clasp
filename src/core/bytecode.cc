
#define DEBUG_LEVEL_FULL

//#include <llvm/Support/system_error.h>
#include <unistd.h>
#include <dlfcn.h>
#include <iomanip>
#include <cstdint>
#include <clasp/core/foundation.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/bytecode.h>
#include <clasp/core/array.h>
#include <clasp/core/virtualMachine.h>
#include <clasp/core/primitives.h> // cl__fdefinition

namespace core {


void BytecodeModule_O::initialize() {
  this->_Literals = ComplexVector_T_O::make(16);
  this->_Bytecode = ComplexVector_byte8_t_O::make(1024,0,true,core::make_fixnum(0),nil<core::T_O>(),false, core::make_fixnum(0));
}

CL_DEFMETHOD
BytecodeModule_O::Literals_sp_Type BytecodeModule_O::literals() const {
  return this->_Literals;
}

CL_DEFMETHOD
void BytecodeModule_O::setf_literals(BytecodeModule_O::Literals_sp_Type o) {
  this->_Literals = o;
}

CL_DEFMETHOD
BytecodeModule_O::Bytecode_sp_Type BytecodeModule_O::bytecode() const {
  return this->_Bytecode;
}

CL_DEFMETHOD
void BytecodeModule_O::setf_bytecode(BytecodeModule_O::Bytecode_sp_Type o) {
  this->_Bytecode = o;
}

gctools::return_type bytecode_call(unsigned char* pc, core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args)
{
  ClosureWithSlots_O* closure = gctools::untag_general<ClosureWithSlots_O*>((ClosureWithSlots_O*)lcc_closure);
  // Do we need the lookup entryPoint? - if so - maybe we should pass it to bytecode_call
  // Meh, it's just a lookup and we are going to read closed over slots
  // which should be in the same cache line.
  core::GlobalBytecodeEntryPoint_sp entryPoint = gctools::As_unsafe<core::GlobalBytecodeEntryPoint_sp>(closure->_EntryPoint.load());
  printf("%s:%d:%s This is where we evaluate bytecode functions pc: %p\n", __FILE__, __LINE__, __FUNCTION__, pc );
  size_t nlocals = entryPoint->localsFrameSize();
  BytecodeModule_sp module = entryPoint->code();
  SimpleVector_sp literals = gc::As<SimpleVector_sp>(module->literals());

  VirtualMachine& vm = my_thread->_VM;
  vm.push_frame(nlocals);
  while (1) {
    switch (*pc) {
    case vm_ref: // 0 ref
        printf("ref %hu\n", *(pc+1));
        vm.push(*(vm.reg(*(++pc))));
        pc++;
        break;
    case vm_const: // 1 constant
        printf("const %hu\n", *(pc+1));
        vm.push(literals->rowMajorAref(*(++pc)).raw_());
        pc++;
        break;
    case vm_closure: // 2 closure
        printf("closure %hu\n", *(pc+1));
        vm.push((*closure)[*(++pc)].raw_());
        pc++;
        break;
    case vm_call: {
      printf("call %hu\n", *(pc+1));
      size_t nargs = *(++pc);
      T_O* func = *(vm.stackref(nargs));
      T_O** args = vm.stackref(nargs-1);
      T_mv res = funcall_general<core::Function_O>((gc::Tagged)func, nargs, args);
      res.saveToMultipleValue0();
      vm.drop(nargs+1);
      pc++;
      break;
    }
    case vm_call_receive_one: {
      printf("call-receive-one %hu\n", *(pc+1));
      size_t nargs = *(++pc);
      T_O* func = *(vm.stackref(nargs));
      T_O** args = vm.stackref(nargs-1);
      T_sp res = funcall_general<core::Function_O>((gc::Tagged)func, nargs, args);
      vm.drop(nargs+1);
      vm.push(res.raw_());
      pc++;
      break;
    }
        /*
    case vm_call_receive_fixed:
*/
    case vm_bind: {
      printf("bind %hu %hu\n", *(pc+1), *(pc+2));
      size_t nelems = *(++pc);
      size_t base = *(++pc);
      vm.copytoreg(vm.stackref(nelems-1), nelems, base);
      vm.drop(nelems);
      pc++;
      break;
    }
    case vm_set:
        printf("set %hu\n", *(pc+1));
        vm.copytoreg(vm.stackref(0), 1, *(++pc));
        vm.drop(1);
        pc++;
        break;
    case vm_make_cell: {
      printf("make-cell\n");
      T_sp car((gctools::Tagged)(vm.pop()));
      T_sp cdr((gctools::Tagged)nil<T_O*>);
      vm.push(Cons_O::create(car, cdr).raw_());
      pc++;
      break;
    }
    case vm_cell_ref: {
      printf("cell-ref\n");
      T_sp cons((gctools::Tagged)vm.pop());
      vm.push(oCar(cons).raw_());
      pc++;
      break;
    }
    case vm_cell_set: {
      printf("cell-set\n");
      T_O* val = vm.pop();
      T_sp tval((gctools::Tagged)val);
      T_sp cons((gctools::Tagged)vm.pop());
      CONS_CAR(cons) = tval;
      pc++;
      break;
    }
    // 11 is closure
    case vm_return: { // 12 return
      printf("return\n");
      size_t numValues = vm.npushed(nlocals);
      printf("  numValues = %zu\n", numValues);
      if (numValues == 0) { // Return mv register as-is.
        vm.pop_frame(nlocals);
        core::MultipleValues &mv = core::lisp_multipleValues();
        size_t nvalues = mv.getSize();
        return gctools::return_type(mv.valueGet(0, nvalues).raw_(), nvalues);
      } else {
        vm.copyto(numValues - 1, &my_thread->_MultipleValues._Values[1]);
        vm.drop(numValues - 1);
        T_O* primary = vm.pop();
        vm.pop_frame(nlocals);
        return gctools::return_type(primary, numValues);
      }
    }
    case vm_bind_required_args:
        printf("bind-required-args %hu\n", *(pc+1));
        vm.copytoreg(lcc_args, *(++pc), 0);
        pc++;
        break;
    // bind-optional-args 14 listify-rest-args 15 parse-key-args 16
    case vm_jump: // 17 jump
        printf("jump %hu\n", *(pc+1));
        pc += *(++pc);
        break;
    case vm_jump_if: { // 18 jump-if
      printf("jump-if %hu\n", *(pc+1));
      T_sp tval((gctools::Tagged)vm.pop());
      if (tval.notnilp()) pc += *(++pc);
      else pc += 2;
      break;
    }
    // jump-if-supplied
    case vm_check_arg_count_LE_: {
      printf("check-arg-count<= %hu\n", *(pc+1));
      size_t max_nargs = *(++pc);
      if (lcc_nargs > max_nargs) {
        T_sp tclosure((gctools::Tagged)lcc_closure);
        throwTooManyArgumentsError(tclosure, lcc_nargs, max_nargs);
      }
      pc++;
      break;
    }
    case vm_check_arg_count_GE_: {
      printf("check-arg-count>= %hu\n", *(pc+1));
      size_t min_nargs = *(++pc);
      if (lcc_nargs < min_nargs) {
        T_sp tclosure((gctools::Tagged)lcc_closure);
        throwTooFewArgumentsError(tclosure, lcc_nargs, min_nargs);
      }
      pc++;
      break;
    }
    case vm_check_arg_count_EQ_: {
      printf("check-arg-count= %hu\n", *(pc+1));
      size_t req_nargs = *(++pc);
      if (lcc_nargs != req_nargs) {
        T_sp tclosure((gctools::Tagged)lcc_closure);
        wrongNumberOfArguments(tclosure, lcc_nargs, req_nargs);
      }
      pc++;
      break;
    }
    case vm_fdefinition: { // 36 fdefinition
      printf("fdefinition\n");
      vm.push(cl__fdefinition(literals->rowMajorAref(*(++pc))).raw_());
      pc++;
      break;
    }
    case vm_nil: // 37 nil
        printf("nil\n");
        vm.push(nil<T_O>().raw_());
        pc++;
        break;
    default:
        SIMPLE_ERROR("Unknown opcode %hu", *pc);
    };
  }
}


};

