#include <clasp/core/foundation.h>
#include <clasp/core/pathname.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/telemetry.h>

namespace telemetry {

void throw_if_invalid_global_telemetry_search() {
  if (global_telemetry_search == NULL) {
    SIMPLE_ERROR(BF("No global_telemetry_search has been defined - use telemetry-open"));
  }
}

CL_LAMBDA(pathname);
CL_DECLARE();
CL_DOCSTRING("");
CL_DEFUN void core__telemetry_open(core::T_sp tpathname) {
  if (tpathname.nilp()) SIMPLE_ERROR(BF("%s was about to pass nil to pathname") % __FUNCTION__);
  core::Pathname_sp pathname = core::cl__pathname(tpathname);
  core::String_sp filename = core::cl__namestring(pathname);
  global_telemetry_search = new Telemetry();
  global_telemetry_search->open_read(filename->get_std_string().c_str());
  if (global_telemetry_search->_File == NULL ) {
    printf("Could not open file: %s\n", _rep_(pathname).c_str());
  }
}
#define MAX_WORDS 16

#define CANONICAL_POINTER(p) (p & (~0x7))

CL_LAMBDA(addresses);
CL_DECLARE();
CL_DOCSTRING("");
CL_DEFUN void core__telemetry_search(core::List_sp addresses) {
  throw_if_invalid_global_telemetry_search();
  global_telemetry_search->seek0();
  size_t prev, cur;
  Telemetry::Header header;
  std::vector<std::string> results;
  std::vector<Word> tests;
  for (auto it : addresses) {
    core::T_sp address = oCar(it);
    if (!address.fixnump()) {
      SIMPLE_ERROR(BF("Inputs must all be fixnums"));
    }
    tests.push_back(address.unsafe_fixnum() & (~0x7));
  }
  Handle label;
  Word data[MAX_WORDS];
  while (1) {
    bool read = global_telemetry_search->read_header(header);
    if (!read)
      break;
    if (global_telemetry_search->process_header(header))
      continue;
    size_t num_read = global_telemetry_search->read_data(label, MAX_WORDS, data);
    for (int i(0); i < num_read; ++i) {
      for (auto it : tests) {
        if (it == CANONICAL_POINTER(data[i])) {
          std::string entry = global_telemetry_search->entry_as_string(label, num_read, data);
          results.push_back(entry);
        }
      }
    }
  }
  for (auto &it : results) {
    printf("%s:%d  %s\n", __FILE__, __LINE__, it.c_str());
  }
}

CL_LAMBDA(label &optional (begin 0) end);
CL_DECLARE();
CL_DOCSTRING("");
CL_DEFUN void core__telemetry_search_labels(core::List_sp labels) {
  throw_if_invalid_global_telemetry_search();
  global_telemetry_search->seek0();
  size_t prev, cur;
  Telemetry::Header header;
  std::vector<std::string> results;
  std::vector<Word> tests;
  for (auto it : labels) {
    core::T_sp address = oCar(it);
    if (!address.fixnump()) {
      SIMPLE_ERROR(BF("Inputs must all be fixnums"));
    }
    tests.push_back(address.unsafe_fixnum() & (~0x7));
  }
  Handle label;
  Word data[MAX_WORDS];
  size_t index = 0, pos;
  while (1) {
    bool read = global_telemetry_search->read_header(header);
    if (!read)
      break;
    if (global_telemetry_search->process_header(header))
      continue;
    size_t num_read = global_telemetry_search->read_data(label, MAX_WORDS, data);
    for (int i(0); i < num_read; ++i) {
      for (auto it : tests) {
        if (it == label) {
          std::string entry = global_telemetry_search->entry_as_string(label, num_read, data);
          results.push_back(entry);
        }
      }
    }
  }
  for (auto &it : results) {
    printf("%s:%d  %s\n", __FILE__, __LINE__, it.c_str());
  }
}

CL_LAMBDA(address);
CL_DECLARE();
CL_DOCSTRING("");
CL_DEFUN void core__telemetry_follow(core::T_sp address) {
  throw_if_invalid_global_telemetry_search();
  global_telemetry_search->seek0();
  size_t prev, cur;
  Telemetry::Header header;
  std::vector<std::string> results;
  std::vector<Word> tests;
  if (!address.fixnump()) {
    SIMPLE_ERROR(BF("Input must be fixnum"));
  }
  tests.push_back(address.unsafe_fixnum() & (~0x7));
  Handle label;
  Word data[MAX_WORDS];
  while (1) {
    bool read = global_telemetry_search->read_header(header);
    if (!read)
      break;
    if (global_telemetry_search->process_header(header))
      continue;
    size_t num_read = global_telemetry_search->read_data(label, MAX_WORDS, data);
    for (auto it : tests) {
      if (label == label_obj_fwd) {
        if (it == CANONICAL_POINTER(data[0])) {
          tests.push_back(data[1]);
          goto SAVE_RESULT;
        }
      }
      for (int i(0); i < num_read; ++i) {
        if (it == CANONICAL_POINTER(data[i])) {
          goto SAVE_RESULT;
        }
      }
    }
    continue;
  SAVE_RESULT:
    std::string entry = global_telemetry_search->entry_as_string(label, num_read, data);
    results.push_back(entry);
  }
  for (auto &it : results) {
    printf("%s:%d  %s\n", __FILE__, __LINE__, it.c_str());
  }
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("");
CL_DEFUN void core__telemetry_labels() {
  throw_if_invalid_global_telemetry_search();
  for (int i(0); i < global_telemetry_search->_Labels.size(); ++i) {
    printf("[%d] %s\n", i, global_telemetry_search->_Labels[i].c_str());
  }
}

CL_LAMBDA(&optional (begin 0) end);
CL_DECLARE();
CL_DOCSTRING("");
CL_DEFUN void core__telemetry_dump(core::T_sp begin, core::T_sp end) {
  throw_if_invalid_global_telemetry_search();
  if (!begin.fixnump()) {
    SIMPLE_ERROR(BF("begin must be a FIXNUM"));
  }
  core::Fixnum fn_begin = begin.unsafe_fixnum();
  core::Fixnum fn_end;
  if (end.nilp()) {
    fn_end = gctools::most_positive_uint;
  } else if (end.fixnump()) {
    fn_end = end.unsafe_fixnum();
  } else {
    SIMPLE_ERROR(BF("Illegal value for end"));
  }
  global_telemetry_search->seek0();
  size_t cur;
  Telemetry::Header header;
  Handle label;
  Word data[MAX_WORDS];
  while (1) {
    bool read = global_telemetry_search->read_header(header);
    if (!read)
      break;
    if (global_telemetry_search->process_header(header))
      continue;
    size_t num_read = global_telemetry_search->read_data(label, MAX_WORDS, data);
    if (global_telemetry_search->_Index < fn_begin)
      continue;
    if (global_telemetry_search->_Index > fn_end)
      break;
    std::string entry = global_telemetry_search->entry_as_string(label, num_read, data);
    printf("%s\n", entry.c_str());
    if ((global_telemetry_search->_Index % 1000000) == 0 ) {
      gctools::poll_signals();
      printf("%s:%d Searching record index %" PRu " at file offset %lu\n", __FILE__, __LINE__, global_telemetry_search->_Index, global_telemetry_search->_ThisRecordPos);
    }
  }
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("");
CL_DEFUN size_t core__telemetry_count() {
  throw_if_invalid_global_telemetry_search();
  global_telemetry_search->seek0();
  size_t prev, cur;
  Telemetry::Header header;
  Handle label;
  Word data[MAX_WORDS];
  while (1) {
    bool read = global_telemetry_search->read_header(header);
    if (!read) break;
    if (global_telemetry_search->process_header(header)) continue;
    size_t num_read = global_telemetry_search->read_data(label, MAX_WORDS, data);
    if ((global_telemetry_search->_Index % 1000000) == 0 ) {
      gctools::poll_signals();
      printf("%s:%d Searching record index %" PRu " at file offset %lu\n", __FILE__, __LINE__, global_telemetry_search->_Index, global_telemetry_search->_ThisRecordPos);
    }
  }
  return global_telemetry_search->_Index;
}

char *global_clasp_telemetry_file;


void Telemetry::dump_entry_varargs(Handle label, size_t num, ... )
{
  Word data[8];
  va_list arguments;
  va_start(arguments,num);
  for ( int x = 0; x<num; ++x ) {
    data[x] = va_arg(arguments,Word);
  }
  va_end(arguments);
  std::string msg = this->entry_as_string(label,num,data);
  printf("%s\n", msg.c_str());
}

std::string Telemetry::entry_as_string(Handle label, size_t num_read, Word data[]) {
  std::string slabel = global_telemetry_search->_Labels[label];
  char buffer[1024];
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wformat-security"
  switch (num_read) {
  case 0:
    sprintf(buffer, slabel.c_str());
    break;
  case 1:
    sprintf(buffer, slabel.c_str(), data[0]);
    break;
  case 2:
    sprintf(buffer, slabel.c_str(), data[0], data[1]);
    break;
  case 3:
    sprintf(buffer, slabel.c_str(), data[0], data[1], data[2]);
    break;
  case 4:
    sprintf(buffer, slabel.c_str(), data[0], data[1], data[2], data[3]);
    break;
  case 5:
    sprintf(buffer, slabel.c_str(), data[0], data[1], data[2], data[3], data[4]);
    break;
  case 6:
    sprintf(buffer, slabel.c_str(), data[0], data[1], data[2], data[3], data[4], data[5]);
    break;
  case 7:
    sprintf(buffer, slabel.c_str(), data[0], data[1], data[2], data[3], data[4], data[5], data[6]);
    break;
  default:
    sprintf(buffer, "Add support for %zu arguments", num_read);
  }
#pragma clang diagnostic pop
  stringstream ss;
  ss << "[" << this->_Index << "] fp: " << std::hex << this->_ThisRecordPos << " : " << buffer;
  return ss.str();
}

void Telemetry::initialize() {
  this->intern("telemetry undefined label", label_undefined);
  this->intern("mps_allocation base @%p client@%p client_end@%p kind: %" PRu "", label_allocation);
  this->intern("obj_pad base@%p size: %" PRu "", label_obj_pad);
  this->intern("obj_scan_start client@%p limit@%p", label_obj_scan_start);
  this->intern("obj_scan client@%p after_client@%p kind: %" PRu "", label_obj_scan);
  this->intern("obj_isfwd == TRUE client@%p base@%p forward@%p", label_obj_isfwd_true);
  this->intern("obj_isfwd == FALSE client@%p base@%p", label_obj_isfwd_false);
  this->intern("obj_skip in-client@%p  out-client@%p size=%" PRu "", label_obj_skip);
  this->intern("obj_fwd old-client@%p new-client@%p", label_obj_fwd);
  this->intern("obj_finalize addr@%p", label_obj_finalize);
  this->intern("root_scan_start", label_root_scan_start);
  this->intern("root_scan_stop", label_root_scan_stop);
  this->intern("smart_ptr_fix ptr@%p value before@%p after@%p", label_smart_ptr_fix);
  this->intern("tagged_pointer_fix ptr@%p value before@%p after@%p", label_tagged_pointer_fix);
  this->intern("Message address: %p value: %" PRu "", label_msg);
  this->intern("label_stack_frame_scan_start base@%p limit@%p", label_stack_frame_scan_start);
  this->intern("label_stack_frame_scan base@%p base_end@%p type=%" PRu "", label_stack_frame_scan);
  this->intern("label_stack_frame_skip base@%p base_end@%p size: %" PRu "", label_stack_frame_skip);
  this->intern("label_stack_frame_pad  base@%p size: %" PRu "", label_stack_frame_pad);
  this->intern("label_stack_push_prepare ap@%p ap->init@%p ap->alloc@%p ap->limit@%p ap->_frameptr@%p ap->_enabled:%" PRu " ap->_lwpoppending:%lu", label_stack_push_prepare);
  this->intern("label_stack_push ap@%p frame@%p depth:%" PRu "", label_stack_push);
  this->intern("label_stack_allocate alloc@%p size: %" PRu "", label_stack_allocate);
  this->intern("label_stack_pop ap@%p frame@%p", label_stack_pop);
  this->intern("obj_deallocate_unmanaged_instance addr@%p", label_obj_deallocate_unmanaged_instance);
  this->intern("cons_mps_allocation base @%p client@%p client_end@%p kind: %" PRu "", label_cons_allocation);
    this->intern("cons_pad base@%p size: %" PRu "", label_cons_pad);
  this->intern("cons_scan_start client@%p limit@%p", label_cons_scan_start);
  this->intern("cons_scan client@%p after_client@%p kind: %" PRu "", label_cons_scan);
  this->intern("cons_isfwd == TRUE client@%p base@%p forward@%p", label_cons_isfwd_true);
  this->intern("cons_isfwd == FALSE client@%p base@%p", label_cons_isfwd_false);
  this->intern("cons_skip in-client@%p  out-client@%p size=%" PRu "", label_cons_skip);
  this->intern("cons_fwd old-client@%p new-client@%p", label_cons_fwd);

};

void initialize_telemetry_functions() {
}
};

extern "C" {
void global_telemetry_flush() {
#ifdef DEBUG_TELEMETRY
  telemetry::global_telemetry_search->flush();
#endif
};
};
