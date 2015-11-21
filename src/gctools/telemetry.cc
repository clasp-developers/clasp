#include <clasp/gctools/telemetry.h>
#include <clasp/core/foundation.h>
#include <clasp/core/str.h>
#include <clasp/core/pathname.h>
#include <clasp/core/wrappers.h>

namespace telemetry {

Telemetry *global_telemetry_search = NULL;

void throw_if_invalid_global_telemetry_search() {
  if (global_telemetry_search == NULL) {
    SIMPLE_ERROR(BF("No global_telemetry_search has been defined - use telemetry-open"));
  }
}

#define ARGS_core_telemetry_open "(pathname)"
#define DECL_core_telemetry_open ""
#define DOCS_core_telemetry_open ""
void core_telemetry_open(core::T_sp pathname) {
  core::Str_sp filename = core::cl_namestring(pathname);
  global_telemetry_search = new Telemetry();
  global_telemetry_search->open_read(filename->c_str());
}
#define MAX_WORDS 16

#define CANONICAL_POINTER(p) (p & (~0x7))

#define ARGS_core_telemetry_search "(addresses)"
#define DECL_core_telemetry_search ""
#define DOCS_core_telemetry_search ""
void core_telemetry_search(core::List_sp addresses) {
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

#define ARGS_core_telemetry_search_labels "(label &optional (begin 0) end)"
#define DECL_core_telemetry_search_labels ""
#define DOCS_core_telemetry_search_labels ""
void core_telemetry_search_labels(core::List_sp labels) {
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

#define ARGS_core_telemetry_follow "(address)"
#define DECL_core_telemetry_follow ""
#define DOCS_core_telemetry_follow ""
void core_telemetry_follow(core::T_sp address) {
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

#define ARGS_core_telemetry_labels "()"
#define DECL_core_telemetry_labels ""
#define DOCS_core_telemetry_labels ""
void core_telemetry_labels() {
  throw_if_invalid_global_telemetry_search();
  for (int i(0); i < global_telemetry_search->_Labels.size(); ++i) {
    printf("[%d] %s\n", i, global_telemetry_search->_Labels[i].c_str());
  }
}

#define ARGS_core_telemetry_dump "(&optional (begin 0) end)"
#define DECL_core_telemetry_dump ""
#define DOCS_core_telemetry_dump ""
void core_telemetry_dump(core::T_sp begin, core::T_sp end) {
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
  }
}

#define ARGS_core_telemetry_count "()"
#define DECL_core_telemetry_count ""
#define DOCS_core_telemetry_count ""
size_t core_telemetry_count() {
  throw_if_invalid_global_telemetry_search();
  global_telemetry_search->seek0();
  size_t prev, cur;
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
  }
  return global_telemetry_search->_Index;
}

char *global_clasp_telemetry_file;
Telemetry *global_telemetry = NULL;

std::string Telemetry::entry_as_string(Handle label, size_t num_read, Word data[]) {
  std::string slabel = global_telemetry_search->_Labels[label];
  char buffer[1024];
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
    sprintf(buffer, "Add support for %d arguments", num_read);
  }
  stringstream ss;
  ss << "[" << this->_Index << "] fp: " << std::hex << this->_ThisRecordPos << " : " << buffer;
  return ss.str();
}

void Telemetry::initialize() {
  this->intern("telemetry undefined label", label_undefined);
  this->intern("mps_allocation base @%p client@%p client_end@%p kind: %lu", label_allocation);
  this->intern("obj_pad base@%p size: %lu", label_obj_pad);
  this->intern("obj_scan_start client@%p limit@%p", label_obj_scan_start);
  this->intern("obj_scan client@%p after_client@%p kind: %lu", label_obj_scan);
  this->intern("obj_isfwd == TRUE client@%p base@%p forward@%p", label_obj_isfwd_true);
  this->intern("obj_isfwd == FALSE client@%p base@%p", label_obj_isfwd_false);
  this->intern("obj_skip in-client@%p  out-client@%p size=%lu", label_obj_skip);
  this->intern("obj_fwd old-client@%p new-client@%p", label_obj_fwd);
  this->intern("obj_finalize addr@%p", label_obj_finalize);
  this->intern("root_scan_start", label_root_scan_start);
  this->intern("root_scan_stop", label_root_scan_stop);
  this->intern("smart_ptr_fix ptr@%p value before@%p after@%p", label_smart_ptr_fix);
  this->intern("tagged_pointer_fix ptr@%p value before@%p after@%p", label_tagged_pointer_fix);
  this->intern("Message address: %p value: %lu", label_msg);
  this->intern("label_stack_frame_scan_start base@%p limit@%p", label_stack_frame_scan_start);
  this->intern("label_stack_frame_scan base@%p base_end@%p type=%lu", label_stack_frame_scan);
  this->intern("label_stack_frame_skip base@%p base_end@%p size: %lu", label_stack_frame_skip);
  this->intern("label_stack_frame_pad  base@%p size: %lu", label_stack_frame_pad);
  this->intern("label_stack_push_prepare ap@%p ap->init@%p ap->alloc@%p ap->limit@%p ap->_frameptr@%p ap->_enabled:%lu ap->_lwpoppending:%lu", label_stack_push_prepare);
  this->intern("label_stack_push ap@%p frame@%p depth:%lu", label_stack_push);
  this->intern("label_stack_allocate alloc@%p size: %lu", label_stack_allocate);
  this->intern("label_stack_pop ap@%p frame@%p", label_stack_pop);
  this->intern("obj_deallocate_unmanaged_instance addr@%p", label_obj_deallocate_unmanaged_instance);
};

void initialize_telemetry_functions() {
  CoreDefun(telemetry_open);
  CoreDefun(telemetry_search);
  CoreDefun(telemetry_search_labels);
  CoreDefun(telemetry_labels);
  CoreDefun(telemetry_dump);
  CoreDefun(telemetry_count);
  CoreDefun(telemetry_follow);
}
};

extern "C" {
void global_telemetry_flush() {
  telemetry::global_telemetry->flush();
};
};
