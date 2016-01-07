#ifndef telemetry_H
#define telemetry_H

#include <cstddef>
#include <stdio.h>
#include <vector>
#include <sstream>
#include <map>
#include <cstring>
#include <string>
namespace telemetry {

typedef uintptr_t Handle;
typedef uintptr_t Word;
constexpr int StringBufferSize = 256;
constexpr Handle label_undefined = 0; // must be zero
constexpr Handle label_allocation = 1;
constexpr Handle label_obj_pad = 2;
constexpr Handle label_obj_scan_start = 3;
constexpr Handle label_obj_scan = 4;
constexpr Handle label_obj_isfwd_true = 5;
constexpr Handle label_obj_isfwd_false = 6;
constexpr Handle label_obj_skip = 7;
constexpr Handle label_obj_fwd = 8;
constexpr Handle label_obj_finalize = 9;
constexpr Handle label_root_scan_start = 10;
constexpr Handle label_root_scan_stop = 11;
constexpr Handle label_smart_ptr_fix = 12;
constexpr Handle label_tagged_pointer_fix = 13;
constexpr Handle label_msg = 14;
constexpr Handle label_stack_frame_scan_start = 15;
constexpr Handle label_stack_frame_scan = 16;
constexpr Handle label_stack_frame_skip = 17;
constexpr Handle label_stack_frame_pad = 18;
constexpr Handle label_stack_push_prepare = 19;
constexpr Handle label_stack_push = 20;
constexpr Handle label_stack_allocate = 21;
constexpr Handle label_stack_pop = 22;
constexpr Handle label_obj_deallocate_unmanaged_instance = 23;

struct Telemetry {
  typedef size_t Header;
  typedef size_t Handle;
  bool _Write;
  FILE *_File;
  size_t _Index;
  size_t _ThisRecordPos;
  size_t _Mask;
  std::vector<std::string> _Labels;
  std::map<std::string, Handle> _LabelsToHandles;

  const Header intern_header = 0xBEF4;
  const Header data_header = 0xDADA;
  static const size_t GC_telemetry = 0x01;
  static const size_t Message_telemetry = 0x02;
  static const size_t STACK_telemetry = 0x04;

  Telemetry() : _File(NULL), _ThisRecordPos(0), _Mask(0) {
    this->initialize();
  }

  void open_write(const char *file_name) {
    this->_File = fopen(file_name, "wb");
    this->_Write = true;
  }

  void open_read(const char *file_name) {
    this->_File = fopen(file_name, "rb");
    this->_Write = false;
  }

  void initialize();

  void set_mask(size_t mask) {
    this->_Mask = mask;
  }

  void flush() {
    if (this->_File)
      fflush(this->_File);
  }

  void close() {
    if (this->_File)
      fclose(this->_File);
  }

  Handle intern(const std::string &label, size_t predefined_handle = 0) {
    std::map<std::string, Handle>::iterator it = this->_LabelsToHandles.find(label);
    if (label.size() >= StringBufferSize) {
      printf("%s:%d The internd string is too long: %s\n", __FILE__, __LINE__, label.c_str());
      abort();
    }
    Handle handle;
    if (it == this->_LabelsToHandles.end()) {
      handle = this->_Labels.size();
      this->_LabelsToHandles[label] = handle;
      this->_Labels.push_back(std::string(label));
      if (predefined_handle == 0 && this->_Write) {
        this->write_header(intern_header);
        this->write_label(label);
        this->write_footer();
      }
    } else {
      handle = it->second;
    }
    if (predefined_handle != 0 && handle != predefined_handle) {
      printf("%s:%d The predefined_handle[%lu] doesn't match the handle[%lu] assigned - they must match or telemetry won't work - this is for label: %s\n", __FILE__, __LINE__, predefined_handle, handle, label.c_str());
      abort();
    }
    return handle;
  }

  void write_header(Header header) {
    this->_ThisRecordPos = ftell(this->_File);
    fwrite(&header, sizeof(Header), 1, this->_File);
  }

  bool read_header(Header &header) {
    this->_ThisRecordPos = ftell(this->_File);
    ++this->_Index;
    if (feof(this->_File)) {
      printf("%s:%d Hit end of telemetry\n", __FILE__, __LINE__);
      return false;
    }
    size_t read = fread(&header, sizeof(Header), 1, this->_File);
    if (read != 1)
      return false;
    return true;
  }

  void write_footer() {
    fwrite(&this->_ThisRecordPos, sizeof(this->_ThisRecordPos), 1, this->_File);
  }

  size_t read_footer() {
    size_t prev;
    fread(&prev, sizeof(this->_ThisRecordPos), 1, this->_File);
    return prev;
  }

  void write_handle(Handle l) {
    fwrite(&l, sizeof(l), 1, this->_File);
  }

  void read_handle(Handle &handle) {
    fread(&handle, sizeof(handle), 1, this->_File);
  }
  void write_label(const std::string &label) {
    size_t size = label.size();
    std::stringstream buffer;
    buffer << label << std::string(StringBufferSize, ' ');
    fwrite(&size, sizeof(size_t), 1, this->_File);                  // number of bytes in string
    fwrite(buffer.str().c_str(), StringBufferSize, 1, this->_File); // string without zero terminator
  }

  std::string read_label() {
    size_t size;
    fread(&size, sizeof(size_t), 1, this->_File); // read number of bytes in string
    std::string s = std::string(StringBufferSize, ' ');
    fread((void *)s.data(), StringBufferSize, 1, this->_File);
    std::string label = s.substr(0, size);
    return label;
  }

  std::string entry_as_string(Handle label, size_t num, Word data[]);

  void write(size_t kind, Handle label) {
    if ((this->_Mask & kind) && this->_File) {
      this->write_header(data_header);
      this->write_handle(label);
      size_t size = 0;
      fwrite(&size, sizeof(size_t), 1, this->_File);
      this->write_footer();
    }
  }

  void write(size_t kind, Handle label, Word data0) {
    if ((this->_Mask & kind) && this->_File) {
      this->write_header(data_header);
      this->write_handle(label);
      size_t size = 1;
      fwrite(&size, sizeof(size_t), 1, this->_File);
      fwrite(&data0, sizeof(Word), 1, this->_File);
      this->write_footer();
    }
  }

  void write(size_t kind, Handle label, Word data0, Word data1) {
    if ((this->_Mask & kind) && this->_File) {
      this->write_header(data_header);
      this->write_handle(label);
      size_t size = 2;
      fwrite(&size, sizeof(size_t), 1, this->_File);
      fwrite(&data0, sizeof(Word), 1, this->_File);
      fwrite(&data1, sizeof(Word), 1, this->_File);
      this->write_footer();
    }
  }

  void write(size_t kind, Handle label, Word data0, const std::string &msg) {
    if ((this->_Mask & kind) && this->_File) {
      this->write_header(data_header);
      this->write_handle(label);
      size_t size = 2;
      fwrite(&size, sizeof(size_t), 1, this->_File);
      fwrite(&data0, sizeof(Word), 1, this->_File);
      Word data1 = 0;
      int i;
      for (i = 0; i < sizeof(Word) - 1; ++i) {
        ((char *)(&data1))[i] = msg[i];
      }
      fwrite(&data1, sizeof(Word), 1, this->_File);
      this->write_footer();
    }
  }

  void write(size_t kind, Handle label, Word data0, Word data1, Word data2) {
    if ((this->_Mask & kind) && this->_File) {
      this->write_header(data_header);
      this->write_handle(label);
      size_t size = 3;
      fwrite(&size, sizeof(size_t), 1, this->_File);
      fwrite(&data0, sizeof(Word), 1, this->_File);
      fwrite(&data1, sizeof(Word), 1, this->_File);
      fwrite(&data2, sizeof(Word), 1, this->_File);
      this->write_footer();
    }
  }

  void write(size_t kind, Handle label, Word d0, Word d1, Word d2, Word d3) {
    if ((this->_Mask & kind) && this->_File) {
      this->write_header(data_header);
      this->write_handle(label);
      size_t size = 4;
      fwrite(&size, sizeof(size_t), 1, this->_File);
      fwrite(&d0, sizeof(Word), 1, this->_File);
      fwrite(&d1, sizeof(Word), 1, this->_File);
      fwrite(&d2, sizeof(Word), 1, this->_File);
      fwrite(&d3, sizeof(Word), 1, this->_File);
      this->write_footer();
    }
  }

  void write(size_t kind, Handle label, Word d0, Word d1, Word d2, Word d3, Word d4) {
    if ((this->_Mask & kind) && this->_File) {
      this->write_header(data_header);
      this->write_handle(label);
      size_t size = 5;
      fwrite(&size, sizeof(size_t), 1, this->_File);
      fwrite(&d0, sizeof(Word), 1, this->_File);
      fwrite(&d1, sizeof(Word), 1, this->_File);
      fwrite(&d2, sizeof(Word), 1, this->_File);
      fwrite(&d3, sizeof(Word), 1, this->_File);
      fwrite(&d4, sizeof(Word), 1, this->_File);
      this->write_footer();
    }
  }

  void write(size_t kind, Handle label, Word d0, Word d1, Word d2, Word d3, Word d4, Word d5) {
    if ((this->_Mask & kind) && this->_File) {
      this->write_header(data_header);
      this->write_handle(label);
      size_t size = 6;
      fwrite(&size, sizeof(size_t), 1, this->_File);
      fwrite(&d0, sizeof(Word), 1, this->_File);
      fwrite(&d1, sizeof(Word), 1, this->_File);
      fwrite(&d2, sizeof(Word), 1, this->_File);
      fwrite(&d3, sizeof(Word), 1, this->_File);
      fwrite(&d4, sizeof(Word), 1, this->_File);
      fwrite(&d5, sizeof(Word), 1, this->_File);
      this->write_footer();
    }
  }

  void write(size_t kind, Handle label, Word d0, Word d1, Word d2, Word d3, Word d4, Word d5, Word d6) {
    if ((this->_Mask & kind) && this->_File) {
      this->write_header(data_header);
      this->write_handle(label);
      size_t size = 7;
      fwrite(&size, sizeof(size_t), 1, this->_File);
      fwrite(&d0, sizeof(Word), 1, this->_File);
      fwrite(&d1, sizeof(Word), 1, this->_File);
      fwrite(&d2, sizeof(Word), 1, this->_File);
      fwrite(&d3, sizeof(Word), 1, this->_File);
      fwrite(&d4, sizeof(Word), 1, this->_File);
      fwrite(&d5, sizeof(Word), 1, this->_File);
      fwrite(&d6, sizeof(Word), 1, this->_File);
      this->write_footer();
    }
  }

  bool process_header(Header header) {
    if (header == intern_header) {
      std::string label = this->read_label();
      this->read_footer();
      this->intern(label.c_str());
      return true;
    } else if (header == data_header) {
      return false;
    }
  }

  // Return false if no more record are available
  size_t read_data(Handle &label, size_t num, Word *words) {
    fread(&label, sizeof(Handle), 1, this->_File);
    size_t real_num;
    fread(&real_num, sizeof(size_t), 1, this->_File);
    size_t read_num = (real_num < num) ? real_num : num;
    // Read what will fit.
    fread(words, sizeof(Word), read_num, this->_File);
    // Read the rest of them
    for (int i(read_num); i < real_num; ++i) {
      Word dummy;
      fread(&dummy, sizeof(size_t), 1, this->_File);
    }
    this->read_footer();
    return read_num;
  }

  void seek0() {
    fseek(this->_File, 0, SEEK_SET);
    this->_Index = 0;
  }
};

extern char *global_clasp_telemetry_file;
extern Telemetry *global_telemetry;

void initialize_telemetry_functions();

#ifdef DEBUG_TELEMETRY
#define GC_TELEMETRY0(label) telemetry::global_telemetry->write(telemetry::Telemetry::GC_telemetry, label)
#define GC_TELEMETRY1(label, arg0) telemetry::global_telemetry->write(telemetry::Telemetry::GC_telemetry, label, (uintptr_t)arg0)
#define GC_TELEMETRY2(label, arg0, arg1) telemetry::global_telemetry->write(telemetry::Telemetry::GC_telemetry, label, (uintptr_t)arg0, (uintptr_t)arg1)
#define GC_TELEMETRY3(label, arg0, arg1, arg2) telemetry::global_telemetry->write(telemetry::Telemetry::GC_telemetry, label, (uintptr_t)arg0, (uintptr_t)arg1, (uintptr_t)arg2)
#define GC_TELEMETRY4(label, arg0, arg1, arg2, arg3) telemetry::global_telemetry->write(telemetry::Telemetry::GC_telemetry, label, (uintptr_t)arg0, (uintptr_t)arg1, (uintptr_t)arg2, (uintptr_t)arg3)
#else
#define GC_TELEMETRY0(label)
#define GC_TELEMETRY1(label, arg0)
#define GC_TELEMETRY2(label, arg0, arg1)
#define GC_TELEMETRY3(label, arg0, arg1, arg2)
#define GC_TELEMETRY4(label, arg0, arg1, arg2, arg3)
#endif
#ifdef DEBUG_STACK_TELEMETRY
#define STACK_TELEMETRY0(label) telemetry::global_telemetry->write(telemetry::Telemetry::STACK_telemetry, label)
#define STACK_TELEMETRY1(label, arg0) telemetry::global_telemetry->write(telemetry::Telemetry::STACK_telemetry, label, (uintptr_t)arg0)
#define STACK_TELEMETRY2(label, arg0, arg1) telemetry::global_telemetry->write(telemetry::Telemetry::STACK_telemetry, label, (uintptr_t)arg0, (uintptr_t)arg1)
#define STACK_TELEMETRY3(label, arg0, arg1, arg2) telemetry::global_telemetry->write(telemetry::Telemetry::STACK_telemetry, label, (uintptr_t)arg0, (uintptr_t)arg1, (uintptr_t)arg2)
#define STACK_TELEMETRY7(label, a0, a1, a2, a3, a4, a5, a6) telemetry::global_telemetry->write(telemetry::Telemetry::STACK_telemetry, label, (uintptr_t)a0, (uintptr_t)a1, (uintptr_t)a2, (uintptr_t)a3, (uintptr_t)a4, (uintptr_t)a5, (uintptr_t)a6)
#else
#define STACK_TELEMETRY0(label)
#define STACK_TELEMETRY1(label, arg0)
#define STACK_TELEMETRY2(label, arg0, arg1)
#define STACK_TELEMETRY3(label, arg0, arg1, arg2)
#define STACK_TELEMETRY7(label, a0, a1, a2, a3, a4, a5, a6)
#endif
};
#endif
