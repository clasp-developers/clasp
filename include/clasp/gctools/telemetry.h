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

    struct Telemetry {
        typedef size_t Header;
        typedef size_t Handle;
        bool _Write;
        FILE* _File;
        size_t _ThisRecordPos;
        size_t _Mask;
        std::vector<std::string> _Labels;
        std::map<std::string,Handle> _LabelsToHandles;

        typedef uintptr_t Word;
        const int StringBufferSize = 256;

        const Header intern_header = 0xBEF4;
        const Header data_header = 0xDADA;
        static const size_t GC_telemetry = 0x01;
    
        Telemetry() : _File(NULL), _ThisRecordPos(0), _Mask(0) {
            this->initialize();
        }

        void open_write(const char* file_name) {
            this->_File = fopen(file_name,"wb");
            this->_Write = true;
        }

        void open_read(const char* file_name) {
            this->_File = fopen(file_name,"rb");
            this->_Write = false;
        }

        void initialize() {
            char* intern = "INTERN";
            this->_Labels.push_back(std::string(intern));
            this->_LabelsToHandles[intern] = 0;
        }

        void set_mask(size_t mask) {
            this->_Mask = mask;
        }
    
        void flush() {
            if (this->_File) fflush(this->_File);
        }

        void close() {
            if ( this->_File) fclose(this->_File);
        }
    
        Handle intern(const std::string& label) {
            std::map<std::string,Handle>::iterator it = this->_LabelsToHandles.find(label);
            if ( label.size() >= StringBufferSize ) {
                printf("%s:%d The internd string is too long: %s\n", __FILE__, __LINE__, label.c_str() );
                abort();
            }
            Handle handle;
            if (it == this->_LabelsToHandles.end() ) {
                handle = this->_Labels.size();
                this->_LabelsToHandles[label] = handle;
                this->_Labels.push_back(std::string(label));
                if ( this->_Write ) {
                    this->write_header(intern_header);
                    this->write_label(label);
                    this->write_footer();
                }
            } else {
                handle = it->second;
            }
            return handle;
        }

        void write_header( Header header ) {
            this->_ThisRecordPos = ftell(this->_File);
            fwrite(&header,sizeof(Header),1,this->_File);
        }

        bool read_header( Header& header )
        {
            if (feof(this->_File)) {
                printf("%s:%d Hit end of telemetry\n", __FILE__, __LINE__ );
                return false;
            }
            size_t read = fread(&header,sizeof(Header),1,this->_File);
            if (read != 1 ) return false;
            return true;
        }

        void write_footer() {
            fwrite(&this->_ThisRecordPos,sizeof(this->_ThisRecordPos),1,this->_File);
        }

        size_t read_footer() {
            size_t prev;
            fread(&prev,sizeof(this->_ThisRecordPos),1,this->_File);
            return prev;
        }

        void write_handle(Handle l)
        {
            fwrite(&l,sizeof(l),1,this->_File);
        }

        void read_handle(Handle& handle)
        {
            fread(&handle,sizeof(handle),1,this->_File);
        }
        void write_label(const std::string& label) {
            size_t size = label.size();
            std::stringstream buffer;
            buffer << label << std::string(StringBufferSize,' ');
            fwrite(&size,sizeof(size_t),1,this->_File); // number of bytes in string
            fwrite(buffer.str().c_str(),StringBufferSize,1,this->_File); // string without zero terminator
        }

        std::string read_label() {
            size_t size;
            fread(&size,sizeof(size_t),1,this->_File); // read number of bytes in string
            std::string s = std::string(StringBufferSize,' ');
            fread((void*)s.data(),StringBufferSize,1,this->_File);
            std::string label = s.substr(0,size);
            return label;
        }

        std::string entry_as_string(Handle label, size_t num, Word data[] );

        void write(size_t kind, Handle label ) {
            if ( this->_Mask & kind ) {
                this->write_header(data_header);
                this->write_handle(label);
                size_t size = 0;
                fwrite(&size,sizeof(size_t),1,this->_File);
                this->write_footer();
            }
        }

        void write(size_t kind, Handle label, Word data0 ) {
            if ( this->_Mask & kind ) {
                this->write_header(data_header);
                this->write_handle(label);
                size_t size = 1;
                fwrite(&size,sizeof(size_t),1,this->_File);
                fwrite(&data0,sizeof(Word),1,this->_File);
                this->write_footer();
            }
        }

        void write(size_t kind, Handle label, Word data0, Word data1 ) {
            if ( this->_Mask & kind ) {
                this->write_header(data_header);
                this->write_handle(label);
                size_t size = 2;
                fwrite(&size,sizeof(size_t),1,this->_File);
                fwrite(&data0,sizeof(Word),1,this->_File);
                fwrite(&data1,sizeof(Word),1,this->_File);
                this->write_footer();
            }
        }

        void write(size_t kind, Handle label, Word data0, Word data1, Word data2 ) {
            if ( this->_Mask & kind ) {
                this->write_header(data_header);
                this->write_handle(label);
                size_t size = 3;
                fwrite(&size,sizeof(size_t),1,this->_File);
                fwrite(&data0,sizeof(Word),1,this->_File);
                fwrite(&data1,sizeof(Word),1,this->_File);
                fwrite(&data2,sizeof(Word),1,this->_File);
                this->write_footer();
            }
        }

    
        bool process_header( Header header) {
            if ( header == intern_header ) {
                std::string label = this->read_label();
                this->read_footer();
                this->intern(label.c_str());
                return true;
            } else if (header == data_header) {
                return false;
            }
        }
    
        // Return false if no more record are available
        size_t read_data( Handle& label, size_t num, Word* words ) {
            fread(&label,sizeof(Handle),1,this->_File);
            size_t real_num;
            fread(&real_num,sizeof(size_t),1,this->_File);
            size_t read_num = (real_num < num) ? real_num : num;
            // Read what will fit.
            fread(words,sizeof(Word),read_num,this->_File);
            // Read the rest of them
            for ( int i(read_num); i<real_num; ++i ) {
                Word dummy;
                fread(&dummy, sizeof(size_t),1,this->_File);
            }
            this->read_footer();
            return read_num;
        }

        void seek(size_t pos) {
            fseek(this->_File,pos,SEEK_SET);
        }
    };

    extern char* global_clasp_telemetry_file;
    extern Telemetry global_telemetry;

    extern Telemetry::Handle label_allocation;
    extern Telemetry::Handle label_obj_pad;
    extern Telemetry::Handle label_obj_scan_start;
    extern Telemetry::Handle label_obj_scan;
    extern Telemetry::Handle label_obj_isfwd_true;
    extern Telemetry::Handle label_obj_isfwd_false;
    extern Telemetry::Handle label_obj_pad;
    extern Telemetry::Handle label_obj_skip;
    extern Telemetry::Handle label_obj_fwd;
    extern Telemetry::Handle label_root_scan_start;
    extern Telemetry::Handle label_root_scan_stop;
    extern Telemetry::Handle label_smart_ptr_fix;
    extern Telemetry::Handle label_tagged_pointer_fix;
    extern Telemetry::Handle label_obj_finalize;

    void initialize_telemetry();
    void initialize_telemetry_defuns();
};
#endif
