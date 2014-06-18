/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    cdata.d -- Data for compiled files.
*/
/*
    Copyright (c) 2011, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <string.h>
#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>

#define HEADER_PREFIX "eClDaTa20110719"
#define HEADER_PREFIX_LENGTH 15

typedef struct {
        char code[16];
        cl_index offset, size;
} cdata_header;

ecl_def_ct_base_string(str_no_data,"",0,static,const);

cl_object
si_get_cdata(cl_object filename)
{
        cl_object map, array, displaced;
        cdata_header *header;
        map = si_mmap(3, filename, @':direction', @':input');
        array = si_mmap_array(map);
        {
                char *v = (char*)array->base_string.self
                        + array->base_string.dim
                        - sizeof(cdata_header);
                header = (cdata_header*)v;
                
        }
        if (memcmp(header->code, HEADER_PREFIX, HEADER_PREFIX_LENGTH)) {
		displaced = str_no_data;
        } else {
                displaced = cl_funcall(8, @'make-array',
                                       ecl_make_fixnum(header->size),
                                       @':element-type', @'base-char',
                                       @':displaced-to', array,
                                       @':displaced-index-offset',
                                       ecl_make_fixnum(header->offset));
        }
        @(return map displaced);
}

cl_object
si_add_cdata(cl_object filename, cl_object data)
{
        cl_object stream, offset;
        cdata_header header;

        data = si_copy_to_simple_base_string(data);
        stream = cl_open(9, filename,
                         @':element-type', @'base-char',
                         @':direction', @':output',
                         @':if-does-not-exist', @':error',
                         @':if-exists', @':append');
        offset = ecl_file_length(stream);
        ecl_file_position_set(stream, offset);
        cl_write_sequence(2, data, stream);
        memcpy(header.code, HEADER_PREFIX, HEADER_PREFIX_LENGTH);
        header.offset = fixnnint(offset);
        header.size = data->base_string.dim;
        {
                unsigned char *c = (unsigned char *)&header;
                int i;
                for (i = 0; i < sizeof(header); i++) {
                        ecl_write_byte(ecl_make_fixnum(c[i]), stream);
                }
        }
        cl_close(1, stream);
        @(return)
}
