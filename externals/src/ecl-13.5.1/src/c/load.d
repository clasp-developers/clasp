/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    load.d -- Binary loader (contains also open_fasl_data).
*/
/*
    Copyright (c) 1990, Giuseppe Attardi and William F. Schelter.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>

#ifdef ENABLE_DLOPEN
cl_object
si_load_binary(cl_object filename, cl_object verbose,
               cl_object print, cl_object external_format)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object block, map, array;
	cl_object basename;
	cl_object init_prefix, prefix;
	cl_object output;

	/* We need the full pathname */
	filename = cl_truename(filename);

	/* Try to load shared object file */
	block = ecl_library_open(filename, 1);
	if (block->cblock.handle == NULL) {
		output = ecl_library_error(block);
		goto OUTPUT;
	}

	/* Fist try to call "init_CODE()" */
        init_prefix = _ecl_library_default_entry();
        block->cblock.entry =
                ecl_library_symbol(block, (char *)init_prefix->base_string.self, 0);
	if (block->cblock.entry != NULL)
		goto GO_ON;

	/* Next try to call "init_FILE()" where FILE is the file name */
	prefix = ecl_symbol_value(@'si::*init-function-prefix*');
        init_prefix = _ecl_library_init_prefix();
	if (Null(prefix)) {
                prefix = init_prefix;
        } else {
                prefix = @si::base-string-concatenate(3,
                                                      init_prefix,
                                                      prefix,
                                                      make_constant_base_string("_"));
        }
	basename = cl_pathname_name(1,filename);
	basename = @si::base-string-concatenate(2, prefix, @string-upcase(1, funcall(4, @'nsubstitute', ECL_CODE_CHAR('_'), ECL_CODE_CHAR('-'), basename)));
	block->cblock.entry = ecl_library_symbol(block, (char*)basename->base_string.self, 0);

	if (block->cblock.entry == NULL) {
		output = ecl_library_error(block);
		ecl_library_close(block);
		goto OUTPUT;
	}

GO_ON:
	/* Finally, perform initialization */
	ecl_init_module(block, (void (*)(cl_object))(block->cblock.entry));
	output = ECL_NIL;
OUTPUT:
	ecl_return1(the_env, output);
}
#endif /* !ENABLE_DLOPEN */

cl_object
si_load_source(cl_object source, cl_object verbose, cl_object print, cl_object external_format)
{
	cl_env_ptr the_env = ecl_process_env();
	cl_object x, strm;

	/* Source may be either a stream or a filename */
	if (ecl_t_of(source) != t_pathname && ecl_t_of(source) != t_base_string) {
		/* INV: if "source" is not a valid stream, file.d will complain */
		strm = source;
	} else {
		strm = ecl_open_stream(source, ecl_smm_input, ECL_NIL, ECL_NIL, 8,
				       ECL_STREAM_C_STREAM, external_format);
		if (Null(strm))
			@(return ECL_NIL)
	}
	ECL_UNWIND_PROTECT_BEGIN(the_env) {
		cl_object form_index = ecl_make_fixnum(0);
                cl_object pathname = ECL_SYM_VAL(the_env, @'*load-pathname*');
		cl_object location = CONS(pathname, form_index);
		ecl_bds_bind(the_env, @'ext::*source-location*', location);
		for (;;) {
                        form_index = ecl_file_position(strm);
                        ECL_RPLACD(location, form_index);
			x = si_read_object_or_ignore(strm, OBJNULL);
			if (x == OBJNULL)
				break;
                        if (the_env->nvalues) {
                                si_eval_with_env(1, x);
                                if (print != ECL_NIL) {
                                        @write(1, x);
                                        @terpri(0);
                                }
                        }
		}
		ecl_bds_unwind1(the_env);
	} ECL_UNWIND_PROTECT_EXIT {
		/* We do not want to come back here if close_stream fails,
		   therefore, first we frs_pop() current jump point, then
		   try to close the stream, and then jump to next catch
		   point */
		if (strm != source)
			cl_close(3, strm, @':abort', @'t');
	} ECL_UNWIND_PROTECT_END;
	@(return ECL_NIL)
}


cl_object
si_load_bytecodes(cl_object source, cl_object verbose, cl_object print, cl_object external_format)
{
	cl_env_ptr env = ecl_process_env();
	cl_object forms, strm;
	cl_object old_eptbc = env->packages_to_be_created;

	/* Source may be either a stream or a filename */
	if (ecl_t_of(source) != t_pathname && ecl_t_of(source) != t_base_string) {
		/* INV: if "source" is not a valid stream, file.d will complain */
		strm = source;
	} else {
		strm = ecl_open_stream(source, ecl_smm_input, ECL_NIL, ECL_NIL, 8,
				       ECL_STREAM_C_STREAM, external_format);
		if (Null(strm))
			@(return ECL_NIL)
	}
	ECL_UNWIND_PROTECT_BEGIN(env) {
                {
                cl_object progv_list = ECL_SYM_VAL(env, @'si::+ecl-syntax-progv-list+');
                cl_index bds_ndx = ecl_progv(env, ECL_CONS_CAR(progv_list),
                                             ECL_CONS_CDR(progv_list));
                env->packages_to_be_created_p = ECL_T;
                forms = cl_read(1, strm);
                env->packages_to_be_created_p = ECL_NIL;
                ecl_bds_unwind(env, bds_ndx);
                }
                while (!Null(forms)) {
                        if (ECL_LISTP(forms)) {
                                cl_object x = ECL_CONS_CAR(forms);
                                forms = ECL_CONS_CDR(forms);
                                if (ecl_t_of(x) == t_bytecodes) {
                                        _ecl_funcall1(x);
                                        continue;
                                }
                        }
                        FEerror("Corrupt bytecodes file ~S", 1, source);
                }
                {
                cl_object x;
                x = cl_set_difference(2, env->packages_to_be_created, old_eptbc);
                old_eptbc = env->packages_to_be_created;
                unlikely_if (!Null(x)) {
                        CEerror(ECL_T,
                                Null(ECL_CONS_CDR(x))?
                                "Package ~A referenced in "
				"compiled file~&  ~A~&but has not been created":
                                "The packages~&  ~A~&were referenced in "
				"compiled file~&  ~A~&but have not been created",
				2, x, source);
		}
                }
	} ECL_UNWIND_PROTECT_EXIT {
		/* We do not want to come back here if close_stream fails,
		   therefore, first we frs_pop() current jump point, then
		   try to close the stream, and then jump to next catch
		   point */
		if (strm != source)
			cl_close(3, strm, @':abort', @'t');
	} ECL_UNWIND_PROTECT_END;
	@(return ECL_NIL)
}

@(defun load (source
	      &key (verbose ecl_symbol_value(@'*load-verbose*'))
		   (print ecl_symbol_value(@'*load-print*'))
		   (if_does_not_exist @':error')
                   (external_format @':default')
	           (search_list ecl_symbol_value(@'si::*load-search-list*'))
	      &aux pathname pntype hooks filename function ok)
	bool not_a_filename = 0;
@
	/* If source is a stream, read conventional lisp code from it */
	if (ecl_t_of(source) != t_pathname && !ecl_stringp(source)) {
		/* INV: if "source" is not a valid stream, file.d will complain */
		filename = source;
		function = ECL_NIL;
		not_a_filename = 1;
		goto NOT_A_FILENAME;
	}
	/* INV: coerce_to_file_pathname() creates a fresh new pathname object */
	source   = cl_merge_pathnames(1, source);
	pathname = coerce_to_file_pathname(source);
	pntype   = pathname->pathname.type;

	filename = ECL_NIL;
	hooks = ecl_symbol_value(@'ext::*load-hooks*');
	if (Null(pathname->pathname.directory) &&
	    Null(pathname->pathname.host) &&
	    Null(pathname->pathname.device) &&
	    !Null(search_list))
	{
		loop_for_in(search_list) {
			cl_object d = CAR(search_list);
			cl_object f = cl_merge_pathnames(2, pathname, d);
			cl_object ok = cl_load(11, f, @':verbose', verbose,
					       @':print', print,
					       @':if-does-not-exist', ECL_NIL,
                                               @':external-format', external_format,
					       @':search-list', ECL_NIL);
			if (!Null(ok)) {
				@(return ok);
			}
		} end_loop_for_in;
	}
	if (!Null(pntype) && (pntype != @':wild')) {
		/* If filename already has an extension, make sure
		   that the file exists */
                cl_object kind;
		filename = si_coerce_to_filename(pathname);
                kind = si_file_kind(filename, ECL_T);
		if (kind != @':file' && kind != @':special') {
			filename = ECL_NIL;
		} else {
			function = cl_cdr(ecl_assoc(pathname->pathname.type, hooks));
		}
	} else loop_for_in(hooks) {
		/* Otherwise try with known extensions until a matching
		   file is found */
                cl_object kind;
		filename = pathname;
		filename->pathname.type = CAAR(hooks);
		function = CDAR(hooks);
                kind = si_file_kind(filename, ECL_T);
		if (kind == @':file' || kind == @':special')
			break;
		else
			filename = ECL_NIL;
	} end_loop_for_in;
	if (Null(filename)) {
		if (Null(if_does_not_exist))
			@(return ECL_NIL)
		else
			FEcannot_open(source);
	}
NOT_A_FILENAME:
	if (verbose != ECL_NIL) {
		cl_format(3, ECL_T, make_constant_base_string("~&;;; Loading ~s~%"),
			  filename);
	}
	ecl_bds_bind(the_env, @'*package*', ecl_symbol_value(@'*package*'));
	ecl_bds_bind(the_env, @'*readtable*', ecl_symbol_value(@'*readtable*'));
	ecl_bds_bind(the_env, @'*load-pathname*', not_a_filename? ECL_NIL : source);
	ecl_bds_bind(the_env, @'*load-truename*',
		     not_a_filename? ECL_NIL : (filename = cl_truename(filename)));
	if (!Null(function)) {
		ok = funcall(5, function, filename, verbose, print, external_format);
	} else {
#if 0 /* defined(ENABLE_DLOPEN) && !defined(ECL_MS_WINDOWS_HOST)*/
		/*
		 * DISABLED BECAUSE OF SECURITY ISSUES!
		 * In systems where we can do this, we try to load the file
		 * as a binary. When it fails, we will revert to source
		 * loading below. Is this safe? Well, it depends on whether
		 * your op.sys. checks integrity of binary exectables or
		 * just loads _anything_.
		 */
		if (not_a_filename) {
			ok = ECL_T;
		} else {
			ok = si_load_binary(filename, verbose, print);
		}
		if (!Null(ok))
#endif
		ok = si_load_source(filename, verbose, print, external_format);
	}
	ecl_bds_unwind_n(the_env, 4);
	if (!Null(ok))
		FEerror("LOAD: Could not load file ~S (Error: ~S)",
			2, filename, ok);
	if (print != ECL_NIL) {
		cl_format(3, ECL_T, make_constant_base_string("~&;;; Loading ~s~%"),
			  filename);
	}
	@(return filename)
@)
