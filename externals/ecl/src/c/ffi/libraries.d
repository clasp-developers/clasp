/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    load.d -- Shared library and bundle opening / copying / closing
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
/*
 * Choosing the interface for loading binary files. Currently we recognize
 * three different methods:
 *	- Windows API, provided by ECL_MS_WINDOWS_HOST
 *	- dlopen, provided HAVE_DLFCN_H is defined
 *	- NSLinkModule, provided HAVE_MACH_O_DYLD_H is defined
 * They are chosen in this precise order. In order to make the code for these
 * methods mutually exclusive, when one method is present, the other macros
 * get undefined. Handling of dynamically loaded libraries is constrained to
 * this file and thus the changes can be limited to this file.
 */
#ifdef ECL_MS_WINDOWS_HOST
# ifdef HAVE_DLFCN_H
#  undef HAVE_DLFCN_H
# endif
# ifdef HAVE_MACH_O_DYLD_H
#  undef HAVE_MACH_O_DYLD_H
# endif
#endif
#ifdef HAVE_DLFCN_H
# ifdef HAVE_MACH_O_DYLD_H
#  undef HAVE_MACH_O_DYLD_H
# endif
#endif
#ifdef ENABLE_DLOPEN
# ifdef cygwin
#  include <w32api/windows.h>
#  include <sys/stat.h>
# endif
# ifdef HAVE_DLFCN_H
#  include <dlfcn.h>
#  define INIT_PREFIX "init_fas_"
#  ifdef bool
#   undef bool
#  endif
# endif
# ifdef HAVE_MACH_O_DYLD_H
#  include <mach-o/dyld.h>
#  define INIT_PREFIX "_init_fas_"
# endif
# ifdef HAVE_LINK_H
#  include <link.h>
# endif
# if defined(ECL_MS_WINDOWS_HOST)
#  include <windows.h>
#  include <windef.h>
#  include <winbase.h>
#  include <tlhelp32.h>
#  define INIT_PREFIX "init_fas_"
# else
#  include <unistd.h>
# endif
#endif /* ENABLE_DLOPEN */
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>

#define GC_call_with_alloc_lock(f,arg) f(arg)

cl_object
ecl_make_codeblock()
{
        cl_object block = ecl_alloc(t_codeblock);
        block = ecl_alloc_object(t_codeblock);
        block->cblock.self_destruct = 0;
        block->cblock.locked = 0;
        block->cblock.handle = NULL;
        block->cblock.data = NULL;
        block->cblock.data_size = 0;
        block->cblock.temp_data = NULL;
        block->cblock.temp_data_size = 0;
        block->cblock.data_text = NULL;
        block->cblock.next = ECL_NIL;
        block->cblock.name = ECL_NIL;
        block->cblock.links = ECL_NIL;
        block->cblock.cfuns_size = 0;
        block->cblock.cfuns = NULL;
        block->cblock.source = ECL_NIL;
	block->cblock.error = ECL_NIL;
        block->cblock.refs = ecl_make_fixnum(0);
        si_set_finalizer(block, ECL_T);
        return block;
}

static cl_object
copy_object_file(cl_object original)
{
	int err;
	cl_object copy = make_constant_base_string("TMP:ECL");
	copy = si_coerce_to_filename(si_mkstemp(copy));
        /*
         * We either have to make a full copy to convince the loader to load this object
         * file again, or we want to retain the possibility of overwriting the object
         * file we load later on (case of Windows, which locks files that are loaded).
         * The symlinks do not seem to work in latest versions of Linux.
         */
#if defined(ECL_MS_WINDOWS_HOST)
	ecl_disable_interrupts();
	err = !CopyFile(original->base_string.self, copy->base_string.self, 0);
	ecl_enable_interrupts();
	if (err) {
		FEwin32_error("Error when copying file from~&~3T~A~&to~&~3T~A",
			      2, original, copy);
	}
#else
	err = Null(si_copy_file(original, copy));
	if (err) {
		FEerror("Error when copying file from~&~3T~A~&to~&~3T~A",
			2, original, copy);
	}
#endif
#ifdef cygwin
	{
		cl_object new_copy = make_constant_base_string(".dll");
		new_copy = si_base_string_concatenate(2, copy, new_copy);
		cl_rename_file(2, copy, new_copy);
		copy = new_copy;
	}
	ecl_disable_interrupts();
	err = chmod(copy->base_string.self, S_IRWXU) < 0;
	ecl_enable_interrupts();
	if (err) {
		FElibc_error("Unable to give executable permissions to ~A",
			     1, copy);
	}
#endif
	return copy;
}

#ifdef ENABLE_DLOPEN

static void
set_library_error(cl_object block) {
	cl_object output;
	ecl_disable_interrupts();
#ifdef HAVE_DLFCN_H
	output = make_base_string_copy(dlerror());
#endif
#ifdef HAVE_MACH_O_DYLD_H
	{
		NSLinkEditErrors c;
		int number;
		const char *filename;
		NSLinkEditError(&c, &number, &filename, &message);
		output = make_base_string_copy(message);
	}
#endif
#if defined(ECL_MS_WINDOWS_HOST)
	{
		const char *message;
		FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM |
			      FORMAT_MESSAGE_ALLOCATE_BUFFER,
			      0, GetLastError(), 0, (void*)&message, 0, NULL);
		output = make_base_string_copy(message);
		LocalFree(message);
	}
#endif
	ecl_enable_interrupts();
	block->cblock.error = output;
}

static void
dlopen_wrapper(cl_object block)
{
	cl_object filename = block->cblock.name;
        char *filename_string = (char*)filename->base_string.self;
#ifdef HAVE_DLFCN_H
	block->cblock.handle = dlopen(filename_string, RTLD_NOW|RTLD_GLOBAL);
#endif
#ifdef HAVE_MACH_O_DYLD_H
	{
	NSObjectFileImage file;
        static NSObjectFileImageReturnCode code;
	code = NSCreateObjectFileImageFromFile(filename_string, &file);
	if (code != NSObjectFileImageSuccess) {
		block->cblock.handle = NULL;
	} else {
		NSModule out = NSLinkModule(file, filename_string,
					    NSLINKMODULE_OPTION_PRIVATE|
					    NSLINKMODULE_OPTION_BINDNOW|
					    NSLINKMODULE_OPTION_RETURN_ON_ERROR);
		block->cblock.handle = out;
	}}
#endif
#if defined(ECL_MS_WINDOWS_HOST)
	block->cblock.handle = LoadLibrary(filename_string);
#endif
	if (block->cblock.handle == NULL)
		set_library_error(block);
}

static void
dlclose_wrapper(cl_object block)
{
        if (block->cblock.handle != NULL) {
#ifdef HAVE_DLFCN_H
                dlclose(block->cblock.handle);
#endif
#ifdef HAVE_MACH_O_DYLD_H
                NSUnLinkModule(block->cblock.handle, NSUNLINKMODULE_OPTION_NONE);
#endif
#if defined(ECL_MS_WINDOWS_HOST)
                FreeLibrary(block->cblock.handle);
#endif
                block->cblock.handle = NULL;
        }
}

static cl_object
ecl_library_find_by_name(cl_object filename)
{
	cl_object l;
	for (l = cl_core.libraries; l != ECL_NIL; l = ECL_CONS_CDR(l)) {
		cl_object other = ECL_CONS_CAR(l);
		cl_object name = other->cblock.name;
		if (!Null(name) && ecl_string_eq(name, filename)) {
			return other;
		}
	}
	return ECL_NIL;
}

static cl_object
ecl_library_find_by_handle(void *handle)
{
	cl_object l;
	for (l = cl_core.libraries; l != ECL_NIL; l = ECL_CONS_CDR(l)) {
		cl_object other = ECL_CONS_CAR(l);
		if (handle == other->cblock.handle) {
			return other;
		}
	}
	return ECL_NIL;
}

static cl_object
ecl_library_open_inner(cl_object filename, bool self_destruct)
{
        const cl_env_ptr the_env = ecl_process_env();
	cl_object block = ecl_make_codeblock();
	block->cblock.self_destruct = self_destruct;
	block->cblock.name = filename;
        block->cblock.refs = ecl_make_fixnum(1);

        ECL_WITH_GLOBAL_LOCK_BEGIN(the_env) {
	ecl_disable_interrupts();
        GC_call_with_alloc_lock(dlopen_wrapper, block);
        if (block->cblock.handle != NULL) {
                /* Have we already loaded this library? If so, then unload this
                 * copy and increase the reference counter so that we can keep
                 * track (in lisp) of how many copies we use.
                 */
                cl_object other = ecl_library_find_by_handle(block->cblock.handle);
                if (other != ECL_NIL) {
			GC_call_with_alloc_lock(dlclose_wrapper, block);
                        block = other;
                        block->cblock.refs = ecl_one_plus(block->cblock.refs);
                } else {
                        si_set_finalizer(block, ECL_T);
                        cl_core.libraries = CONS(block, cl_core.libraries);
                }
        }
	ecl_enable_interrupts();
        } ECL_WITH_GLOBAL_LOCK_END;
        return block;
}

cl_object
ecl_library_open(cl_object filename, bool force_reload) {
	cl_object block;
	bool self_destruct = 0;
	char *filename_string;

	/* Coerces to a file name but does not merge with cwd */
	filename = coerce_to_physical_pathname(filename);
        filename = ecl_namestring(filename,
                                  ECL_NAMESTRING_TRUNCATE_IF_ERROR |
                                  ECL_NAMESTRING_FORCE_BASE_STRING);

	if (!force_reload) {
		/* When loading a foreign library, such as a dll or a
		 * so, it cannot contain any executable top level
		 * code. In that case force_reload=0 and there is no
		 * need to reload it if it has already been loaded. */
		block = ecl_library_find_by_name(filename);
		if (!Null(block)) {
			return block;
		}
	} else {
		/* We are using shared libraries as modules and
		 * force_reload=1.  Here we have to face the problem
		 * that many operating systems do not allow to load a
		 * shared library twice, even if it has changed. Hence
		 * we have to make a unique copy to be able to load
		 * the same FASL twice. In Windows this copy is
		 * _always_ made because otherwise it cannot be
		 * overwritten. In Unix we need only do that when the
		 * file has been previously loaded. */
#if defined(ECL_MS_WINDOWS_HOST) || defined(cygwin)
		filename = copy_object_file(filename);
		self_destruct = 1;
#else
		block = ecl_library_find_by_name(filename);
		if (!Null(block)) {
			filename = copy_object_file(filename);
			self_destruct = 1;
		}
#endif
	}
 DO_LOAD:
        block = ecl_library_open_inner(filename, self_destruct);
	/*
	 * A second pass to ensure that the dlopen routine has not
	 * returned a library that we had already loaded. If this is
	 * the case, we close the new copy to ensure we do refcounting
	 * right.
	 */
	if (block->cblock.refs != ecl_make_fixnum(1)) {
                if (force_reload) {
                        ecl_library_close(block);
                        filename = copy_object_file(filename);
                        self_destruct = 1;
                        goto DO_LOAD;
                }
	}
	return block;
}

void *
ecl_library_symbol(cl_object block, const char *symbol, bool lock) {
	void *p;
	if (block == @':default') {
		cl_object l;
		for (l = cl_core.libraries; l != ECL_NIL; l = ECL_CONS_CDR(l)) {
			cl_object block = ECL_CONS_CAR(l);
			p = ecl_library_symbol(block, symbol, lock);
			if (p) return p;
		}
		ecl_disable_interrupts();
#if defined(ECL_MS_WINDOWS_HOST)
 		{
		HANDLE hndSnap = NULL;
		HANDLE hnd = NULL;
		hndSnap = CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, GetCurrentProcessId());
		if (hndSnap != INVALID_HANDLE_VALUE)
		{
			MODULEENTRY32 me32;
			me32.dwSize = sizeof(MODULEENTRY32);
			if (Module32First(hndSnap, &me32))
			{
				do
					hnd = GetProcAddress(me32.hModule, symbol);
				while (hnd == NULL && Module32Next(hndSnap, &me32));
			}
			CloseHandle(hndSnap);
		}
		p = (void*)hnd;
		}
#endif
#ifdef HAVE_DLFCN_H
		p = dlsym(0, symbol);
#endif
#if !defined(ECL_MS_WINDOWS_HOST) && !defined(HAVE_DLFCN_H)
		p = 0;
#endif
		ecl_enable_interrupts();
	} else {
		ecl_disable_interrupts();
#ifdef HAVE_DLFCN_H
		p = dlsym(block->cblock.handle, symbol);
#endif
#if defined(ECL_MS_WINDOWS_HOST)
		{
			HMODULE h = (HMODULE)(block->cblock.handle);
			p = GetProcAddress(h, symbol);
		}
#endif
#ifdef HAVE_MACH_O_DYLD_H
		NSSymbol sym;
		sym = NSLookupSymbolInModule((NSModule)(block->cblock.handle),
					     symbol);
		if (sym == 0) {
			p = 0;
		} else {
			p = NSAddressOfSymbol(sym);
		}
#endif
		ecl_enable_interrupts();
		/* Libraries whose symbols are being referenced by the FFI should not
		 * get garbage collected. Until we find a better solution we simply lock
		 * them for the rest of the runtime */
		if (p) {
			block->cblock.locked |= lock;
		}
	}
	if (!p)
		set_library_error(block);
	return p;
}

cl_object
ecl_library_error(cl_object block) {
	return block->cblock.error;
}

void
ecl_library_close(cl_object block) {
        const cl_env_ptr the_env = ecl_process_env();
        ECL_WITH_GLOBAL_LOCK_BEGIN(the_env) {
                ecl_disable_interrupts();
                if (block->cblock.refs != ecl_make_fixnum(1)) {
                        block->cblock.refs = ecl_one_minus(block->cblock.refs);
                        block = ECL_NIL;
                } else if (block->cblock.handle != NULL) {
                        GC_call_with_alloc_lock(dlclose_wrapper, block);
                        cl_core.libraries = ecl_remove_eq(block, cl_core.libraries);
                }
                ecl_enable_interrupts();
        } ECL_WITH_GLOBAL_LOCK_END;
	if (block != ECL_NIL && block->cblock.self_destruct) {
                if (!Null(block->cblock.name)) {
                        unlink((char*)block->cblock.name->base_string.self);
                }
        }
}

void
ecl_library_close_all(void)
{
	while (cl_core.libraries != ECL_NIL) {
		ecl_library_close(ECL_CONS_CAR(cl_core.libraries));
	}
}

ecl_def_ct_base_string(init_prefix, INIT_PREFIX, sizeof(INIT_PREFIX)-1, static, const);

cl_object
_ecl_library_init_prefix(void)
{
	return init_prefix;
}

ecl_def_ct_base_string(default_entry, INIT_PREFIX "CODE", sizeof(INIT_PREFIX "CODE")-1,
                   static, const);

cl_object
_ecl_library_default_entry(void)
{
	return default_entry;
}
#endif /* ENABLE_DLOPEN */
