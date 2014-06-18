all:
	@echo You must specify a compiler type: mingw or msvc

mingw: import-mingw.exe

import-mingw.exe: import.c
	gcc -g `ecl-config --cflags` import.c -o $@ `ecl-config --ldflags`

msvc: import-msvc.exe

import-msvc.exe: import.c
	ecl-cc --compile -c import.c
	ecl-cc --link -Fe$@ import.obj
	del /q import.obj
