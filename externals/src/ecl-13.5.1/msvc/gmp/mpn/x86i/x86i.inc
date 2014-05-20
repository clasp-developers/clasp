
%macro	FR_push	1
	push	%1
%assign	frame	frame+4
%endmacro

%macro	FR_pop	1
	pop		%1
%assign	frame	frame-4
%endmacro

%macro	FR_sesp	1
%assign	frame	frame+%1
%endmacro

%macro	FR_aesp	1
%assign	frame	frame-%1
%endmacro

%assign	frame	0

%macro do_name 2

%ifdef	DLL
	export %1
%else
	export %1@%2
%endif

%ifndef STDCALL
	global %1
%1:
%else
	global %1@%2
%1@%2:
%endif

%endmacro

%macro do_ret 1
%ifndef STDCALL
	ret
%else
    ret %1
%endif
%endmacro
