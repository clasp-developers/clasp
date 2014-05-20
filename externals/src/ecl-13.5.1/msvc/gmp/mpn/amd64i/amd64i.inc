
; the symbol 'frame' keeps track of how much stack space is
; being used locally in order to be able to track where the
; routine parameters arew relative to the current value of 
; the stack pointer in rsp

; save registers on the stack and adjust 'frame' accordingly

%macro f_push 1-*
%rep %0
	push %1
%rotate 1
%assign	frame	frame + 8
%endrep
%endmacro

; restore a register from the stack and adjust 'frame' accordingly

%macro f_pop 1
%rep %0
%rotate -1
	pop %1
%endrep
%assign	frame	frame - 8
%endmacro

; restore registers from the stack at the end of a routine where
; 'frame' does not need to be adjusted because it won't be used
; again

%macro f_pop 2-*
%rep %0
%rotate -1
	pop %1
%endrep
%endmacro

; obtain some local space on the stack

%macro f_add	1
	sub	rsp,%1
%assign frame frame + %1
%endmacro

; return local space on the stack at the end of a routine without the 
; need to adjust 'frame'

%macro f_sub	1
	add	rsp,%1
%endmacro

%assign	frame	0

%define x86_regs	rbx,rsi,rdi,rbp
