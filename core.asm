; vim: set ft=fasm : set tabstop=2 :

; DERIVED FROM:
; A sometimes minimal FORTH compiler and tutorial for Linux / i386 systems.
; By Richard W.M. Jones http://rwmj.wordpress.com/2010/08/07/jonesforth-git-repository/
; OEM build instructions
;	gcc -m32 -nostdlib -static -Wl,-Ttext,0 -Wl,--build-id=none -o jonesforth jonesforth.S

; Modified toward ANS comapatibility by Chen Chang Wu and others 18-AUG-2010
; Modified for assembly with FASM 1.68 by SGM (aka Tarkin) 25-AUG-2010
; Introductory material and most comments have been edited out by SGM.

; Further modified for use as a web scripting language
; FASM updated to 1.69.34

; (SGM:) Unless you received this software under a different license:
; THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
; WARRANTY.  IN PARTICULAR, THE AUTHORS MAKE NO
; REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
; OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
; fasm, elf.inc are Copyright (C) 2011 Tomasz Gryzstar. All rights reserved.
; unistd-26.inc is derived GNU/Linux unistd.h. See http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html
; core.asm and data.asm are Copyright (C) 2011 Steven G. Messervey. All rights reserved.

; (SGM:) This version directly generates an ELF executable with fasm

use32
format ELF executable 3
entry _start

; feature flags
;DEBUG_STARTUP = 1           ; debuggery: save CPU state at startup

include 'include/import32.inc' ; improved dynamic link/loading
include 'include/unistd-26.inc' ; 2.6 series kernels
;include 'include/unistd-24.inc' ; 2.4 series kernels

; Flags
F_IMMED   = 0x80						; immediate flag bitmask
F_HIDDEN  = 0x20						; hidden flag bitmask
F_LENMASK = 0x1F						; length flag bitmask

INIT_DAT_SEG_SIZE = 0x4000  ; initial amount of memory to request
RETURN_STACK_SIZE = 0x400 	; static return stack buffer 
ALT_STACK_SIZE    = 0x80 	  ; static slternate stack buffer 
BUFFER_SIZE       = 0x4000 	; static input buffer
FLOW_VERSION      = 09			; version # (perpetually beta, it seems..)
MAX_STRING_LENGTH = 0x100   ; max string length for string ops

LAST_ENTRY equ name_DUP			; For altering the intitial dictionary.

; fasm quirk. Other assemblers have simpler ways of using reserved words as labels
macro undefine [name] {
	local undefined
	name equ undefined 
}

BELFRY:
undefine DUP
undefine STORE
undefine CMOVE
undefine WORD
undefine DP
ENDBELFRY:


; NEXT macro.
macro NEXT {
	lodsd
	jmp near DWORD [eax]
}

; Macros to deal with the return stack.
macro PUSHRSP reg {
	lea ebp,[ebp - 4]            ; push reg on to return stack
	mov [ebp],reg
}

macro POPRSP reg {
	mov reg,[ebp]                ; pop top of return stack to reg
	lea ebp,[ebp + 4]
}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXERIMENTAL DYNAMIC LOADING SUPPORT ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; thanks to fasm/Tomasz G., I can now have flow dynamically load and unload  ;
; ELF shared object (.so) files. The possiblities now become nearly endless, ;
; because Forth application programmers can program for libraries already    ;
; present on their system, or can reasonably be assumed to exist for a given ;
; Linux distribution or set of distributions.                                ;
; Examples include X, GTK2, Qt, OpenSSL, GMP, and many, many more....        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
interpreter '/lib/ld-linux.so.2'
needed 'libdl.so.2'
import dlopen,dlsym,dlerror,dlclose

;;; BOOTSTRAP
segment readable writeable executable
align 4
_start:
  cld                         ; clear the direction flag

if defined DEBUG_STARTUP      ; 
	mov [INIT.edi],edi
	mov edi,INIT.eax
	stosd
	mov eax,ebx
	stosd
	mov eax,ecx
	stosd
	mov eax,edx
	stosd
	add edi,4
	mov eax,esi
	stosd
	mov eax,esp
	stosd
	mov eax,ebp
	stosd
	mov eax,_start
	stosd
end if

  mov edx,esp                 ; save ESP in EDX            
  mov [var_S0],edx            ; S0 = EDX = initial Data Stack Pointer

  pop eax                     ; EAX = ARGC
  mov [var_ARGC],eax          ; save ARGC
  mov [var_ARGS],esp          ; save &ARGV[]
  shl eax,2                   ; fast * 4
  add eax,esp                 ; EAX = *NULL
  mov esp,eax                 ; adjust ESP
	pop ecx                     ; ECX = 0
	mov [var_ENVS],esp          ; ESP -> ENVS, save it to ENVS

.envloop:                     ; count environment variables
  pop eax
  inc ecx
  test eax,eax
  jnz .envloop
  dec ecx                     ; adjust for loop
  mov [var_ENVC],ecx          ; save count of environment variables

  mov ebp,return_stack_top    ; Initialise the return stack.
  xor ebx,ebx                 ; Call brk(0)
  mov eax,__NR_brk
  int 0x80
  mov [var_DP],eax            ; Initialise DP to point at beginning of data segment.
  add eax,INIT_DAT_SEG_SIZE   ; Reserve nn bytes of memory for initial data segment.
  mov ebx,eax                 ; Call brk(DP+INIT_DAT_SEG_SIZE)
  mov eax,__NR_brk
  int 0x80                    ; like a fool, I ASSUME this works. It always has for me, anyway....
  mov esi,cold_start          ; Initialize the interpreter.
  mov esp,edx                 ; restore ESP
  NEXT                        ; Run the interpreter!
align 1

if defined DEBUG_STARTUP      ; 
code_INITST:
	push DWORD INIT.eax
	NEXT
end if

;;; DL* WRAPPERS

; void *dlopen(const char *path, int flag)
; DLOPEN ( u cstring -- x ) 0 for success
code_DLOPEN:
	call [dlopen]
	add esp,8
	push eax
	NEXT

; void *dlsym(void *handle, const char *symbol)
; DLSYM ( cstring x -- addr )
code_DLSYM:
	call [dlsym]
	add esp,8
	push eax
	NEXT

; const char *dlerror(void)
; DLERR ( -- cstring )
code_DLERR:
	call [dlerror]
	push eax
	NEXT

; int dlclose(void *handle)
; DLCLOSE ( x -- 0 | errno ) 0 for success
code_DLCLOSE:
	call [dlclose]
	add esp,4
	push eax
	NEXT

; call dynamically loaded function x with u # of parameters,
; passed on the stack as y0..yn
; DYNCALL ( yn..y0 u x -- x ) 
code_DYNCALL:
	pop eax
	pop ebx
	call eax
	add esp,ebx
	push eax
	NEXT

;;; INNER INTERPRETER
DOCOL:											  ; DOCOL - the interpreter!
	PUSHRSP esi								  ; push esi on to the return stack
	add DWORD eax,4						  ; eax points to codeword, so make
	mov esi,eax								  ; esi point to first data word
	NEXT

;;; BEHAVIOR DEFINING WORDS
DODOES: 
	cmp DWORD [eax+4],0       ; Has DOES been executed?
	jz .skip
	lea ebp,[ebp-4]
	mov [ebp],esi
	mov esi,[eax+4]           ; Get pointer to behavior words
.skip:
  lea eax,[eax+8]
	push eax                  ; Push the pointer to its data
	NEXT

; EXIT ( -- ) exit from within a word. ';' has an implied EXIT
code_EXIT:
	POPRSP esi                ; pop return stack into esi
	NEXT

;;; CODE PRIMITIVES ;;;

;;; PARAMETER STACK MANIPULATION
; DROP ( x -- )
code_DROP:
	pop eax										; drop top of stack
	NEXT

; SWAP ( x2 x1 -- x1 x2 )
code_SWAP:
	pop eax										; swap top two elements on stack
	pop ebx
	push eax
	push ebx
	NEXT

; DUP ( x -- x x )
code_DUP:
	mov eax,[esp]							; duplicate top of stack
	push eax
	NEXT

; OVER ( x1 x2 -- x1 x2 x1 )
code_OVER:
	mov eax,[esp+4]						; get the second element of stack
	push eax									; and push it on top
	NEXT

; ROT ( x3 x2 x1 -- x2 x1 x3 )
code_ROT:
	pop eax
	pop ebx
	pop ecx
	push ebx
	push eax
	push ecx
	NEXT

; -ROT ( x2 x2 x1 -- x1 x3 x2 )
code_NROT:
	pop eax
	pop ebx
	pop ecx
	push eax
	push ecx
	push ebx
	NEXT

; 2DROP ( x2 x1 -- )
code_TWODROP:
	pop eax										; drop top two elements of stack
	pop eax
	NEXT

; 2DUP ( x2 x1 -- x2 x1 x2 x1 )
code_TWODUP:
	mov eax,[esp]							; duplicate top two elements of stack
	mov ebx,[esp+4]
	push ebx
	push eax
	NEXT

; 2SWAP ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
code_TWOSWAP:
	pop eax										; swap top two pairs of elements of stack
	pop ebx
	pop ecx
	pop edx
	push ebx
	push eax
	push edx
	push ecx
	NEXT

; NIP ( x1 x2 -- x2 )
code_NIP:
	pop eax
	pop ebx
	push eax
	NEXT

; TUCK ( x1 x2 -- x2 x1 x2 )
code_TUCK:
	mov eax,DWORD [esp+4]
	push eax
	NEXT

; PICK ( xu ... x1 x0 u -- xu ... x1 x0 xu )
code_PICK:
	pop eax
	shl eax,2
	mov eax,DWORD [esp+eax]
	push eax
	NEXT


; ?DUP ( x1 -- x1 x1 | x1 )
code_QDUP:
	mov eax,[esp]							; duplicate top of stack if non-zero
	test eax,eax
	jz .skip
	push eax
.skip:	NEXT

;;; MATHEMATICAL OPERATIONS
; 1+ ( x -- x+1 )
code_INCR:
	inc DWORD [esp]						; increment top of stack
	NEXT

; 1- ( x -- x-1 )
code_DECR:
	dec DWORD [esp]						; decrement top of stack
	NEXT

; 4+ ( x -- x+4 )
code_INCR4:
	add [esp], DWORD 4				; add 4 to top of stack
	NEXT

; 4- ( x -- x-4 )
code_DECR4:
	sub [esp], DWORD 4				; subtract 4 from top of stack
	NEXT

; + ( x y -- x+y )
code__ADD:
	pop eax										; get top of stack
	add DWORD [esp],eax				; and add it to next word on stack
	NEXT

; - ( y x -- x-y )
code__SUB:
	pop eax										; get top of stack
	sub DWORD [esp],eax				; and subtract it from next word on stack
	NEXT

; * ( x y -- x*y ) signed integer multiply
code__MUL:
	pop eax										; multiply top two items on stack
	pop ebx
	imul eax,ebx
	push eax									; ignore overflow
	NEXT

; / ( x y -- x/y ) signed integer divide
code__DIV:
	pop ebx
	pop eax
	cdq
	idiv ebx
	push eax
	NEXT

; MOD ( x y -- x%y ) signed integer modulo
code__MOD:
	pop ebx
	pop eax
	cdq
	idiv ebx
	push edx                    ; push remainder
	NEXT


; /MOD (  x y -- x/y x%y ) signed integer divide with remainder
code_DIVMOD:
	pop ebx
	pop eax
	cdq
	idiv ebx
	push edx                  ; push remainder
	push eax                  ; push quotient
	NEXT

; U/ ( x y -- x/y ) unsigned integer divide
code_UDIV:
	pop ebx
	pop eax
	div ebx
	push eax
	NEXT

; UMOD ( x y -- x%y) unsigned integer modulo
code_UMOD:
	pop ebx
	pop eax
	div ebx
	push edx
	NEXT

; U/MOD ( x y -- x/y x%y ) unsigned integer with remainder
code_UDIVMOD:
	pop ebx
	pop eax
	div ebx
	push edx                  ; push remainder
	push eax                  ; push quotient
	NEXT

; WITHIN ( u1 u2 x -- 0 | 1 ) 1 IFF u1 < x < u2
code_WITHIN:   ; again, 10x easier in assembly
	pop eax
	pop ebx
	pop ecx
	xor edx,edx
	cmp eax,ecx
	jle .false
	cmp eax,ebx
	jge .false
	inc edx
.false:
	push edx
	NEXT

; number base manipulation - all are ( -- )
code_BINARY:
	mov [var_BASE],dword 2
	NEXT

code_OCTAL:
	mov [var_BASE],dword 8
	NEXT

code_HEX:
	mov [var_BASE],dword 16
	NEXT

code_DECIMAL:
	mov [var_BASE],dword 10
	NEXT

;;; LOGICAL  & COMPARISON OPERATORS

;	ANS FORTH says that the comparison words should
; return all (binary) 1's for TRUE and all 0's for FALSE.
; (SGM:) these comparison words are DPANS94 compliant

code__EQU:				  ; top two words are equal?
	pop eax
	pop ebx
	cmp eax,ebx
	sete al
	bt eax,0
	salc
	mov ah,al
	push ax
	push ax
	NEXT

code_NEQU:				  ; top two words are not equal?
	pop eax
	pop ebx
	cmp eax,ebx
	setne al
	bt eax,0
	salc
	mov ah,al
	push ax
	push ax
	NEXT

code_LT:            ; ( x y -- TRUE | FALSE )
	pop eax
	pop ebx
	cmp ebx,eax
	setl al
	bt eax,0
	salc
	mov ah,al
	push ax
	push ax
	NEXT

code_GT:            ; ( x y -- TRUE | FALSE )
	pop eax
	pop ebx
	cmp ebx,eax
	setg al
	bt eax,0
	salc
	mov ah,al
	push ax
	push ax
	NEXT

code_LE:
	pop eax
	pop ebx
	cmp ebx,eax
	setle al
	bt eax,0
	salc
	mov ah,al
	push ax
	push ax
	NEXT

code_GE:
	pop eax
	pop ebx
	cmp ebx,eax
	setge al
	bt eax,0
	salc
	mov ah,al
	push ax
	push ax
	NEXT

code_ZEQU:	; top of stack equals 0?
	pop eax
	or eax,eax
	setz al
	bt eax,0
	salc
	mov ah,al
	push ax
	push ax
	NEXT

code_ZNEQU:	; top of stack not 0?
	pop eax
	test eax,eax
	setnz al
	bt eax,0
	salc
	mov ah,al
	push ax
	push ax
	NEXT

; comparisons with 0

code_ZLT:
	xor ebx,ebx
	pop eax
	cmp eax,ebx
	setl al
	bt eax,0
	salc
	mov ah,al
	push ax
	push ax
	NEXT

code_ZGT:
	xor ebx,ebx
	pop eax
	cmp eax,ebx
	setg al
	bt eax,0
	salc
	mov ah,al
	push ax
	push ax
	NEXT

code_ZLE:
	xor ebx,ebx
	pop eax
	cmp eax,ebx
	setle al
	bt eax,0
	salc
	mov ah,al
	push ax
	push ax
	NEXT

code_ZGE:
	xor ebx,ebx
	pop eax
	cmp eax,ebx
	setge al
	bt eax,0
	salc
	mov ah,al
	push ax
	push ax
	NEXT

; AND ( x y -- x&y )
code__AND:
	pop eax
	and DWORD [esp],eax
	NEXT

; OR ( x y -- x|y )
code__OR:
	pop eax
	or DWORD [esp],eax
	NEXT

; XOR ( x y -- x^y )
code__XOR:
	pop eax
	xor DWORD [esp],eax
	NEXT

; this is the FORTH bitwise "NOT" function (cf. NEGATE and NOT)
; INVERT ( x -- !x )
code_INVERT:
	not DWORD [esp]
	NEXT

; NEGATE ( x -- -x 
code_NEGATE:
	neg DWORD [esp]
	NEXT

; NOT ( x -- 0 | 1 ) 1 IFF x != 0
code__NOT:
	xor eax,eax
	pop ebx
	or ebx,ebx
	setnz al
	NEXT

; TRUE ( -- 1 )
code_TRUE:
	push DWORD 1
	NEXT

; FALSE ( -- 0 )
code_FALSE:
	push DWORD 0
	NEXT

;;; DATA MANIPULATION

; ESI points to the next command, but in this case it points to the next
; literal 32 bit integer.  Get that literal into EAX and increment ESI.
; On x86, it's a convenient single byte instruction!  (cf. NEXT macro)
; LIT ( -- )
code_LIT:
	lodsd
	push eax                  ; push the literal number on to stack
	NEXT

; ! ( y x -- ) STORE y AT x
code_STORE:
	pop ebx                   ; address to store at
	pop eax                   ; data to store there
	mov [ebx],eax             ; store it
	NEXT

; @ ( x -- [x] ) FETCH data AT x
code_FETCH:
	pop ebx                   ; address to fetch
	mov eax,[ebx]             ; fetch it
	push eax                  ; push value onto stack
	NEXT

; +@ ( y x -- ) STORE [x] + y AT x
code_ADDSTORE:
	pop ebx                   ; address
	pop eax                   ; the amount to add
	add [ebx],eax             ; add it
	NEXT

; -@ ( y x -- ) STORE [x] + y AT x
code_SUBSTORE:
	pop ebx                   ; address
	pop eax                   ; the amount to subtract
	sub [ebx],eax             ; add it
	NEXT

; C! ( x -- ) STORE (x & 000000FF) AT Y
code_STOREBYTE:
	pop ebx                   ; address to store at
	pop eax                   ; data to store there
	mov [ebx],al              ; store it
	NEXT

; C@ ( x -- [c] ) FETCH BYTE AT x
code_FETCHBYTE:
	pop ebx                   ; address to fetch
	xor eax,eax
	mov al,[ebx]              ; fetch it
	push eax                  ; push value onto stack
	NEXT

; single byte copy
; C@C! ( x1 x2 -- x1++ x2++) 
code_CCOPY:
	mov ebx,[esp+4]           ; source address
	mov al,[ebx]              ; get source character
	pop edi                   ; destination address
	stosb                     ; copy to destination
	push edi                  ; increment destination address
	inc DWORD [esp+4]         ; increment source address
	NEXT

; byte copy with range
; CMOVE ( x1 x2 x3 -- ) 
code_CMOVE:
	mov edx,esi               ; preserve ESI
	pop ecx                   ; length
	pop edi                   ; destination address
	pop esi                   ; source address
	rep movsb                 ; copy source to destination
	mov esi,edx               ; restore ESI
	NEXT

; CMOVED ( x1 x2 x3 -- x3 )
code_CMOVED:
	mov edx,esi
	pop ecx
	pop edi
	pop esi
	push ecx
	rep movsb
	mov esi,edx
	NEXT

; BUILT-IN VARIABLES
; The built-in variables are:
;	STATE		Is the interpreter executing code (0) or compiling a word (non-zero)?
;	LATEST	Points to the latest (most recently defined) word in the dictionary.
;	DP			Points to the next free byte of memory.  When compiling, compiled words go here.
;	S0			Stores the address of the top of the parameter stack.
;	BASE		The current base for printing and reading numbers.

; STATE ( -- x )
code_STATE:
	push var_STATE
	NEXT

; DP ( -- x )
code_DP:
	push var_DP
	NEXT

; LATEST ( -- x )
code_LATEST:
	push var_LATEST
	NEXT
	
; S0 ( -- x )
code_S0:
	push var_S0
	NEXT

; BASE ( -- x )
code_BASE:
	push var_BASE
	NEXT

;;; FILE MANIPULATION VARIABLES

code_FDESC:
	push var_FDESC
	NEXT

code_FBUFF:
	push var_FBUFF
	NEXT

code_FSTAT:
	push var_FSTAT
	NEXT

;;; ALTERNATE STACK LOCATION
code_ASTAK:
	push var_ASTAK
	NEXT

; BUILT-IN CONSTANTS

; BUILT-IN CONSTANTS
;	The built-in constants are:
;	VERSION		Is the current version of this FORTH.
;	R0		    The address of the top of the return stack.
;	DOCOL		  Pointer to DOCOL.
;	F_IMMED		The IMMEDIATE flag's actual value.
;	F_HIDDEN	The HIDDEN flag's actual value.
;	F_LENMASK	The length mask in the flags/len byte.
;	SYS_*		  and the numeric codes of various Linux syscalls (from <asm/unistd.h>)

; VERSION ( -- x )
code_VERSION:
	push FLOW_VERSION
	NEXT
	
; R0 ( -- x )
code_RZ:
	push return_stack_top
	NEXT
	
; DOCOL ( -- x )
code_DOCOL:
	push DOCOL
	NEXT
	
; F_IMMED ( -- x )
code___F_IMMED:
	push F_IMMED
	NEXT
	
; F_HIDDEN ( -- x )
code___F_HIDDEN:
	push F_HIDDEN
	NEXT
	
; F_LENMASK ( -- x )
code___F_LENMASK:
	push F_LENMASK
	NEXT
	
; SYS_EXIT ( -- x )
code_SYS_EXIT:
	push __NR_exit
	NEXT
	
; SYS_OPEN ( -- x )
code_SYS_OPEN:
	push __NR_open
	NEXT
	
; SYS_CLOSE ( -- x )
code_SYS_CLOSE:
	push __NR_close
	NEXT
	
; SYS_READ ( -- x )
code_SYS_READ:
	push __NR_read
	NEXT
	
; SYS_WRITE ( -- x )
code_SYS_WRITE:
	push __NR_write
	NEXT
	
; SYS_CREAT ( -- x )
code_SYS_CREAT:
	push __NR_creat
	NEXT

; SYS_STAT ( -- x )
code_SYS_STAT:
	push __NR_stat
	NEXT

; SYS_FSTAT ( -- x )
code_SYS_FSTAT:
	push __NR_fstat
	NEXT

; SYS_BRK ( -- x )
code_SYS_BRK:
	push __NR_brk
	NEXT

; SYS_FORK ( -- x )
code_SYS_FORK:
	push __NR_fork
	NEXT

; SYS_EXECVE ( -- x )
code_SYS_EXECVE:
	push __NR_execve
	NEXT

; __O_RDONLY ( -- x )
code___O_RDONLY:
	push 0x00000000
	NEXT
	
; __O_WRONLY ( -- x )
code___O_WRONLY:
	push 0x00000001
	NEXT
	
; __O_RDWR ( -- x )
code___O_RDWR:
	push 0x00000002
	NEXT
	
; __O_CREAT ( -- x )
code___O_CREAT:
	push 0x00000040
	NEXT
	
; __O_EXCL ( -- x )
code___O_EXCL:
	push 0x00000080
	NEXT
	
; __O_TRUNC ( -- x )
code___O_TRUNC:
	push 0x00000200
	NEXT
	
; __O_APPEND ( -- x )
code___O_APPEND:
	push 0x00000400
	NEXT
	
; __O_NONBLOCK ( -- x )
code___O_NONBLOCK:
	push 0x00000800
	NEXT
	
; _DODOES ( -- x )
code___DODOES:
	push DODOES
	NEXT

;;; EXPERIMENTAL ARGS / ENV SUPPORT
code_ARGC:
	push DWORD [var_ARGC]
	NEXT

code_ARGS:
	push DWORD [var_ARGS]
	NEXT

code_ARG0:
	push DWORD var_ARG0
	NEXT

code_ARG1:
	push DWORD var_ARG1
	NEXT

code_ENVC:
	push DWORD [var_ENVC]
	NEXT

code_ENVS:
	push DWORD [var_ENVS]
	NEXT

;;; EXERIMENTAL DL* SUPPORT
code_RTLDLOCAL:
	push DWORD 0x00000000
	NEXT

code_RTLDLAZY:
	push DWORD 0x00000001
	NEXT

code_RTLDNOW:
	push DWORD 0x00000002
	NEXT

code_RTLDBINDMASK:
	push DWORD 0x00000003
	NEXT

code_RTLDNOLOAD:
	push DWORD 0x00000004
	NEXT

code_RTLDDEEPBIND:
	push DWORD 0x00000008
	NEXT

code_RTLDGLOBAL:
	push DWORD 0x00000100
	NEXT

code_RTLDNODELETE:
	push DWORD 0x00001000
	NEXT

; RETURN STACK
	
; >R ( x -- ) 
code_TOR:
	pop eax                   ; pop parameter stack into EAX
	PUSHRSP eax               ; push it on to the return stack
	NEXT

; R> ( -- x )
code_FROMR:
	POPRSP eax                ; pop return stack on to %eax
	push eax                  ; and push on to parameter stack
	NEXT

; RSP@ ( -- x )
code_RSPFETCH:
	push ebp
	NEXT

; RSP! ( x -- )
code_RSPSTORE:
	pop ebp
	NEXT

; RDROP ( -- )
code_RDROP:
	add ebp,4                 ; pop return stack and throw away
	NEXT

; PARAMETER STACK

; PSP@ ( -- PSP )
code_PSPFETCH:
	mov eax,esp
	push eax
	NEXT

; PSP! ( x -- )
code_PSPSTORE:
	pop eax
	mov esp,eax
	NOP
	NOP
	NEXT

;;; ALTERNATE STACK WORDS

; AS> - pop a value from the Alternate and push it onto the Parameter Stack
code_ASPFROM:
	push ebp
	mov ebp,esp
	mov esp,[var_ASTAK]
	pop eax
	mov [var_ASTAK],esp
	mov esp,ebp
	pop ebp
	push eax
	NEXT

code_ASPTO:
	pop eax
	push ebp
	mov ebp,esp
	mov esp,[var_ASTAK]
	push eax
	mov [var_ASTAK],esp
	mov esp,ebp
	pop ebp
	NEXT

;;; DICTIONARY & INTERPRETER OPERATIONS
; BYE ( -- ) exit the process 
code_BYE:
	xor ebx,ebx
	mov eax,__NR_exit
	int 0x80

; HERE ( -- addr )
code_HERE:
	push dword [var_DP]
	NEXT

; (INTERPRET) ( -- )
code_INTERP:
	pop ecx
	pop edi
	jmp code_INTERPRET.jumpin

; INTERPRET ( -- )
code_INTERPRET:
	call _PWORD		            ; Returns ecx = length, edi = pointer to word.
; Is it in the dictionary?
.jumpin:
	xor eax,eax
	mov [interpret_is_lit],eax; Not a literal number (not yet anyway ...)
	call _FIND                ; Returns eax = pointer to header or 0 if not found.
	test eax,eax              ; Found?
	jz .Q1
; In the dictionary.  Is it an IMMEDIATE codeword?
	mov edi,eax               ; EDI = dictionary entry
	mov byte al,[edi+4]       ; Get name+flags.
	push eax                  ; Just save it for now.
	call _TCFA                ; Convert dictionary entry (in %edi) to codeword pointer.
	pop eax
	and al,byte F_IMMED				; Is IMMED flag set?
	mov eax,edi
	jnz .Q4                   ; If IMMED, jump straight to executing.
	jmp .Q2

.Q1: ; Not in the dictionary (not a word) so assume it's a literal number.
	inc dword [interpret_is_lit]
	call _NUMBER              ; Returns the parsed number in EAX, ECX > 0 if error
	test ecx,ecx
	jnz .Q6
	mov ebx,eax
	mov eax,LIT               ; The word is LIT
.Q2: ; Are we compiling or executing?
	mov edx,[var_STATE]
	test edx,edx
	jz .Q4                    ; Jump if executing.
; Compiling - just append the word to the current dictionary definition.
	call _COMMA
	mov ecx,[interpret_is_lit]; Was it a literal?
	test ecx,ecx
	jz .Q3
	mov eax,ebx               ; Yes, so LIT is followed by a number.
	call _COMMA
.Q3:
  NEXT
.Q4: ; Executing - run it!
	mov ecx,[interpret_is_lit]; Literal?
	test ecx,ecx              ; Literal?
	jnz .Q5
; Not a literal, execute it now.  This never returns, but the codeword will
; eventually call NEXT which will reenter the loop in QUIT.
	jmp near dword [eax]
.Q5: ; Executing a literal, which means push it on the stack.
	push ebx
	NEXT
.Q6:
; Parse error (not a known word or a number in the current BASE).
; Print an error message followed by up to 40 characters of context.
	mov ebx,2                 ; 1st param: stderr
	mov ecx,errmsg            ; 2nd param: error message
	mov edx,errmsgend-errmsg  ; 3rd param: length of string
	mov eax,__NR_write        ; write syscall
	int 0x80
	mov ecx,[currkey]         ; the error occurred just before currkey position
	mov edx,ecx
	sub edx,buffer            ; edx = currkey - buffer (length in buffer before currkey)
	cmp edx,40                ; if > 40, then print only 40 characters
	jle .Q7
	mov edx,40
.Q7:
  sub ecx,edx               ; ecx = start of area to print, edx = length
	mov eax,__NR_write        ; write syscall
	int 0x80
	mov ecx,errmsgnl          ; newline
	mov edx,1
	mov eax,__NR_write        ; write syscall
	int 0x80
	NEXT

; (STDIN) ( -- ) reset current input pointer to STDIN
code_PAREN_STDIN:
	xor eax,eax
	mov [currinp],eax
	NEXT

; (EVAL) ( x -- ) set current input pointer to x
code_PAREN_EVAL:
	pop eax                   ; get fd
	mov [currinp],eax         ; make it the new input fd
	jmp code_INTERPRET				; and go, daddy, go

; [:WORD] ( cccc c -- addr u ) read the next full word delimited by <TOS>
code_BRACKET_WORD:
	pop ebx                     ; get delimiter
	mov edi,word_buffer         ; prep EDI
	push edi                    ; save buffer start on stack
.loop:
	push ebx                    ; save delimter on stack
	call _KEY                   ; get next char
	stosb                       ; put char in buffer, address++
	pop ebx                     ; restore delimiter
	cmp al,bl                   ; char == delimiter?
	jne .loop                   ; loop iff char != delimiter, else fall through
	mov eax,edi                 ; EAX = address of last char in buffer
	pop ebx                     ; get start of buffer off stack
	sub eax,ebx                 ; calculate length
	dec eax                     ; adj for delimiter
	push ebx                    ; buffer start address
	push eax                    ; length
	NEXT
	
; (WORD) ( -- addr u )
code_PAREN_WORD:
	call _PWORD
	push edi                  ; push base address
	push ecx                  ; push length
	NEXT

_PWORD: ; Search for first non-blank character.  Also skip \ comments. Also skip from # until EOL.
.W1:
	call _KEY                 ; get next key, returned in eax
	cmp al,0x5C               ; start of a comment? ('\')
	je .W3                    ; if so, skip the comment / line
	cmp al,0x23               ; start of shebang / bash comment?
	je .W3                    ; if so, skip the line
	cmp al,0x20               ; SPACE ? (' ')
	jbe .W1				            ; if so, keep looking
; Search for the end of the word, storing chars as we go.
	mov edi,word_buffer       ; pointer to return buffer
.W2:
	stosb                     ; add character to return buffer
	call _KEY                 ; get next key, returned in al
	cmp al,0x20 	            ; is blank?
	ja .W2                    ; if not, keep looping
; Return the word (well, the static buffer) and length.
	sub edi,word_buffer
	mov ecx,edi               ; return length of the word
	mov edi,word_buffer       ; return address of the word
	ret
; Code to skip \ comments to end of the current line.
.W3:
	call _KEY
	cmp al,0x0A               ; end of line yet?
	jne .W3
	jmp .W1

;defcode "NUMBER",6,0,NUMBER
code_NUMBER:
	pop ecx                   ; length of string
	pop edi                   ; start address of string
	call _NUMBER
	push eax                  ; parsed number
	push ecx                  ; number of unparsed characters (0 = no error)
	NEXT

_NUMBER:
	xor eax,eax
	xor ebx,ebx
	test ecx,ecx		          ; parsing a zero-length string is an error, but will return 0.
	jz .N5
	mov edx,[var_BASE]        ; get BASE (in dl)
; Check if first character is '-'.
	mov bl,[edi]              ; bl = first character in string
	inc edi
	push eax                  ; push 0 on stack
	cmp bl,0x2D               ; negative number?
	jnz .N2
	pop eax
	push ebx                  ; push <> 0 on stack, indicating negative
	dec ecx
	jnz .N1
	pop ebx                   ; error: string is only '-'.
	mov ecx,1
	ret
; Loop reading digits.
.N1:
  imul eax,edx              ; eax *= BASE
	mov byte bl,[edi]         ; bl = next character in string
	inc edi
; Convert 0-9, A-Z to a number 0-35.
.N2:
  sub bl,0x30               ; < '0'?
	jb .N4
	cmp bl,10                 ; <= '9'?
	jb .N3
	sub bl,17                 ; < 'A'? (17 is 'A'-'0')
	jb .N4
	add bl,10
.N3:
  cmp bl,dl                 ; >= BASE?
	jge .N4
; OK, so add it to eax and loop.
	add eax,ebx
	dec ecx
	jnz .N1
; Negate the result if first character was '-' (saved on the stack).
.N4:
  pop ebx
	test ebx,ebx
	jz .N5
	neg eax
.N5:	
  ret

; (FIND) ( addr u -- x ) NULL = 0 = not found
code_PAREN_FIND:
	pop ecx                   ; ecx = length
	pop edi                   ; edi = address
	call _FIND
	push eax                  ; eax = address of dictionary entry (or NULL)
	NEXT

_FIND:
	push esi                  ; Save esi so we can use it in string comparison.
; Now we start searching backwards through the dictionary for this word.
	mov edx,[var_LATEST]      ; LATEST points to name header of the latest word in the dict.
.F1:
  test edx,edx              ; NULL pointer?  (end of the linked list)
	je .F3
; Compare the length expected and the length of the word.
; Note that if the F_HIDDEN flag is set on the word, then by a bit of trickery
; this won't pick the word (the length will appear to be wrong).
	xor eax,eax
	mov al,[edx+4]            ; al = flags+length field
	and byte al,(F_HIDDEN or F_LENMASK) ; al = name length
	cmp al,cl                 ; Length is the same?
	jne .F2
; Compare the strings in detail.
	push ecx                  ; Save the length
	push edi                  ; Save the address (repe cmpsb will move this pointer)
	lea esi,[edx+5]           ; Dictionary string we are checking against.
	repe cmpsb                ; Compare the strings.
	pop edi
	pop ecx
	jne .F2                   ; Not the same.
; The strings are the same - return the header pointer in eax
	pop esi
	mov eax,edx
	ret
.F2:
  mov edx,[edx]             ; Move back through the link field to the previous word
	jmp .F1                   ; .. and loop.

.F3:	; Not found.
	pop esi
	xor eax,eax               ; Return zero to indicate not found.
	ret

; >CFA ( x1 -- x2 ) convert dictionary pointer to xt
code_TCFA:
	pop edi
	call _TCFA
	push edi
	NEXT
_TCFA:
	xor eax,eax
	add edi,4                 ; Skip link pointer.
	mov al,[edi]              ; Load flags+len into %al.
	inc edi                   ; Skip flags+len byte.
	and al,F_LENMASK          ; Just the length, not the flags.
	add edi,eax               ; Skip the name.
	add edi,3                 ; The codeword is 4-byte aligned.
	and edi,(not 3)
	ret

; COMPILING

; HEADER, ( addr u -- ) create a dictionary header
code_HEADER_COMMA:
	pop ecx                   ; ecx = length
	pop ebx                   ; ebx = address of name
; Link pointer.
	mov edi,[var_DP]          ; edi is the address of the header
	mov eax,[var_LATEST]      ; Get link pointer
	stosd                     ; and store it in the header.
; Length byte and the word itself.
	mov al,cl                 ; Get the length.
	stosb                     ; Store the length/flags byte.
	push esi
	mov esi,ebx               ; esi = word
	rep movsb                 ; Copy the word
	pop esi
	add edi,3				          ; Align to next 4 byte boundary.
	and edi,(not 3)
; Update LATEST and DP.
	mov eax,[var_DP]
	mov [var_LATEST],eax
	mov [var_DP],edi
  ;mov edi,eax
	NEXT

; , ( x -- ) append x to DP and update DP
code_COMMA:
	pop eax                   ; DWORD to store
	call _COMMA
	NEXT
_COMMA:
	mov edi,[var_DP]					; get DP
	stosd                     ; store it
	mov [var_DP],edi					; update DP
	ret

; [	LBRAC		STATE := 0	Switch to immediate mode.
; ]	RBRAC		STATE := 1	Switch to compile mode.
; [ (LBRAC) is itself an IMMEDIATE word.  

; [ ( -- )
code_LBRAC:
	xor eax,eax
	mov DWORD [var_STATE],eax	; Set STATE to 0.
	NEXT

; ] ( -- )
code_RBRAC:
	mov [var_STATE], DWORD 1  ; Set STATE to 1.
	NEXT

; IMMEDIATE ( -- )
code_IMMEDIATE:
	mov edi,[var_LATEST]      ; LATEST word.
	add edi,4                 ; Point to name/flags byte.
	xor [edi],byte F_IMMED   ; Toggle the IMMED bit.
	NEXT

; HIDDEN ( -- )
code_HIDDEN:
	pop edi		                ; Dictionary entry.
	add edi,4		              ; Point to name/flags byte.
	xor [edi],byte F_HIDDEN		; Toggle the HIDDEN bit.
	NEXT

; ['] ( -- x )
code_BRACKET_TICK:
	lodsd                     ; Get the address of the next word and skip it.
	push eax                  ; Push it on the stack.
	NEXT

; ALIGNED ( -- ) align the address on <TOS> to DWORD
code_ALIGNED:
	pop eax
	mov bl,3
	movzx ebx,bl
	add eax,ebx
	not ebx
	and eax,ebx
	push eax
	NEXT

; C, ( x -- )
; append the low eight bits of the DWORD on <TOS> to HERE and update HERE
code_CHARCOMMA:
	pop eax
	mov ebx,[var_DP]
	mov [ebx],al
	inc ebx
	mov [var_DP],ebx
	NEXT

; DLEN ( -- x ) return the number of XTs in the dictionary
code_DLEN:
	xor eax,eax
	mov ebx,var_LATEST
.loop:
	mov ebx,[ebx]
	inc eax
	or ebx,ebx
	jnz .loop
.done:
	push eax
	NEXT

;;; LOOPS AND BRANCHING

; BRANCH ( -- )
code_BRANCH:
	add esi,[esi]             ; add the offset to the instruction pointer
	NEXT

; 0BRANCH ( -- )
code_ZBRANCH:
	pop eax
	test eax,eax              ; top of stack is zero?
	jz code_BRANCH            ; if so, jump back to the branch function above
	lodsd                     ; otherwise we need to skip the offset
	NEXT

; EXECUTE ( xt -- )
code_EXECUTE:
	pop eax                   ; Get xt into EAX
	jmp DWORD [eax]           ; and jump to it
; After xt runs its NEXT will continue executing the current word.

; IF ( -- addr )
code__IF:
	mov eax,ZBRANCH   ; ZBRANCH XT
	mov edi,[var_DP]  ; HERE
	stosd             ; ,
	push edi          ; <TOS> = HERE
	xor eax,eax       ; EAX = 0
	stosd             ; 0 ,
	mov [var_DP],edi  ; update HERE
	NEXT

; THEN ( addr -- )
code__THEN:
	pop edi          ; offset address
	mov eax,[var_DP] ; HERE
	sub eax,edi      ; calculate offset
	stosd            ; fill in offset
	NEXT

; ELSE ( addr1 -- addr2 )
code__ELSE:
	pop ebx          ; pop offset address
	mov eax,BRANCH   ; BRANCH XT
	mov edi,[var_DP] ; HERE
	stosd            ; ,
	push edi         ; <TOS> = HERE
	xor eax,eax      ; EAX = 0
	stosd            ; (dummy offset) ,
	mov [var_DP],edi ; update HERE
	mov eax,edi      ; EAX = HERE
	sub eax,ebx      ; calculate offset
	mov [ebx],eax    ; store offset
	NEXT

; BEGIN ( -- addr )
code_BEGIN:
	push dword [var_DP]
	NEXT

; UNTIL ( addr -- )
code_UNTIL:
	mov eax,ZBRANCH  ; ZBRANCH XT
	mov edi,[var_DP] ; HERE
	stosd            ; XT ,
	mov ebx,edi      ; EBX = HERE
	pop eax          ; get offset address
	sub eax,ebx      ; calculate offset
	stosd            ; ,
	mov [var_DP],edi ; update HERE
	NEXT

; AGAIN ( addr -- )
code_AGAIN:
	mov eax,BRANCH   ; BRANCH XT
	mov edi,[var_DP] ; HERE
	stosd            ; XT ,
	mov ebx,edi      ; EBX = HERE
	pop eax          ; get offset address
	sub eax,ebx      ; calculate offset
	stosd            ; ,
	mov [var_DP],edi ; update HERE
	NEXT

; WHILE ( addr1 -- addr2 addr1 )
code__WHILE:
	mov eax,ZBRANCH   ; ZBRANCH XT
	mov edi,[var_DP] ; HERE
	stosd             ; XT ,
	mov eax,edi       ; EAX = HERE
	pop ebx           ; get offset address
	push eax          ; offset HERE SWAP
	push ebx
	xor eax,eax       ; EAX = 0
	stosd             ; LIT 0 ,
	mov [var_DP],edi  ; update HERE
	NEXT

; REPEAT ( addr2 addr1 -- )
code__REPEAT:
	mov eax,BRANCH
	mov edi,[var_DP]
	stosd            ; ['] BRANCH ,
	pop eax
	mov ebx,edi
	sub eax,ebx
	stosd
	pop ebx
	mov eax,edi
	sub eax,ebx
	mov [ebx],eax
	mov [var_DP],edi
	NEXT

; UNLESS ( -- )
code_UNLESS:
	mov eax,INVERT
	mov edi,[var_DP]
	stosd
	mov eax,_IF
	stosd
	mov [var_DP],edi
	NEXT

; CASE ( -- 0 )
code_CASE:
	xor eax,eax
	push eax
	NEXT

;;; TERMINAL I/O, CHARACTER HANDLING

; ( ( ccc -- )
code_LPAREN: ; 10x easier in assembly,IMHO
	mov ebx,1
.loop:
	push ebx
	call _KEY
	pop ebx
	cmp al,'('
	jnz .skip
	inc ebx
	jmp .loop
.skip:
	cmp al,')'
	jnz .loop
	dec ebx
	jnz .loop
	NEXT

; KEY ( -- c )
code_KEY:
	call _KEY
	push eax                  ; push return value on stack
	NEXT
_KEY:
	mov ebx,[currkey]
	cmp ebx,[bufftop]
	jge .K1                   ; exhausted the input buffer?
	xor eax,eax
	mov al,[ebx]              ; get next key from input buffer
	inc ebx
	mov [currkey],ebx         ; increment currkey
	ret

; Out of input; use read(2) to fetch more input from stdin
; SGM - modded to use currinp, a variable holding the current
; file descriptor to read from
.K1:
	mov ebx,[currinp]         ; 1st param: fd to read from
	mov ecx,buffer            ; 2nd param: buffer
	mov [currkey],ecx
	mov edx,BUFFER_SIZE       ; 3rd param: max length
	mov eax,__NR_read         ; syscall: read
	int 0x80
	test eax,eax              ; If %eax <= 0, then exit.
	jbe .K2
	add ecx,eax               ; buffer+%eax = bufftop
	mov [bufftop],ecx
	jmp _KEY

.K2: ; Error or end of input: check input source
	or ebx,ebx                ; currinp == stdin?
	jz .K3                    ; yes, exit
	mov eax,__NR_close        ; no - close the file descriptor
	int 0x80
	xor eax,eax								; eax = 0
	mov [currinp],ax          ; reset current fd to stdin
	mov eax,buffer            ; get address of buffer start
	mov [bufftop],eax         ; reset buffer top
	mov [currkey],eax         ; reset current key
	jmp _KEY                  ; jump to reading stdin
.K3:
	xor ebx,ebx
	mov eax,__NR_exit         ; syscall: exit
	int 0x80

; EMIT ( c -- )
code_EMIT:
	pop eax
	call _EMIT
	NEXT
_EMIT:
	mov [emit_scratch],al			; write needs the address of the byte to write
_EMIT2:
	mov ebx,1                 ; 1st param: stdout
	mov ecx,emit_scratch      ; 2nd param: address
	mov edx,1                 ; 3rd param: nbytes = 1
	mov eax,__NR_write        ; write syscall
	int 0x80
	ret

; CHAR ( -- c )
code_CHAR:
	call _PWORD		            ; Returns ECX = length, EDI = pointer to word.
	xor eax,eax
	mov byte al,[edi]         ; Get the first character of the word.
	push eax                  ; Push it onto the stack.
	NEXT

; CLS assumes ANSI-compatible terminal
; CLS ( -- ) clear the terminal screen
code_CLS:
	mov eax,__NR_write				; write()
	mov ebx,1									; stdout
	mov ecx,clsseq						; ANSI sequence
	mov edx,clsseqend-clsseq	; length
	int 0x80									; syscall
	NEXT

; SPACES ( u -- ) emit <TOS> spaces
code_SPACES:
	pop ecx
	call _SPACES
	NEXT
_SPACES:
	xor eax,eax
	cmp ecx,eax
	jle .done
	mov [emit_scratch],byte 32
.loop:
	push ecx
	call _EMIT2
	pop ecx
	loop .loop
.done:
	ret

; LINUX OS INTERFACE

; SYSCALL3 ( x4 x3 x2 x1 -- y ) y = return value
code_SYSCALL3:
	pop eax                   ; System call number (see <asm/unistd.h>)
	pop ebx                   ; First parameter.
	pop ecx                   ; Second parameter
	pop edx                   ; Third parameter
	int 0x80
	push eax                  ; Result (negative for -errno)
	NEXT

; SYSCALL2 ( x3 x2 x1 -- y ) y = retrun value
code_SYSCALL2:
	pop eax                   ; System call number (see <asm/unistd.h>)
	pop ebx                   ; First parameter.
	pop ecx                   ; Second parameter
	int 0x80
	push eax                  ; Result (negative for -errno)
	NEXT

; SYSCALL1 ( x2 x1 -- y ) y = return value
code_SYSCALL1:
	pop eax                   ; System call number (see <asm/unistd.h>)
	pop ebx                   ; First parameter.
	int 0x80
	push eax                  ; Result (negative for -errno)
	NEXT

; SYSCALL0 ( x -- y ) y = return value
code_SYSCALL0:
	pop eax                   ; System call number (see <asm/unistd.h>)
	int 0x80
	push eax                  ; Result (negative for -errno)
	NEXT

; support for shell scripting
; discard any line beginning with '#!'
; #! ( -- )
code_SHEBANG:
.loop:
	call _KEY                   ; get next char
	cmp al,0x0A                 ; \n ?
	jne .loop
	NEXT

; ENV? ( addr1 u -- addr2 | 0 ) test if string is an environment variable
; return the address of the first character after '=' on success,
; 0 on failure
code_ENVQ:
	pop ecx                      ; pop length
	pop ebx                      ; pop string addr
	push esi                     ; save ESI
	push ebp                     ; save EBP
	mov ebp,ebx
	mov [.esp],esp               ; save ESP
	mov [.str],ebx
	mov [.cnt],ecx
	mov esp,[var_ENVS]           ; ESP -> ENV[]
	mov ebx,.esi
.outer:
	pop eax
	or eax,eax
	jz .done
	mov esi,eax
	mov [ebx],eax
	mov ebp,[.str]
	mov ecx,[.cnt]
.inner:
	lodsb
	mov ah,[ebp]
	inc ebp
	cmp ah,al
	jnz .outer
	dec ecx
	jnz .inner
	mov eax,[ebx]
	mov ecx,[.cnt]
	inc eax                      ; skip '='
	add eax,ecx
.done:
	mov esp,[.esp]
	pop ebp
	pop esi
	push eax
	NEXT
.esp: dd 0
.esi: dd 0
.str: dd 0
.cnt: dd 0


;;; MEMORY MANAGEMENT

; CELLS ( u -- u*4 )
code_CELLS:
	pop eax
	shl eax,2
	push eax
	NEXT

; GET-BRK ( -- x ) returns the breakpoint
code_GETBRK:
	call _GETBRK
	push eax
	NEXT
_GETBRK:
	xor ebx,ebx
_BRK: ; MAKE SURE an integer is in EBX before calling!
	mov eax,__NR_brk
	int 0x80
	ret

; BRK ( x -- 0 | -1 ) set the breakpoint, 0 on success
code_BRK:
	pop ebx
	call _BRK
	push eax
	NEXT

; MORECORE ( u -- 0 | -1 ) request x DWORDS of memory, 0 on success
code_MORECORE:
	call _GETBRK
	pop ebx
	shl ebx,2
	add eax,ebx
	xchg eax,ebx
	call _BRK
	push eax
	NEXT

; UNUSED ( -- u ) 
code_UNUSED:
	call _GETBRK
	mov ebx,dword [var_DP]
	sub eax,ebx
	shr eax,2
	push eax
	NEXT

;;; FILESYSTEM INTERFACE
; yes, some of these are duplicates of the O_* constants.
; disk and core are cheap, readability is priceless

code_READONLY:
	xor eax,eax
	push eax
	NEXT

code_READWRITE:
	xor eax,eax
	inc eax
	inc eax
	push eax
	NEXT

; STRINGS 

; DEPTH ( -- ) leave the number of stack items before this word on <TOS>
code_DEPTH:     ; very fast in assembler
	mov eax,[var_S0]
	mov ebx,esp
	sub eax,ebx
	shr eax,2
	push eax
	NEXT

; LITSTRING ( -- addr u )
code_LITSTRING:
	lodsd                     ; get the length of the string
	push esi                  ; push the address of the start of the string
	push eax                  ; push length on the stack
	add esi,eax               ; skip past the string
 	add esi,3									; but round up to next 4 byte boundary
	and esi,(not 3)
	NEXT

; TYPE ( addr u -- )
code_TYPE:
	mov ebx,1                 ; 1st param: stdout
	pop edx                   ; 3rd param: length of string
	pop ecx                   ; 2nd param: address of string
	mov eax,__NR_write        ; write syscall
	int 0x80
	NEXT

; (U.) ( u -- ) display an unpadded, unsigned number in the current base
code_PAREN_UDOT:
	pop eax                     ; EAX = number to display
	mov edi,udottop             ; EDI = end of buffer
	mov ebx,[var_BASE]          ; get BASE into EBX
	push edi                    ; save address of buffer end on stack
	std                         ; string ops decrement EDI
.loop:
	xor edx,edx                 ; handily clears the overflow flag
	mov ecx,edx                 ; clear ECX
	cmp eax,ebx                 ; EAX < BASE ?
	jb .skip1
	idiv ebx                     ; EDX:EAX = EDX:EAX / EBX ; quot=EAX, rem=EDX
	mov ecx,eax                 ; save quotient for next pass
	mov al,dl                   ; al = remainder

.skip1:
	cmp al,10
	jb .skip2
	add al,7
.skip2:
	add al,'0'
	stosb                       ; store char, EDI--
	mov eax,ecx                 ; restore the quotient or zero
	or eax,eax                  ; quotient == 0 ?
	jnz .loop
.done:
	cld                         ; reset direction flag
	pop eax                     ; get buffer end
	mov ebx,edi                 ; EBX = last digit
	sub eax,ebx                 ; calculate length
	inc ebx
	push ebx                    ; push address
	push eax                    ; push length
	jmp code_TYPE               ; jump to TYPE, it's NEXT will end this word

; push number of chars neccessary to print, in the current base,
; the usigned integer on <TOS>
; UWIDTH ( u -- u )
code_UWIDTH:
	xor ecx,ecx
	pop eax
	mov ebx,var_BASE
	mov ebx,[ebx]
.loop:
	inc ecx
	xor edx,edx
	div ebx
	or eax,eax
	jnz .loop
	push ecx
	NEXT

; .R ( x u -- ) display a signed number x padded to width u
code_DOTR:
	pop ecx ; width
	pop eax ; #
	bt eax,31
	pushf
	jnc .skip1
	dec ecx
	neg eax
.skip1:
	popf
	push eax
	push ecx
	jnc .skip2
	mov [emit_scratch],byte '-'
	call _EMIT2
.skip2:
	pop ecx
	call _SPACES
	jmp code_PAREN_UDOT

; STRMATCH ( addr1 addr2 -- -1 | 0 | 1 ) analagous to C strcmp
code_STRMATCH:
	xor edx,edx                   ; zero EDX
	mov eax,esi                   ; save ESI
	mov ebx,edi                   ; save EDI
	mov ecx,MAX_STRING_LENGTH     ; safety guaranteed
	pop esi                       ; ESI = start of string 2
	pop edi                       ; EDI = start of string 1
	push eax
	push ebx
.loop:
	mov al,[esi]                  ; char from string 2
	mov ah,[edi]                  ; char from string 1
	inc esi                       ; next address
	inc edi                       ; next address
	cmp al,ah                     ; compare chars
	jz .match                     ; char 2 == char 1
	jl .lt                        ; char 2 < char 1
	jg .gt                        ; char 2 > char 1
.match:
	or al,ah
	jz .done                      ; done if matching zeroes
	loop .loop                    ; fail if the loop falls through
.fail:
	or al,al                      ; check if char 2 == 0
	jz .gt                        ; long string, but matched 'til now
	or ah,ah                      ; check if char 1 == 0
	jz .lt                        ; lnog string, but matched 'til now
	inc edx                       ; string limit reached-
	inc edx                       ;   return 2 for failure
.done:
	pop edi                       ; restore EDI
	pop esi                       ; restore ESI
	push edx                      ; <TOS> == result
	NEXT                          ; all done
.lt:
	dec edx                       
	jmp .done
.gt:
	inc edx
	jmp .done

; STRCHR / STRIDX (C-style strings only ; see CSTRING)
; search a string for a char ( addr char -- 0 | addr2 | index )
; return the address or index of the first char matched on success
; return -1 on fail
; STRCHR _may_ lend itself to recursive use

code_STRIDX:
	xor ebx,ebx
	bts ebx,7
	clc
	jmp code_STRCHR.strindex

code_STRCHR:
	xor ebx,ebx
.strindex:
	mov ecx,MAX_STRING_LENGTH
	mov edx,edi
	pop eax
	pop edi
.loop:
	scasb
	je .match
	or al,al
	jz .strend
	loop .loop
.fail:
	mov eax,ebx
	bts eax,1
	jmp .done
.match:
	btc ebx,7
	jnc .skip
	mov eax,MAX_STRING_LENGTH
	sub eax,ecx
	jmp .done
.skip:
	dec edi
	mov eax,edi
.done:
	mov edi,edx
	push eax
	NEXT
.strend:
	mov eax,ebx
	inc eax
	neg eax
	jmp .done

include 'data.asm'

