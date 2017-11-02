; vim: set ft=fasm: set ts=2:

; (SGM:) Unless you received this software under a different license:
; THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
; WARRANTY.  IN PARTICULAR, THE AUTHORS MAKE NO
; REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
; OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
; fasm, elf.inc are Copyright (C) 2011 Tomasz Gryzstar. All rights reserved.
; unistd-26.inc is derived GNU/Linux unistd.h. See http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html
; core.asm and data.asm are Copyright (C) 2011 Steven G. Messervey. All rights reserved.

; (SGM:) This version directly generates an ELF executable with fasm
;        this file contains the dictionary proper
; (SGM:) 04-SEP-2010
;        rearranged dictionary to put oft-used words first.
;        Through some shell magic, I have determined the most
;        oft-used internal words from jansforth.f are:
; 53 dup
; 42 swap
; 36 immediate
; 15 drop
; 13 emit
; 10 word
; 10 exit
; 10 char

; (SGM:) 26-OCT-2011
;        The numbers above are skewed by defining words
;        that are now in 'dd ...' statements in the source.
;        'char' was also used around ten times to define
;        shortcut words, so it's apparent usage is skewed.

;;; macros to generate code primitive or word definition headers

; generate a code primitive header
macro CHEAD Qname,Len,Mask,NAME {
name_#NAME:
	dd link
	link=name_#NAME
	db Len + Mask , Qname
align 4
NAME:
	dd code_#NAME
}

; generate a word definition header - note the 'dd DOCOL'
macro DHEAD Qname,Len,Mask,NAME {
name_#NAME:
	dd link
	link=name_#NAME
	db Len + Mask , Qname
align 4
NAME:
	dd DOCOL
}

segment readable 
align 4
cold_start:	dd CLI_PARSE			; High-level code without a codeword.
;cold_start:	dd QUIT					  ; High-level code without a codeword.
errmsg:     db "PARSE ERROR: "
errmsgend:
errmsgnl:   db 0x0A
clsseq:     db 0x1B,'[2J',0x1B,'[1;1H'
clsseqend:

; initialize  the chain of links.
link = 0

CHEAD "ARG0",4,0,ARG0
CHEAD "ARG1",4,0,ARG1
CHEAD "CLS",3,0,CLS
CHEAD "DLEN",4,0,DLEN

if defined DEBUG_STARTUP      ; 
CHEAD "INITSTRUCT",10,0,INITST
DHEAD "DNEXT",5,0,NXT
	dd DUP,INCR4,SWAP,FETCH,UDOT,_CR
	dd EXIT
DHEAD "DUMP_INIT",9,0,DMPINIT
	dd BASE,FETCH,HEX
	dd INITST
	dd LIT,.str1,LIT,6,TYPE,NXT
	dd LIT,.str2,LIT,6,TYPE,NXT
	dd LIT,.str3,LIT,6,TYPE,NXT
	dd LIT,.str4,LIT,6,TYPE,NXT
	dd LIT,.str5,LIT,6,TYPE,NXT
	dd LIT,.str6,LIT,6,TYPE,NXT
	dd LIT,.str7,LIT,6,TYPE,NXT
	dd LIT,.str8,LIT,6,TYPE,NXT
	dd LIT,.str9,LIT,6,TYPE,NXT
	dd DROP,BASE,STORE,QUIT
.str1: db 'EAX : '
.str2: db 'EBX : '
.str3: db 'ECX : '
.str4: db 'EDX : '
.str5: db 'EDI : '
.str6: db 'ESI : '
.str7: db 'ESP : '
.str8: db 'EBP : '
.str9: db 'EIP : '
end if

DHEAD "QUIT",4,0,QUIT
	dd RZ,RSPSTORE
	dd INTERPRET
	dd BRANCH,-8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;   EXPERIMENTAL CLI file parsing SUPPORT   ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; check to see if ARGV[1]..ARGV[ARGC] is non-null.
; if a file fails to be found, all other args are ignored
;;;
; CLI-PARSE
DHEAD "CLI-PARSE",8,F_HIDDEN,CLI_PARSE
  dd LIT,1,DUP
  ;  ARGC == 1 IF?
  dd ARGC,_EQU,ZBRANCH,32
  dd   LIT,.str,LIT,.strlen,TYPE
  dd DROP,QUIT
  ; BEGIN
  dd DUP,ARGC,_EQU,ZBRANCH,12
  ; ARGC == n IF?
  dd   BRANCH,-32
  ; THEN
  dd DUP,INCR,SWAP,CELLS,ARGS,_ADD,FETCH,_INCL,BRANCH,-72
.str: db "NO FILES PARSED",0x0A
.stre:
.strlen = .stre - .str

; ERR0 ( -- ) type "ERROR:"
DHEAD "ERR0",4,F_HIDDEN,ERR0
	dd LIT,errmsg,LIT,errmsgend-errmsg-2,TYPE
	dd EXIT

DHEAD "PRINT",5,0,PRINT
	dd DEPTH,LIT,1,GE,ZBRANCH,20
	dd DUP,STRLEN,TYPE,_CR,EXIT

DHEAD "WC",2,F_IMMED,WORD_COUNT
	dd LIT,0,LATEST
; BEGIN
	dd FETCH,DUP
; 0= IF
	dd ZBRANCH,24
; THEN
	dd SWAP,INCR,SWAP,BRANCH,-32
; REAPEAT
	dd DROP,UDOT,_CR,EXIT

CHEAD "(INTERPRET)",11,0,INTERP
CHEAD "INTERPRET",9,0,INTERPRET
CHEAD "DODOES",6,0,__DODOES
CHEAD "BINARY",6,0,BINARY
CHEAD "OCTAL",5,0,OCTAL
CHEAD "BYE",3,F_IMMED,BYE
CHEAD "OVER",4,0,OVER
CHEAD "ROT",3,0,ROT
CHEAD "-ROT",4,0,NROT
CHEAD "2DROP",5,0,TWODROP
CHEAD "2DUP",4,0,TWODUP
CHEAD "2SWAP",5,0,TWOSWAP
CHEAD "NIP",3,0,NIP
CHEAD "TUCK",4,0,TUCK
CHEAD "PICK",4,0,PICK
CHEAD "?DUP",4,0,QDUP
CHEAD "1+",2,0,INCR
CHEAD "1-",2,0,DECR
CHEAD "4+",2,0,INCR4
CHEAD "4-",2,0,DECR4
CHEAD "+",1,0,_ADD	
CHEAD "-",1,0,_SUB	
CHEAD "*",1,0,_MUL
CHEAD "/",1,0,_DIV
CHEAD "MOD",3,0,_MOD
CHEAD "/MOD",4,0,DIVMOD
CHEAD "U/",2,0,UDIV
CHEAD "UMOD",4,0,UMOD
CHEAD "U/MOD",5,0,UDIVMOD
CHEAD "=",1,0,_EQU
CHEAD "<>",2,0,NEQU
CHEAD "<",1,0,LT
CHEAD ">",1,0,GT
CHEAD "<=",2,0,LE	
CHEAD ">=",2,0,GE	
CHEAD "0=",2,0,ZEQU	
CHEAD "0<>",3,0,ZNEQU
CHEAD "0<",2,0,ZLT
CHEAD "0>",2,0,ZGT
CHEAD "0<=",3,0,ZLE
CHEAD "0>=",3,0,ZGE
CHEAD "AND",3,0,_AND
CHEAD "OR",2,0,_OR
CHEAD "XOR",3,0,_XOR
CHEAD "INVERT",6,0,INVERT
CHEAD "NEGATE",6,0,NEGATE
CHEAD "NOT",3,0,_NOT
CHEAD "TRUE",4,0,TRUE
CHEAD "FALSE",5,0,FALSE
CHEAD "LIT",3,0,LIT
CHEAD "!",1,0,STORE
CHEAD "@",1,0,FETCH
CHEAD "+!",2,0,ADDSTORE
CHEAD "-!",2,0,SUBSTORE
CHEAD "C!",2,0,STOREBYTE
CHEAD "C@",2,0,FETCHBYTE
CHEAD "C@C!",4,0,CCOPY
CHEAD "CMOVE",5,0,CMOVE
CHEAD "CMOVED",6,0,CMOVED
CHEAD "STATE",5,0,STATE
CHEAD "DP",2,0,DP
CHEAD "LATEST",6,0,LATEST
CHEAD "(FDESC)",7,0,FDESC
CHEAD "(FBUFF)",7,0,FBUFF
CHEAD "(FSTAT)",7,0,FSTAT
CHEAD "(ASTAK)",7,0,ASTAK
CHEAD "S0",2,0,S0
CHEAD "BASE",4,0,BASE
CHEAD "VERSION",7,0,VERSION
CHEAD "R0",2,0,RZ
DHEAD "DOCOL",5,0,__DOCOL ; the DHEAD macro provides 'dd DOCOL'
CHEAD "F_IMMED",7,0,__F_IMMED
CHEAD "F_HIDDEN",8,0,__F_HIDDEN
CHEAD "F_LENMASK",9,0,__F_LENMASK
CHEAD "SYS_EXIT",8,0,SYS_EXIT
CHEAD "SYS_OPEN",8,0,SYS_OPEN
CHEAD "SYS_CLOSE",9,0,SYS_CLOSE
CHEAD "SYS_READ",8,0,SYS_READ
CHEAD "SYS_WRITE",9,0,SYS_WRITE
CHEAD "SYS_CREAT",9,0,SYS_CREAT
CHEAD "SYS_STAT",8,0,SYS_STAT
CHEAD "SYS_FSTAT",9,0,SYS_FSTAT
CHEAD "SYS_BRK",7,0,SYS_BRK
CHEAD "SYS_FORK",8,0,SYS_FORK
CHEAD "SYS_EXECVE",10,0,SYS_EXECVE
CHEAD "O_RDONLY",8,0,__O_RDONLY
CHEAD "O_WRONLY",8,0,__O_WRONLY
CHEAD "O_RDWR",6,0,__O_RDWR
CHEAD "O_CREAT",7,0,__O_CREAT
CHEAD "O_EXCL",6,0,__O_EXCL
CHEAD "O_TRUNC",7,0,__O_TRUNC
CHEAD "O_APPEND",8,0,__O_APPEND
CHEAD "O_NONBLOCK",10,0,__O_NONBLOCK
CHEAD "RTLD_LOCAL",10,0,RTLDLOCAL
CHEAD "RTLD_LAZY",9,0,RTLDLAZY
CHEAD "RTLD_NOW",8,0,RTLDNOW
CHEAD "RTLD_BINDING_MASK",17,0,RTLDBINDMASK
CHEAD "RTLD_NOLOAD",11,0,RTLDNOLOAD
CHEAD "RTLD_DEEPBIND",13,0,RTLDDEEPBIND
CHEAD "RTLD_GLOBAL",11,0,RTLDGLOBAL
CHEAD "RTLD_NODELETE",13,0,RTLDNODELETE
CHEAD "DLOPEN",6,0,DLOPEN
CHEAD "DLSYM",5,0,DLSYM
CHEAD "DLERR",5,0,DLERR
CHEAD "DLCLOSE",7,0,DLCLOSE
CHEAD "DYNCALL",7,0,DYNCALL
DHEAD "DLERROR",7,0,DLERROR
	dd DLERR,PRINT
	dd EXIT
; ' ( ccc -- xt )

DHEAD "'",1,0,TICK
	dd PAREN_WORD,PAREN_FIND,TCFA
	dd EXIT

CHEAD "ARGC",4,0,ARGC
CHEAD "ARGS",4,0,ARGS
CHEAD "ENVC",4,0,ENVC
CHEAD "ENVS",4,0,ENVS
CHEAD "(EVAL)",6,0,PAREN_EVAL

DHEAD "_LF",3,0,_LF
	dd LIT,0x0A,EXIT

DHEAD "_CR",3,0,__CR
	dd LIT,0x0D,EXIT

DHEAD "BL",2,0,BLANK
	dd LIT,0x20,EXIT

DHEAD "CR",2,0,_CR
	dd LIT,0x0A,EMIT,EXIT

DHEAD "NEWL",4,0,NEWL
	dd LIT,0x0A,LIT,0x0D,EMIT,EMIT,EXIT

DHEAD "SPACE",5,0,SPACE
	dd LIT,0x20,EMIT,EXIT

CHEAD ">R",2,0,TOR
CHEAD "R>",2,0,FROMR
CHEAD "RSP@",4,0,RSPFETCH
CHEAD "RSP!",4,0,RSPSTORE
CHEAD "RDROP",5,0,RDROP
CHEAD "PSP@",4,0,PSPFETCH
CHEAD "PSP!",4,0,PSPSTORE
CHEAD "AS>",3,0,ASPFROM
CHEAD ">AS",3,0,ASPTO
CHEAD "(STDIN)",7,0,PAREN_STDIN
CHEAD "KEY",3,0,KEY
CHEAD "NUMBER",6,0,NUMBER
CHEAD "(FIND)",6,0,PAREN_FIND
CHEAD ">CFA",4,0,TCFA

DHEAD "CFA>",4,0,FCFA
  dd LATEST,FETCH
  ; BEGIN
  dd QDUP,ZBRANCH,44
  ; WHILE TOS != 0
  dd TWODUP,SWAP,LT,ZBRANCH,12
  ; < IF
  dd NIP,EXIT
  ; THEN
  dd FETCH
  ; REPEAT
  dd BRANCH,-48
  dd DROP,LIT,0
  dd EXIT

; >DFA ( addr -- xt )
DHEAD ">DFA",4,0,TDFA
	dd TCFA                    ; >CFA		(get code field address)
	dd INCR4                   ; 4+		(add 4 to it to get to next word)
	dd EXIT                    ; EXIT		(return from FORTH word)

; :NONAME ( -- addr ) create a word with no name
DHEAD ":NONAME",7,0,COLNONAME
	dd LIT,0,LIT,0,HEADER_COMMA ; we need a dict. header because ';' expects it
	dd HERE		                  ; current DP value = the xt
	dd LIT,DOCOL,COMMA          ; compile DOCOL (the codeword)
	dd RBRAC                    ; go into compile mode
	dd EXIT

CHEAD "HEADER,",7,0,HEADER_COMMA
CHEAD ",",1,0,COMMA
CHEAD "[",1,F_IMMED,LBRAC
CHEAD "]",1,0,RBRAC

DHEAD ":",1,0,COLON	
	dd PAREN_WORD	           	 ; Get the name of the new word
	dd HEADER_COMMA            ; Create the dictionary entry / header
	dd LIT, DOCOL, COMMA       ; Append DOCOL  (the codeword).
	dd LATEST, FETCH, HIDDEN   ; Make the word hidden (see below for definition).
	dd RBRAC                   ; Go into compile mode.
	dd EXIT                    ; Return from the function.

DHEAD ";",1,F_IMMED,SEMICOLON
	dd LIT, EXIT, COMMA        ; Append EXIT (so the word will return).
	dd LATEST, FETCH, HIDDEN   ; Toggle hidden flag -- unhide the word
	dd LBRAC                   ; Go back to IMMEDIATE mode.
	dd EXIT                    ; Return from the function.

DHEAD "HIDE",4,0,HIDE
	dd PAREN_WORD				       ; Get the word (after HIDE).
	dd PAREN_FIND              ; Look up in the dictionary.
	dd HIDDEN                  ; Set F_HIDDEN flag.
	dd EXIT                    ; Return.

CHEAD "HIDDEN",6,0,HIDDEN
CHEAD "[']",3,0,BRACKET_TICK
CHEAD "BRANCH",6,0,BRANCH
CHEAD "0BRANCH",7,0,ZBRANCH
CHEAD "LITSTRING",9,0,LITSTRING
CHEAD "TYPE",4,0,TYPE

; useful for declaring charcter literals
; use like --: 'A' [ CHAR A ] LITERAL ;
DHEAD "LITERAL",7,F_IMMED,LITERAL
	dd BRACKET_TICK,LIT,COMMA,COMMA
	dd EXIT

; C!++ ( x y -- x++ ) factor of some string operations
DHEAD "C!++",4,0,STOREBYTE_INC
	dd OVER,STOREBYTE,INCR
	dd EXIT

; S" ( -- addr u ) temporarily compile the string terminated by '"'
DHEAD 'S"',2,F_IMMED,SQUOTE
	dd STATE,FETCH
	; compiling? IF
	dd ZBRANCH,12
	dd BRACKET_STRING,EXIT
	; ELSE
	dd DP,FETCH
	; BEGIN
	dd KEY
	; WHILE
	dd DUP,LIT,'"',NEQU,ZBRANCH,16
	dd STOREBYTE_INC
	; REPEAT
	dd BRANCH,-36
	dd DROP,DP,FETCH,_SUB,HERE,SWAP 
	dd EXIT

; ." ( -- ) emit the string terminated by '"'
DHEAD '."',2,F_IMMED,DOTQUOTE
	dd STATE,FETCH
	; compiling? IF
	dd ZBRANCH,24
	dd BRACKET_STRING,LIT,TYPE,COMMA,EXIT
	; ELSE
	; BEGIN
	dd KEY
	; WHILE
	dd DUP,LIT,'"',NEQU,ZBRANCH,16
	dd EMIT,BRANCH,-36
	; REPEAT
	dd DROP
	dd EXIT

; Q" ( -- addr u ) leave a quoted string in (addr,len) format on <TOS>
DHEAD 'Q"',2,F_IMMED,QQUOTE
	dd HERE,DUP,LIT,34,STOREBYTE_INC,DUP
	dd PAREN_WORD,CMOVED,_ADD
	dd HERE,_SUB
	dd EXIT

; (") ( -- c ) emit a double quote
DHEAD '(")',3,0,PQUOTE
	dd LIT,34,EMIT
	dd EXIT

; QUOTE - emit a quoted string, with leading and trailing space,
; or compile the the string + emit
; Useful for HTML/XML attributes
; : QUOTE IMMEDIATE 
;   ['] LITSTRING , HERE 0 , 
;   34 C, HERE (WORD) CMOVED DUP DP @ + DP ! 34 C, 32 C, 3 + SWAP ! ALIGN 
;   ['] TYPE , 
; ;
; QUOTE ( ccc -- ) execution | ( ccc -- addr u ) interpretation
DHEAD 'QUOTE',5,F_IMMED,QUOTED
	dd STATE,FETCH,ZBRANCH,132
	; COMPILING? IF
	dd BRACKET_TICK,LITSTRING,COMMA
	dd HERE,LIT,0,COMMA
	dd LIT,34,CHARCOMMA
	dd HERE,PAREN_WORD,CMOVED,DUP,DP,ADDSTORE
	dd LIT,34,CHARCOMMA,LIT,32,CHARCOMMA
	dd LIT,3,_ADD,SWAP,STORE,_ALIGN
	dd BRACKET_TICK,TYPE,COMMA
	dd EXIT
	; THEN
	dd PQUOTE,PAREN_WORD,TYPE,PQUOTE
	dd EXIT

; Z" - emit a zero-terminated string, or leave the address of one on <TOS>
; Z" ( ccc -- ) execution | ( ccc -- addr ) interpretation
DHEAD 'Z"',2,F_IMMED,ZQUOTE
	dd STATE,FETCH,ZBRANCH,100
	; compiling? IF
	dd BRACKET_TICK,LITSTRING,COMMA
	dd HERE,LIT,0,COMMA
	dd HERE,DUP,PAREN_WORD,CMOVED,_ADD,LIT,0,SWAP,STOREBYTE
	dd DUP,DP,FETCH,_ADD,DP,STORE
	dd SWAP,STORE,_ALIGN
	dd BRACKET_TICK,DROP,COMMA
	dd EXIT
	; ELSE
	dd HERE,DUP,DUP,PAREN_WORD,DECR,CMOVED,_ADD,LIT,0,SWAP,STOREBYTE
	dd EXIT

; CSTRING - convert an (addr,len) pair to a zero-terminated string
;  CSTRING ( addr u -- addr2 )
DHEAD "CSTRING",7,0,CSTRING ; ( addr len -- c-addr )
	dd SWAP,OVER
	dd HERE,NROT
	dd CMOVE                ; len
	dd HERE,_ADD            ; daddr+len
	dd LIT,0,SWAP,STOREBYTE ; store terminating NULL char
	dd HERE                 ; push start address
	dd EXIT

; STRLEN ( addr -- u ) calculate the length of a zero-terminated string
DHEAD "STRLEN",6,0,STRLEN
	dd DUP                            ; save start address
	; BEGIN
	dd DUP,FETCHBYTE,ZNEQU,ZBRANCH,16 ; zero byte found?
	; 0<> WHILE
	dd INCR
	; REPEAT
	dd BRANCH,-28
	dd SWAP,_SUB                      ; calculate the length
	dd EXIT

; EXPERIMENTAL: simple runtime 'if/else'
; if ( ccc -- xt )
DHEAD "if",2,0,_if
	dd TICK,EXIT

; else ( f xt -- )
DHEAD "else",4,0,_else
	dd SWAP,ZEQU,ZBRANCH,12,DROP,EXIT
	dd EXECUTE,PAREN_WORD,TWODROP
	dd EXIT

; vector-stlye array. Useful structured data primitve
DHEAD "ARRAY",5,0,ARRAY
	dd CREATE
	dd   CELLS,ALLOT
	dd DOES
	dd   SWAP,CELLS,_ADD
	dd EXIT

CHEAD "STRMATCH",8,0,STRMATCH
CHEAD "STRIDX",6,0,STRIDX
CHEAD "STRCHR",6,0,STRCHR

DHEAD "ALLOT",5,0,ALLOT
	dd DP,ADDSTORE
	dd EXIT

CHEAD "CELLS",5,0,CELLS

; CREATE/DOES etc courtesy c.l.f. / Richard Russell

DHEAD "CREATE",6,0,CREATE
	dd PAREN_WORD,HEADER_COMMA,LIT,DODOES,COMMA,LIT,0,COMMA
	dd EXIT

DHEAD "DOES",4,0,DOES
	dd FROMR,LATEST,FETCH,TDFA,STORE
	dd EXIT

DHEAD ">BODY",5,0,TOBODY      ; ??? what is this good for?
	dd 8,_ADD                     ; 2 CELLS +
	dd EXIT

DHEAD "VAR",3,0,VAR
	dd PAREN_WORD,HEADER_COMMA
	dd LIT,DODOES,COMMA,LIT,0,COMMA
	dd LIT,4,ALLOT ; allocate 1 cell of memory (4 bytes on x86-32)
	dd EXIT

DHEAD "CONST",5,0,CONSTANT
	dd PAREN_WORD,HEADER_COMMA,LIT,DOCOL,COMMA
	dd LIT,LIT,COMMA,COMMA,LIT,EXIT,COMMA
	dd EXIT

CHEAD "[:WORD]",7,0,BRACKET_WORD
CHEAD "EXECUTE",7,0,EXECUTE
CHEAD "SYSCALL3",8,0,SYSCALL3
CHEAD "SYSCALL2",8,0,SYSCALL2
CHEAD "SYSCALL1",8,0,SYSCALL1
CHEAD "SYSCALL0",8,0,SYSCALL0
CHEAD "#!",2,0,SHEBANG
CHEAD "ENV?",4,0,ENVQ
CHEAD "GET-BRK",7,0,GETBRK
CHEAD "BRK",3,0,BRK
CHEAD "MORECORE",8,0,MORECORE
CHEAD "UNUSED",6,0,UNUSED
CHEAD "R/O",3,0,READONLY
CHEAD "R/W",3,0,READWRITE

; OPEN-FILE ( addr u fam -- fd 0 | addr u fam -- errno -1 ) 0 for success
DHEAD "OPEN-FILE",9,0,FOPEN
	dd NROT		               ; fam addr u
	dd CSTRING		           ; fam cstring
	dd SYS_OPEN,SYSCALL2     ; open (filename, flags)
	dd DUP,ZLT,ZBRANCH,20    ; errno?
	; 0< IF
	dd NEGATE,LIT,-1,EXIT    ; errno -1
	; ELSE
	dd LIT,0                 ; fd 0
	; THEN
	dd EXIT

; CREATE-FILE ( addr u fam -- fd 0 | addr u fam -- errno -1 ) 0 for success
DHEAD "CREATE-FILE",11,0,FCREAT
	dd LIT,0x240,_OR          ; fam | O_CREAT | O_TRUNC 
	dd NROT                   ; fam addr u
	dd CSTRING                ; fam cstring
	dd LIT,420,NROT           ; 0644 fam cstring )
	dd SYS_OPEN,SYSCALL3      ; open (filename, flags|O_TRUNC|O_CREAT, 0644)
	dd DUP,ZLT,ZBRANCH,20     ; errno?
	; 0< IF
	dd NEGATE,LIT,-1,EXIT     ; errno -1
	;ELSE
	dd LIT,0                  ; fd 0
	;THEN
	dd EXIT

; CLOSE-FILE ( fd -- 0 | errno ) 0 for success
DHEAD "CLOSE-FILE",10,0,FCLOSE
	dd SYS_CLOSE,SYSCALL1
	dd NEGATE
	dd EXIT

;  ( u addr fd -- 0 | +x | -x ) -x for failure, |x| is errno
DHEAD "READ-FILE",9,0,FREAD
	dd SYS_READ,SYSCALL3
	dd EXIT

; ERRMSG ( x -- ) print 'ERROR: ' follwed by <TOS> as an UINT32 in current BASE
DHEAD "ERRMSG",6,0,ERRMSG
	dd LIT,.str,LIT,.strend-.str,TYPE,DOT,_CR
	dd EXIT
.str:
	db 'ERROR: '
.strend:

; FLOAD - load the content of a file into memory
; caution - memory the size of the file's contents
; is allocated using BRK ; a pointer to this buffer
; is available via the word, '(FBUFF)'
DHEAD "FLOAD",5,0,FLOAD
	dd CSTRING,READONLY,SWAP,SYS_OPEN,SYSCALL2               ; ( string -- fd | n )
	dd DUP,LIT,-1,_EQU,ZBRANCH,36                            ; ( fd -- | -- fd )
	; -1 = IF 
	dd   ERRMSG,LIT,.s1,LIT,.s1e-.s1,TYPE,_CR,EXIT           ; fopen err
	; THEN
  dd DUP,FDESC,STORE                                       ; ( fd -- fd )
	dd FSTAT,SWAP,SYS_FSTAT,SYSCALL2                         ; ( fd -- n )
	dd DUP,ZNEQU,ZBRANCH,36
	; 0<> IF ( n -- )
	dd ERRMSG,LIT,.s2,LIT,.s2e-.s2,TYPE,_CR,EXIT             ; (  --  )
	; THEN
	dd DROP
	dd FSTAT,LIT,20,_ADD,FETCH,DUP,LIT,2,_DIV,MORECORE       ; ( -- u n )
	dd ZLT,ZBRANCH,36
	; 0< IF ( u n -- u )
	dd   ERRMSG,LIT,.s3,LIT,.s3e-.s3,TYPE,_CR,EXIT           ; ( u -- | u -- u )
	; THEN
	dd DUP,HERE,UNUSED,_ADD,SWAP,_SUB,DUP,FBUFF,STORE        ; ( u -- u1 addr )
	; BEGIN
	dd   TWODUP,FDESC,FETCH,SYS_READ,SYSCALL3                ; ( u addr -- u addr n )
  ;  ( u addr n -- | u addr n )
  dd   DUP,LIT,-1,_EQU,ZBRANCH,60
	; -1 = IF
	dd     ERRMSG,LIT,.s4,LIT,.s4e-.s4,TYPE,_CR
	dd     DROP,FDESC,FETCH,SYS_CLOSE,SYSCALL1,DROP,EXIT
	; THEN
  dd   DUP,NROT,_ADD,NROT,DUP,NROT,_SUB,SWAP               ; ( u addr n  -- addr2 n2 n )
  dd   ZEQU,ZBRANCH,-140
	; 0= IF
	dd     FDESC,FETCH,SYS_CLOSE,SYSCALL1,DROP,TWODROP
	dd     LIT,.s5,LIT,.s5e-.s5,TYPE,_CR,EXIT
  ; AGAIN
.s1: db 'FILE OPEN'
.s1e:
.s2: db 'FILE STAT'
.s2e:
.s3: db 'FILE READ'
.s3e:
.s4: db 'MEMORY'
.s4e:
.s5: db 'FILE LOADED'
.s5e:

DHEAD "DBG",3,0,DBUG
	dd LIT,.msg,LIT,.msgend - .msg,TYPE,EXIT
.msg: db 'DEBUGGERY',0x0A
.msgend:


; INCLUDE - file loading, flow-style
; takes an (addr,len) string off <TOS>, opens the file,
; interprets the file's contents, closes the file,
; and resumes interpreting STDIN
DHEAD "INCLUDE",7,0,INCL
	dd CSTRING,_INCL
DHEAD "_INCLUDE",8,0,_INCL
	dd READONLY,SWAP,SYS_OPEN,SYSCALL2  ; ( string -- fd | n )
	dd DUP,ZLT,ZBRANCH,36
	; 0< IF 
	dd NEGATE,ERRMSG,LIT,.str,LIT,.strend-.str,TYPE,EXIT
	; THEN
	dd PAREN_EVAL
	dd QUIT 
.str: db 'COULD NOT OPEN FILE',0x0A
.strend:

CHEAD "SPACES",6,0,SPACES
CHEAD "(U.)",4,0,PAREN_UDOT
CHEAD "UWIDTH",6,0,UWIDTH
; U.R ( u2 u1 -- ) display unsigned integer u2 padded to u1
DHEAD "U.R",3,0,UDOTR
	dd SWAP,DUP,UWIDTH,ROT,SWAP,_SUB,SPACES,PAREN_UDOT
	dd EXIT
CHEAD ".R",2,0,DOTR
; ( x -- ) . display the DWORD on <TOS> as a signed integer
DHEAD ".",1,0,DOT
	dd LIT,0,DOTR,SPACE
	dd EXIT
; U. ( u -- ) display the DWORD on <TOS> as an unsigned integer
DHEAD "U.",2,0,UDOT
	dd PAREN_UDOT,SPACE
	dd EXIT
; ? ( x -- ) fetch the DWORD at x and display it as a signed integer
DHEAD "?",1,0,FETCHDOT
	dd FETCH,DOT
	dd EXIT
CHEAD "WITHIN",6,0,WITHIN
CHEAD "DECIMAL",7,0,DECIMAL
CHEAD "HEX",3,0,HEX
CHEAD "DEPTH",5,0,DEPTH
CHEAD "ALIGNED",7,0,ALIGNED
CHEAD "C,",2,0,CHARCOMMA

; .S ( -- ) display the contents of the stack as signed integers
DHEAD ".S",2,0,DOTESS
	dd DEPTH
	dd DUP,ZNEQU,ZBRANCH,36
	; if TOS != 0 
	dd   DECR,DUP,TOR,PICK,DOT,FROMR,BRANCH,-44
	; else
	dd   DROP,_CR,EXIT

; ALIGN ( -- ) align the memory at HERE to DWORD
DHEAD "ALIGN",5,0,_ALIGN
	dd HERE,ALIGNED,DP,STORE 
	dd EXIT

; [STRING] ( -- ) factor of string compiliation?
DHEAD "[STRING]",8,F_IMMED,BRACKET_STRING
	dd LIT,LITSTRING,COMMA
	dd HERE,LIT,0,COMMA
	dd LIT,'"',BRACKET_WORD,SWAP
	dd DUP,FETCHBYTE,DUP,LIT,'"',NEQU,ZBRANCH,20
	dd CHARCOMMA,INCR,BRANCH,-44
	dd TWODROP,SWAP,STORE,_ALIGN
	dd EXIT

; primitive loops, advanced results...
CHEAD "IF",2,F_IMMED,_IF
CHEAD "THEN",4,F_IMMED,_THEN
CHEAD "ELSE",4,F_IMMED,_ELSE
CHEAD "BEGIN",5,F_IMMED,BEGIN
CHEAD "UNTIL",5,F_IMMED,UNTIL
CHEAD "AGAIN",5,F_IMMED,AGAIN
CHEAD "WHILE",5,F_IMMED,_WHILE
CHEAD "REPEAT",6,F_IMMED,_REPEAT
CHEAD "UNLESS",6,F_IMMED,UNLESS

CHEAD "CASE",4,F_IMMED,CASE
; OF ( -- )

DHEAD "OF",2,F_IMMED,OF
	dd LIT,OVER,COMMA
	dd LIT,_EQU,COMMA
	dd LIT,ZBRANCH,COMMA,HERE,LIT,0,COMMA
	dd LIT,DROP,COMMA
	dd EXIT
; ENDOF ( -- )

DHEAD "ENDOF",5,F_IMMED,ENDOF
	dd LIT,BRANCH,COMMA,HERE,LIT,0,COMMA,SWAP
	dd DUP,HERE,SWAP,_SUB,SWAP,STORE 
	dd EXIT
; ENDOF ( addr -- )

DHEAD "ENDCASE",7,F_IMMED,ENDCASE
	dd LIT,DROP,COMMA
	; BEGIN
	dd QDUP,ZBRANCH,36
	; WHILE TOS != 0
	dd DUP,HERE,SWAP,_SUB,SWAP,STORE
	dd BRANCH,-40
	dd EXIT

CHEAD "(",1,F_IMMED,LPAREN
CHEAD "CHAR",4,0,CHAR

;;; TOP TEN ;;;
CHEAD "HERE",4,0,HERE
CHEAD "EXIT",4,0,EXIT
CHEAD "(WORD)",6,0,PAREN_WORD
CHEAD "EMIT",4,0,EMIT
CHEAD "DROP",4,0,DROP
CHEAD "IMMEDIATE",9,0,IMMEDIATE
CHEAD "SWAP",4,0,SWAP
CHEAD "DUP",3,0,DUP

;;;;; .data
segment readable writeable
align 4

;;; DEBUGGERY ;;;
if defined DEBUG_STARTUP      ; 
INIT:
.eax: dd 0
.ebx: dd 0
.ecx: dd 0
.edx: dd 0
.edi: dd 0
.esi: dd 0
.esp: dd 0
.ebp: dd 0
.eip: dd 0
END_INIT:
end if

var_ARG0:              dd 0
var_ARG1:              dd 0
var_ARG2:              dd 0
var_ARG3:              dd 0
var_ARG4:              dd 0
var_ARG5:              dd 0
var_ARG6:              dd 0
var_ARG7:              dd 0
;var_ARG0:    times 32 db 0
;var_ARG1:    times 32 db 0
;var_ARG2:    times 32 db 0
;var_ARG3:    times 32 db 0
;var_ARG4:    times 32 db 0
;var_ARG5:    times 32 db 0
;var_ARG6:    times 32 db 0
;var_ARG7:    times 32 db 0
;var_ARGC:             dd 0
;var_ARGS:             dd var_ARG0,var_ARG1,var_ARG2,var_ARG3
;                      dd var_ARG4,var_ARG5,var_ARG6,var_ARG7
var_ARGC:             dd 0
var_ARGS:             dd 0
var_ENVC:             dd 0
var_ENVS:             dd 0
var_STATE:            dd 0
var_DP:               dd 0
var_LATEST:           dd LAST_ENTRY
var_S0:               dd 0
var_BASE:             dd 10
var_FDESC:            dd 0      ; spare DWORD for a file descriptor
var_FBUFF:            dd 0      ; spare DWORD for a pointer to file buffer
var_FSTAT: times 24   dd 0      ; enough memory to hold a 'stat' struct
var_ASTAK:            dd alt_stack_top ; alternate stack pointer
word_buffer: times 32 db 0      ; static buffer for WORD
interpret_is_lit:	    dd 0	    ; flag indicating parsing a literal
currinp:              dd 0      ; current file descriptor, most often stdin
currkey:              dd buffer ; current address in input buffer
bufftop:              dd buffer ; last valid data in input buffer + 1.
emit_scratch:         dd 0,0,0         ; scratch used by 'EMIT' and 'U.'
                      db 0,0,0
udottop:              db 0
ENDPROGRAM:

;;;;; .bss
segment readable writeable
align 4096
return_stack: rb RETURN_STACK_SIZE
return_stack_top:						; Initial top of return stack.
align 4096
alt_stack:    rb ALT_STACK_SIZE
alt_stack_top:              ; Initial top of alternate stack
align 4096
buffer: rb BUFFER_SIZE			; Temporary input buffer when reading from files or the terminal.
