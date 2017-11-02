\ vim: set ft=forth:
: BASE? BASE DUP @ DUP DECIMAL U. SWAP ! ;
: HEXA BASE @ SWAP DUP HEX U. SWAP BASE ! ;
: ENV ENVS BEGIN DUP @ ?DUP 0= IF 2DROP EXIT THEN DUP STRLEN TYPE CR 4+ AGAIN ;
: BZERO CELLS BEGIN DUP -1 > WHILE SWAP DUP 1+ SWAP 0 SWAP C! SWAP 1- REPEAT DROP ;
: FORGET (WORD) (FIND) ?DUP 0= IF EXIT THEN 4+ DUP C@ HIDDEN AND ! ;
: y ." YES" CR ;
: n ." NO" CR ;
\ jonesforth used the following method declare character literals -
\ : ':' [ CHAR : ] LITERAL ;
\ While clever, it simply isn't necessary for character literals
\ So, just use ASCII.
: ':' 58 ;
: ';' 59 ;
: '(' 40 ;
: ')' 41 ;
: '"' 34 ;
: 'A' 65 ;
: '0' 48 ;
: '-' 45 ;
: '.' 46 ;

\ factor of SEE
: ID.
	4+		        \ skip over the link pointer
	DUP C@		    \ get the flags/length byte
	F_LENMASK AND	\ mask out the flags - just want the length

	BEGIN
		DUP 0<>		  \ length != 0?
	WHILE
		SWAP 1+		  \ addr len -- len addr+1
		DUP C@		  \ len addr -- len addr char | get the next character
		EMIT		    \ len addr char -- len addr | and print it
		SWAP 1-		  \ len addr -- addr len-1    | subtract one from length
	REPEAT
	2DROP		      \ len addr --
;

\ factor of SEE
: ?HIDDEN
	4+		            \ skip over the link pointer
	C@		            \ get the flags/length byte
	F_HIDDEN AND 0<>	\ mask the F_HIDDEN flag and return it (as a truth value)
;

\ factor of SEE
: ?IMMEDIATE
	4+		          \ skip over the link pointer
	C@		          \ get the flags/length byte
	F_IMMED AND	0<> \ mask the F_IMMED flag and return it (as a truth value)
;

: SEE
	(WORD) 
	(FIND)	 \ find the dictionary entry to decompile
	DUP 0= IF DROP ." NOT FOUND" CR EXIT THEN
	\ Now we search again, looking for the next word in the dictionary.
	\ This gives us the length of the word that we will be decompiling.
	\ (Well, mostly it does).
	HERE		       \ address of the end of the last compiled word
	LATEST @	     \ word last curr
	BEGIN
		2 PICK		   \ word last curr word
		OVER		     \ word last curr word curr
		<>		       \ word last curr word<>curr?
	WHILE			     \ word last curr
		NIP		       \ word curr
		DUP @		     \ word curr prev (which becomes: word last curr)
	REPEAT

	DROP		       \ at this point, the stack is: start-of-word end-of-word
	SWAP		       \ end-of-word start-of-word

	\ begin the definition with : NAME [IMMEDIATE]
	':' EMIT SPACE DUP ID. SPACE
	DUP ?IMMEDIATE IF ." IMMEDIATE " THEN
	>DFA		       \ get the data address, ie. points after DOCOL | end-of-word start-of-data

	\ now we start decompiling until we hit the end of the word
	BEGIN		       \ end start
		2DUP >
	WHILE
		DUP @		     \ end start codeword

		CASE
		['] LIT OF	 \ is it LIT ?
			4 + DUP @	 \ get next word which is the integer constant
			.			     \ and print it
		ENDOF
		['] LITSTRING OF \ is it LITSTRING ?
			83 EMIT '"' EMIT SPACE
			4 + DUP @	     \ get the length word
			SWAP 4 + SWAP	 \ end start+4 length
			2DUP TYPE		   \ print the string
			'"' EMIT SPACE \ finish the string with a final quote
			+ ALIGNED		   \ end start+4+len, aligned
			4 -			       \ because we're about to add 4 below
		ENDOF
		['] 0BRANCH OF	\ is it 0BRANCH ?
			." 0BRANCH ( "
			4 + DUP @		  \ print the offset
			.
			." ) "
		ENDOF
		['] BRANCH OF		\ is it BRANCH ?
			." BRANCH ( "
			4 + DUP @		  \ print the offset
			.
			." ) "
		ENDOF
		['] ['] OF			\ is it ['] (BRACKET_TICK) ?
			." ['] "
			4 + DUP @     \ get the next codeword
			CFA>			    \ and force it to be printed as a dictionary entry
			ID. SPACE
		ENDOF
		['] EXIT OF		  \ is it EXIT?
		\ We expect the last word to be EXIT, and if it is then we don't print it
		\ because EXIT is normally implied by ;.  EXIT can also appear in the middle
		\ of words, and then it needs to be printed.
			2DUP			    \ end start end start
			4 +			      \ end start end start+4
			<> IF			    \ end start | we're not at the end
				." EXIT "
			THEN
		ENDOF
		\ default case:
			DUP			      \ in the default case we always need to DUP before using
			CFA>			    \ look up the codeword to get the dictionary entry
			ID. SPACE		  \ and print it
		ENDCASE
		4 +		          \ end start+4
	REPEAT

	';' EMIT CR

	2DROP		          \ restore stack
;

\ factor of DUMP
: @8. 
DUP 8 2DUP U.R SPACE
BEGIN ?DUP 0> WHILE
	1- SWAP DUP C@ 3 U.R SPACE 1+ SWAP
REPEAT
DROP
;

\ factor of DUMP
: @8E
8
BEGIN ?DUP 0> WHILE
	1- SWAP DUP C@ DUP
	32 126 ROT
	WITHIN 0= IF DROP 46 THEN
	EMIT SPACE 1+ SWAP
REPEAT
;

\ DUMP - dump u items starting at addr
\ ( addr u -- )
VAR __CURRENT_BASE
: DUMP
	BASE @ __CURRENT_BASE ! HEX
BEGIN
	?DUP 0> WHILE
		1- SWAP @8. @8E CR SWAP
REPEAT
	DROP __CURRENT_BASE @ BASE !
;

\ /proc/self/maps interface
\ *** KERNEL MUST SUPPORT PROCFS ***
\ /proc/self/maps has no 'length',
\ so we can't just FLOAD it.
VAR bfPSM 2044 ALLOT ( shouldn't be more than 1KB... )
: /proc/self/maps
S" /proc/self/maps" R/O READ-FILE
?DUP 0= IF
	0
	BEGIN
		OVER SWAP bfPSM + 1024 SWAP ROT READ-FILE 
		?DUP 0= IF CLOSE-FILE DROP bfPSM PRINT EXIT THEN
	AGAIN
ELSE
	ERRMSG
THEN
;

\ $CELL reserves a DWORD and pushes it's address on the stack
: $CELL HERE 4 ALLOT ;
\ $CELLS reserves <TOS> DWORDs and pushes the origin address on the stack
: $CELLS HERE SWAP CELLS ALLOT ;

\ ---------------------------------+
\ EXPERIMENTAL PAGE BUFFER SUPPORT |
\   I thought this might be useful |
\  for web programming / CGI...    |
\ ---------------------------------+
\ PAGE : 
\  CREATEs 2 DWORDS and <TOS>*cells sized-buffer, DOES pushes 2 addresses:
\  1st DWORD - holds the current offeset into buffer and is init'd to 0,
\  2nd DWORD - holds the addrress of the next free byte in the buffer
: PAGE CREATE $CELL $CELL ROT $CELLS SWAP ! 0 SWAP ! DOES DUP DUP 4+ SWAP @ + ;
\ : PAGE CREATE $CELL $CELL 1024 $CELLS SWAP ! 0 SWAP ! DOES DUP DUP 4+ SWAP @ + ;
\ PAGE+ : update the buffer on <TOS>
: PAGE+ DUP 1+ SWAP 2SWAP ROT CMOVE OVER @ + SWAP ! ;
\ .PAGE : write the page buffer to current output
: .PAGE DROP DUP 4+ SWAP @ TYPE CR ;

\ 

CLS ." DEBUGGING WORDS AVAILABLE" CR

