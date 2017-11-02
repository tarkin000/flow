\ vim: set ft=fforth: ts=2:
\ libhello.so exports a single function:
\ void hello()
\ I drop the return value from DYNCALL because:
\ A) functions returning VOID don't really return anything,
\ and B) whatever is left in EAX after hello is garbage -
\ though, after executing this a couple of times, I get
\ the integer '14' consistently. I expect that number to
\ change after say, a reboot...
VAR SO
VAR SYM
RTLD_NOW S" ./libhello.so" CSTRING DLOPEN SO !
\ should check for errors here, after DLOPEN ...
S" hello" CSTRING SO @ DLSYM SYM !
0 SYM @ DYNCALL DROP
SO @ DLCLOSE DROP \ don't care if it failed
BYE
