; *******************************************************************
; *** This software is copyright 2004 by Michael H Riley          ***
; *** You have permission to use, modify, copy, and distribute    ***
; *** this software so long as this copyright notice is retained. ***
; *** This software may not be used in commercial applications    ***
; *** without express written permission from the author.         ***
; *******************************************************************

; R7 - Program counter
; R8 - Line pointer
; R9 - Data block
; RA - Stack pointer

include    bios.inc
include    kernel.inc

           org     8000h
           lbr     0ff00h
           db      'pilot',0
           dw      9000h
           dw      endrom+7000h
           dw      2000h
           dw      endrom-2000h
           dw      2000h
           db      0
 
           org     2000h
           br      start

include    date.inc
include    build.inc
           db      'Written by Michael H. Riley',0
matched:   db      0

start:     sep     scall               ; display header
           dw      o_inmsg
           db      'Rc/Pilot 0.1',10,13,0
           lda     ra                  ; move past any spaces
           smi     ' '
           lbz     start
           dec     ra                  ; move back to non-space character
           ghi     ra                  ; copy argument address to rf
           phi     rf
           glo     ra
           plo     rf
loop1:     lda     rf                  ; look for first less <= space
           smi     33
           lbdf    loop1
           dec     rf                  ; backup to char
           ldi     0                   ; need proper termination
           str     rf
           ghi     ra                  ; back to beginning of name
           phi     rf
           glo     ra
           plo     rf
           ldn     rf                  ; get byte from argument
           lbnz    good                ; jump if filename given
           sep     scall               ; otherwise display usage message
           dw      o_inmsg
           db      'Usage: pilot filename',10,13,0
           sep     sret                ; and return to os
good:      ldi     high fildes         ; get file descriptor
           phi     rd
           ldi     low fildes
           plo     rd
           ldi     0                   ; flags for open
           plo     r7
           sep     scall               ; attempt to open file
           dw      o_open
           lbnf    load                ; jump if file was opened
           ldi     high errmsg         ; get error message
           phi     rf
           ldi     low errmsg
           plo     rf
           sep     scall               ; display it
           dw      o_msg
           lbr     o_wrmboot           ; and return to os

; *****************************************
; ***** File opened, load the program *****
; *****************************************
load:      mov     r7,program          ; point to program space
           mov     r8,program+1        ; where first text byte will go
           ldi     0                   ; line size
           plo     r9
loadlp:    mov     rc,128              ; attempt to read 128 bytes
           mov     rf,buffer           ; transfer buffer
           sep     scall               ; read next block of bytes
           dw      o_read
           glo     rc                  ; see if bytes were read
           lbnz    load1               ; jump if so
           sep     scall               ; close the file
           dw      o_close
           glo     r9                  ; get line size
           lbz     load_dn             ; jump if no bytes
           adi     2                   ; +2 for size byte and terminator
           str     r7                  ; store line size at beginning of line
load_dn:   ldi     0                   ; write terminator to program space
           str     r8
           lbr     run                 ; then jump to run program
load1:     mov     rf,buffer           ; point to beginning of buffer
load1_lp:  lda     rf                  ; get byte from buffer
           plo     re                  ; save a copy
           smi     10                  ; check for end of line
           lbz     load_eol            ; jump if so
           smi     3                   ; check for 0x0c as well
           lbz     load_eol            ; jump if so
           glo     re                  ; recover byte
           smi     ' '                 ; check for space
           lbnz    load2               ; jump if not
           glo     r9                  ; get line size
           lbz     load3               ; do not store leading spaces
load2:     glo     re                  ; recover byte
           str     r8                  ; store byte into program space
           inc     r8
           inc     r9                  ; increment line size
load3:     dec     rc                  ; decrement block count
           glo     rc                  ; see if done with block
           lbz     loadlp              ; jump to load next block
           lbr     load1_lp            ; loop back to next char in block
load_eol:  glo     r9                  ; check line size
           lbz     load3               ; do nothing if empty line
           adi     2                   ; +2 for size byte and terminator
           str     r7                  ; store line size
           ldi     0                   ; terminate current line
           str     r8
           inc     r8
           plo     r9                  ; reset line count
           mov     r7,r8               ; next start position
           inc     r8                  ; where next text byte goes
           lbr     load3               ; keep processing input

run:       mov     r7,program          ; point to first program line
           mov     r9,matched          ; point to data block
           ldi     0                   ; clear matched flag
           str     r9
           mov     ra,pcstack          ; set initial stack pointer
           mov     rf,accept           ; clear input buffer
           ldi     0
           str     rf

runloop:   ldn     r7                  ; get line length byte
           lbz     progend             ; jump if no more program
           mov     r8,r7               ; set R8 to program line text
           inc     r8
           sep     scall               ; move past any leading spaces
           dw      trim
           ldn     r8                  ; get first byte of line
           smi     '*'                 ; check for label
           lbnz    nolabel             ; jump if not
runlp1:    lda     r8                  ; get next character
           lbz     lineend             ; jump if line terminator found
           smi     ' '                 ; look for end of label
           lbnz    runlp1              ; loop until a space is found
           sep     scall               ; now move past any more spaces
           dw      trim
nolabel:   lda     r8                  ; get command byte
           ani     05fh                ; make sure it is uppercase
           plo     rb

; *******************************************
; ***** Check for conditional execution *****
; *******************************************
           sep     scall               ; move past any spaces
           dw      trim
           ldn     r8                  ; get byte from program
           smi     'N'                 ; check for no match condition
           lbz     c_nomatch           ; jump if so
           ldn     r8                  ; check lc n
           smi     'n'
           lbz     c_nomatch
           ldn     r8                  ; check for y/Y
           smi     'Y'
           lbz     c_ysmatch
           ldn     r8
           smi     'y'
           lbz     c_ysmatch
           lbr     c_nocond            ; no condition
c_nomatch: inc     r8                  ; move past condition byte
           ldi     low matched         ; get matched flag
           plo     r9
           ldn     r9
           lbnz    lineend             ; skip command if match is true
           lbr     c_nocond            ; otherwise process command
c_ysmatch: inc     r8                  ; move past condition byte
           ldi     low matched         ; get matched flag
           plo     r9
           ldn     r9
           lbz     lineend             ; skip command if match is false
           lbr     c_nocond            ; otherwise process command
c_nocond:  sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; get next byte
           smi     ':'                 ; must be a colon
           lbnz    synerr              ; jump if not

           glo     rb                  ; get command
           smi     'A'                 ; check for A command
           lbz     cmd_a
           glo     rb                  ; get command
           smi     'E'                 ; check for E command
           lbz     cmd_e
           glo     rb                  ; check for T command
           smi     'T'
           lbz     cmd_t
           glo     rb                  ; check for N command
           smi     'N'
           lbz     cmd_n
           glo     rb                  ; check for Y command
           smi     'Y'
           lbz     cmd_y
           glo     rb                  ; check for J command
           smi     'J'
           lbz     cmd_j
           glo     rb                  ; check for U command
           smi     'U'
           lbz     cmd_u
           glo     rb                  ; check for R command
           smi     'R'
           lbz     lineend
           glo     rb                  ; check for M command
           smi     'M'
           lbz     cmd_m

           lbr     synerr              ; syntax error if invalid command

lineend:   ldn     r7                  ; get line length
           str     r2                  ; store for add
           glo     r7                  ; add length to position
           add
           plo     r7
           ghi     r7                  ; propagate carry
           adci    0
           phi     r7
           lbr     runloop             ; jump to process next line

; ***********************************
; ***** Command A, Accept input *****
; ***********************************
cmd_a:     sep     scall               ; show prompt
           dw      o_inmsg
           db      '? ',0
           mov     rf,accept           ; point to accept buffer
           sep     scall               ; get input
           dw      o_input
           sep     scall               ; display a cr/lf
           dw      crlf
           lbr     lineend             ; then continue

; **********************************************
; ***** Command E, exit subroutine/program *****
; **********************************************
cmd_e:     glo     ra                  ; see if stack empty
           smi     pcstack.0
           lbnz    cmd_e_ret           ; jump if need to return
           ghi     ra
           smi     pcstack.1
           lbnz    cmd_e_ret
           lbr     o_wrmboot           ; exit program
cmd_e_ret: dec     ra                  ; recover pc from stack
           ldn     ra
           plo     r7
           dec     ra
           ldn     ra
           phi     r7
           lbr     lineend             ; and then continue processing

; ************************************
; ***** Command J, jump to label *****
; ************************************
cmd_j:     sep     scall               ; move past any spaces
           dw      trim
           ldn     r8                  ; get first byte of argument
           smi     '*'                 ; must be a star
           lbnz    synerr              ; otherwise error
           lbr     findline            ; find line with label

; ****************************
; ***** Command M, Match *****
; ****************************
cmd_m:     sep     scall               ; move past leading spaces
           dw      trim
           mov     rf,accept           ; point to accept buffer
cmd_m_lp:  ldn     rf                  ; see if at end of accept buffer
           lbz     cmd_m_no            ; jump if end of string
           push    rf                  ; save accept buffer position
           push    r8                  ; save match string position
cmd_m_cmp: lda     r8                  ; get byte from match string
           lbz     m_yes               ; jump if end found
           str     r2                  ; store for compare
           smi     ','                 ; comma also terminates
           lbz     m_yes
           lda     rf                  ; get byte from accept buffer
           sm                          ; compare to match string
           lbz     cmd_m_cmp           ; jump if a match
cmd_m_nm:  pop     r8                  ; recover addresses
           pop     rf
           inc     rf                  ; point to next character
           lbr     cmd_m_lp            ; and keep looking
m_yes:     pop     r8                  ; recover addresses
           pop     rf
           lbr     cmd_m_yes           ; and signal a match
cmd_m_no:  ldi     low matched         ; point to matched flag
           plo     r9
           ldi     0                   ; signal no match
           str     r9
           lbr     lineend             ; and on to the next line
cmd_m_yes: ldi     low matched         ; point to matched flag
           plo     r9
           ldi     0ffh                ; signal match
           str     r9
           lbr     lineend             ; and on to the next line

; ****************************************
; ***** Command N, type if matched=0 *****
; ****************************************
cmd_n:     ldi     low matched         ; point to matched flag
           plo     r9
           ldn     r9                  ; get matched flag
           lbz     cmd_t               ; jump if matched == 0
           lbr     lineend             ; otherwise ignore rest of line

; ***************************
; ***** Command T, Type *****
; ***************************
cmd_t:     lda     r8                  ; read byte from arguments
           lbz     cmd_t_dn            ; jump if line end found
           plo     re                  ; save character
           smi     '\'                 ; check for backslash
           lbz     lineend             ; this end output without cr/lf
           glo     re                  ; recover character
           sep     scall               ; display byte
           dw      o_type
           lbr     cmd_t               ; loop until end of line
cmd_t_dn:  sep     scall               ; display cr/lf
           dw      crlf
           lbr     lineend             ; and then process next line

; *****************************************
; ***** Command U, jump to subroutine *****
; *****************************************
cmd_u:     sep     scall               ; move past any spaces
           dw      trim
           ldn     r8                  ; get first byte of argument
           smi     '*'                 ; must be a star
           lbnz    synerr              ; otherwise error
           ghi     r7                  ; place current stack pointer on stack
           str     ra
           inc     ra
           glo     r7
           str     ra
           inc     ra
           lbr     findline            ; find line with label

; *****************************************
; ***** Command Y, type if matched<>0 *****
; *****************************************
cmd_y:     ldi     low matched         ; point to matched flag
           plo     r9
           ldn     r9                  ; get matched flag
           lbnz    cmd_t               ; jump if matched != 0
           lbr     lineend             ; otherwise ignore rest of line


synerr:    sep     scall               ; display error message
           dw      o_inmsg
           db      'Syntax error: ',0
errline:   mov     rf,r7               ; point to program line
           inc     rf                  ; program text
           sep     scall               ; display line
           dw      o_msg
           sep     scall               ; display cr/lf
           dw      crlf
           lbr     o_wrmboot           ; and then exit

crlf:      sep     scall
           dw      o_inmsg
           db      10,13,0
           sep     sret


progend:   lbr     o_wrmboot           ; return to Elf/OS

; ************************************************************
; ***** Find line with label in R8 and jump to that line *****
; ************************************************************
findline:  mov     rf,program          ; point to beginning of program
findlp:    ldn     rf                  ; get line size
           lbz     findno              ; jump if label not found
           inc     rf                  ; move to first byte
           ldn     rf                  ; see if first byte is a label
           smi     '*'
           lbnz    find1               ; jump if not
           sep     scall               ; compare labels
           dw      strcmp
           lbdf    findy               ; jump if match
find1:     dec     rf                  ; move back to size byte
           ldn     rf                  ; get size byte
           str     r2                  ; store for add
           glo     rf                  ; and add to line address
           add
           plo     rf
           ghi     rf
           adci    0
           phi     rf
           lbr     findlp              ; check next line
findy:     dec     rf                  ; move back to size byte
           mov     r7,rf               ; and set program counter
           lbr     runloop             ; and continue execution
findno:    sep     scall               ; print error
           dw      o_inmsg
           db      'Label not found: ',0
           lbr     errline             ; and print line

; *****************************************
; ***** Compare strings               *****
; ***** R8 - String 1                 *****
; ***** RF - String 2                 *****
; ***** Returns: DF=1 - strings match *****
; *****          DF=0 - No match      *****
; *****************************************
strcmp:    push    r8                  ; save string addresses
           push    rf
strcmp1:   lda     r8                  ; get byte from string 1
           str     r2                  ; store for comparison
           lbz     strcmpe             ; jump if end of string 1
           smi     ' '                 ; space also terminates
           lbz     strcmpe
           lda     rf                  ; get byte from string2
           sm                          ; compare to byte from string1
           lbz     strcmp1             ; jumpt if a match so far
strcmpn:   ldi     0                   ; signal no match
strcmpd:   shr                         ; shift result into DF
           pop     rf                  ; recover addresses
           pop     r8
           sep     sret                ; and return
strcmpe:   lda     rf                  ; get byte from string2
           lbz     strcmpy             ; jumpt if string2 is done
           smi     ' '                 ; space also terminates
           lbnz    strcmpn             ; jump if no match
strcmpy:   ldi     1                   ; signal a match
           lbr     strcmpd             ; and finish up

; ***********************************
; ***** Move R8 past any spaces *****
; ***********************************
trim:      lda     r8                  ; get byte from R8
           smi     ' '                 ; check for space
           lbz     trim                ; keep moving past spaces
           dec     r8                  ; move back to non-space
           sep     sret                ; and return to caller


errmsg:    db      'File not found',10,13,0
fildes:    db      0,0,0,0
           dw      dta
           db      0,0
           db      0
           db      0,0,0,0
           dw      0,0
           db      0,0,0,0

endrom:    equ     $

buffer:    ds      128
cbuffer:   ds      80
dta:       ds      512
pcstack:   ds      256
accept:    dw      256
program:   ds      1

