; *******************************************************************
; *** This software is copyright 2021 by Michael H Riley          ***
; *** You have permission to use, modify, copy, and distribute    ***
; *** this software so long as this copyright notice is retained. ***
; *** This software may not be used in commercial applications    ***
; *** without express written permission from the author.         ***
; *******************************************************************

; R7 - Program counter
; R8 - Line pointer
; R9 - Data block
; RA - PC Stack pointer

OP_MUL:    equ     042h
OP_DIV:    equ     041h
OP_ADD:    equ     032h
OP_SUB:    equ     031h
OP_GT:     equ     026h
OP_LT:     equ     025h
OP_GTE:    equ     024h
OP_LTE:    equ     023h
OP_EQ:     equ     022h
OP_NE:     equ     021h
OP_AND:    equ     013h
OP_OR:     equ     012h
OP_XOR:    equ     011h
OP_OP:     equ     008h
OP_CP:     equ     009h
OP_NUM:    equ     000h

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
numtokens: db      0
parens:    db      0
vartable:  dw      0
varend:    dw      0

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
           inc     r8
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

; **********************************
; ***** Prepare program to run *****
; **********************************
run:       mov     r7,program          ; point to first program line
           mov     r9,matched          ; point to data block
           ldi     0                   ; clear matched flag
           str     r9
           mov     ra,pcstack          ; set initial stack pointer
           mov     rf,accept           ; clear input buffer
           ldi     0
           str     rf
           ldi     low vartable        ; point to variable pointer
           plo     r9
           ghi     r8                  ; set to just after program text
           str     r9
           inc     r9
           glo     r8
           str     r9
           ldi     0                   ; clear variable table
           str     r8

; *****************************
; ***** Main program loop *****
; *****************************
runloop:   ldn     r7                  ; get line length byte
           lbz     progend             ; jump if no more program
           mov     r8,r7               ; set R8 to program line text
           inc     r8
           sep     scall               ; move past any leading spaces
           dw      trim

; ****************************************
; ***** Move past label if it exists *****
; ****************************************
           ldn     r8                  ; get first byte of line
           smi     '*'                 ; check for label
           lbnz    nolabel             ; jump if not
runlp1:    lda     r8                  ; get next character
           lbz     lineend             ; jump if line terminator found
           smi     ' '                 ; look for end of label
           lbnz    runlp1              ; loop until a space is found

; ***************************
; ***** Get the command *****
; ***************************
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
           ldn     r8                  ; check for expression
           smi     '('
           lbnz    c_nocond            ; jump if not
           inc     r8                  ; move past open parens
           glo     rb                  ; save command
           stxd
           sep     scall               ; evaluate expression
           dw      evaluate
           irx                         ; recover command
           ldx
           plo     rb
           glo     rf                  ; check return value for nonzero
           lbnz    expgood
           ghi     rf
           lbz     lineend             ; zero, so do not execute
expgood:   inc     r8                  ; move past close parens
           lbr     c_nocond            ; continue to execute command

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

; ***********************************
; ***** Now process the command *****
; ***********************************
           glo     rb                  ; get command
           smi     'A'                 ; check for A command
           lbz     cmd_a
           glo     rb                  ; get command
           smi     'C'                 ; check for C command
           lbz     cmd_c
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

; *************************************************
; ***** Move program counter to the next line *****
; *************************************************
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

; ***********************************
; ***** Command C, Compute      *****
; ***********************************
cmd_c:     sep     scall               ; move past any leading spaces
           dw      trim
           push    r8                  ; save what should be a variable
cmd_c_a:   lda     r8                  ; read next byte
           lbz     synerr              ; syntax error if end of line found
           smi     '='                 ; looking for equals
           lbnz    cmd_c_a             ; loop until = found
           sep     scall               ; evaluate expression
           dw      evaluate
           pop     r8                  ; pop what should be variable name
           sep     scall               ; set variable to value
           dw      setivar
           lbr     lineend             ; and then continue

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
           smi     '#'                 ; check for integer variable
           lbnz    cmd_t_1             ; jump if not
           sep     scall               ; get variable value
           dw      getivar
           sep     scall               ; and display it
           dw      itoa
           lbr     cmd_t               ; loop for more characters
cmd_t_1:
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

; **********************************
; ***** Expression Evaluator   *****
; ***** Uses: RB - token stack *****
; *****       RC - arg 1       *****
; *****       RD - arg 2       *****
; *****       RF - operation   *****
; **********************************
rsub:      glo     rd
           str     r2
           glo     rc
           sm  
           plo     rc
           ghi     rd
           str     r2
           ghi     rc
           smb
           phi     rc
           sep     sret

; ************************************
; *** make both arguments positive ***
; *** Arg1 RC                      ***
; *** Arg2 RD                      ***
; *** Returns D=0 - signs same     ***
; ***         D=1 - signs difer    ***
; ************************************
mdnorm:    ghi     rc                  ; get high byte if divisor
           str     r2                  ; store for sign check
           ghi     rd                  ; get high byte of dividend
           xor                         ; compare
           shl                         ; shift into df
           ldi     0                   ; convert to 0 or 1
           shlc                        ; shift into D
           plo     re                  ; store into sign flag
           ghi     rc                  ; need to see if RC is negative
           shl                         ; shift high byte to df
           lbnf    mdnorm2             ; jump if not
           ghi     rc                  ; 2s compliment on RC
           xri     0ffh
           phi     rc
           glo     rc
           xri     0ffh
           plo     rc
           inc     rc
mdnorm2:   ghi     rd                  ; now check rD for negative
           shl                         ; shift sign bit into df
           lbnf    mdnorm3             ; jump if not
           ghi     rd                  ; 2 compliment on RD
           xri     0ffh
           phi     rd
           glo     rd
           xri     0ffh
           plo     rd
           inc     rd
mdnorm3:   glo     re                  ; recover sign flag
           sep     sret                ; and return to caller

; *********************************************
; *** Function to multiply 2 16 bit numbers ***
; *** RC *= RD                              ***
; *********************************************
mul16:     push    rf                  ; save consumed registers
           ldi     0                   ; zero out total
           phi     rf
           plo     rf
mulloop:   glo     rd                  ; get low of multiplier
           lbnz    mulcont             ; continue multiplying if nonzero
           ghi     rd                  ; check hi byte as well
           lbnz    mulcont
           mov     rc,rf               ; transfer answer
           pop     rf                  ; recover consumed registers
           sep     sret                ; return to caller
mulcont:   ghi     rd                  ; shift multiplier
           shr
           phi     rd
           glo     rd
           shrc
           plo     rd
           lbnf    mulcont2            ; loop if no addition needed
           glo     rc                  ; add RC to RF
           str     r2
           glo     rf
           add
           plo     rf
           ghi     rc
           str     r2
           ghi     rf
           adc
           phi     rf
mulcont2:  glo     rc                  ; shift first number
           shl
           plo     rc
           ghi     rc
           shlc
           phi     rc
           lbr     mulloop             ; loop until done

; *** RC = RB/R7
; *** RB = remainder
; *** uses R8 and R9
; *********************************************
; *** Function to divide 2 16 bit numbers   ***
; *** RC /= RD                              ***
; *********************************************
div16:     sep     scall               ; normalize numbers
           dw      mdnorm
           plo     re                  ; save sign comparison
           glo     rd                  ; check for divide by zero
           lbnz    div16_1
           ghi     rd
           lbnz    div16_1
           mov     rc,0                ; return 0 as div/0
           sep     sret                ; and return to caller
div16_1:   push    rf                  ; save consumed registers
           push    r9
           push    r8
           ldi     0                   ; clear answer
           phi     rf
           plo     rf
           phi     r8                  ; set additive
           plo     r8
           inc     r8
d16lp1:    ghi     rd                  ; get high byte from rd
           ani     128                 ; check high bit
           lbnz    divst               ; jump if set
           glo     rd                  ; lo byte of divisor
           shl                         ; multiply by 2
           plo     rd                  ; and put back
           ghi     rd                  ; get high byte of divisor
           shlc                        ; continue multiply by 2
           phi     rd                  ; and put back
           glo     r8                  ; multiply additive by 2
           shl
           plo     r8
           ghi     r8
           shlc
           phi     r8
           lbr     d16lp1              ; loop until high bit set in divisor
divst:     glo     rd                  ; get low of divisor
           lbnz    divgo               ; jump if still nonzero
           ghi     rd                  ; check hi byte too
           lbnz    divgo
           glo     re                  ; get sign flag
           shr                         ; move to df
           lbnf    divret              ; jump if signs were the same
           ghi     rf                  ; perform 2s compliment on answer
           xri     0ffh
           phi     rf
           glo     rf
           xri     0ffh
           plo     rf
           inc     rf
divret:    mov     rc,rf               ; move answer to rc
           pop     r8                  ; recover consumed registers
           pop     r9
           pop     rf
           sep     sret                ; jump if done
divgo:     mov     r9,rc               ; copy dividend
           glo     rd                  ; get lo of divisor
           str     r2                  ; store for subtract
           glo     rc                  ; get low byte of dividend
           sm                          ; subtract
           plo     rc                  ; put back into r6
           ghi     rd                  ; get hi of divisor
           str     r2                  ; store for subtract
           ghi     rc                  ; get hi of dividend
           smb                         ; subtract
           phi     rc                  ; and put back
           lbdf    divyes              ; branch if no borrow happened
           mov     rc,r9               ; recover copy
           lbr     divno               ; jump to next iteration
divyes:    glo     r8                  ; get lo of additive
           str     r2                  ; store for add
           glo     rf                  ; get lo of answer
           add                         ; and add
           plo     rf                  ; put back
           ghi     r8                  ; get hi of additive
           str     r2                  ; store for add
           ghi     rf                  ; get hi byte of answer
           adc                         ; and continue addition
           phi     rf                  ; put back
divno:     ghi     rd                  ; get hi of divisor
           shr                         ; divide by 2
           phi     rd                  ; put back
           glo     rd                  ; get lo of divisor
           shrc                        ; continue divide by 2
           plo     rd
           ghi     r8                  ; get hi of divisor
           shr                         ; divide by 2
           phi     r8                  ; put back
           glo     r8                  ; get lo of divisor
           shrc                        ; continue divide by 2
           plo     r8
           lbr     divst               ; next iteration

reduce:    ldi     low numtokens       ; need number of tokens
           plo     r9
           ldn     r9
           smi     3                   ; see if less than 3
           lbdf    reduce1             ; jump if not
           sep     sret                ; otherwise return to caller

reduce1:   dec     rb                  ; retrieve argument 2
           ldn     rb                  ; retrieve lsb
           plo     rd
           dec     rb                  ; retrieve msb
           ldn     rb
           phi     rd
           dec     rb                  ; get type
           ldn     rb
     
           dec     rb                  ; get operation
           dec     rb
           dec     rb
           ldn     rb
           plo     rf

           dec     rb                  ; retrieve argument 1
           ldn     rb                  ; retrieve lsb
           plo     rc
           dec     rb                  ; retrieve msb
           ldn     rb
           phi     rc
           dec     rb                  ; get type
           ldn     rb

; ----- OP_ADD RC += RD
           glo     rf                  ; check OP_ADD
           smi     OP_ADD
           lbnz    r_n_add
           glo     rd
           str     r2
           glo     rc
           add
           plo     rc
           ghi     rd
           str     r2
           ghi     rc
           adc
           phi     rc
           lbr     r_done
; ----- OP_SUB RC -= RD
r_n_add:   glo     rf                  ; check OP_SUB
           smi     OP_SUB
           lbnz    r_n_sub
           sep     scall               ; perform subtraction
           dw      rsub
           lbr     r_done
; ----- OP_MUL RC *= RD
r_n_sub:   glo     rf                  ; check OP_MUL
           smi     OP_MUL
           lbnz    r_n_mul
           sep     scall               ; perform multiplication
           dw      mul16
           lbr     r_done
; ----- OP_DIV RC /= RD
r_n_mul:   glo     rf                  ; check OP_DIV
           smi     OP_DIV
           lbnz    r_n_div
           sep     scall               ; perform multiplication
           dw      div16
           lbr     r_done
; ----- OP_AND RC -= RD
r_n_div:   glo     rf                  ; check OP_AND
           smi     OP_AND
           lbnz    r_n_and
           glo     rd
           str     r2
           glo     rc
           and 
           plo     rc
           ghi     rd
           str     r2
           ghi     rc
           and
           phi     rc
           lbr     r_done
; ----- OP_OR RC -= RD
r_n_and:   glo     rf                  ; check OP_OR
           smi     OP_OR
           lbnz    r_n_or
           glo     rd
           str     r2
           glo     rc
           or 
           plo     rc
           ghi     rd
           str     r2
           ghi     rc
           or
           phi     rc
           lbr     r_done
; ----- OP_XOR RC -= RD
r_n_or:    glo     rf                  ; check OP_XOR
           smi     OP_XOR
           lbnz    r_n_xor
           glo     rd
           str     r2
           glo     rc
           xor 
           plo     rc
           ghi     rd
           str     r2
           ghi     rc
           xor
           phi     rc
           lbr     r_done
; ----- OP_NE RC != RD
r_n_xor:   glo     rf                  ; check OP_NE
           smi     OP_NE
           lbnz    r_n_ne
           sep     scall               ; perform compare
           dw      rsub
           glo     rc
           lbnz    r_logic1
           ghi     rc
           lbnz    r_logic1
           lbr     r_logic0
; ----- OP_EQ RC != RD
r_n_ne:    glo     rf                  ; check OP_EQ
           smi     OP_EQ
           lbnz    r_n_eq
           sep     scall               ; perform compare
           dw      rsub
           glo     rc
           lbz     r_logic1
           ghi     rc
           lbz     r_logic1
           lbr     r_logic0
; ----- OP_GT RC != RD
r_n_eq:    glo     rf                  ; check OP_GT
           smi     OP_GT
           lbnz    r_n_gt
           sep     scall               ; perform compare
           dw      rsub
           ghi     rc
           shl
           lbnf    r_logic1
           lbr     r_logic0
; ----- OP_LT RC != RD
r_n_gt:    glo     rf                  ; check OP_LT
           smi     OP_LT
           lbnz    r_n_lt
           sep     scall               ; perform compare
           dw      rsub
           ghi     rc
           shl
           lbdf    r_logic1
           lbr     r_logic0
; ----- OP_GTE RC != RD
r_n_lt:    glo     rf                  ; check OP_GTE
           smi     OP_GTE
           lbnz    r_n_gte
           sep     scall               ; perform compare
           dw      rsub
           glo     rc
           str     r2
           ghi     rc
           or
           lbz     r_logic1
           ghi     rc
           shl
           lbnf    r_logic1
           lbr     r_logic0
; ----- OP_LTE RC != RD
r_n_gte:   glo     rf                  ; check OP_LTE
           smi     OP_LTE
           lbnz    r_n_lte
           sep     scall               ; perform compare
           dw      rsub
           glo     rc
           str     r2
           ghi     rc
           or
           lbz     r_logic1
           ghi     rc
           shl
           lbdf    r_logic1
           lbr     r_logic0

r_n_lte:

r_logic0:  ldi     0                   ; set result to logic 0
           plo     rc
           phi     rc
           lbr     r_done
r_logic1:  ldi     0ffh
           plo     rc
           phi     rc
r_done:    ldi     OP_NUM              ; push result back on stack
           str     rb
           inc     rb
           ghi     rc
           str     rb
           inc     rb
           glo     rc
           str     rb
           inc     rb
           ldi     low numtokens       ; numtokens -= 2
           plo     r9
           ldn     r9
           smi     2
           str     r9
           sep     sret                ; return to caller

addop:     plo     re                  ; store op
addoplp:   ldi     low numtokens       ; need to get number of tokens
           plo     r9
           ldn     r9                  ; retrieve number of tokens
           smi     3                   ; see if greater than 2
           lbnf    addop_go            ; jump if not
           dec     rb                  ; need type of symbol 2 back
           dec     rb
           dec     rb
           dec     rb
           dec     rb
           dec     rb
           lda     rb                  ; retrieve type
           inc     rb                  ; and put rb back
           inc     rb
           inc     rb
           inc     rb
           inc     rb
           ani     0f0h                ; keep only level
           str     r2                  ; store for comparison
           glo     re                  ; recover op
           ani     0f0h                ; keep only level
           sd                          ; compare to operator on stack
           lbnf    addop_go            ; jump if new level is lower
           glo     re                  ; preserve op
           stxd
           sep     scall               ; perform a reduce operation
           dw      reduce
           irx                         ; recover op
           ldx
           plo     re
           lbr     addoplp             ; loop for more reductions
addop_go:  glo     re                  ; recover op
           str     rb                  ; store op to token stack
           inc     rb                  ; and increment pointer
           inc     rb
           inc     rb
           ldi     low numtokens       ; point to numtokens
           plo     r9
           ldn     r9                  ; and increment it
           adi     1
           str     r9
           sep     sret                ; and return to caller

evaluate:  ldi     low parens          ; point to parens
           plo     r9
           ldi     0                   ; set to zero
           str     r9
           ldi     low numtokens       ; point to numtokens
           plo     r9
           ldi     0                   ; set to zero
           str     r9
           mov     rb,tokens           ; setup token pointer
eval_lp:   ldn     r8                  ; get byte from input
           lbz     eval_dn             ; jump if end of input reached
           sep     scall               ; see if numeric
           dw      is_number
           lbnf    eval_1              ; jump if not
           smi     '0'                 ; conver to binary
           plo     rf                  ; put into RF
           ldi     0                   ; clear high byte
           phi     rf
           inc     r8                  ; move past character
eval_0_1:  lda     r8                  ; get next character
           sep     scall               ; is it a number
           dw      is_number
           lbnf    eval_0_2            ; jump if not
           smi     '0'                 ; convert it to binary
           plo     re                  ; and set it aside for now
           glo     rf                  ; multiply total by 2
           shl
           plo     rf
           plo     rc                  ; keep a copy here too
           ghi     rf
           shlc
           phi     rf
           phi     rc
           glo     rc                  ; multiply rc by 2
           shl
           plo     rc
           ghi     rc
           shlc
           phi     rc
           glo     rc                  ; multiply rc by 4
           shl
           plo     rc
           ghi     rc
           shlc
           phi     rc
           glo     rc                  ; rf += rc
           str     r2
           glo     rf
           add
           plo     rf
           ghi     rc
           str     r2
           ghi     rf
           adc
           phi     rf
           glo     re                  ; rf += new number
           str     r2
           glo     rf
           add
           plo     rf
           ghi     rf
           adci    0
           phi     rf
           lbr     eval_0_1            ; loop back for more numerals
eval_0_2:  dec     r8                  ; move back to non-numeral character
eval_val:  ldi     OP_NUM              ; mark token as 
           str     rb                  ; store into token
           inc     rb
           ghi     rf                  ; store number into token
           str     rb
           inc     rb
           glo     rf
           str     rb
           inc     rb
           ldi     low numtokens       ; point to numtokens
           plo     r9
           ldn     r9                  ; and increment it
           adi     1
           str     r9
           lbr     eval_lp             ; loop back for more tokens

; ***** Process variables *****
eval_1:    ldn     r8                  ; get byte
           smi     '#'                 ; check for integer marker
           lbz     eval_1a             ; jump if so
           ldn     r8                  ; recover character
           sep     scall               ; see if a variable character
           dw      is_varchr
           lbnf    eval_2              ; jump if not
           dec     r8
eval_1a:   inc     r8                  ; move past #
           sep     scall               ; retrieve variable value
           dw      getivar
           lbr     eval_val            ; store value into next token

; ***** Check for operators *****
eval_2:    sep     scall               ; check for operator
           dw      isop
           lbnf    eval_3              ; jump if not
           sep     scall               ; add the new operator
           dw      addop
           lbr     eval_lp             ; and loop back for more
; ***** Check for open parens *****
eval_3:    ldn     r8                  ; get character
           smi     '('                 ; is it open parens
           lbnz    eval_4              ; jump if not
           inc     r8                  ; point to next character
           ldi     OP_OP               ; push OP_OP to token stack
           str     rb
           inc     rb
           inc     rb
           inc     rb
           ldi     low numtokens       ; point to numtokens
           plo     r9
           ldn     r9
           adi     1
           str     r9
           ldi     low parens          ; point to parens
           plo     r9
           ldn     r9
           adi     1
           str     r9
           lbr     eval_lp             ; loop for more tokens
; ***** Check for close parens *****
eval_4:    ldn     r8                  ; get character
           smi     ')'                 ; check for close parens
           lbnz    eval_5              ; jump if not
           ldi     low parens          ; need parens count
           plo     r9
           ldn     r9
           lbz     eval_dn             ; jump if no open parens
           smi     1                   ; remove one open parens
           str     r9                  ; write it back
eval_4_a:  ldi     low numtokens       ; need number of tokens
           plo     r9
           ldn     r9
           smi     3                   ; is it >= 3
           lbnf    eval_4_z            ; jump if not
           glo     rb                  ; point two tokens back
           smi     6
           plo     rf
           ghi     rb
           smbi    0
           phi     rf
           ldn     rf                  ; retrieve the type
           smi     OP_OP               ; is it an open parens
           lbz     eval_4_b            ; jump if so
           ldn     rf                  ; retrieve type
           ani     0f0h                ; keep only high nybble
           lbz     eval_4_b            ; jump if not operator
           sep     scall               ; execute a reduce
           dw      reduce
           lbr     eval_4_a            ; loop until no more reductions
eval_4_b:  ldi     low numtokens       ; point to numtokens
           plo     r9
           ldn     r9
           smi     2                   ; check for 2 or more tokens
           lbdf    eval_4_z            ; jump if 2 or more
           sep     scall               ; otherwise display error
           dw      o_inmsg
           db      'Expression error: ',0
           lbr     errline
eval_4_z:  dec     rb                  ; decrement stack pointer
           mov     rf,rb               ; make a copy of stack pointer
           dec     rf                  ; move to prior token
           dec     rf
           dec     rf
           ldn     rb                  ; copy token to prior token
           str     rf
           dec     rb
           dec     rf
           ldn     rb                  ; copy token to prior token
           str     rf
           dec     rb
           dec     rf
           ldn     rb                  ; copy token to prior token
           str     rf
           ldi     low numtokens       ; point to numtokens
           plo     r9
           ldn     r9                  ; decrement count
           smi     1
           str     r9
           inc     r8                  ; move past close parens
           lbr     eval_lp             ; and then keep processing
; ***** Check for space *****
eval_5:    ldn     r8                  ; get character
           smi     ' '                 ; see if space
           lbnz    eval_dn             ; jump if not
           inc     r8                  ; move past space
           lbr     eval_lp             ; loop back for more tokens

eval_dn:   ldi     low numtokens       ; point to tokens
           plo     r9
           ldn     r9                  ; get number of tokens
           smi     3                   ; see if less than 3
           lbnf    eval_ex             ; jump if done
           sep     scall               ; otherwise call reduce
           dw      reduce
           lbr     eval_dn             ; loop for more possible reductions
eval_ex:   mov     rc,tokens+1         ; point to final value
           lda     rc                  ; and set rf
           phi     rf
           lda     rc
           plo     rf
           sep     sret                ; return to caller

isop:      mov     rf,ops              ; point to ops table
op_1:      ldn     rf                  ; get byte from table
           lbz     op_no               ; jump if no matches found
           push    r8                  ; save buffer position
op_2:      lda     rf                  ; get byte from ops table
           shl                         ; shift high bit into DF
           lbdf    op_3                ; jump if last byte of token
           shr                         ; shift it back
           str     r2                  ; store for compare
           lda     r8                  ; get byte from input
           sm                          ; and compare
           lbz     op_2                ; jump if match so far
op_2_1:    lda     rf                  ; get byte from table
           shl                         ; need to check high bit
           lbnf    op_2_1              ; loop until last byte found
           inc     rf                  ; then move past value field
           pop     r8                  ; recover input position
           lbr     op_1                ; and check next opeartor
op_3:      shr                         ; shift value back to the right
           str     r2                  ; store for comparison
           lda     r8                  ; get byte from input
           sm                          ; and compare
           lbz     op_yes              ; jump if good
           pop     r8                  ; recover input address
           inc     rf                  ; move past token value
           lbr     op_1                ; and check next token
op_yes:    irx                         ; remove address from stack
           irx
           ldi     1                   ; signal operator found
           shr
           lda     rf                  ; recover operator value into D
           sep     sret                ; and return to caller
op_no:     ldi     0                   ; signal not an operator
           shr
           sep     sret                ; and return

ops:       db      ('+'+080h),OP_ADD
           db      ('-'+080h),OP_SUB
           db      ('*'+080h),OP_MUL
           db      ('/'+080h),OP_DIV
           db      ('&'+080h),OP_AND
           db      ('|'+080h),OP_OR
           db      ('^'+080h),OP_XOR
           db      '<',('='+080h),OP_LTE
           db      '>',('='+080h),OP_GTE
           db      '<',('>'+080h),OP_NE
           db      ('<'+080h),OP_LT
           db      ('>'+080h),OP_GT
           db      ('='+080h),OP_EQ
           db      0

; **************************************
; ***** Check D for 0-9            *****
; ***** Returns: DF=1 - is number  *****
; *****          DF=0 - not number *****
; **************************************
is_number: plo     re                  ; save original value
           smi     '0'                 ; check for below numerals
           lbnf    not_chr             ; jump if not in range
           smi     9                   ; check high of range
           lbdf    not_chr             ; jump if not in range
           lbr     is_chr              ; otherwise singal in range
not_chr:   ldi     0                   ; signal not in range
           shr
           glo     re                  ; recover original value
           sep     sret                ; and return
is_chr:    ldi     1                   ; sginal is in range
           shr
           glo     re
           sep     sret

; **************************************
; ***** Check D for a-z            *****
; ***** Returns: DF=1 - is number  *****
; *****          DF=0 - not number *****
; **************************************
is_lc:     plo     re                  ; save original value
           smi     'a'                 ; check for below range
           lbnf    not_chr             ; jump if not in range
           smi     26                  ; check high of range
           lbdf    not_chr             ; jump if above range
           lbr     is_chr              ; otherwise mark in range

; **************************************
; ***** Check D for A-Z            *****
; ***** Returns: DF=1 - is number  *****
; *****          DF=0 - not number *****
; **************************************
is_uc:     plo     re                  ; save original value
           smi     'A'                 ; check for below range
           lbnf    not_chr             ; jump if not in range
           smi     26                  ; check high of range
           lbdf    not_chr             ; jump if above range
           lbr     is_chr              ; otherwise mark in range

; **************************************************
; ***** Check D for allowed variable character *****
; ***** 0-9 ,a-z ,A-Z, _                       *****
; ***** Returns: DF=1 - valid character        *****
; *****          DF=0 - not valid character    *****
; **************************************************
is_varchr: sep     scall               ; check for number
           dw      is_number
           lbnf    varchr1             ; jump if not
           sep     sret                ; return true to caller
varchr1:   sep     scall               ; check for lowercase
           dw      is_lc
           lbnf    varchr2             ; jump if not
           sep     sret                ; return true to caller
varchr2:   sep     scall               ; check for uppercase
           dw      is_uc
           lbnf    varchr3             ; jump if not
           sep     sret                ; return true to caller
varchr3:   plo     re                  ; save value
           smi     '_'                 ; check for underscore
           lbnz    not_chr             ; false if not
           lbr     is_chr              ; otherwise true


; *************************************************************************
; *****                     Variable Handling                         *****
; *************************************************************************

; ***********************************************************
; ***** Find variable                                   *****
; ***** R8   - Pointer to variable name                 *****
; ***** RC.0 - Variable type                            *****
; ***** Returns: when found                             *****
; *****          DF=1                                   *****
; *****          RF - Address of variable data          *****
; *****          R8 - First character after name        *****
; *****          When not found                         *****
; *****          DF=0                                   *****
; *****          R8 - First character of name           *****
; *****          RF - End of variable table             *****
; ***********************************************************
findvar:   ldi     low vartable        ; need to get address of variable table
           plo     r9
           lda     r9
           phi     rf
           ldn     r9
           plo     rf
var_1:     ldn     rf                  ; get byte from table
           lbz     var_no              ; jump if no matches found
           ani     3                   ; keep only type bits
           str     r2                  ; compare types
           glo     rc
           sm
           lbnz    var_3               ; jump if wrong type
           push    r8                  ; save buffer position
           push    rf                  ; save var table position
           inc     rf                  ; move past type/size byte
var_2:     lda     r8                  ; get byte from source line
           sep     scall               ; check for allowed char
           dw      is_varchr
           lbnf    var_4               ; jump if not
           str     r2                  ; store for compare
           lda     rf                  ; get byte from variable
           sm                          ; compare with source
           lbz     var_2               ; keep looking if matched
var_5:     pop     rf                  ; recover pointers
           pop     r8
var_3:     ldn     rf                  ; shift out type to get size
           shr 
           shr
           str     r2                  ; store for add
           glo     rf                  ; point to next variable
           add
           plo     rf
           ghi     rf
           adci    0
           phi     rf
           lbr     var_1               ; loop to check more
var_4:     dec     r8                  ; move back to non-var character
           lda     rf                  ; get byte from variable name
           lbnz    var_5               ; not found if not at end of name
           irx                         ; do not recover rf
           irx
           irx                         ; do not recover r8
           irx                         ; leave it after variable name
           ldi     1
           shr
           sep     sret                ; return to caller
var_no:    ldi     0                   ; signal error
           shr
           sep     sret                ; return to caller
     

; ***********************************************************
; ***** Get integer variable value                      *****
; ***** R8   - Pointer to variable name                 *****
; ***** Returns: when found                             *****
; *****          RF - Variable value                    *****
; *****          R8 - First character after name        *****
; ***********************************************************
getivar:   ldi     1                   ; search for integer variable
           plo     rc
           sep     scall               ; find variable address
           dw      findvar
           lbnf    varntfnd            ; jump if variable not found
           lda     rf                  ; get variable value
           plo     re                  ; save for a moment
           lda     rf                  ; get lsb
           plo     rf
           glo     re                  ; recover msg
           phi     rf
           sep     sret                ; and return to caller
varntfnd:  sep     scall               ; display error
           dw      o_inmsg
           db      'Variable not found: ',0
           lbr     errline             ; show program line and exit

; ***************************************************
; ***** Set integer variable                    *****
; ***** R8 - Pointer to variable name           *****
; ***** RF - Value                              *****
; ***************************************************
setivar:   push    rc                  ; save used registers
           push    rd
           glo     rf                  ; save value
           stxd
           ghi     rf
           stxd
           ldi     1                   ; search for integer variable
           plo     rc
           sep     scall               ; find variable address
           dw      findvar
           lbnf    newivar             ; jump if new variable needed
           irx                         ; set variable value
           ldxa
           str     rf
           inc     rf
           ldx
           str     rf
           pop     rd                  ; recover used registers
           pop     rc
           sep     sret                ; and return to caller
newivar:   mov     rd,rf               ; keep copy of variable address
           ldi     4                   ; set count
           plo     rc
           inc     rf                  ; point to where name goes
newivar1:  lda     r8                  ; get byte from var name
           sep     scall               ; check for valid character
           dw      is_varchr
           lbnf    newivar2            ; jump if not
           str     rf                  ; store into var table
           inc     rf
           inc     rc                  ; increment count
           lbr     newivar1            ; loop until name is copied
newivar2:  dec     r8                  ; move back to non-var character
           ldi     0                   ; terminate name
           str     rf
           inc     rf
           irx                         ; set value
           ldxa
           str     rf
           inc     rf
           ldx
           str     rf
           inc     rf
           ldi     0                   ; write new end of table
           str     rf
           ldi     low varend          ; end of variable table
           plo     r9
           ghi     rf                  ; write new end
           str     r9
           inc     r9
           glo     rf
           str     r9
           glo     rc                  ; get count
           shl                         ; shift it two bits
           shl
           ori     1                   ; flag integer variable
           str     rd                  ; and store to table
           pop     rd                  ; recover registers
           pop     rc
           sep     sret                ; and return to caller


; **************************************
; ***** Convert RF to bcd in M[RD] *****
; **************************************
tobcd:     push    rd           ; save address
           ldi     5            ; 5 bytes to clear
           plo     re
tobcdlp1:  ldi     0
           str     rd           ; store into answer
           inc     rd
           dec     re           ; decrement count
           glo     re           ; get count
           lbnz    tobcdlp1     ; loop until done
           pop     rd           ; recover address
           ldi     16           ; 16 bits to process
           plo     r9
tobcdlp2:  ldi     5            ; need to process 5 cells
           plo     re           ; put into count
           push    rd           ; save address
tobcdlp3:  ldn     rd           ; get byte
           smi     5            ; need to see if 5 or greater
           lbnf    tobcdlp3a    ; jump if not
           adi     8            ; add 3 to original number
           str     rd           ; and put it back
tobcdlp3a: inc     rd           ; point to next cell
           dec     re           ; decrement cell count
           glo     re           ; retrieve count
           lbnz    tobcdlp3     ; loop back if not done
           glo     rf           ; start by shifting number to convert
           shl
           plo     rf
           ghi     rf
           shlc
           phi     rf
           shlc                 ; now shift result to bit 3
           shl
           shl
           shl
           str     rd
           pop     rd           ; recover address
           push    rd           ; save address again
           ldi     5            ; 5 cells to process
           plo     re
tobcdlp4:  lda     rd           ; get current cell
           str     r2           ; save it
           ldn     rd           ; get next cell
           shr                  ; shift bit 3 into df
           shr
           shr
           shr
           ldn     r2           ; recover value for current cell
           shlc                 ; shift with new bit
           ani     0fh          ; keep only bottom 4 bits
           dec     rd           ; point back
           str     rd           ; store value
           inc     rd           ; and move to next cell
           dec     re           ; decrement count
           glo     re           ; see if done
           lbnz    tobcdlp4     ; jump if not
           pop     rd           ; recover address
           dec     r9           ; decrement bit count
           glo     r9           ; see if done
           lbnz    tobcdlp2     ; loop until done
           sep     sret         ; return to caller

; ***************************************************
; ***** Output 16-bit integer                   *****
; ***** RF - 16-bit integer                     *****
; ***************************************************
itoa:      push    rf           ; save consumed registers
           push    r9
           push    r8
           push    r7
           glo     r2           ; make room on stack for buffer
           smi     6
           plo     r2
           ghi     r2
           smbi    0
           phi     r2
           mov     rd,r2        ; RD is output buffer
           inc     rd
           ghi     rf           ; get high byte
           shl                  ; shift bit to DF
           lbdf    itoan        ; negative number
itoa1:     sep     scall        ; convert to bcd
           dw      tobcd
           mov     rd,r2
           inc     rd
           ldi     5
           plo     r8
           ldi     4            ; max 4 leading zeros
           phi     r8
itoalp1:   lda     rd
           lbz     itoaz        ; check leading zeros
           str     r2           ; save for a moment
           ldi     0            ; signal no more leading zeros
           phi     r8
           ldn     r2           ; recover character
itoa2:     adi     030h
           sep     scall        ; display it
           dw      o_type
itoa3:     dec     r8
           glo     r8
           lbnz    itoalp1
           glo     r2           ; pop work buffer off stack
           adi     6
           plo     r2
           ghi     r2
           adci    0
           phi     r2
           pop     r7
           pop     r8           ; recover consumed registers
           pop     r9
           pop     rf
           sep     sret         ; return to caller
itoaz:     ghi     r8           ; see if leading have been used up
           lbz     itoa2        ; jump if so
           smi     1            ; decrement count
           phi     r8
           lbr     itoa3        ; and loop for next character
itoan:     ldi     '-'          ; show negative
           sep     scall        ; display it
           dw      o_type
           glo     rf           ; 2s compliment
           xri     0ffh
           adi     1
           plo     rf
           ghi     rf
           xri     0ffh
           adci    0
           phi     rf
           lbr     itoa1        ; now convert/show number


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
tokens:    ds      60*3
program:   ds      1

; Var table format:
; byte 0 - type
;          bits 0-1 - variable type
;                    1 - integer
;                    2 - string
;          bits 2-7 - entry size
;       3-13 - name as ASCIIZ
;         14 - value msb
;         15 - value lsb

