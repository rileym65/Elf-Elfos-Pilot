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

OP_FPEEK:  equ     051h
OP_FINP:   equ     052h
OP_FEF:    equ     053h
OP_FRND:   equ     054h
OP_FLEN:   equ     055h
OP_FASC:   equ     056h
OP_FVAL:   equ     057h
OP_FFRE:   equ     058h
OP_FHEAP:  equ     059h
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
heap:      dw      0
lfsr:      db      0,0,0,0
overflow:  db      0

readef:    ldi     0                   ; start with 0
           bn1     ef1                 ; jump if EF1=0
           ori     1                   ; signal on
ef1:       bn2     ef2                 ; jump if EF2=0
           ori     2                   ; signal on
ef2:       bn3     ef3                 ; jump if EF3=0
           ori     4                   ; signal on
ef3:       bn4     ef4                 ; jump if EF4=0
           ori     8                   ; signal on
ef4:       sep     sret                ; return to caller

p_setivar: lbr     setivar
p_getivar: lbr     getivar
p_setsvar: lbr     setsvar
p_getsvar: lbr     getsvar
p_findvar: lbr     findvar
p_alloc:   lbr     alloc
p_dealloc: lbr     dealloc
p_itoa:    lbr     itoa
p_atoi:    lbr     atoi
p_eval:    lbr     evaluate

start:     sep     scall               ; display header
           dw      o_inmsg
           db      'Rc/Pilot+ rc-1',10,13,0
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
           ldi     0                   ; indicate there are no disk bytes to read
           plo     rc
loadlp:    mov     rf,accept           ; use accept buffer to read line
           sep     scall               ; read next line of program
           dw      readln
           mov     r8,accept           ; point to beginning of line
           sep     scall               ; move past leading spaces
           dw      trim
           ldi     0                   ; set line size
           plo     r9
           mov     ra,r7               ; set destination for line
           inc     ra
           ldn     r8                  ; get first byte
           smi     '*'                 ; is it a label
           lbnz    load2a              ; jump if not
load1:     lda     r8                  ; copy the label
           str     ra
           inc     ra
           inc     r9
           lbz     load3               ; jump if line terminator copied
           smi     9                   ; was a tab copied
           lbz     load2a              ; jump if so
           smi     23                  ; was a space copied
           lbnz    load1               ; back to label loop if not
load2a:    sep     scall               ; move past any spaces
           dw      trim
           mov     rf,aliases          ; attempt to tokenize command
           sep     scall
           dw      tkn_1
           lbdf    tkn_fnd             ; write token to program if found
load2:     lda     r8                  ; get byte from line
tkn_fnd:   str     ra                  ; store into destination
           inc     ra
           inc     r9                  ; increment line size
           lbnz    load2               ; jump if not end of line
load3:     inc     r9                  ; account for size byte
           glo     r9                  ; store line size
           str     r7
           mov     r7,ra               ; set new line position
           ghi     rc                  ; need to see if eof was reached in last line
           shr                         ; shift flag into DF
           lbnf    loadlp              ; jump if eof not reached
           ldi     0                   ; terminate program text
           str     r7
           mov     r8,ra               ; need to know where end of program space was
           inc     r8
           lbr     run                 ; setup to run



; *************************************
; ***** Read line                 *****
; ***** RF - where to put it      *****
; ***** Returns: DF=1 - EOF       *****
; *****          DF=0 - Good read *****
; *************************************
readln:    glo     rc                  ; are there still disk bytes
           lbnz    readln_1            ; jump if so
           push    rf                  ; save buffer position
           sep     scall               ; read next block from disk
           dw      rdblk
           pop     rf                  ; recover address
           lbdf    readln_e            ; jump if eof encountered
readln_1:  lda     rb                  ; get next byte from disk buffer
           plo     re                  ; save a copy
           dec     rc                  ; decrement byte count
           ani     0e0h                ; check for control characters
           lbz     readln              ; ignore any leading control characters
readln_2:  glo     re                  ; recover character
           str     rf                  ; write into destination buffer
           inc     rf
           glo     rc                  ; are there more bytes in disk buffer
           lbnz    readln_2a           ; jump if so
           push    rf                  ; save buffer position
           sep     scall               ; read next block from disk
           dw      rdblk
           pop     rf
           lbdf    readln_e            ; jump if eof found
readln_2a: lda     rb                  ; get next byte from buffer
           plo     re                  ; save a copy
           dec     rc                  ; decrement byte count
           smi     10                  ; check for line feed
           lbz     readln_e2           ; jump if end of line
           smi     3                   ; check carriage return as well
           lbnz    readln_2            ; jump if good line character
readln_e2: ldi     0                   ; signal not eof
           shr
readln_e:  ldi     0                   ; store terminator into line
           str     rf
           sep     sret                ; return to caller

; ********************************************
; ***** Read 128 bytes from input file   *****
; ***** Returns: DF=1 - EOF hit          *****
; *****          DF=0 - good read        *****
; *****            RB - pointer to bytes *****
; *****            RC - Count of bytes   *****
; ********************************************
rdblk:     mov     rc,128              ; attempt to read 128 bytes
           mov     rf,buffer           ; transfer buffer
           sep     scall               ; read next block of bytes
           dw      o_read
           glo     rc                  ; see if bytes were read
           lbz     eof                 ; jump if no bytes were read
           mov     rb,buffer           ; set point to bytes
           ldi     0                   ; signal not at eof
           phi     rc
           shr
           sep     sret                ; and return
eof:       sep     scall               ; close the file
           dw      o_close
           ldi     1                   ; signal eof
           phi     rc
           shr
           sep     sret                ; and return


; **********************************
; ***** Prepare program to run *****
; **********************************
run:       mov     r2,stack
           mov     r7,program          ; point to first program line
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
           ldi     low varend          ; point to variable table end
           plo     r9
           ghi     r8                  ; set to just after program text
           str     r9
           inc     r9
           glo     r8
           str     r9
           ldi     low heap            ; point to heap pointer
           plo     r9
           mov     rf,HIMEM            ; point to Elf/OS high memory pointer
           lda     rf                  ; retrieve it
           plo     re
           ldn     rf
           plo     rf
           glo     re
           phi     rf
           dec     rf                  ; minus one byte
           ldi     0                   ; mark end of heap
           str     rf
           ghi     rf                  ; store address in heap pointer
           str     r9
           inc     r9
           glo     rf
           str     r9

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
           ldn     r8                  ; check for o/O
           smi     'O'
           lbz     c_otest
           smi     32
           lbz     c_otest
           ldn     r8                  ; check for expression
           smi     '('
           lbnz    c_nocond            ; jump if not
           inc     r8                  ; move past open parens
           sep     scall               ; move past any spaces
           dw      trim
           ldn     r8                  ; get next byte
           smi     '$'                 ; check for string comparison
           lbz     exp_str             ; jump if string expression
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
exp_str:   inc     r8                  ; move past dollar sign
           sep     scall               ; get string variable data
           dw      getsvar
           sep     scall               ; move past any spaces
           dw      trim
           ldi     0                   ; clear flags
           plo     rc
exp_str1:  lda     r8                  ; get next character
           lbz     synerr              ; jump if end of line encountered
           smi     '<                  ; check for less than
           lbnz    exp_str1a           ; jump if not
           glo     rc                  ; get flags
           ori     2                   ; set less than flag
           plo     rc
           lbr     exp_str1            ; check for more symbols
exp_str1a: smi     1                   ; check for = sign
           lbnz    exp_str1b           ; jump if not
           glo     rc                  ; get flags
           ori     1                   ; set equals flag
           plo     rc
           lbr     exp_str1            ; check for more symbols
exp_str1b: smi     1                   ; check for > sign
           lbnz    exp_str1c           ; jump if not
           glo     rc                  ; get flags
           ori     4                   ; set greater than flag
           plo     rc
           lbr     exp_str1            ; check for more
exp_str1c: dec     r8                  ; move back to non-comparison character
           glo     rc                  ; save flags on stack
           stxd
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; get next byte, must be " or $
           smi     34                  ; first check quote
           lbz     exp_qt              ; jump if quoted string
           smi     2                   ; check for $ sign
           lbnz    synerr              ; error if not " or $
           push    rf                  ; save first string address
           sep     scall               ; get address of second string
           dw      getsvar
           mov     rd,rf               ; move address to rd
           pop     rf                  ; recover first string
exp_str2:  irx                         ; recover rc
           ldn     r2
           plo     rc
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; check for closing )
           smi     ')
           lbnz    synerr              ; error if not found
           sep     scall               ; compare strings
           dw      strcmp2
           plo     re                  ; copy a copy of result
           lbnf    exp_eq              ; jump if the strings were not equal
           glo     rc                  ; get comparison flags
           shr                         ; shift equal test bit into df
           lbdf    c_nocond            ; if equal test is present, test passes
           lbr     lineend             ; otherwise not
exp_eq:    glo     rc                  ; look for unequal test
           ani     06h
           smi     06h
           lbz     c_nocond            ; if unequal was the test, test passes
           glo     rc                  ; recover test bits
           ani     2                   ; keep only less than flag
           lbz     exp_lt              ; jump if less than is not being tested
           glo     re                  ; recover test result
           smi     0ffh                ; see if result was -1
           lbz     c_nocond            ; test pases
           lbr     lineend             ; otherwise next line
exp_lt:    glo     rc                  ; check for greater than test
           ani     04h
           lbz     lineend             ; test fails if not
           glo     re                  ; recover test result
           smi     1                   ; check for +1
           lbz     c_nocond            ; jump if test passes
           lbr     lineend             ; otherwise end
exp_qt:    mov     rd,dta              ; point to dta
exp_qt1:   lda     r8                  ; get byte from program
           lbz     synerr              ; error if end of line reached before a "
           str     rd                  ; write into temp spaced
           inc     rd
           smi     34                  ; check for ending quote
           lbnz    exp_qt1             ; lo
           dec     rd                  ; move back to quote
           ldi     0                   ; and terminate string
           str     rd
           mov     rd,dta              ; point back to beginning of string
           lbr     exp_str2            ; now do comparison


c_otest:   inc     r8                  ; move past condition byte
           ldi     low overflow        ; get overflow flag
           plo     r9
           ldn     r9
           lbnz    c_nocond            ; jump if overflow
           lbr     lineend             ; otherwise line end
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
           smi     '@'
           lbz     cmd_at
           smi     1                   ; check for A command
           lbz     cmd_a
           smi     1                   ; check for B command
           lbz     cmd_b
           smi     1                   ; check for C command
           lbz     cmd_c
           smi     2                   ; check for E command
           lbz     cmd_e
           smi     1                   ; check for F command
           lbz     cmd_f
           smi     3                   ; check for I command
           lbz     cmd_i
           smi     1                   ; check for J command
           lbz     cmd_j
           smi     1                   ; check for K command
           lbz     cmd_k
           smi     2                   ; check for M command
           lbz     cmd_m
           smi     1                   ; check for N command
           lbz     cmd_n
           smi     1                   ; check for O command
           lbz     cmd_o
           smi     2                   ; check for Q command
           lbz     o_wrmboot
           smi     1                   ; check for R command
           lbz     lineend
           smi     1                   ; check for S command
           lbz     cmd_s
           smi     1                   ; check for T command
           lbz     cmd_t
           smi     1                   ; check for U command
           lbz     cmd_u
           smi     1                   ; check for V command
           lbz     cmd_v
           smi     3                   ; check for Y command
           lbz     cmd_y
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
           sep     scall               ; move past any spaces
           dw      trim
           ldn     r8                  ; get byte from program
           smi     '$'                 ; check for string variable
           lbnz    cmd_a_1             ; jump if not
           inc     r8                  ; move past $ sign
           mov     rf,accept           ; point to accept buffer
           sep     scall               ; set the string variable
           dw      setsvar
           lbr     cmd_a_4
cmd_a_1:   ldn     r8                  ; recover byte from program
           smi     '#'                 ; is it integer marker
           lbz     cmd_a_2             ; jump if so
           ldn     r8                  ; check for varname
           sep     scall               ; check for lowercase letter
           dw      is_lc
           lbdf    cmd_a_3             ; jump if so
           sep     scall               ; check for uppercase letter
           dw      is_uc
           lbdf    cmd_a_3             ; jump if so
           lbr     lineend             ; othwerwise done
cmd_a_2:   inc     r8                  ; move past # symbol
cmd_a_3:   push    r8                  ; save program position
           mov     r8,accept           ; point to accept buffer
           sep     scall               ; convert to 16-bit integer
           dw      atoi
           pop     r8                  ; recover program position
           sep     scall               ; set variable
           dw      setivar
cmd_a_4:   sep     scall               ; move past any spaces
           dw      trim
           ldn     r8                  ; get next program byte
           smi     ','                 ; check for comma
           lbnz    lineend             ; jump if not
           inc     r8                  ; move past comma
           lbr     cmd_a               ; and get more input

; *****************************************
; ***** Command B, Block alloc/dalloc *****
; *****************************************
cmd_b:     sep     scall               ; move past any leading spaces
           dw      trim
           ldi     '='                 ; check for allocate form
           sep     scall
           dw      haschr
           lbdf    cmd_b_a             ; jump if equals found
           sep     scall               ; evaluate expression
           dw      evaluate
           sep     scall               ; deallocate memory block
           dw      dealloc
           lbr     lineend             ; end of command
cmd_b_a:   ldn     r8                  ; get first byte
           smi     '#'                 ; check for hashmark
           lbnz    cmd_b_a1            ; jump if not
           inc     r8                  ; move past hash
cmd_b_a1:  push    r8                  ; save position of variable name
cmd_b_a2:  lda     r8                  ; read nextbyte
           lbz     synerr              ; syntax error if line end found
           smi     '='                 ; looking for = sign
           lbnz    cmd_b_a2            ; loop until found
           sep     scall               ; evalute for block size
           dw      evaluate
           mov     rc,rf               ; move block size for allocate
           sep     scall               ; allocate the memory block
           dw      alloc
           pop     r8                  ; recover address of variable name
           sep     scall               ; set variable to block address
           dw      setivar
           lbr     lineend             ; then on to next line


; ***********************************
; ***** Command C, Compute      *****
; ***********************************
cmd_c:     sep     scall               ; move past any leading spaces
           dw      trim
           ldn     r8                  ; get first byte of varname
           smi     '$'                 ; check for string var
           lbz     cmd_c_s             ; jump if string
           ldn     r8                  ; get first byte of varname
           smi     '#'                 ; check for hash
           lbnz    cmd_c_1             ; jump if not
           inc     r8                  ; move past hash
cmd_c_1:   push    r8                  ; save what should be a variable
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
cmd_c_s:   inc     r8                  ; move past $ symbol
           push    r8                  ; save var name address
cmd_c_s_1: lda     r8                  ; read next byte
           lbz     synerr              ; syntax error if end of line found
           smi     '='                 ; looking for = sign
           lbnz    cmd_c_s_1           ; jump if not found
           sep     scall               ; perform string evaluation
           dw      sevaluate
           pop     r8                  ; recover varname address
           sep     scall               ; and set it
           dw      setsvar
           lbr     lineend             ; done with line

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
; ***** Command F, Read EF flags *****
; ************************************
cmd_f:     sep     scall               ; move past any leading spaces
           dw      trim
           ldn     r8                  ; check for hash
           smi     '#'
           lbnz    cmd_f_1             ; jump if not
           inc     r8                  ; otherwise move past hash
cmd_f_1:   sep     scall               ; read EF flags
           dw      readef
           plo     rf                  ; put into value for variable
           ldi     0                   ; high byte is zero
           phi     rf
           sep     scall               ; set the variable
           dw      setivar
           lbr     lineend             ; then on to the next line

; **************************************
; ***** Command I, Input from port *****
; **************************************
cmd_i:     sep     scall               ; move past any leading spaces
           dw      trim
           ldn     r8                  ; get first byte of varname
           smi     '#'                 ; check for hash
           lbnz    cmd_i_1             ; jump if not
           inc     r8                  ; move past hash
cmd_i_1:   push    r8                  ; save what should be a variable
cmd_i_a:   lda     r8                  ; read next byte
           lbz     synerr              ; syntax error if end of line found
           smi     '='                 ; looking for equals
           lbnz    cmd_i_a             ; loop until = found
           sep     scall               ; evaluate expression
           dw      evaluate
           glo     rf                  ; check port for range
           lbz     rangeerr            ; jump if out of range
           ani     0f8h                ; check if greater than 7
           lbnz    rangeerr            ; jump if out of range
           mov     rc,cmd_in           ; where to write inp command
           glo     rf                  ; recover value
           ori     068h                ; convert to INP command
           str     rc                  ; store for exeuction
cmd_in:    db      0c4h
           plo     rf                  ; put read value into RF
           ldi     0                   ; clear high byte
           phi     rf
           pop     r8                  ; pop what should be variable name
           sep     scall               ; set variable to value
           dw      setivar
           lbr     lineend             ; and then continue

; ************************************
; ***** Command J, jump to label *****
; ************************************
cmd_j:     sep     scall               ; move past any spaces
           dw      trim
cmd_j_2:   ldn     r8                  ; get first byte of argument
           smi     '*'                 ; check for a star
           lbnz    cmd_j_1             ; otherwise computed jump
           lbr     findline            ; find line with label
cmd_j_1:   sep     scall               ; compute expression
           dw      evaluate
           sep     scall               ; move past any spaces
           dw      trim
           ldn     r8                  ; get next character
           smi     ','                 ; check for comma
           lbz     cmd_j_a             ; jump if on-goto style jump
           sep     scall               ; evaluate expression
           dw      evaluate
           mov     rb,dta              ; where to assemble label
           ldi     '*'                 ; need to start with *
           str     rb
           inc     rb
           sep     scall               ; convert number to string
           dw      itoa
           mov     r8,dta              ; point to constructed label
           lbr     findline            ; and attempt to jump
cmd_j_a:   glo     rf                  ; get expression result
           lbz     lineend             ; no jump if zero
           inc     r8                  ; move past comma
cmd_j_a1:  sep     scall               ; move past any spaces
           dw      trim
           ldn     r8                  ; get byte from line
           lbz     lineend             ; no jump if end hit
           dec     rf                  ; decrement expression result
           glo     rf                  ; see if at correct entry
           lbnz    cmd_j_a2            ; jump if not
           ldn     r8                  ; get byte from label
           smi     '*'                 ; it must be a star
           lbnz    synerr              ; otherwise syntax error
           lbr     findline            ; attempt jump
cmd_j_a2:  lda     r8                  ; get next byte
           lbz     lineend             ; no jump if end of line hit
           smi     ','                 ; check for a comma
           lbnz    cmd_j_a2            ; keep looking
           lbr     cmd_j_a1            ; check for correct slot

; ****************************************
; ***** Command K, type ASCII code   *****
; ****************************************
cmd_k:     sep     scall               ; move past any spaces
           dw      trim
           sep     scall               ; evaluate port
           dw      evaluate
           glo     rf                  ; check if in range
           sep     scall               ; display it
           dw      o_type
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; get next character
           smi     ','                 ; check for comma
           lbz     cmd_k               ; process next code
           lbr     lineend             ; otherwise done

; ****************************
; ***** Command M, Match *****
; ****************************
cmd_m:     sep     scall               ; move past leading spaces
           dw      trim
           ldn     r8                  ; check for string variable
           smi     '$'
           lbnz    cmd_m_1             ; jump if not
           inc     r8                  ; move past $ symbol
           sep     scall               ; retrieve variable
           dw      getsvar
           mov     rd,accept           ; point to accept buffer
           lbr     cmd_m_2             ; check for match
cmd_m_1:   mov     rd,accept           ; point to accept buffer
           mov     rf,r8               ; text to search for
cmd_m_2:   sep     scall               ; search for string
           dw      strstr
           lbdf    cmd_m_yes           ; jump if matched
cmd_m_no:  lda     r8                  ; get byte from program text
           lbz     cmd_m_no2           ; jump if end of line found
           smi     ','                 ; look for comma
           lbz     cmd_m               ; jump if found, try next option
           lbr     cmd_m_no            ; keep looking for end or ,
cmd_m_no2: ldi     low matched         ; point to matched flag
           plo     r9
           ldi     0                   ; signal no match
           str     r9
           lbr     lineend             ; and on to the next line
cmd_m_yes: ldi     low matched         ; point to matched flag
           plo     r9
           ldi     0ffh                ; signal match
           str     r9
           mov     rf,accept           ; point to accept buffer
           mov     rb,dta              ; point to free space
           push    r8                  ; save program counter
cmd_m_y1:  glo     rc                  ; get byte count
           lbz     cmd_m_y2            ; jump if end
           lda     rf                  ; read byte from accept buffer
           str     rb                  ; writ to buffer
           inc     rb
           dec     rc
           lbr     cmd_m_y1            ; loop until all bytes before match
cmd_m_y2:  push    rf                  ; save position
           ldi     0                   ; terminate string
           str     rb
           mov     r8,left             ; point to LEFT variable
           mov     rf,dta              ; point to left data
           sep     scall               ; store it
           dw      setsvar
           pop     rf                  ; recover position
           mov     rb,dta              ; point to free space
cmd_m_y3:  glo     rf                  ; compare rf to rd
           str     r2
           glo     rd
           sm
           lbnz    cmd_m_y4            ; jump if they do not match
           ghi     rf                  ; check high byte as well
           str     r2
           ghi     rd
           sm
           lbz     cmd_m_y5            ; jump if at aend
cmd_m_y4:  lda     rf                  ; get byte from input
           str     rb                  ; write into output
           inc     rb
           lbr     cmd_m_y3            ; loop until match string copied
cmd_m_y5:  ldi     0                   ; terminate string
           str     rb
           mov     r8,match            ; point to match variable
           mov     rf,dta              ; point to match text
           sep     scall               ; and store it
           dw      setsvar
           mov     r8,right            ; point to $RIGHT
           mov     rf,rd               ; point to text after match
           sep     scall               ; set the variable
           dw      setsvar
           pop     r8                  ; recover program counter
           lbr     lineend             ; and on to the next line

; ****************************************
; ***** Command O, Out to port       *****
; ****************************************
cmd_o:     sep     scall               ; move past any spaces
           dw      trim
           sep     scall               ; evaluate port
           dw      evaluate
           glo     rf                  ; check if in range
           lbz     rangeerr            ; jump if not
           ani     0f8h                ; check for > 7
           lbnz    rangeerr            ; jump if too high
           mov     rd,cmd_out          ; where to write it
           glo     rf                  ; recover port
           ori     060h                ; convert ot output instruction
           str     rd
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; get next character
           smi     ','                 ; must be a comma
           lbnz    synerr              ; syntax error if not
           sep     scall               ; move past any spaces
           dw      trim
           sep     scall               ; evaluate expression
           dw      evaluate
           dec     r2                  ; prepare for out
           glo     rf    
           str     r2                  ; store for out
cmd_out:   db      0c4h
           lbr     lineend             ; on to the next line

; ****************************************
; ***** Command N, type if matched=0 *****
; ****************************************
cmd_n:     ldi     low matched         ; point to matched flag
           plo     r9
           ldn     r9                  ; get matched flag
           lbz     cmd_t               ; jump if matched == 0
           lbr     lineend             ; otherwise ignore rest of line

; ****************************************
; ***** Command S, Read/Write memory *****
; ****************************************
cmd_s:     sep     scall               ; move past any leading spaces
           dw      trim
           ldi     ','                 ; need to see if write mode
           sep     scall               ; is a comma present
           dw      haschr
           lbdf    cmd_s_2             ; jump if so
           ldn     r8                  ; get first byte of varname
           smi     '#'                 ; check for hash
           lbnz    cmd_s_1             ; jump if not
           inc     r8                  ; move past hash
cmd_s_1:   push    r8                  ; save what should be a variable
cmd_s_a:   lda     r8                  ; read next byte
           lbz     synerr              ; syntax error if end of line found
           smi     '='                 ; looking for equals
           lbnz    cmd_s_a             ; loop until = found
           sep     scall               ; evaluate expression
           dw      evaluate
           lda     rf                  ; read byte from memory
           plo     rf
           ldi     0                   ; clear high byte
           phi     rf
           pop     r8                  ; pop address of varname
           sep     scall               ; set variable to read value
           dw      setivar
           lbr     lineend             ; then on to next line
cmd_s_2:   sep     scall               ; get address
           dw      evaluate
           mov     rc,rf               ; move address to rc
cmd_s_2a:  sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; get next byte from program
           lbz     lineend             ; jump if end of line
           smi     ','                 ; otherwise must be comma
           lbnz    synerr              ; if not, syntax error
           push    rc                  ; save address
           sep     scall               ; evaluate next argument
           dw      evaluate
           pop     rc                  ; recover address
           glo     rf                  ; write argument to memory
           str     rc
           inc     rc                  ; and increment
           lbr     cmd_s_2a            ; loop for more

; ***************************
; ***** Command T, Type *****
; ***************************
cmd_t:     lda     r8                  ; read byte from arguments
           lbz     cmd_t_dn            ; jump if line end found
           plo     re                  ; save character
           smi     123                 ; check for expression 
           lbz     cmd_t_exp           ; jump if so
           glo     re
           smi     '\'                 ; check for backslash
           lbz     cmd_t_esc           ; Need to check escape codes
           glo     re                  ; recover character
           smi     '#'                 ; check for integer variable
           lbnz    cmd_t_1             ; jump if not
           sep     scall               ; get variable value
           dw      getivar
           mov     rb,dta              ; where to build it
           sep     scall               ; and display it
           dw      itoa
           mov     rf,dta              ; now display it
           sep     scall
           dw      o_msg
           lbr     cmd_t               ; loop for more characters
cmd_t_1:   glo     re                  ; recover character
           smi     '$'                 ; check for string variable
           lbnz    cmd_t_2             ; jump if not
           sep     scall               ; get variable value
           dw      getsvar
cmd_t_1a:  lda     rf                  ; get byte from string
           lbz     cmd_t               ; back to main print loop if term.
           sep     scall               ; display it
           dw      o_type
           lbr     cmd_t_1a            ; keep display until terminator
cmd_t_2:   glo     re                  ; recover character
cmd_t_go:  sep     scall               ; display byte
           dw      o_type
           lbr     cmd_t               ; loop until end of line
cmd_t_dn:  sep     scall               ; display cr/lf
           dw      crlf
           lbr     lineend             ; and then process next line
cmd_t_esc: lda     r8                  ; get next character
           lbz     lineend             ; end of line is done
           plo     re                  ; save a copy of it
           smi     'b'                 ; check for b
           lbnz    cmd_t_a             ; jump if not
           ldi     7                   ; bell character
           lbr     cmd_t_go            ; output it
cmd_t_a:   smi     3                   ; check for 'e'
           lbnz    cmd_t_b             ; jump if not
           ldi     27                  ; send esc
           lbr     cmd_t_go
cmd_t_b:   smi     9                   ; check for 'n'
           lbnz    cmd_t_c             ; jump if not
           ldi     10                  ; output LF
           lbr     cmd_t_go
cmd_t_c:   smi     4                   ; check for 'r'
           lbnz    cmd_t_d             ; jump if not
           ldi     13                  ; output CR
           lbr     cmd_t_go
cmd_t_d:   smi     2                   ; check for 't'
           lbnz    cmd_t_2             ; just output character if not
           ldi     9                   ; output tab
           lbr     cmd_t_go
cmd_t_exp: sep     scall               ; evaluate expression
           dw      evaluate
           mov     rb,dta              ; where to put it
           sep     scall               ; display result
           dw      itoa
           mov     rf,dta              ; now display it
           sep     scall
           dw      o_msg
           lda     r8                  ; next char must be }
           smi     '}'
           lbnz    synerr              ; jump if not
           lbr     cmd_t               ; keep processing line

; *****************************************
; ***** Command U, jump to subroutine *****
; *****************************************
cmd_u:     sep     scall               ; move past any spaces
           dw      trim
           ldn     r8                  ; get first byte of argument
           smi     '*'                 ; check for a star
           lbnz    cmd_u_1             ; otherwise computed jump
cmd_u_2:   ghi     r7                  ; place current stack pointer on stack
           str     ra
           inc     ra
           glo     r7
           str     ra
           inc     ra
           lbr     findline            ; find line with label
cmd_u_1:   sep     scall               ; compute expression
           dw      evaluate
           sep     scall               ; move past any spaces
           dw      trim
           ldn     r8                  ; get next character
           smi     ','                 ; check for comma
           lbz     cmd_u_a             ; jump if on-goto style jump
           sep     scall               ; evaluate expression
           dw      evaluate
           mov     rb,dta              ; where to assemble label
           ldi     '*'                 ; need to start with *
           str     rb
           inc     rb
           sep     scall               ; convert number to string
           dw      itoa
           mov     r8,dta              ; point to constructed label
           lbr     cmd_u_2             ; and attempt to jump
cmd_u_a:   glo     rf                  ; get expression result
           lbz     lineend             ; no jump if zero
           inc     r8                  ; move past comma
cmd_u_a1:  sep     scall               ; move past any spaces
           dw      trim
           ldn     r8                  ; get byte from line
           lbz     lineend             ; no jump if end hit
           dec     rf                  ; decrement expression result
           glo     rf                  ; see if at correct entry
           lbnz    cmd_u_a2            ; jump if not
           ldn     r8                  ; get character from line
           smi     '*'                 ; must be a star
           lbnz    synerr              ; otherwise syntax error
           lbz     cmd_u_2             ; attempt jump
cmd_u_a2:  lda     r8                  ; get next byte
           lbz     lineend             ; no jump if end of line hit
           smi     ','                 ; check for a comma
           lbnz    cmd_u_a2            ; keep looking
           lbr     cmd_u_a1            ; check for correct slot

; ***********************************
; ***** Command V, VarPtr       *****
; ***********************************
cmd_v:     sep     scall               ; move past any leading spaces
           dw      trim
           push    r8                  ; save destination variable name
cmd_v_a:   lda     r8                  ; read next byte
           lbz     synerr              ; syntax error if end of line found
           smi     '='                 ; looking for equals
           lbnz    cmd_v_a             ; loop until = found
           sep     scall               ; move past any spaces
           dw      trim
           ldn     r8                  ; get first byte of varname
           str     r2                  ; store for comparisons
           ldi     '$'
           sm
           lbz     cmd_v_s             ; jump if string variable
           ldi     '#'                 ; check for explicit integer
           sm
           lbnz    cmd_v_i             ; jump if integer
           inc     r8                  ; move past hash mark
cmd_v_i:   ldi     1                   ; want to search for integer variable
           plo     rc
           lbr     cmd_v_1             ; go find it
cmd_v_s:   inc     r8                  ; move past $ symbol
           ldi     2                   ; want to search for string variable
           plo     rc
cmd_v_1:   sep     scall               ; attemp to find variable
           dw      findvar
           lbnf    varerr              ; error out if not found
           pop     r8                  ; pop what should be variable name
           glo     rc                  ; check for string variable
           smi     2
           lbnz    cmd_v_2             ; jump if not
           lda     rf                  ; retrieve where string is stored
           plo     re
           ldn     rf
           plo     rf
           glo     re
           phi     rf
cmd_v_2:   sep     scall               ; set variable to value
           dw      setivar
           lbr     lineend             ; and then continue

; *****************************************
; ***** Command Y, type if matched<>0 *****
; *****************************************
cmd_y:     ldi     low matched         ; point to matched flag
           plo     r9
           ldn     r9                  ; get matched flag
           lbnz    cmd_t               ; jump if matched != 0
           lbr     lineend             ; otherwise ignore rest of line

; *****************************************
; ***** Command @, Call ML subroutine *****
; *****************************************
cmd_at:    sep     scall               ; move past any leading spaces
           dw      trim
           ldn     r8                  ; get first byte of varname
           smi     '#'                 ; check for hash
           lbnz    cmd_at_1            ; jump if not
           inc     r8                  ; move past hash
cmd_at_1:  push    r8                  ; save what should be a variable
cmd_at_a:  lda     r8                  ; read next byte
           lbz     synerr              ; syntax error if end of line found
           smi     '='                 ; looking for equals
           lbnz    cmd_at_a            ; loop until = found
           sep     scall               ; evaluate expression
           dw      evaluate
           mov     rc,cmd_at_ad        ; point to address
           ghi     rf                  ; write address into jump
           str     rc
           inc     rc
           glo     rf
           str     rc
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; see if a parameter is provided
           lbz     cmd_at_go           ; jump if not
           smi     ','
           lbnz    synerr              ; if not comma, then syntax error
           sep     scall               ; evaluate argument
           dw      evaluate
cmd_at_go: sep     scall               ; call the routine
cmd_at_ad: dw      o_wrmboot
           pop     r8                  ; pop what should be variable name
           sep     scall               ; set variable to value
           dw      setivar
           lbr     lineend             ; process next line


experr:    sep     scall               ; display error message
           dw      o_inmsg
           db      'Expression error: ',0
           lbr     errline

rangeerr:  sep     scall               ; display error message
           dw      o_inmsg
           db      'Range error: ',0
           lbr     errline

varerr:    sep     scall               ; display error message
           dw      o_inmsg
           db      'Variable not found: ',0
           lbr     errline

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

; *********************************************************************
; *****                   Expression Evaluator                    *****
; *********************************************************************

; **********************************
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
mul16:     push    rf                  ; save consumed register
           sep     scall               ; normalize numbers
           dw      mdnorm
           plo     re                  ; save for later
           ldi     0                   ; zero out total
           phi     rf
           plo     rf
mulloop:   glo     rd                  ; get low of multiplier
           lbnz    mulcont             ; continue multiplying if nonzero
           ghi     rd                  ; check hi byte as well
           lbnz    mulcont
           mov     rc,rf               ; transfer answer
           glo     re                  ; get sign comparison
           shr                         ; shift into DF
           lbnf    mulexit             ; jump if signs were the same
           glo     rc                  ; 2s compliment answer
           xri     0ffh
           adi     1
           plo     rc
           ghi     rc
           xri     0ffh
           adci    0
           phi     rc
mulexit:   pop     rf                  ; recover consumed registers
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


; *********************************************
; *** Modulo function of 2 16 bit numbers   ***
; *** RC = RC % RD                          ***
; *********************************************
mod16:     push    rc                  ; will need both original numbers later
           push    rd
           sep     scall               ; perform division
           dw      div16
           pop     rd                  ; multiply by original RD
           sep     scall
           dw      mul16
           pop     rd                  ; recover original RC
           glo     rc                  ; RC = RD - RC
           str     r2
           glo     rd
           sm
           plo     rc
           ghi     rc
           str     r2
           ghi     rd
           smb
           phi     rc
           sep     sret                ; return to caller

; *****************************************************
; ***** Perform calculation on top of token stack *****
; *****************************************************
reduce:    ldi     low numtokens       ; need number of tokens
           plo     r9
           ldn     r9
           lbz     return_0            ; return if nothing on stack
           smi     1                   ; check for 1 token
           lbz     return_0            ; also nothing to do if only 1 token

           dec     rb                  ; retrieve argument 2
           ldn     rb
           plo     rd
           dec     rb
           ldn     rb
           phi     rd
           dec     rb                  ; get type
           ldn     rb

           dec     rb                  ; get operation
           dec     rb
           dec     rb
           ldn     rb
           plo     rf

; ----- OP_FPEEK RC = peek(RD)
           smi     OP_FPEEK            ; check for peek
           lbnz    reduce_1a           ; jump if not
           ldn     rd                  ; 
           plo     rc
           ldi     0
           phi     rc
           lbr     r_done1

; ----- OP_FEF RC = ef(RD)
reduce_1a: glo     rf                  ; recover command
           smi     OP_FEF              ; check for ef
           lbnz    reduce_1b           ; jump if not
           sep     scall               ; get EF pins
           dw      readef
           plo     rc
           ldi     0
           phi     rc
           lbr     r_done1

; ----- OP_FINP RC = inp(RD)
reduce_1b: glo     rf                  ; recover command
           smi     OP_FINP             ; check for input
           lbnz    reduce_1c           ; jump if not
           mov     rc,red_inp          ; point to command
           glo     rd                  ; extract port
           ani     7
           ori     068h                ; convert to input command
           str     rc                  ; write it
red_inp:   db      0                   ; input command written here
           plo     rc
           ldi     0
           phi     rc
           lbr     r_done1

; ----- OP_FEF RC = rnd(RD)
reduce_1c: glo     rf                  ; recover command
           smi     OP_FRND             ; check for rnd
           lbnz    reduce_1d           ; jump if not
           sep     scall               ; get random number
           dw      fn_lfsr
           ghi     rc                  ; no negatives
           ani     07fh
           phi     rc
           sep     scall               ; and perform modulo function against range
           dw      mod16
           lbr     r_done1

; ----- OP_FLEN RC = len($RD)
reduce_1d: glo     rf                  ; recover command
           smi     OP_FLEN             ; check for len
           lbnz    reduce_1e           ; jump if not
           push    rf                  ; save rf
           mov     rf,rd               ; move string address for strlen
           sep     scall
           dw      strlen
           pop     rf                  ; recover rf
           lbr     r_done1

; ----- OP_FASC RC = asc($RD)
reduce_1e: glo     rf                  ; recover command
           smi     OP_FASC             ; check for asc
           lbnz    reduce_1f           ; jump if not
           lda     rd                  ; get first byte of string
           plo     rc
           ldi     0
           phi     rc
           lbr     r_done1

; ----- OP_FVAL RC = val($RD)
reduce_1f: glo     rf                  ; recover command
           smi     OP_FVAL             ; check for VAL
           lbnz    reduce_1g           ; jump if not
           push    r8                  ; save consumed registers
           push    rf
           mov     r8,rd               ; move ascii address to r8
           sep     scall               ; convert to binary
           dw      atoi
           mov     rc,rf               ; move result
           pop     rf                  ; recover registers
           pop     r8
           lbr     r_done1

; ----- OP_FASC RC = fre($RD)
reduce_1g: glo     rf                  ; recover command
           smi     OP_FFRE             ; check for fre
           lbnz    reduce_1h           ; jump if not
           ldi     low heap            ; get address of heap
           plo     r9
           lda     r9                  ; get heap start address
           phi     rc
           lda     r9
           plo     rc
           ldi     low varend+1        ; need end of variable table
           plo     r9
           ldn     r9                  ; get msb
           str     r2                  ; store for subtract
           glo     rc                  ; subtract from heap start
           sm
           plo     rc
           dec     r9                  ; move to msb of varend
           ldn     r9                  ; retrieve it
           str     r2                  ; store for subtract
           ghi     rc
           smb
           phi     rc
           lbr     r_done1

; ----- OP_FHEAP RC = heap($RD)
reduce_1h: glo     rf                  ; recover command
           smi     OP_FHEAP            ; check for heap
           lbnz    reduce_1i           ; jump if not
           sep     scall               ; get amount of free heap memory
           dw      hfree
           lbr     r_done1

reduce_1i:

           ldn     r9                  ; get number of tokens
           smi     3                   ; see if less than 3
           lbdf    reduce2             ; jump if not
           inc     rb                  ; put rb back
           inc     rb
           inc     rb
           inc     rb
           inc     rb
           inc     rb
return_0:  ldi     0                   ; indicate nothing done
           shr
           sep     sret                ; otherwise return to caller
return_1:  ldi     1                   ; indicate something done
           shr 
           sep     sret

;reduce2:   dec     rb                  ; retrieve argument 2
;           ldn     rb                  ; retrieve lsb
;           plo     rd
;           dec     rb                  ; retrieve msb
;           ldn     rb
;           phi     rd
;           dec     rb                  ; get type
;           ldn     rb
;     
;           dec     rb                  ; get operation
;           dec     rb
;           dec     rb
;           ldn     rb
;           plo     rf

reduce2:   dec     rb                  ; retrieve argument 1
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
addsub:    ghi     rc                  ; compare signs
           str     r2
           ghi     rd
           xor
           plo     re                  ; save comparison for now
           dec     r2                  ; preserve original value
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
           inc     r2                  ; put r2 back
           glo     re                  ; recover sign comparison
           shl                         ; shift into DF
           lbdf    r_done              ; jump if signs were different
           ghi     rc                  ; compare result sign with original
           xor
           shl                         ; shift into DF
           lbnf    r_done              ; jump if sign did not change
           ldi     low overflow        ; need to signal overflow
           plo     r9
           ldi     0ffh
           str     r9
           lbr     r_done
; ----- OP_SUB RC -= RD
r_n_add:   glo     rf                  ; check OP_SUB
           smi     OP_SUB
           lbnz    r_n_sub
           glo     rd                  ; 2s compliment RD
           xri     0ffh
           adi     1
           plo     rd
           ghi     rd
           xri     0ffh
           adci    0
           phi     rd
           lbr     addsub              ; now use addition
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
           lbr     return_1
r_done1:   ldi     OP_NUM              ; push result back on stack
           str     rb
           inc     rb
           ghi     rc
           str     rb
           inc     rb
           glo     rc
           str     rb
           inc     rb
           ldi     low numtokens       ; numtokens -= 1
           plo     r9
           ldn     r9
           smi     1
           str     r9
           lbr     return_1

addop:     plo     re                  ; store op
addoplp:   ldi     low numtokens       ; need to get number of tokens
           plo     r9
           ldn     r9                  ; retrieve number of tokens
           smi     2                   ; see if greater than 1
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

evaluate:  ldi     low overflow        ; point to overflow flag
           plo     r9
           ldi     0                   ; clear it
           str     r9
           ldi     low parens          ; point to parens
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
           lbnf    eval_0              ; jump if not
           sep     scall               ; convert to binary
           dw      atoi
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

; ***** Check for binary number *****
eval_0:    ldn     r8                  ; get byte from input
           smi     '%'                 ; check for binary marker
           lbnz    eval_6              ; jump if not
           inc     r8                  ; move past %
           mov     rf,0                ; start number at 0
eval_0a:   lda     r8                  ; get next byte from input
           lbz     eval_6z             ; jump if end of input
           smi     '0'                 ; was it a zero?
           lbnz    eval_0b             ; jump if not
           ldi     0                   ; clear DF
eval_0s:   shr
           glo     rf                  ; shift number left
           shlc
           plo     rf
           ghi     rf
           shlc
           phi     rf
           lbr     eval_0a             ; and keep processing
eval_0b:   smi     1                   ; check for 1
           lbnz    eval_0c             ; jump if not
           ldi     1                   ; need to shift in a 1
           lbr     eval_0s
eval_0c:   smi     46                  ; check for underscore
           lbz     eval_0a             ; valid character, but no effect
           dec     r8                  ; move back to non-binary character
           lbr     eval_val            ; store number

eval_6:    ldn     r8                  ; get byte from input
           smi     '$'                 ; check for hex constant
           lbnz    eval_1              ; jump if not
           inc     r8                  ; need next character to be sure
           ldn     r8                  ; retrieve it
           sep     scall               ; is it a number
           dw      is_number
           lbdf    eval_6a             ; jump if so
           dec     r8                  ; move pointer back
           lbr     eval_1              ; and check next type
eval_6a:   mov     rf,0                ; clear number
eval_6b:   lda     r8                  ; get next byte
           lbz     eval_6z             ; end of constant if end of input
           smi     '0'                 ; check if below digits
           lbnf    eval_6z             ; jump if so
           smi     10                  ; check for end of numbers
           lbdf    eval_6c             ; jump if so
           adi     10                  ; restore number
eval_6s:   str     r2                  ; save it for later
           ldi     4                   ; need 4 shifts
           plo     re
eval_6d:   glo     rf                  ; shift number
           shl
           plo     rf
           ghi     rf
           shlc
           phi     rf
           dec     re                  ; decrement count
           glo     re                  ; see if done
           lbnz    eval_6d             ; jump if not
           glo     rf                  ; add in new digit
           or
           plo     rf
           lbr     eval_6b             ; keep reading hex digits

eval_6c:   smi     7                   ; check for A
           lbnf    eval_6z             ; jump if below A
           smi     6                   ; check if below F
           lbdf    eval_6e             ; jump if above F
           adi     16                  ; conver to binary
           lbr     eval_6s             ; combine with rest of number
eval_6e:   smi     26                  ; check for a
           lbnf    eval_6z             ; jump if below a
           smi     6                   ; check if below f
           lbdf    eval_6z             ; jump if not
           adi     16                  ; convert to proper binary
           lbr     eval_6s             ; and combine with rest of number
eval_6z:   dec     r8                  ; move back to non-valid character
           lbr     eval_val            ; and store value

; ***** Check for operators *****
eval_1:    sep     scall               ; check for operator
           dw      isop
           lbnf    eval_2              ; jump if not
           sep     scall               ; add the new operator
           dw      addop
           lbr     eval_lp             ; and loop back for more

; ***** Process variables *****
eval_2:    ldn     r8                  ; get byte
           smi     '#'                 ; check for integer marker
           lbz     eval_2a             ; jump if so
           ldn     r8                  ; recover character
           sep     scall               ; see if a variable character
           dw      is_varchr
           lbnf    eval_21             ; jump if not
           dec     r8
eval_2a:   inc     r8                  ; move past #
           sep     scall               ; retrieve variable value
           dw      getivar
           lbr     eval_val            ; store value into next token

eval_21:   ldn     r8                  ; get byte
           smi     '$'                 ; check for string
           lbnz    eval_3              ; jump if not
           inc     r8                  ; must past $ sign
           sep     scall               ; retrieve variable address
           dw      getsvar
           lbr     eval_val            ; store into next token

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

;eval_dn:   ldi     low numtokens       ; point to tokens
;           plo     r9
;           ldn     r9                  ; get number of tokens
;           smi     3                   ; see if less than 3
;           lbnf    eval_ex             ; jump if done
;           sep     scall               ; otherwise call reduce
;           dw      reduce
;           lbr     eval_dn             ; loop for more possible reductions
eval_dn:   sep     scall               ; call reduce
           dw      reduce
           lbdf    eval_dn             ; jump if reduction was dones
eval_ex:   mov     rc,tokens+1         ; point to final value
           lda     rc                  ; and set rf
           phi     rf
           lda     rc
           plo     rf
           sep     sret                ; return to caller

isop:      mov     rf,ops              ; point to ops table
           lbr     tkn_1               ; read token table

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
           db      'pee',('k'+080h),OP_FPEEK
           db      'in',('p'+080h),OP_FINP
           db      'rn',('d'+080h),OP_FRND
           db      'e',('f'+080h),OP_FEF
           db      'le',('n'+080h),OP_FLEN
           db      'as',('c'+080h),OP_FASC
           db      'va',('l'+080h),OP_FVAL
           db      'fr',('e'+080h),OP_FFRE
           db      'hea',('p'+080h),OP_FHEAP
           db      0

; *********************************************
; ***** Get 16-bit unsigned random number *****
; ***** RC - random number                *****
; *********************************************
fn_lfsr:   ldi     16                  ; need to perform 16 shifts
           plo     rc
           push    r7                  ; save r7
lfsr_lp:   ldi     high lfsr           ; point to lfsr
           phi     r7
           ldi     low lfsr
           plo     r7
           inc     r7                  ; point to lsb
           inc     r7
           inc     r7
           ldn     r7                  ; retrieve it
           plo     re                  ; put into re  ( have bit 0)
           shr                         ; shift bit 1 into first position
           str     r2                  ; xor with previous value
           glo     re
           xor
           plo     re                  ; keep copy
           ldn     r2                  ; get value
           shr                         ; shift bit 2 into first position
           str     r2                  ; and combine
           glo     re
           xor
           plo     re
           ldn     r2                  ; now shift to bit 4
           shr
           shr
           str     r2                  ; and combine
           glo     re
           xor
           plo     re
           ldn     r2                  ; now shift to bit 6
           shr
           shr
           str     r2                  ; and combine
           glo     re
           xor
           plo     re
           dec     r7                  ; point to lfsr msb
           dec     r7
           dec     r7
           ldn     r7                  ; retrieve it
           shl                         ; shift high bit to low
           shlc
           str     r2                  ; combine with previous value
           glo     re
           xor
           xri     1                   ; combine with a final 1
           shr                         ; shift new bit into DF
           ldn     r7                  ; now shift the register
           shrc
           str     r7
           inc     r7                  ; now byte 1
           ldn     r7                  ; now shift the register
           shrc
           str     r7
           inc     r7                  ; now byte 2
           ldn     r7                  ; now shift the register
           shrc
           str     r7
           inc     r7                  ; now byte 3
           ldn     r7                  ; now shift the register
           shrc
           str     r7
           dec     rc                  ; decrement count
           glo     rc                  ; see if done
           lbnz    lfsr_lp             ; jump if not
           ldi     high lfsr           ; point to lfsr
           phi     r7
           ldi     low lfsr
           plo     r7
           lda     r7                  ; retrieve 16 bits from register
           plo     rc
           ldn     r7
           phi     rc
           pop     r7                  ; recover r7
           sep     sret                ; and return

; *********************************************************************
; *****              End of  Expression Evaluator                 *****
; *********************************************************************

; *********************************************************************
; *****               String Expression Evaluator                 *****
; *********************************************************************

sevaluate: mov     rd,dta              ; Where to assemble new string
seval_lp:  sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; get byte from program
           lbz     seval_dn            ; jump if end of program line
           plo     re                  ; save a copy
           smi     34                  ; check for quote
           lbz     seval_qt            ; jump if quoted text
           glo     re                  ; recover character
           smi     '$'                 ; check for string variable
           lbz     seval_st            ; jump if so
           glo     re                  ; recover character
           smi     '#'                 ; check for integer variable
           lbz     seval_iv            ; jump if so
           glo     re                  ; check for L()
           ori     020h                ; make lowercase
           plo     re                  ; save this
           smi     'l'
           lbz     seval_lf            ; jump if so
           glo     re                  ; check for M()
           smi     'm'
           lbz     seval_m             ; jump if so
           glo     re                  ; check for R()
           smi     'r'
           lbz     seval_r             ; jump if so
           glo     re                  ; check for C()
           smi     'c'
           lbz     seval_c             ; jump if so
           glo     re                  ; check for S()
           smi     's'
           lbz     seval_s             ; jump if so
           glo     re                  ; check for U()
           smi     'u'
           lbz     seval_u             ; jump if so
           glo     re                  ; check for O()
           smi     'o'
           lbz     seval_o             ; jump if so
           glo     re                  ; check for T()
           smi     't'
           lbz     seval_t             ; jump if so
           glo     re                  ; check for W()
           smi     'w'
           lbz     seval_w             ; jump if so
           glo     re                  ; check for N()
           smi     'n'
           lbz     seval_n             ; jump if so
           lbr     synerr              ; otherwise syntax error
seval_qt:  lda     r8                  ; get next byte
           lbz     synerr              ; syntax error if end of line
           plo     re                  ; save a copy
           smi     34                  ; is it a quote
           lbz     seval_nx            ; jump if so
           glo     re                  ; recover character
           str     rd                  ; store into output
           inc     rd
           lbr     seval_qt            ; keep copying quoted characters
seval_st:  sep     scall               ; retrieve string address
           dw      getsvar
seval_sta: lda     rf                  ; get byte from variable
           lbz     seval_nx            ; done with string
           str     rd                  ; store into destination
           inc     rd
           lbr     seval_sta           ; keep copying string
seval_nx:  sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; get nextbyte
           lbz     seval_dn            ; jump if end of line
           smi     '+'                 ; only plus is allowed
           lbz     seval_lp            ; process next token
           lbr     synerr              ; otherwise syntax error
seval_dn:  ldi     0                   ; terminate constructed string
           str     rd
           mov     rf,dta              ; point back to beginning of string
           sep     sret                ; and return to caller
seval_lf:  lda     r8                  ; get next byte
           smi     '('                 ; must be open parens
           lbnz    synerr              ; otherwise syntax error
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; check for string variable
           smi     '$'
           lbnz    synerr              ; jump if not
           sep     scall               ; get string variable address
           dw      getsvar
seval_lm:  sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; next symbol must be comma
           smi     ','
           lbnz    synerr              ; else syntax error
           push    rf                  ; save string address
           sep     scall               ; evaluate expression
           dw      evaluate
           pop     rc                  ; retrieve string address
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; next symbol must be )
           smi     ')'
           lbnz    synerr              ; otherwise syntax error
seval_lf1: glo     rf                  ; check size
           lbnz    seval_lf2           ; jump if more to copy
           ghi     rf
           lbz     seval_nx            ; done, on to next
seval_lf2: lda     rc                  ; get byte from string
           lbz     seval_nx            ; done if terminator found
           str     rd                  ; store into destination
           inc     rd
           dec     rf                  ; decrement count
           lbr     seval_lf1           ; loop until done
seval_m:   lda     r8                  ; get next byte
           smi     '('                 ; must be open parens
           lbnz    synerr              ; otherwise syntax error
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; check for string variable
           smi     '$'
           lbnz    synerr              ; jump if not
           sep     scall               ; get string variable address
           dw      getsvar
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; next character must be ,
           smi     ','
           lbnz    synerr              ; else syntax error
           push    rf                  ; save string address
           sep     scall               ; evaluate expression
           dw      evaluate
           pop     rc                  ; retrieve string address
seval_m1:  glo     rf                  ; see if done
           lbnz    seval_m2            ; jump if not
           ghi     rf
           lbnz    seval_m2
seval_m3:  mov     rf,rc               ; move string address back to RF
           lbr     seval_lm            ; now use left code to copy
seval_m2:  ldn     rc                  ; get byte from string
           lbz     seval_m3            ; jump if at terminator
           inc     rc                  ; otherwise increment
           dec     rf                  ; decrement count
           lbr     seval_m1            ; loop until start position found
seval_r:   lda     r8                  ; get next byte
           smi     '('                 ; must be open parens
           lbnz    synerr              ; otherwise syntax error
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; check for string variable
           smi     '$'
           lbnz    synerr              ; jump if not
           sep     scall               ; get string variable address
           dw      getsvar
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; next symbol must be comma
           smi     ','
           lbnz    synerr              ; else syntax error
           push    rf                  ; save string address
           sep     scall               ; evaluate expression
           dw      evaluate
           pop     rc                  ; retrieve string address
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; next symbol must be )
           smi     ')'
           lbnz    synerr              ; otherwise syntax error
           push    rf                  ; save count
           ldi     0                   ; zero count
           plo     rb
           phi     rb
seval_r1:  ldn     rc                  ; get byte from string
           lbz     seval_r2            ; jump if end found
           inc     rc                  ; move to next character
           inc     rb                  ; increment count
           lbr     seval_r1            ; loop until end found
seval_r2:  glo     rb                  ; check rb=0
           str     r2
           ghi     rb
           or
           lbz     seval_r3            ; jump if zero
           glo     rf                  ; check rf=0
           str     r2
           ghi     rf
           or
           lbz     seval_r3            ; jump if zero 
           dec     rc                  ; decrement string position
           dec     rb                  ; decrement counts
           dec     rf
           lbr     seval_r2            ; loop until end found
seval_r3:  pop     rf                  ; start found, recover count
           lbr     seval_lf1           ; now use left to copy string
seval_c:   lda     r8                  ; get next byte
           smi     '('                 ; must be open parens
           lbnz    synerr              ; otherwise syntax error
           sep     scall               ; move past any spaces
           dw      trim
           sep     scall               ; evaluate expression
           dw      evaluate
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; next symbol must be )
           smi     ')'
           lbnz    synerr              ; otherwise syntax error
           glo     rf                  ; get byte
           str     rd                  ; and write to output
           inc     rd
           lbr     seval_nx            ; process next token
seval_s:   lda     r8                  ; get next byte
           smi     '('                 ; must be open parens
           lbnz    synerr              ; otherwise syntax error
           sep     scall               ; move past any spaces
           dw      trim
           sep     scall               ; evaluate expression
           dw      evaluate
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; next symbol must be )
           smi     ')'
           lbnz    synerr              ; otherwise syntax error
           glo     rf                  ; get byte
           mov     rb,rd               ; set buffer for atoi
           sep     scall               ; perform atoi
           dw      itoa
           mov     rd,rb               ; set output pointer after number
           lbr     seval_nx            ; process next token
seval_u:   lda     r8                  ; get next byte
           smi     '('                 ; must be open parens
           lbnz    synerr              ; otherwise syntax error
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; check for string variable
           smi     '$'
           lbnz    synerr              ; jump if not
           sep     scall               ; get string variable address
           dw      getsvar
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; next symbol must be )
           smi     ')'
           lbnz    synerr              ; otherwise syntax error
seval_u1:  lda     rf                  ; read byte from string
           lbz     seval_nx            ; done if terminator encountered
           sep     scall               ; is it lowercase
           dw      is_lc
           lbnf    seval_u2            ; jump if not
           smi     32                  ; convert to uppercase
seval_u2:  str     rd                  ; write to destination
           inc     rd
           lbr     seval_u1            ; process until done
seval_o:   lda     r8                  ; get next byte
           smi     '('                 ; must be open parens
           lbnz    synerr              ; otherwise syntax error
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; check for string variable
           smi     '$'
           lbnz    synerr              ; jump if not
           sep     scall               ; get string variable address
           dw      getsvar
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; next symbol must be )
           smi     ')'
           lbnz    synerr              ; otherwise syntax error
seval_o1:  lda     rf                  ; read byte from string
           lbz     seval_nx            ; done if terminator encountered
           sep     scall               ; is it lowercase
           dw      is_uc
           lbnf    seval_o2            ; jump if not
           adi     32                  ; convert to uppercase
seval_o2:  str     rd                  ; write to destination
           inc     rd
           lbr     seval_o1            ; process until done
seval_iv:  sep     scall               ; retrieve variable value
           dw      getivar
           mov     rb,rd               ; set buffer for atoi
           sep     scall               ; perform atoi
           dw      itoa
           mov     rd,rb               ; set output pointer after number
           lbr     seval_nx            ; process next token
seval_t:   lda     r8                  ; get next byte
           smi     '('                 ; must be open parens
           lbnz    synerr              ; otherwise syntax error
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; check for string variable
           smi     '$'
           lbnz    synerr              ; jump if not
           sep     scall               ; get string variable address
           dw      getsvar
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; next symbol must be )
           smi     ')'
           lbnz    synerr              ; otherwise syntax error
           sep     scall               ; move past whitespace
           dw      seval_trm
seval_t3:  lda     rf                  ; get next byte
           lbz     seval_nx            ; jump if done
           str     rd                  ; write to destination
           inc     rd
           lbr     seval_t3            ; process until done
seval_w:   lda     r8                  ; get next byte
           smi     '('                 ; must be open parens
           lbnz    synerr              ; otherwise syntax error
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; check for string variable
           smi     '$'
           lbnz    synerr              ; jump if not
           sep     scall               ; get string variable address
           dw      getsvar
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; next symbol must be )
           smi     ')'
           lbnz    synerr              ; otherwise syntax error
           sep     scall               ; move past whitespace
           dw      seval_trm
seval_w3:  lda     rf                  ; get next byte
           lbz     seval_nx            ; jump if done
           plo     re                  ; save character
           smi     9                   ; check for tab
           lbz     seval_nx            ; jump if so
           smi     23                  ; check for space
           lbz     seval_nx            ; jump if so
           glo     re                  ; recover character
           str     rd                  ; write to destination
           inc     rd
           lbr     seval_w3            ; process until done
seval_n:   lda     r8                  ; get next byte
           smi     '('                 ; must be open parens
           lbnz    synerr              ; otherwise syntax error
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; check for string variable
           smi     '$'
           lbnz    synerr              ; jump if not
           sep     scall               ; get string variable address
           dw      getsvar
           sep     scall               ; move past any spaces
           dw      trim
           lda     r8                  ; next symbol must be )
           smi     ')'
           lbnz    synerr              ; otherwise syntax error
           sep     scall               ; move past whitespace
           dw      seval_trm
seval_n3:  lda     rf                  ; get next byte
           lbz     seval_nx            ; jump if done
           plo     re                  ; save character
           smi     9                   ; check for tab
           lbz     seval_n4            ; jump if so
           smi     23                  ; check for space
           lbz     seval_n4            ; jump if so
           lbr     seval_n3            ; process until done
seval_n4:  sep     scall               ; move past whitespace
           dw      seval_trm
           lbr     seval_t3            ; copy rest of string

seval_trm: lda     rf                  ; read byte from string
           lbz     seval_trt           ; done
           smi     9                   ; check for tab
           lbz     seval_trm           ; jump if tab
           smi     23                  ; check for space
           lbz     seval_trm           ; jump if space
seval_trt: dec     rf                  ; move back to non-whitespace
           sep     sret                ; and return

; *********************************************************************
; *****            End of String Expression Evaluator             *****
; *********************************************************************

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
; ***** Find/create variable                            *****
; ***** R8   - Pointer to variable name                 *****
; ***** RC.0 - Variable type                            *****
; ***** Returns: RF - Address of variable data          *****
; *****          R8 - First character after name        *****
; ***********************************************************
findcrvar: sep     scall               ; see if var already exists
           dw      findvar
           lbdf    vexists             ; jump if the variable exists
           mov     rd,rf               ; keep copy of variable address
           glo     rc                  ; move type to high byte
           str     rf                  ; store variable type
           ldi     4                   ; set count
           plo     rc
           inc     rf                  ; point to where name goes
newvar1:   lda     r8                  ; get byte from var name
           sep     scall               ; check for valid character
           dw      is_varchr
           lbnf    newvar2             ; jump if not
           str     rf                  ; store into var table
           inc     rf
           inc     rc                  ; increment count
           lbr     newvar1             ; loop until name is copied
newvar2:   dec     r8                  ; move back to non-var character
           ldi     0                   ; terminate name
           str     rf
           inc     rf                  ; rf pointing at var data
           ldi     0                   ; write new end of table
           str     rf                  ; set initial value to zero
           inc     rf                  ; move past data
           str     rf
           inc     rf
           str     rf                  ; set end of table
           ldi     low varend          ; end of variable table
           plo     r9
           ghi     rf                  ; write new end
           str     r9
           inc     r9
           glo     rf
           str     r9
           dec     rf                  ; move rf back to data
           dec     rf
           glo     rc                  ; get count
           shl                         ; shift it two bits
           shl
           str     r2                  ; need to combine with type
           ldn     rd
           or
           str     rd                  ; write combined value
           sep     scall               ; check for out of memory
           dw      checkeom
vexists:   sep     sret                ; and return to caller

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
           sep     scall               ; find/create variable
           dw      findcrvar
           irx                         ; set variable value
           ldxa
           str     rf
           inc     rf
           ldx
           str     rf
           pop     rd                  ; recover consumed registers
           pop     rc
           sep     sret                ; and return to caller

; ***********************************************************
; ***** Get string variable value                       *****
; ***** R8   - Pointer to variable name                 *****
; ***** Returns: when found                             *****
; *****          RF - Variable value                    *****
; *****          R8 - First character after name        *****
; ***********************************************************
getsvar:   ldi     2                   ; search for string variable
           plo     rc
           sep     scall               ; find variable address
           dw      findvar
           lbnf    varntfnd            ; jump if variable not found
           lda     rf                  ; get variable value
           plo     re                  ; save for a moment
           lda     rf                  ; get lsb
           plo     rf
           glo     re                  ; recover msb
           phi     rf
           sep     sret                ; and return to caller

; ***************************************************
; ***** Set string variable                     *****
; ***** R8 - Pointer to variable name           *****
; ***** RF - Pointer to source string           *****
; ***************************************************
setsvar:   push    rc                  ; save used registers
           push    rd
           push    rf                  ; save value
           ldi     2                   ; search for string variable
           plo     rc
           sep     scall               ; find/create variable
           dw      findcrvar
           mov     rd,rf               ; move variable data to RD
           lda     rd                  ; need to see if memory is allocated
           phi     rf                  ; put into RF in case need to dealloc
           str     r2
           ldn     rd
           plo     rf
           or
           dec     rd                  ; move pointer back
           lbz     setsvar_1           ; jump if not allocated
           sep     scall               ; deallocate the memory
           dw      dealloc
setsvar_1: pop     rf                  ; recover string
           push    rf                  ; and keep on stack
           sep     scall               ; get length of string
           dw      strlen
           inc     rc                  ; plus 1 byte for terminator
           push    rd                  ; save variable data pointer
           sep     scall               ; allocate memory for string
           dw      alloc
           pop     rd                  ; recover variable data
           ghi     rf                  ; store alloc memory into var
           str     rd
           inc     rd
           glo     rf
           str     rd
           mov     rd,rf               ; move destination to rd
           pop     rf                  ; recover source string
           sep     scall               ; and copy to destination
           dw      strcpy
           pop     rd                  ; recover consumed registers
           pop     rc
           sep     sret                ; and return to caller

; *************************************************************************
; *****                 End of  Variable Handling                     *****
; *************************************************************************

; *************************************************************************
; *****                       Heap Manager                            *****
; *************************************************************************

; *******************************************
; ***** Allocate memory                 *****
; ***** RC - requested size             *****
; ***** Returns: RF - Address of memory *****
; *******************************************
alloc:     ldi     low heap            ; get heap address
           plo     r9
           lda     r9
           phi     rd
           ldn     r9
           plo     rd
           dec     r9                  ; leave pointer at heap address
alloc_1:   lda     rd                  ; get flags byte
           lbz     alloc_new           ; need new if end of table
           plo     re                  ; save flags
           lda     rd                  ; get block size
           phi     rf
           lda     rd
           plo     rf
           glo     re                  ; is block allocated?
           smi     2
           lbz     alloc_nxt           ; jump if so
           glo     rc                  ; subtract size from block size
           str     r2
           glo     rf
           sm
           plo     rf
           ghi     rc
           str     r2
           ghi     rf
           smb
           phi     rf                  ; RF now has difference
           lbnf    alloc_nxt           ; jumpt if block is too small
           ghi     rf                  ; see if need to split block
           lbnz    alloc_sp            ; jump if so
           glo     rf                  ; get low byte of difference
           ani     0f8h                ; want to see if at least 8 extra bytes
           lbnz    alloc_sp            ; jump if so
alloc_2:   mov     rf,rd               ; set address for return
           dec     rd                  ; move back to flags byte
           dec     rd
           dec     rd
           ldi     2                   ; mark block as used
           str     rd
           sep     sret                ; and return to caller
alloc_sp:  push    rd                  ; save this address
           dec     rd                  ; move to lsb of block size
           glo     rc                  ; write requested size
           str     rd
           dec     rd
           ghi     rc                  ; write msb of size
           str     rd
           inc     rd                  ; move back to data
           inc     rd
           glo     rc                  ; now add size
           str     r2
           glo     rd
           add
           plo     rd
           ghi     rd
           str     r2
           ghi     rc
           adc
           phi     rd                  ; rd now points to new block
           ldi     1                   ; mark as a free block
           str     rd
           inc     rd
           dec     rf                  ; remove 3 bytes from block size
           dec     rf
           dec     rf
           ghi     rf                  ; and write into block header
           str     rd
           inc     rd
           glo     rf
           str     rd
           pop     rd                  ; recover address
           lbr     alloc_2             ; finish allocating
alloc_nxt: glo     rf                  ; add block size to address
           str     r2
           glo     rd
           add
           plo     rd
           ghi     rf
           str     r2
           ghi     rd
           adc
           phi     rd
           lbr     alloc_1             ; check next cell
alloc_new: lda     r9                  ; retrieve start of heap
           phi     rd
           ldn     r9
           plo     rd
           glo     rc                  ; subtract req. size from pointer
           str     r2
           glo     rd
           sm
           plo     rd
           ghi     rc
           str     r2
           ghi     rd
           smb
           phi     rd
           dec     rd                  ; point to lsb of block size
           glo     rc                  ; write size
           str     rd
           dec     rd
           ghi     rc
           str     rd
           dec     rd
           ldi     2                   ; mark as allocated block
           str     rd
           mov     rf,rd               ; set address
           inc     rf                  ; point to actual data space
           inc     rf
           inc     rf
           glo     rd                  ; write new heap address
           str     r9
           dec     r9
           ghi     rd
           str     r9
           sep     scall               ; check for out of memory
           dw      checkeom
           sep     sret                ; return to caller

; **************************************
; ***** Deallocate memory          *****
; ***** RF - address to deallocate *****
; **************************************
dealloc:   dec     rf                  ; move to flags byte
           dec     rf
           dec     rf
           ldi     1                   ; mark block as free
           str     rf
heapgc:    push    rc                  ; save consumed registers
           push    rd
           ldi     low heap            ; need start of heap
           plo     r9
           lda     r9                  ; retrieve heap start address
           phi     rd
           lda     r9
           plo     rd   
heapgc_1:  lda     rd                  ; retrieve flags byte
           lbz     heapgc_dn           ; return if end of heap found
           plo     re                  ; save copy of flags
           lda     rd                  ; retrieve block size
           phi     rc
           lda     rd
           plo     rc
           glo     rd                  ; RF=RD+RC, point to next block
           str     r2
           glo     rc
           add
           plo     rf
           ghi     rd
           str     r2
           ghi     rc
           adc
           phi     rf
           lda     rf                  ; retrieve flags for next block
           lbz     heapgc_dn           ; return if on last block
           smi     2                   ; is block allocated?
           lbz     heapgc_a            ; jump if so
           glo     re                  ; check flags of current block
           smi     2                   ; is it allocated
           lbz     heapgc_a            ; jump if so
           lda     rf                  ; retrieve next block size into RF
           plo     re
           lda     rf
           plo     rf
           glo     re
           phi     rf
           inc     rf                  ; add 3 bytes for header
           inc     rf
           inc     rf
           glo     rf                  ; RC += RF, combine sizes
           str     r2
           glo     rc
           add
           plo     rc
           ghi     rf
           str     r2
           ghi     rc
           adc
           phi     rc
           dec     rd                  ; write size of combined blocks
           glo     rc
           str     rd
           dec     rd
           ghi     rc
           str     rd
           dec     rd                  ; move back to flags byte
           lbr     heapgc_1            ; keep checking for merges
heapgc_a:  mov     rd,rf               ; move pointer to next block
           dec     rd                  ; move back to flags byte
           lbr     heapgc_1            ; and check next block
heapgc_dn: pop     rd                  ; recover consumed registers
           pop     rc
           sep     sret                ; return to caller

; *********************************************
; ***** Return amount of free heap memory *****
; ***** Returns: RC - free heap memory    *****
; *********************************************
hfree:     push    rf                  ; save consumed registers
           push    rd
           mov     rc,0                ; clear count
           ldi     low heap            ; setup heap pointer
           plo     r9
           lda     r9                  ; retrieve start of heap
           phi     rf
           ldn     r9
           plo     rf
hfree_lp:  lda     rf                  ; get heap allocation status byte
           lbz     hfree_dn            ; jump if end of heap
           plo     re                  ; save this for a moment
           lda     rf                  ; retrieve block size
           phi     rd
           lda     rf
           plo     rd
           str     r2                  ; add size to block address
           glo     rf
           add
           plo     rf
           ghi     rd
           str     r2
           ghi     rf
           adc
           phi     rf
           glo     re                  ; recover status byte
           smi     1                   ; is it a free block
           lbnz    hfree_lp            ; jump if not
           glo     rd                  ; add block size to count
           str     r2
           glo     rc
           add
           plo     rc
           ghi     rd
           str     r2
           ghi     rc
           adc
           phi     rc
           lbr     hfree_lp            ; check next block
hfree_dn:  pop     rd                  ; recover consumed registers
           pop     rf
           sep     sret                ; and return

; *************************************************************************
; *****                   End of  Heap Manager                        *****
; *************************************************************************


; *************************************************************************
; *****                     Utility functions                         *****
; *************************************************************************

; *************************************
; ***** Get string length         *****
; ***** RF - pointer to string    *****
; ***** Returns: RC - string size *****
; *************************************
strlen:    push    rf           ; save string address
           ldi     0            ; clear counter
           plo     rc
           phi     rc
strlen_1:  lda     rf           ; get next byte
           lbz     strlen_2     ; jump if terminator found
           inc     rc           ; increment size
           lbr     strlen_1     ; loop until terminator found
strlen_2:  pop     rf           ; recover string address
           sep     sret         ; and return to caller

; **************************************************
; ***** Copy string                            *****
; ***** RF - First string                      *****
; ***** RD - Second string                     *****
; **************************************************
strcpy:    push    rd           ; save addresses
           push    rf
strcpy_1:  lda     rf           ; read source byte
           str     rd           ; write into destination
           inc     rd
           lbnz    strcpy_1     ; copy until terminator copied
           pop     rf           ; recover addresses
           pop     rd
           sep     sret         ; and return to caller


; **************************************************
; ***** Compare strings                        *****
; ***** RF - First string                      *****
; ***** RD - Second string                     *****
; ***** Returns: DF=1 - Strins equal           *****
; *****           D=0 - Strings equal          *****
; *****           D=1 - First string greater   *****
; *****           D=FF - Second strint greater *****
; **************************************************
strcmp2:   push    rd           ; save addresses
           push    rf
strcmp_1:  lda     rf           ; get byte from source string
           lbz     strcmp_e     ; jump if end of string
           str     r2           ; store for comprare
           lda     rd           ; get byte from string 2
           lbz     strcmp_s1    ; jump if string2 ended
           sm                   ; otherwise compare bytes
           lbz     strcmp_1     ; check more characters if same
           lbdf    strcmp_s2    ; jump if first string was less
strcmp_s1: ldi     0            ; mark strings unequal
           shr
           ldi     1            ; signal string 1 is greater
           lbr     strcmp_ex    ; and return
strcmp_e:  lda     rd           ; get byte from second string
           lbz     strcmp_eq    ; jump if strings are equal
strcmp_s2: ldi     0            ; mark strings as unequal
           shr
           ldi     0ffh         ; string 2 is greater
strcmp_ex: plo     re           ; save copy of return value
           pop     rf           ; recover addresses
           pop     rd
           glo     re           ; recover result
           sep     sret         ; and return to caller
strcmp_eq: ldi     1            ; mark strings as equal
           shr
           ldi     0            ; D shows equal strings
           lbr     strcmp_ex    ; and return

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
; ***** RB - where to put it                    *****
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
           str     rb
           inc     rb
;           sep     scall        ; display it
;           dw      o_type
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
           ldi     0            ; terminate string
           str     rb
           sep     sret         ; return to caller
itoaz:     ghi     r8           ; see if leading have been used up
           lbz     itoa2        ; jump if so
           smi     1            ; decrement count
           phi     r8
           lbr     itoa3        ; and loop for next character
itoan:     ldi     '-'          ; show negative
           str     rb
           inc     rb
;           sep     scall        ; display it
;           dw      o_type
           glo     rf           ; 2s compliment
           xri     0ffh
           adi     1
           plo     rf
           ghi     rf
           xri     0ffh
           adci    0
           phi     rf
           lbr     itoa1        ; now convert/show number

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
           ldn     r2                  ; recover character
           smi     ','                 ; comma also terminates
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

; ******************************************
; ***** Search for substring           *****
; ***** RD - String to search          *****
; ***** RF - Substring                 *****
; ***** Returns: DF=1 - String found   *****
; *****          RC   - Position count *****
; *****          RD   - After match    *****
; ******************************************
strstr:    ldi     0                   ; set position count
           plo     rc
           phi     rc
strstr_1:  ldn     rd                  ; get byte from string to search
           lbz     strstr_no           ; jump if at end
           push    rf                  ; save positions
           push    rd
strstr_1a: lda     rf                  ; get byte from substring
           lbz     strstr_1b           ; jump if end of substring
           plo     re                  ; keep a copy
           smi     ','                 ; comma also terminates substring
           lbz     strstr_1b
           glo     re                  ; recover character
           str     r2                  ; store for comparison
           lda     rd                  ; get byte from string
           sm                          ; do they match
           lbz     strstr_1a           ; loop back if so
           pop     rd                  ; recover positions
           pop     rf
           inc     rd                  ; move to next character
           inc     rc                  ; increment start counter
           lbr     strstr_1            ; and keep looking for a match
strstr_1b: irx                         ; clear entries from stack
           irx
           irx
           irx
           ldi     1                   ; signal found
           shr
           sep     sret                ; and return to caller
strstr_no: ldi     0                   ; signal not found
           shr
           sep     sret                ; and return

; ***********************************
; ***** Move R8 past any spaces *****
; ***********************************
trim:      lda     r8                  ; get byte from R8
           smi     9                   ; check for tab
           lbz     trim                ; skip tabs
           smi     23                  ; check for space
           lbz     trim                ; keep moving past spaces
           dec     r8                  ; move back to non-space
           sep     sret                ; and return to caller

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

; **************************************
; ***** Check D for 0-9            *****
; ***** Returns: DF=1 - is number  *****
; *****          DF=0 - not number *****
; **************************************
is_number: plo     re                  ; save original value
           smi     '0'                 ; check for below numerals
           lbnf    not_chr             ; jump if not in range
           smi     10                  ; check high of range
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
           smi     27                  ; check high of range
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
           smi     27                  ; check high of range
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

; ***********************************
; ***** Check for out of memory *****
; ***********************************
checkeom:  push    rc                  ; save consumed register
           ldi     low varend          ; get end of variable table
           plo     r9
           lda     r9                  ; retrieve variable table end
           phi     rc
           lda     r9
           plo     rc
           ldi     low heap            ; point to heap start
           plo     r9
           inc     r9                  ; point to lsb
           ldn     r9                  ; get heap
           str     r2
           glo     rc                  ; subtract from variable table end
           sm
           dec     r9                  ; point to msb
           ldn     r9                  ; retrieve it
           str     r2
           ghi     rc                  ; subtract from variable table end
           smb
           lbdf    oom                 ; jump of out of memory
           pop     rc                  ; recover consumed reigster
           sep     sret                ; and return to caller
oom:       sep     scall               ; display out of memory error
           dw      o_inmsg
           db      'Out of memory: ',0
           lbr     errline             ; show line of error and exit
                   
; *****************************************
; ***** Check line for character      *****
; ***** R8 - text to serach           *****
; *****  D - character to serach for  *****
; ***** Returns: DF=1 character found *****
; *****          DF=0 not found       *****
; *****************************************
haschr:    plo     re                  ; save character
           push    r8                  ; save position
           glo     re                  ; recover character
           str     r2                  ; store for comparisons
haschr_1:  lda     r8                  ; get next byte ffomr line
           lbz     haschr_n            ; jump if end reached
           sm                          ; compare character to search char
           lbnz    haschr_1            ; if not, keep looking
           ldi     1                   ; signal found
haschr_2:  shr
           pop     r8                  ; recover address
           sep     sret                ; and return
haschr_n:  ldi     0                   ; signal not found
           lbr     haschr_2            ; and return

; ****************************************
; ***** Convert ASCII to integer     *****
; ***** R8 - Pointer to ASCII number *****
; ***** Returns: RF - 16-bit integer *****
; ****************************************
atoi:      sep     scall               ; move past any spaces
           dw      trim
           ldi     0                   ; clear total
           plo     rf
           phi     rf
atoi_0_1:  lda     r8                  ; get next character
           sep     scall               ; is it a number
           dw      is_number
           lbnf    atoi_0_2            ; jump if not
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
           lbr     atoi_0_1            ; loop back for more numerals
atoi_0_2:  dec     r8                  ; move back to non-numeral character
           sep     sret                ; and return to caller

; ********************************************
; ***** Search token table               *****
; ***** R8 - Input stream                *****
; ***** RF - Pointer to token table      *****
; ***** Returns: DF=1 - token found      *****
; *****             D - Token value      *****
; *****            R8 - char after token *****
; *****          DF=0 - Not found        *****
; *****            R8 - original value   *****
; ********************************************
tkn_1:     ldn     rf                  ; get byte from table
           lbz     tkn_no              ; jump if no matches found
           push    r8                  ; save buffer position
tkn_2:     lda     rf                  ; get byte from ops table
           shl                         ; shift high bit into DF
           lbdf    tkn_3               ; jump if last byte of token
           shr                         ; shift it back
           stxd                        ; store for compare
           lda     r8                  ; get byte from input
           sep     scall               ; check for uppercase
           dw      is_uc
           lbnf    tkn_2a              ; jump if not
           adi     32                  ; convert to lowercase
tkn_2a:    irx
           sm                          ; and compare
           lbz     tkn_2               ; jump if match so far
tkn_2_1:   lda     rf                  ; get byte from table
           shl                         ; need to check high bit
           lbnf    tkn_2_1             ; loop until last byte found
           inc     rf                  ; then move past value field
           pop     r8                  ; recover input position
           lbr     tkn_1               ; and check next opeartor
tkn_3:     shr                         ; shift value back to the right
           stxd                        ; store for comparison
           lda     r8                  ; get byte from input
           sep     scall               ; check for uppercase
           dw      is_uc
           lbnf    tkn_3a              ; jump if not
           adi     32                  ; convert to lowercase
tkn_3a:    irx
           sm                          ; and compare
           lbz     tkn_yes             ; jump if good
           pop     r8                  ; recover input address
           inc     rf                  ; move past token value
           lbr     tkn_1               ; and check next token
tkn_yes:   irx                         ; remove address from stack
           irx
           ldi     1                   ; signal operator found
           shr
           lda     rf                  ; recover operator value into D
           sep     sret                ; and return to caller
tkn_no:    ldi     0                   ; signal not an operator
           shr
           sep     sret                ; and return

aliases:   db      'accep',('t'+080h),'a'
           db      'allo',('c'+080h),'b'
           db      'comput',('e'+080h),'c'
           db      'deallo',('c'+080h),'b'
           db      'le',('t'+080h),'c'
           db      'en',('d'+080h),'e'
           db      'retur',('n'+080h),'e'
           db      'flag',('s'+080h),'f'
           db      'inpor',('t'+080h),'i'
           db      'jum',('p'+080h),'j'
           db      'asci',('i'+080h),'k'
           db      'matc',('h'+080h),'m'
           db      'n',('o'+080h),'n'
           db      'outpor',('t'+080h),'o'
           db      'qui',('t'+080h),'q'
           db      'remar',('k'+080h),'r'
           db      'pee',('k'+080h),'s'
           db      'pok',('e'+080h),'s'
           db      'typ',('e'+080h),'t'
           db      'cal',('l'+080h),'u'
           db      'varpt',('r'+080h),'v'
           db      'ye',('s'+080h),'y'
           db      'us',('r'+080h),'@'
           db      ('a'+080h),'a'
           db      ('b'+080h),'b'
           db      ('c'+080h),'c'
           db      ('e'+080h),'e'
           db      ('f'+080h),'f'
           db      ('i'+080h),'i'
           db      ('j'+080h),'j'
           db      ('k'+080h),'k'
           db      ('m'+080h),'m'
           db      ('n'+080h),'n'
           db      ('o'+080h),'o'
           db      ('q'+080h),'q'
           db      ('r'+080h),'r'
           db      ('s'+080h),'s'
           db      ('t'+080h),'t'
           db      ('u'+080h),'u'
           db      ('v'+080h),'v'
           db      ('y'+080h),'y'
           db      ('@'+080h),'@'
           db      0

errmsg:    db      'File not found',10,13,0
left:      db      'LEFT',0
right:     db      'RIGHT',0
match:     db      'MATCH',0
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
accept:    ds      256
tokens:    ds      60*3
           ds      64
stack:     ds      1
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

