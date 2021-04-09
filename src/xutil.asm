title 'XUTIL.ASM'
;************************************************
;*                                              *
;*              XUTIL.ASM                       *
;*                                              *
;*  General purpose utility routines for X.25   *
;*  protocol interface program                  *
;*                                              *
;*  rev 0.16    08/21/84        E. Elizondo     *
;*                                              *
;*  (c) 1984 E. Elizondo - all rights reserved. *
;*                                              *
;*    This program may be used freely for non-  *
;*  commercial applications. It may not be sold *
;*  or used for commercial applications without *
;*  written permission of the author.           *
;*                                              *
;************************************************
;       
        
        maclib  Z80             ;DR Z80 macro library


;       subroutine entry points

        public  ilprt           ;in-line print routine
        public  ctype           ;output char to console and printer
        public  cview           ;output char to console only
        public  move            ;move block of data
        public  decbin          ;convert decimal ASCII to binary
        public  hexbin          ;convert hex ASCII digit to binary
        public  pdec            ;print binary number in ASCII decimal
        public  pbin            ;print byte in binary
        public  phex            ;print byte in hex
        public  phex1           ;print nibble in hex
        public  disptch         ;dispatch routine
        public  instr           ;input string from console
        public  poll            ;poll console & printer
        public  delay           ;wait a bit

;       address hooks
        public  inbuf           ;console input buffer
        public  prnflg          ;printer ena/dis flag

;       external subroutines

        extrn   putbuf          ;put byte into a fifo buffer
        extrn   getbuf          ;get byte from a fifo buffer

;       external addresses

        extrn   cibcb           ;A(console input buffer)
        extrn   cobcb           ;A(console output buffer)
        extrn   pobcb           ;A(printer buffer)


;       standard ASCII equates

bell    equ     07h
tab     equ     09h
cr      equ     0dh
lf      equ     0ah
esc     equ     1bh

;       CP/M BDOS function equates

conotf  equ     2               ;console output function
listf   equ     5               ;list output function
diriof  equ     6               ;direct console I/O
frcbuf  equ     10              ;read edited input line
        
;       CP/M address equates:

fcb     equ     005ch           ;default file control block
bdos    equ     0005h           ;BDOS entry address

        cseg                    ;code section

;       in-line print routine
;       on entry:       text to be printed follows call
;                       to this routine, and is
;                       terminated by a 0
;       on exit:        <a>, flags clobbered
;                       all other registers unchanged
;                       routine returns to instruction 
;                       immediately following message
;

ilprt:  xthl                    ;point to first byte of msg
ilplp:  mov     a,m             ;get byte
        ora     a               ;terminator?
        jz      ilpret          ;yes, exit
;
        call    ctype           ;else output character
        inx     h               ;bump pointer
        jmp     ilplp           ;and go back for more
;
ilpret: inx     h               ;bump past terminator
        xthl                    ;put next address on stack
        ret                     ;and go there



;       output character in <a> to console buffer
;       and to printer buffer if printer enabled
;       on entry:       <a>= character
;       on exit:        all flags, regs unchanged
;
ctype:  push    psw             ;save regs
        push    h               ;       /
        push    d               ;      /
        push    b               ;     /
        mov     b,a             ;save char in <b>
        lxi     h,cobcb         ;point to console output buffer
        call    putbuf          ;and write it there
        lda     prnflg          ;get print flag
        ora     a               ;printer enabled?
        jz      ctyexi          ;no, exit
;
        mov     a,b             ;get back byte
        lxi     h,pobcb         ;point to printer output buffer
        call    putbuf          ;write char there
;
;       common exit
ctyexi: call    copol           ;poll crt output
        pop     b               ;restore regs
        pop     d               ;       /  
        pop     h               ;      /
        pop     psw             ;     /
        ret



;       output character in <a> to console buffer (only)
;       (used for clearing crt screen, end of line, etc)
;       on entry:       <a>= character
;       on exit:        all flags, regs unchanged
;
cview:  push    psw             ;save regs
        push    h               ;       /
        push    d               ;      /
        push    b               ;     /
        lxi     h,cobcb         ;point to console output buffer
        call    putbuf          ;and write it there
        call    copol           ;poll crt output
        pop     b               ;restore regs
        pop     d               ;       /  
        pop     h               ;      /
        pop     psw             ;     /
        ret



;       general purpose move routine
;       (internally and externally called)
;       on entry:       <hl>=source address
;                       <de>=destination address
;                       <bc>=number of bytes to move
;       on exit:        <hl>=last source address+1
;                       <de>=last dest address+1
;                       <bc>=0

move:   
        ldir
        ret



;       dispatch routine
;       on entry:       <a>=offset into table (0-127)
;                       table of addresses follows calling routine
;       on exit:        <a>,flags clobbered
;                       all other regs unchanged
;
disptch:
        rlc             ;double the offset bits
        xthl            ;save hl, get adress of table
        push    d       ;save de
        mov     e,a     ;put doubled code in e
        mvi     d,0     ;and clear d
        dad     d       ;offset into table
        mov     a,m     ;get low byte of destination
        inx     h       ;and now high byte
        mov     h,m     ;high byte into h
        mov     l,a     ;and low byte into l
        pop     d       ;restore de
        xthl            ;put dest address on stack, restore hl
        ret             ;dispatch to destination




;       get edited console string
;       on entry:       no parameters
;       on exit:        all regs unchanged
;                       console string is in inbuf

instr:
        push    b
        push    d
        push    h
        mvi     c,frcbuf
        lxi     d,inbuf
        call    bdos
        pop     h
        pop     d
        pop     b
        ret



;       convert ASCII decimal number in input buffer to binary
;       on entry:       no parameters
;       on exit:        <de>=binary number
;                       carry set if conversion error
;
decbin: push    h       ;save regs
        push    b       ;       /
        lxi     d,0     ;clear binary number
        lxi     h,inbuf+1       ;<hl>=A(# of input characters)
        mov     b,m     ;<b>,<a>=# of input characters
        mov     a,b     ;       /
        ora     a       ;no input?
        jz      addexi  ;yes, return with <de>=0
;
dbloop: inx     h       ;get a digit
        mov     a,m     ;       /
        sui     '0'     ;convert to binary
        jc      addexi  ;return with carry if <0
;
        cpi     10      ;or if >9
        cmc             ;       /
        jc      addexi  ;      /
;
        ora     a       ;=0?
        jz      nxtdig  ;yes, get next digit
;
adddig: inx     d       ;else add digit in <a> to <de>
        dcr     a       ;       /
        jnz     adddig  ;      /
;
nxtdig: dcr     b       ;last digit?
        jz      addexi  ;yes, normal exit
;
        push    h       ;else save <hl>
        lxi     h,0     ;clear <hl>
        mvi     c,10    ;and multiply <de> by 10
mult10: dad     d       ;       /
        dcr     c       ;      /
        jnz     mult10  ;     /
;
        xchg            ;restore number to <de>
        pop     h       ;restore <hl>
        jmp     dbloop  ;and get next digit
;
addexi: pop     b       ;restore regs
        pop     h       ;       /
        ret


;       convert ASCII hex digit to binary
;       (externally called)
;       on entry:       <a>= hex digit
;       on exit:        <a>= binary equivalent
;                       carry set if conversion error
;                       other flags clobbered
;                       all other regs unchanged
hexbin:
        sui     '0'     ;subtract ASCII bias
        rc              ;error if <0
;
        cpi     10      ;is it <10?
        cmc             ;yes, return with value
        rnc             ;       /
;
        ani     0101$1111b      ;convert to upper case
        cpi     'G'-'0' ;is it >F?
        cmc             ;yes, return with carry
        rc              ;       /
;
        sui     'A'-'9'-1       ;else adjust A-F
        cpi     10      ;set carry if not >10
        ret                     



;       print binary number in ASCII
;       on entry:       <hl>=binary number
;       on exit:        <a>,flags clobbered
;                       all other regs unchanged

pdec:   
        push    h               ;save regs
        push    d               ;       /
        push    b               ;      /
        lxi     d,numbuf        ;point to number buffer
        call    bindec          ;convert # to ASCII
        lxi     h,numbuf        ;point to number buffer
        mvi     b,4             ;max # of leading zeros
        mvi     a,'0'           ;blank leading zeros
blnklp: cmp     m               ;is char a zero?
        jnz     pdec1           ;no, all finished
;
        mvi     m,' '           ;yes, blank it
        inx     h               ;bump pointer
        dcr     b               ;last leading zero?
        jnz     blnklp          ;no, keep going
;
pdec1:  call    ilprt           ;print it
numbuf: db      '00000',0
        pop     b               ;restore regs
        pop     d               ;       /
        pop     h               ;      /
        ret




;       convert binary number 0-65535 to decimal ASCII
;       (internally called)
;       on entry:       <hl>=binary number
;                       <de>=address of buffer to put ASCII digits
;       on exit:        buffer contains ASCII number 
;                       terminated by binary 0
;                       regs, flags clobbered

bindec: lxi     b,-10000        ;digit value
        call    cdigit          ;convert first digit
        lxi     b,-1000         ;convert next digit
        call    cdigit          ;       /
        lxi     b,-100          ;convert next digit
        call    cdigit          ;       /
        lxi     b,-10           ;convert next digit
        call    cdigit          ;       /
        lxi     b,-1            ;convert last digit
        call    cdigit          ;       /
        mvi     a,0             ;put terminator in buffer
        stax    d               ;       /
        ret
        

;       convert a digit
cdigit: mvi     a,'0'-1         ;initialize ASCII value
        push    d               ;save buffer pointer
cdloop: mov     e,l             ;save last iteration
        mov     d,h             ;       /
        inr     a               ;increment ASCII digit
        dad     b               ;subtract value
        jc      cdloop          ;repeat till underflow
;
        mov     l,e             ;get previous iteration
        mov     h,d             ;       /
        pop     d               ;restore buffer pointer
        stax    d               ;store byte in buffer
        inx     d               ;bump pointer
        ret     




;       print byte in binary '1''s and '0''s
;       (clever routine adapted from TDL)
;       on entry:       <a>=byte
;       on exit:        <a>,flags clobbered
;                       all other regs unchanged
pbin:
        push    b               ;save <bc>
        mvi     b,8             ;# of bits to output
pbit:   ral                     ;move msb to carry
        push    psw             ;sabe byte
        mvi     a,'0'/2         ;make '0' or '1'
        adc     a               ;       /
        call    ctype           ;output char
        pop     psw             ;restore byte
        dcr     b               ;last bit?
        jnz     pbit            ;no, keep going
;
        pop     b               ;else restore <bc>
        ret


;       print byte in ASCII hex format (phex)
;       print nibble in ASCII hex format (phex1)
;       (externally and internally called)
;       on entry:       <a>=byte
;       on exit:        <a>,flags clobbered
;                       all other regs unchanged

phex:   push    psw             ;save byte
        rrc                     ;move upper nibble down
        rrc                     ;       /
        rrc                     ;      /
        rrc                     ;     /
        call    phex1           ;output it
        pop     psw             ;restore byte
phex1:  ani     0fh             ;strip lower nibble
        adi     90h             ;convert to ASCII character
        daa                     ;       /
        aci     40h             ;      /
        daa                     ;     /
        call    ctype           ;output hex digit
        ret



;       poll console and printer
;       (externally and internally called)
;       on entry:       no parameters
;       on exit:        all registers unchanged

poll:   push    psw             ;save all registers & flags
        push    b               ;       /
        push    d               ;      /
        push    h               ;     /
        call    cipol           ;poll console input
        call    copol           ;poll console output
        call    popol           ;poll printer output
        pop     h               ;restore registers & flags
        pop     d               ;       /
        pop     b               ;      /
        pop     psw             ;     /
        ret     


;--> poll console input

cipol:  mvi     c,diriof        ;direct I/O function
        mvi     e,0ffh          ;console request
        call    bdos            ;do it
        ora     a               ;console input ready?
        rz                      ;no, return
;
        lxi     h,cibcb         ;else put byte in queue buffer
        call    putbuf          ;       /
        rnc                     ;return if all ok
;
        call    ilprt           ;else display error message
        db      cr,lf,'L4: console input buffer overflow',cr,lf,bell,0
        ret


;--> output byte to console if available

copol:  lxi     h,cobcb         ;get byte from queue buffer
        call    getbuf          ;       /
        rc                      ;return if no byte available
;
        mvi     c,diriof        ;direct I/O function
        mov     e,a             ;else, output byte
        call    bdos
        ret


;--> poll printer output

popol:  lhld    0001h           ;get start of bios vector table
        lxi     d,2dh-3h        ;offset to list status entry point
        dad     d               ;       /
        call    go              ;get list device status
        ora     a               ;ready?
        rz                      ;no, return
;
        lxi     h,pobcb         ;get byte from queue buffer
        call    getbuf          ;       /
        rc                      ;return if no byte available
;
        mov     e,a             ;else, output byte
        mvi     c,listf         ;list output function
        call    bdos            ;and do it
        ret

go:     pchl                    ;dispatch to <hl>



;       wait a little bit
;       on entry:       no parameters
;       on exit:        all regs, flags unchanged

delay:  push    psw             ;save regs
        push    h               ;       /
        push    d               ;      /
        push    b               ;     /
        lxi     d,0             ;<de>= outer loop
        lxi     h,0             ;<hl>= inner loop
dloop:  dcx     d               ;bump <de>
dloop1: dcx     h               ;bump <hl>
        mov     a,h             ;all counted out?
        cmp     l               ;       /
        jnz     dloop1          ;no, keep counting
;
        mov     a,d             ;outer loop counted out?
        cmp     e               ;       /
        jnz     dloop           ;no, keep counting
;
        pop     b               ;restore regs
        pop     d               ;       /
        pop     h               ;      /
        pop     psw             ;     /
        ret




;       *****************
;       *  data area    *
;       *****************


        dseg                    ;data segment

prnflg  db      0               ;print on/off flag
inbuf:  db      128,0,0         ;initialized console input buffer
        ds      127             ;console input buffer area

