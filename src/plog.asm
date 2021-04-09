title 'PLOG.ASM'
;************************************************
;*                                              *
;*              PLOG.ASM                        *
;*                                              *
;*  program to print session history log        *
;*  from X.25 protocol interface program        *
;*                                              *
;*  rev 0.5     07/18/83        E. Elizondo     *
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
        maclib  seqio           ;DR sequential I/O library
        maclib  Z80             ;DR Z80 library

;       local parameters
bsize:  equ     16              ;block size in log file


;       X.25 standard parameters:

;       frame addresses (X.25 para 2.4.1)
;       bit #   8765$4321
addra   equ     0000$0011b      ;address A
addrb   equ     0000$0001b      ;address B

;       frame control (id) bytes (table 3/X.25)
;       bit #   8765$4321
ifid    equ     0000$0000b      ;I
rrfid   equ     0000$0001b      ;RR
rnrfid  equ     0000$0101b      ;RNR
rejfid  equ     0000$1001b      ;REJ
sarmfid equ     0000$1111b      ;SARM (not used in LAPB)
dmfid   equ     0000$1111b      ;DM
sabmfid equ     0010$1111b      ;SABM
discfid equ     0100$0011b      ;DISC
uafid   equ     0110$0011b      ;UA
cmdrfid equ     1000$0111b      ;CMDR
frmrfid equ     1000$0111b      ;FRMR
badfid  equ     1111$1111b      ;bad frame for testing

;       misc constants
cr      equ     0dh             ;carriage ret
lf      equ     0ah             ;line feed
ff      equ     0ch             ;form feed
tab     equ     09h             ;horizontal tab

;       CP/M equates
conotf  equ     2               ;console output function
bdos    equ     0005h           ;BDOs entry point
wboot   equ     0000h           ;warm boot address

        cseg                    ;code section

        lxi     sp,stack        ;set up local stack


;       open file for access
        file    infile,logfil,,X25,LOG,1024,logbuf
;
        call    ilprt
        db      ff,tab,tab,tab,'X.25 session log',cr,lf
        db      cr,lf,'tx/rx',tab,'addr',tab,'p/f',tab,'frame'
        db      tab,'N(r)',tab,'N(s)',0

mloop:  mvi     c,bsize         ;<c>=# bytes in block
        call    ilprt
        db      cr,lf,0
;
bloop:  call    getdat          ;read first byte
        cpi     1ah             ;end of file?
        jz      exit            ;yes, exit
        dcr     c               ;decrement byte count
        mvi     h,0             ;move contents to <hl>
        mov     l,a             ;       /
        call    pdec            ;print it
        call    ilprt           ;tab to next column
        db      tab,0           ;       /  
        call    getdat          ;read address byte
        dcr     c               ;decrement byte count
        cpi     addra           ;address A?
        jnz     addr1           ;no, keep going
        call    ilprt           ;else print it
        db      '  A',0         ;       /
        jmp     ctrl1           ;and go for next byte
addr1:  cpi     addrb           ;address B?
        jnz     addr2           ;no, keep going
        call    ilprt           ;else print it
        db      '  B',0 ;       /
        jmp     ctrl1           ;and go for next byte
addr2:  mov     l,a             ;else print address in decimal
        mvi     h,0             ;       /
        call    pdec            ;       /
        call    ilprt           ;and a '?'
        db      '?',0           ;       /
;
ctrl1:  call    getdat          ;get next byte
        mov     b,a             ;save byte in <b>
        dcr     c               ;decrement byte count
        ani     0001$0000b      ;extract P/F bit
        rrc                     ;move to bit 0
        rrc                     ;       /
        rrc                     ;      /
        rrc                     ;     /
        mov     l,a             ;print it
        mvi     h,0             ;       /
        call    pdec            ;      /
        call    ilprt           ;and tab over
        db      tab,0           ;       /
        mov     a,b             ;get back control byte
        bit     0,a             ;I frame?
        jz      fi              ;yes, process it
;
        ani     1110$1111b      ;extract all bits except p/f
        cpi     sabmfid         ;SABM frame?
        jz      fsabm           ;yes, process it
;
        cpi     discfid         ;DISC frame?
        jz      fdisc           ;yes, process it
;
        cpi     dmfid           ;DM frame?
        jz      fdm             ;yes, process it
;
        cpi     uafid           ;UA frame?
        jz      fua             ;yes, process it
;
        cpi     cmdrfid         ;CMDR/FRMR frame?
        jz      fcmdr           ;yes, process it

;       branch to numbered frames
        ani     0000$1111b      ;discard N(r) sequence bits
        cpi     rrfid           ;RR frame?
        jz      frr             ;yes, process it
;
        cpi     rnrfid          ;RNR frame?
        jz      frnr            ;yes, process it
;
        cpi     rejfid          ;REJ frame?
        jz      frej            ;yes, process it
;
        mov     l,b             ;else id is unrecognized
        call    pbin            ;so print it
        call    ilprt           ;followed by a '?'
        db      '?',0
;
zloop:  call    getdat          ;read rest of block
        jz      exit            ;exit if end of file
        dcr     c               ;until end 
        jnz     zloop           ;       /
        jmp     mloop           ;and then go for another block
;
exit:   call    ilprt           ;end of page
        db      cr,ff,0
        finis   logfil          ;close file
        jmp     wboot           ;and exit
;
;
;       process SABM frame
;
fsabm:  call    ilprt
        db      'SABM',0
        jmp     zloop
;
fdisc:  call    ilprt
        db      'DISC',0
        jmp     zloop
;
fua:    call    ilprt
        db      'UA',0
        jmp     zloop
;
fi:     call    ilprt
        db      'INFO',0
        call    prtnr   
        call    ilprt           ;tab to next column
        db      tab,0
        mov     a,b             ;get back control byte
        ani     0000$1110b      ;extract N(s)
        rrc                     ;move down to bits 0-2
        mov     l,a             ;print it
        mvi     h,0             ;       /
        call    pdec            ;      /
        jmp     zloop
;
frr:    call    ilprt
        db      'RR',0
        call    prtnr
        jmp     zloop
;
frnr:   call    ilprt
        db      'RNR',0
        call    prtnr
        jmp     zloop
;
frej:   call    ilprt
        db      'REJ',0
        call    prtnr
        jmp     zloop
;
fdm:    call    ilprt
        db      'DM',0
        jmp     zloop
;
fcmdr:  call    ilprt
        db      'CMDR',0
        jmp     zloop
;
prtnr:  call    ilprt           ;tab to next column
        db      tab,0
        mov     a,b             ;get control byte
        ani     1110$0000b      ;extract N(r)
        rrc                     ;move to bits 0-2
        rrc                     ;       /
        rrc                     ;      /
        rrc                     ;     /
        rrc                     ;    /
        mov     l,a             ;print it
        mvi     h,0             ;       /
        call    pdec            ;      /
        ret


;       get a byte of data from log disk file
;       on entry:       no parameters
;       on exit:        <a>=character, if available
;                       zero flag set if end of file
;                       all other regs unchanged
getdat: push    h               ;save regs
        push    d               ;       /
        push    b               ;      /
        get     logfil          ;read byte
        pop     b               ;restore regs
        pop     d               ;       /  
        pop     h               ;      /
        ret


;       in-line print routine
;       on entry:       text to be printed follows call
;                       to this routine, and is
;                       terminated by a 0
;       on exit:        <a>, flags clobbered
;                       all other registers unchanged
;                       routine returns to instruction 
;                       immediately following message
;

ilprt:  xthl
ilplp:  mov     a,m
        ora     a
        jz      ilpret
        call    ctype
        inx     h
        jmp     ilplp
;
ilpret: xthl
        ret


;       output character in <a> to console buffer
;       on entry:       <a>= character
;       on exit:        all flags, regs unchanged
;
ctype:  push    b
        push    d
        push    h
        mvi     c,conotf
        mov     e,a
        call    bdos
        pop     h
        pop     d
        pop     b
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
        mvi     m,' '           ;yes, blank it
        inx     h               ;bump pointer
        dcr     b               ;last leading zero?
        jnz     blnklp          ;no, keep going
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
        pop     b               ;else restore <bc>
        ret


;       convert byte to ASCII hex format
;       (externally and internally called)
;       on entry:       <a>=byte
;                       <hl>=address of ASCII output buffer
;       on exit:        <a>,flags clobbered
;                       <hl>=<hl>+2
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
        mov     m,a             ;output character to buffer
        inx     h               ;and bump pointer
        ret


;       ***************
;       *  data area  *
;       ***************

        dseg

        ds      80h             ;local stack area
stack:  equ     $
logbuf  ds      1024            ;file buffer area

