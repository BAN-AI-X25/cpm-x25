title 'X25.ASM'
;************************************************
;*                                              *
;*              X25.ASM                         *
;*                                              *
;*  X.25 protocol packet interface main program *
;*                                              *
;*  rev 0.46    05/04/85        E. Elizondo     *
;*                                              *
;*  (c) 1985 E. Elizondo - all rights reserved. *
;*                                              *
;*    This program may be used freely for non-  *
;*  commercial applications. It may not be sold *
;*  or used for commercial applications without *
;*  written permission of the author.           *
;*                                              *
;************************************************
;       
revmon  equ     05              ;program revision month
revday  equ     04              ;program revision day
revyr   equ     85              ;program revision year
        
        maclib  Z80             ;DR Z80 macro library

;       miscellaneous equates
false   equ     0
true    equ     not false
debug   equ     true

;       standard ASCII equates
bell    equ     07h
tab     equ     09h
cr      equ     0dh
lf      equ     0ah
ff      equ     0ch
esc     equ     1bh
bs      equ     08h
del     equ     7fh


;       design parameters

;       system commands
mencmd: equ     esc             ;command for main menu
delcmd: equ     'U'-40h         ;command to delete typed line
lstcmd: equ     'P'-40h         ;command to toggle printer flag
brkcmd: equ     'B'-40h         ;command to send interrupt packet

;       Big Board parameters
pfm:    equ     0f003h          ;PFM-80 warm start entry
clscrn: equ     1ah             ;clear screen character
clrlin: equ     18h             ;clear to end of line

;       CP/M address equates:
wboot:  equ     0000h           ;warm boot address



;       hooks for other programs
        public  l4stat          ;level 4 status flags

;       definition of l4stat status bits
;       bit     set condition
;       0       prompt console for packet input
;       1       unasigned
;       2       unasigned
;       3       unasigned
;       4       unasigned
;       5       unasigned
;       6       unasigned
;       7       unasigned



;       external subroutines

;       from level1 module:
        extrn   inisio          ;initialize level 1 hardware
        extrn   txabo           ;transmit level 1 abort
        extrn   poll            ;poll & service hardware
        extrn   sbaud           ;set baud rate
        extrn   t1off           ;turn off T1 timer

;       from level2 module:
        extrn   initl2          ;initialize level 2 parameters
        extrn   conlk           ;connect level 2 link
        extrn   disclk          ;disconnect level 2 link
        extrn   qlksta          ;query level 2 link status
        extrn   txifrm          ;transmit level 2 I frame
        extrn   rxfrm           ;process received level 2 frame
        extrn   txbadf          ;transmit bad frame

;       from level3 module:
        extrn   initl3          ;initialize level 3 parameters
        extrn   txdpk           ;transmit data packet if avail
        extrn   rxpk            ;receive packet if avail
        extrn   txintp          ;transmit interrupt packet
        extrn   txclrr          ;transmit clear request packet
        extrn   txstar          ;transmit restart packet

;       from files module:
        extrn   iparms          ;initialize system parameters
        extrn   wparms          ;write system parameters to disk
        extrn   otxfil          ;open tx file
        extrn   orxfil          ;open rx file
        extrn   olgfil          ;open log file
        extrn   crxfil          ;close rx file
        extrn   ctxfil          ;close tx file
        extrn   clgfil          ;close log file

;       from xutil module:
        extrn   ilprt           ;in-line print routine
        extrn   ctype           ;type character
        extrn   cview           ;display char on console (only)
        extrn   instr           ;input console string
        extrn   decbin          ;convert ASCII decimal to binary
        extrn   hexbin          ;convert ASCII hex to binary
        extrn   pbin            ;print byte in '1' and '0's
        extrn   pdec            ;print binary number in decimal
        extrn   phex1           ;print nibble in hex
        extrn   disptch         ;dispatch to address in table

;       from buffers module:
        extrn   inibuf          ;initialize buffer parameters
        extrn   clrtxb          ;clear all tx buffers
        extrn   putbuf          ;put byte into a fifo buffer
        extrn   getbuf          ;get byte from a fifo buffer
        extrn   dcrbuf          ;decrement buffer count
        extrn   clrbuf          ;clear buffer
        extrn   setrdy          ;set buffer ready flag
        extrn   getrdy          ;get state of buffer ready flag
        extrn   clrrdy          ;clear buffer ready flag
        extrn   getbct          ;get buffer count


        
;       external addresses
        
        extrn   inbuf           ;A(console input buffer)
        extrn   cibcb           ;A(console input buffer bcb)
        extrn   cobcb           ;A(console output buffer bcb)
        extrn   pobcb           ;A(printer output buffer bcb)
        extrn   ctbcb           ;A(console transmit buffer bcb)
        extrn   crbcb           ;A(console receive buffer bcb)
        extrn   rxfree          ;A(list of free rx buffers bcb)
        extrn   rxflst          ;A(list of rx frame buffers bcb)
        extrn   rxplst          ;A(list of rx packet buffers bcb)


;       system status flags

        extrn   rxstat          ;level 1 rx status
        extrn   txstat          ;level 1 tx status
        extrn   tistat          ;timer status
        extrn   lkstat          ;level 2 link status
        extrn   l2stat          ;level 2 status flags
        extrn   dtemod          ;DTE/DCE mode flag
        extrn   l3stat          ;level 3 status flags
        extrn   chstat          ;level 3 logical channel status
        extrn   fstat           ;file status
        extrn   prnflg          ;printer ena/dis flag

;       level 1 parameters
        extrn   baudop          ;default baud rate option

;       level 2 parameters

        extrn   vr              ;V(r)
        extrn   vs              ;V(s)
        extrn   lastnr          ;last received N(r)
        extrn   lastvs          ;last sent V(s)
        extrn   maxvs           ;highest sent V(s)
        extrn   pfbit           ;P/F bit

;       level 3 parameters

        extrn   pr              ;P(r)
        extrn   ps              ;P(s)
        extrn   lastpr          ;last rx P(r)
        extrn   lrxps           ;last rx P(s)
        extrn   ltxps           ;last tx P(s)
        extrn   pvcmod          ;PVC/VC mode flag
        extrn   laddr           ;local DTE address
        extrn   raddr           ;remote DTE address
        extrn   laddrl          ;local DTE address length
        extrn   raddrl          ;remote DTE address length
        extrn   chan            ;logical channel #
        extrn   qbit            ;Q bit
        extrn   dbit            ;D bit
        extrn   group           ;logical group #
        
;       level 1 diagnostic counters

        extrn   rxbfct          ;bad rx frames
        extrn   rxgfct          ;good rx frames
        extrn   rxbbct          ;rx chars lost due to buff not avail
        extrn   rxboct          ;rx chars lost due to buff overrun
        extrn   rxcect          ;rx crc errors
        extrn   rxoect          ;rx overrun errors
        extrn   rxabct          ;rx aborts
        extrn   txefct          ;tx end of frames
        extrn   txuect          ;tx underrun errors
        extrn   txabct          ;tx aborts

;       level 2 diagnostic counters

        extrn   rbafct          ;rx bad address frames
        extrn   rxbrct          ;rx bad response frames
        extrn   rxbcct          ;rx bad command frames
        extrn   rxcfct          ;rx command frames
        extrn   rxrfct          ;rx response frames
        extrn   rxifct          ;rx I frames
        extrn   txifct          ;tx I frames
        extrn   txfct           ;tx frames
        extrn   txirct          ;I frame retransmissions

;       level 3 diagnostic counters

        extrn   txpct           ;total tx packets
        extrn   txfpct          ;total tx file data packets
        extrn   txcpct          ;total tx console data packets
        extrn   ntxbct          ;errors due to no free tx buffer
        extrn   rxpct           ;total rx packets
        extrn   rxdpct          ;total rx data packets
        extrn   rbfpct          ;total rx bad format packets
        extrn   rbcpct          ;total rx bad channel packets
        extrn   rbapct          ;total rx bad address packets
        extrn   rbipct          ;total rx bad id packets
        extrn   rbgpct          ;total rx bad group packets
        extrn   rxxpct          ;total discarded rx packets


;       *************************
;       *  start of program     *
;       *************************


        jmp     begin           ;jump around debug entry point
        jmp     mloop           ;debug entry point
begin:
        di                      ;disable interrupts
        lxi     h,0             ;get & save stack pointer
        dad     sp              ;       /
        shld    oldstk          ;      /
        lxi     sp,stack        ;set up local stack
        ei                      ;enable interrupts
;
        call    clear           ;clear crt screen
        call    ilprt           ;log on
        db      cr,lf
        db      'X.25 packet communications program '
        db      '(','0'+revmon/10,'0'+revmon mod 10,'/'
        db      '0'+revday/10,'0'+revday mod 10,'/'
        db      '0'+revyr/10,'0'+revyr mod 10,')',cr,lf
        db      '(c) 1984 Ed Elizondo - all rights reserved'
        db      cr,lf,0
;
        call    iparms          ;initialize system defaults
        call    inibuf          ;initialize buffer parameters
        call    inisio          ;initialize level 1 hardware
        call    initl2          ;initialize level 2 parameters
        call    initl3          ;initialize level 3 parameters
;
        call    menu2           ;get option
        call    plstat          ;print link status
        


;       *************************
;       * main program loop     *
;       *************************
;       
;       note: form optimum thruput, the following 
;             is recommended:
;             # of calls to rxfrm = 2 x # of calls to txifrm
;             # of calls to txifrm = 2 x # of calls to rxpk
;             # of calls to rxpk = 2 x # of calls to txdpk

mloop:  call    poll            ;service non interrupt hardware
        call    pchar           ;process typed char, if any
        call    rxfrm           ;process received frame
        call    rxpk            ;process received packet
        call    txdpk           ;transmit data packet to level 2
        call    txifrm          ;transmit I frame
        call    rxmsg           ;process received packet message
        call    prompt          ;prompt for console packet data
        jmp     mloop           ;keep doing it forever


;       *************************************************
;       *  service routines for main program loop       *
;       *************************************************


;       get and process typed character, if any
pchar:  xra     a               ;clear <a> and flags
        lxi     h,cibcb         ;console character avail?
        call    getbuf          ;       /
        rc                      ;no, exit
;
        ani     07fh            ;else strip parity
        cpi     mencmd          ;escape?
        jz      menu1           ;yes, go display main menu
;
        cpi     del             ;delete?
        jz      delchar         ;yes, go process it
;
        cpi     delcmd          ;delete line command?
        jz      deline          ;yes, go process it
;
        cpi     brkcmd          ;break command?
        jz      intrpt          ;yes, go process it
;
        cpi     lstcmd          ;printer command?
        jz      ptoggle         ;yes, go process it
;
        cpi     cr              ;carriage return?
        jz      cxfin           ;yes, handle end of message
;
;       else typed character is not special command
        call    ctype           ;display char on console
        call    cxmit           ;transmit char if link connected
        ret


;       delete character previously typed
delchar:
        mvi     a,bs            ;get a backspace
        call    ctype           ;back up cursor
        mvi     a,' '           ;blank last typed char
        call    ctype           ;       /
        mvi     a,bs            ;and back up cursor again
        call    ctype           ;       /
        lxi     h,lkstat        ;link connected?
        bit     2,m             ;       /
        rz                      ;no, do nothing else
;
        lxi     h,ctbcb         ;else point to console tx buffer
        call    getrdy          ;ready flag set?
        rnz                     ;yes, do nothing
;
        call    dcrbuf          ;else delete last char
        ret


;       delete last line typed
deline:
        mvi     a,cr            ;move cursor to beginning of line
        call    cview           ;       /
        mvi     a,clrlin        ;now clear to end of line
        call    cview           ;       /
        lxi     h,lkstat        ;link connected?
        bit     2,m             ;       /
        rz                      ;no, do nothing else
;
        lxi     h,ctbcb         ;point to console tx buffer
        call    getrdy          ;ready flag set?
        rnz                     ;yes, do nothing
;
        call    clrbuf          ;else clear buffer
        ret


;       send interrupt packet
intrpt:
        mvi     b,0             ;<b>=interrupt cause (0 for now)
        call    txintp          ;transmit interrupt packet
        ret     


;       toggle printer on/off
ptoggle:
        lda     prnflg          ;get printer on/off flag
        cma                     ;toggle it
        ora     a               ;printer going off?
        jz      ptog1           ;yes, keep going
;
        mvi     a,bell          ;else ding the console
        call    cview           ;...like CP/M+
        mvi     a,0ffh          ;get printer on flag
;
ptog1:  sta     prnflg          ;and save new flag
        ret


;       transmit character if link connected
;       on entry:       <a>=character
;       on exit:        flags, <a>,<hl> clobbered

cxmit:  lxi     h,lkstat        ;link connected?
        bit     2,m             ;       /
        rz                      ;no, do nothing else
;
        lxi     h,ctbcb         ;else point to console tx buffer
        call    putbuf          ;put in tx buffer
        ret


;       handle end of message if in flow control ready state
cxfin:  call    ctype           ;display cr on console
        call    plf             ;and also a line feed
        lxi     h,lkstat        ;link connected?
        bit     2,m             ;       /
        rz                      ;no, do nothing else
;
        lxi     h,ctbcb         ;else point to console tx buffer
        mvi     a,cr            ;put in cr
        call    putbuf          ;       /
        mvi     a,lf            ;and line feed
        call    putbuf          ;       /
        call    setrdy          ;signal buffer ready
        lxi     h,l3stat        ;point to level 3 status
        setb    7,m             ;and signal message waiting
        ret


;       print a line feed on console
plf:    mvi     a,lf
        call    ctype
        ret


;       process a byte from received packet message to console
rxmsg:
        lxi     h,crbcb         ;point to console rx buffer
        call    getrdy          ;ready?
        rz                      ;no, exit
;
        call    getbuf          ;is anything there?
        rc                      ;no, exit
;
        if      debug           
        mov     b,a             ;save first byte in <b>
;       tag all received packets for debug with '<r>'
        mvi     a,'<'           ;indicate received msg
        call    ctype           ;       /
        mvi     a,'r'           ;      /
        call    ctype           ;     /
        mvi     a,'>'           ;    /
        call    ctype           ;   /
        mov     a,b             ;now restore first byte
        endif
;
;       strip out all non-displayable characters
rxmsg1: cpi     20h             ;displayable ASCII char?
        jnc     rxmsg2          ;yes, keep going
;
        cpi     cr              ;carriage ret?
        jz      rxmsg2          ;yes, keep going
;
        cpi     lf              ;line feed?
        jz      rxmsg2          ;yes, keep going
;
        cpi     ff              ;form feed?
        jz      rxmsg2          ;yes, keep going
;
        cpi     tab             ;horizontal tab?
        jz      rxmsg2          ;yes, keep going
;
        cpi     bell            ;bell?
        jz      rxmsg2          ;yes, keep going
;
        cpi     bs              ;back space?
        jz      rxmsg2          ;yes, keep going
;
        mvi     a,'.'           ;else substitute a period
;
rxmsg2:
        call    ctype           ;send to console
        call    getbuf          ;get next byte
        jnc     rxmsg1          ;and process it if there
        ret                     ;else quit


;       clear crt screen
;       (internally called)
clear:  mvi     a,clscrn
        call    cview
        ret


;       process menu command:
;       print main option menu
menu1:  call    clear           ;clear crt screen
menu2:  call    ilprt
        db      cr,lf,'X.25 main menu:',cr,lf
        db      lf,'normal operation:          '
        db      tab,'diagnostics:',cr,lf
        db      '    1. initialize link        '
        db      tab,tab,'10. display system parameters',cr,lf
        db      '    2. transmit restart packet'
        db      tab,tab,'11. display status flags',cr,lf
        db      '    3. transmit file          '
        db      tab,tab,'12. display L1 and L2 statistics',cr,lf
        db      '    4. abort file transmission'
        db      tab,tab,'13. display L3 statistics',cr,lf
        db      '    5. receive file           '
        db      tab,tab,'14. set flow control ready flags',cr,lf
        db      '    6. abort file reception   '
        db      tab,tab,'15. transmit bad frame',cr,lf
        db      '    7. transmit clear req. packet'
        db      tab,'16. change frame sequence',cr,lf
        db      '    8. query link status      '
        db      tab,tab,'17. start frame logging',cr,lf
        db      '    9. disconnect link        '
        db      tab,tab,'18. stop  frame logging',cr,lf
        db      lf,'miscellaneous:',cr,lf
        db      '   19. change default parameters'
        db      tab,'21. exit to monitor',cr,lf
        db      '   20. change remote DTE address'
        db      tab,'22. exit to CP/M',cr,lf
        db      0
optlp:  call    getopt          ;get option
        rz                      ;exit if nothing there
;
        jc      optlp           ;get another if invalid
;
        cpi     (optend-optbeg)/2       ;in range?
        jnc     optlp           ;no, get another one
;
        call    disptch         ;else dispatch on option in a
optbeg: dw      optlp           ;option 0 address
        dw      conlk           ;option 1
        dw      txstar          ;option 2
        dw      txfile          ;option 3
        dw      txfabo          ;option 4
        dw      rxfile          ;option 5
        dw      crxfil          ;option 6
        dw      txclrr          ;option 7
        dw      qlksta          ;option 8
        dw      disclk          ;option 9
        dw      pparms          ;option 10
        dw      pflags          ;option 11
        dw      pl2stat         ;option 12
        dw      pl3stat         ;option 13
        dw      setlkc          ;option 14
        dw      txbadf          ;option 15
        dw      chgseq          ;option 16
        dw      olgfil          ;option 17
        dw      clgfil          ;option 18
        dw      cparms          ;option 19
        dw      craddr          ;option 20
        dw      pfmexit         ;option 21
        dw      cpmexit         ;option 22
optend: equ     $


;       get selected option
;       on entry:       no parameters
;       on exit:        <a>= option #
;                       carry set if invalid (non numeric)

getopt: call    ilprt
        db      lf,'option (cr to exit)= ',0
        call    instr           ;input reply
        call    plf             ;type line feed
        lxi     h,inbuf         ;get # of characters
        inx     h               ;       /
        mov     a,m             ;      /
        ora     a               ;any there?
        rz                      ;no, return with no action
;
        call    decbin          ;convert input to binary
        rc                      ;return with carry set if invalid
;
        mov     a,d             ;is upper byte=0?
        ora     a               ;       /
        stc                     ;return with carry set if not
        rnz                     ;       /
;
        mov     a,e             ;get lower byte of number
        ora     a               ;set flags
        ret                             


;       prompt for console packet data input
prompt:
        lxi     h,l4stat        ;point to level 4 status
        bit     0,m             ;prompt request?
        rz                      ;no, return
;
        res     0,m             ;else reset request flag
        call    ilprt           ;and show prompt
        db      cr,lf,'>>',0
        ret



;       *************************
;       *  command options      *
;       *************************

        
;       exit program to CP/M
cpmexit:
        call    crxfil          ;close receiv file if open
        call    ctxfil          ;close transmit file if open
        call    clgfil          ;close log file if open
        lhld    oldstk          ;get old stack pointer
        sphl                    ;restore it
        jmp     wboot           ;and warm boot CP/M


;       exit program to Big Board Monitor (PFM)

pfmexit:jmp     pfm


;       transmit file
txfile: call    otxfil          ;open tx file
        call    plstat          ;print link status
        ret

;       abort file transmission
txfabo: lxi     h,fstat         ;point to file status
        bit     1,m             ;tx file open?
        rz                      ;no, do nothing
;
        call    ilprt           ;else tell operator
        db      cr,lf,'L4: file transmission aborted',cr,lf,0
        call    ctxfil          ;close tx file
        ret


;       receive file
rxfile: call    orxfil          ;open rx file
        call    plstat          ;print link status
        ret

;       change frame transmit sequence number
;       (to force a reject packet & retransmission)
chgseq: lda     vs              ;get current V(s)
        dcr     a               ;decrement mod 7
        ani     7               ;       /
        sta     vs              ;and update it
        ret


;       print system parameters

pparms:
        call    clear           ;clear crt screen
        call    ilprt           ;print title
        db      tab,tab,'System Parameters:',cr,lf
        db      cr,lf,lf,tab,'Level 2'
        db      tab,tab,tab,tab,'Level 1'
        db      cr,lf,lf,'mode         =   ',0
        lda     dtemod          ;get DTE/CE mode flag
        cpi     1               ;is it DTE?
        jnz     ppar1           ;no, keep going
;
        call    ilprt           ;yes, print 'DTE'
        db      'DTE      ',0   ;       /
        jmp     ppar2           ;and keep going
;
ppar1:  cpi     2               ;is it DCE?
        jnz     ppar1a          ;no, keep going
;
        call    ilprt           ;yes, print 'DCE'
        db      'DCE      ',0   ;       /
        jmp     ppar2           ;and keep going
;
ppar1a: cpi     3               ;is it self test mode?
        jnz     ppar2           ;no, don't know mode
;
        call    ilprt           ;else print 'self test'
        db      'self test',0   ;       /
;
ppar2:  call    ilprt           ;print baud rate
        db      tab,tab,'baud rate    = ',0
        lda     baudop          ;get current baud option
        lxi     h,150           ;<hl>= 150 baud
dbaud:  dad     h               ;multiply baud rate
        dcr     a               ;by number of option
        jnz     dbaud           ;until finished
;
        call    pdec            ;and print resulting number
;
        mvi     h,0             ;clear <h>
        call    ilprt           ;print last N(r)
        db      cr,lf,'last N(r)    = ',0
        lda     lastnr  
        mov     l,a     
        call    pdec    
;
        call    ilprt           ;print V(r)
        db      cr,lf,'V(r)         = ',0
        lda     vr
        mov     l,a
        call    pdec
;
        call    ilprt           ;print V(s)
        db      cr,lf,'V(s)         = ',0
        lda     vs
        mov     l,a
        call    pdec
;
        call    ilprt           ;print last V(s) sent
        db      cr,lf,'last V(s)    = ',0
        lda     lastvs
        mov     l,a
        call    pdec
;
        call    ilprt           ;print max V(s) sent
        db      cr,lf,'max  V(s)    = ',0
        lda     maxvs
        mov     l,a
        call    pdec
;
        call    ilprt           ;print P(r)
        db      cr,lf,lf,tab,tab,tab,'Level 3',lf
        db      cr,lf,'mode         =   ',0
        lda     pvcmod          ;get PVC/VC mode flag
        cpi     1               ;is it VC?
        jnz     ppar3           ;no, keep going
;
        call    ilprt           ;else print 'VC'
        db      ' VC',0
        jmp     ppar4           ;and keep going
;
ppar3:  cpi     2               ;is it PVC?
        jnz     ppar4           ;no, keep going
;
        call    ilprt           ;else print 'PVC'
        db      'PVC',0
;
ppar4:
        call    ilprt
        db      tab,tab,'local DTE address length = ',0
        lda     laddrl
        mov     l,a     
        call    pdec
;
        call    ilprt
        db      cr,lf,'P(r)         = ',0
        lda     pr
        mov     l,a
        call    pdec
;
        call    ilprt
        db      tab,tab,'local DTE address (hex)  = ',0
        lda     laddrl                  ;get local address length
        inr     a                       ;increment by one
        mov     b,a                     ;and save in <b>
        lxi     h,laddr                 ;point to address
ppar5:  dcr     b                       ;end of address?
        jz      ppar6                   ;yes, exit loop
;
        mov     a,m                     ;else get char
        call    phex1                   ;display it in hex
        inx     h                       ;bump pointer
        jmp     ppar5                   ;and get next char
;
ppar6:  call    ilprt           ;print P(s)
        db      cr,lf,'P(s)         = ',0
        lda     ps
        mvi     h,0
        mov     l,a
        call    pdec
;
        call    ilprt
        db      tab,tab,'remote DTE address length= ',0
        lda     raddrl
        mov     l,a     
        mvi     h,0
        call    pdec
;
        call    ilprt           ;print last tx P(s)
        db      cr,lf,'last tx P(s) = ',0
        lda     ltxps
        mov     l,a
        call    pdec
;
        call    ilprt
        db      tab,tab,'remote DTE address (hex) = ',0
        lda     raddrl                  ;get remote address length
        inr     a                       ;increment by one
        mov     b,a                     ;and save in <b>
        lxi     h,raddr                 ;point to address
ppar7:  dcr     b                       ;end of address?
        jz      ppar8                   ;yes, exit loop
;
        mov     a,m                     ;else get char
        call    phex1                   ;display it in hex
        inx     h                       ;bump pointer
        jmp     ppar7                   ;and get next char
;
ppar8:  call    ilprt           ;print last rx P(r)
        db      cr,lf,'last rx P(r) = ',0
        lda     lastpr
        mov     l,a
        mvi     h,0
        call    pdec
;
        call    ilprt
        db      tab,tab,'logical group number     = ',0
        lda     group
        mov     l,a
        mvi     h,0
        call    pdec
;
        call    ilprt           ;print last rx P(s)
        db      cr,lf,'last rx P(s) = ',0
        lda     lrxps
        mov     l,a
        mvi     h,0
        call    pdec
;
        call    ilprt
        db      tab,tab,'logical channel number   = ',0
        lda     chan
        mov     l,a
        mvi     h,0
        call    pdec
;
        call    ilprt
        db      cr,lf,lf,0
        ret




;       change system defaults
cparms: 
        call    setbaud         ;select & set baud rate
        call    setl2m          ;select level 2 mode
        call    initl2          ;and re-initialize level 2
        call    setl3m          ;select level 3 mode
        call    setaddr         ;select level 3 addresses
        call    initl3          ;and re-initialize level 3
        call    wparms          ;write parameters on file
        call    ilprt
        db      cr,lf,lf,0
        ret
        

;       select & set baud rate
;       on entry:       no parameters
;       on exit:        all regs, flags clobbered

setbaud:
        call    ilprt
        db      cr,lf,lf
        db      'baud rates:',cr,lf,lf
        db      '1.   300 baud',cr,lf
        db      '2.   600 baud',cr,lf
        db      '3.  1200 baud',cr,lf
        db      '4.  2400 baud',cr,lf
        db      '5.  4800 baud',cr,lf
        db      '6.  9600 baud',cr,lf
        db      '7. 19200 baud',cr,lf,0
bdlp:   call    getopt                  ;get option
        rz                              ;return if cr typed
;
        jc      bdlp                    ;if invalid try again
        cpi     8                       ;out of range?
        jnc     bdlp                    ;yes, get another one
;
        call    sbaud                   ;set baud rate
        ret



;       set level 2 DTE/DCE mode
;       on entry:       no parameters
;       on exit:        all regs, flags clobbered

setl2m:
        call    ilprt
        db      cr,lf,lf
        db      'level 2 mode:',cr,lf,lf
        db      '1. DTE (normal mode)',cr,lf
        db      '2. DCE (test mode)',cr,lf
        db      '3. self-test mode (with loopback connector)',cr,lf,0
modlp:  call    getopt                  ;get option
        rz                              ;return if cr typed
;
        jc      modlp                   ;if invalid get another
;
        cpi     4                       ;out of range
        jnc     modlp                   ;yes, get another
;
        sta     dtemod                  ;else update mode flag
        ret


;       set level 3 VC/PVC mode
;       on entry:       no parameters
;       on exit:        all regs, flags clobbered

setl3m:
        call    ilprt
        db      cr,lf,lf
        db      'level 3 mode:',cr,lf,lf
        db      '1. Virtual Call circuit (VC)',cr,lf
        db      '2. Permanent Virtual Circuit (PVC)',cr,lf,0
modlp3: call    getopt                  ;get option
        rz                              ;return if cr typed
;
        jc      modlp3                  ;if invalid get another
;
        cpi     3                       ;out of range
        jnc     modlp3                  ;yes, get another
;
        sta     pvcmod                  ;update mode flag
        ret


;       set level 3 dte addresses
;       on entry:       no parameters
;       on exit:        all regs, flags clobbered

setaddr:
;       clear local DTE address
        mvi     b,15            ;max address size (hex digits)
        lxi     h,laddr         ;point to address
        xra     a               ;clear <a>
        sta     laddrl          ;and address length
seta1:  mov     m,a             ;clear digit
        inx     h               ;bump pointer
        dcr     b               ;last digit?
        jnz     seta1           ;no, keep going

;       get local DTE address
        call    ilprt
        db      cr,lf,'local DTE address '
        db      '(0-15 hex digits) = ',0
        call    instr           ;input reply
        call    plf             ;print line feed
        lxi     h,inbuf         ;get # of hex digits
        inx     h               ;       /
        mov     a,m             ;      /
        ora     a               ;any there?
        jz      craddr          ;no, next parameter
;
        cpi     16              ;>15 digits?
        jnc     setaddr         ;yes, invalid
;
        sta     laddrl          ;else save address length
        mov     b,a             ;and save it in <b>
        inx     h               ;increment input buffer pointer
        lxi     d,laddr         ;point <de> to address buffer
seta2:  mov     a,m             ;get a char from input buffer
        call    hexbin          ;convert to binary
        jc      setaddr         ;restart if conversion error
;
        stax    d               ;else save in buffer
        inx     h               ;bump pointers
        inx     d               ;       /
        dcr     b               ;last char?
        jnz     seta2           ;no, get another
;       else drop through to:


;       set level 3 remote DTE address
;       (internally called)
;       on entry:       no parameters
;       on exit:        all regs, flags clobbered
craddr:
;       clear remote DTE address
        mvi     b,15            ;max address size (hex digits)
        lxi     h,raddr         ;point to address
        xra     a               ;clear <a>
        sta     raddrl          ;and address length
seta4:  mov     m,a             ;clear digit
        inx     h               ;bump pointer
        dcr     b               ;last digit?
        jnz     seta4           ;no, keep going

;       get remote DTE address
        call    ilprt
        db      cr,lf,'remote DTE address '
        db      '(0-15 hex digits) = ',0
        call    instr           ;input reply
        call    plf             ;print line feed
        lxi     h,inbuf         ;get # of hex digits
        inx     h               ;       /
        mov     a,m             ;      /
        ora     a               ;any there?
        rz                      ;no, return
;
        cpi     16              ;>15 digits?
        jnc     craddr          ;yes, invalid
;
        sta     raddrl          ;else save address length
        mov     b,a             ;and save it in <b>
        inx     h               ;increment input buffer pointer
        lxi     d,raddr         ;point <de> to address buffer
seta5:  mov     a,m             ;get a char from input buffer
        call    hexbin          ;convert to binary
        jc      craddr          ;restart if conversion error
;
        stax    d               ;else save in buffer
        inx     h               ;bump pointers
        inx     d               ;       /
        dcr     b               ;last char?
        jnz     seta5           ;no, get another
        ret                     ;else exit



;       get number input
;       on entry:       no parameters
;       on exit:        carry set if error
;                       <de>=number in binary (0-35756)

getnum:
        call    instr           ;input reply
        call    plf             ;print line feed
        lxi     h,inbuf         ;get # of characters
        inx     h               ;       /
        mov     a,m             ;      /
        ora     a               ;any there?
        stc                     ;no, set carry
        rz                      ;and return
;
        call    decbin          ;else convert input to binary
        ret                     ;return with carry if invalid



;       display status flags

pflags:
        call    clear                   ;clear crt screen
        call    ilprt
        db      lf
        db      tab,tab,tab,'System Status Flags',cr,lf,lf
        db      tab,'Level 1',tab,tab,tab,tab,'Level 2'
        db      cr,lf,'          bit # 76543210'
        db      tab,'            bit # 76543210'
        db      cr,lf,'     rx status: ',0
        lda     rxstat
        call    pbin
        call    ilprt
        db      tab,'     link status: ',0
        lda     lkstat  
        call    pbin
        call    ilprt
        db      cr,lf,'     tx status: ',0
        lda     txstat
        call    pbin
        call    ilprt
        db      tab,'         P/F bit: ',0
        lda     pfbit
        call    pbin
        call    ilprt
        db      cr,lf,'  Timer status: ',0
        lda     tistat
        call    pbin
        call    ilprt
        db      tab,'     flow status: ',0
        lda     l2stat
        call    pbin
        call    ilprt
        db      cr,lf,lf
        db      tab,'Level 3',tab,tab,tab,tab,'Disk Files'
        db      cr,lf,'          bit # 76543210'
        db      tab,'            bit # 76543210'
        db      cr,lf,'channel status: ',0
        lda     chstat
        call    pbin
        call    ilprt
        db      tab,'disk file status: ',0
        lda     fstat   
        call    pbin
        call    ilprt
        db      cr,lf,'flow status   : ',0
        lda     l3stat
        call    pbin
        call    ilprt
        db      cr,lf,'         Q bit: ',0
        lda     qbit
        call    pbin
        call    ilprt
        db      tab,tab,'Level 4'
        db      cr,lf,'         D bit: ',0
        lda     dbit
        call    pbin
        call    ilprt
        db      tab,'            bit # 76543210',0
        call    ilprt
        db      cr,lf,tab,tab,tab
        db      tab,'     flow status: ',0
        lda     l4stat
        call    pbin    
        call    ilprt
        db      cr,lf,lf,0
        ret


;       print level 1 and level 2 statistics
pl2stat:
        call    clear           ;clear crt screen
        call    ilprt
        db      lf
        db      tab,tab,tab,'    System Statistics:',cr,lf
        db      tab,'receive',tab,tab,tab,tab,tab,tab,'transmit',cr,lf
        db      tab,'-------',tab,tab,tab,tab,tab,tab,'--------',cr,lf
        db      tab,tab,tab,tab,'Level 1',cr,lf
        db      'good frames        = ',0
        lhld    rxgfct
        call    pdec
        call    ilprt
        db      tab,tab,' total completed frames= ',0
        lhld    txefct
        call    pdec
        call    ilprt
        db      cr,lf,'bad  frames        = ',0
        lhld    rxbfct
        call    pdec
        call    ilprt
        db      tab,tab,tab,'underrun errors= ',0
        lhld    txuect
        call    pdec
        call    ilprt
        db      cr,lf,'aborts             = ',0
        lhld    rxabct
        call    pdec
        call    ilprt
        db      tab,tab,tab,'aborts         = ',0
        lhld    txabct
        call    pdec
        call    ilprt
        db      cr,lf,'crc errors         = ',0
        lhld    rxcect
        call    pdec
        call    ilprt
        db      cr,lf,'overrun errors     = ',0
        lhld    rxoect
        call    pdec
        call    ilprt
        db      cr,lf,'chars lost due to buffer unavail= ',0
        lhld    rxbbct
        call    pdec
        call    ilprt
        db      cr,lf,'chars lost due to buffer overrun= ',0
        lhld    rxboct
        call    pdec
        call    ilprt
        db      cr,lf,lf,tab,tab,tab,tab,'Level 2'
        db      cr,lf,'command frames     = ',0
        lhld    rxcfct
        call    pdec
        call    ilprt
        db      tab,tab,tab,'   total frames= ',0
        lhld    txfct
        call    pdec
        call    ilprt
        db      cr,lf,'I frames           = ',0
        lhld    rxifct
        call    pdec
        call    ilprt   
        db      tab,tab,tab,'       I frames= ',0
        lhld    txifct
        call    pdec
        call    ilprt   
        db      cr,lf,'response frames    = ',0
        lhld    rxrfct
        call    pdec
        call    ilprt
        db      tab,tab,'I frame retransmissions= ',0
        lhld    txirct
        call    pdec
        call    ilprt
        db      cr,lf,'bad address frames = ',0
        lhld    rbafct
        call    pdec
        call    ilprt
        db      cr,lf,'bad command frames = ',0
        lhld    rxbcct
        call    pdec
        call    ilprt
        db      cr,lf,'bad response frames= ',0
        lhld    rxbrct
        call    pdec
        call    ilprt
        db      cr,lf,lf,0
        ret

;       print level 3 statistics
pl3stat:
        call    clear           ;clear crt screen
        call    ilprt
        db      lf
        db      tab,tab,tab,'    System Statistics:',cr,lf
        db      tab,'receive',tab,tab,tab,tab,tab,tab,'transmit',cr,lf
        db      tab,'-------',tab,tab,tab,tab,tab,tab,'--------',cr,lf
        db      cr,lf,tab,tab,tab,tab,'Level 3',cr,lf
        db      cr,lf,'total packets           = ',0
        lhld    rxpct
        call    pdec
        call    ilprt
        db      tab,tab,'total packets                = ',0
        lhld    txpct
        call    pdec
        call    ilprt
        db      cr,lf,'data packets            = ',0
        lhld    rxdpct
        call    pdec
        call    ilprt   
        db      tab,tab,'file data packets            = ',0
        lhld    txfpct
        call    pdec
        call    ilprt   
        db      cr,lf,'bad format packets      = ',0
        lhld    rbfpct
        call    pdec
        call    ilprt
        db      tab,tab,'console data packets         = ',0
        lhld    txcpct
        call    pdec
        call    ilprt
        db      cr,lf,'bad channel packets     = ',0
        lhld    rbcpct
        call    pdec
        call    ilprt
        db      tab,tab,'errors due to no free buffer = ',0
        lhld    ntxbct
        call    pdec
        call    ilprt
        db      cr,lf,'bad address packets     = ',0
        lhld    rbapct
        call    pdec
        call    ilprt
        db      cr,lf,'bad identifier packets  = ',0
        lhld    rbipct
        call    pdec
        call    ilprt
        db      cr,lf,'bad group # packets     = ',0
        lhld    rbgpct
        call    pdec
        call    ilprt
        db      cr,lf,'total discarded packets = ',0
        lhld    rxxpct
        call    pdec
        call    ilprt
        db      cr,lf,lf
        db      tab,tab,tab,tab,'Buffers',cr,lf
        db      cr,lf,'free rx buffers         = ',0
        lxi     h,rxfree
        call    getbct
        xchg
        call    pdec
        call    ilprt
        db      cr,lf,'buffers with frames     = ',0
        lxi     h,rxflst
        call    getbct
        xchg
        call    pdec
        call    ilprt
        db      cr,lf,'buffers with packets    = ',0
        lxi     h,rxplst
        call    getbct
        xchg
        call    pdec
        call    ilprt
        db      cr,lf,lf,0
        ret

;       set link connect and flow control ready flags (for debug)
;       (internally called)
;       on entry:       no parameters
;       on exit:        all flags, regs unchanged
setlkc: push    psw                     ;save flags
        push    h                       ;save <hl>
        call    t1off                   ;turn off T1 timer
        lxi     h,lkstat                ;point to link status flags
        mvi     m,0                     ;clear all flags
        setb    2,m                     ;and set link connected flag
        lxi     h,chstat                ;point to chanel status flags
        mvi     m,0                     ;clear all flags
        setb    0,m                     ;and set flow control ready state
        call    plstat                  ;tell operator
        pop     h                       ;restore <hl>
        pop     psw                     ;restore flags
        ret


;       print link status
;       (internally called)
;       on entry:       no parmameters
;       on exit:        <a>,flags clobbered
;                       all other regs unchanged
plstat: push    h               ;save <hl>
        lxi     h,lkstat        ;point to link status
        bit     0,m             ;link connect in process?
        jnz     plsta2          ;yes, exit
;
        bit     2,m             ;link connected?
        jz      plsta1          ;no, keep going
;
        call    ilprt           ;else display status
        db      'L2: link connected',cr,lf,0
        jmp     plsta2          ;and exit
;
plsta1: call    ilprt
        db      'L2: waiting for link connect from DCE',cr,lf,0
plsta2: pop     h               ;restore <hl>
        ret                     ;and exit



;       *****************
;       *  data area    *
;       *****************


        dseg            ;data segment

l4stat: db      0       ;level 4 status flags
oldstk: ds      2       ;old stack pointer
        ds      200h    ;really big local stack area
stack:  equ     $

