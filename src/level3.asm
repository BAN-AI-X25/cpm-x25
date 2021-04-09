title 'LEVEL3.ASM'
;************************************************
;*                                              *
;*              LEVEL3.ASM                      *
;*                                              *
;*  X.25 level 3 (packet) protocol handler      *
;*  (implements single logical channel DTE      *
;*   for use in a Virtual Call circuit (VC)     *
;*   or Permanent Virtual Circuit (PVC))        *
;*                                              *
;*  rev 1.30    08/21/84        E. Elizondo     *
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
        maclib  Z80     ;DR Z80 macro library

;       assembly time options
false   equ     0
true    equ     not false
debug   equ     false           ;display P(s) value

;       design parameters
        
n3      equ     2       ;retry count for level 3 timeout
kpack   equ     4       ;acknowledgement delay count

;       X.25 standard parameters:

;       logical group & channel number (para 2.4.1)
lchan   equ     1               ;logical channel 1
lgroup  equ     0               ;logical group 0

;       flow control parameters:
wsize   equ     2               ;window size (para 4.4.1.2)

;       packet type identifiers (table 8/X.25)
;       bit #   8765$4321
calrid: equ     0000$1011b      ;call request
calaid: equ     0000$1111b      ;call accepted
clrrid: equ     0001$0011b      ;clear request
clrcid: equ     0001$0111b      ;clear confirmation
intid:  equ     0010$0011b      ;interrupt
intcid: equ     0010$0111b      ;interrupt confirmation
rrid:   equ     0000$0001b      ;RR
rnrid:  equ     0000$0101b      ;RNR
rstrid: equ     0001$1011b      ;reset request
rstcid: equ     0001$1111b      ;reset confirmation
starid: equ     1111$1011b      ;restart request
stacid: equ     1111$1111b      ;restart confirmation
diagid: equ     1111$0001b      ;diagnostic

;       misc constants
cr      equ     0dh             ;carriage ret
lf      equ     0ah             ;line feed

;       hooks for main program
;       subroutines

        public  initl3          ;initialize level 3 parameters
        public  txdpk           ;transmit data packet if avail
        public  rxpk            ;receive packet if available
        public  txintp          ;transmit interrupt packet
        public  txstar          ;transmit restart packet
        public  txclrr          ;transmit clear request packet

;       addresses
        public  chstat          ;level 3 logical channel state
        public  l3stat          ;level 3 flow control flags

;       level 3 parameters
        public  ps              ;P(s)
        public  pr              ;P(r)
        public  lastpr          ;last rx P(r)
        public  ltxpr           ;last tx P(r)
        public  lrxps           ;last rx P(s)
        public  ltxps           ;last tx P(s)
        public  pvcmod          ;PVC mode flag
        public  laddr           ;local DTE address
        public  raddr           ;remote DTE address
        public  caddr           ;rx calling DTE address 
        public  laddrl          ;local DTE address length
        public  raddrl          ;remote DTE address length
        public  caddrl          ;rx calling DTE address length
        public  qbit            ;Q bit
        public  dbit            ;D bit
        public  chan            ;logical channel #
        public  group           ;logical group #

;       diagnostic counters
        public  txpct           ;total tx packets
        public  txfpct          ;total tx file data packets
        public  txcpct          ;total tx console data packets
        public  ntxbct          ;errors due to no free tx buffer
;
        public  rxpct           ;total rx packets
        public  rxdpct          ;total rx data packets
        public  rbfpct          ;total rx bad format packets
        public  rbcpct          ;total rx bad channel packets
        public  rbapct          ;total rx bad address packets
        public  rbipct          ;total rx bad id packets
        public  rbgpct          ;total rx bad group packets
        public  rxxpct          ;total discarded rx packets


;       definition of channel status (chstat) bits
;       (note that state r1 (packet level ready)
;        corresponds to level 2 link conn)
;       bit     set condition
;       0       flow control ready state (d1)
;       1       DTE restart request state (r2)
;       2       DTE waiting state (p2)
;       3       DTE reset request state (d2)
;       4       DTE clear request state (p6)
;       5       ready (p1)
;       6       undefined
;       7       undefined

;       definition of flow control (l3stat) status bits
;       bit     set condition
;       0       DTE busy
;       1       DCE busy
;       2       DTE interrupt pending confirmation
;       3       undefined
;       4       undefined
;       5       ualled DTE address error semaphore (internal)
;       6       transmission completed - ready to clear
;       7       outgoing message waiting for call setup


;       external subroutines

;       from buffers module:
        extrn   inibuf          ;initialize all buffers
        extrn   putbuf          ;put char in buffer
        extrn   getbuf          ;get char from buffer
        extrn   topbuf          ;put char at beginning of buffer
        extrn   bpoint          ;point to selected bcb
        extrn   rlsrxb          ;release rx buffer
        extrn   clrbuf          ;clear buffer for new use
        extrn   getbct          ;get buffer count
        extrn   getrdy          ;get state of buffer ready flag
        extrn   setrdy          ;set buffer ready flag
        extrn   clrrdy          ;clear ready flag

;       from level1 module:
        extrn   t20on           ;turn on timer T20
        extrn   t20off          ;turn off timer T20
        extrn   t21on           ;turn on timer T21
        extrn   t21off          ;turn off timer T21
        extrn   t22on           ;turn on timer T22
        extrn   t22off          ;turn off timer T22
        extrn   t23on           ;turn on timer T23
        extrn   t23off          ;turn off timer T23


;       from level2 module:
        extrn   gettxb          ;get address of free tx bcb

;       from files module
        extrn   gfdata          ;get byte from transmit file
        extrn   pfdata          ;write file to receive file
        extrn   ctxfil          ;close transmit file
        extrn   crxfil          ;close receive file

;       from xutil module:
        extrn   ilprt           ;in line print routine
        extrn   ctype           ;print char in <a>
        extrn   pdec            ;print <hl> in decimal
        extrn   phex            ;print <a> in hex
        extrn   phex1           ;print nibble in <a> in hex
        extrn   delay           ;wait a bit

;       external addresses
        
;       from level 1 module
        extrn   tistat          ;timer status flags
        extrn   rxstat          ;receive status

;       from level 2 module
        extrn   lkstat          ;level 2 status flags

;       from buffers module
        extrn   rxplst  ;A(list of rx packt buffer #'s)
        extrn   rxbtab  ;A(table of rx bcb pointers)
        extrn   txbtab  ;A(table of tx bcb pointers)
        extrn   ctbcb   ;A(bcb for console transmit)
        extrn   crbcb   ;A(bcb for console receive)

;       from files module
        extrn   fstat           ;file status flags

;       from x25 module
        extrn   l4stat          ;level 4 status flags

        cseg            ;code section


;       **************************
;       * initialization section *
;       **************************


;       initialize level 3 parameters
;       (externally and internally called)
;       on entry:       no parameters
;       on exit:        all regs, flags clobbered

initl3:
        mvi     a,lchan         ;get default channel number
        sta     chan            ;and initialize logical channel #
        mvi     a,lgroup        ;get default group number
        sta     group           ;and initialize logical group #
        mvi     a,0000$0001b    ;get modulo 8 general format id
        sta     gfi             ;initialize gfi
        call    reset           ;reset flow control variables
        xra     a
        sta     qbit            ;clear Q bit
        sta     dbit            ;clear D bit
        sta     chstat          ;clear logical channel state
        sta     l3stat          ;channel in ready (p1) state
        ret



;       reset flow control variables
;       (internally called)
;       on entry:       no parameters
;       on exit:        <a>,flags clobbered
;                       all other regs unchanged
reset:  xra     a
        sta     pr              ;clear P(r)
        sta     ps              ;clear P(s)
        sta     lastpr          ;clear last rx P(r)
        sta     ltxpr           ;and last tx P(r)
        mvi     a,7             ;initialize last rx P(s)
        sta     lrxps           ;       /
        sta     ltxps           ;and last tx P(s)
        ret


 
        ****************************
;       *  packet transmit section *
;       ****************************

;       check and process timeout condition or
;       transmit data packet if available and
;               a) Level 3 timers not timed out
;               b) level 2 link is connected
;               c) level 3 in data transfer ready state
;               d) DCE not busy
;               e) new frame P(s) is within window
;               f) free tx buffer available
;       and either      g) transmit file is open
;       or              h) console transmit buffer is ready
;               (file data takes precedence, as it's
;                transmitted as a continuous sequence)
;       if no I is available, transmit RR or RNR if any
;       received I packets need acknowledgement
;       (externally called)
;       on entry:       no parameters
;       on exit:        all regs, flags clobbered
;

txdpk:
        lxi     h,tistat        ;point to timer status flags
        bit     1,m             ;timer T20 timed out?
        jnz     t20to           ;yes, service it
;
        bit     2,m             ;timer T21 timed out?
        jnz     t21to           ;yes, service it
;
        bit     3,m             ;timer T22 timed out?
        jnz     t22to           ;yes, service it
;
        bit     4,m             ;timer T23 timed out?
        jnz     t23to           ;yes, service it
;
        lxi     h,lkstat        ;point to level 2 status
        bit     2,m             ;link connected?
        rz                      ;no, return
;
        lxi     h,chstat        ;point back to channel status
        bit     5,m             ;ready state (p1)?
        jnz     setup           ;yes, see if we want to call
;
        bit     0,m             ;flow control ready state (d1)?
        rz                      ;no, return
;
        lxi     h,l3stat        ;point to level 3 status
        bit     1,m             ;DCE busy?
        rnz                     ;yes, return
;
        bit     6,m             ;message complete?
        jnz     clear           ;yes, see if we want to clear
;
;
;       did we transmit data packet already?
        lda     ltxps           ;get last tx P(s)
        mov     b,a             ;save in <b>
        lda     ps              ;get current ps
        cmp     b               ;same?
        jz      txackp          ;yes, we did
;
;       check that P(s) is within window
        lda     lastpr          ;get last rx P(r)
        adi     wsize+1         ;calculate just past window
        ani     7               ;..mod 7
        mov     b,a             ;save in <b>
        lda     ps              ;get P(s)
        cmp     b               ;is P(s)=past top of window?
        jz      txackp          ;yes, don't transmit it yet
;
txdp0:  lxi     h,fstat         ;point to file status
        bit     1,m             ;tx file open?
        jz      txcdpk          ;no, see if console packet avail
;
;       transmit disk file data packet
;       (default to category A packet)
        call    gettxb          ;else get free tx bcb
        rc                      ;return if none avail
;
        xra     a               ;clear <a>
        sta     qbit            ;clear Q bit
        sta     dbit            ;clear D bit
        call    octet1          ;assemble octet 1 in <a>
        call    putbuf          ;put octet 1 in tx buffer
        lda     chan            ;get logical channel #
        call    putbuf          ;put octet 2 in tx buffer
        call    octet3          ;assemble octet 3 in <a>
        setb    4,a             ;set M=1 for now
        call    putbuf          ;store octet 3 in tx buffer

;       fill up user data field in packet
        mvi     b,128           ;max user data octets in packet
txdp1:  
        call    gfdata          ;get data byte from file
        jz      txdp2           ;exit if end of file
        call    putbuf          ;else put in buffer
        dcr     b               ;is packet full?
        jnz     txdp1           ;no, keep going
;
        jmp     txdp3           ;else exit
;
;       end of file - make into category B packet
txdp2:
        call    getbuf          ;get octet 1
        rc                      ;exit if not there
;
        mov     b,a             ;save in <b>
        call    getbuf          ;get octet 2
        rc                      ;exit if not there
;
        mov     c,a             ;save in <c>
        call    getbuf          ;get octet 3
        rc                      ;exit if not there
;
        res     4,a             ;set M=0
        call    topbuf          ;put back octet 3
        mov     a,c             ;get octet 2
        call    topbuf          ;and put it back
        mov     a,b             ;get octet 1
        setb    6,a             ;set D=1
        call    topbuf          ;and put it back
        call    ctxfil          ;close tx file

;       common exit routine
txdp3:
        call    txgo            ;transmit packet
        lda     ps              ;P(s)=P(s)+1 mod 7
;       
        if      debug           ;if debug mode
        call    prtps           ;print P(s)
        endif
;
        sta     ltxps           ;update last tx P(s)
        inr     a               ;increment P(s)
        ani     7               ;      /
        sta     ps              ;update P(s)
        lhld    txfpct          ;increment tx file packet count
        inx     h               ;       /
        shld    txfpct          ;      /
        mvi     a,kpack         ;initiaize ack delay count
        sta     packct          ;       /
        call    delay           ;and wait a bit
        ret



;       transmit console data packet if available
;       (internally called)
;       on entry:       no parameters
;       on exit:        all regs, flags clobbered
;
txcdpk:
        lxi     h,ctbcb         ;point to console transmit bcb
        call    getrdy          ;is it ready?
        jz      txackp          ;no, see if we want to acknowledge
;
        call    gettxb          ;else get free tx bcb
        rc                      ;return if none avail
;
;       this is a category B packet
        push    h               ;save bcb address
        lxi     h,qbit          ;point to Q bit
        res     7,m             ;set Q=0
        lxi     h,dbit          ;point to D bit
;***    setb    6,m             ;set D=1  (if desired)
        pop     h               ;restore bcb address
        call    octet1          ;assemble octet 1 in <a>
        call    putbuf          ;put octet 1 in tx buffer
        lda     chan            ;get logical channel #
        call    putbuf          ;put octet 2 in tx buffer
        call    octet3          ;build octet 3 in <a> with M=0
        call    putbuf          ;store in tx buffer
        mvi     b,128           ;max user data octets in packet

;       fill user data fields from console tx buffer
        xchg                    ;save <hl> in <de>
        lxi     h,ctbcb         ;point to console transmit bcb
txcdp1: 
        call    getbuf          ;get data byte from console buffer
        jc      txcdp2          ;exit if end of buffer
        xchg                    ;restore tx buffer bcb address
        call    putbuf          ;else put into tx buffer
        xchg                    ;point back to console xmit bcb
        dcr     b               ;is packet full?
        jnz     txcdp1          ;no, keep going
;
;       common exit routine
txcdp2:
        call    clrrdy          ;clear console xmit buffer flag
        xchg                    ;point back to tx buffer bcb
        call    txgo            ;transmit packet
        lda     ps              ;P(s)=P(s)+1 mod 7
;
        if debug
        call    prtps           ;print P(s)
        endif
;
        sta     ltxps           ;update last tx P(s)
        inr     a               ;incrment P(s)
        ani     7               ;      /
        sta     ps              ;update P(s)
        lxi     h,l4stat        ;point to level 4 status
        setb    0,m             ;signal prompt for next packet
        lhld    txcpct          ;increment tx console packet count
        inx     h               ;       /
        shld    txcpct          ;      /
        lxi     h,l3stat        ;point to flow status
        res     7,m             ;reset message waiting flag
        mvi     a,kpack         ;initialize ack delay count
        sta     packct          ;       /
        ret


;       transmit acknowledgement (RR or RNR) packet 
;       (internally called)
;       on entry:       no parameters
;       on exit:        all flags, regs clobbered
txackp:
        lda     ltxpr           ;get last tx P(r)
        mov     b,a             ;save in <b>
        lda     pr              ;get current P(r)
        cmp     b               ;same?
        rz                      ;yes, nothing to acknowledge
;
;       check delay count to see if any data packets are coming
        lda     packct          ;get delay count
        dcr     a               ;decrement it
        sta     packct          ;       /
        rnz                     ;wait a bit till it's 0
;
;       delay completed, transmit acknowledge packet
        mvi     a,kpack         ;initialize delay count
        sta     packct          ;       /
        lxi     h,l3stat        ;point to level 3 flow status
        bit     0,m             ;DTE busy?
        jnz     txrnr           ;yes, transmit RNR packet
;
        jmp     txrr            ;else transmit RR packet



;       print P(s)
;       (internally called)
;       on entry:       <a>=P(s)
;       on exit:        all flags, regs unchanged

prtps:  push    psw             ;save P(s)
        mvi     a,'['
        call    ctype
        pop     psw             ;restore P(s)
        push    psw             ;and save it again
        adi     '0'             ;convert to ASCII
        call    ctype           ;display it
        mvi     a,']'
        call    ctype
        pop     psw             ;restore P(s)
        ret


;       set up call if in VC mode
;       (internally called)
;       on entry:       no parameters
;       on exit:        all flags, regs clobbered

setup:
        lxi     h,chstat        ;point to channel status
        bit     2,m             ;DTE waiting state (p2)?
        rnz                     ;yes, do nothing
;
        lxi     h,l3stat        ;point to flow status
        bit     7,m             ;message ready?
        rz                      ;no, return
;
;       ready to set up call
        res     7,m             ;reset message waiting flag
        lxi     h,chstat        ;point back to channel status
        bit     0,m             ;flow control ready state (d1)?
        rnz                     ;yes, no setup needed
;
        jmp     txcr            ;else transmit call request packet



;       clear call if in VC mode
;       (internally called)

clear:
        lxi     h,l3stat        ;point to level 3 status
        res     6,m             ;reset message complete flag
        lda     pvcmod          ;get VC/PVC mode flag
        cpi     2               ;PVC?
        rz                      ;yes, do nothing if PVC
;
        jmp     txclrr          ;else transmit clear request packet


;       transmit call request packet
;       (internally called)
;       on entry:       no parameters
;       on exit:        all flags, regs clobbered

txcr:   
        call    gettxb          ;get free tx bcb
        jc      enotxb          ;error if none available
;
        mvi     c,calrid        ;get call request id
        call    put3oct         ;put octets 1-3 in tx buffer
        call    putaddr         ;put address octets in tx buffer

;***    (no facilities or call user data implemented)
        mvi     a,0             ;facilities length=0
        call    putbuf          ;put octet in tx buffer
        call    txgo            ;transmit packet
        call    reset           ;reset flow control variables
        call    t21on           ;start timer T21
        lxi     h,chstat        ;point to channel status
        bit     0,m             ;flow control ready state?
        rnz                     ;yes, all done
;
        setb    2,m             ;else signal DTE waiting state (p2)
        ret



;       transmit call accepted packet
;       (internally called)
;       on entry:       no parameters
;       on exit:        all flags, regs clobbered

txca:   
        call    gettxb          ;get free tx bcb
        jc      enotxb          ;error if none available
;
        mvi     c,calaid        ;get call accepted id
        call    put3oct         ;put octets 1-3 in tx buffer
        call    putaddr         ;put address octets in tx buffer
;***    (no facilities implemented)
        call    txgo            ;transmit packet
        ret



;       transmit clear request packet
;       (internally called)
;       on entry:       no parameters
;       on exit:        all flags, regs clobbered

txclrr:
        call    gettxb          ;get free tx bcb
        jc      enotxb          ;error if none available
;
        mvi     c,clrrid        ;get clear request id
        call    put3oct         ;put octets 1-3 in tx buffer
        mvi     a,0             ;clearing cause=0 for DTE
        call    putbuf          ;put octet 4 in tx buffer
        call    txgo            ;transmit packet
        call    reset           ;reset flow control variables
        call    t23on           ;start timer T23
        lxi     h,chstat        ;indicate DTE clear reqest state (P6)
        setb    4,m
        call    ctxfil          ;close transmit file
        call    crxfil          ;and receive file
        ret             



;       transmit clear confirmation packet
;       (internally called)
;       on entry:       no parameters
;       on exit:        all flags, regs clobbered

txclrc:
        call    gettxb          ;get free tx bcb
        jc      enotxb          ;error if none available
;
        mvi     c,clrcid        ;get clear confirmation id
        call    put3oct         ;put octets 1-3 in tx buffer
        call    txgo            ;transmit packet
        ret             



;       transmit interrupt packet
;       (externally called)
;       on entry:       <b>=interupt user data for octet 4
;       on exit:        all flags, regs clobbered

txintp: lxi     h,lkstat        ;point to level 2 status
        bit     2,m             ;link connected?
        jnz     txint1          ;yes, keep going
;
        call    ilprt           ;else tell operator
        db      'L3: link not connected - ',0
        jmp     txint5          ;and exit
;
txint1: lxi     h,chstat        ;point to channel status
        bit     0,m             ;flow control ready state?
        jnz     txint2          ;yes, keep going
;
        call    ilprt           ;else tell operator
        db      'L3: link not in data xfer state - ',0
        jmp     txint5          ;and exit

txint2: lxi     h,l3stat        ;point to level 3 flow status
        bit     2,m             ;interrupt already pending
        jz      txint3          ;no, keep going
;
        call    ilprt           ;else tell operator
        db      'L3: DTE interrupt is pending - ',0
txint5: call    ilprt
        db      'cannot send interrupt',cr,lf,0
        ret

txint3: call    gettxb          ;get free tx bcb
        jc      enotxb          ;error if none available
;
        mvi     c,intid         ;get interrupt id
        call    put3oct         ;put octets 1-3 in tx buffer
        mov     a,b             ;get interrupt user data
        call    putbuf          ;put octet 4 in tx buffer
        call    txgo            ;transmit packet
        lxi     h,l3stat        ;point to level 3 flow status
        setb    2,m             ;set interrupt pending flag
        ret             



;       transmit interrupt confirmation packet
;       (internally called)
;       on entry:       no parameters
;       on exit:        all flags, regs clobbered

txintc:
        call    gettxb          ;get free tx bcb
        jc      enotxb          ;error if none available
;
        mvi     c,intcid        ;get interrupt confirmation id 
        call    put3oct         ;put octets 1- 3 in tx buffer
        call    txgo            ;transmit packet
        ret             



;       transmit RR packet
;       (internally called)
;       on entry:       no parameters
;       on exit:        all flags, regs clobbered

txrr:
        mvi     c,rrid          ;<c>=RR packet identifier
        jmp     txrcom          ;and do common stuff



;       transmit RNR packet
;       (internally called)
;       on entry:       no parameters
;       on exit:        all flags, regs clobbered

txrnr:
        mvi     c,rnrid         ;<c>=RNR packet identifier
;
;       common stuff for RR and RNR
;       on entry:       <c>=packet identifier
txrcom: call    gettxb          ;get free tx bcb
        jc      enotxb          ;error if none available
;
        xra     a               ;clear Q and D bits
        sta     qbit            ;       /
        sta     dbit            ;      /
        call    octet1          ;assemble octet 1 in <a>
        call    putbuf          ;put octet 1 in tx buffer
        lda     chan            ;get logical channel #
        call    putbuf          ;put octet 2 in tx buffer
        lda     pr              ;get P(r)
        sta     ltxpr           ;update last tx P(r)
        rlc                     ;move to bits 8-6
        rlc                     ;       /
        rlc                     ;      /
        rlc                     ;     /
        rlc                     ;    /
        ani     1110$0000b      ;zero other bits
        ora     c               ;merge with P(r)
        call    putbuf          ;put octet 3 in tx buffer
        call    txgo            ;transmit packet
        ret             



;       transmit reset request packet
;       (internally called)
;       on entry:       no parameters
;       on exit:        all flags, regs clobbered

txrstr: call    gettxb          ;get free tx bcb
        jc      enotxb          ;error if none available
;
        mvi     c,rstrid        ;get reset request id
        call    put3oct         ;put octets 1-3 in tx buffer
        mvi     a,0             ;resetting cause=0 for DTE
        call    putbuf          ;put octet 4 in tx buffer
        call    txgo            ;transmit packet
        call    reset           ;reset flow control variables
        call    t22on           ;start timer T22
        lxi     h,chstat        ;point to channel status
        setb    3,m             ;signal reset request state
        ret             



;       transmit reset confirmation packet
;       (internally called)
;       on entry:       no parameters
;       on exit:        all flags, regs clobbered

txrstc: call    gettxb          ;get free tx bcb
        jc      enotxb          ;error if none available
;
        mvi     c,rstcid        ;get reset confirmation id
        call    put3oct         ;put octets 1-3 in tx buffer
        call    txgo            ;and transmit packet
        ret



;       transmit restart request packet
;       (internally & externally called)
;       on entry:       no parameters
;       on exit:        all flags, regs clobbered

txstar: lxi     h,lkstat        ;point to level 2 status
        bit     2,m             ;link connected?
        jnz     txstar1         ;yes, keep going
;
        call    ilprt           ;else tell operator
        db      'L3: link not connected',cr,lf,0
        ret
;
txstar1:
        call    gettxb          ;get free tx bcb
        jc      enotxb          ;error if none available
;
        mvi     c,starid        ;get restart request packet id
        call    txscom          ;do common stuff
        mvi     a,0             ;set restarting cause=0 for DTE
        call    putbuf          ;       /
        call    txgo            ;transmit packet
        call    reset           ;reset flow control variables
        lxi     h,chstat        ;point to channel status
        setb    1,m             ;indicate restart request state
        call    t20on           ;start timer T20
        call    t21off          ;and stop all others
        call    t22off          ;       /
        call    t23off          ;      /
        ret


;       transmit restart confirmation packet
;       (internally called)
;       on entry:       no parameters
;       on exit:        all flags, regs clobbered

txstac: call    gettxb          ;get free tx bcb
        jc      enotxb          ;error if none available
;
        mvi     c,stacid        ;get restart confirmation id
        call    txscom          ;do common stuff
        call    txgo            ;transmit packet
        ret

;       common stuff for restart request/confirmation
;       on entry:       <c>=packet id
txscom: lda     gfi             ;get general format identifier
        rlc                     ;move to bits 8-5
        rlc                     ;       /
        rlc                     ;      /
        rlc                     ;     /
        ani     1111$0000b      ;make sure lower bits are all 0
        call    putbuf          ;put octet 1 in tx buffer
        xra     a               ;next octet is all 0's
        call    putbuf          ;put octet 2 in tx buffer
        mov     a,c             ;get packet identifier
        call    putbuf          ;put octet 3 in tx buffer
        ret             



;       error handling routine for no available free tx buffer
;       (internally called)

enotxb:
        lhld    ntxbct          ;increment error count
        inx     h               ;       /
        shld    ntxbct          ;      /
        ret


;       *****************************************
;       * utility routines for tx section       *
;       *****************************************

;       transmit packet
;       (internally called)
;       on entry:       <hl>=address of tx buffer bcb
;       on exit:        all regs, flags clobbered
 
txgo:   call    setrdy          ;set tx buffer ready flag
        lhld    txpct           ;increment total tx packet count
        inx     h               ;       /
        shld    txpct           ;      /
        ret


;       put octets 1-3 in tx buffer
;       (internally called)
;       on entry:       <hl>=bcb address of tx buffer
;                       <c>=packet identifier
;       on exit:        <a>,flags clobbered
;                       all other regs unchanged
put3oct:
        xra     a               ;clear Q and D bits
        sta     qbit            ;       /
        sta     dbit            ;      /
        call    octet1          ;assemble octet 1 in <a>
        call    putbuf          ;put octet 1 in tx buffer
        lda     chan            ;get logical channel #
        call    putbuf          ;put octet 2 in tx buffer
        mov     a,c             ;get packet identifier
        call    putbuf          ;put octet 3 in tx buffer
        ret             


;       build octet 1
;       (internally called)
;       on entry:       no parameters
;       on exit:        <a>=octet 1
;                       flags clobbered
;                       all other regs unchanged

octet1: push    b               ;save <bc>
        lda     gfi             ;get general format identifier
        rlc                     ;move to bits 8-5
        rlc                     ;       /
        rlc                     ;      /
        rlc                     ;     /
        mov     b,a             ;save result in <b>
        lda     qbit            ;get q bit
        ora     b               ;merge with gfi
        mov     b,a             ;and save result in <b>
        lda     dbit            ;get d bit
        ora     b               ;merge with gfi
        ani     0f0h            ;make sure lower 4 bits are 0
        mov     b,a             ;and save result in <b>
        lda     group           ;get logical group #
        ani     0fh             ;zero upper 4 bits
        ora     b               ;merge with gfi
        pop     b               ;restore <bc>
        ret             



;       build octet 3 of data packet
;       (internally called)
;       on entry:       no parameters
;       on exit:        <a>= octet 3
;                       all other regs unchanged

octet3:
        push    b               ;save <bc>
        lda     pr              ;get P(r)
        sta     ltxpr           ;update last tx P(r)
        rlc                     ;move to bits 8-6
        rlc                     ;       /
        rlc                     ;      /
        rlc                     ;     /
        rlc                     ;    /
        ani     1110$0000b      ;zero other bits
        mov     b,a             ;save result in <b>
        lda     ps              ;get P(s)
        rlc                     ;move to bits 4-2
        ani     0000$1110b      ;zero other bits
        ora     b               ;merge with P(r) and M
        pop     b               ;restore <bc>
        ret


;       put address octets in tx buffer
;       (internally called)
;       on entry:       <hl>=tx bcb address
;       on exit:        <a>,flags clobbered
;                       all other regs unchanged

putaddr:
        push    b               ;save <bc>
        push    d               ;save <de>
;
;       build octet 4 in tx buffer
        lda     laddrl          ;get local address length
        rlc                     ;move to bits 8-5
        rlc                     ;       /
        rlc                     ;      /
        rlc                     ;     /
        ani     1111$0000b      ;make sure lower 4 bits are 0
        mov     b,a             ;save result in <b>
        lda     raddrl          ;get remote address length
        ani     0000$1111b      ;make sure upper 4 bits are 0
        ora     b               ;merge with local address length
        call    putbuf          ;put octet 4 in tx buffer
;
;       now build called DTE address, if present
        lda     raddrl          ;get remote address length
        ora     a               ;length=0?
        jz      putad2          ;yes, skip remote address
;
        mov     b,a             ;save length in <b>
        lxi     d,raddr         ;point <de> to remote address
putad1: ldax    d               ;get first char
        rlc                     ;move to bits 8-5
        rlc                     ;       /
        rlc                     ;      /
        rlc                     ;     /
        ani     1111$0000b      ;make sure lower bits are 0
        mov     c,a             ;save result in <c>
        inx     d               ;point to next address digit
        dcr     b               ;last digit?
        jz      putad3          ;yes, get local address
;
        ldax    d               ;else get next char
        ani     0000$1111b      ;make sure upper bits are 0
        ora     c               ;merge with last digit
        call    putbuf          ;and put in tx buffer
        inx     d               ;point to next address digit
        dcr     b               ;last digit
        jnz     putad1          ;no, get another
;
;       
;       build calling DTE address, if present
;       entry point if called DTE address is even # of digits
putad2: lda     laddrl          ;get local address length
        ora     a               ;length=0?
        jz      putadexi        ;yes, exit
;
        mov     b,a             ;else save length in <b>
        lxi     d,laddr         ;point <de> to local address
        jmp     putad4          ;and keep going
;
;       entry point if called DTE address is odd # of digits
putad3: lda     laddrl          ;get local address length
        ora     a               ;length=0?
        jz      putadexi        ;yes, exit
;
        mov     b,a             ;else save length in <b>
        lxi     d,laddr         ;point <de> to local address
        jmp     putad5          ;and keep going
;
;       build msb digit
putad4: ldax    d               ;get first char
        rlc                     ;move to bits 8-5
        rlc                     ;       /
        rlc                     ;      /
        rlc                     ;     /
        ani     1111$0000b      ;make sure lower bits are 0
        mov     c,a             ;save result in <c>
        inx     d               ;point to next address digit
        dcr     b               ;last digit?
        jnz     putad5          ;no, get next character
;
        call    putbuf          ;else put last octet in tx buffer
        jmp     putadexi        ;and exit
;
;       build lsb digit
putad5: ldax    d               ;get next char
        ani     0000$1111b      ;make sure upper bits are 0
        ora     c               ;merge with last digit
        call    putbuf          ;and put in tx buffer
        inx     d               ;point to next address digit
        dcr     b               ;last digit
        jnz     putad4          ;no, get another
;
;       common exit
putadexi:
        pop     d               ;restore <de>
        pop     b               ;restore <bc>
        ret



;       *************************
;       *  process timeouts     *
;       *************************



;       process timeout of DTE timer T20
;       (internally called)
;       on entry:       no parameters
;       on exit:        all flags, regs clobbered

t20to:  
        call    t20off          ;turn off timer
        lxi     h,rtryct        ;get retry count
        dcr     m               ;last retry?
        jz      t20to1          ;yes, other end is dead
;
        call    ilprt           ;else try once more
        db      'L3: T20 timed out - '
        db      'retransmitting restart request packet',cr,lf,0
        call    t20on           ;restart timer T20
        jmp     txstar          ;and transmit restart
;
t20to1: call    ilprt
        db      'L3: tx retry count exhausted - '
        db      'no reply from DCE',cr,lf,0
        ret                     



;       process timeout of DTE timer T21
;       (internally called)
;       on entry:       no parameters
;       on exit:        all flags, regs clobbered

t21to:  
        call    t21off          ;turn off timer
        call    ilprt           ;and tell operator
        db      'L3: T21 timed out - '
        db      'transmitting clear request packet',cr,lf,0
        jmp     txclrr          ;and transmit clear request



;       process timeout of DTE timer T22
;       (internally called)
;       on entry:       no parameters
;       on exit:        all flags, regs clobbered

t22to:  
        call    t22off          ;turn off timer
        lxi     h,rtryct        ;get retry count
        dcr     m               ;last retry?
        jz      t22to1          ;yes, other end is dead
;
        call    ilprt           ;else try once more
        db      'L3: T22 timed out - '
        db      'retransmitting reset request packet',cr,lf,0
        call    t22on           ;restart timer T22
        jmp     txrstr          ;and transmit reset request
;
t22to1: call    ilprt
        db      'L3: tx retry count exhausted - '
        db      'logical channel out of order',cr,lf,0
        ret                     



;       process timeout of DTE timer T23
;       (internally called)
;       on entry:       no parameters
;       on exit:        all flags, regs clobbered

t23to:  
        call    t23off          ;turn off timer
        lxi     h,rtryct        ;get retry count
        dcr     m               ;last retry?
        jz      t23to1          ;yes, other end is dead
;
        call    ilprt           ;else try once more
        db      'L3: T23 timed out - '
        db      'retransmitting clear request packet',cr,lf,0
        call    t23on           ;restart timer T23
        jmp     txclrr          ;transmit clear request
;
t23to1: call    ilprt
        db      'L3: tx retry count exhausted - '
        db      'logical channel out of order',cr,lf,0
        ret                     



;       ***************************
;       *  packet receive section *
;       ***************************

;       receive packet if available
;       on entry:       no parameters
;       on exit:        <hl>=bcb address if packet avail
;                       <b>= packet octet # 3 (packet id)
;                       <c>= rx buffer #


rxpk:   lxi     h,rxplst        ;point to received packet list
        call    getbuf          ;any there?
        rc                      ;no, exit
;
        lhld    rxpct           ;increment rx packet count
        inx     h               ;       /
        shld    rxpct           ;      /
;
        mov     c,a             ;else save rx buffer # in <c>
        lxi     h,rxbtab        ;get buffer bcb address
        call    bpoint          ;       /
        call    getbuf          ;get octet 1
        jc      rxfmer          ;format error if not there
;
        mov     d,a             ;save octet 1 in <d>
        call    getbuf          ;get octet 2
        jc      rxfmer          ;format error if not there
;
        mov     e,a             ;save octet 2 in <e>
        call    getbuf          ;get octet 3
        jc      rxfmer          ;format error if not there
;
        mov     b,a             ;save octet 3 in <b>
        cpi     starid          ;restart request?
        jz      chkrst          ;yes, check format
;
        cpi     stacid          ;restart confirmation
        jz      chkrst          ;yes, check format
;
        cpi     diagid          ;diagnostic
        jz      chkrst          ;yes, check format
;
;       process packets having logical group and channel
        lda     chan            ;get logical channel #
        cmp     e               ;match?
        jnz     rxcher          ;no, channel # error
;
        mov     a,d             ;get back octet 1
        ani     0011$0000b      ;get bits 6-5
        cpi     0001$0000b      ;numbering modulo 8?
        jnz     rxfmer          ;no, format error
;
        mov     a,d             ;get back octet 1
        ani     0000$1111b      ;get bits 4-1
        push    b               ;save <bc>
        mov     b,a             ;save logical group # in <b>
        lda     group           ;get our logical group #
        cmp     b               ;same?
        pop     b               ;restore <bc>
        jnz     rxgrer          ;no, group # error
;
;       branch to process known packet types
        mov     a,b             ;get back octet 3
        bit     0,a             ;data packet?
        jz      rxdata          ;yes, process it
;
        cpi     intid           ;interrupt packet?
        jz      rxint           ;yes, process it
;
        cpi     intcid          ;interrupt confirmation?
        jz      rxintc          ;yes, process it
;
        cpi     rstrid          ;reset request?
        jz      rxrstr          ;yes, process it
;
        cpi     rstcid          ;reset confirmation?
        jz      rxrstc          ;yes, process it
;
        cpi     calrid          ;call request?
        jz      rxcr            ;yes, process it
;
        cpi     calaid          ;call accepted?
        jz      rxca            ;yes, process it
;
        cpi     clrrid          ;clear request?
        jz      rxclr           ;yes, process it
;
        cpi     clrcid          ;clear confirmation?
        jz      rxclc           ;yes, process it
;
;       process packets numbered modulo 8
rxpk3:  ani     0001$1111b      ;leave only bits 5-1
        cpi     rrid            ;RR
        jz      rxrr            ;yes, process it
;
        cpi     rnrid           ;RNR?
        jz      rxrnr           ;yes, process it
;
        jmp     rxpk4           ;else process id error

;       check format of restart and diagnostic packets
chkrst: mov     a,d             ;get back octet 1
        cpi     0001$0000b      ;modulo 8 format?
        jnz     rxfmer          ;no, format error
;
        mov     a,e             ;get back octet 2
        ora     a               ;all zero?
        jnz     rxfmer          ;no, format error
;
;       now branch to known restart packet types
        mov     a,b             ;get back octet 3
        cpi     starid          ;restart?
        jz      rxstar          ;yes, process it
;
        cpi     stacid          ;restart confirmation?
        jz      rxstac          ;yes, process it
;
        cpi     diagid          ;diagnostic?
        jz      rxdiag          ;yes, process it
;       else id is unrecognized so drop through to
;
;       process unrecognized id error
rxpk4:  call    flush           ;discard packet
        lhld    rbipct          ;increment bad id counter
        inx     h               ;       /
        shld    rbipct          ;      /
        ret

;       process packet format error
rxfmer: call    flush           ;discard packet
        lhld    rbfpct          ;increment bad format counter
        inx     h               ;       /
        shld    rbfpct          ;      /
        ret

;       process packet group error
rxgrer: call    flush           ;discard packet
        lhld    rbgpct          ;increment bad group counter
        inx     h               ;       /
        shld    rbgpct          ;      /
        ret

;       process packet channel error
rxcher: call    flush           ;discard packet
        lhld    rbcpct          ;increment bad channel counter
        inx     h               ;       /
        shld    rbcpct          ;      /
        ret



;       process rx data packet
;       (internally called)
;       on entry:       <hl>=rx buffer bcb address
;                       <b>=packet octet #3
;                       <c>=rx buffer #
;       on exit:        all regs, flags clobbered

rxdata:
        xchg                    ;save bcb address
        lxi     h,chstat        ;point to channel state
        bit     0,m             ;flow control ready state?
        xchg                    ;get back bcb address
        jz      flush           ;no, discard packet
;
        call    ackd            ;process P(s) and P(r)
        jc      dterst          ;reset channel if invalid
;
        xchg                    ;save bcb address
        lxi     h,fstat         ;point to file status
        bit     0,m             ;receive file open?
        xchg                    ;restore bcb address
        jnz     rxfdat          ;yes, write packet in file
;
;       else write packet to console receive buffer
        lxi     d,crbcb         ;point to console rx buffer
rxcdat: call    getbuf          ;get data octet from rx buffer
        jc      rxcd1           ;exit when empty
;
        xchg                    ;point to console rx buffer
        call    putbuf          ;put octet in console rx buffer
        xchg                    ;point back to rx buffer
        jmp     rxcdat          ;and go back for more
;
;       process end of packet to console
rxcd1:  xchg                    ;point to console rx buffer
        call    setrdy          ;set buffer ready flag
        jmp     rxdexi          ;do common exit stuff


;       write packet into disk file
rxfdat:
        call    getbuf          ;get data octet from rx buffer
        jc      rxfd1           ;exit if empty
;
        call    pfdata          ;else put data in file
        jmp     rxfdat          ;and loop until empty
;
;       process end of rx packet data
rxfd1:
        mvi     a,'+'           ;tell console packet rx
        call    ctype           ;       /
        jmp     rxdexi          ;do common exit stuff

;       common exit for rx packet data
rxdexi:
        mov     a,c             ;get rx buffer #
        call    rlsrxb          ;release rx buffer
        lhld    rxdpct          ;increment rx data packet count
        inx     h               ;       /
        shld    rxdpct          ;      /
        ret



;       process rx incoming call packet
;       (internally called)
;       on entry:       <hl>=bcb address
;                       <b>=packet octet 3 (id)
;                       <c>=rx buffer #

rxcr:
        call    getbuf          ;get octet 4
        jc      rxfmer          ;format error if not there
;
        sta     rxoc4           ;save the octet
        xchg                    ;save bcb address
        lxi     h,chstat        ;point to channel status
        bit     0,m             ;flow control ready state (d1)?
        jnz     rxcr1           ;yes, accept packet
;
        bit     1,m             ;DTE restart request state (r2)?
        jnz     flush           ;yes, discard packet 
;
        bit     2,m             ;waiting state (p2)?
        jnz     rxcr1           ;yes, accept packet
;
        bit     3,m             ;reset request state (d2)?
        jnz     rxcr1           ;yes, accept packet
;
        bit     4,m             ;clear request state (p6)?
        jnz     rxcr1           ;yes, accept packet
;
        bit     5,m             ;ready state (p1)?
        jnz     rxcr1           ;yes, accept packet
;
        jmp     flush           ;else discard packet
;
rxcr1:  xchg                    ;get back bcb address
        call    chkcadr         ;check whether address is for us
        jc      badadr          ;refuse call if invalid
;
        call    getbuf          ;get facilities length octet
        jc      rxfmer          ;format error if not there
;       
;***    special facilities not implemented
        ora     a               ;facilities requested?
        jnz     calrej          ;yes, refuse call
;
        mov     a,c             ;get rx buffer #
        call    rlsrxb          ;release it
        call    reset           ;reset flow control variables
        call    t21off          ;turn off timer T21
        call    t22off          ;turn off timer T22
        call    t23off          ;turn off timer T23
        call    ilprt
        db      'L3: rx incoming call ',0
        lda     caddrl          ;get calling DTE address length
        ora     a               ;=0?
        jz      rxca3           ;yes, keep going
;
        inr     a               ;increment length by one
        mov     b,a             ;and save in <b>
        call    ilprt           ;display calling address
        db      'from: ',0
        lxi     h,caddr         ;point to calling address
rxca2:  dcr     b               ;end of address?
        jz      rxca3           ;yes, exit loop
;
        mov     a,m             ;else get nibble
        call    phex1           ;display it in hex
        inx     h               ;bump pointer
        jmp     rxca2           ;and get next nibble
;
rxca3:  call    ilprt           ;terminate message
        db      cr,lf,0
        lxi     h,l3stat        ;point to flow status
        res     1,m             ;reset DCE busy flag
        res     2,m             ;reset interrupt pending flag
        res     6,m             ;reset end of message flag
        lxi     h,chstat        ;point to channel status
        setb    0,m             ;set flow control ready state (d1)
        res     3,m             ;reset DTE reset request state (d2)
        res     4,m             ;reset DTE clear request state (p6)
        res     5,m             ;reset ready state (p1)
        bit     2,m             ;DTE waiting state (p2)?
        jz      txca            ;no, tx call accepted packet
;
;       call collision state (p5)
        res     2,m             ;else reset state p2
        ret                     ;and exit



;       process rx call connected packet
;       (internally called)
;       on entry:       <hl>=bcb address
;                       <b>=packet octet 3 (id)
;                       <c>=rx buffer #

rxca:
        xchg                    ;save bcb address
        lxi     h,chstat        ;point to channel status
        bit     2,m             ;DTE waiting state (p2)?
        jnz     rxca0           ;yes, accept packet
;
        jmp     flush           ;else discard packet
;
rxca0:  xchg                    ;get back bcb address
        call    getbuf          ;get octet 4 if present
        jc      rxca1           ;skip address check if not present
;
        sta     rxoc4           ;save octet 4 if present
        call    chkadr          ;check addresses
        jc      badadr          ;refuse call if invalid
;
        call    getbuf          ;get octet 5 if present
        jc      rxca1           ;skip facilities check if not present
;
;***    special facilities not implemented
;
;       checks all done, accept call
rxca1:  mov     a,c             ;get rx buffer #
        call    rlsrxb          ;release it
        call    reset           ;reset flow control variables
        call    t21off          ;turn off timer T21
        call    t22off          ;turn off timer T22
        call    t23off          ;turn off timer T23
        call    ilprt
        db      'L3: rx call connected',cr,lf,0
        lxi     h,l3stat        ;point to flow control status
        res     1,m             ;reset DCE busy flag
        res     2,m             ;reset interrupt pending flag
        res     6,m             ;reset message complete flag
        lxi     h,chstat        ;point to channel status
        setb    0,m             ;set flow control ready state (d1)
        res     2,m             ;reset DTE waiting state (p2)
        res     3,m             ;reset DTE reset request state (d2)
        res     4,m             ;reset DTE clear request state (p6)
        res     5,m             ;reset ready state (p1)
        ret                     ;and exit


;       process invalid address
;       (internally called)
;       on entry:       <c>=rx buffer #
;       on exit:        all flags, regs clobbered

;       
badadr: call    flush           ;discard packet
        lhld    rbapct          ;increment bad address count
        inx     h               ;       /
        shld    rbapct          ;      /
        call    ilprt
        db      'L3: bad address - tx clear request',cr,lf,0
        jmp     txclrr          ;transmit clear request


;       refuse call
;       (internally called)
;       on entry:       <c>=rx buffer #
;       on exit:        all flags, regs clobbered


calrej: call    flush           ;discard packet
        call    ilprt
        db      'L3: call refused - tx clear request',cr,lf,0
        jmp     txclrr          ;transmit clear request



;       process rx clear indication (clear request) packet
;       (internally called)
;       on entry:       <hl>=bcb address
;                       <b>=packet octet 3 (id)
;                       <c>=rx buffer #

rxclr:
        call    getbuf          ;get octet 4
        jc      rxfmer          ;format error if not there
;
        mov     b,a             ;save octet 4 in <b>
        xchg                    ;save bcb address
        lxi     h,chstat        ;point to channel status
        bit     0,m             ;flow control ready state (d1)?
        jnz     rxclr0          ;yes, accept packet
;
        bit     4,m             ;clear request state (p6)?
        jnz     rxclr0          ;yes, accept packet
;
        jmp     flush           ;else discard packet
;
rxclr0: xchg                    ;restore bcb address
        call    ilprt           ;tell operator
        db      'L3: rx clear indication packet, cause: ',0
        mov     a,b             ;get back octet 4
        call    phex            ;print in hex
        call    ilprt           ;       /
        db      ' (hex)',cr,lf,0                ;      /
        call    getbuf          ;get octet 5
        jc      rxclr2          ;keep going if not there
;
        mov     b,a             ;save octet 5 in <b>
        call    ilprt
        db      '    diagnostic code      : ',0
        mov     l,b             ;print diagnostic code
        mvi     h,0             ;       /
        call    pdec            ;print in decimal
        call    ilprt
        db      cr,lf,0
;
;       perform flow control actions
rxclr2:
        mov     a,c             ;get rx buffer #
        call    rlsrxb          ;and release it
        call    reset           ;reset flow control variables
        call    t21off          ;stop timers
        call    t22off          ;      /
        call    t23off          ;     /
        call    ctxfil          ;close transmit file
        call    crxfil          ;close receive file
        lxi     h,chstat        ;point to channel status
        res     0,m             ;reset state d1
        res     2,m             ;reset state p2
        res     3,m             ;reset state d2
        setb    5,m             ;set ready state p1
        bit     4,m             ;was it DTE clear request state (p6)?
        jz      txclrc          ;no, tx clear confirmation packet
;
;       DTE clear request state (p6)
        res     4,m             ;else reset clear request state 
        ret                     ;and exit



;       process rx clear confirmation packet
;       (internally called)
;       on entry:       <hl>=bcb address
;                       <b>=packet octet 3 (id)
;                       <c>=rx buffer #

rxclc:
        call    getbuf          ;get octet 4
        jnc     rxfmer          ;format error if there
;
        lxi     h,chstat        ;point to channel status
        bit     4,m             ;DTE clear request state (p6)?
        jz      flush           ;no, discard packet
;
;       DTE clear request state (p6)
        res     4,m             ;reset back to ready state
        mov     a,c             ;get rx buffer #
        call    rlsrxb          ;and release it
        call    reset           ;reset flow control variables
        call    ilprt           ;tell operator
        db      'L3: rx clear confirmation packet',cr,lf,0
        ret


        
;       process rx INT packet
;       (internally called)
;       on entry:       <hl>=bcb address
;                       <c>=rx buffer #

rxint:
        call    getbuf          ;get octet 4
        jc      rxfmer          ;format error if not there
;
        xchg                    ;save bcb address in <de>
        lxi     h,chstat        ;point to channel status
        bit     0,m             ;flow control ready state (d1)?
        jz      flush           ;no, discard packet
;
        xchg                    ;restore bcb address
        mov     b,a             ;save octet 4 in <b>
        call    ilprt
        db      'L3: rx INT packet - user data: ',0
        mov     a,b             ;get back octet 4
        call    phex            ;print in hex
        call    ilprt           ;terminate line
        db      cr,lf,0
        mov     a,c             ;get rx buffer #
        call    rlsrxb          ;release it
        jmp     txintc          ;and transmit int confirmation


;       process rx INT confirmation packet
;       (internally called)
;       on entry:       <hl>=bcb address
;                       <c>=rx buffer #

rxintc:
        call    getbuf          ;tryto get octet 4
        jnc     rxfmer          ;format error if there
;
        lxi     h,chstat        ;point to channel status
        bit     0,m             ;flow control ready state (d1)?
        jz      flush           ;no, discard packet
;
        lxi     h,l3stat        ;point to level 3 status
        bit     2,m             ;interrupt pending?
        jz      flush           ;no, discard packet
;
        res     2,m             ;else clear int pending flag
        call    ilprt
        db      'L3: rx interrupt confirmation packet',cr,lf,0
        mov     a,c             ;get rx buffer #
        call    rlsrxb          ;release it
        ret


;       process DIAG packet
;       (internally called)
;       on entry:       <hl>=bcb address
;                       <b>=packet octet #3 (packet id)
;                       <c>=rx buffer #

rxdiag:
        call    getbuf          ;get octet 4
        jc      rxfmer          ;format error if not there
;
        xchg                    ;save bcb address in <de>
        mov     b,a             ;save octet 4 in <b>
        call    ilprt
        db      'L3: rx DIAG packet - diagnostic # ',0
        mov     l,b             ;print diagnostic code
        mvi     h,0             ;       /
        call    pdec            ;      /
        call    ilprt           ;print explanation if present
        db      cr,lf,'L3: diagnostic explanation: ',0
        xchg                    ;get back bcb address

;       print diagnostic explanation
diaglp: call    getbuf          ;get next octet
        jc      diagexi         ;exit if not there
;
        call    phex            ;print in hex
        call    ilprt           ;and separator
        db      ' ',0
        jmp     diaglp          ;and try for next byte
;
diagexi:
        call    ilprt           ;terminate explanation line
        db      cr,lf,0
        mov     a,c             ;get rx buffer
        call    rlsrxb          ;release it
        ret



;       process rx RR packet
;       (internally called)
;       on entry:       <b>=octet 3             
;                       <c>=rx buffer #

rxrr:   lxi     h,chstat        ;point to channel status
        bit     0,m             ;flow control ready state?
        jz      flush           ;no, discard packet
;
        call    chkpr           ;check P(r) and update window
        jc      dterst          ;local procedure error
;
        lxi     h,l3stat        ;point to level 3 status
        res     1,m             ;clear DCE busy
        mov     a,c             ;release rx buffer
        call    rlsrxb          ;       /
        ret



;       process rx RNR packet
;       (internally called)
;       on entry:       <b>=octet 3             
;                       <c>=rx buffer #

rxrnr:  lxi     h,chstat        ;point to channel status
        bit     0,m             ;flow control ready state?
        jz      flush           ;no, discard packet
;
        call    chkpr           ;check P(r) & update window
        jc      dterst          ;local procedure error
;
        lxi     h,l3stat        ;point to level 3 status
        setb    1,m             ;signal DCE busy
        mov     a,c             ;release rx buffer
        call    rlsrxb          ;       /
        ret



;       process received reset indication
;       (internally called)
;       on entry:       <b>=octet 3             
;                       <c>=rx buffer #

rxrstr: lxi     h,chstat        ;point to channel status
        bit     3,m             ;reset request state (d2)?
        jnz     rxstr0          ;yes, accept packet
;
        bit     0,m             ;flow control ready state (d1)
        jnz     rxstr1          ;yes, accept packet
;
        jmp     flush           ;else discard packet
;
;       reset collision (para 4.4.3.3)
rxstr0:
        res     3,m             ;clear reset request state
        setb    0,m             ;set flow control ready state d1
        call    t22off          ;stop timer T22
        mov     a,c             ;get rx buffer #
        call    rlsrxb          ;release buffer
        ret                     ;and exit
;
;       DTE in flow control ready (d1) state
rxstr1:
        mov     a,c             ;release rx buffer
        call    rlsrxb          ;       /
        call    reset           ;reset flow control variables
        lxi     h,l3stat        ;point to level 3 status
        res     1,m             ;clear DCE busy status
        jmp     txrstc          ;and transmit reset confirmation



;       process received reset confirmation
;       (internally called)
;       on entry:       <b>=octet 3             
;                       <c>=rx buffer #

rxrstc: lxi     h,chstat        ;point to channel status
        bit     3,m             ;DTE reset request state?
        jz      flush           ;no, discard packet
;
;       DTE in reset request state (d2)
        res     3,m             ;clear state d2
        setb    0,m             ;set state d1
        call    t22off          ;stop timer T22
        mov     a,c             ;get rx buffer #
        call    rlsrxb          ;release buffer
        lxi     h,l3stat        ;point to level 3 status
        res     1,m             ;clear DCE busy status
        ret                     ;and exit
;


;       process received restart indication
;       (internally called)
;       on entry:       <b>=octet 3             
;                       <c>=rx buffer #

rxstar: lxi     h,chstat        ;point to channel status
        res     0,m             ;clear state d1 for now
        res     2,m             ;and state p2
        res     3,m             ;and state d2
        res     4,m             ;and state p6
        setb    5,m             ;set ready state p1
        lda     pvcmod          ;get pvc mode flag
        cpi     1               ;VC?
        jz      rstar1          ;yes, keep going
;
;       set flow control ready if in PVC mode
        res     5,m             ;reset ready state p1
        setb    0,m             ;and set flow control state d1
;
rstar1:
        mov     a,c             ;release buffer
        call    rlsrxb          ;       /
        bit     1,m             ;DTE restart request state (r2)?
        jz      rstar2          ;no, keep going
;
;       DTE in restart request state (r2)
        res     1,m             ;clear state r2
        call    t20off          ;stop timer T20
        ret                     ;and exit
;
;       DTE not in restart request state
rstar2:
        call    reset           ;reset flow control variables
        lxi     h,l4stat        ;point to level 4 status
        setb    0,m             ;signal console prompt

;***    do other things here?
;
        call    ilprt
        db      'L3: rx restart',cr,lf,0
        jmp     txstac          ;and transmit restart confirmation



;       process received restart confirmation
;       (internally called)
;       on entry:       <b>=octet 3             
;                       <c>=rx buffer #

rxstac: lxi     h,chstat        ;point to channel status
        bit     1,m             ;DTE restart request state?
        jz      flush           ;no, discard packet
;
;       DTE in restart request state (r2)
        res     0,m             ;clear state d1 for now
        res     1,m             ;and state r2
        res     2,m             ;and state p2
        res     3,m             ;and state d2
        res     4,m             ;and state p6
        setb    5,m             ;set ready state p1
        lda     pvcmod          ;get PVC mode flag
        cpi     1               ;VC?
        jz      rstac1          ;yes, keep going
;
;       set flow control ready if in PVC mode
        res     5,m             ;reset ready state p1
        setb    0,m             ;and set flow control state d1
;
rstac1:
        call    t20off          ;stop timer T20
        mov     a,c             ;get rx buffer #
        call    rlsrxb          ;release buffer
        lxi     h,l4stat        ;point to level 4 status
        setb    0,m             ;signal console prompt
        ret                     ;and exit
;



;       reset DTE

dterst:
        mov     a,c             ;release buffer
        call    rlsrxb          ;       /
        call    ilprt
        db      cr,lf,'L3: local procedure error - '
        db      'resetting channel',cr,lf,0
        call    reset           ;reset flow control variables
        jmp     txrstr          ;and transmit reset request






;       *************************************************
;       *  utility subroutines for receive section      *
;       *************************************************


;       discard data packet
;       (internally called)
;       on entry:       <c>=rx buffer #
;       on exit:        all regs, flags clobbered

flush:  
        mov     a,c             ;get rx buffer #
        call    rlsrxb          ;release it
        lhld    rxxpct          ;increment discarded packet count
        inx     h               ;       /
        shld    rxxpct          ;      /
        ret


;       acknowledge received data packet & update window
;       (internally called)
;       on entry:       <b>=octet 3 of data packet
;       on exit:        carry set if P(s) or P(r) sequence error
;                       <a>, other flags clobbered
;                       all other regs unchanged

ackd:   
        push    h               ;save regs
        push    d               ;       /
        push    b               ;      /
        call    chkpr           ;check P(r) and update window
        jc      ackdexi         ;exit with carry if invalid P(s)

;       check for valid rx P(s)
        mov     a,b             ;get octet 3
        ani     0000$1110b      ;extract rx P(s)
        rrc                     ;move to bits 0-2
        mov     c,a             ;save rx P(s) in <c>
;
;       check for P(s) next in sequence
        lda     lrxps           ;get last rx P(s)
        inr     a               ;bump mod 7
        ani     7               ;       /
        cmp     c               ;next in sequence?
        jnz     ackderr         ;no, invalid
;
;       calculate top of valid range for rx P(s)
        lda     pr              ;get local P(r)
        adi     wsize+1         ;add window size+1
        ani     7               ;mod 7
        mov     e,a             ;and save in <e>
;
;       check for rx P(s) within window
        lda     pr              ;bottom edge= local P(r)
ackd3:  cmp     e               ;past top of window?
        jz      ackderr         ;yes, error
;
        cmp     c               ;rx P(s) at bottom edge?
        jz      ackdok          ;yes, valid rx P(r)
;
        inr     a               ;else bump bottom edge
        ani     7               ;       /
        jmp     ackd3           ;and keep looping
;
;       valid rx P(s)
ackdok: mov     a,c             ;get rx P(s)
        sta     lrxps           ;update last rx P(s)
        inr     a               ;let P(r)=P(s)+1..
        ani     7               ;..mod 7
        sta     pr              ;and update P(r)
        stc                     ;clear carry bit
        cmc                     ;       /
        jmp     ackdexi         ;and exit
;
;       invalid  rx P(s)
ackderr:
        stc                     ;set carry bit

;       common exit
ackdexi:
        pop     b               ;restore regs
        pop     d               ;       /
        pop     h               ;      /
        ret


;       check received P(r) and update window if valid
;       (internally called)
;       on entry:       <b>=octet 3 of rx packet
;       on exit:        carry set if invalid rx P(r)
;                       <a>,other flags clobbered
;                       all other regs unchanged

chkpr:  push    h               ;save regs
        push    d               ;       /
        push    b               ;      /
        mov     a,b             ;get octet 3
        ani     1110$0000b      ;extract rx P(r)
        rrc                     ;move to bits 0-2
        rrc                     ;       /
        rrc                     ;      /
        rrc                     ;     /
        rrc                     ;    /
        mov     c,a             ;save rx P(r) in <c>
        lda     lastpr          ;get last rx P(r)
        cmp     c               ;same as this one?
        jz      chkexi          ;yes, nothing new
;
;       calculate top of valid range for rx P(r)
        lda     ps              ;get local P(s)
        inr     a               ;increment mod 7...
        inr     a               ;..past top of valid range
        ani     7               ;       /
        mov     e,a             ;save in <e>
;
;       check for rx P(r) within window
        lda     lastpr          ;bottom edge=last rx P(r)
chkpr1: cmp     e               ;past top of window?
        jz      chkerr          ;yes, invalid
        cmp     c               ;rx P(r) at bottom edge?
        jz      chkpr2          ;yes, valid
        inr     a               ;else bump bottom edge
        ani     7               ;mod 7
        jmp     chkpr1          ;and keep looping
;
;       valid rx P(r)
chkpr2: mov     a,c             ;get rx P(r)
        sta     lastpr          ;update lower window edge
        xra     a               ;clear carry flag
        jmp     chkexi          ;and exit
;
;       invalid rx P(r)
chkerr: stc                     ;set carry flag
;
;       common exit
chkexi: pop     b               ;restore regs
        pop     d               ;       /
        pop     h               ;      /
        ret


;       check for valid local DTE address in call request packet
;       (internally called)
;       on entry:       <hl>=bcb address
;                       <c>=rx buffer #
;       on exit:        carry set if address is invalid
;                       <a>,flags clobbered
;                       all other regs unchanged

chkcadr:
        push    b               ;save regs
        push    d               ;       /
        push    h               ;      /
        lxi     h,l3stat        ;point to level 3 status
        res     5,m             ;clear address error flag
        pop     h               ;restore bcb address
        lda     rxoc4           ;get rx address length octet
        mov     b,a             ;save in <b>
        call    getmsn          ;get high nibble
        sta     caddrl          ;store calling address length
        mov     a,b             ;get back octet 4
        ani     0000$1111b      ;get called address length
        mov     b,a             ;save it in <b>
        lda     laddrl          ;get our address length
        cmp     b               ;same?
        cnz     adderr          ;no, signal error
        lxi     d,laddr         ;<de> points to our address
        inr     b               ;bump received called address length
;
;       check to see if called address is ours
laddr1: dcr     b               ;last called address byte?
        jz      caddr1          ;yes, go read calling address
;
        call    getbuf          ;else get next address octet
        cc      adderr          ;signal error if not there
        mov     c,a             ;save octet in <c>
        call    getmsn          ;get high nibble
        xchg                    ;point to our address
        cmp     m               ;match?
        cnz     adderr          ;no, signal error
        xchg                    ;point back to bcb
        inx     d               ;bump pointers
        dcr     b               ;last called address byte?
        jz      caddr0          ;yes, go read calling address
;
        mov     a,c             ;get back octet
        ani     0000$1111b      ;get low nibble
        xchg                    ;point to our address
        cmp     m               ;match?
        cnz     adderr          ;no, signal error
        xchg                    ;point back to bcb
        inx     d               ;bump pointer
        jmp     laddr1          ;and look for next octet
;
;       read and store calling address if at high nibble
caddr0: lda     caddrl          ;get calling address length
        mov     b,a             ;save in <b>
        lxi     d,caddr         ;point to calling address buffer
        inr     b               ;bump received calling address length
        jmp     caddr3          ;and keep going
;
;       read and store calling address if at low nibble
caddr1: lda     caddrl          ;get calling address length
        mov     b,a             ;save in <b>
        lxi     d,caddr         ;point to calling address buffer
        inr     b               ;bump received calling address length
;
;       read address if at low nibble
caddr2: dcr     b               ;last address nibble?
        jz      cadexi          ;yes, exit
;
        call    getbuf          ;get next octet
        cc      adderr          ;else signal error
        mov     c,a             ;save in <c>
        call    getmsn          ;get high nibble
        stax    d               ;save at pointer location
        inx     d               ;bump pointers
;
;       read address if at high nibble
caddr3: dcr     b               ;last address nibble?
        jz      cadexi          ;yes, exit
;
        mov     a,c             ;else get back octet
        ani     0000$1111b      ;get low nibble
        stax    d               ;and save at pointer location
        inx     d               ;bump pointer
        jmp     caddr2          ;and get next octet
;
;       exit with carry set if error occured
cadexi: stc                     ;set carry flag
        push    h               ;save bcb
        lxi     h,l3stat        ;point to level 3 status
        bit     5,m             ;called address error?
        pop     h               ;restore bcb
        jnz     cadexi1         ;and exit with carry
;
        cmc                     ;else clear carry
cadexi1:
        pop     d               ;restore regs
        pop     b               ;       /
        ret


;       get high nibble of octet
;       (internally called)
;       on entry:       <a>=octet
;       on exit:        <a>=high nibble of octet
;                       all other regs unchanged

getmsn:
        ani     1111$0000b      ;strip out high nibble
        rrc                     ;and move to bits 0-4
        rrc                     ;       /
        rrc                     ;      /
        rrc                     ;     /
        ret


;       signal called DTE address error
;       (internally called)
;       on entry:       no parameters
;       on exit:        l3stat bit 5=1
;                       all regs unchanged
;
adderr: push    h               ;save <hl>
        lxi     h,l3stat        ;point to level 3 status
        setb    5,m             ;set address error bit
        pop     h               ;restore <hl>
        ret

;       check for valid addresses in call accepted packet
;       (internally called)
;       on entry:       <hl>=rx bcb address
;       on exit:        carry set if address error
;                       all other regs unchaged
chkadr:
;***    not implemented yet
        xra     a
        ret




;       *****************
;       *  data area    *
;       *****************

        dseg            ;data area

;       level 3 status indicators

l3stat: db      0       ;level 3 status flags
chstat: db      0       ;logical channel state

;       X.25 address and channel parameters
group   db      0       ;logical group number
chan    db      lchan   ;logical channel number
gfi     db      0       ;general format identifier
laddrl: db      0       ;local address length (# of hex chars)
raddrl: db      0       ;remote address length (# of hex chars)
caddrl: db      0       ;rx calling DTE address length
laddr:  ds      15      ;local DTE address (1 hex char/byte)
raddr:  ds      15      ;remote DTE address (1 hex char/byte)
caddr:  ds      15      ;rx calling DTE address (1 hex char/byte)
rxoc4:  db      0       ;received address length octet 4


;       X.25 packet flow control variables

pr      db      0       ;P(r)
ps      db      0       ;P(s)
lastpr  db      0       ;last rx P(r)
ltxpr   db      0       ;last tx P(r)
lrxps   db      7       ;last rx P(s)
ltxps   db      7       ;last tx P(s)
qbit    db      0       ;Q (qualifier) bit (bit 7)
dbit    db      0       ;D (delivery confirmation) bit (bit 6)
rtryct  db      n3      ;tx retry count
pvcmod  db      1       ;VC/PVC mode flag (1=VC,2=PVC)
packct  db      kpack   ;packet acknowledgement delay count

;       level 3 diagnostic counters
        
txpct:  dw      0       ;total tx packets
txfpct: dw      0       ;tx file data packets
txcpct: dw      0       ;tx console data packets
ntxbct: dw      0       ;errors due to no free tx bcb
;
rxpct:  dw      0       ;total rx packets
rxdpct: dw      0       ;total rx data packets
rbfpct: dw      0       ;total rx bad format packets
rbcpct: dw      0       ;total rx bad channel packets
rbapct: dw      0       ;total rx bad address packets
rbipct: dw      0       ;total rx bad id packets
rbgpct: dw      0       ;total rx bad group packets
rxxpct: dw      0       ;total discarded rx packets

