;                           P4TEST.ASM                  й Agner Fog 2004-01-17
;
comment %
This is a general program for testing how many clock cycles a piece of
code takes on a Pentium 4 microprocessor. It cannot be used for other 
microprocessors if the performance monitor counters are enabled.
 
Your piece of code is repeated several times and the number of clock cycles
is counted each time.

NOTE:
This program is intended to run in REAL MODE. This is only possible in 
Windows 98 and earlier versions of Windows and DOS. If you don't have
access to Windows 98 or earlier then you can only make tests in 16-bit
mode without performance monitor counters. Set WORDSIZE = 2 and 
USE_PERFORMANCE_COUNTERS = 0.

If you are running under DOS or 16-bit Windows then skip EMM386 or any other 
memory manager in CONFIG.SYS. If you are running under Windows 95 or 98 
then reboot your computer and press F8 while booting. Then select
"Safe mode command prompt only" (or "Command prompt only").

If the computer has a later version of Windows installed, then make a 
bootable floppy disk with Windows 98 system files; make an executable of
this test program (test.exe) and copy it to the floppy disk; reboot the
computer from the floppy; and run the test program. If you want to edit
and remake the test program while the computer is in this mode then you
need TASM version 3.0 and an ASCII editor for DOS. If you are making many
tests then it may be worth the effort to insert a Windows 98 bootable
hard disk in the computer or install a boot manager.

If you want to use performance monitor counters on Pentium 1 or Pentium MMX
then use p1test.asm. If you want to use performance monitor counters on 
Pentium 2 or Pentium 3 then use p23test.asm. There is no test program
supporting performance monitor counters on AMD processors.

Instructions:
-------------

User-definable areas are marked with stars ***.

Insert the piece of code you want to test after the line marked with:
"***   BEGIN INSTRUCTIONS TO TEST   ***"
Any necessary data or initializations may be inserted in the appropriately
marked spaces.

It is possible to test the same code in 16 bit real mode and 32 bit
protected mode: set WORDSIZE = 2 for 16 bit mode, or 4 for 32 bit mode.

You can set up to 6 event counters to count the number of uops etc.

Adjust OVERHC so that you get zero clock counts when there is nothing
in the INSTRUCTIONS TO TEST field.

It is not possible to output any data to the screen from the piece of code
in the INSTRUCTIONS TO TEST field. If you want to output any results from
your code then use CALL UOUT. This procedure stores the contents of the
EAX register to be output after return to 16-bit mode.

This test program works with TASM version 3 or later, or MASM version 5 or 
later. Use a linker that supports mixed 16-bit and 32-bit records.

й 2003 GNU General Public License www.gnu.org/copyleft/gpl.html
%
;#######################   SET OPTIONS   #######################

USE_PERFORMANCE_COUNTERS = 0    ; = 1 if you want to use performance counters

; set ITER = desired number of iterations
ITER = 12

; set NOUT = desired number of data output
NOUT = ITER

IFNDEF WORDSIZE
WORDSIZE = 4          ; 4 for 32-bit protected mode, 2 for 16-bit real mode
ENDIF

;=======================   DEFINE MACROS   =======================
; define macros for new instructions:

.486p
INCLUDE P4MACROS.ASI    ; define new instructions if not supported by assembler

IFDEF @version ; Microsoft assembler
  IF @version GE 611
    .686p
  ENDIF
ENDIF

; other macro definitions:

FNSTSWAX MACRO  ; FNSTSW AX. TASM 3.0 has a bug on this instruction
   DB 0DFH, 0E0H
ENDM

COMBINENAMES MACRO a, b, c
; combine texts or numbers b and c into a single name and get its value into a
a equ &b&c
ENDM

COMBINENAMES2 MACRO a, b, c
; combine texts or numbers a and b into a single name and give it the value c
&a&b equ c
ENDM

NCounters = 0  ; number of performance counters, incremented by macro below

; Set up performance monitor counter
; parameters:
; CounterNumber: performance counter number (0 - 11H)
; ESCRNumber: select which Event Selection Control Register to use (0 - 7)
; ESCRAddr: address of Event Selection Control Register  (3A0H - 3E1H)
; Event: event select (0 - 3FH)
; EventMask: (0 - FFFFH)
; Overhead: number to subtract from count because of overhead
; Text: a text to describe counter, max 11 char, enclosed in < >

SETCOUNTER MACRO CounterNumber, ESCRNumber, ESCRAddr, Event, EventMask, Overhead, Text
        tag     = 0001B   ; tag value
        xor     edx, edx
        mov     eax, 11100B OR (tag SHL 5) OR (EventMask SHL 9) OR (Event SHL 25)
        mov     ecx, ESCRAddr
        WRMSR             ; write ESCR
        mov     eax, (ESCRNumber SHL 13) OR (1 SHL 12) OR (3 SHL 16)
        mov     ecx, CounterNumber + 360H
        WRMSR             ; write CCCR, start counting
        xor     eax, eax
        mov     ecx, CounterNumber + 300H
        WRMSR             ; reset counter
; remember counter number
        COMBINENAMES2 Counter, %NCounters, CounterNumber    ; Counter0, etc. = CounterNumber
; remember counter overhead
        COMBINENAMES2 Adjust, %NCounters, Overhead    ; Adjust0, etc. = Overhead
; write text
        mov     ah, 2
        I = 12
        IRPC LETTER,<&Text>
        mov     dl,'&LETTER'
        int     21h                    ; write one character
        I = I - 1
        IF I EQ 0
          EXITM
        ENDIF
        ENDM
        mov     dl, ' '
        REPT I
        int     21H                    ; fill with spaces
        ENDM

        NCounters = NCounters + 1
ENDM

; define aliases for general registers of desired size:
IF WORDSIZE EQ 2
@AX     EQU     <AX>
@BX     EQU     <BX>
@CX     EQU     <CX>
@DX     EQU     <DX>
@SI     EQU     <SI>
@DI     EQU     <DI>
@BP     EQU     <BP>
@SP     EQU     <SP>
ELSE
@AX     EQU     <EAX>
@BX     EQU     <EBX>
@CX     EQU     <ECX>
@DX     EQU     <EDX>
@SI     EQU     <ESI>
@DI     EQU     <EDI>
@BP     EQU     <EBP>
@SP     EQU     <ESP>
ENDIF

WRITETEXT MACRO ttt                    ; write a text string
        mov     dx, offset ds:ttt
        mov     ah,9
        int     21h
ENDM


; =======================   SEGMENT DEFINITIONS   =======================
IF 1          ; 1 if you want code segment first

  CODE16 SEGMENT PARA PUBLIC USE16 'code' ; 16-bit code segment
  CODE16 ENDS

  IF WORDSIZE EQ 4
    CODE32  SEGMENT PAGE  PUBLIC USE32 'code' ; 32-bit code segment
    CODE32  ENDS
  ENDIF

ENDIF

; Stack segment
IF WORDSIZE EQ 4
  STAK    SEGMENT DWORD STACK USE32 'STACK'
ELSE
  STAK    SEGMENT DWORD STACK USE16 'STACK'
ENDIF
        DD      1024  DUP (?)     ; size of stack
STAKEND LABEL DWORD
STAK    ENDS

; Data segment
IF WORDSIZE EQ 4
  DATA    SEGMENT PARA PUBLIC USE32 'DATA'
ELSE
  DATA    SEGMENT PARA PUBLIC USE16 'DATA'
ENDIF

; global descriptor table
gdt             dd      0
                dd      0
code16_desc     dd      0000ffffh   ; 16-bit code segment
                dd      00009a00h
code32_desc     dd      0000ffffh   ; 32-bit code segment
                dd      004f9a00h
code32w_desc    dd      0000ffffh   ; write access to code segment
                dd      004f9200h
dgroup_desc     dd      0000ffffh   ; data segments
                dd      00cf9200h
gdt_size        =       $-gdt

gdt_label       label   fword
gdt_limit       dw      gdt_size-1
gdt_base        dd      ?
cr0copy         dd      ?
profamily       dd      ?       ; microprocessor family
profeatures     dd      ?       ; microprocessor features flag
espsave         dd      ?

; segment selectors
code16_s        =       code16_desc - gdt
code32_s        =       code32_desc - gdt
code32w_s       =       code32w_desc - gdt
dgroup_s        =       dgroup_desc - gdt

; program data:
num             dt   0  ; buffer for conversion of numbers to decimal
privilege       db   0  ; 1 if we have access to privileged instructions

; text strings
Heading         db 'Clocks      ',36
WrongProc       db 'Error: Not a Pentium 4 or later microprocessor.',13,10
                db 'Please disable performance counters or use another test program.',13,10
                db 'Family = '
fam1            db '0',13,10,36
WrongMode       db 'Warning: Microprocessor not in real mode.',13,10
                db 'Can only run in 16-bit mode without performance counters.',13,10
                db 'Please read instructions for how to get into real mode.',13,10,10,36
No32bitMode     db 'Error: Microprocessor not in real mode.',13,10
                db 'Can only run in 16-bit mode without performance counters.',13,10
                db 'Please specify WORDSIZE = 2 or read instructions for how '
                db 'to get into real mode.',13,10,36
NoPerformanceCounters db 'Error: No access to performance counters in this mode.',13,10
                db 'Please set USE_PERFORMANCE_COUNTERS = 0 or '
                db 'run in real mode',13,10,36
UHead           db 'Additional user data:',13,10,36

; test data
ALIGN 8
tics            dd ?             ; count clock cycles
counter         dd 0             ; loop counter

NDAT = ITER         ; number of data output lines
IF NOUT GT ITER
NDAT = NOUT
ENDIF
LISTLEN = NDAT+1    ; length of PMCLIST lists

MAXPMC = 6    ; max number of performance monitor counters

; storage for output data:
CLOCKLIST       dd LISTLEN dup (0) ; clock cycles
PMCLIST         dd MAXPMC*LISTLEN dup (0) ; PMC data
USERLIST        dd 100  dup (0) ; any user data
NUSER           dd 0            ; number of data from UOUT calls

align 16
;###################   USER DATA BEGIN   ##############################
; put any data here that your test code needs


align 16
d0      dd      0
d1      dd      0
d2      dd      0
d3      dd      0
d4      dd      0
d5      dd      0
        db      4096 dup (0)

; ====================   USER DATA END     ====================

@curseg ENDS

; define code segments
CODE16 SEGMENT PARA PUBLIC USE16 'code'
CODE16 ENDS

IF WORDSIZE EQ 4
CODE32  SEGMENT PAGE  PUBLIC USE32 'code'
CODE32  ENDS
ENDIF

DGROUP  GROUP DATA,STAK

; ====================   16 BIT CODE SEGMENT   ====================
CODE16 SEGMENT PARA PUBLIC USE16 'code'
ASSUME cs:code16,ds:dgroup,ss:dgroup

START:
        MOV     AX,DGROUP
        MOV     DS,AX
        MOV     SS,AX
        MOV     SP,OFFSET DGROUP:STAKEND

; test processor type
        call    PROCESSORTYPE
        ADD     [FAM1],AL
        mov     [profamily], eax
        mov     [profeatures], edx
        CMP     AX,0Fh
        JNB     SHORT L_2
L_1:    
IF USE_PERFORMANCE_COUNTERS NE 0
        WRITETEXT WrongProc            ; not a Pentium 4. Performance counters not compatible
        MOV     AX,4C02H
        INT     21H                    ; exit
ENDIF        

L_2:    CALL    PROCESSORMODE          ; test if processor is in real mode
        MOV     [privilege], 1
        TEST    AX,AX
        JZ      SHORT L_3
        MOV     [privilege], 0         ; we have no access to privileged instructions
IF  WORDSIZE EQ 2
        WRITETEXT WrongMode            ; not in real mode, write warning
IF USE_PERFORMANCE_COUNTERS NE 0
        WRITETEXT NoPerformanceCounters ; trying to use performance counters, write error message
        MOV     AX,4C01H
        INT     21H                    ; exit
ENDIF
ELSE
        WRITETEXT No32bitMode          ; not in real mode, write error
        MOV     AX,4C01H
        INT     21H                    ; exit
ENDIF
L_3:    ; processortype and mode OK
        test    [profeatures], 1 shl 25
        jz      L_5                    ; xmm instructions not supported
        cmp     [privilege], 0
        je      L_5
        ; enable xmm instructions and performance monitor counters
        DB      0FH,20H,0E0H           ; MOV EAX, CR4
        OR      EAX, (1 SHL 9) or (1 SHL 8)
        DB      0FH,22H,0E0H           ; MOV CR4, EAX
        xor     eax, eax
        cpuid                          ; check if this is an Intel processor
        cmp     ch, 't'
        jne     L_5                    ; skip if not Intel
        mov     ecx, 3f1h              ; IA32_PEBS_ENABLE register
        rdmsr
        or      eax, (1 shl 24) or (1 shl 25)
        wrmsr
        
L_5:    WRITETEXT Heading              ; write heading

; ##################   SET UP EVENT COUNTERS   ##########################

IF USE_PERFORMANCE_COUNTERS  NE 0
;
; Here you can decide which performance counters to use (max 6).
; You cannot have two counters with the same CounterNumber or ESCRAddr.
; When you add or remove a counter, you have to adjust all Overheads.

; SETCOUNTER CounterNumber, ESCRNumber, ESCRAddr, Event, EventMask, Overhead

;SETCOUNTER 4, 0, 3c0h, 9, 7, 384, <uops queued>   ; uops queued from any source
SETCOUNTER 4, 0, 3c0h, 9, 2, 35, <uops TC>   ; uops from trace cache
;SETCOUNTER 6, 0, 3c1h, 9, 1, 0, <uops build>   ; uops from TC build
;SETCOUNTER 6, 0, 3c1h, 9, 4, 243, <microcode>   ; uops from microcode
;SETCOUNTER 12, 4, 3b8h, 1, 1, 0, <uops retir>   ; non-bogus uops retired
SETCOUNTER 15, 4, 3b9h, 02h, 0ch, 0, <bogus uops>   ; bogus uops

;SETCOUNTER 0, 2, 3aah, 3, 3aH , 0, <replaymob>   ; load replay

;SETCOUNTER 8, 1, 3a4h, 4, 8000H , 0, <FP uops>   ; FP uops
;SETCOUNTER 9, 1, 3a4h, 2eh, 8H , 2, <FPmov P0>   ; FPmov Port 0
;SETCOUNTER 10, 1, 3a5h, 2eh, 10H , 0, <FPload P2>   ; FPload Port 2
;SETCOUNTER 11, 1, 3a5h, 2h, 8000H , 0, <mmx 64bit>   ; 64-bit MMX uops
;SETCOUNTER 8, 1, 3a4h, 1ah, 8000H , 0, <xmm 128bit>   ; 128-bit MMX uops

;SETCOUNTER 12, 5, 3cch, 6, 5, 0, <predicted>     ; predicted branches
SETCOUNTER 14, 5, 3cdh, 6, 0ah, 0, <mispred.>   ; mispredicted branches
;SETCOUNTER 14, 5, 3cdh, 6, 0ch, 0, <taken>       ; taken branches

ENDIF  ; USE_PERFORMANCE_COUNTERS

; overhead to subtract from clock counter
OVERHC = 644

; =====================================================================

L_6:    CALL    NEWLINE

IF NCounters GT 0
        cmp     [privilege],0   ; check if we have access to performance counters
        jne     L_7
        WRITETEXT NoPerformanceCounters     ; no access to performance counters
        MOV     AX,4C01H
        INT     21H                    ; exit
L_7:
ENDIF
        
IF WORDSIZE EQ 4
; setup global descriptor table
        XOR     EAX, EAX
        MOV     AX, DGROUP
        SHL     EAX, 4
        XOR     EBX, EBX
        MOV     BX, OFFSET DGROUP:GDT
        ADD     EAX, EBX
        MOV     GDT_BASE, EAX

; runtime fixup of segment descriptors
; code32
        XOR     EAX, EAX
        MOV     AX, CODE32
        SHL     EAX, 4
        MOV     WORD PTR CODE32_DESC+2, AX
        MOV     WORD PTR CODE32W_DESC+2, AX
        SHR     EAX, 16
        MOV     BYTE PTR CODE32_DESC+4, AL
        MOV     BYTE PTR CODE32W_DESC+4, AL
; code16
        XOR     EAX, EAX
        MOV     AX, CODE16
        SHL     EAX, 4
        MOV     WORD PTR CODE16_DESC+2, AX
        SHR     EAX, 16
        MOV     BYTE PTR CODE16_DESC+4, AL
; dgroup
        XOR     EAX, EAX
        MOV     AX, DGROUP
        SHL     EAX, 4
        MOV     WORD PTR DGROUP_DESC+2, AX
        SHR     EAX, 16
        MOV     BYTE PTR DGROUP_DESC+4, AL

; switch to protected mode
        AND     ESP, 0FFFFH
        MOV     [espsave], ESP
        CLI
        DB      66H
        LGDT    GDT_LABEL              ; load global descriptor table
        MOV     EAX, CR0
        MOV     [CR0COPY], EAX
        OR      AL, 1
        AND     EAX, 7fffffffh
        MOV     CR0, EAX
        DB      66H, 0EAH              ; JMP FAR PTR PROTECTED_CODE
        DD      OFFSET CODE32:PROTECTED_CODE
        DW      CODE32_S

; return from protected mode:
ret_real:
        MOV     EAX, [CR0COPY]
        AND     AL, NOT 1
        MOV     CR0, EAX               ; return to real mode
        DB      0EAH                   ; JMP FAR PTR S16
        DW      OFFSET CODE16:S16
        DW      CODE16
        
S16:    MOV     AX, DGROUP             ; restore segment registers and stack pointer
        MOV     DS, AX
        MOV     SS, AX
        MOV     SP, OFFSET DGROUP:STAKEND
        STI
        
ELSE    ; run in 16-bit mode:
        CLI
        JMP     PROTECTED_CODE
WRITERES:
        STI
ENDIF

; stop all performance counters
        cmp     [privilege], 0
        je      L_8            ; no access to performance counters
        mov     ebx, 360H
        xor     eax, eax
        xor     edx, edx
RESETCOUNTERS: mov  ecx, ebx
        WRMSR                  ; reset counter control register
        sub     ecx, 60H
        WRMSR                  ; reset counter register
        inc     ebx
        cmp     ebx, 371H
        jna     RESETCOUNTERS
L_8:
; write results
        xor     edi, edi        
NEXTRESULT: ; loop through result lists

        ; write clock count
        MOV     EAX, CLOCKLIST[EDI]
        CALL    WRITENUM

        ; write performance counters
        I       = 0
        REPT    NCounters       ; macro loop for all performance counters
        MOV     DL, ' '
        MOV     AH, 2
        INT     21H                    ; write a space
        MOV     eax, [PMCLIST + I*LISTLEN*4][EDI]  ; performance count
        CALL    writenum               ; write number
        I       = I + 1
        ENDM

        CALL    NEWLINE
        ADD     EDI, 4
        CMP     EDI, SIZE CLOCKLIST - 4
        JB      NEXTRESULT
        
; loop through user list:        
        XOR     ESI, ESI
        CMP     ESI, [NUSER]
        JE      S20
        WRITETEXT UHead
S18:    CMP     ESI, [NUSER]
        JNB     S19
        MOV     EAX, USERLIST[ESI*4]
        CALL    WRITEHEX
        MOV     DL, ' '
        MOV     AH, 2
        INT     21H                    ; write spaces
        MOV     AH, 2
        INT     21H
        INC     ESI
        JMP     S18
S19:    CALL    NEWLINE

S20:    ; exit
        finit
        xor     ebp, ebp               ; high dword must be 0
        mov     ax, 4C00H
        int     21H                    ; exit

; subroutines (16 bit mode):

PROCESSORTYPE PROC NEAR  ; detects microprocessor type
; return value: AX = family, DX = feature flags if CPUID instruction supported
        XOR     AX, AX
        XOR     DX, DX
        PUSH    SP                     ; TEST IF 8088
        POP     BX
        CMP     BX, SP
        JE      SHORT PT1
PT0:    RET
PT1:    MOV     AL, 2
        PUSH    0F200H                 ; test if 80286
        POPF
        PUSHF
        POP     BX
        TEST    BX, 0F000H
        JZ      PT0
        INC     AX                     ; test if 80386
        PUSHFD
        POP     EBX
        MOV     ECX, EBX
        xor     ecx, 40000h            ; flip AC bit in EFLAGS
        push    ecx
        popfd
        pushfd
        pop     ecx
        xor     ecx, ebx
        jz      PT0                    ; can't toggle AC bit, processor=80386
        inc     ax                     ; test if CPUID supported
        mov     ecx, ebx
        xor     ecx, 200000h           ; flip ID bit in EFLAGS
        push    ecx
        popfd
        pushfd
        pop     ecx
        xor     ecx, ebx
        jz      PT9                    ; can't toggle ID bit, processor=80486
        push    esi
        push    edi
        xor     eax, eax
        CPUID
        xor     edx, edx
        cmp     eax, 1
        mov     eax, 4
        jl      short PT9              ; CPUID level 1 not supported
        mov     eax, 1
        CPUID
        shr     eax, 8
        and     eax, 0fh               ; isolate family number
PT9:    pop     edi
        pop     esi
        ret
PROCESSORTYPE ENDP

PROCESSORMODE PROC NEAR                ; tests if processor is in real mode
; return value AX:  0 = real mode, 
;                   1 = protected or enhanced virtual mode
;                   2 or 3 = virtual mode
        pushfd
        pop     ebx
        shr     ebx, 16
        and     bx, 2                  ; isolate VM flag
        mov     al, 1                  ; set bit in case we can't read CR0
        smsw    ax                     ; read lower part of CR0
        and     eax, 1                 ; isolate protection bit
        or      ax, bx
        ret
PROCESSORMODE ENDP

WRITENUM PROC NEAR  ; writes number in EAX as decimal signed integer
        push    si
        push    di
        xor     di, di
        test    eax, eax
        jns     short WN1
        neg     eax                    ; negative
        push    eax
        mov     dl,'-'
        mov     ah, 2
        int     21h                    ; write '-'
        inc     di
        pop     eax
WN1:    mov     si, offset ds:NUM      ; 10 byte temp. buffer
        xor     ecx, ecx
        mov     [si], eax
        mov     [si+4], ecx
        fild    qword ptr [si]
        fbstp   tbyte ptr [si]         ; convert to packed BCD
        add     si, 6
        std
        wait
WN2:    lodsb                          ; read two BCD digits
        mov     ah, al                 ; unpack two BCD digits
        and     al, 0fh
        shr     ah, 4
        mov     dl, ah
        or      ah, cl
        jz      short WN3              ; skip leading zeros
        inc     cx
        add     dl, '0'
        push    ax
        mov     ah, 2
        int     21h                    ; print digit
        inc     di
        pop     ax
WN3:    mov     dl, al
        or      al, cl
        jz      short WN4              ; skip leading zeros
        inc     cx
        add     dl, '0'
        mov     ah, 2
        int     21h                    ; print digit
        inc     di
WN4:    cmp     si, offset ds:NUM
        jnb     WN2                    ; next two digits
        test    cl, cl
        jnz     short WN5
        mov     dl, '0'                ; no digits have been written
        mov     ah, 2
        int     21h                    ; write a '0'
        inc     di
WN5:    cmp     di, 11
        jnb     WN9
        mov     dl, ' '                ; fill with spaces
        mov     ah, 2
        int     21h                    ; write a ' '
        inc     di
        jmp     WN5
WN9:    cld
        pop     di
        pop     si
        ret
WRITENUM        ENDP

WRITEHEX PROC NEAR                      ; writes EAX as hexadecimal number
        PUSH    EBX
        mov     bx, 8
WDL:    rol     eax, 4
        push    bx
        push    eax
        call    writeal
        pop     eax
        pop     bx
        dec     bx
        jnz     wdl
        POP     EBX
        ret
WRITEHEX ENDP

WRITEAL PROC NEAR                     ; writes AL low nibble as Hex digit
        and al, 0fh
        add al, '0'
        cmp al, '9'
        jna wrh2
        add al, 'A'-'9'-1
        wrh2:
        mov dl, al
        mov ah, 2
        int 21h
        ret
WRITEAL ENDP

NEWLINE PROC NEAR                      ; write a linefeed
        MOV     DL, 13
        MOV     AH, 2
        INT     21H                    ; carriage return
        MOV     DL, 10
        MOV     AH, 2
        INT     21H                    ; linefeed
        RET
NEWLINE ENDP

; procedure to output additional data EAX (from 16 bit mode)
UOUT16  PROC    NEAR
        PUSH    EBX
        MOV     EBX, [NUSER]
        CMP     EBX, 100
        JNB     UO169
        MOV     USERLIST[EBX*4], EAX
        INC     EBX
        MOV     [NUSER], EBX
UO169:  POP     EBX
        RET
UOUT16  ENDP

; =======================   32 BIT CODE SEGMENT   =======================
IF WORDSIZE EQ 4
CODE16  ENDS

CODE32  SEGMENT PAGE  PUBLIC USE32 'CODE'
ASSUME  cs:code32,ds:dgroup,es:dgroup,ss:dgroup
ENDIF

; user procedure to output additional data EAX
UOUT    PROC    NEAR
        PUSH    EBX
        MOV     EBX, [NUSER]
        CMP     EBX, 100
        JNB     UO9
        MOV     USERLIST[EBX*4], EAX
        INC     EBX
        MOV     [NUSER], EBX
UO9:    POP     EBX
        RET
UOUT    ENDP

protected_code:
IF WORDSIZE EQ 4
        mov     ax, dgroup_s    ; data segments
        mov     ds, ax
        mov     es, ax
        mov     ss, ax
        mov     bx, code32w_s   ; FS only needed for self-modifying code
        mov     fs, bx
ENDIF

;###################   BEGIN USER INITIALIZATIONS 1   #######################
; insert any initializations here that you want to run only once:

        FINIT              ; initialize floating point registers
        CLD

;       pxor xmm0,xmm0     ; initialize XMM registers
;       pxor xmm1,xmm1
;       pxor xmm2,xmm2
;       pxor xmm3,xmm3
;       pxor xmm4,xmm4
;       pxor xmm5,xmm5
;       pxor xmm6,xmm6
;       pxor xmm7,xmm7

;#######################   END USER INITIALIZATIONS 1   ######################
             
ALIGN   16
AGAIN:  ; top of loop

;################   BEGIN USER INITIALIZATIONS 2 ############################
; insert any initializations here that you want to run for each iteration:

        mov esi, offset ds:d0


;=======================   END USER INITIALIZATIONS 2   =======================

        ; start to count clock cycles:
        XOR     EAX, EAX
        CPUID                   ; serialize
        MOV     ebx, [COUNTER]
        
        I       = 0
        REPT    NCounters       ; macro loop for all performance counters
        COMBINENAMES cn, Counter, %I  ; cn = Counter0, etc.
        mov     ecx, cn OR (1 SHL 31)
        RDPMC                   ; read counter
        ; Note: if you use WRMSR to reset the counters here then the test
        ; instructions will not go into the trace cache!
        mov     [PMCLIST+I*LISTLEN*4+4][ebx], eax  ; store counter value in list
        I       = I + 1
        ENDM

        RDTSC                   ; read clock counter
        mov     [TICS], eax

        XOR     EAX, EAX
        CPUID                   ; serialize again

; #####################   BEGIN INSTRUCTIONS TO TEST   ######################
; insert the instructions to test here:
; лл


REPT 10                 ; replace this example with your own code
        shr eax,1
ENDM



; ######################    END INSTRUCTIONS TO TEST   ######################
        ; serialize
        XOR     EAX, EAX
        CPUID
        
        ; read clock
        RDTSC       
        MOV     EDI, [COUNTER]
        MOV     CLOCKLIST[EDI], EAX   ; store clock count in table

        ; read performance counters
        I       = 0
        REPT    NCounters       ; macro loop for all performance counters
        COMBINENAMES cn, Counter, %I  ; cn = Counter0, etc.
        mov     ecx, cn OR (1 SHL 31)
        RDPMC                   ; read counter
        mov     [PMCLIST+I*LISTLEN*4][EDI], EAX  ; save count in list
        I       = I + 1
        ENDM

        ; serialize again
        XOR     EAX, EAX
        CPUID      

        ; subtract overhead from all counters
        I       = 0             
        REPT    NCounters       ; macro loop for all performance counters
        COMBINENAMES ai, Adjust, %I    ; ai = Adjust0, Adjust1, ...
        mov     eax, [PMCLIST+I*LISTLEN*4+4][EDI] ; previous count
        add     eax, ai   ; overhead for performance count
        sub     [PMCLIST+I*LISTLEN*4][EDI], eax ; subtract previous count and overhead
        I       = I + 1
        ENDM

        mov     EAX, [TICS]
        add     EAX, OVERHC ; subtract the number of clocks used by surrounding code
        sub     CLOCKLIST[EDI], EAX   ; subtract overhead from clock count

        ; repetion loop ends here
        MOV     ESP, espsave
        ADD     EDI, TYPE CLOCKLIST
        MOV     [COUNTER], EDI
        CMP     EDI, ITER * (TYPE CLOCKLIST)

IF WORDSIZE EQ 4

IF 0
        JB      AGAIN
ELSE
; conditional jump replaced by conditional move to avoid interfering
; with branch prediction tests (and to avoid bug in MASM 5.10):
        MOV      EAX, OFFSET CS:AGAIN
        MOV      EBX, OFFSET CS:FINISHTEST
        CMOVNB   EAX, EBX
        JMP      EAX
ENDIF

FINISHTEST:

; return to 16 bit mode
        DB      66H, 0EAH              ; hardcode jmp far ptr ret_real
        DW      OFFSET CODE16:RET_REAL
        DW      CODE16_S, 0
ELSE  ; 16 bit mode:
        JB      AGAIN
        JMP     WRITERES
ENDIF 

@CURSEG ENDS

END     START
