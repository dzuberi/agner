;                           64TEST.ASM                  й Agner Fog 2005-06-04
;
comment %
This is a general program for testing how many clock cycles a piece of
code takes on a 64-bit microprocessor in 64 bit mode.
The performance monitor counters can be used only for Intel processors.
 
Your piece of code is repeated several times and the number of clock cycles
is counted each time.

NOTE:
This program is intended to run in REAL MODE. This is only possible in 
Windows 98 and earlier versions of Windows and DOS. 

To get into REAL MODE you must boot the computer from a floppy disk or
an USB memory stick or a hard disk with Windows 98 or earlier.
If you are booting from a hard disk (Windows 98) then press F8 while
booting and select "Safe mode command prompt only" (or "Command prompt only").
Make sure you don't have EMM386 or any other memory manager in the 
config.sys file.

To test in 16 or 32 bit mode use p4test.asm

Instructions:
-------------

User-definable areas are marked with  ###.

Insert the piece of code you want to test after the line marked with:
"###   BEGIN INSTRUCTIONS TO TEST   ###"
Any necessary data or initializations may be inserted in the appropriately
marked spaces.

You can set up to 6 event counters to count the number of uops etc.

Adjust OVERHC so that you get zero clock counts when there is nothing
in the INSTRUCTIONS TO TEST field.

It is not possible to output any data to the screen from the piece of code
in the INSTRUCTIONS TO TEST field. If you want to output any results from
your code then use CALL UOUT. This procedure stores the contents of the
EAX register to be output after return to 16-bit mode.

This test program works with TASM version 3 or later, or MASM version 5 or 
later. Use a linker that supports mixed 16-bit and 32-bit records.

It is useful to use an editor that works under DOS.

Note that the DOS assembler doesn't support 64 bit mode. The 64 bit code
appears to the assembler to be 32 bit code. This means that any instruction
with a 64 bit code that differs from the 32 bit code must be hand-coded.
This applies to all instructions with 64 bit integer operands, all instructions
using r8-r15 and xmm8-xmm15, and inc register and dec register.

For example,  ADD RAX,RBX  can be coded as:
        db    REXW
        add   eax,ebx

Data in the 16 bit data segment can be accessed through RBP as pointer.
For example, to read D0 use:
        mov   eax,[ebp][D0-BP0]

Data in the 64 bit segment can be accessed through rip-relative addressing.
Use a label after the instruction as reference. For example, to read E0 use:
        mov   ebx,ds:[E0-L1]
L1:

Don't use eax in the above example, as this will give you 64-bit absolute
addressing.

й 2005 GNU General Public License www.gnu.org/copyleft/gpl.html
%

;===============   DEFINE PREFIXES FOR 64 BIT MODE   ================
; The following constants can be used for hand-coding REX prefixes.
; The values can be ORed.

REXW = 48H    ; 64 bit operand size
REXR = 44H    ; change first register
REXX = 42H    ; change index register
REXB = 41H    ; change second register or base register

; ######################   SET OPTIONS   #######################

USE_PERFORMANCE_COUNTERS = 0    ; = 1 if you want to use performance counters

; set ITER = desired number of iterations
ITER = 16

; set NOUT = desired number of data output
NOUT = ITER

; ######################   END OF OPTIONS   ####################


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

WRITETEXT MACRO ttt                    ; write a text string
        mov     dx, offset ds:ttt
        mov     ah,9
        int     21h
ENDM


; =======================   SEGMENT DEFINITIONS   =======================
IF 1          ; 1 if you want code segment first

  CODE16 SEGMENT PARA PUBLIC USE16 'code' ; 16-bit code segment
  CODE16 ENDS

  CODE64  SEGMENT PAGE  PUBLIC USE32 'code' ; 64-bit code segment
  CODE64  ENDS

ENDIF

; Stack segment
STAK    SEGMENT DWORD STACK USE32 'STACK'
        DD      1024  DUP (?)     ; size of stack
STAKEND LABEL DWORD
STAK    ENDS

; Data segment
DATA    SEGMENT PARA PUBLIC USE16 'DATA'

; page translation tables, long mode 2 Mb tables, identity mapping of 1 Gb:
PAGE_TABLES     db      4000h dup (?)
; PML4E table is at first physical address divisible by 1000h hereafter
; PDPE  table is at PML4E + 1000h
; PDE   table is at PDPE  + 1000h

PAGEMAP         dd      0           ; physical address of PML4E

; global descriptor table
gdt             dd      0
                dd      0
code16_desc     dd      0000ffffh   ; 16-bit code segment
                dd      00009a00h
code64_desc     dd      0000ffffh   ; 64-bit code segment
                dd      002f9a00h                
code32_desc     dd      0000ffffh   ; 32-bit code segment
                dd      004f9a00h
code32w_desc    dd      0000ffffh   ; write access to code segment
                dd      004f9200h
dgroup_desc     dd      0000ffffh   ; data segments
                dd      00cf9200h
gdt_size        =       $-gdt

gdt_label       label   fword
gdt_limit       dw      gdt_size-1
gdt_base        dd      0, 0

; segment selectors
code16_s        =       code16_desc - gdt
code64_s        =       code64_desc - gdt
code32_s        =       code32_desc - gdt
code32w_s       =       code32w_desc - gdt
dgroup_s        =       dgroup_desc - gdt

; system data
cr0copy         dd      ?       ; save copy of cr0
cr3copy         dd      ?       ; save copy of cr3
cr4copy         dd      ?       ; save copy of cr4
profamily       dd      ?       ; byte 0 = microprocessor family, byte 1 = 64 bit
profeatures     dd      ?       ; microprocessor features flag
espsave         dd      ?       ; save stack pointer

; jump address to ENTRY_CODE64
jump64          dd      ?
                dd      code64_s
                
; jump address to RetTo16
jump16          dw      offset code16:RetTo16, 0
                dw      code16_s, 0

; program data:
num             dt   0  ; buffer for conversion of numbers to decimal
privilege       db   0  ; 1 if we have access to privileged instructions

; text strings
Heading         db 'Clocks      ',36
WrongProc       db 'Error: Not a Pentium 4 or later microprocessor.',13,10
                db 'Please disable performance counters or use another test program.',13,10
                db 'Family = '
fam1            db '0',13,10,36
No64Support     db 'Error: Microprocessor does not support 64 bit mode.',13,10,36
WrongMode       db 'Warning: Microprocessor not in real mode.',13,10
                db 'Can only run in 16-bit mode without performance counters.',13,10
                db 'Please read instructions for how to get into real mode.',13,10,10,36
NotRealMode     db 'Error: Microprocessor not in real mode.',13,10
                db 'Can only run in 16-bit mode without performance counters.',13,10
                db 'Please read instructions for how '
                db 'to get into real mode.',13,10,36
NoPerformanceCounters db 'Error: No access to performance counters in this mode.',13,10
                db 'Please set USE_PERFORMANCE_COUNTERS = 0 or '
                db 'run in real mode',13,10,36
UHead           db 'Additional user data:',13,10,36

checkout          db 'check',13,10,36

; test data
ALIGN 8

; define an arbitrary reference point for addressing data from 64 bit mode
; without RIP relative addressing. RBP will point to this place:
BP0             label dword

tics            dd ?             ; count clock cycles
counter         dd 0             ; loop counter

NDAT = ITER         ; number of data output lines
IF NOUT GT ITER
NDAT = NOUT
ENDIF
LISTLEN = NDAT+1    ; length of PMCLIST lists

MAXPMC = 6          ; max number of performance monitor counters

; storage for output data:
CLOCKLIST       dd LISTLEN dup (0)        ; clock cycles
PMCLIST         dd MAXPMC*LISTLEN dup (0) ; PMC data
USERLIST        dd 100  dup (0)           ; any user data
NUSER           dd 0                      ; number of data from UOUT calls

align 16
; ##################   USER DATA BEGIN   ##############################

; put any data here that your test code needs

align 16
d0      dd      0
d1      dd      0
d2      dd      0
d3      dd      0
d4      dd      0
d5      dd      0
        db      4096 dup (0)

; ##################   USER DATA END     ##############################

@curseg ENDS

; define code segments
CODE16 SEGMENT PARA PUBLIC USE16 'code'
CODE16 ENDS

CODE64  SEGMENT PAGE  PUBLIC USE32 'code'
CODE64  ENDS

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
        add     [FAM1],al
        mov     [profamily], eax
        mov     [profeatures], edx
        cmp     al,0Fh
        JNB     SHORT L_2
L_1:    
IF USE_PERFORMANCE_COUNTERS NE 0
        WRITETEXT WrongProc            ; not a Pentium 4. Performance counters not compatible
        MOV     AX,4C02H
        INT     21H                    ; exit
ENDIF        

L_2:    test    ah, ah
        jnz     short L_3
        WRITETEXT No64Support          ; not a 64 bit processor
        MOV     AX,4C02H
        INT     21H                    ; exit
L_3:    CALL    PROCESSORMODE          ; test if processor is in real mode
        MOV     [privilege], 1
        TEST    AX,AX
        JZ      SHORT L_4
        MOV     [privilege], 0         ; we have no access to privileged instructions
        WRITETEXT NotRealMode          ; not in real mode, write error
        MOV     AX,4C01H
        INT     21H                    ; exit
L_4:    ; processortype and mode OK
        test    [profeatures], 1 shl 25
        jz      L_5                    ; xmm instructions not supported
        cmp     [privilege], 0
        je      L_5
        ; enable xmm instructions and performance monitor counters
        db      0fh,20h,0e0h           ; mov eax, cr4
        or      eax, (1 shl 9) OR (1 shl 8)
        db      0fh,22h,0e0h           ; mov cr4, eax
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
SETCOUNTER 6, 0, 3c1h, 9, 4, 243, <microcode>   ; uops from microcode
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

; overhead to subtract from clock counter,
; adjust this to get 0 when test code empty:

OVERHC = 107

; ##################   END OF EVENT COUNTERS   ##########################


L_6:    CALL    NEWLINE

; setup global descriptor table
        XOR     EAX, EAX
        MOV     AX, DGROUP
        SHL     EAX, 4
        XOR     EBX, EBX
        MOV     BX, OFFSET DGROUP:GDT
        ADD     EAX, EBX
        MOV     GDT_BASE, EAX

; runtime fixup of segment descriptors
; code64 needs no fixup
; code32
        XOR     EAX, EAX
        MOV     AX, CODE64
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
        SHL     EAX, 4               ; physical address of DGROUP
        mov     ebx, eax
        MOV     WORD PTR DGROUP_DESC+2, AX
        SHR     EAX, 16
        MOV     BYTE PTR DGROUP_DESC+4, AL

; set up page maps
        xor     edi, edi
        mov     di, offset dgroup:PAGE_TABLES
        add     edi, ebx             ; physical address of PAGE_TABLES
        add     edi, 1000h
        and     edi, -1000h          ; round up to nearest divisible by 1000h
        mov     [PAGEMAP], edi       ; physical address of PML4E
        mov     eax, edi
        sub     edi, ebx             ; relative address of PML4E
        add     eax, 1000h           ; physical address of PDPE
        or      eax, 7               ; set bits P, R/W, U/S 
        xor     edx, edx
        mov     [edi], eax           ; write PML4E, points to PDPE
        mov     [edi+4], edx
        add     eax, 1000h           ; point to PDE
        add     edi, 1000h           ; relative address of PDPE
        mov     [edi], eax           ; write PDPE, points to PDP
        mov     [edi+4], edx
        mov     eax, 87h             ; set PS bit to 1 indicating 2Mb pages, base address = 0
        add     edi, 1000h           ; relative address of PDE
        mov     cx, 200h             ; number of PDE entries
L_8:    mov     [edi], eax           ; write PDE, points to memory
        mov     [edi+4], edx
        add     eax, 1 shl 21        ; point to next 2Mb page
        add     edi, 8               ; next PDE entry
        loop    L_8

; switch to protected mode
        cli
        and     esp, 0ffffh
        mov     [espsave], esp
        db      66H
        lgdt    gdt_label              ; load global descriptor table
        mov     eax, cr0
        mov     [cr0copy], eax
        or      eax, 1
        and     eax, not 80000000h
        mov     cr0, eax
        db      0EAH                   ; jmp far ptr PROTECTED_CODE16
        dw      offset CODE16:PROTECTED_CODE16
        dw      CODE16_S

; 16-bit protected mode:
PROTECTED_CODE16:
; set up stack
        mov     ax, dgroup_s
        mov     ss, ax
        mov     ds, ax
        mov     es, ax
; calculate data reference point
        xor     ebp, ebp
        mov     bp, dgroup
        shl     ebp, 4
        add     ebp, offset dgroup:BP0  ; physical address of BP0
; calculate jump address
        xor     eax, eax
        mov     ax, code64
        shl     eax, 4
        add     eax, offset code64:ENTRY_CODE64
        mov     [jump64], eax
; enable page translation        
        DB      0FH,20H,0E0H           ; MOV EAX, CR4
        mov     [cr4copy], eax
        bts     eax, 5
        DB      0FH,22H,0E0H           ; MOV CR4, EAX
; set address of page tables
        mov     eax, cr3
        mov     [cr3copy], eax
        mov     eax, [PAGEMAP]
        mov     cr3, eax
; enable long mode, set EFER.LME
        mov     ecx, 0C0000080H
        rdmsr
        bts     eax, 8
        wrmsr
        ; enable paging to activate long mode
        mov     eax, cr0
        or      eax, 80000000h
        mov     cr0, eax
; now we are in 16 bit compatibility mode
; jump to 64 bit code
        jmp     fword ptr [jump64]
        

; return from 64 bit mode:
RetTo16:
; disable paging to deactivate long mode
        mov     eax, cr0
        and     eax, 7FFFFFFFh
        mov     cr0, eax

; reset EFER.LME
        mov     ecx, 0C0000080H
        rdmsr
        btr     eax, 8
        wrmsr

; back to real mode
        mov     eax, [cr0copy]
        mov     cr0, eax               ; return to real mode
        DB      0EAH                   ; JMP FAR PTR S16
        DW      OFFSET CODE16:S16
        DW      CODE16

S16:    mov     ax, DGROUP             ; restore segment registers and stack pointer
        mov     ds, ax
        mov     es, ax
        mov     ss, ax
        mov     sp, OFFSET  DGROUP:STAKEND
        sti

IF USE_PERFORMANCE_COUNTERS NE 0
; stop all performance counters
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
ENDIF

; write results
        xor     edi, edi        
NEXTRESULT: ; loop through result lists

        ; write clock count
        MOV     EAX, CLOCKLIST[EDI]
        CALL    WRITENUM

        ; write performance counters
        I       = 0
        REPT    NCOUNTERS       ; macro loop for all performance counters
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
        sti
        finit
        xor     ebp, ebp              ; DOS will crash if high dword not zero
        mov     ax, 4C00H
        int     21H                   ; EXIT

; subroutines (16 bit mode):

PROCESSORTYPE PROC NEAR  ; detects microprocessor type
; return value: AL = family, AH = 1 if 64 bit mode supported
; DX = feature flags if CPUID instruction supported
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
        mov     esi, eax               ; family
        mov     edi, edx               ; features
        mov     eax, 80000000h
        CPUID
        cmp     eax, 80000000h
        jbe     PT7
        mov     eax, 80000001h
        CPUID
        bt      edx, 29
        jnc     PT7
        or      esi, 100h              ; 64 bit supported        
PT7:    mov     eax, esi
        mov     edx, edi
        pop     edi
        pop     esi        
PT9:    ret
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

CODE16  ENDS

; =======================   64 BIT CODE SEGMENT   =======================
CODE64  SEGMENT PAGE  PUBLIC USE32 'CODE'
ASSUME  cs:code64,ds:code64,es:nothing,ss:code64


; ########## YOU MAY PLACE DATA HERE ####################################

E0      dd      012345h 
E1      dd      100h dup (0)

; ############## END OF USER DATA #######################################


; user procedure to output additional data EAX
uout    proc    near
        push    ebx
        mov     ebx, [ebp][nuser-bp0]
        cmp     ebx, 100
        jnb     uo9
        mov     [ebp][userlist-bp0][ebx*4], eax
        add     ebx, 1
        mov     [ebp][nuser-bp0], ebx
uo9:    pop     ebx
        ret
uout    endp

; entry point for 64 bit code:
ENTRY_CODE64:
        mov     ebp, ebp    ; reset high dword of rbp
        xor     esp, esp    ; calculate stack pointer
        mov     sp, DGROUP
        shl     esp, 4
        add     esp, offset DGROUP:STAKEND

        
; ##################   BEGIN USER INITIALIZATIONS 1   #######################
; insert any initializations here that you want to run only once:

        FINIT              ; initialize floating point registers
        CLD

        pxor xmm0,xmm0     ; initialize XMM registers
        pxor xmm1,xmm1
        pxor xmm2,xmm2
        pxor xmm3,xmm3
        pxor xmm4,xmm4
        pxor xmm5,xmm5
        pxor xmm6,xmm6
        pxor xmm7,xmm7

; ######################   END USER INITIALIZATIONS 1   ######################
             
ALIGN   16
AGAIN:  ; top of loop

; ###############   BEGIN USER INITIALIZATIONS 2 ############################
; insert any initializations here that you want to run for each iteration:

        lea esi, [ebp].d0-BP0      ; address of d0


; ################  END USER INITIALIZATIONS 2   ############################

        ; start to count clock cycles:
        XOR     EAX, EAX
        CPUID                   ; serialize
        MOV     ebx, [ebp].[COUNTER-BP0]
        
        I       = 0
        REPT    NCounters       ; macro loop for all performance counters
        COMBINENAMES cn, Counter, %I  ; cn = Counter0, etc.
        mov     ecx, cn OR (1 SHL 31)
        RDPMC                   ; read counter
        ; Note: if you use WRMSR to reset the counters here then the test
        ; instructions will not go into the trace cache!
        mov     [ebp].[PMCLIST-BP0+I*LISTLEN*4+4][ebx], eax  ; store counter value in list
        I       = I + 1
        ENDM

        RDTSC                   ; read clock counter
        mov     [ebp].[TICS-BP0], eax

        XOR     EAX, EAX
        CPUID                   ; serialize again

        
; #####################   BEGIN INSTRUCTIONS TO TEST   ######################
; insert the instructions to test here:
; лл

; replace this example with your own code:

REPT 100 
        db   rexw
        imul eax,5          ; imul rax, 5
ENDM


; ######################    END INSTRUCTIONS TO TEST    ######################


        ; serialize
        XOR     EAX, EAX
        CPUID
        
        ; read clock
        RDTSC       
        MOV     EDI, [ebp].[COUNTER-BP0]
        MOV     [ebp].[CLOCKLIST-BP0][EDI], EAX   ; store clock count in table

        ; read performance counters
        I       = 0
        REPT    NCounters       ; macro loop for all performance counters
        COMBINENAMES cn, Counter, %I  ; cn = Counter0, etc.
        mov     ecx, cn OR (1 SHL 31)
        RDPMC                   ; read counter
        mov     [ebp].[PMCLIST-BP0+I*LISTLEN*4][EDI], EAX  ; save count in list
        I       = I + 1
        ENDM

        ; serialize again
        XOR     EAX, EAX
        CPUID      

        ; subtract overhead from all counters
        I       = 0             
        REPT    NCounters       ; macro loop for all performance counters
        COMBINENAMES ai, Adjust, %I    ; ai = Adjust0, Adjust1, ...
        mov     eax, [ebp].[PMCLIST-BP0+I*LISTLEN*4+4][EDI] ; previous count
        add     eax, ai   ; overhead for performance count
        sub     [ebp].[PMCLIST-BP0+I*LISTLEN*4][EDI], eax ; subtract previous count and overhead
        I       = I + 1
        ENDM

        mov     EAX, [ebp].[TICS-BP0]
        add     EAX, OVERHC ; subtract the number of clocks used by surrounding code
        sub     [ebp].[CLOCKLIST-BP0][EDI], EAX   ; subtract overhead from clock count

        ; repetion loop ends here
        MOV     ESP, [ebp].[ESPSAVE-BP0]
        ADD     EDI, TYPE CLOCKLIST
        MOV     [ebp].[COUNTER-BP0], EDI
        CMP     EDI, ITER * (TYPE CLOCKLIST)

        JB      AGAIN

FINISHTEST:
; return to 16 bit mode
        jmp     fword ptr [ebp].[jump16-BP0]

@CURSEG ENDS

END     START
