;                             AMDTEST.ASM                    й Agner Fog 2006
;
comment %
This is a general program for testing how many clock cycles a piece of
code takes on an AMD microprocessor.
It cannot be used for other processors.
 
Your piece of code is repeated several times and the number of clock cycles
is counted each time.

Insert the code you want to test after the line marked with:
"###   BEGIN INSTRUCTIONS TO TEST   ###"
Any necessary data or initializations may be inserted in the appropriately
marked spaces.

The program is intended to run only on a Pentium Pro/II/III processor and
only in REAL MODE. You have to boot your computer from DOS (not Windows)
and skip EMM386 or any other memory manager in CONFIG.SYS. If you have
Windows then reboot your computer and press F8 while booting. Then select
"Safe mode command prompt only". The program cannot run from Windows or
any other protected operating system, and it cannot run if a memory manager
is installed. The reason for this is that the program needs complete control
of the microprocessor.

It is possible to test the same code in 16 bit real mode and 32 bit
protected mode: set WORDSIZE = 2 for 16 bit mode, or 4 for 32 bit mode.

Set the event counters EVENT0 and EVENT1 if you want to use performance
monitor counters. Adjust OVERH0 and OVERH1 so that you get zero counts
when there is nothing in the INSTRUCTIONS TO TEST field.

Adjust OVERHC so that you get zero clock counts when there is nothing
in the INSTRUCTIONS TO TEST field.

It is not possible to output any data to the screen from the piece of code
in the INSTRUCTIONS TO TEST field. If you want to output any results from
your code then use CALL UOUT. This procedure stores the contents of the
EAX register to be output after return to 16-bit mode.

й 2006 GNU General Public License www.gnu.org/copyleft/gpl.html
%
.386

;###################   CHOOSE EVENT COUNTERS   ############################
; set events to count on performance monitor counters:

EVENT0 = 0C0H  ; counter 0: instructions retired
EVENT1 = 0C1H  ; counter 1: uops retired

EMASK0 = 0     ; unit mask for counter 0
EMASK1 = 0     ; unit mask for counter 1

; overhead to subtract from event counters
OVERH0 = 9
OVERH1 = 71

; overhead to subtract from clock counter
OVERHC = 111

;###################   SET NUMBER OF ITERATIONS   ############################

; set ITER = desired number of iterations
ITER = 10

; set NOUT = desired number of data output
NOUT = ITER

IFNDEF WORDSIZE
WORDSIZE = 4          ; 4 for 32 bit protected mode, 2 for 16 bit real mode
ENDIF

;-------------------   DEFINE MACROS   ----------------------------
; define macros

INCLUDE P4MACROS.ASI    ; define new instructions if not supported by assembler

RDTSC   MACRO   ; read time stamp counter
        DB      0FH,31H
ENDM

CPUID   MACRO   ; cpuid instruction
        DB      0FH,0A2H
ENDM

WRMSR   MACRO   ; write model specific register
        DB      0FH,030H
ENDM

RDMSR   MACRO   ; read model specific register
        DB      0FH,032H
ENDM

RDPMC   MACRO   ; read performance monitor counter
        DB      0FH,33H
ENDM

FNSTSWAX MACRO  ; FNSTSW AX. TASM 3.0 has a bug on this instruction
        DB 0DFH, 0E0H
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

.386p

; --------------------   DATA SEGMENT   ----------------------------
data    segment para public use16 'data'

; global descriptor table
gdt             dd      0
                dd      0
code16_desc     dd      0000ffffh
                dd      00009a00h
code32_desc     dd      0000ffffh
                dd      004f9a00h
dgroup_desc     dd      0000ffffh
                dd      00cf9200h
gdt_size        =       $-gdt

gdt_label       label   fword
gdt_limit       dw      gdt_size-1
gdt_base        dd      ?
cr0copy         dd      ?
profamily       dd 0            ; microprocessor family
profeatures     dd 0            ; microprocessor features flag

; segment selectors
code16_s        =       code16_desc - gdt
code32_s        =       code32_desc - gdt
dgroup_s        =       dgroup_desc - gdt

num             dt ?

; text strings
Heading         db 'Clocks      PMC0         PMC1',13,10,36
WrongProc       db 'Error: Not a Pentium Pro or later microprocessor',13,10
                db 'family = '
fam1            db '0',13,10,36
WrongMode       db 'Error: Microprocessor not in real mode',13,10
                db 'Please boot from DOS without EMM386',13,10,36
WarningProc     db 'Warning: Not a Pentium Pro or later microprocessor.',13,10
                db 'family = '
fam2            db '0',13,10,10,36
UHead           db 'Additional user data:',13,10,36

ALIGN 8
IF      WORDSIZE EQ 4               ; make part of data segment 32 bit
data    ends
data32  segment para public use32 'data'
dgroup  group data32
ENDIF

; test data
tics            dd ?
counter         dd 0

NDAT = ITER
IF NOUT GT ITER
NDAT = NOUT
ENDIF

RESULTLIST      dd NDAT dup (0) ; clock cycles
OUTLIST1        dd NDAT dup (0) ; PMC1
OUTLIST2        dd NDAT dup (0) ; PMC2
USERLIST        dd 100  dup (0) ; any user data
NUSER           dd 0            ; number of user data

align 16
;###################   USER DATA BEGIN   ##############################
; put any data here that your test code needs

d0      dd      0
        db      4096 dup (0)

;###################   USER DATA END    ###############################

@curseg ends

stak    segment dword stack use16 'stack'
dd      256 dup (?)
stakend label dword
stak    ends

dgroup  group data,stak

; -------------------------   16 BIT CODE SEGMENT   -------------------------
code16 segment para public use16 'CODE'
assume cs:code16,ds:dgroup,ss:dgroup

START:
        MOV     AX,DGROUP
        MOV     DS,AX
        MOV     SS,AX
        MOV     SP,OFFSET DGROUP:STAKEND

; test processor type
        call    PROCESSORTYPE
        ADD     [FAM1],AL
        ADD     [FAM2],AL
        mov     [profamily], eax
        mov     [profeatures], edx
        CMP     AX,6
        JNB     SHORT L_2
L_1:    WRITETEXT WrongProc            ; family < 6
        MOV     AX,4C02H
        INT     21H                    ; EXIT

L_2:    TEST    DX,10H                 ; test for RDTSC instruction
        JZ      L_1                    ; RDTSC instr. not available
        CALL    PROCESSORMODE          ; test if processor is in real mode
        TEST    AX,AX
        JZ      SHORT L_3
        WRITETEXT WrongMode            ; not in real mode
        MOV     AX,4C01H
        INT     21H                    ; EXIT
L_3:    cmp     [profamily],6
        JAE      SHORT L_4
        WRITETEXT WarningProc          ; not a Pentium but RDTSC supported
L_4:    ; processortype and mode OK
        test    [profeatures], 1 shl 25
        jz      L_5                    ; xmm instructions not supported
        ; enable xmm instructions and MSR counters
        DB      0FH,20H,0E0H           ; MOV EAX, CR4
        OR      EAX, (1 SHL 9) OR 100H ; enable MSR counters
        AND     EAX, NOT 4
        DB      0FH,22H,0E0H           ; MOV CR4, EAX
L_5:    WRITETEXT Heading

IF WORDSIZE EQ 4
; setup global descriptor table
        XOR     EAX,EAX
        MOV     AX,DGROUP
        SHL     EAX,4
        XOR     EBX,EBX
        MOV     BX,OFFSET DGROUP:GDT
        ADD     EAX,EBX
        MOV     GDT_BASE,EAX

; runtime fixup of segment descriptors
; code32
        XOR     EAX,EAX
        MOV     AX,CODE32
        SHL     EAX,4
        MOV     WORD PTR CODE32_DESC+2, AX
        SHR     EAX, 16
        MOV     BYTE PTR CODE32_DESC+4, AL
; code16
        XOR     EAX,EAX
        MOV     AX, CODE16
        SHL     EAX, 4
        MOV     WORD PTR CODE16_DESC+2, AX
        SHR     EAX, 16
        MOV     BYTE PTR CODE16_DESC+4, AL
; dgroup
        XOR     EAX,EAX
        MOV     AX, DGROUP
        SHL     EAX, 4
        MOV     WORD PTR DGROUP_DESC+2, AX
        SHR     EAX, 16
        MOV     BYTE PTR DGROUP_DESC+4, AL

; switch to protected mode
        AND     ESP,0FFFFH
        CLI
        DB      66H
        LGDT    GDT_LABEL
        MOV     EAX,CR0
        MOV     CR0COPY,EAX
        OR      AL,1
        AND     EAX,7fffffffh
        MOV     CR0,EAX
;       JMP     FAR PTR PROTECTED_CODE
        DB      66H, 0EAH
        DD      OFFSET CODE32:PROTECTED_CODE
        DW      CODE32_S

; return from protected mode
ret_real:       
        MOV     EAX,CR0COPY
        AND     AL,NOT 1
        MOV     CR0,EAX
;       JMP     FAR PTR S16
        DB      0EAH
        DW      OFFSET CODE16:S16
        DW      CODE16
S16:    MOV     AX,DGROUP
        MOV     DS,AX
        MOV     SS,AX
        MOV     SP,OFFSET DGROUP:STAKEND
        STI
ELSE    ; 16 bit mode:
        CLI
        JMP     PROTECTED_CODE
WRITERES:
        STI
ENDIF

; write results
        MOV     @DI, OFFSET DGROUP:RESULTLIST
        MOV     @SI, OFFSET DGROUP:OUTLIST1
        MOV     @BP, OFFSET DGROUP:OUTLIST2
NEXTRESULT:
        MOV     EAX,[DI]
        ADD     DI, TYPE RESULTLIST
        CALL    WRITENUM
IF NOUT
        MOV     DL,' '                 ; FILL WITH SPACES
        MOV     AH,2
        INT     21H                    ; WRITE A ' '
        MOV     EAX,[SI]
        ADD     SI, TYPE OUTLIST1
        CALL    writenum
        MOV     DL,' '                 ; FILL WITH SPACES
        MOV     AH,2
        INT     21H                    ; WRITE A ' '
        MOV     DL,' '                 ; FILL WITH SPACES
        MOV     AH,2
        INT     21H                    ; WRITE A ' '
        MOV     EAX,DS:[BP]
        ADD     BP, TYPE OUTLIST2
        CALL    WRITENUM
ENDIF
        CALL    NEWLINE
        CMP     @DI, OFFSET DGROUP:RESULTLIST + SIZE RESULTLIST
        JB      NEXTRESULT
        XOR     ESI, ESI
        CMP     ESI, [NUSER]
        JE      S20
        WRITETEXT UHead
S18:    CMP     ESI, [NUSER]
        JNB     S19
        MOV     EAX, USERLIST[ESI*4]
        CALL    WRITEHEX
        MOV     DL,' '
        MOV     AH,2
        INT     21H                    ; write spaces
        MOV     AH,2
        INT     21H
        INC     ESI
        JMP     S18
S19:    CALL    NEWLINE
S20:    ; exit
        FINIT
        xor     ebp, ebp               ; high dword must be 0
        MOV     AX,4C00H
        INT     21H                    ; exit

; subroutines

PROCESSORTYPE PROC NEAR  ; detects microprocessor type
; return value: AX = family, DX = feature flags if CPUID instruction supported
        XOR     AX,AX
        XOR     DX,DX
        PUSH    SP                     ; TEST IF 8088
        POP     BX
        CMP     BX,SP
        JE      SHORT PT1
PT0:    RET
PT1:    MOV     AL,2
        PUSH    0F200H                 ; test if 80286
        POPF
        PUSHF
        POP     BX
        TEST    BX,0F000H
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
        xor     eax,eax
        CPUID
        xor     edx,edx
        cmp     eax,1
        mov     eax,4
        jl      short PT9              ; CPUID level 1 not supported
        mov     eax,1
        CPUID
        shr     eax,8
        and     ax,0fh                 ; isolate family number
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
        shr     ebx,16
        and     bx,2                   ; isolate VM flag
        mov     al,1                   ; set bit in case we can't read cr0
        smsw    ax
        and     ax,1                   ; isolate protection bit
        or      ax,bx
        ret
PROCESSORMODE ENDP

WRITENUM PROC NEAR  ; writes number in EAX as decimal signed integer
        push    si
        push    di
        xor     di,di
        test    eax,eax
        jns     short WN1
        neg     eax                    ; negative
        push    eax
        mov     dl,'-'
        mov     ah,2
        int     21h                    ; write '-'
        inc     di
        pop     eax
WN1:    mov     si, offset ds:NUM      ; 10 byte temp. buffer
        xor     ecx,ecx
        mov     [si],eax
        mov     [si+4],ecx
        fild    qword ptr [si]
        fbstp   tbyte ptr [si]         ; convert to packed BCD
        add     si,6
        std
        wait
WN2:    lodsb                          ; read two BCD digits
        mov     ah,al                  ; unpack two BCD digits
        and     al,0fh
        shr     ah,4
        mov     dl,ah
        or      ah,cl
        jz      short WN3              ; skip leading zeros
        inc     cx
        add     dl,'0'
        push    ax
        mov     ah,2
        int     21h                    ; print digit
        inc     di
        pop     ax
WN3:    mov     dl,al
        or      al,cl
        jz      short WN4              ; skip leading zeros
        inc     cx
        add     dl,'0'
        mov     ah,2
        int     21h                    ; print digit
        inc     di
WN4:    cmp     si, offset ds:NUM
        jnb     WN2                    ; next two digits
        test    cl,cl
        jnz     short WN5
        mov     dl,'0'                 ; no digits have been written
        mov     ah,2
        int     21h                    ; write a '0'
        inc     di
WN5:    cmp     di,11
        jnb     WN9
        mov     dl,' '                 ; fill with spaces
        mov     ah,2
        int     21h                    ; write a ' '
        inc     di
        jmp     WN5
WN9:    cld
        pop     di
        pop     si
        ret
WRITENUM        ENDP

WRITEHEX PROC NEAR                      ; writes EAX as hex
        PUSH    EBX
        mov     bx,8
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
        and al,0fh
        add al,'0'
        cmp al,'9'
        jna wrh2
        add al,'A'-'9'-1
        wrh2:
        mov dl,al
        mov ah,2
        int 21h
        ret
WRITEAL ENDP

NEWLINE PROC NEAR
        MOV     DL,13
        MOV     AH,2
        INT     21H                    ; carriage return
        MOV     DL,10
        MOV     AH,2
        INT     21H                    ; linefeed
        RET
NEWLINE ENDP

; ----------------------   32 BIT CODE SEGMENT   -------------------------

IF WORDSIZE EQ 4
code16  ends

code32  segment page  public use32 'CODE'
assume  cs:code32,ds:dgroup,es:dgroup,ss:dgroup

protected_code:
        mov     ax,dgroup_s
        mov     ds,ax
        mov     es,ax
        mov     ss,ax
ELSE
protected_code:
ENDIF

; ------------------   SET UP EVENT COUNTERS   --------------------------

        XOR     EDX, EDX
        MOV     ECX, 0c0010000h
        MOV     EAX, EVENT0 OR (EMASK0 SHL 8) OR (3 SHL 16) or (1 shl 22)
        WRMSR                   ; performance monitor counter 0
        INC     ECX
        MOV     EAX, EVENT1 OR (EMASK1 SHL 8) OR (3 SHL 16) or (1 shl 22)
        WRMSR                   ; performance monitor counter 1

;###################   BEGIN USER INITIALIZATIONS 1   #######################
; insert any initializations here that you want to run only once

        FINIT
        CLD
        LEA     ESI, D0

;###################    END USER INITIALIZATIONS 1    #######################
             
align   16
again:  ; top of loop

;################   BEGIN USER INITIALIZATIONS 2   ##########################
; insert any initializations here that you want to run for each iteration


;################    END USER INITIALIZATIONS 2    ##########################
        XOR     EAX,EAX
        CPUID                   ; serialize
        XOR     EAX,EAX
        XOR     EDX,EDX
        MOV     ECX, 0C0010004H
        WRMSR                   ; reset performance mon counters
        INC     ECX
        WRMSR
        RDTSC                   ; read clock counter
        mov     [TICS], eax
        XOR     EAX,EAX
        CPUID                   ; serialize

; #####################   BEGIN INSTRUCTIONS TO TEST   ######################
; insert the instructions to test here: 
; лл


rept 100    ; eaxmple: 100 shift instructions

shr eax,1

endm


;#####################    END INSTRUCTIONS TO TEST    #######################

        XOR     EAX,EAX
        CPUID                   ; serialize
        RDTSC
        PUSH    EAX
        XOR     ECX,ECX
        RDPMC                   ; perf mon counter 0
        PUSH    EAX
        INC     ECX
        RDPMC                   ; perf mon counter 1
        PUSH    EAX
        XOR     EAX,EAX
        CPUID                   ; serialize
        MOV     EDX, [COUNTER]
        POP     EAX
        SUB     EAX, OVERH1
        MOV     OUTLIST2[EDX], EAX
        POP     EAX
        SUB     EAX, OVERH0
        MOV     OUTLIST1[EDX], EAX
        POP     EAX
        SUB     EAX, [TICS]
        SUB     EAX, OVERHC ; subtract the number of clocks used by filler
                         ; instructions etc. You may have to adjust this number
                         ; for microprocessors other than Pentium II/III

        MOV     [RESULTLIST][EDX], EAX   ; store result in table
        ADD     EDX, TYPE RESULTLIST
        MOV     [COUNTER], EDX
        CMP     EDX, ITER * (TYPE RESULTLIST)

IF WORDSIZE EQ 4
; MASM v. 5.10 has a bug with near jumps in 32 bit code.
; If you have another assembler you may write JB AGAIN here
        DB      0FH,82H                ; hardcode  JB AGAIN
        DD      AGAIN - $ - 4

; return to 16 bit mode
        DB      66H,0EAH               ; hardcode jmp far ptr ret_real
        DW      OFFSET CODE16:RET_REAL
        DW      CODE16_S,0
ELSE  ; 16 bit mode:
        JB      AGAIN
        JMP     WRITERES
ENDIF 

; procedure to output additional data EAX
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

@CURSEG ENDS

END     START
