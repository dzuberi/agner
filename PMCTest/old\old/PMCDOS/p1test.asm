;                    P1TEST.ASM                          © Agner Fog 1998
;
comment %
This is a general program for testing how many clock cycles a piece of
code takes on a PENTIUM or PENTIUM MMX microprocessor. It cannot be used
on other processors.

Your piece of code is repeated several times and the number of clock cycles
is counted each time.

Insert the code you want to test after the line marked with:
"###   BEGIN INSTRUCTIONS TO TEST   ###"
Any necessary data or initializations may be inserted in the appropriately
marked spaces.

The program is intended to run only on a Pentium 1 or Pentium MMX processor and
only in REAL MODE. You have to boot your computer from DOS (not Windows)
and skip EMM386 or any other memory manager in CONFIG.SYS. If you have
Windows then reboot your computer and press F8 while booting. Then select
"Safe mode command prompt only". The program cannot run from Windows or
any other protected operating system, and it cannot run if a memory manager
is installed. The reason for this is that the program needs complete control
of the microprocessor.

It is possible to test the same code in 16 bit mode and 32 bit mode:
set WORDSIZE = 2 for 16 bit mode, or 4 for 32 bit mode.

© 1998 GNU General Public License www.gnu.org/copyleft/gpl.html
%

IFNDEF WORDSIZE
WORDSIZE = 4
ENDIF

; Adjust overhead to subtract from clock count
OVERHC = 188    ; overhead to subtract from clock counter

; set ITER = desired number of iterations
ITER = 18

; set NOUT = desired number of data output
NOUT = ITER

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

RDTSC   MACRO   ; read time stamp counter on pentium chip
        DB      0FH,31H
ENDM

CPUID   MACRO   ; cpuid instruction
        DB      0FH,0A2H
ENDM

WRITETEXT MACRO ttt                    ; write a text string
        mov     dx, offset ds:ttt
        mov     ah,9
        int     21h
ENDM

.386p

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

; segment selectors
code16_s        =       code16_desc - gdt
code32_s        =       code32_desc - gdt
dgroup_s        =       dgroup_desc - gdt

num             dt ?

; text strings
ClockCyc        db 'Clock cycles:',13,10,36
WrongProc       db 'Error: Not a Pentium microprocessor',13,10
                db 'family = '
fam1            db '0',13,10,36
WrongMode       db 'Error: Microprocessor not in real mode',13,10
                db 'Please boot from DOS without EMM386',13,10,36
WarningProc     db 'Warning: Not a Pentium microprocessor.',13,10
                db 'family = '
fam2            db '0',13,10,10,36

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

resultlist      dd NDAT dup (0)
dd 100 dup (0)

outlist         dd NDAT dup (0)

align 8
;###################   USER DATA BEGIN   ##############################
; You may insert any data used by your code here

d0      dd      0
d1      dd      1
d2      dd      375.
d3      dd      2.64564e4
d4      dd      0.113431e5
d5      dd      4.616
d6      dd      75.0456
d7      dd      0
d8      dd      0
d9      dd      0
d10     dd      0

        db      4096 dup (0)

;###################   USER DATA END   ################################

@curseg ends

stak    segment dword stack use16 'stack'
dd      256 dup (?)
stakend label dword
stak    ends

dgroup  group data,stak

code16 segment para public use16 'code'
assume cs:code16,ds:dgroup,ss:dgroup

start:
        mov     ax,dgroup
        mov     ds,ax
        mov     ss,ax
        mov     sp,offset dgroup:stakend

; test processor type
        call    PROCESSORTYPE
        add     [fam1],al
        add     [fam2],al
        mov     si,ax
        cmp     ax,5
        jnb     short L_2
L_1:    WRITETEXT WrongProc            ; not a Pentium
        mov     ax,4c02h
        int     21h                    ; exit

L_2:    test    dx,10h                 ; test for RDTSC instruction
        jz      L_1                    ; RDTSC instr. not available
        call    PROCESSORMODE          ; test if processor is in real mode
        test    ax,ax
        jz      short L_3
        WRITETEXT WrongMode            ; not in real mode
        mov     ax,4c01h
        int     21h                    ; exit
L_3:     cmp     si,5
        je      short L_4
        WRITETEXT WarningProc          ; not a Pentium but RDTSC supported
L_4:
; all checks OK
        WRITETEXT ClockCyc

IF WORDSIZE EQ 4
; setup global descriptor table
        xor     eax,eax
        mov     ax,dgroup
        shl     eax,4
        xor     ebx,ebx
        mov     bx,offset dgroup:gdt
        add     eax,ebx
        mov     gdt_base,eax

; runtime fixup of segment descriptors
; code32
        xor     eax,eax
        mov     ax,code32
        shl     eax,4
        mov     word ptr code32_desc+2, ax
        shr     eax, 16
        mov     byte ptr code32_desc+4, al
; code16
        xor     eax,eax
        mov     ax, code16
        shl     eax, 4
        mov     word ptr code16_desc+2, ax
        shr     eax, 16
        mov     byte ptr code16_desc+4, al
; dgroup
        xor     eax,eax
        mov     ax, dgroup
        shl     eax, 4
        mov     word ptr dgroup_desc+2, ax
        shr     eax, 16
        mov     byte ptr dgroup_desc+4, al

; switch to protected mode
        and     esp,0ffffh
        cli
        db      66h
        lgdt    gdt_label
        mov     eax,cr0
        mov     cr0copy,eax
        or      al,1
        and     eax,7FFFFFFFH
        mov     cr0,eax
;       jmp     far ptr protected_code
        db      66h, 0eah
        dd      offset code32:protected_code
        dw      code32_s

; return from protected mode
ret_real:       
        mov     eax,cr0copy
        and     al,not 1
        mov     cr0,eax
;       jmp     far ptr s16
        db      0eah
        dw      offset code16:s16
        dw      code16
s16:    mov     ax,dgroup
        mov     ds,ax
        mov     ss,ax
        mov     sp,offset dgroup:stakend
        sti
ELSE    ; 16 bit mode:
        cli
        jmp     protected_code
writeres:
        sti
ENDIF

; write results
        mov     @di, offset dgroup:resultlist
        mov     @si, offset dgroup:outlist
nextresult:
        mov     eax,[di]
        add     di, type resultlist
        call    WRITENUM
IF NOUT
        mov     eax,[si]
        add     si, type outlist
        push    eax
        call    WRITENUM
        mov     dl,' '                 ; fill with spaces
        mov     ah,2
        int     21h                    ; write a ' '
        mov     dl,' '                 ; fill with spaces
        mov     ah,2
        int     21h                    ; write a ' '
        pop     eax
        call    WRITEHEX
ENDIF
        call    NEWLINE
        cmp     @di, offset dgroup:resultlist + size resultlist
        jb      nextresult

; exit
        finit
        xor     ebp, ebp               ; high dword must be 0
        mov     ax,4c00h
        int     21h

; subroutines

PROCESSORTYPE PROC NEAR  ; detects microprocessor type
; return value: AX = family, DX = feature flags if CPUID instruction supported
        xor     ax,ax
        xor     dx,dx
        push    sp                     ; test if 8088
        pop     bx
        cmp     bx,sp
        je      short PT1
PT0:    ret
PT1:    mov     al,2
        push    0f200h                 ; test if 80286
        popf
        pushf
        pop     bx
        test    bx,0f000h
        jz      PT0
        inc     ax                     ; test if 80386
        pushfd
        pop     ebx
        mov     ecx, ebx
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
        mov     eax,cr0
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
        mov bx,8
WDL:    rol eax, 4
        push bx
        push eax
        call writeal
        pop eax
        pop bx
        dec bx
        jnz wdl
        ret
writehex endp

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
writeal endp

NEWLINE PROC NEAR
        MOV     DL,13
        MOV     AH,2
        INT     21H                    ; carriage return
        MOV     DL,10
        MOV     AH,2
        INT     21H                    ; linefeed
        RET
NEWLINE ENDP

IF WORDSIZE EQ 4
code16  ends

code32  segment para  public use32
assume  cs:code32,ds:dgroup,ss:dgroup

protected_code:
        mov     ax,dgroup_s
        mov     ds,ax
        mov     es,ax
        mov     ss,ax
ELSE
protected_code:
ENDIF

;###################   BEGIN USER INITIALIZATIONS 1   #######################
; Here you may insert any initialization to be executed only once

        finit

;###################    END USER INITIALIZATIONS 1    #######################

align   8
again:  ; top of loop

;################   BEGIN USER INITIALIZATIONS 2 (inside loop)   ############
; Here you may insert any initialization to be executed before each iteration

        mov esi,offset ds:d0

;################    END USER INITIALIZATIONS 2    ##########################

        RDTSC            ; read on chip clock counter
        mov     [TICS], eax
        cld              ; non-pairable filler
REPT    8
        nop              ; fillers to avoid shadowing effect
ENDM

;#####################   BEGIN INSTRUCTIONS TO TEST   #######################
; Insert the piece of code to test here:

        inc     [d0]    ; just an example

;#####################    END INSTRUCTIONS TO TEST    #######################

        clc              ; non pairable 1 byte instruction as filler
        RDTSC
        sub     eax, [TICS]
        sub     eax, OVERHC  ; subtract the number of clocks used by overhead
        mov     edx, [COUNTER]
        mov     [RESULTLIST][edx], eax   ; store result in table
        add     edx, type RESULTLIST
        mov     [COUNTER], edx
        cmp     edx, ITER * (type RESULTLIST)

IF WORDSIZE EQ 4
; MASM v. 5.10 has a bug with near jumps in 32 bit code.
; If you have another assembler you may write JB AGAIN here
        DB      0FH,82H                ; hardcode  JB AGAIN
        DD      AGAIN - $ - 4

; return to 16 bit mode
        db      66h,0eah               ; hardcode jmp far ptr ret_real
        dw      offset code16:ret_real
        dw      code16_s,0
code32  ends
ELSE  ; 16 bit mode:
        jb      again
        jmp     writeres
code16  ends
ENDIF 

END     start
