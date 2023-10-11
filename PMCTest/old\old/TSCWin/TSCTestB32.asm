comment %
              TSCTestB32.asm                           © 2005-07-16 Agner Fog


Test program to measure how many clock cycles a little piece of code takes.

The program will execute the code to test NUMTESTS times and then print
out a list of clock counts.

The project consists of 
TSCTestA.cpp and TSCTestB32.asm for 32 bit mode, or
TSCTestA.cpp and TSCTestB64.asm for 64 bit mode.

Instructions:

Insert the code to test at the place marked "### Test code here ###"
in TSCTestB32.asm or TSCTestB64.asm

Compile and link for Windows, console mode, 32 og 64 bits.

© 2004 GNU General Public License www.gnu.org/copyleft/gpl.html
%

.686
.mmx
.xmm
.model flat

; ###################### DEFINE CONSTANTS ######################

; set number of times to run test
NUMTESTS = 20

; ###################### END DEFINE CONSTANTS ######################

; ---------------------- DATA SEGMENT ----------------------
.data

clocks   dword   ?                ; pointer to results array
overhead qword   ?                ; timing overhead
espSave  dword   ?                ; save stack pointer
_NumberOfTests dword NUMTESTS     ; number of tests
public _NumberOfTests


; ###################### User data ######################

align 16

; Insert any data your test code needs here:

d0	real8 0., 0., 0., 0.

        dq 1024 dup (?)


; ###################### End of user data ######################


; ---------------------- code segment ----------------------
.code

; C++ prototype:
; extern "C" int ClockTest (__int64 clocks[]);

_ClockTest PROC NEAR
        push    ebx
        push    esi
        push    edi
        push    ebp
        mov     espSave, esp

        ; pointer to results list
        mov     esi, [esp+20]     ; clocks[]
        mov     clocks, esi

        ; get list into cache
        cld
        mov     ecx, NUMTESTS * 2
        rep     lodsd

        ; dummy test loop without test code to measure overhead 
        xor     ebp, ebp
        mov     edi, clocks
CLOOP1:
        sub     eax, eax
        cpuid                     ; serialize
        rdtsc                     ; read time stamp counter into edx:eax
        mov     [edi+ebp], eax    ; save in list
        mov     [edi+ebp+4], edx  ; save high dword in list
        sub     eax, eax
        cpuid                     ; serialize again

        ; void to measure overhead only

        sub     eax, eax
        cpuid                     ; serialize
        rdtsc                     ; read time stamp counter into edx:eax
        sub     eax, [edi+ebp]    ; subtract old value
        sbb     edx, [edi+ebp+4]
        mov     [edi+ebp], eax    ; save difference in list
        mov     [edi+ebp+4], edx
        sub     eax, eax
        cpuid                     ; serialize again

        add     ebp, 8           
        cmp     ebp, NUMTESTS*8
        jb      CLOOP1            ; dummy loop end

        ; find smallest clock count:
        mov     edi, clocks
        mov     eax, -1
        cdq
        xor     ebx, ebx
        xor     ecx, ecx
IF NUMTESTS GT 1
        mov     [edi], eax        ; exclude first count from minimum
        mov     [edi+4], edx
ENDIF
        xor     ebp, ebp
MINLOOP:
; (If cpu doesn't support conditional moves then replace all cmov by conditional jumps)

        cmp     [edi+ebp],eax     ; compare low dword and get result into bl
        setb    bl
        cmp     [edi+ebp+4],edx   ; compare high dword and get result into cl
        setb    cl
        cmove   ecx, ebx          ; replace cl by bl if high dword equal
        test    cl, cl
        cmovnz  eax, [edi+ebp]    ; get lowest qword into edx:eax
        cmovnz  edx, [edi+ebp+4]
        add     ebp, 8
        cmp     ebp, NUMTESTS*8
        jb      MINLOOP
        mov     dword ptr overhead, eax  ; save minimum overhead
        mov     dword ptr overhead+4, edx

        ; loop to measure clock cycles of test code
        xor     ebp, ebp
        mov     edi, clocks
CLOOP2:
        sub     eax, eax
        cpuid                     ; serialize
        rdtsc                     ; read time stamp counter into edx:eax
        mov     [edi+ebp], eax    ; save in list
        mov     [edi+ebp+4], edx  ; save high dword in list
        sub     eax, eax
        cpuid                     ; serialize again


; ###################### Test code here ######################


; put your test code here. Example:

        REPT 100          ; repeat code 100 times
        shr eax, 5
        ENDM


; ###################### Test code end  ######################

        sub     eax, eax
        cpuid                     ; serialize
        rdtsc                     ; read time stamp counter into edx:eax
        sub     eax, [edi+ebp]    ; subtract old value
        sbb     edx, [edi+ebp+4]
        mov     [edi+ebp], eax    ; save difference in list
        mov     [edi+ebp+4], edx
        sub     eax, eax
        cpuid                     ; serialize again

        add     ebp, 8
        cmp     ebp, NUMTESTS*8
        jb      CLOOP2

        ; subtract overhead from clock counts:
        xor     ebp, ebp
        mov     edi, clocks
        mov     eax, dword ptr overhead
        mov     edx, dword ptr overhead + 4
OVHLOOP:
        sub     [edi+ebp], eax
        sbb     [edi+ebp+4], edx
        add     ebp, 8
        cmp     ebp, NUMTESTS*8
        jb      OVHLOOP

        mov     eax, NUMTESTS
        mov     esp, espSave
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
        ret
_ClockTest ENDP

END
