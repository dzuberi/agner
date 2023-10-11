comment %
              TSCTestB64.asm                           © 2005-07-16 Agner Fog


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

; ###################### DEFINE CONSTANTS ######################

; set number of times to run test
NUMTESTS = 20

; ###################### END DEFINE CONSTANTS ######################

; ---------------------- DATA SEGMENT ----------------------
.data

clocks   qword   ?                ; pointer to results array
overhead qword   ?                ; timing overhead
rspSave  qword   ?                ; save stack pointer
NumberOfTests dword NUMTESTS      ; number of tests
public NumberOfTests


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

ClockTest PROC
        push    rbx
		push    rsi
		push    rdi
		push    rbp
		push    r12
		push    r13
		push    r14
		push    r15
                mov     rspSave, rsp

		; pointer to results list
		mov     clocks, rcx

		; get list into cache
		cld
		mov     rsi, rcx
		mov     ecx, NUMTESTS
		rep     lodsq        

		; dummy test loop without test code to measure overhead 
        xor     r15d, r15d
		mov     r14, clocks
CLOOP1:
        sub     eax, eax
        cpuid                     ; serialize
        rdtsc                     ; read time stamp counter into edx:eax
		shl     rdx, 32
		or      rax, rdx          ; combine into rax
        mov     [r14+r15], rax    ; save in list
        sub     eax, eax
        cpuid                     ; serialize again

        ; void to measure overhead only

        sub     eax, eax
        cpuid                     ; serialize
        rdtsc                     ; read time stamp counter into edx:eax
		shl     rdx, 32
		or      rax, rdx          ; combine into rax
		sub     rax, [r14+r15]    ; subtract old value
        mov     [r14+r15], rax    ; save difference in list
        sub     eax, eax
        cpuid                     ; serialize again

		add     r15d, 8           
		cmp     r15d, NUMTESTS*8
		jb      CLOOP1            ; dummy loop end

		; find smallest clock count:
		mov     r14, clocks
		or      rax, -1
IF NUMTESTS GT 1
        mov     [r14], rax        ; exclude first count from minimum
ENDIF
        xor     r15d, r15d
MINLOOP:cmp     [r14+r15],rax
		cmovb   rax, [r14+r15]
   		add     r15d, 8
		cmp     r15d, NUMTESTS*8
		jb      MINLOOP
		mov     overhead, rax

		; loop to measure clock cycles of test code
        xor     r15d, r15d
		mov     r14, clocks
CLOOP2:
        sub     eax, eax
        cpuid                     ; serialize
        rdtsc                     ; read time stamp counter into edx:eax
		shl     rdx, 32
		or      rax, rdx          ; combine into rax
        mov     [r14+r15], rax    ; save in list
        sub     eax, eax
        cpuid                     ; serialize again


; ###################### Test code here ######################


; put your test code here. Example:

        REPT 100          ; repeat code 100 times
		shr rax, 5
		ENDM


; ###################### Test code end  ######################


        sub     eax, eax
        cpuid                     ; serialize
        rdtsc                     ; read time stamp counter into edx:eax
		shl     rdx, 32
		or      rax, rdx          ; combine into rax
		sub     rax, [r14+r15]    ; subtract old value
        mov     [r14+r15], rax    ; save difference in list
        sub     eax, eax
        cpuid                     ; serialize again

		add     r15d, 8
		cmp     r15d, NUMTESTS*8
		jb      CLOOP2

		; subtract overhead from clock counts:
        xor     r15d, r15d
		mov     r14, clocks
		mov     rax, overhead
OVHLOOP:sub     [r14+r15], rax
   		add     r15d, 8
		cmp     r15d, NUMTESTS*8
		jb      OVHLOOP

		mov     eax, NUMTESTS
                mov     rsp, rspSave
        pop     r15
		pop     r14
		pop     r13
		pop     r12
        pop     rbp
		pop     rdi
		pop     rsi
		pop     rbx
        ret
ClockTest ENDP 

END
