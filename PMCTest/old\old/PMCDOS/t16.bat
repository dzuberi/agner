rem                      t16.bat                          2005-07-16 Agner Fog

rem Batch file for assembling and running test program in 16-bit mode 
rem with Borland TASM 3.0 on DOS command line

rem Instructions:
rem 1. use p1test.asm for Pentium 1 or Pentium MMX,
rem    use p23test for Pentium Pro, Pentium II, Pentium III, Pentium M,
rem    use p4test.asm for Pentium 4,
rem    use p4test.asm with USE_PERFORMANCE_COUNTERS = 0 for other processors
rem 2. Get computer into DOS real mode
rem 3. copy p1test.asm, p23test.asm or p4test.asm to test.asm, 
rem 4. use a DOS editor to modify test.asm
rem 5. insert the code to test in test.asm at the place indicated,
rem 6. then run t16.bat

tasm -Dwordsize=2 /m test,test,test;
if errorlevel 1 goto end
link16 test.obj;
if errorlevel 1 goto end
echo off
test.exe
:end
