rem                      m16.bat                          2005-07-16 Agner Fog

rem Batch file for assembling and running test program in 16-bit mode 
rem with MASM 6.15

rem Instructions:
rem 1. use p1test.asm for Pentium 1 or Pentium MMX,
rem    use p23test for Pentium Pro, Pentium II, Pentium III, Pentium M,
rem    use p4test.asm for Pentium 4,
rem    use p4test.asm with USE_PERFORMANCE_COUNTERS = 0 for other processors
rem 2. Get computer into command line shell
rem 3. copy p1test.asm, p23test.asm or p4test.asm to test.asm, 
rem 4. insert the code to test in test.asm at the place indicated,
rem 5. then run m16.bat

ml /c /omf /Zm /Cx /Dwordsize=2 test.asm
if errorlevel 1 goto end
link16 test.obj;
if errorlevel 1 goto end
echo off
test.exe
:end
