rem                      m32.bat                          2005-07-16 Agner Fog

rem Batch file for assembling and running test program in 32-bit mode 
rem with MASM 6.15
rem The test program will switch the microprocessor to 32 bit privileged mode,
rem do the test, switch back to 16 bit mode, and print out the test results.

rem Instructions:
rem 1. use p1test.asm for Pentium 1 or Pentium MMX,
rem    use p23test for Pentium Pro, Pentium II, Pentium III, Pentium M,
rem    use p4test.asm for Pentium 4,
rem    use p4test.asm with USE_PERFORMANCE_COUNTERS = 0 for other processors
rem 2. copy p1test.asm, p23test.asm or p4test.asm to test.asm, 
rem 3. insert the code to test in test.asm at the place indicated,
rem 4. run m32.bat
rem 5. copy test.exe to a bootable disk or bootable USB memory stick
rem 6. get the computer you want to run the test on into DOS real mode
rem 7. run test.exe

ml /c /omf /Zm /Cx /Dwordsize=4 test.asm
if errorlevel 1 goto end
link16 test.obj;
if errorlevel 1 goto end
echo off
test.exe
:end
