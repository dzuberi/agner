rem                      t64.bat                          2005-07-16 Agner Fog

rem Batch file for assembling and running test program in 64-bit mode 
rem with Borland TASM 3.0 on DOS command line.
rem Only for 64 bit microprocessors. Supports performance monitor counters
rem on 64-bit Intel Pentium processors. Runs on 64 bit AMD processors only
rem with performance monitor counters switched off.
rem
rem The test program will switch the microprocessor to 64 bit privileged mode,
rem do the test, switch back to 16 bit mode, and print out the test results.

rem Instructions:
rem 1. Get computer into DOS real mode
rem 2. copy 64test.asm to test.asm,
rem 3. use a DOS editor to modify test.asm
rem 4. set USE_PERFORMANCE_COUNTERS = 0 if running on AMD processor
rem 5. insert the code to test in test.asm at the place indicated,
rem 6. then run t64.bat


tasm /m test,test,test;
if errorlevel 1 goto end
link16 test.obj;
if errorlevel 1 goto end
echo off
test.exe
:end
pause
