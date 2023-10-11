rem                  MakeMS32.bat                        2005-07-17 AgF

rem  Makes TSC test program for 32 bit Windows from TSCTest.cpp
rem  with Microsoft C++ compiler


rem  Set path to MS driver development kit or other C++ compiler. 
rem  You have to change this to the appropriate directory
set  CompilerDir=C:\WINDDK\3790

rem  Set path to compiler and linker. You may have to change this
path %CompilerDir%\bin\x86

rem  Set path to *.h include files. You may have to change this
set include=%CompilerDir%\inc\w2k;%CompilerDir%\inc\ddk\w2k;%CompilerDir%\inc\wnet;%CompilerDir%\inc\crt;

rem  Set path to *.lib library files
set lib=%CompilerDir%\lib\w2k;%CompilerDir%\lib\w2k\i386

rem  Compile cpp file
cl TSCTest.cpp
if errorlevel 1 pause

rem  run new program
if not errorlevel 1 TSCTest.exe
pause


