rem                  MakeBor32.bat                        2005-07-27 AgF

rem  Makes TSC test program for 32 bit Windows from TSCTest.cpp
rem  with Borland C++ compiler.
rem  Requires alib.h and alibOMF.lib from alib.zip.


rem  Set path to Borland C++ compiler. 
rem  You have to change this to the appropriate directory
rem  set  CompilerDir=C:\BC5
set  CompilerDir=C:\Program Files\Borland\CBuilder

rem  Set path to compiler and linker
path %CompilerDir%\bin

rem  Set path to *.h include files
set include=%CompilerDir%\include

rem  Set path to *.lib library files
set lib=%CompilerDir%\lib

rem  Compile cpp file
bcc32 -DUSE_ALIB=1 TSCTest.cpp alibOMF.lib
if errorlevel 1 pause

rem  run new program
if not errorlevel 1 TSCTest.exe
pause


