@echo off
rem                   MakeTSCTest32.bat                        2005-07-16 AgF

rem  Makes TSC test program for 32 bit Windows

rem  System requirements:
rem  Windows 2000 or NT or later
rem  Microsoft assembler v. 6.15 or later
rem  Microsoft Visual C++ compiler or other C++ compiler


rem  Set path to software development kit. 
rem  You have to change this to the appropriate directory:
set CompilerDir=C:\WINDDK\3790

rem  Set path to MS driver development kit or other C++ compiler and assembler 
rem  You have to change this to the appropriate directory
path %CompilerDir%\bin\x86

rem  Set path to *.h include files. You may have to change this
set include=%CompilerDir%\inc\w2k;%CompilerDir%\inc\ddk\w2k;%CompilerDir%\inc\wnet;%CompilerDir%\inc\crt;

rem  Set path to *.lib library files. You may have to change this
set lib=%CompilerDir%\lib\w2k;%CompilerDir%\lib\w2k\i386

rem  Delete old program
if EXIST TSCTest32.exe del TSCTest32.exe

rem  Compile cpp file. Not needed if TSCTestA.cpp unchanged
cl /c /FoTSCTestA32.obj TSCTestA.cpp
if errorlevel 1 pause

rem  Assemble asm file
ml /c /Cx /coff TSCTestB32.asm
if errorlevel 1 pause

rem  Link into .exe file
link /SUBSYSTEM:CONSOLE /out:TSCTest32.exe TSCTestA32.obj TSCTestB32.obj
if errorlevel 1 pause

rem  Delete TSCTestB32.obj because it interferes with IDE
del TSCTestB32.obj

rem  run new program
if not errorlevel 1 TSCTest32.exe
pause


