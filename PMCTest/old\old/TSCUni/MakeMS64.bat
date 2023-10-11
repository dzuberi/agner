@echo off
rem                   MakeMS64.bat                        2005-07-27 AgF

rem  Makes TSC test program for 64 bit Windows from TSCTest.cpp
rem  using Microsoft C++ compiler.


rem  Set path to MS platform software development kit or other compiler package.
rem  You may have to change this to the appropriate directory:
set SDKDir=%ProgramFiles%\Microsoft Platform SDK

rem  Set path to compiler. You may have to change this
Path %SDKDir%\Bin\win64\x86\AMD64

rem  Set path to *.h include files.
SET Include=%SDKDir%\Include;%SDKDir%\Include\crt

rem  Set path to *.lib library files
Set Lib=%SDKDir%\Lib\AMD64

rem  Delete old program
if EXIST TSCTest.exe  del TSCTest.exe

rem  Compile cpp file and link with alib
cl /DUSE_ALIB TSCTest.cpp alibPE64.lib bufferoverflowU.lib
if errorlevel 1 pause

rem  Run new program
if not errorlevel 1  TSCTest.exe
pause

