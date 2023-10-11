@echo off
rem                   MakeI64.bat                        2005-07-17 AgF

rem  Makes TSC test program for 64 bit Windows from TSCTest.cpp
rem  using Intel C++ compiler.

rem  System requirements:
rem  Windows XP x64 or later
rem  Microsoft Windows server 2003 Platform SDK, SP1 or later
rem  Intel C++ compiler for Windows version 9.0 or later

rem  Set path to MS platform software development kit. 
rem  You may have to change this to the appropriate directory:
set SDKDir=%ProgramFiles%\Microsoft Platform SDK

rem  Set path to Intel compiler.
rem  You may have to change this to the appropriate directory:
SET ICPP=%ProgramFiles(x86)%\Intel\Compiler\C++\9.0

rem  Set path to Intel license file if not defined
IF NOT DEFINED INTEL_LICENSE_FILE  SET INTEL_LICENSE_FILE=%CommonProgramFiles(x86)%\Intel\Licenses

rem  Set path to compiler and linker.
Path %SDKDir%\Bin\win64\x86\AMD64;%ICPP%\EM64T\Bin;

rem  Set path to *.h include files.
SET Include=%SDKDir%\Include;%SDKDir%\Include\crt

rem  Set path to *.lib library files
Set Lib=%SDKDir%\Lib\AMD64;%ICPP%\EM64T\Lib

rem  Delete old program
if EXIST TSCTest.exe  del TSCTest.exe

rem  Compile cpp file
icl TSCTest.cpp
if errorlevel 1 pause

rem  Run new program
if not errorlevel 1  TSCTest.exe
pause

