@echo off
rem                   MakeTSCTest64.bat                        2005-07-16 AgF

rem  Makes TSC test program for 64 bit Windows

rem  System requirements:
rem  Windows XP x64 or later
rem  Microsoft Windows server 2003 Platform SDK, SP1 or later

rem  Set path to MS platform software development kit. 
rem  You may have to change this to the appropriate directory:
set SDKDir=%ProgramFiles%\Microsoft Platform SDK

rem  Set path to compiler and linker.
Path %SDKDir%\Bin\win64\x86\AMD64

rem  Set path to *.h include files.
SET Include=%SDKDir%\Include;%SDKDir%\Include\crt

rem  Set path to *.lib library files
Set Lib=%SDKDir%\Lib\AMD64

rem  Delete old program
if EXIST TSCTest64.exe  del TSCTest64.exe

rem  Compile cpp file
cl /c TSCTestA.cpp
if errorlevel 1 pause

rem assemble asm file
ml64 /c /Cx /W3 /Zi TSCTestB64.asm


rem  Link together
link /SUBSYSTEM:CONSOLE /DEBUG /INCREMENTAL:NO /OUT:TSCTest64.exe TSCTestA.obj TSCTestB64.obj bufferoverflowU.lib
rem advapi32.lib
if errorlevel 1 pause

rem  Run new program
if not errorlevel 1  TSCTest64.exe
pause

