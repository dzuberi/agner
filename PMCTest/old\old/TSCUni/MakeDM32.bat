rem                  MakeDM32.bat                        2005-07-17 AgF

rem  Makes TSC test program for 32 bit Windows from TSCTest.cpp
rem  with Digital Mars C++ compiler

rem  Set path to Digital Mars directory
rem  You may have to change this to the appropriate directory
set DMARS=%ProgramFiles%\DigitalMars

rem  Set path to compiler and linker.
path %DMARS%\bin

rem  Set path to *.h include files.
set include=%DMARS%\include

rem  Set path to *.lib library files
set lib=%DMARS%\lib

rem  Compile cpp file
dmc TSCTest.cpp
if errorlevel 1 pause

rem  run new program
if not errorlevel 1 TSCTest.exe
pause


