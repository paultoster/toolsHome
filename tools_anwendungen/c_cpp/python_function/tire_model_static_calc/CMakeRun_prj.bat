@ECHO OFF
TITLE %~nx0
SET PR=prj
SET PRDIR=.\%PR%

IF NOT EXIST %PRDIR% (
ECHO.
ECHO Creating new build dir: %PRDIR%
MKDIR %PRDIR%
)

CD %PRDIR%

ECHO.
cmake .. -G "Visual Studio 14 2015" -A x64
PAUSE


rem cmake -G "Visual Studio 16 2019" -A Win32 -S \path_to_source\ -B "build32"
rem cmake -G "Visual Studio 16 2019" -A x64 -S \path_to_source\ -B "build64"
rem cmake --build build32 --config Release
rem cmake --build build64 --config Release