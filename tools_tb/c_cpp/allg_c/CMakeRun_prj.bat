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
cmake .. -G"Visual Studio 14 2015"
PAUSE