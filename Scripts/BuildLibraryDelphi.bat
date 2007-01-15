@echo OFF
echo Building VampyreImaging.dll using Delphi

set ROOTDIR=..
set LIBFILE=%ROOTDIR%\Source\Projects\VampyreImaging.dpr
set OUTPUT=-E%ROOTDIR%\Bin
set UNITS=-U%ROOTDIR%\Source -U%ROOTDIR%\Source\JpegLib -U%ROOTDIR%\Source\ZLib 
set UNITS=%UNITS% -U%ROOTDIR%\Source\Extensions -U%ROOTDIR%\Extras\Extensions
set INCLUDE=-I%ROOTDIR%\Source 
set OPTIONS=

dcc32 %OPTIONS% %LIBFILE% %OUTPUT% %UNITS% %INCLUDE%

if errorlevel 1 goto ERROR
goto SUCC

:ERROR
  echo Error when building library!
  goto END
:SUCC
  echo Library successfuly build in Bin directory
  goto END
:END

call Clean.bat