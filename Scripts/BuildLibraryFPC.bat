@echo OFF
echo Building VampyreImaging.dll using Free Pascal

set ROOTDIR=..
set LIBFILE=%ROOTDIR%\Source\Projects\VampyreImaging.dpr
set UNITS=-Fu%ROOTDIR%\Source -Fu%ROOTDIR%\Source\JpegLib -Fu%ROOTDIR%\Source\ZLib
set UNITS=%UNITS% -Fu%ROOTDIR%\Source\Extensions -Fu%ROOTDIR%\Extras\Extensions
set INCLUDE=-Fi%ROOTDIR%\Source 
set LIBS=-Fl%ROOTDIR%\Extras\Extensions\J2KObjects
set OUTPUT=-oVampyreImaging.dll -FE%ROOTDIR%\Bin
set OPTIONS=-Sgi -O2p3 -Xs

set TARGET=-Twin32

fpc %TARGET% %OPTIONS% %OUTPUT% %LIBFILE% %UNITS% %INCLUDE% %LIBS% 

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
