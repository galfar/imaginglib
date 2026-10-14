@echo OFF
setlocal
echo Building Demos using Delphi
echo.

rem Set DCC to use other compiler than dcc32 from PATH, for example:
rem   set DCC=dcc64
rem   set DCC="C:\Program Files (x86)\Borland\Delphi7\Bin\dcc32.exe"
if not defined DCC set DCC=dcc32

rem Compiler is run in the directory of each demo, older Delphi versions
rem do not find units listed in the project file otherwise
set ROOTDIR=..\..\..
set BINDIR=%ROOTDIR%\Demos\Bin
set OUTPUT=-E%BINDIR%
set INCLUDE=-I%ROOTDIR%\Source
set UNITOUT=-N%BINDIR%\Dcu
rem -NS Unit scopes are needed for newer Delphi and argument is happily ignored by D7
set OPTIONS=-B -$D- -$L- -$Y- -Q -DRELEASE -NSSystem;Winapi;Vcl;Vcl.Shell

set DEFINES=-DLINK_ALL_FILE_FORMATS
set UNITS=-U%ROOTDIR%\Source -U%ROOTDIR%\Source\Libs -U%ROOTDIR%\Extensions -U%ROOTDIR%\Extensions\Libs -U%ROOTDIR%\Extensions\LibTiff
set UNITS=%UNITS% -U..\Common

set DEMOSBUILD=0
set DEMOCOUNT=3

call :BUILD Benchmark Bench
call :BUILD VampConvert VampConvert
call :BUILD ImageBrowser ImgBrowser

goto END

:BUILD
  pushd "%~dp0..\Demos\ObjectPascal\%1"
  mkdir %BINDIR%\Dcu 2>nul
  del %BINDIR%\%2.exe 2>nul
  %DCC% %OPTIONS% %2.dpr %OUTPUT% %UNITS% %INCLUDE% %DEFINES% %UNITOUT%
  rem Some Delphi editions (CE, Trials) cannot compile from command line and still return success
  if errorlevel 1 (
    echo Error when building %2
  ) else if not exist %BINDIR%\%2.exe (
    echo Error when building %2
  ) else (
    set /a DEMOSBUILD+=1
  )
  popd
  echo.
goto :EOF

:END
if "%DEMOSBUILD%"=="%DEMOCOUNT%" (
  echo [92mBuild Successful - all %DEMOSBUILD% of %DEMOCOUNT% build in Demos/Bin directory[0m
) else (
  echo [91mErrors during building - only %DEMOSBUILD% of %DEMOCOUNT% demos build[0m
  exit /b 1
)
