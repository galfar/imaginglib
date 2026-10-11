@echo OFF
setlocal
echo Building Unit Tests using Delphi
echo.

rem Set DCC to use other compiler than dcc32 from PATH, for example:
rem   set DCC=dcc64
rem   set DCC="C:\Program Files (x86)\Embarcadero\RAD Studio\10.0\bin\dcc32.exe"
rem DUnit is needed: it comes with Delphi 2005 and newer. For Delphi 7 get
rem DUnit 9.x sources and set DUNIT_DIR to its "src" directory.
if not defined DCC set DCC=dcc32

rem Compiler is run in the directory of the project, older Delphi versions
rem do not find units listed in the project file otherwise
set ROOTDIR=..\..
set TESTEXE=%ROOTDIR%\Bin\ImagingTests.exe
set OUTPUT=-E%ROOTDIR%\Bin
set INCLUDE=-I%ROOTDIR%\Source
set UNITOUT=-N%ROOTDIR%\Bin\Dcu
rem -NS Unit scopes are needed for newer Delphi and argument is happily ignored by D7
set OPTIONS=-B -$D+ -$L+ -$C+ -$R+ -$Q+ -$O- -Q -DDEBUG -NSSystem;Winapi

set DEFINES=-DLINK_ALL_FILE_FORMATS
set UNITS=-U%ROOTDIR%\Source -U%ROOTDIR%\Source\Libs -U%ROOTDIR%\Extensions -U%ROOTDIR%\Extensions\Libs -U%ROOTDIR%\Extensions\LibTiff
if defined DUNIT_DIR set UNITS=%UNITS% -U"%DUNIT_DIR%"

pushd "%~dp0..\Tests\UnitTests"

mkdir %ROOTDIR%\Bin\Dcu 2>nul
del %TESTEXE% 2>nul

%DCC% %OPTIONS% ImagingTests.dpr %OUTPUT% %UNITS% %INCLUDE% %DEFINES% %UNITOUT%

rem Some Delphi editions cannot compile from command line and still return success
if errorlevel 1 goto FAILED
if not exist %TESTEXE% goto FAILED

popd
echo.
echo Build Successful - run Bin\ImagingTests.exe
exit /b 0

:FAILED
popd
echo Errors during building of unit tests
exit /b 1
