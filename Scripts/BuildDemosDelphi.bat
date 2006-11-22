@echo OFF
echo Building Demos using Delphi

set ROOTDIR=..
set DEMOPATH=%ROOTDIR%\Demos\ObjectPascal
set OUTPUT=-E%ROOTDIR%\Demos\Bin
set UNITS=-U%ROOTDIR%\Source -U%ROOTDIR%\Source\JpegLib -U%ROOTDIR%\Source\ZLib -U%DEMOPATH%\Common
set INCLUDE=-I%ROOTDIR%\Source 
set OPTIONS=

set DEMOSBUILD=0
set DEMOCOUNT=3

call :BUILD Benchmark\Bench.dpr
call :BUILD VampConvert\VampConvert.dpr
call :BUILD VCLImageBrowser\ImgBrowser.dpr

goto END

:BUILD
  dcc32 %OPTIONS% %DEMOPATH%\%1 %OUTPUT% %UNITS% %INCLUDE%
  if errorlevel 1 (echo Error when building %1) else (set /a DEMOSBUILD+=1)
goto :EOF

:END
if "%DEMOSBUILD%"=="%DEMOCOUNT%" (
  echo Build Successful - all %DEMOSBUILD% of %DEMOCOUNT% build
) else (
  echo Errors during building - only %DEMOSBUILD% of %DEMOCOUNT% demos build
)

call Clean.bat
