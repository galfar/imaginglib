@echo OFF
echo Building Demos using Free Pascal

set ROOTDIR=..
set DEMOPATH=%ROOTDIR%\Demos\ObjectPascal
set UNITS=-Fu%ROOTDIR%\Source -Fu%ROOTDIR%\Source\JpegLib -Fu%ROOTDIR%\Source\ZLib -Fu%ROOTDIR%\Extras\Extensions -Fu%DEMOPATH%\Common
set INCLUDE=-Fi%ROOTDIR%\Source 
set OUTPUT=-FE%ROOTDIR%\Demos\Bin
set OPTIONS=-Sgi2dh -OG2p3 -Xs 
set TARGET=-Tgo32v2

set DEMOSBUILD=0
set DEMOCOUNT=2

call :BUILD Benchmark\Bench.dpr -oBenchDOS.exe
call :BUILD VampConvert\VampConvert.dpr -oVampConvertDOS.exe

goto END

:BUILD
  fpc %TARGET% %OPTIONS% %OUTPUT% "%DEMOPATH%\%1" %INCLUDE% %UNITS% %2
  if errorlevel 1 (echo Error when building %1) else (set /a DEMOSBUILD+=1)
goto :EOF

:END
if "%DEMOSBUILD%"=="%DEMOCOUNT%" (
  echo Build Successful - all %DEMOSBUILD% of %DEMOCOUNT% build
) else (
  echo Errors during building - only %DEMOSBUILD% of %DEMOCOUNT% demos build
)

call Clean.bat
