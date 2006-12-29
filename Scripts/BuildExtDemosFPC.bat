@echo OFF
echo Building Extension Demos using Free Pascal

rem Important! Set this dirs on your system for the demos to compile!
set SDLDIR=
set OPENGLDIR=
set D3DDIR=

set ROOTDIR=..
set DEMOPATH=%ROOTDIR%\Demos\ObjectPascal
set UNITS=-Fu%ROOTDIR%\Source -Fu%ROOTDIR%\Source\JpegLib -Fu%ROOTDIR%\Source\ZLib -Fu%DEMOPATH%\Common
set UNITS=%UNITS% -Fu%ROOTDIR%\Source\Extensions -Fu%ROOTDIR%\Extras\Extensions -Fu"%SDLDIR%" -Fu"%OPENGLDIR%" -Fu"%D3DDIR%"
set INCLUDE=-Fi%ROOTDIR%\Source -Fi"%SDLDIR%" -Fi"%OPENGLDIR%" -Fi"%D3DDIR%"
set OUTPUT=-FE%ROOTDIR%\Demos\Bin
set OPTIONS=-Sgi2dh -OG2p3 -Xs
set TARGET=-Twin32

set DEMOSBUILD=0
set DEMOCOUNT=3

set CURRDEMO=SDLDemo\SDLDemo.dpr
if "%SDLDIR%"=="" (echo SDL search directory not set - skipping %CURRDEMO%) else (call :BUILD %CURRDEMO% -oSDLDemo.exe)

set CURRDEMO=OpenGLDemo\OpenGLDemo.dpr
if "%OPENGLDIR%"=="" (echo OpenGL search directory not set - skipping %CURRDEMO%) else (call :BUILD %CURRDEMO% -oOpenGLDemo.exe)

set CURRDEMO=D3DDemo\D3DDemo.dpr
if "%D3DDIR%"=="" (echo D3D search directory not set - skipping %CURRDEMO%) else (call :BUILD %CURRDEMO% -oD3DDemo.exe)

goto END

:BUILD
  fpc %TARGET% %OPTIONS% %OUTPUT% "%DEMOPATH%\%1" %UNITS% %INCLUDE% %2
  if errorlevel 1 (echo Error when building %1) else (set /a DEMOSBUILD+=1)
goto :EOF

:END
if "%DEMOSBUILD%"=="%DEMOCOUNT%" (
  echo Build Successful - all %DEMOSBUILD% of %DEMOCOUNT% build
) else (
  echo Errors during building - only %DEMOSBUILD% of %DEMOCOUNT% demos build
)

call Clean.bat
