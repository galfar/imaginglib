{
  Vampyre Imaging Library
  by Marek Mauder
  https://github.com/galfar/imaginglib
  https://imaginglib.sourceforge.io
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.
}

{ Checks for builds that use third party shared libraries (zlib, TurboJPEG,
  libwebp, libtiff) instead of the code compiled into Imaging. Most of them
  fall back to the Pascal code silently when a library is not found, so
  the other tests would pass without testing the library at all; these tests
  fail then.

  Registered only with --dynlibs option. The build must define
  ZLIB_USE_DYNLIB, JPEG_USE_TURBO_DYNLIB and TIFF_USE_LIBTIFF_DYNLIB (libwebp
  is used whenever it is found); Scripts/CI.DynLibs.sh builds and runs it.}
unit TestImagingDynLibs;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, ImagingTypes, Imaging, ImagingJpegLib, ImagingZLib,
{$IFDEF ZLIB_USE_DYNLIB}
  ZLibDynLib,
{$ENDIF}
{$IFNDEF DONT_LINK_EXT_FILE_FORMATS}
  ImagingLibWebP, LibWebPDynLib, ImagingTiffBackend,
  {$IFDEF TIFF_USE_LIBTIFF_DYNLIB}
  LibTiffDynLib,
  {$ENDIF}
{$ENDIF}
  ImagingTestCase;

type
  TTestDynLibs = class(TImagingTestCase)
  published
    procedure TestZLib;
    procedure TestTurboJpeg;
    procedure TestLibWebP;
    procedure TestLibTiff;
  end;

{ Returns True when the test program was started with --dynlibs option.}
function DynLibTestsRequested: Boolean;

implementation

function DynLibTestsRequested: Boolean;
begin
  // Second dash is taken as a part of the switch name
  Result := FindCmdLineSwitch('-dynlibs', ['-'], True);
end;

{ The library versions go to the output, to see in CI logs what was tested.}
procedure Report(const Name, Version: string);
begin
  WriteLn(Name, ' in use: ', Version);
end;

procedure TTestDynLibs.TestZLib;
begin
{$IFDEF ZLIB_USE_DYNLIB}
  // The library is loaded at start, the program does not run without it
  CheckTrue(Assigned(zlibVersion), 'zlib library is loaded');
  CheckTrue(ZLibBackend = zbDynLib, 'zlib library is the backend');
  Report('zlib', string(AnsiString(zlibVersion)));
{$ELSE}
  Fail('Built without ZLIB_USE_DYNLIB');
{$ENDIF}
end;

procedure TTestDynLibs.TestTurboJpeg;
begin
{$IFDEF JPEG_USE_TURBO_DYNLIB}
  CheckTrue(JpegBackend = jbTurboJpeg, 'TurboJPEG library (2.0+) is found and used');
  Report('TurboJPEG', 'yes');
{$ELSE}
  Fail('Built without JPEG_USE_TURBO_DYNLIB');
{$ENDIF}
end;

procedure TTestDynLibs.TestLibWebP;
begin
{$IFNDEF DONT_LINK_EXT_FILE_FORMATS}
  CheckTrue(WebPBackend = wbLibWebP, 'libwebp library is found and used');
  Report('libwebp', GetLibWebPVersion);
{$ELSE}
  Fail('Built without extension file formats');
{$ENDIF}
end;

procedure TTestDynLibs.TestLibTiff;
begin
{$IF not Defined(DONT_LINK_EXT_FILE_FORMATS) and Defined(TIFF_USE_LIBTIFF_DYNLIB)}
  CheckTrue(TiffBackend = tbLibTiff, 'libtiff library (4.0+) is found and used');
  Report('libtiff', GetLibTiffVersion);
{$ELSE}
  Fail('Built without TIFF_USE_LIBTIFF_DYNLIB');
{$IFEND}
end;

initialization
  if DynLibTestsRequested then
    RegisterImagingTest(TTestDynLibs);
end.
