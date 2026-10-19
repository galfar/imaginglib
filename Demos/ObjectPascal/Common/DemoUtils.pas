unit DemoUtils;

{$I ImagingOptions.inc}

interface

uses
  SysUtils,
  Classes,
  ImagingTypes,
  Imaging, 
  ImagingUtility;

const
  SDataDir = 'Data';
  SSourceDir = 'Source';


function ExpandFileTo(const FileName, BasePath: string): string;
function SwapPathDelims(const FileName: string; const NewDelim: string = PathDelim): string;

function GetDataDir: string;
function FileNameInDataDir(const BaseFileName: string): string; {$IFDEF USE_INLINE}inline;{$ENDIF}

function GetRootDir: string;

{ Returns next valid image format.}
function NextFormat(Format: TImageFormat): TImageFormat;

{ Returns description of the build: target CPU and OS, compiler and its
  version, and build type, e.g. 'x64 Windows, FPC 3.2.2, Release'.
  Meant to be shown in titles of demos.}
function GetBuildInfo: string;

{ Returns third party libraries (codecs) in use and where they come from,
  e.g. 'zlib: FPC paszlib 1.2, JPEG: Pascal, TIFF: PasTiff, ...'. Backends
  selected at run time are asked for, so shared libraries are reported
  only when they are found and used.}
function GetLibsInfo: string;

implementation

{$IF not Defined(DONT_LINK_EXT_FILE_FORMATS) and not Defined(DONT_LINK_FILE_FORMATS)}
  {$IFNDEF DONT_LINK_TIFF}
    {$DEFINE LIBS_INFO_TIFF}
  {$ENDIF}
  {$IFNDEF DONT_LINK_WEBP}
    {$DEFINE LIBS_INFO_WEBP}
  {$ENDIF}
{$IFEND}

uses
  ImagingZLib,
{$IFDEF LIBS_INFO_TIFF}
  ImagingTiffBackend,
  {$IFDEF HAS_DYNLIBS}
  LibTiffDynLib,
  {$ENDIF}
{$ENDIF}
{$IFDEF LIBS_INFO_WEBP}
  ImagingLibWebP,
  {$IFDEF HAS_DYNLIBS}
  LibWebPDynLib,
  {$ENDIF}
{$ENDIF}
  ImagingJpegLib;

const
  SBuildCpu =
  {$IF Defined(CPUX64)}
    'x64'
  {$ELSEIF Defined(CPUX86)}
    'x86'
  {$ELSEIF Defined(CPUAARCH64) or Defined(CPUARM64)}
    'ARM64'
  {$ELSEIF Defined(CPUARM) or Defined(CPUARM32)}
    'ARM'
  {$ELSEIF Defined(CPUPOWERPC64) and Defined(FPC_LITTLE_ENDIAN)}
    'PowerPC64LE'
  {$ELSEIF Defined(CPURISCV64)}
    'RISC-V64'
  {$ELSEIF Defined(CPUWASM32)}
    'WASM32'
  {$ELSEIF Defined(FPC)}
    {$I %FPCTARGETCPU%}
  {$ELSE}
    'unknown CPU'
  {$IFEND};

  SBuildOs =
  {$IF Defined(ANDROID)}
    'Android'
  {$ELSEIF Defined(MSWINDOWS)}
    'Windows'
  {$ELSEIF Defined(LINUX)}
    'Linux'
  {$ELSEIF Defined(IOS)}
    'iOS'
  {$ELSEIF Defined(MACOS)}
    'macOS'
  {$ELSEIF Defined(FREEBSD)}
    'FreeBSD'
  {$ELSEIF Defined(WASI)}
    'WASI'
  {$ELSEIF Defined(FPC)}
    {$I %FPCTARGETOS%}
  {$ELSE}
    'unknown OS'
  {$IFEND};

  SBuildType = {$IFDEF DEBUG}'Debug'{$ELSE}'Release'{$ENDIF};

function ExpandFileTo(const FileName, BasePath: string): string;
var
 OldPath: string;
begin
  GetDir(0, OldPath);
  try
   if SysUtils.DirectoryExists(BasePath) then
   begin
     ChDir(BasePath);
     Result:= ExpandFileName(FileName);
   end
   else
    Result:=FileName;
  finally
   ChDir(OldPath);
  end;
end;

function SwapPathDelims(const FileName, NewDelim: string): string;
begin
  Result := FileName;
  Result := StringReplace(Result, '\', NewDelim, [rfReplaceAll]);
  Result := StringReplace(Result, '/', NewDelim, [rfReplaceAll]);
end;

function GetDataDir: string;
var
  Iter: Integer;
begin
  Iter := 0;
  Result := GetAppDir;
  while not DirectoryExists(Result + PathDelim + SDataDir) and (Iter < 7) do
  begin
    Result := ExtractFileDir(Result);
    Inc(Iter);
  end;
  Result := Result + PathDelim + SDataDir;
end;

function FileNameInDataDir(const BaseFileName: string): string;
begin
  Result := GetDataDir + PathDelim + BaseFileName;
end;

function GetRootDir: string;
var
  Iter: Integer;
begin
  Iter := 0;
  Result := GetAppDir;
  while not DirectoryExists(Result + PathDelim + SSourceDir) and (Iter < 7) do
  begin
    Result := ExtractFileDir(Result);
    Inc(Iter);
  end;
end;

function NextFormat(Format: TImageFormat): TImageFormat;
var
  Info: TImageFormatInfo;
begin
  repeat
    if Format < High(TImageFormat) then
      Format := Succ(Format)
    else
      Format := ifIndex8;
  until GetImageFormatInfo(Format, Info);
  Result := Format;
end;

function GetBuildInfo: string;
var
  Compiler: string;
begin
{$IFDEF FPC}
  Compiler := 'FPC ' + {$I %FPCVERSION%};
{$ELSE}
  Compiler := 'DCC ' + SysUtils.Format('%.1f', [CompilerVersion], GetFormatSettingsForFloats);
{$ENDIF}
  Result := SBuildCpu + ' ' + SBuildOs + ', ' + Compiler + ', ' + SBuildType;
end;

function GetLibsInfo: string;
const
  SZLibBackends: array[TZLibBackend] of string = ('bundled', 'FPC paszlib',
    'Delphi RTL', 'dynlib');
  SNotLinked = 'not linked';
var
  S: string;
begin
  Result := 'zlib: ' + SZLibBackends[ZLibBackend] + ' ' + GetZLibVersion;

  if JpegBackend = jbTurboJpeg then
    S := 'TurboJPEG dynlib'
  else
    S := 'Pascal';
  Result := Result + ', JPEG: ' + S;

{$IFDEF LIBS_INFO_TIFF}
  case TiffBackend of
    tbPasTiff: S := 'PasTiff';
  {$IFDEF HAS_DYNLIBS}
    tbLibTiff: S := 'libtiff ' + GetLibTiffVersion + ' dynlib';
  {$ENDIF}
  else
    S := 'none';
  end;
{$ELSE}
  S := SNotLinked;
{$ENDIF}
  Result := Result + ', TIFF: ' + S;

{$IFDEF LIBS_INFO_WEBP}
  case WebPBackend of
    wbPascal: S := 'Pascal (load only)';
  {$IFDEF HAS_DYNLIBS}
    wbLibWebP: S := 'libwebp ' + GetLibWebPVersion + ' dynlib';
  {$ENDIF}
  else
    S := 'none';
  end;
{$ELSE}
  S := SNotLinked;
{$ENDIF}
  Result := Result + ', WebP: ' + S;

  // OpenJPEG objects are linked only on some platforms, the format unit
  // is empty elsewhere
  if FindImageFileFormatByExt('jp2') <> nil then
    S := 'OpenJPEG'
  else
    S := SNotLinked;
  Result := Result + ', JPEG 2000: ' + S;
end;

end.
