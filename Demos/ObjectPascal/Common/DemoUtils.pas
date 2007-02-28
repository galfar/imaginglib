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

{ }
function ExpandFileTo(const FileName, BasePath: string): string;
{ }
function SwapPathDelims(const FileName: string; const NewDelim: string = PathDelim): string;
{ }
function GetDataDir: string;
{ }
function GetRootDir: string;
{ Returns next valid image format.}
function NextFormat(Format: TImageFormat): TImageFormat;

implementation

function ExpandFileTo(const FileName, BasePath: string): string;
var
 OldPath: string;
begin
  OldPath:= GetCurrentDir;
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
begin
  Result := GetAppDir + PathDelim + SDataDir;
  if not DirectoryExists(Result) then
    Result := ExtractFileDir(GetAppDir) + PathDelim + SDataDir;
  if not DirectoryExists(Result) then
    Result := ExtractFileDir(ExtractFileDir(GetAppDir)) + PathDelim + SDataDir;
end;

function GetRootDir: string;
begin
  Result := ExtractFileDir(GetAppDir);
  if not DirectoryExists(Result + PathDelim + 'Source') then
  begin
    Result := ExtractFileDir(Result);
    if not DirectoryExists(Result + PathDelim + 'Source') then
    begin
      Result := ExtractFileDir(Result);
      if not DirectoryExists(Result + PathDelim + 'Source') then
      begin
        Result := ExtractFileDir(Result);
        if not DirectoryExists(Result + PathDelim + 'Source') then
          Result := ExtractFileDir(Result);
      end;
    end;
  end;
end;

function NextFormat(Format: TImageFormat): TImageFormat;
var
  Info: TImageFormatInfo;
begin
  repeat
    if Format < High(TImageFormat) then
{$IFDEF DCC}
      Format := Succ(Format)
{$ELSE}
      Format := TImageFormat(Succ(LongInt(Format)))
{$ENDIF}
    else
      Format := ifIndex8;
  until GetImageFormatInfo(Format, Info);
  Result := Format;
end;

end.
