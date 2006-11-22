unit DemoUtils;

{ $I ImagingOptions.inc}

interface

uses
  SysUtils,
  Classes,
  ImagingTypes,
  Imaging, 
  ImagingUtility;

const
  SDataDir = 'Data';

type
  { Options for BuildFileList function:
    flFullNames - file names in result will have full path names
                (ExtractFileDir(Path)+FileName)
    flRelNames  - file names in result will have names relative to
                ExtractFileDir(Path) dir
    flRecursive - adds files in subdirectories foun in Path.}
  TFileListOption = (flFullNames, flRelNames, flRecursive);
  TFileListOptions = set of TFileListOption;

{ This function fills Files string list with names of files found
  with FindFirst/FindNext functions (See details on Path/Atrr here).
  - BuildFileList('c:\*.*',faAnyFile, List, [flRecursive]) returns
    list of all files (only name.ext - no path) on C drive
  - BuildFileList('d:\*.*',faDirectory, List, [flFullNames]) returns
    list of all directories (d:\dirxxx) in root of D drive.}
function BuildFileList(Path: string; Attr: LongInt; Files: TStrings;
  Options: TFileListOptions = []): Boolean;
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

function BuildFileList(Path: string; Attr: LongInt;
  Files: TStrings; Options: TFileListOptions): Boolean;
var
  FileMask: string;
  RootDir: string;
  Folders: TStringList;
  CurrentItem: LongInt;
  Counter: LongInt;
  LocAttr: LongInt;

  procedure BuildFolderList;
  var
    FindInfo: TSearchRec;
    Rslt: LongInt;
  begin
    Counter := Folders.Count - 1;
    CurrentItem := 0;
    while CurrentItem <= Counter do
    begin
      // searching for subfolders
      Rslt := FindFirst(Folders[CurrentItem] + '*', faDirectory, FindInfo);
      try
        while Rslt = 0 do
        begin
          if (FindInfo.Name <> '.') and (FindInfo.Name <> '..') and
            (FindInfo.Attr and faDirectory = faDirectory) then
            Folders.Add(Folders[CurrentItem] + FindInfo.Name + PathDelim);
          Rslt := FindNext(FindInfo);
        end;
      finally
        FindClose(FindInfo);
      end;
      Counter := Folders.Count - 1;
      Inc(CurrentItem);
    end;
  end;

  procedure FillFileList(CurrentCounter: LongInt);
  var
    FindInfo: TSearchRec;
    Res: LongInt;
    CurrentFolder: string;
  begin
    CurrentFolder := Folders[CurrentCounter];
    Res := FindFirst(CurrentFolder + FileMask, LocAttr, FindInfo);
    if flRelNames in Options then
      CurrentFolder := ExtractRelativePath(RootDir, CurrentFolder);
    try
      while Res = 0 do
      begin
        if (FindInfo.Name <> '.') and (FindInfo.Name <> '..') then
        begin
          if (flFullNames in Options) or (flRelNames in Options) then
            Files.Add(CurrentFolder + FindInfo.Name)
          else
            Files.Add(FindInfo.Name);
        end;
        Res := FindNext(FindInfo);
      end;
    finally
      FindClose(FindInfo);
    end;
  end;

begin
  FileMask := ExtractFileName(Path);
  RootDir := ExtractFilePath(Path);
  Folders := TStringList.Create;
  Folders.Add(RootDir);
  Files.Clear;
{$IFDEF DCC}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}
  if Attr = faAnyFile then
    LocAttr := faSysFile or faHidden or faArchive or faReadOnly
  else
    LocAttr := Attr;
{$IFDEF DCC}
  {$WARN SYMBOL_PLATFORM ON}
{$ENDIF}
  // here's the recursive search for nested folders
  if flRecursive in Options then
    BuildFolderList;
  if Attr <> faDirectory then
    for Counter := 0 to Folders.Count - 1 do
      FillFileList(Counter)
  else
    Files.AddStrings(Folders);
  Folders.Free;
  Result := True;
end;

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
