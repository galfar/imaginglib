{
  $Id$
  Vampyre Imaging Library
  by Marek Mauder
  http://imaginglib.sourceforge.net

  The contents of this file are used with permission, subject to the Mozilla
  Public License Version 1.1 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  Alternatively, the contents of this file may be used under the terms of the
  GNU Lesser General Public License (the  "LGPL License"), in which case the
  provisions of the LGPL License are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the LGPL License and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting  the provisions above and
  replace  them with the notice and other provisions required by the LGPL
  License.  If you do not delete the provisions above, a recipient may use
  your version of this file under either the MPL or the LGPL License.

  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
}

{ This unit contains image format loader for X Window Pixmap images.}
unit ImagingXpm;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, ImagingTypes, Imaging, ImagingUtility, ImagingFormats,
  ImagingIO, ImagingCanvases;

type
  { Class for loading X Window Pixmap images known as XPM.
    It is ASCII-text-based format, basicaly a fragment of C code
    declaring static array. Loaded image is in ifA8R8G8B8 data format.
    Only loading is supported now.}
  TXPMFileFormat = class(TImageFileFormat)
  protected
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
  public
    constructor Create; override;
    function TestFormat(Handle: TImagingHandle): Boolean; override;
  end;

implementation

const
  SXPMFormatName = 'X Window Pixmap';
  SXPMMasks      = '*.xpm';

const
  SXPMId = '/* XPM */';
  WhiteSpaces = [#9, #10, #13, #32];

type
  TColorHolder = class
  public
    Color: TColor32;
  end;

{
  TXPMFileFormat implementation
}

constructor TXPMFileFormat.Create;
begin
  inherited Create;
  FName := SXPMFormatName;
  FCanLoad := True;
  FCanSave := False;
  FIsMultiImageFormat := False;

  AddMasks(SXPMMasks);
end;

function TXPMFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Contents, PalLookup: TStringList;
  S: AnsiString;
  I, J, NumColors, Cpp, Line: Integer;

  procedure SkipWhiteSpace(var Line: string);
  begin
    while (Length(Line) > 0) and (Line[1] in WhiteSpaces) do
      Delete(Line, 1, 1);
  end;

  function ReadString(var Line: string): string;
  begin
    Result := '';
    SkipWhiteSpace(Line);
    while (Length(Line) > 0) and (Line[1] in WhiteSpaces) do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result)] := Line[1];
      Delete(Line, 1, 1);
    end;
  end;

  function ReadInt(var Line: string): Integer;
  begin
    Result := StrToInt(ReadString(Line));
  end;

  function ParseHeader: Boolean;
  var
    S: string;
  begin
    S := Contents[0];
    try
      Images[0].Width := ReadInt(S);
      Images[0].Height := ReadInt(S);
      NumColors := ReadInt(S);
      Cpp := ReadInt(S);
      Line := 1;
      Result := True;
    except
      Result := False;
    end;
  end;

  function NamedToColor(const ColStr: string): TColor32;
  var
    S: string;
  begin
    S := LowerCase(ColStr);
    if (S = 'transparent') or (S = 'none') then
      Result := pcClear
    else if S = 'black' then
      Result := pcBlack
    else if S = 'blue' then
      Result := pcBlue
    else if S = 'green' then
      Result := pcGreen
    else if S = 'cyan' then
      Result := pcAqua
    else if S = 'red' then
      Result := pcRed
    else if S = 'magenta' then
      Result := pcFuchsia
    else if S = 'yellow' then
      Result := pcYellow
    else if S = 'white' then
      Result := pcWhite
    else if S = 'gray' then
      Result := pcLtGray
    else if S = 'dkblue' then
      Result := pcNavy
    else if S = 'dkgreen' then
      Result := pcGreen
    else if S = 'dkcyan' then
      Result := pcTeal
    else if S = 'dkred' then
      Result := pcMaroon
    else if S = 'dkmagenta' then
      Result := pcPurple
    else if S = 'dkyellow' then
      Result := pcOlive
    else if S = 'maroon' then
      Result := pcMaroon
    else if S = 'olive' then
      Result := pcOlive
    else if S = 'navy' then
      Result := pcNavy
    else if S = 'purple' then
      Result := pcPurple
    else if S = 'teal' then
      Result := pcTeal
    else if S = 'silver' then
      Result := pcSilver
    else if S = 'lime' then
      Result := pcLime
    else if S = 'fuchsia' then
      Result := pcFuchsia
    else if S = 'aqua' then
      Result := pcAqua
    else
      Result := pcClear;
  end;

  procedure ParsePalette;
  var
    I: Integer;
    S, ColType, ColStr, Code: string;
    Color: TColor32;
    Holder: TColorHolder;
  begin
    for I := 0 to NumColors - 1 do
    begin
      Holder := TColorHolder.Create;
      // Parse pixel code and color
      S := Contents[Line + I];
      Code := Copy(S, 1, Cpp);
      Delete(S, 1, Cpp);
      ColType := ReadString(S);
      ColStr := ReadString(S);
      // Convert color from hex number or named constant
      if ColStr[1] = '#' then
      begin
        Delete(ColStr, 1, 1);
        Color := LongWord(StrToInt('$' + ColStr)) or $FF000000;
      end
      else
        Color := NamedToColor(ColStr);
      // Store code and color in table for later lookup
      Holder.Color := Color;
      PalLookup.AddObject(Code, Holder);
    end;
    Inc(Line, NumColors);
  end;

  procedure ParsePixels;
  var
    X, Y, Idx: Integer;
    S, Code: string;
    Pix: PColor32;
  begin
    Pix := Images[0].Bits;
    for Y := 0 to Images[0].Height - 1 do
    begin
      S := Contents[Line + Y];
      for X := 0 to Images[0].Width - 1 do
      begin
        // Read code and look up color in the palette
        Code := Copy(S, X * Cpp + 1, Cpp);
        if PalLookup.Find(Code, Idx) then
          Pix^ := TColorHolder(PalLookup.Objects[Idx]).Color
        else
          Pix^ := pcClear;

        Inc(Pix);
      end;
    end;
  end;

begin
  Result := False;
  SetLength(Images, 1);
  with GetIO, Images[0] do
  begin
    // Look up table for XPM palette entries
    PalLookup := TStringList.Create;
    PalLookup.Sorted := True;
    PalLookup.CaseSensitive := True;
    // Read whole file and assign it to string list
    Contents := TStringList.Create;
    SetLength(S, GetInputSize(GetIO, Handle));
    Read(Handle, @S[1], Length(S));
    Contents.Text := S;
    // Remove quotes and other stuff
    for I := Contents.Count - 1 downto 0 do
    begin
      J := Pos('"', Contents[I]);
      if J > 0 then
        Contents[I] := Copy(Contents[I], J + 1, LastDelimiter('"', Contents[I]) - J - 1)
      else
        Contents.Delete(I);
    end;
    // Parse header and create new image
    if not ParseHeader then
      Exit;
    NewImage(Width, Height, ifA8R8G8B8, Images[0]);
    // Read palette entries and assign colors to pixels
    ParsePalette;
    ParsePixels;

    Contents.Free;
    for I := 0 to PalLookup.Count - 1 do
      PalLookup.Objects[I].Free;
    PalLookup.Free;
    Result := True;
  end;
end;

function TXPMFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Id: array[0..8] of AnsiChar;
  ReadCount: Integer;
begin
  Result := False;
  if Handle <> nil then
  begin
    ReadCount := GetIO.Read(Handle, @Id, SizeOf(Id));
    GetIO.Seek(Handle, -ReadCount, smFromCurrent);
    Result := (Id = SXPMId) and (ReadCount = SizeOf(Id));
  end;
end;

initialization
  RegisterImageFileFormat(TXPMFileFormat);

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.25.0 Changes/Bug Fixes -----------------------------------
    - Added XPM loading.
    - Unit created.
}

end.
