{
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

{ This unit contains image format loader/saver for Radiance HDR/RGBE images.}
unit ImagingRadiance;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, Imaging, ImagingTypes;

type
  { }
  THdrFileFormat = class(TImageFileFormat)
  protected
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); override;
  public
    function TestFormat(Handle: TImagingHandle): Boolean; override;
  end;

implementation

uses
  Math, ImagingUtility, ImagingIO;

const
  SHdrFormatName = 'Radiance HDR/RGBE';
  SHdrMasks      = '*.hdr';
  HdrSupportedFormats: TImageFormats = [ifR32G32B32F, ifB32G32R32F];

type
  TSignature = array[0..9] of AnsiChar;
  THdrFormat = (hfRgb, hfXyz);

  THdrHeader = record
    Format: THdrFormat;
    Width: Integer;
    Height: Integer;
  end;

const
  RadianceSignature: TSignature = '#?RADIANCE';
  RgbeSignature: TSignature = '#?RGBE';
  MaxLineLength = 256;
  SFmtRgbeRle = '32-bit_rle_rgbe';
  SFmtXyzeRle = '32-bit_rle_xyze';

resourcestring
  SErrorBadHeader = 'Bad HDR/RGBE header format.';
  SWringScanLineWidth = 'Wrong scanline width.';

{ THdrFileFormat }

procedure THdrFileFormat.Define;
begin
  inherited;
  FName := SHdrFormatName;
  FCanLoad := True;
  FCanSave := False;
  FSupportedFormats := HdrSupportedFormats;

  AddMasks(SHdrMasks);
end;

function THdrFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
type
  TRgbe = packed record
    R, G, B, E: Byte;
  end;
  PRgbe = ^TRgbe;
  TDynRgbeArray = array of TRgbe;
var
  Header: THdrHeader;
  IO: TIOFunctions;

  function ReadHeader: Boolean;
  const
    CommentIds: TAnsiCharSet = ['#', '!'];
  var
    Line: string;
    HasResolution: Boolean;
    Count, Idx: Integer;
    ValStr: string;
    ValFloat: Double;
  begin
    Result := False;
    HasResolution := False;
    Count := 0;

    repeat
      if not ReadLine(IO, Handle, Line) then
        Exit;

      Inc(Count);
      if Count > 16 then // Too long header for HDR
        Exit;

      if Length(Line) = 0 then
        Continue;
      if AnsiChar(Line[1]) in CommentIds then
        Continue;

      if StrMaskMatch(Line, 'Format=*') then
      begin
        // Data format parsing
        ValStr := Copy(Line, 8, MaxInt);
        if ValStr = SFmtRgbeRle then
          Header.Format := hfRgb
        else if ValStr = SFmtXyzeRle then
          Header.Format := hfXyz
        else
          Exit;
      end;

      if StrMaskMatch(Line, 'Gamma=*') then
      begin
        ValStr := Copy(Line, 7, MaxInt);
        if TryStrToFloat(ValStr, ValFloat, GetFormatSettingsForFloats) then
          FMetadata.AddMetaItem(SMetaGamma, ValFloat);
      end;

      if StrMaskMatch(Line, 'Exposure=*') then
      begin
        ValStr := Copy(Line, 10, MaxInt);
        if TryStrToFloat(ValStr, ValFloat, GetFormatSettingsForFloats) then
          FMetadata.AddMetaItem(SMetaExposure, ValFloat);
      end;

      if StrMaskMatch(Line, '?Y * ?X *') then
      begin
        Idx := Pos('X', Line);
        ValStr := SubString(Line, 4, Idx - 2);
        if not TryStrToInt(ValStr, Header.Height) then
          Exit;
        ValStr := Copy(Line, Idx + 2, MaxInt);
        if not TryStrToInt(ValStr, Header.Width) then
          Exit;

        if (Line[1] = '-') then
          Header.Height := -Header.Height;
        if (Line[Idx - 1] = '-') then
          Header.Width := -Header.Width;

        HasResolution := True;
      end;

    until HasResolution;
    Result := True;
  end;

  procedure DecodeRgbe(const Src: TRgbe; Dest: PColor96FPRec); {$IFDEF USE_INLINE}inline;{$ENDIF}
  var
    Mult: Single;
  begin
    if Src.E > 0 then
    begin
      Mult := Math.Ldexp(1, Src.E - 128);
      Dest.R := Src.R / 255 * Mult;
      Dest.G := Src.G / 255 * Mult;
      Dest.B := Src.B / 255 * Mult;
    end
    else
    begin
      Dest.R := 0;
      Dest.G := 0;
      Dest.B := 0;
    end;
  end;

  procedure ReadCompressedLine(Width, Y: Integer; var DestBuffer: TDynRgbeArray);
  var
    Pos: Integer;
    I, X, Count: Integer;
    Code, Value: Byte;
    LineBuff: TDynByteArray;
    Rgbe: TRgbe;
    Ptr: PByte;
  begin
    SetLength(LineBuff, Width);
    IO.Read(Handle, @Rgbe, SizeOf(Rgbe));

    if ((Rgbe.B shl 8) or Rgbe.E) <> Width  then
      RaiseImaging(SWringScanLineWidth);

    for I := 0 to 3 do
    begin
      Pos := 0;
      while Pos < Width do
      begin
        IO.Read(Handle, @Code, SizeOf(Byte));
        if Code > 128 then
        begin
          Count := Code - 128;
          IO.Read(Handle, @Value, SizeOf(Byte));
          FillMemoryByte(@LineBuff[Pos], Count, Value);
        end
        else
        begin
          Count := Code;
          IO.Read(Handle, @LineBuff[Pos], Count * SizeOf(Byte));
        end;
        Inc(Pos, Count);
      end;

      Ptr := @PByteArray(@DestBuffer[0])[I];
      for X := 0 to Width - 1 do
      begin
        Ptr^ := LineBuff[X];
        Inc(Ptr, 4);
      end;
    end;
  end;

  procedure ReadPixels(var Image: TImageData);
  var
    Y, X, SrcLineLen: Integer;
    Dest: PColor96FPRec;
    Compressed: Boolean;
    Rgbe: TRgbe;
    Buffer: TDynRgbeArray;
  begin
    Dest := Image.Bits;
    Compressed := not ((Image.Width < 8) or (Image.Width > $7FFFF));
    SrcLineLen := Image.Width * SizeOf(TRgbe);

    IO.Read(Handle, @Rgbe, SizeOf(Rgbe));
    IO.Seek(Handle, -SizeOf(Rgbe), smFromCurrent);

    if (Rgbe.R <> 2) or (Rgbe.G <> 2) or ((Rgbe.B and 128) > 0) then
      Compressed := False;

    SetLength(Buffer, Image.Width);

    for Y := 0 to Image.Height - 1 do
    begin
      if Compressed then
        ReadCompressedLine(Image.Width, Y, Buffer)
      else
        IO.Read(Handle, @Buffer[0], SrcLineLen);

      for X := 0 to Image.Width - 1 do
      begin
        DecodeRgbe(Buffer[X], Dest);
        Inc(Dest);
      end;
    end;
  end;

begin
  IO := GetIO;
  SetLength(Images, 1);

  // Read header, allocate new image and, then read and convert the pixels
  if not ReadHeader then
    RaiseImaging(SErrorBadHeader);
  NewImage(Abs(Header.Width), Abs(Header.Height), ifR32G32B32F, Images[0]);
  ReadPixels(Images[0]);

  // Flip/mirror the image as needed (height < 0 is default top-down)
  if Header.Width < 0 then
    MirrorImage(Images[0]);
  if Header.Height > 0 then
    FlipImage(Images[0]);

  Result := True;
end;

function THdrFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: Integer): Boolean;
begin

end;

procedure THdrFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
begin
  inherited;

end;

function THdrFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  FileSig: TSignature;
  ReadCount: Integer;
begin
  Result := False;
  if Handle <> nil then
  begin
    ReadCount := GetIO.Read(Handle, @FileSig, SizeOf(FileSig));
    GetIO.Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount = SizeOf(FileSig)) and
      ((FileSig = RadianceSignature) or CompareMem(@FileSig, @RgbeSignature, 6));
  end;
end;

initialization
  RegisterImageFileFormat(THdrFileFormat);

{
  File Notes:

  -- 0.77.1 ---------------------------------------------------
    - Unit created with initial stuff

}

end.
