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

{ This unit contains image format loader/saver for GIF images.}
unit ImagingGif;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, Imaging, ImagingTypes, ImagingUtility;

type
  { GIF (Graphics Interchange Format) loader/saver class. GIF was
    (and is still used) popular format for storing images supporting
    multiple images per file and single color transparency.
    Pixel format is 8 bit indexed where each image frame can have
    its own color palette. GIF uses lossless LZW compression
    (patent expired few years ago).
    Imaging can load and save all GIFs with all frames and supports
    transparency.}
  TGIFFileFormat = class(TImageFileFormat)
  private
    procedure LZWDecompress(IO: TIOFunctions; Handle: TImagingHandle;
      Width, Height: Integer; Interlaced: Boolean; OutData: PChar);
  protected
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); override;
  public
    constructor Create; override;
    function TestFormat(Handle: TImagingHandle): Boolean; override;
  end;

implementation

const
  SGIFFormatName = 'Graphics Interchange Format';
  SGIFMasks      = '*.gif';
  GIFSupportedFormats: TImageFormats = [ifIndex8];

type
  TGIFVersion = (gv87, gv89);

const
  GIFSignature: TChar3 = 'GIF';
  GIFVersions: array[TGIFVersion] of TChar3 = ('87a', '89a');

  // Masks for accessing fields in PackedFields of TGIFHeader
  GIFGlobalColorTable = $80;
  GIFColorResolution  = $70;
  GIFColorTableSorted = $08;
  GIFColorTableSize   = $07;

  // Masks for accessing fields in PackedFields of TImageDescriptor
  GIFLocalColorTable  = $80;
  GIFInterlaced       = $40;
  GIFLocalTableSorted = $20;

  // Block identifiers
  GIFPlainText               = $01;
  GIFGraphicControlExtension = $F9;
  GIFCommentExtension        = $FE;
  GIFApplicationExtension    = $FF;
  GIFImageDescriptor         = Ord(',');
  GIFExtensionIntroducer     = Ord('!');
  GIFTrailer                 = Ord(';');

type
  TGIFHeader = packed record
    // File header part
    Signature: TChar3;  // Header Signature (always "GIF")
    Version: TChar3;    // GIF format version("87a" or "89a")
    // Logical Screen Descriptor part
    ScreenWidth: Word;  // Width of Display Screen in Pixels
    ScreenHeight: Word; // Height of Display Screen in Pixels
    PackedFields: Byte; // Screen and color map information
    BackgroundColorIndex: Byte; // Background color index (in global color table)
    AspectRatio: Byte;  // Pixel aspect ratio, ratio = (AspectRatio + 15) / 64
  end;

  TImageDescriptor = packed record
    //Separator: Byte; // leave that out since we always read one bye ahead
    Left: Word;        // X position of image with respect to logical screen
    Top: Word;         // Y position
    Width: Word;
    Height: Word;
    PackedFields: Byte;
  end;

const
  // GIF extension labels
  GIFExtTypeGraphic     = $F9;
  GIFExtTypePlainText   = $01;
  GIFExtTypeApplication = $FF;
  GIFExtTypeComment     = $FE;

type
  TGraphicControlExtension = packed record
    BlockSize: Byte;
    PackedFields: Byte;
    DelayTime: Word;
    TransparentColorIndex: Byte;
    Terminator: Byte;
  end;

{
  TGIFFileFormat implementation
}

constructor TGIFFileFormat.Create;
begin
  inherited Create;
  FName := SGIFFormatName;
  FCanLoad := True;
  FCanSave := not True;
  FIsMultiImageFormat := True;
  FSupportedFormats := GIFSupportedFormats;

  AddMasks(SGIFMasks);
end;

procedure TGIFFileFormat.LZWDecompress(IO: TIOFunctions; Handle: TImagingHandle;
  Width, Height: Integer; Interlaced: Boolean; OutData: PChar);
// The decompressor is based on work done by
//   * readgif.c (GIFTOOL) by David Koblas <koblas@netcom.com>
type
  KeyInt = LongInt;
  CodeInt = SmallInt;
const
  HashKeyBits     = 13;
  GIFCodeBits     = 12;
  HashSize        = 8009;
  StackSize       = 2 shl GIFCodeBits;
  TableSize       = 1 shl GIFCodeBits;
  HashKeyMax      = 1 shl HashKeyBits - 1;
  GIFCodeMax      = 1 shl GIFCodeBits - 1;
  TableMaxMaxCode = 1 shl GIFCodeBits;
  TableMaxFill    = TableMaxMaxCode - 1;
  HashKeyMask     = HashKeyMax;
  GIFCodeMask     = GIFCodeMax;
  HashEmpty       = $000FFFFF;
var
  Table: array[0..1, 0..TableSize - 1] of Integer;
  Stack: array[0..StackSize - 1] of Integer;
  Buf: array[0..257] of Byte;
  Dest: PChar;
  Source: PInteger;
  InitialBitsPerCode: Byte;
  MaxCode: CodeInt;
  I, FirstCode, OldCode, BitsPerCode, Step, EOFCode, MaxCodeSize, ClearCode,
    StartBit, LastBit, LastByte, V, XPos, YPos, Pass: Integer;
  GetDone, ReturnClear, ZeroBlock: Boolean;

  function Read(var Buffer; Size: LongInt): Boolean;
  begin
    Result := IO.Read(Handle, @Buffer, Size) = Size;
  end;

  function NextCode(BitsPerCode: Integer): Integer;
  const
    Masks: array[0..15] of Integer =
      ($0000, $0001, $0003, $0007,
       $000F, $001F, $003F, $007F,
       $00FF, $01FF, $03FF, $07FF,
       $0FFF, $1FFF, $3FFF, $7FFF);
  var
    StartIndex, EndIndex, Ret, EndBit: Integer;
    Count: Byte;
  begin
    if ReturnClear then
    begin
      ReturnClear := False;
      Result := ClearCode;
      Exit;
    end;
    EndBit := StartBit + BitsPerCode;
    if EndBit >= LastBit then
    begin
      if GetDone then
      begin
        Result := -1;
        Exit;
      end;
      Buf[0] := Buf[LastByte - 2];
      Buf[1] := Buf[LastByte - 1];
      if not Read(Count, SizeOf(Count)) then
      begin
        Result := -1;
        Exit;
      end;
      if Count = 0 then
      begin
        ZeroBlock := True;
        GetDone := True;
      end
      else
        Read(Buf[2], Count);

      LastByte := 2 + Count;
      StartBit := (StartBit - LastBit) + 16;
      LastBit := LastByte * 8;
      EndBit := StartBit + BitsPerCode;
    end;

    EndIndex := EndBit div 8;
    StartIndex := StartBit div 8;
    if StartIndex = EndIndex then
      Ret := Buf[StartIndex]
    else if StartIndex + 1 = EndIndex then
      Ret := Buf[StartIndex] or (Buf[StartIndex + 1] shl 8)
    else
      Ret := Buf[StartIndex] or (Buf[StartIndex + 1] shl 8) or (Buf[StartIndex + 2] shl 16);

    Ret := (Ret shr (StartBit and $0007)) and Masks[BitsPerCode];
    Inc(StartBit, BitsPerCode);
    Result := Ret;
  end;

  function NextLWZ: Integer;
  var
    I, Code, InCode: Integer;
    B: Byte;
  begin
    Code := NextCode(BitsPerCode);
    while Code >= 0 do
    begin
      if Code = ClearCode then
      begin
        for I := 0 to ClearCode-1 do
        begin
          Table[0, I] := 0;
          Table[1, I] := I;
        end;
        for I := ClearCode to TableSize-1 do
        begin
          Table[0, I] := 0;
          Table[1, I] := 0;
        end;
        BitsPerCode := InitialBitsPerCode+1;
        MaxCodeSize := 2 * ClearCode;
        MaxCode := ClearCode + 2;
        Source := @Stack;
        repeat
          FirstCode := NextCode(BitsPerCode);
          OldCode := FirstCode;
        until (FirstCode <> ClearCode);
        Result := FirstCode;
        Exit;
      end;

      if Code = EOFCode then
      begin
        Result := -2;
        if (ZeroBlock) then
          Exit;
        if not Read(B, SizeOf(B)) then
          Exit;
        while (B <> 0) do
        begin
          IO.Seek(Handle, B, smFromCurrent);
          if not Read(B, SizeOf(B)) then
            Exit;
        end;
        Exit;
      end;
      InCode := Code;
      if Code >= MaxCode then
      begin
        Source^ := FirstCode;
        Inc(Source);
        Code := OldCode;
      end;
      while Code >= ClearCode do
      begin
        Source^ := Table[1, Code];
        Inc(Source);
        if (Code = Table[0, Code]) then
           raise EImagingError.Create('Unrecognized GIF image: LZW decompression failed');
        Code := Table[0, Code];
      end;
      FirstCode := Table[1, Code];
      Source^ := FirstCode;
      Inc(Source);
      Code := MaxCode;
      if Code <= GIFCodeMax then
      begin
        Table[0, Code] := OldCode;
        Table[1, Code] := FirstCode;
        Inc(MaxCode);
        if (MaxCode >= MaxCodeSize) and (MaxCodeSize <= GIFCodeMax) then
        begin
          MaxCodeSize := MaxCodeSize * 2;
          Inc(BitsPerCode);
        end;
      end;
      OldCode := InCode;
      if Cardinal(Source) > Cardinal(@Stack) then
      begin
        Dec(Source);
        Result := Source^;
        Exit;
      end
    end;
    Result := Code;
  end;

  function ReadLWZ: Integer;
  begin
    if Cardinal(Source) > Cardinal(@Stack) then
    begin
      Dec(Source);
      Result := Source^;
    end
    else
      Result := NextLWZ;
  end;

begin
  Read(InitialBitsPerCode, SizeOf(InitialBitsPerCode));
  BitsPerCode := InitialBitsPerCode + 1;
  ClearCode := 1 shl InitialBitsPerCode;
  EOFCode := ClearCode + 1;
  MaxCodeSize := 2 * ClearCode;
  MaxCode := ClearCode + 2;
  StartBit := 0;
  LastBit := 0;
  LastByte := 2;
  ZeroBlock := False;
  GetDone := False;
  ReturnClear := True;
  Source := @Stack;

  if Interlaced then
  begin
    YPos := 0;
    Pass := 0;
    Step := 8;
    for I := 0 to Height - 1 do
    begin
      Dest := OutData + Width * YPos;
      for XPos := 0 to Width - 1 do
      begin
        V := ReadLWZ;
        if V < 0 then
          Exit;
        Dest^ := Char(V);
        Inc(Dest);
      end;

      Inc(YPos, Step);
      if YPos >= Height then
      repeat
        if Pass > 0 then
          Step := Step div 2;
        Inc(Pass);
        YPos := Step div 2;
      until YPos <= Height;
    end;
  end
  else
  begin
    Dest := OutData;
    for YPos := 0 to Height * Width - 1 do
    begin
      V := ReadLWZ;
      if V < 0 then
        Exit;
      Dest^ := Char(V);
      Inc(Dest);
    end;
  end;
end;

function TGIFFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Header: TGIFHeader;
  HasGlobalPal: Boolean;
  BitsPerPixel, GlobalPalLength: Integer;
  GlobalPal: TPalette32Size256;
  I: Integer;
  BlockID: Byte;
  HasGraphicExt: Boolean;
  GraphicExt: TGraphicControlExtension;

  function ReadBlockID: Byte;
  begin
    Result := GIFTrailer;
    GetIO.Read(Handle, @Result, SizeOf(Result));
  end;

  procedure ReadExtensions;
  var
    BlockSize, ExtType: Byte;
  begin
    HasGraphicExt := False;

    // Read extensions until image descriptor is found. Only graphic extension
    // is stored now (for transparency), others are skipped.
    while BlockID = GIFExtensionIntroducer do
    with GetIO do
    begin
      Read(Handle, @ExtType, SizeOf(ExtType));

      case ExtType of
        GIFGraphicControlExtension:
          begin
            HasGraphicExt := True;
            Read(Handle, @GraphicExt, SizeOf(GraphicExt));
          end;
        GIFCommentExtension, GIFApplicationExtension, GIFPlainText:
          repeat
            // Read block sizes and skip them
            Read(Handle, @BlockSize, SizeOf(BlockSize));
            Seek(Handle, BlockSize, smFromCurrent);
          until BlockSize = 0;
      end;

      // Read ID of following block
      BlockID := ReadBlockID;
    end;
  end;

  procedure ReadFrame;
  var
    ImageDesc: TImageDescriptor;
    HasLocalPal, Interlaced, HasTransparency: Boolean;
    I, Idx, LocalPalLength: Integer;
    LocalPal: TPalette32Size256;
  begin
    Idx := Length(Images);
    SetLength(Images, Idx + 1);
    with GetIO, Images[Idx] do
    begin
      // Read and parse image descriptor
      Read(Handle, @ImageDesc, SizeOf(ImageDesc));
      HasLocalPal := (ImageDesc.PackedFields and GIFLocalColorTable) = GIFLocalColorTable;
      Interlaced := (ImageDesc.PackedFields and GIFInterlaced) = GIFInterlaced;
      LocalPalLength := Header.PackedFields and GIFColorTableSize;
      LocalPalLength := 1 shl (LocalPalLength + 1);   // Total pal length is 2^(n+1)

      // Create image for this frame
      NewImage(ImageDesc.Width, ImageDesc.Height, ifIndex8, Images[Idx]);

      // Load local palette if there is any
      if HasLocalPal then
        for I := 0 to LocalPalLength - 1 do
        begin
          LocalPal[I].A := 255;
          Read(Handle, @LocalPal[I].R, SizeOf(LocalPal[I].R));
          Read(Handle, @LocalPal[I].G, SizeOf(LocalPal[I].G));
          Read(Handle, @LocalPal[I].B, SizeOf(LocalPal[I].B));
        end;

      // Use local pal if present or global pal if present or create
      // default pal if neither of them is present
      if HasLocalPal then
        Move(LocalPal, Palette^, SizeOf(LocalPal))
      else if HasGlobalPal then
        Move(GlobalPal, Palette^, SizeOf(GlobalPal))
      else
        FillCustomPalette(Palette, GlobalPalLength, 3, 3, 2);

      // Data decompression finally
      LZWDecompress(GetIO, Handle, Width, Height, Interlaced, PChar(Bits));

      if HasGraphicExt then
      begin
        HasTransparency := (GraphicExt.PackedFields and 1) = 1;
        if HasTransparency then
        begin
          Palette[GraphicExt.TransparentColorIndex].A := 0;
        end;
      end;
    end;
  end;

begin
  Result := False;
  SetLength(Images, 0);
  with GetIO do
  begin
    // Read GIF header
    Read(Handle, @Header, SizeOf(Header));
    HasGlobalPal := Header.PackedFields and GIFGlobalColorTable = GIFGlobalColorTable; // Bit 7
    BitsPerPixel := Header.PackedFields and GIFColorResolution shr 4 + 1; // Bits 4-6
    GlobalPalLength := Header.PackedFields and GIFColorTableSize; // Bits 0-2
    GlobalPalLength := 1 shl (GlobalPalLength + 1);   // Total pal length is 2^(n+1)

    // Read global palette from file if present
    if HasGlobalPal then
      for I := 0 to GlobalPalLength - 1 do
      begin
        GlobalPal[I].A := 255;
        Read(Handle, @GlobalPal[I].R, SizeOf(GlobalPal[I].R));
        Read(Handle, @GlobalPal[I].G, SizeOf(GlobalPal[I].G));
        Read(Handle, @GlobalPal[I].B, SizeOf(GlobalPal[I].B));
      end;

    // Read ID of the first block
    BlockID := ReadBlockID;

    // Now read all data blocks in the file until file trailer is reached
    while BlockID <> GIFTrailer do
    begin
      // Read supported and skip unsupported extensions
      ReadExtensions;
      // If image frame is found read it
      if BlockID = GIFImageDescriptor then
        ReadFrame;
      // Read next block's ID
      BlockID := ReadBlockID;
      // If block ID is unknown set it to end-of-GIF marker
      if not (BlockID in [GIFExtensionIntroducer, GIFTrailer, GIFImageDescriptor]) then
        BlockID := GIFTrailer;
    end;

    Result := True;
  end;
end;

function TGIFFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: Integer): Boolean;
begin

end;

procedure TGIFFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
begin
  ConvertImage(Image, ifIndex8);
end;

function TGIFFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Header: TGIFHeader;
  ReadCount: LongInt;
begin
  Result := False;
  if Handle <> nil then
  begin
    ReadCount := GetIO.Read(Handle, @Header, SizeOf(Header));
    GetIO.Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount >= SizeOf(Header)) and
      (Header.Signature = GIFSignature) and
      ((Header.Version = GIFVersions[gv87]) or (Header.Version = GIFVersions[gv89]));
  end;
end;

initialization
  RegisterImageFileFormat(TGIFFileFormat);

{
  File Notes:

 -- TODOS ----------------------------------------------------
    - nothing now
    - add some initial stuff!

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Unit created with initial stuff!
}

end.
