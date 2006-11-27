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

{ This unit contains image format loader/saver for Windows Bitmap images.}
unit ImagingBitmap;

{$I ImagingOptions.inc}

interface

uses
  ImagingTypes, Imaging, ImagingUtility, ImagingFormats;

type
  { Class for loading and saving Windows Bitmap images.
    It can load/save 8bit indexed, 16, 24, 32 bit RGB or ARGB
    images with or without RLE compression. It can also load 1/4 bit
    indexed images and OS2 bitmaps.}
  TBitmapFileFormat = class(TImageFileFormat)
  protected
    { Controls that RLE compression is used during saving. Accessible trough
      ImagingBitmapRLE option.}
    FUseRLE: LongBool;
    function GetSupportedFormats: TImageFormats; override;
    procedure LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean); override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
    function MakeCompatible(const Image: TImageData; var Comp: TImageData;
      out MustBeFreed: Boolean): Boolean; override;
  public
    constructor Create; override;
    function TestFormat(Handle: TImagingHandle): Boolean; override;
  end;

const
  SBitmapExtensions = 'bmp,dib';
  SBitmapFormatName = 'Windows Bitmap Image';
  BitmapSupportedFormats: TImageFormats = [ifIndex8, ifA1R5G5B5, ifA4R4G4B4,
    ifR8G8B8, ifA8R8G8B8, ifX1R5G5B5, ifX4R4G4B4, ifX8R8G8B8];
  BitmapDefaultRLE = True;  

implementation

const
  { Bitmap file identifier 'BM'.}
  BMMagic: Word = 19778;

  { Constants for the TBitmapInfoHeader.Compression field.}
  BI_RGB = 0;
  BI_RLE8 = 1;
  BI_RLE4 = 2;
  BI_BITFIELDS = 3;

type
  { File Header for Windows/OS2 bitmap file.}
  TBitmapFileHeader = packed record
    ID: Word;           // Is always 19778 : 'BM'
    Size: LongWord;     // Filesize
    Reserved1: Word;
    Reserved2: Word;
    Offset: LongWord;   // Offset from start pos to beginning of image bits
  end;

  { Info Header for Windows bitmap file.}
  TBitmapInfoHeader = packed record
    Size: LongWord;
    Width: LongInt;
    Height: LongInt;
    Planes: Word;
    BitCount: Word;
    Compression: LongWord;
    SizeImage: LongWord;
    XPelsPerMeter: LongInt;
    YPelsPerMeter: LongInt;
    ClrUsed: LongInt;
    ClrImportant: LongInt;
  end;

  { Info Header for OS2 bitmaps.}
  TBitmapCoreHeader = packed record
    Size: LongWord;
    Width: Word;
    Height: Word;
    Planes: Word;
    BitCount: Word;
  end;

  { Used with BitmapInfo.Compression = BI_BITFIELDS.}
  TLocalPixelFormat = packed record
    RBitMask, GBitMask, BBitMask: LongWord;
  end;

procedure Convert1To8(DataIn: Pointer; DataOut: Pointer; Width, Height,
  WidthBytes: LongInt);
const
  Mask1: array[0..7] of Byte = ($80, $40, $20, $10, $08, $04, $02, $01);
  Shift1: array[0..7] of Byte = (7, 6, 5, 4, 3, 2, 1, 0);
var
  X, Y: LongInt;
begin
  for Y := 0 to Height - 1 do
    for X := 0 to Width - 1 do
      PByteArray(DataOut)[Y * Width + X] :=
        (PByteArray(DataIn)[Y * WidthBytes + X shr 3] and
        Mask1[X and 7]) shr Shift1[X and 7];
end;

procedure Convert4To8(DataIn: Pointer; DataOut: Pointer; Width, Height,
  WidthBytes: LongInt);
const
  Mask4: array[0..1] of Byte = ($F0, $0F);
  Shift4: array[0..1] of Byte = (4, 0);
var
  X, Y: LongInt;
begin
  for Y := 0 to Height - 1 do
    for X := 0 to Width - 1 do
      PByteArray(DataOut)[Y * Width + X] :=
        (PByteArray(DataIn)[Y * WidthBytes + X shr 1] and
        Mask4[X and 1]) shr Shift4[X and 1];
end;


{ TBitmapFileFormat class implementation }

constructor TBitmapFileFormat.Create;
begin
  inherited Create;
  FName := SBitmapFormatName;
  FCanLoad := True;
  FCanSave := True;
  FIsMultiImageFormat := False;

  FUseRLE := BitmapDefaultRLE;

  AddExtensions(SBitmapExtensions);
  RegisterOption(ImagingBitmapRLE, @FUseRLE);
end;

function TBitmapFileFormat.GetSupportedFormats: TImageFormats;
begin
  Result := BitmapSupportedFormats;
end;

procedure TBitmapFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean);
var
  BF: TBitmapFileHeader;
  BI: TBitmapInfoHeader;
  BC: TBitmapCoreHeader;
  IsOS2: Boolean;
  LocalPF: TLocalPixelFormat;
  PalRGB: PPalette24;
  I, FPalSize, AlignedSize, StartPos, AlignedWidthBytes, WidthBytes: LongInt;
  FmtInfo: PImageFormatInfo;
  Data: Pointer;

  procedure LoadRGB;
  var
    I: LongInt;
    LineBuffer: PByte;
  begin
    with Images[0], GetIO do
    begin
      // if BI.Height is < 0 then image data are stored non-flipped
      // but default in windows is flipped so if Height is positive we must
      // flip it

      if BI.BitCount < 8 then
      begin
        // for 1 and 4 bit images load aligned data, they will be converted to
        // 8 bit and unaligned later
        GetMem(Data, AlignedSize);

        if BI.Height < 0 then
        begin
          Read(Handle, Data, AlignedSize);
        end
        else
          for I := Height - 1 downto 0 do
            Read(Handle, @PByteArray(Data)[I * AlignedWidthBytes], AlignedWidthBytes);
      end
      else
      begin
        // images with pixels of size >= 1 Byte are read line by line and
        // copied to image bits without padding bytes
        GetMem(LineBuffer, AlignedWidthBytes);

        if BI.Height < 0 then
        begin
          for I := 0 to Height - 1 do
          begin
            Read(Handle, LineBuffer, AlignedWidthBytes);
            Move(LineBuffer^, PByteArray(Bits)[I * WidthBytes], WidthBytes);
          end;
        end
        else
        begin
          for I := Height - 1 downto 0 do
          begin
            Read(Handle, LineBuffer, AlignedWidthBytes);
            Move(LineBuffer^, PByteArray(Bits)[I * WidthBytes], WidthBytes);
          end;
        end;

        FreeMemNil(LineBuffer);
      end;
    end;
  end;

  procedure LoadRLE4;
  var
    RLEData, Src, PLine, P: PByte;
    X, Y, I, S, C: LongInt;
  begin
    GetMem(Data, AlignedSize);
    GetMem(RLEData, BI.SizeImage);
    GetIO.Read(Handle, RLEData, BI.SizeImage);
    with Images[0] do
    try
      Src := RLEData;
      Y := 0;
      X := 0;
      while Y < Height do
      begin
        C := Src^;
        Inc(Src);
        S := Src^;
        Inc(Src);
        if C = 0 then
        begin
          case S of
            0:
              begin
                // next line
                Inc(Y);
                X := 0;
              end;
            1: Break; // end of bitmap
            2:
              begin
                // delta of coordinates
                Inc(Src);
                Inc(X, Src^);
                Inc(Src);
                Inc(Y, Src^);
              end;
          else
            begin
              // absolute data
              PLine := @PByteArray(Data)[Y * AlignedWidthBytes];
              for I := 0 to S - 1 do
              begin
                if I and 1 = 0 then
                begin
                  C := Src^;
                  Inc(Src);
                end
                else
                begin
                  C := C shl 4;
                end;
                P := @PByteArray(PLine)[X shr 1];
                if X and 1 = 0 then
                  P^ := (P^ and $0F) or (C and $F0)
                else
                  P^ := (P^ and $F0) or ((C and $F0) shr 4);
                Inc(X);
              end;
            end;
          end;
        end
        else
        begin
          // encoded data
          PLine := @PByteArray(Data)[Y * AlignedWidthBytes];
          for I := 0 to C - 1 do
          begin
            P := @PByteArray(PLine)[X shr 1];
            if X and 1 = 0 then
              P^ := (P^ and $0F) or (S and $F0)
            else
              P^ := (P^ and $F0) or ((S and $F0) shr 4);
            Inc(X);
            S := (S shr 4) or (S shl 4);
          end;
        end;
        Inc(Src, Longint(Src) and 1);
      end;
    finally
      FreeMem(RLEData);
    end;
  end;

  procedure LoadRLE8;
  var
    RLEData, Src: PByte;
    X, Y, I, S: LongInt;
  begin
    GetMem(Data, AlignedSize);
    GetMem(RLEData, BI.SizeImage);
    GetIO.Read(Handle, RLEData, BI.SizeImage);
    with Images[0] do
    try
      Src := RLEData;
      Y := 0;
      X := 0;
      while Y < Height do
      begin
        if Src^ = 0 then
        begin
          Inc(Src);
          case Src^ of
            0:
              begin
                // next line
                Inc(Y);
                X := 0;
              end;
            1: Break; // end of bitmap
            2:
              begin
                // delta of coordinates
                Inc(Src);
                Inc(X, Src^);
                Inc(Src);
                Inc(Y, Src^);
              end;
          else
            begin
              // absolute data
              I := Src^;
              S := (I + 1) and (not 1);
              Inc(Src);
              Move(Src^, PByteArray(Data)[Y * LongInt(AlignedWidthBytes) + X], S);
              Inc(Src, S - 1);
              Inc(X, I);
            end;
          end;
        end
        else
        begin
          // encoded data
          I := Src^;
          Inc(Src);
          FillChar(PByteArray(Data)[Y * LongInt(AlignedWidthBytes) + X], I, Src^);
          Inc(X, I);
        end;
        Inc(Src);
      end;

    finally
      FreeMem(RLEData);
    end;
  end;

begin
  SetLength(Images, 1);
  with GetIO, Images[0] do
  begin
    StartPos := Tell(Handle);
    Read(Handle, @BF, SizeOf(BF));
    Read(Handle, @BI.Size, SizeOf(BI.Size));
    IsOS2 := BI.Size = SizeOf(TBitmapCoreHeader);

    // Bitmap Info reading
    if IsOS2 then
    begin
      // OS/2 type bitmap, reads info header without 4 already read bytes
      Read(Handle, @PByteArray(@BC)[SizeOf(BI.Size)],
        SizeOf(TBitmapCoreHeader) - SizeOf(BI.Size));
      with BI do
      begin
        ClrUsed := 0;
        Compression := BI_RGB;
        BitCount := BC.BitCount;
        Height := BC.Height;
        Width := BC.Width;
      end;
    end
    else
    begin
      // Windows type bitmap
      Read(Handle, @PByteArray(@BI)[SizeOf(BI.Size)],
        SizeOf(TBitmapInfoHeader) - SizeOf(BI.Size));
      // SizeImage can be 0 for BI_RGB images, but it is here because of:
      // I saved 8bit bitmap in Paint Shop Pro 8 as OS2 RLE compressed.
      // It wrote strange 64 Byte Info header with SizeImage set to 0
      // Some progs were able to open it, some were not.
      if BI.SizeImage = 0 then
        BI.SizeImage := BF.Size - BF.Offset;
    end;
    // Bit mask reading
    if BI.Compression = BI_BITFIELDS then
      Read(Handle, @LocalPF, SizeOf(LocalPF));

    case BI.BitCount of
      1, 4, 8: Format := ifIndex8;
      16:
        if LocalPF.RBitMask = $0F00 then
          Format := ifX4R4G4B4
        else
          if LocalPF.RBitMask = $F800 then
            Format := ifR5G6B5
          else
            Format := ifA1R5G5B5;
      24: Format := ifR8G8B8;
      32: Format := ifA8R8G8B8;
    end;

    NewImage(BI.Width, Abs(BI.Height), Format, Images[0]);
    FmtInfo := GetFormatInfo(Format);
    WidthBytes := Width * FmtInfo.BytesPerPixel;
    AlignedWidthBytes := (((Width * BI.BitCount) + 31) shr 5) * 4;
    AlignedSize := Height * LongInt(AlignedWidthBytes);

    // Palette settings and reading
    if BI.BitCount <= 8 then
    begin
      // seek to the begining of palette
      Seek(Handle, StartPos + SizeOf(TBitmapFileHeader) + LongInt(BI.Size),
        smFromBeginning);
      if IsOS2 then
      begin
        // OS/2 type
        FPalSize := 1 shl BI.BitCount;
        GetMem(PalRGB, FPalSize * SizeOf(TColor24Rec));
        Read(Handle, PalRGB, FPalSize * SizeOf(TColor24Rec));
        for I := 0 to FPalSize - 1 do
          with PalRGB[I] do
          begin
            Palette[I].R := R;
            Palette[I].G := G;
            Palette[I].B := B;
          end;
        FreeMem(PalRGB);
      end
      else
      begin
        // Windows type
        FPalSize := BI.ClrUsed;
        if FPalSize = 0 then
          FPalSize := 1 shl BI.BitCount;
        Read(Handle, Palette, FPalSize * SizeOf(TColor32Rec));
      end;
      for I := 0 to FPalSize - 1 do
        Palette[I].A := $FF;
    end;

    // seek to the begining of image bits
    Seek(Handle, StartPos + LongInt(BF.Offset), smFromBeginning);

    case BI.Compression of
      BI_RGB: LoadRGB;
      BI_RLE4: LoadRLE4;
      BI_RLE8: LoadRLE8;
      BI_BITFIELDS: LoadRGB;
    end;

    // check if there is alpha channel present in A1R5GB5 images, if it is not
    // change format to X1R5G5B5
    if Format = ifA1R5G5B5 then
    begin
      if not Has16BitImageAlpha(Width * Height, Bits) then
        Format := ifX1R5G5B5;
    end;

    if BI.BitCount < 8 then
    begin
      // 1 and 4 bpp images are supported only for loading which is now
      // so we now convert them to 8bpp (and unalign scanlines).
      case BI.BitCount of
        1: Convert1To8(Data, Bits, Width, Height, AlignedWidthBytes);
        4: Convert4To8(Data, Bits, Width, Height, AlignedWidthBytes);
      end;
      FreeMem(Data);
      // enlarge palette
      ReallocMem(Palette, FmtInfo.PaletteEntries * SizeOf(TColor32Rec));
    end
    else if BI.Compression = BI_RLE8 then
    begin
      // scanlines were not unaligned during decoding so remove pad bytes now
      RemovePadBytes(Data, Bits, Width, Height, FmtInfo.BytesPerPixel, AlignedWidthBytes);
      FreeMem(Data);
    end;

    // images were not flipped when decoding
    if BI.Compression in [BI_RLE4, BI_RLE8] then
      if BI.Height > 0 then
        FlipImage(Images[0]);
  end;
end;

function TBitmapFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  StartPos, EndPos, WidthBytes, AlignedSize: LongInt;
  Data: Pointer;
  BF: TBitmapFileHeader;
  BI: TBitmapInfoHeader;
  FmtInfo: PImageFormatInfo;
  ImageToSave: TImageData;
  LocalPF: TLocalPixelFormat;
  MustBeFreed: Boolean;

  procedure SaveRLE8;
  const
    BufferSize = 65536;
  var
    Pos: LongInt;
    B1, B2: Byte;
    L1, L2: LongInt;
    Src, Buf: PByte;
    X, Y: LongInt;

    function AllocByte: PByte; 
    begin
      if Pos mod BufferSize = 0 then
        ReallocMem(Buf, Pos + BufferSize - 1);
      Result := @PByteArray(Buf)[Pos];
      Inc(Pos);
    end;

  begin
    Buf := nil;
    Pos := 0;
    try
      for Y := 0 to ImageToSave.Height - 1 do
      begin
        X := 0;
        Src := @PByteArray(Data)[Y * WidthBytes];
        while X < ImageToSave.Width do
        begin
          if (ImageToSave.Width - X > 2) and
            (Src^ = PByteArray(Src)[1]) then
          begin
            // encoding mode
            B1 := 2;
            B2 := Src^;
            Inc(X, 2);
            Inc(Src, 2);
            while (X < ImageToSave.Width) and (Src^ = B2) and (B1 < 255) do
            begin
              Inc(B1);
              Inc(X);
              Inc(Src);
            end;
            AllocByte^ := B1;
            AllocByte^ := B2;
          end
          else
            if (ImageToSave.Width - X > 2) and (Src^ <> PByteArray(Src)[1]) and
              (PByteArray(Src)[1] = PByteArray(Src)[2]) then
            begin
              // encoding mode
              AllocByte^ := 1;
              AllocByte^ := Src^;
              Inc(Src);
              Inc(X);
            end
            else
            begin
              if (ImageToSave.Width - X < 4) then
              begin
                if ImageToSave.Width - X = 2 then
                begin
                  // encoding mode
                  AllocByte^ := 1;
                  AllocByte^ := Src^;
                  Inc(Src);
                  AllocByte^ := 1;
                  AllocByte^ := Src^;
                  Inc(Src);
                  Inc(X, 2);
                end
                else
                begin
                  AllocByte^ := 1;
                  AllocByte^ := Src^;
                  Inc(Src);
                  Inc(X);
                end;
              end
              else
              begin
                // absolute mode
                L1 := Pos;
                AllocByte;
                L2 := Pos;
                AllocByte;
                B1 := 0;
                B2 := 3;
                Inc(X, 3);
                AllocByte^ := Src^;
                Inc(Src);
                AllocByte^ := Src^;
                Inc(Src);
                AllocByte^ := Src^;
                Inc(Src);
                while (X < ImageToSave.Width) and (B2 < 255) do
                begin
                  if (ImageToSave.Width - X > 3) and
                    (Src^ = PByteArray(Src)[1]) and
                    (Src^ = PByteArray(Src)[2]) and
                    (Src^ = PByteArray(Src)[3]) then
                    Break;
                  AllocByte^ := Src^;
                  Inc(Src);
                  Inc(B2);
                  Inc(X);
                end;
                PByteArray(Buf)[L1] := B1;
                PByteArray(Buf)[L2] := B2;
              end;
            end;
          if Pos and 1 = 1 then
            AllocByte;
        end;
        // end of line
        AllocByte^ := 0;
        AllocByte^ := 0;
      end;
      // end of bitmap
      AllocByte^ := 0;
      AllocByte^ := 1;
      GetIO.Write(Handle, Buf, Pos);
    finally
      FreeMem(Buf);
    end;
  end;

begin
  Result := PrepareSave(Handle, Images, Index);
  if Result and MakeCompatible(Images[Index], ImageToSave, MustBeFreed) then
  with GetIO, ImageToSave do
  try
    FmtInfo := GetFormatInfo(Format);
    StartPos := Tell(Handle);
    FillChar(BF, SizeOf(BF), 0);
    FillChar(BI, SizeOf(BI), 0);
    // other fields will be filled later - we don't know all values now
    BF.ID := BMMagic;
    Write(Handle, @BF, SizeOF(BF));
    // other fields will be filled later - we don't know all values now
    BI.Size := SizeOf(BI);
    BI.Width := Width;
    BI.Height := -Height;
    BI.Planes := 1;
    BI.BitCount := FmtInfo.BytesPerPixel * 8;
    // set compression
    if (FmtInfo.BytesPerPixel = 1) and FUseRLE then
      BI.Compression := BI_RLE8
    else
      if (Format <> ifA1R5G5B5) and (FmtInfo.BytesPerPixel = 2) then
        BI.Compression := BI_BITFIELDS
      else
        BI.Compression := BI_RGB;
    Write(Handle, @BI, SizeOF(BI));
    // write mask info
    if BI.Compression = BI_BITFIELDS then
      with FmtInfo.PixelFormat^ do
      begin
        LocalPF.RBitMask := RBitMask;
        LocalPF.GBitMask := GBitMask;
        LocalPF.BBitMask := BBitMask;
        Write(Handle, @LocalPF, SizeOf(LocalPF));
      end;
    // write palette
    if Palette <> nil then
      Write(Handle, Palette, FmtInfo.PaletteEntries * SizeOf(TColor32Rec));

    BF.Offset := Tell(Handle) - StartPos;

    WidthBytes := (((Width * BI.BitCount) + 31) shr 5) * 4;
    AlignedSize := Height * WidthBytes;
    if Size <> AlignedSize then
    begin
      GetMem(Data, AlignedSize);
      AddPadBytes(Bits, Data, Width, Height, FmtInfo.BytesPerPixel, WidthBytes);
    end
    else
      Data := Bits;

    if BI.Compression = BI_RLE8 then
      SaveRLE8
    else
      Write(Handle, Data, AlignedSize);
    if Data <> Bits then
      FreeMem(Data);

    EndPos := Tell(Handle);
    Seek(Handle, StartPos, smFromBeginning);
    // rewrite header with new values
    BF.Size := EndPos - StartPos;
    BI.SizeImage := BF.Size - BF.Offset;
    Write(Handle, @BF, SizeOf(BF));
    Write(Handle, @BI, SizeOf(BI));
    Seek(Handle, EndPos, smFromBeginning);
  finally
    if MustBeFreed then
      FreeImage(ImageToSave);
  end;
end;

function TBitmapFileFormat.MakeCompatible(const Image: TImageData;
  var Comp: TImageData; out MustBeFreed: Boolean): Boolean;
var
  Info: PImageFormatInfo;
  ConvFormat: TImageFormat;
begin
  if not inherited MakeCompatible(Image, Comp, MustBeFreed) then
  begin
    Info := GetFormatInfo(Comp.Format);
    if Info.HasGrayChannel or Info.IsIndexed then
      // convert all grayscale and indexed images to Index8
      ConvFormat := ifIndex8
    else
      if Info.HasAlphaChannel or Info.IsFloatingPoint then
        // convert images with alpha channel or float to A8R8G8B8
        ConvFormat := ifA8R8G8B8
      else
        if Info.UsePixelFormat then
          // convert 16bit RGB images to A1R5G5B5
          ConvFormat := ifA1R5G5B5
        else
          // convert all other formats to R8G8B8
          ConvFormat := ifR8G8B8;

    ConvertImage(Comp, ConvFormat);
  end;
  Result := Comp.Format in GetSupportedFormats;
end;

function TBitmapFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Hdr: TBitmapFileHeader;
  ReadCount: LongInt;
begin
  Result := False;
  if Handle <> nil then
    with GetIO do
    begin
      ReadCount := Read(Handle, @Hdr, SizeOf(Hdr));
      Seek(Handle, -ReadCount, smFromCurrent);
      Result := (Hdr.ID = BMMagic) and (ReadCount = SizeOf(Hdr));
    end;
end;

initialization
  RegisterImageFileFormat(TBitmapFileFormat);

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - rewrite SaveRLE8, there is some error with MemCheck
    - add alpha check as with 16b bitmaps to 32b bitmaps too

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - changed SaveData, LoadData, and MakeCompatible methods according
      to changes in base class in Imaging unit

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - fixed wrong const that caused A4R4G4B4 BMPs to load as A1R5G5B5
    - fixed the bug that caused 8bit RLE compressed bitmaps to load as
      whole black

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - 16 bit images are usually without alpha but some has alpha
      channel and there is no indication of it - so I have added
      a check: if all pixels of image are with alpha = 0 image is treated
      as X1R5G5B5 otherwise as A1R5G5B5

  -- 0.13 Changes/Bug Fixes -----------------------------------
    - when loading 1/4 bit images with dword aligned dimensions
      there was ugly memory rewritting bug causing image corruption

}

end.

