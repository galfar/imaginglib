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

{ This unit contains image format loader/saver for Photoshop PSD image format.}
unit ImagingPsd;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, ImagingTypes, Imaging, ImagingColors, ImagingUtility;

type
  { Class for loading and saving Adobe Photoshop PSD images.
    Loading and saving of indexed, grayscale, RGB(A), HDR (FP32), and CMYK
    (auto converted to RGB)  images is supported. Non-HDR gray, RGB,
    and CMYK images can have 8bit or 16bit color channels.
    There is no support for loading mono images, duotone images are treated
    like grayscale images, and multichannel and CIE Lab images are loaded as
    RGB images but without actual conversion to RGB color space.}
  TPSDFileFormat = class(TImageFileFormat)
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
  SPSDFormatName = 'Photoshop Image';
  SPSDMasks      = '*.psd,*.pdd';
  PSDSupportedFormats: TImageFormats = [ifIndex8, ifGray8, ifA8Gray8,
    ifR8G8B8, ifA8R8G8B8, ifGray16, ifA16Gray16, ifR16G16B16, ifA16R16G16B16,
    ifR32F, ifA32R32G32B32F];

const
  SPSDMagic = '8BPS';

type
  {$MINENUMSIZE 2}
  { PSD Image color mode.}
  TPSDColorMode = (
    cmMono         = 0,
    cmGrayscale    = 1,
    cmIndexed      = 2,
    cmRGB          = 3,
    cmCMYK         = 4,
    cmMultiChannel = 7,
    cmDuoTone      = 8,
    cmLab          = 9
  );

  { PSD image main header.}
  TPSDHeader = packed record
    Signature: TChar4;             // Format ID '8BPS'
    Version: Word;                 // Always 1
    Reserved: array[0..5] of Byte; // Reserved, all zero
    Channels: Word;                // Number of color channels (1-24) including alpha channels
    Rows : LongWord;               // Height of image in pixels (1-30000)
    Columns: LongWord;             // Width of image in pixels (1-30000)
    Depth: Word;                   // Number of bits per channel (1, 8, and 16)
    Mode: TPSDColorMode;           // Color mode
  end;

procedure SwapHeader(var Header: TPSDHeader);
begin
  Header.Version := SwapEndianWord(Header.Version);
  Header.Channels := SwapEndianWord(Header.Channels);
  Header.Depth := SwapEndianWord(Header.Depth);
  Header.Rows := SwapEndianLongWord(Header.Rows);
  Header.Columns := SwapEndianLongWord(Header.Columns);
  Header.Mode := TPSDColorMode(SwapEndianWord(Word(Header.Mode)));
end; 

{
  TPSDFileFormat class implementation
}

constructor TPSDFileFormat.Create;
begin
  inherited Create;
  FName := SPSDFormatName;
  FCanLoad := True;
  FCanSave := True;
  FIsMultiImageFormat := False;
  FSupportedFormats := PSDSupportedFormats;
  AddMasks(SPSDMasks);
end;

function TPSDFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Header: TPSDHeader;
  ByteCount: LongWord;
  RawPal: array[0..767] of Byte;
  Compression, PackedSize: Word;
  LineSize, ChannelPixelSize, WidthBytes,
    CurrChannel, MaxRLESize, I, Y, X: LongInt;
  Info: TImageFormatInfo;
  PackedLine, LineBuffer: PByte;
  RLELineSizes: array of Word;
  Col32: TColor32Rec;
  Col64: TColor64Rec;
  PCol32: PColor32Rec;
  PCol64: PColor64Rec;
  PColF: PColorFPRec;

  { PackBits RLE decode code from Mike Lischke's GraphicEx library.}
  procedure DecodeRLE(Source, Dest: PByte; PackedSize, UnpackedSize: LongInt);
  var
    Count: LongInt;
  begin
    while (UnpackedSize > 0) and (PackedSize > 0) do
    begin
      Count := ShortInt(Source^);
      Inc(Source);
      Dec(PackedSize);
      if Count < 0 then
      begin
        // Replicate next byte -Count + 1 times
        if Count = -128 then
          Continue;
        Count := -Count + 1;
        if Count > UnpackedSize then
          Count := UnpackedSize;
        FillChar(Dest^, Count, Source^);
        Inc(Source);
        Dec(PackedSize);
        Inc(Dest, Count);
        Dec(UnpackedSize, Count);
      end
      else
      begin
        // Copy next Count + 1 bytes from input
        Inc(Count);
        if Count > UnpackedSize then
          Count := UnpackedSize;
        if Count > PackedSize then
          Count := PackedSize;
        Move(Source^, Dest^, Count);
        Inc(Dest, Count);
        Inc(Source, Count);
        Dec(PackedSize, Count);
        Dec(UnpackedSize, Count);
      end;
    end;
  end;

begin
  Result := False;
  SetLength(Images, 1);
  with GetIO, Images[0] do
  begin
    // Read PSD header
    Read(Handle, @Header, SizeOf(Header));
    SwapHeader(Header);
    // Determine image data format 
    Format := ifUnknown;
    case Header.Mode of
      cmGrayscale, cmDuoTone:
        begin
          if Header.Depth in [8, 16] then
          begin
            if Header.Channels = 1 then
              Format := IffFormat(Header.Depth = 8, ifGray8, ifGray16)
            else if Header.Channels >= 2 then
              Format := IffFormat(Header.Depth = 8, ifA8Gray8, ifA16Gray16);
          end
          else if (Header.Depth = 32) and (Header.Channels = 1) then
            Format := ifR32F;
        end;
      cmIndexed:
        begin
          if Header.Depth = 8 then
            Format := ifIndex8;
        end;
      cmRGB, cmMultiChannel, cmCMYK, cmLab:
        begin
          if Header.Depth in [8, 16] then
          begin
            if Header.Channels = 3 then
              Format := IffFormat(Header.Depth = 8, ifR8G8B8, ifR16G16B16)
            else if Header.Channels >= 4 then
              Format := IffFormat(Header.Depth = 8, ifA8R8G8B8, ifA16R16G16B16);
          end
          else if Header.Depth = 32 then
            Format := ifA32R32G32B32F;
        end;
      cmMono:; // Not supported
    end;

    // Exit if no compatible format was found
    if Format = ifUnknown then
      Exit;

    NewImage(Header.Columns, Header.Rows, Format, Images[0]);
    Info := GetFormatInfo(Format);

    // Read or skip Color Mode Data Block (palette)
    Read(Handle, @ByteCount, SizeOf(ByteCount));
    ByteCount := SwapEndianLongWord(ByteCount);
    if Format = ifIndex8 then
    begin
      // Read palette only for indexed images
      Read(Handle, @RawPal, SizeOf(RawPal));
      for I := 0 to 255 do
      begin
        Palette[I].A := $FF;
        Palette[I].R := RawPal[I + 0];
        Palette[I].G := RawPal[I + 256];
        Palette[I].B := RawPal[I + 512];
      end; 
    end
    else
      Seek(Handle, ByteCount, smFromCurrent);

    // Skip Image Resources Block
    Read(Handle, @ByteCount, SizeOf(ByteCount));
    ByteCount := SwapEndianLongWord(ByteCount);
    Seek(Handle, ByteCount, smFromCurrent);
    // Skip Layer and Mask Information Block
    Read(Handle, @ByteCount, SizeOf(ByteCount));
    ByteCount := SwapEndianLongWord(ByteCount);
    Seek(Handle, ByteCount, smFromCurrent);

    // Read compression flag
    Read(Handle, @Compression, SizeOf(Compression));
    Compression := SwapEndianWord(Compression);

    if Compression = 1 then
    begin
      // RLE compressed PSDs (most) have first lengths of compressed scanlines
      // for each channel stored
      SetLength(RLELineSizes, Height * Header.Channels);
      Read(Handle, @RLELineSizes[0], Length(RLELineSizes) * SizeOf(Word));
      SwapEndianWord(@RLELineSizes[0], Height * Header.Channels);
      MaxRLESize := RLELineSizes[0];
      for I := 1 to High(RLELineSizes) do
      begin
        if MaxRLESize < RLELineSizes[I] then
          MaxRLESize := RLELineSizes[I];
      end;
    end
    else
      MaxRLESize := 0;

    ChannelPixelSize := Info.BytesPerPixel div Info.ChannelCount;
    LineSize := Width * ChannelPixelSize;
    WidthBytes := Width * Info.BytesPerPixel;
    GetMem(LineBuffer, LineSize);
    GetMem(PackedLine, MaxRLESize);

    try
      // Image color chanels are stored separately in PSDs so we will load
      // one by one and copy their data to appropriate addresses of dest image.
      for I := 0 to Header.Channels - 1 do
      begin
        // Now determine to which color channel of destination image we are going
        // to write pixels.
        if I <= 4 then
        begin
          // If PSD has alpha channel we need to switch current channel order -
          // PSDs have alpha stored after blue channel but Imaging has alpha
          // before red.
          if Info.HasAlphaChannel and (Header.Mode <> cmCMYK) then
          begin
            if I = Info.ChannelCount - 1 then
              CurrChannel := I
            else
              CurrChannel := Info.ChannelCount - 2 - I;
          end
          else
            CurrChannel := Info.ChannelCount - 1 - I;
        end
        else
        begin
          // No valid channel remains
          CurrChannel := -1;
        end;

        if CurrChannel >= 0 then
        begin
          for Y := 0 to Height - 1 do
          begin
            if Compression = 1 then
            begin
              // Read RLE line and decompress it
              PackedSize := RLELineSizes[I * Height + Y];
              Read(Handle, PackedLine, PackedSize);
              DecodeRLE(PackedLine, LineBuffer, PackedSize, LineSize);
            end
            else
            begin
              // Just read uncompressed line
              Read(Handle, LineBuffer, LineSize);
            end;

            // Swap endian if needed
            if ChannelPixelSize = 4 then
              SwapEndianLongWord(PLongWord(LineBuffer), Width)
            else if ChannelPixelSize = 2 then
              SwapEndianWord(PWordArray(LineBuffer), Width);

            if Info.ChannelCount > 1 then
            begin
              // Copy each pixel fragment to its right place in destination image
              for X := 0 to Width - 1 do
              begin
                Move(PByteArray(LineBuffer)[X * ChannelPixelSize],
                  PByteArray(Bits)[Y * WidthBytes + X * Info.BytesPerPixel + CurrChannel * ChannelPixelSize],
                  ChannelPixelSize);
              end;
            end
            else
            begin
              // Just copy the line
              Move(LineBuffer^, PByteArray(Bits)[Y * LineSize], LineSize);
            end;
          end;
        end
        else
        begin
          // Skip current color channel, not needed for image loading - just to
          // get stream's position to the and of PSD
          if Compression = 1 then
          begin
            for Y := 0 to Height - 1 do
              Seek(Handle, RLELineSizes[I * Height + Y], smFromCurrent);
          end
          else
            Seek(Handle, LineSize * Height, smFromCurrent);
        end;
      end;

      if Header.Mode = cmCMYK then
      begin
        // Convert CMYK images to RGB (alpha is ignored here). PSD stores CMYK
        // channels in the way that first requires substraction from max channel value
        if ChannelPixelSize = 1 then
        begin
          PCol32 := Bits;
          for X := 0 to Width * Height - 1 do
          begin
            Col32.A := 255 - PCol32.A;
            Col32.R := 255 - PCol32.R;
            Col32.G := 255 - PCol32.G;
            Col32.B := 255 - PCol32.B;
            CMYKToRGB(Col32.A, Col32.R, Col32.G, Col32.B, PCol32.R, PCol32.G, PCol32.B);
            PCol32.A := 255;
            Inc(PCol32);
          end;
        end
        else
        begin
          PCol64 := Bits;
          for X := 0 to Width * Height - 1 do
          begin
            Col64.A := 65535 - PCol64.A;
            Col64.R := 65535 - PCol64.R;
            Col64.G := 65535 - PCol64.G;
            Col64.B := 65535 - PCol64.B;
            CMYKToRGB16(Col64.A, Col64.R, Col64.G, Col64.B, PCol64.R, PCol64.G, PCol64.B);
            PCol64.A := 65535;
            Inc(PCol64);
          end;
        end;
      end;

      if Header.Depth = 32 then
      begin
        if (Header.Channels = 3) and (Header.Mode = cmRGB) then
        begin
          // RGB images were loaded as ARGB so we must wet alpha manually to 1.0
          PColF := Bits;
          for X := 0 to Width * Height - 1 do
          begin
            PColF.A := 1.0;
            Inc(PColF);
          end;
        end;
      end;

      Result := True;
    finally
      FreeMem(LineBuffer);
      FreeMem(PackedLine);
    end;
  end;
end;

function TPSDFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  MustBeFreed: Boolean;
  ImageToSave: TImageData;
  Info: TImageFormatInfo;
  Header: TPSDHeader;
  I, X, Y, CurrChannel, LineSize, ChannelPixelSize, WidthBytes: LongInt;
  LongVal: LongWord;
  WordVal: Word;
  RawPal: array[0..767] of Byte;
  LineBuffer: PByte;
begin
  Result := False;
  if MakeCompatible(Images[Index], ImageToSave, MustBeFreed) then
  with GetIO, ImageToSave do
  try
    Info := GetFormatInfo(Format);
    // Fill header with proper info and save it
    FillChar(Header, SizeOf(Header), 0);
    Header.Signature := SPSDMagic;
    Header.Version := 1;
    Header.Channels := Info.ChannelCount;
    Header.Rows := Height;
    Header.Columns := Width;
    Header.Depth := Info.BytesPerPixel div Info.ChannelCount * 8;
    if Info.IsIndexed then
      Header.Mode := cmIndexed
    else if Info.HasGrayChannel or (Info.ChannelCount = 1) then
      Header.Mode := cmGrayscale
    else
      Header.Mode := cmRGB;

    SwapHeader(Header);
    Write(Handle, @Header, SizeOf(Header));

    // Write palette size and data
    LongVal := SwapEndianLongWord(IffUnsigned(Info.IsIndexed, SizeOf(RawPal), 0));
    Write(Handle, @LongVal, SizeOf(LongVal));
    if Info.IsIndexed then
    begin
      for I := 0 to Info.PaletteEntries - 1 do
      begin
        RawPal[I] := Palette[I].R;
        RawPal[I + 256] := Palette[I].G;
        RawPal[I + 512] := Palette[I].B;
      end;
      Write(Handle, @RawPal, SizeOf(RawPal));
    end;
    // Write resource and layer block sizes
    LongVal := 0;
    Write(Handle, @LongVal, SizeOf(LongVal));
    Write(Handle, @LongVal, SizeOf(LongVal));
    // Set compression off
    WordVal := 0;
    Write(Handle, @WordVal, SizeOf(WordVal));

    ChannelPixelSize := Info.BytesPerPixel div Info.ChannelCount;
    LineSize := Width * ChannelPixelSize;
    WidthBytes := Width * Info.BytesPerPixel;
    GetMem(LineBuffer, LineSize);

    for I := 0 to Info.ChannelCount - 1 do
    begin
      // Now determine which color channel we are going to write to file.
      if Info.HasAlphaChannel then
      begin
        if I = Info.ChannelCount - 1 then
          CurrChannel := I
        else
          CurrChannel := Info.ChannelCount - 2 - I;
      end
      else
        CurrChannel := Info.ChannelCount - 1 - I;

      for Y := 0 to Height - 1 do
      begin
        if Info.ChannelCount > 1 then
        begin
          // Copy each pixel fragment to its right place in destination image
          for X := 0 to Width - 1 do
          begin
            Move(PByteArray(Bits)[Y * WidthBytes + X * Info.BytesPerPixel + CurrChannel * ChannelPixelSize],
              PByteArray(LineBuffer)[X * ChannelPixelSize], ChannelPixelSize);
          end;
        end
        else
          Move(PByteArray(Bits)[Y * LineSize], LineBuffer^, LineSize);

        // Write current channel line to file (swap endian if needed first)
        if ChannelPixelSize = 4 then
          SwapEndianLongWord(PLongWord(LineBuffer), Width)
        else if ChannelPixelSize = 2 then
          SwapEndianWord(PWordArray(LineBuffer), Width);
        Write(Handle, LineBuffer, LineSize);
      end;
    end;

    Result := True;
  finally
    if MustBeFreed then
      FreeImage(ImageToSave);
  end;
end;

procedure TPSDFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
var
  ConvFormat: TImageFormat;
begin
  if Info.IsFloatingPoint then
    ConvFormat :=  IffFormat(Info.ChannelCount = 1, ifR32F, ifA32R32G32B32F)
  else if Info.HasGrayChannel then
    ConvFormat := IffFormat(Info.HasAlphaChannel, ifA16Gray16, ifGray16)
  else if Info.RBSwapFormat in GetSupportedFormats then
    ConvFormat := Info.RBSwapFormat
  else
    ConvFormat := IffFormat(Info.HasAlphaChannel, ifA8R8G8B8, ifR8G8B8);

  ConvertImage(Image, ConvFormat);
end;

function TPSDFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Header: TPSDHeader;
  ReadCount: LongInt;
begin
  Result := False;
  if Handle <> nil then
  begin
    ReadCount := GetIO.Read(Handle, @Header, SizeOf(Header));
    SwapHeader(Header);
    GetIO.Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount >= SizeOf(Header)) and
      (Header.Signature = SPSDMagic) and
      (Header.Version = 1);
  end;
end;

initialization
  RegisterImageFileFormat(TPSDFileFormat);

{
  File Notes:

 -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Saving implemented.
    - Unit created with initial stuff!
}

end.

