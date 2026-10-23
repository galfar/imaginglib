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

{ Tests of basic functions of the low level interface: creating, testing,
  cloning, and freeing images, direct access to pixels.}
unit TestImagingBasics;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, ImagingTypes, Imaging, ImagingFormats, ImagingUtility,
  ImagingIO, ImagingTestCase;

type
  TTestImageLifetime = class(TImagingTestCase)
  private
    FImage: TImageData;
    procedure CheckBitsAreZero(const Image: TImageData; const Msg: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInitImage;
    procedure TestTestImage;
    procedure TestNewImage;
    procedure TestNewImageIndexed;
    procedure TestNewImageDefaultFormat;
    procedure TestNewImageBlockCompressed;
    procedure TestNewImageReplacesOldOne;
    procedure TestFreeImage;
    procedure TestCloneImage;
    procedure TestCloneImageIndexed;
    procedure TestConvertImageInto;
    procedure TestConvertImageIntoSameRecord;
    procedure TestConvertImageIntoSharedBits;
    procedure TestConvertImageIntoSpecialFormats;
    procedure TestConvertImageIntoInvalidSource;
  end;

  TTestPixelAccess = class(TImagingTestCase)
  private
    FImage: TImageData;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPixel32InARGB;
    procedure TestPixel32InRGB24;
    procedure TestPixel32InGray8;
  end;

  { Values computed from dimensions of image: number of pixels and addresses
    of pixels and scanlines.}
  TTestImageGeometry = class(TImagingTestCase)
  private
    FImage: TImageData;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPixelCount;
    procedure TestPixelCountOver32Bits;
    procedure TestPixelAndScanlinePointers;
    procedure TestPointersOver32Bits;
    procedure TestBufferTypes;
  end;

  TTestImageFiles = class(TImagingTestCase)
  published
    procedure TestLoadFromTestData;
    procedure TestSaveAndLoadMemory;
    procedure TestStreamSeekOver4GB;
    procedure TestReadRestOfInput;
  end;

implementation

type
  { Stream with no data that only keeps the position, for seeks to large positions.}
  TSeekOnlyStream = class(TStream)
  private
    FPosition: Int64;
  public
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

function TSeekOnlyStream.Read(var Buffer; Count: LongInt): LongInt;
begin
  Result := 0;
end;

function TSeekOnlyStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  Result := 0;
end;

function TSeekOnlyStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
const
  VirtualSize = Int64(10) shl 30;
begin
  case Origin of
    soBeginning: FPosition := Offset;
    soCurrent:   FPosition := FPosition + Offset;
    soEnd:       FPosition := VirtualSize + Offset;
  end;
  Result := FPosition;
end;

{ TTestImageLifetime }

procedure TTestImageLifetime.SetUp;
begin
  InitImage(FImage);
end;

procedure TTestImageLifetime.TearDown;
begin
  FreeImage(FImage);
end;

procedure TTestImageLifetime.CheckBitsAreZero(const Image: TImageData; const Msg: string);
var
  I: NativeInt;
begin
  for I := 0 to Image.Size - 1 do
  begin
    if PBuffer(Image.Bits)[I] <> 0 then
      Fail(Format('%s: byte at %d is not zero', [Msg, Int64(I)]));
  end;
end;

procedure TTestImageLifetime.TestInitImage;
var
  Image: TImageData;
begin
  FillChar(Image, SizeOf(Image), $FF);
  InitImage(Image);

  CheckEquals(0, Image.Width, 'Width');
  CheckEquals(0, Image.Height, 'Height');
  CheckEquals(Ord(ifUnknown), Ord(Image.Format), 'Format');
  CheckEquals(Int64(0), Image.Size, 'Size');
  CheckTrue(Image.Bits = nil, 'Bits');
  CheckTrue(Image.Palette = nil, 'Palette');
  CheckTrue(Image.Tag = nil, 'Tag');
end;

procedure TTestImageLifetime.TestTestImage;
begin
  CheckFalse(TestImage(FImage), 'Initialized record is not a valid image');

  CheckTrue(NewImage(10, 20, ifR8G8B8, FImage), 'NewImage');
  CheckTrue(TestImage(FImage), 'New image is valid');

  // Size must match dimensions and format
  Inc(FImage.Size);
  CheckFalse(TestImage(FImage), 'Wrong size');
  Dec(FImage.Size);

  Inc(FImage.Width);
  CheckFalse(TestImage(FImage), 'Wrong width');
  Dec(FImage.Width);

  FImage.Format := ifA8R8G8B8;
  CheckFalse(TestImage(FImage), 'Wrong format');
  FImage.Format := ifUnknown;
  CheckFalse(TestImage(FImage), 'Unknown format');
  FImage.Format := ifR8G8B8;

  CheckTrue(TestImage(FImage), 'Valid again');
end;

procedure TTestImageLifetime.TestNewImage;
begin
  CheckTrue(NewImage(7, 5, ifA8R8G8B8, FImage), 'NewImage');

  CheckEquals(7, FImage.Width, 'Width');
  CheckEquals(5, FImage.Height, 'Height');
  CheckEquals(Ord(ifA8R8G8B8), Ord(FImage.Format), 'Format');
  CheckEquals(Int64(7 * 5 * 4), FImage.Size, 'Size');
  CheckTrue(FImage.Bits <> nil, 'Bits allocated');
  CheckTrue(FImage.Palette = nil, 'No palette');
  CheckBitsAreZero(FImage, 'New image');
end;

procedure TTestImageLifetime.TestNewImageIndexed;
var
  Info: TImageFormatInfo;
  I: Integer;
begin
  CheckTrue(NewImage(7, 5, ifIndex8, FImage), 'NewImage');
  CheckTrue(GetImageFormatInfo(ifIndex8, Info), 'Format info');

  CheckEquals(Int64(7 * 5), FImage.Size, 'Size');
  CheckEquals(256, Info.PaletteEntries, 'Palette entries');
  CheckTrue(FImage.Palette <> nil, 'Palette allocated');
  CheckBitsAreZero(FImage, 'New indexed image');

  for I := 0 to Info.PaletteEntries - 1 do
    CheckEqualsColor32(0, FImage.Palette[I].Color, 'Palette entry ' + IntToStr(I));
end;

procedure TTestImageLifetime.TestNewImageDefaultFormat;
begin
  CheckTrue(NewImage(4, 4, ifDefault, FImage), 'NewImage');
  CheckEquals(Ord(ifA8R8G8B8), Ord(FImage.Format), 'Default format');
end;

procedure TTestImageLifetime.TestNewImageBlockCompressed;
begin
  // Dimensions are rounded up to whole 4x4 blocks
  CheckTrue(NewImage(5, 9, ifDXT1, FImage), 'NewImage');
  CheckEquals(8, FImage.Width, 'Width');
  CheckEquals(12, FImage.Height, 'Height');
  CheckEquals(Int64(2 * 3 * 8), FImage.Size, 'Size');
end;

procedure TTestImageLifetime.TestNewImageReplacesOldOne;
begin
  CheckTrue(NewImage(16, 16, ifIndex8, FImage), 'First image');
  CheckTrue(NewImage(3, 2, ifGray16, FImage), 'Second image');

  CheckEquals(3, FImage.Width, 'Width');
  CheckEquals(2, FImage.Height, 'Height');
  CheckEquals(Ord(ifGray16), Ord(FImage.Format), 'Format');
  CheckEquals(Int64(3 * 2 * 2), FImage.Size, 'Size');
  CheckTrue(FImage.Palette = nil, 'Palette of the first image is gone');
end;

procedure TTestImageLifetime.TestFreeImage;
begin
  CheckTrue(NewImage(16, 16, ifIndex8, FImage), 'NewImage');
  FreeImage(FImage);

  CheckEquals(0, FImage.Width, 'Width');
  CheckEquals(0, FImage.Height, 'Height');
  CheckEquals(Ord(ifUnknown), Ord(FImage.Format), 'Format');
  CheckEquals(Int64(0), FImage.Size, 'Size');
  CheckTrue(FImage.Bits = nil, 'Bits');
  CheckTrue(FImage.Palette = nil, 'Palette');

  // Freeing image that's not allocated is fine
  FreeImage(FImage);
  CheckFalse(TestImage(FImage), 'Freed image is not valid');
end;

procedure TTestImageLifetime.TestCloneImage;
var
  Clone: TImageData;
  I: NativeInt;
begin
  CheckTrue(NewImage(9, 6, ifR8G8B8, FImage), 'NewImage');
  for I := 0 to FImage.Size - 1 do
    PBuffer(FImage.Bits)[I] := Byte(I * 7 + 1);

  InitImage(Clone);
  try
    CheckTrue(CloneImage(FImage, Clone), 'CloneImage');
    CheckImagesEqual(FImage, Clone, 'Clone');
    CheckTrue(FImage.Bits <> Clone.Bits, 'Clone has its own bits');

    // Change of clone must not change the original
    PBuffer(Clone.Bits)[0] := 255;
    CheckEquals(1, PBuffer(FImage.Bits)[0], 'Original after change of clone');

    // Clone into existing image replaces it
    CheckTrue(NewImage(2, 2, ifGray8, Clone), 'NewImage for clone');
    CheckTrue(CloneImage(FImage, Clone), 'CloneImage over existing image');
    CheckImagesEqual(FImage, Clone, 'Clone over existing image');
  finally
    FreeImage(Clone);
  end;
end;

procedure TTestImageLifetime.TestConvertImageInto;
var
  Original, Expected, Converted: TImageData;
  I: NativeInt;
begin
  CheckTrue(NewImage(9, 6, ifR8G8B8, FImage), 'NewImage');
  for I := 0 to FImage.Size - 1 do
    PBuffer(FImage.Bits)[I] := Byte(I * 7 + 1);

  InitImage(Original);
  InitImage(Expected);
  InitImage(Converted);
  try
    CheckTrue(CloneImage(FImage, Original), 'CloneImage of original');
    CheckTrue(CloneImage(FImage, Expected) and ConvertImage(Expected, ifIndex8), 'Expected result');

    // Image held by destination is replaced (and freed, heaptrc would report a leak)
    CheckTrue(NewImage(16, 16, ifA16R16G16B16, Converted), 'NewImage for destination');
    CheckTrue(ConvertImageInto(FImage, Converted, ifIndex8), 'ConvertImageInto');
    CheckImagesEqual(Expected, Converted, 'Converted image');
    CheckImagesEqual(Original, FImage, 'Source image');
  finally
    FreeImage(Original);
    FreeImage(Expected);
    FreeImage(Converted);
  end;
end;

procedure TTestImageLifetime.TestConvertImageIntoSameRecord;
var
  Expected: TImageData;
begin
  CheckTrue(NewImage(5, 4, ifA8R8G8B8, FImage), 'NewImage');
  FillChar(FImage.Bits^, FImage.Size, $5A);

  InitImage(Expected);
  try
    CheckTrue(CloneImage(FImage, Expected) and ConvertImage(Expected, ifGray16), 'Expected result');
    // Same record as source and destination is converted in place
    CheckTrue(ConvertImageInto(FImage, FImage, ifGray16), 'ConvertImageInto');
    CheckImagesEqual(Expected, FImage, 'Converted image');
  finally
    FreeImage(Expected);
  end;
end;

procedure TTestImageLifetime.TestConvertImageIntoSharedBits;
var
  Shallow: TImageData;
begin
  CheckTrue(NewImage(5, 4, ifGray8, FImage), 'NewImage');
  FillChar(FImage.Bits^, FImage.Size, 77);

  // Destination record is a copy of the source record: memory of the
  // source must not be freed
  Shallow := FImage;
  try
    CheckTrue(ConvertImageInto(FImage, Shallow, ifA8R8G8B8), 'ConvertImageInto');
    CheckTrue(Shallow.Bits <> FImage.Bits, 'Destination has its own bits');
    CheckTrue(TestImage(FImage), 'Source is still valid');
    CheckEquals(77, PBuffer(FImage.Bits)[FImage.Size - 1], 'Source pixels');
    CheckEquals(77, GetPixel32(Shallow, 4, 3).R, 'Converted pixel');
  finally
    FreeImage(Shallow);
  end;
end;

procedure TTestImageLifetime.TestConvertImageIntoSpecialFormats;
var
  Compressed, Expected, Converted: TImageData;
  I: NativeInt;
begin
  CheckTrue(NewImage(16, 8, ifA8R8G8B8, FImage), 'NewImage');
  for I := 0 to FImage.Size - 1 do
    PBuffer(FImage.Bits)[I] := Byte(I * 13);

  InitImage(Compressed);
  InitImage(Expected);
  InitImage(Converted);
  try
    // To special format
    CheckTrue(CloneImage(FImage, Expected) and ConvertImage(Expected, ifDXT1), 'Expected DXT1');
    CheckTrue(ConvertImageInto(FImage, Compressed, ifDXT1), 'ConvertImageInto DXT1');
    CheckImagesEqual(Expected, Compressed, 'DXT1 image');

    // From special format
    CheckTrue(CloneImage(Compressed, Expected) and ConvertImage(Expected, ifA8R8G8B8), 'Expected A8R8G8B8');
    CheckTrue(ConvertImageInto(Compressed, Converted, ifA8R8G8B8), 'ConvertImageInto A8R8G8B8');
    CheckImagesEqual(Expected, Converted, 'Image from DXT1');
  finally
    FreeImage(Compressed);
    FreeImage(Expected);
    FreeImage(Converted);
  end;
end;

procedure TTestImageLifetime.TestConvertImageIntoInvalidSource;
var
  Invalid: TImageData;
  OldBits: Pointer;
begin
  CheckTrue(NewImage(3, 2, ifGray8, FImage), 'NewImage');
  OldBits := FImage.Bits;
  InitImage(Invalid);

  // Destination stays as it was
  CheckFalse(ConvertImageInto(Invalid, FImage, ifA8R8G8B8), 'ConvertImageInto of invalid image');
  CheckTrue(FImage.Bits = OldBits, 'Destination bits');
  CheckEquals(Ord(ifGray8), Ord(FImage.Format), 'Destination format');
  CheckEquals(3, FImage.Width, 'Destination width');
end;

procedure TTestImageLifetime.TestCloneImageIndexed;
var
  Clone: TImageData;
  I: Integer;
begin
  CheckTrue(NewImage(4, 4, ifIndex8, FImage), 'NewImage');
  for I := 0 to 255 do
    FImage.Palette[I].Color := $FF000000 or UInt32(I shl 16) or UInt32(I);
  for I := 0 to FImage.Size - 1 do
    PBuffer(FImage.Bits)[I] := I * 16;

  InitImage(Clone);
  try
    CheckTrue(CloneImage(FImage, Clone), 'CloneImage');
    CheckImagesEqual(FImage, Clone, 'Indexed clone');
    CheckTrue(Pointer(FImage.Palette) <> Pointer(Clone.Palette), 'Clone has its own palette');

    Clone.Palette[1].Color := 0;
    CheckEqualsColor32($FF010001, FImage.Palette[1].Color, 'Original palette after change of clone');
  finally
    FreeImage(Clone);
  end;

  // Source that is not a valid image is refused
  FreeImage(FImage);
  InitImage(Clone);
  CheckFalse(CloneImage(FImage, Clone), 'CloneImage of invalid image');
end;

{ TTestPixelAccess }

procedure TTestPixelAccess.SetUp;
begin
  InitImage(FImage);
end;

procedure TTestPixelAccess.TearDown;
begin
  FreeImage(FImage);
end;

procedure TTestPixelAccess.TestPixel32InARGB;
const
  Width = 7;
  Height = 5;
  X = 3;
  Y = 2;
var
  Color: TColor32Rec;
  Pixels: PUInt32Array;
  I: Integer;
begin
  CheckTrue(NewImage(Width, Height, ifA8R8G8B8, FImage), 'NewImage');
  Pixels := FImage.Bits;

  Color.Color := $80FF4020;
  SetPixel32(FImage, X, Y, Color);
  CheckEqualsColor32($80FF4020, GetPixel32(FImage, X, Y).Color, 'Pixel read back');
  CheckEqualsColor32($80FF4020, Pixels[Y * Width + X], 'Pixel in memory');

  // Channels in memory are in B, G, R, A order
  CheckEquals($20, Color.B, 'Blue channel');
  CheckEquals($40, Color.G, 'Green channel');
  CheckEquals($FF, Color.R, 'Red channel');
  CheckEquals($80, Color.A, 'Alpha channel');

  for I := 0 to Width * Height - 1 do
  begin
    if I <> Y * Width + X then
      CheckEqualsColor32(0, Pixels[I], 'Other pixel ' + IntToStr(I));
  end;

  // Corners of the image
  Color.Color := $FFFFFFFF;
  SetPixel32(FImage, 0, 0, Color);
  SetPixel32(FImage, Width - 1, Height - 1, Color);
  CheckEqualsColor32($FFFFFFFF, Pixels[0], 'First pixel');
  CheckEqualsColor32($FFFFFFFF, Pixels[Width * Height - 1], 'Last pixel');
end;

procedure TTestPixelAccess.TestPixel32InRGB24;
const
  Width = 5;
  X = 4;
  Y = 1;
var
  Color: TColor32Rec;
  Pixel: PByteArray;
begin
  CheckTrue(NewImage(Width, 3, ifR8G8B8, FImage), 'NewImage');

  Color.Color := $40AABBCC;
  SetPixel32(FImage, X, Y, Color);

  Pixel := @PBuffer(FImage.Bits)[(Y * Width + X) * 3];
  CheckEquals($CC, Pixel[0], 'Blue in memory');
  CheckEquals($BB, Pixel[1], 'Green in memory');
  CheckEquals($AA, Pixel[2], 'Red in memory');
  // Format without alpha is read as fully opaque
  CheckEqualsColor32($FFAABBCC, GetPixel32(FImage, X, Y).Color, 'Pixel read back');
  CheckEqualsColor32($FF000000, GetPixel32(FImage, 0, 0).Color, 'Untouched pixel');
end;

procedure TTestPixelAccess.TestPixel32InGray8;
const
  Width = 4;
var
  Color: TColor32Rec;
begin
  CheckTrue(NewImage(Width, 4, ifGray8, FImage), 'NewImage');

  Color.Color := $FFFFFFFF;
  SetPixel32(FImage, 1, 0, Color);
  Color.Color := $FF808080;
  SetPixel32(FImage, 2, 3, Color);

  CheckEquals(255, PBuffer(FImage.Bits)[1], 'White in memory');
  CheckEquals(128, PBuffer(FImage.Bits)[3 * Width + 2], 'Gray in memory');
  CheckEqualsColor32($FFFFFFFF, GetPixel32(FImage, 1, 0).Color, 'White read back');
  CheckEqualsColor32($FF808080, GetPixel32(FImage, 2, 3).Color, 'Gray read back');
  CheckEqualsColor32($FF000000, GetPixel32(FImage, 0, 0).Color, 'Untouched pixel is black');
end;

{ TTestImageFiles }

procedure TTestImageFiles.TestLoadFromTestData;
var
  FileName: string;
  Image: TImageData;
begin
  FileName := GetTestDataFile('Tigers.bmp');
  CheckTrue(FileExists(FileName), 'Test image exists: ' + FileName);
  CheckEquals('bmp', DetermineFileFormat(FileName), 'File format');

  InitImage(Image);
  try
    CheckTrue(LoadImageFromFile(FileName, Image), 'LoadImageFromFile');
    CheckTrue(TestImage(Image), 'Loaded image is valid');
    CheckEquals(513, Image.Width, 'Width');
    CheckEquals(378, Image.Height, 'Height');
  finally
    FreeImage(Image);
  end;
end;

procedure TTestImageFiles.TestSaveAndLoadMemory;
const
  Capacity = 64 * 1024;
var
  Image, Loaded: TImageData;
  Images: TDynImageDataArray;
  Buffer: TDynByteArray;
  Size: Int64;
begin
  InitImage(Image);
  InitImage(Loaded);
  try
    CheckTrue(NewImage(31, 17, ifR8G8B8, Image), 'NewImage');
    FillChar(Image.Bits^, Image.Size, $5A);
    SetLength(Buffer, Capacity);

    // Size is the capacity of the memory on input and size of the file on output
    Size := Capacity;
    CheckTrue(SaveImageToMemory('tga', Pointer(Buffer), Size, Image), 'SaveImageToMemory');
    CheckTrue((Size > Image.Size) and (Size < Capacity), Format('Size of the saved file: %d', [Size]));
    CheckEquals('tga', DetermineMemoryFormat(Pointer(Buffer), Size), 'DetermineMemoryFormat');
    CheckTrue(LoadImageFromMemory(Pointer(Buffer), Size, Loaded), 'LoadImageFromMemory');
    CheckImagesEqual(Image, Loaded, 'Loaded image');

    // Multi image variants
    SetLength(Images, 1);
    Images[0] := Image;
    Size := Capacity;
    CheckTrue(SaveMultiImageToMemory('tga', Pointer(Buffer), Size, Images), 'SaveMultiImageToMemory');
    Images := nil;
    CheckTrue(LoadMultiImageFromMemory(Pointer(Buffer), Size, Images), 'LoadMultiImageFromMemory');
    CheckEquals(1, Length(Images), 'Number of loaded images');
    CheckImagesEqual(Image, Images[0], 'Loaded multi image');
  finally
    FreeImage(Image);
    FreeImage(Loaded);
    FreeImagesInArray(Images);
  end;
end;

{ TTestImageGeometry }

{$IF Defined(CPU64) or Defined(CPUX64) or Defined(CPU64BITS)}
  {$DEFINE HAS_64BIT_ADDRESSES}
{$IFEND}

procedure TTestImageGeometry.SetUp;
begin
  InitImage(FImage);
end;

procedure TTestImageGeometry.TearDown;
begin
  FreeImage(FImage);
end;

procedure TTestImageGeometry.TestPixelCount;
begin
  CheckTrue(NewImage(7, 5, ifR8G8B8, FImage), 'NewImage');
  CheckEquals(35, GetPixelCount(FImage), '7x5');
  CheckTrue(NewImage(1, 1, ifA16R16G16B16, FImage), 'NewImage');
  CheckEquals(1, GetPixelCount(FImage), '1x1');
  CheckTrue(NewImage(1000, 3000, ifGray8, FImage), 'NewImage');
  CheckEquals(3000000, GetPixelCount(FImage), '1000x3000');
  // Dimensions of images in block formats are rounded up
  CheckTrue(NewImage(5, 5, ifDXT1, FImage), 'NewImage');
  CheckEquals(64, GetPixelCount(FImage), '5x5 DXT1');
end;

procedure TTestImageGeometry.TestPixelCountOver32Bits;
{$IFDEF HAS_64BIT_ADDRESSES}
var
  Image: TImageData;
begin
  // Bits are not touched so no memory needs to be allocated
  InitImage(Image);
  Image.Format := ifGray8;
  Image.Width := 46341;
  Image.Height := 46341;
  CheckEquals(Int64(2147488281), Int64(GetPixelCount(Image)), 'Just over 2^31');
  Image.Width := 65536;
  Image.Height := 65536;
  CheckEquals(Int64(4294967296), Int64(GetPixelCount(Image)), '2^32');
  Image.Width := 48000;
  Image.Height := 44800;
  CheckEquals(Int64(2150400000), Int64(GetPixelCount(Image)), '48000x44800');
{$ELSE}
begin
  // Such images cannot exist with 32-bit addresses
{$ENDIF}
end;

procedure TTestImageGeometry.TestPixelAndScanlinePointers;
const
  Width = 9;
  Height = 6;
  Formats: array[0..6] of TImageFormat = (ifIndex8, ifGray8, ifR5G6B5, ifR8G8B8,
    ifA8R8G8B8, ifA16R16G16B16, ifA32R32G32B32F);
var
  F, X, Y: Integer;
  Info: TImageFormatInfo;
  Expected: Pointer;
  Msg: string;
begin
  for F := Low(Formats) to High(Formats) do
  begin
    CheckTrue(NewImage(Width, Height, Formats[F], FImage), 'NewImage');
    CheckTrue(GetImageFormatInfo(Formats[F], Info), 'Format info');

    for Y := 0 to Height - 1 do
    begin
      Msg := Format('%s line %d', [Info.Name, Y]);
      Expected := @PBuffer(FImage.Bits)[Y * Width * Info.BytesPerPixel];
      CheckTrue(GetScanlinePointer(FImage, Y) = Expected, Msg + ': GetScanlinePointer');
      CheckTrue(GetScanlinePointer(FImage, Y, Info.BytesPerPixel) = Expected,
        Msg + ': GetScanlinePointer with size of pixels');

      for X := 0 to Width - 1 do
      begin
        Expected := @PBuffer(FImage.Bits)[(Y * Width + X) * Info.BytesPerPixel];
        CheckTrue(GetPixelPointer(FImage, X, Y) = Expected, Format('%s: GetPixelPointer of %d', [Msg, X]));
        CheckTrue(GetPixelPointer(FImage, X, Y, Info.BytesPerPixel) = Expected,
          Format('%s: GetPixelPointer of %d with size of pixels', [Msg, X]));
      end;
    end;
  end;
end;

procedure TTestImageGeometry.TestPointersOver32Bits;
{$IFDEF HAS_64BIT_ADDRESSES}
var
  Image: TImageData;
begin
  // Addresses are only computed: with no bits the result is the offset
  InitImage(Image);
  Image.Format := ifA8R8G8B8;
  Image.Width := 70000;
  Image.Height := 70000;

  CheckEquals(Int64(0), Int64(PtrUInt(GetPixelPointer(Image, 0, 0))), 'The first pixel');
  CheckEquals(Int64(19599999996), Int64(PtrUInt(GetPixelPointer(Image, 69999, 69999))), 'The last pixel');
  CheckEquals(Int64(19599720000), Int64(PtrUInt(GetScanlinePointer(Image, 69999))), 'The last scanline');
  // Offset just over 2^31 and 2^32 bytes
  CheckEquals(Int64(2147600000), Int64(PtrUInt(GetScanlinePointer(Image, 7670))), 'Scanline over 2 GB');
  CheckEquals(Int64(4295200004), Int64(PtrUInt(GetPixelPointer(Image, 1, 15340))), 'Pixel over 4 GB');

  Image.Format := ifA32R32G32B32F;
  CheckEquals(Int64(78399999984), Int64(PtrUInt(GetPixelPointer(Image, 69999, 69999))), 'The last pixel, 16 bytes');

  // Variants with the size of pixels given by the caller
  CheckEquals(Int64(19599999996), Int64(PtrUInt(GetPixelPointer(Image, 69999, 69999, 4))), 'The last pixel, given size');
  CheckEquals(Int64(4295200004), Int64(PtrUInt(GetPixelPointer(Image, 1, 15340, 4))), 'Pixel over 4 GB, given size');
  CheckEquals(Int64(19599720000), Int64(PtrUInt(GetScanlinePointer(Image, 69999, 4))), 'The last scanline, given size');
  CheckEquals(Int64(78398880000), Int64(PtrUInt(GetScanlinePointer(Image, 69999, 16))), 'The last scanline, 16 bytes');
{$ELSE}
begin
  // Such images cannot exist with 32-bit addresses
{$ENDIF}
end;

procedure TTestImageGeometry.TestBufferTypes;
var
  Pixels: array[0..3] of TColor32Rec;
  Words: array[0..3] of Word;
  Pixels24: array[0..3] of TColor24Rec;
{$IFDEF HAS_64BIT_ADDRESSES}
  Index: NativeInt;
  Base: Pointer;
{$ENDIF}
begin
  FillChar(Pixels, SizeOf(Pixels), 0);
  PColor32RecBuffer(@Pixels)[2].Color := $11223344;
  PColor32RecBuffer(@Pixels)[3].A := $55;
  CheckEqualsColor32($11223344, Pixels[2].Color, 'Pixel written through PColor32RecBuffer');
  CheckEquals($55, Pixels[3].A, 'Channel written through PColor32RecBuffer');
  CheckEquals($44, PBuffer(@Pixels)[8], 'The same pixel through PBuffer');

  FillChar(Words, SizeOf(Words), 0);
  PWordBuffer(@Words)[3] := $ABCD;
  CheckEquals($ABCD, Words[3], 'Word written through PWordBuffer');

  // Pointer math on 3 byte elements is wrong in Delphi, see PColor24RecBuffer
  FillChar(Pixels24, SizeOf(Pixels24), 0);
  PColor24RecBuffer(@Pixels24)[3].R := $77;
  CheckEquals($77, Pixels24[3].R, 'Pixel written through PColor24RecBuffer');
  CheckEquals($77, PBuffer(@Pixels24)[11], 'The same pixel through PBuffer');

{$IFDEF HAS_64BIT_ADDRESSES}
  // Addresses are only computed: the indexes are over the limits of
  // PByteArray, PWordArray and PColor32RecArray
  Base := nil;
  Index := NativeInt(3) * 1000 * 1000 * 1000;
  CheckEquals(Int64(3000000000), Int64(PtrUInt(@PBuffer(Base)[Index])), 'PBuffer over 2 GB');
  CheckEquals(Int64(6000000000), Int64(PtrUInt(@PWordBuffer(Base)[Index])), 'PWordBuffer over 2^31 words');
  CheckEquals(Int64(12000000000), Int64(PtrUInt(@PColor32RecBuffer(Base)[Index])), 'PColor32RecBuffer over 2^31 pixels');
{$IF Defined(FPC) or (CompilerVersion >= 36.0)}
  // Array type in older Delphi, the index is over its range
  CheckEquals(Int64(9000000000), Int64(PtrUInt(@PColor24RecBuffer(Base)[Index])), 'PColor24RecBuffer over 2^31 pixels');
{$IFEND}
  CheckEquals(Int64(18000000000), Int64(PtrUInt(@PColor48RecBuffer(Base)[Index])), 'PColor48RecBuffer over 2^31 pixels');
  CheckEquals(Int64(24000000000), Int64(PtrUInt(@PColor64RecBuffer(Base)[Index])), 'PColor64RecBuffer over 2^31 pixels');
  CheckEquals(Int64(36000000000), Int64(PtrUInt(@PColor96FPRecBuffer(Base)[Index])), 'PColor96FPRecBuffer over 2^31 pixels');
  CheckEquals(Int64(48000000000), Int64(PtrUInt(@PColorFPRecBuffer(Base)[Index])), 'PColorFPRecBuffer over 2^31 pixels');
  CheckEquals(Int64(12000000000), Int64(PtrUInt(@PUInt32Buffer(Base)[Index])), 'PUInt32Buffer over 2^31 elements');
  CheckEquals(Int64(24000000000), Int64(PtrUInt(@PInt64Buffer(Base)[Index])), 'PInt64Buffer over 2^31 elements');
  CheckEquals(Int64(12000000000), Int64(PtrUInt(@PSingleBuffer(Base)[Index])), 'PSingleBuffer over 2^31 elements');
  Index := 600 * 1000 * 1000;
  CheckEquals(Int64(2400000000), Int64(PtrUInt(@PColor32RecBuffer(Base)[Index])), 'PColor32RecBuffer over MaxInt div 4 pixels');
{$ENDIF}
end;

procedure TTestImageFiles.TestStreamSeekOver4GB;
const
  Far = Int64(5) shl 30 + 3;
var
  Stream: TSeekOnlyStream;
  Handle: TImagingHandle;
begin
  // Stream IO must not pass positions to the 32-bit TStream.Seek overload
  Stream := TSeekOnlyStream.Create;
  try
    Handle := StreamIO.Open(Pointer(Stream), omReadWrite);
    try
      CheckEquals(Far, StreamIO.Seek(Handle, Far, smFromBeginning), 'Seek from beginning');
      CheckEquals(Far, StreamIO.Tell(Handle), 'Position after seek from beginning');
      CheckEquals(Far + Far, StreamIO.Seek(Handle, Far, smFromCurrent), 'Seek from current');
      CheckEquals(Int64(10) shl 30 - Far, StreamIO.Seek(Handle, -Far, smFromEnd), 'Seek from end');
      CheckEquals(Int64(3), StreamIO.Seek(Handle, 3, smFromBeginning), 'Seek back');
    finally
      StreamIO.Close(Handle);
    end;
  finally
    Stream.Free;
  end;
end;

procedure TTestImageFiles.TestReadRestOfInput;
const
  DataSize = 300000; // More blocks of growing size
  Start = 1000;
var
  Stream: TMemoryStream;
  Handle: TImagingHandle;
  Buffer: TDynByteArray;
  I: Integer;
  Same: Boolean;

  procedure CheckRead(MaxSize: NativeInt; Expected: Boolean; ExpectedSize: Integer; const Msg: string);
  begin
    StreamIO.Seek(Handle, Start, smFromBeginning);
    CheckEquals(Expected, ReadRestOfInput(StreamIO, Handle, Buffer, MaxSize), Msg + ': result');
    CheckEquals(ExpectedSize, Length(Buffer), Msg + ': size');
  end;

begin
  Stream := TMemoryStream.Create;
  try
    Stream.Size := DataSize;
    for I := 0 to DataSize - 1 do
      PBuffer(Stream.Memory)[I] := Byte(I * 7 + I shr 8);
    Handle := StreamIO.Open(Pointer(Stream), omReadOnly);
    try
      CheckRead(High(NativeInt), True, DataSize - Start, 'Unlimited');
      Same := CompareMem(@PBuffer(Stream.Memory)[Start], Pointer(Buffer), DataSize - Start);
      CheckTrue(Same, 'Data read');
      CheckEquals(Int64(DataSize), StreamIO.Tell(Handle), 'Position at the end');

      CheckRead(DataSize - Start, True, DataSize - Start, 'Limit equal to the rest');
      CheckRead(DataSize - Start - 1, False, 0, 'Limit smaller than the rest');
      CheckRead(100, False, 0, 'Limit smaller than the first block');

      StreamIO.Seek(Handle, 0, smFromEnd);
      CheckTrue(ReadRestOfInput(StreamIO, Handle, Buffer), 'At the end: result');
      CheckEquals(0, Length(Buffer), 'At the end: size');
    finally
      StreamIO.Close(Handle);
    end;
  finally
    Stream.Free;
  end;
end;

initialization
  RegisterImagingTest(TTestImageLifetime);
  RegisterImagingTest(TTestImageGeometry);
  RegisterImagingTest(TTestPixelAccess);
  RegisterImagingTest(TTestImageFiles);
end.
