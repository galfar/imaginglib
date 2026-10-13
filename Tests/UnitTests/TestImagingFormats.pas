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

{ Tests of image data formats: format descriptors, sizes of pixel data,
  and conversions of single values.}
unit TestImagingFormats;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, ImagingTypes, Imaging, ImagingFormats, ImagingUtility,
  ImagingTestCase;

type
  TTestFormatInfos = class(TImagingTestCase)
  published
    procedure TestInvalidFormats;
    procedure TestInfoConsistency;
  end;

  TTestPixelsSize = class(TImagingTestCase)
  private
    procedure CheckSize(const Expected: Int64; AFormat: TImageFormat; Width, Height: Integer);
  published
    procedure TestStandardFormats;
    procedure TestBinary;
    procedure TestBlockCompressed;
    procedure TestOver32Bits;
    procedure TestMaxDimensions;
    procedure TestTestImageOver32Bits;
  end;

  { Checks whether pixels with 16 and 32 bits use their alpha bits.}
  TTestAlphaChecks = class(TImagingTestCase)
  published
    procedure Test16Bit;
    procedure Test32Bit;
  end;

  TTestHalfFloat = class(TImagingTestCase)
  private
    procedure CheckNormalized(F: Single);
    procedure CheckDenormalized(F: Single);
  published
    procedure TestRoundTrip;
    procedure TestSpecialValues;
  end;

implementation

uses
  Math;

{ TTestFormatInfos }

procedure TTestFormatInfos.TestInvalidFormats;
var
  Info: TImageFormatInfo;
begin
  CheckFalse(IsImageFormatValid(ifUnknown), 'ifUnknown is not valid');
  CheckFalse(GetImageFormatInfo(ifUnknown, Info), 'No info for ifUnknown');
  CheckEquals(Int64(0), GetPixelsSize(ifUnknown, 16, 16), 'No size for ifUnknown');

  // Value in a gap between defined formats
  CheckFalse(IsImageFormatValid(TImageFormat(8)), 'Undefined format is not valid');
  CheckFalse(GetImageFormatInfo(TImageFormat(8), Info), 'No info for undefined format');
  CheckEquals(Int64(0), GetPixelsSize(TImageFormat(8), 16, 16), 'No size for undefined format');

  CheckTrue(IsImageFormatValid(ifA8R8G8B8), 'ifA8R8G8B8 is valid');

  // ifDefault is an alias: it has info of the format used as the default one
  CheckTrue(GetImageFormatInfo(ifDefault, Info), 'Info for ifDefault');
  CheckEquals(Ord(ifA8R8G8B8), Ord(Info.Format), 'Format in info for ifDefault');
  CheckEquals(Int64(16 * 16 * 4), GetPixelsSize(ifDefault, 16, 16), 'Size for ifDefault');
end;

procedure TTestFormatInfos.TestInfoConsistency;
var
  Fmt: TImageFormat;
  Info, OtherInfo: TImageFormatInfo;
  Name: string;
  Count: Integer;
begin
  Count := 0;

  for Fmt := Low(TImageFormat) to High(TImageFormat) do
  begin
    if not IsImageFormatValid(Fmt) or (Fmt = ifDefault) then
      Continue;

    Inc(Count);
    Name := 'Format ' + IntToStr(Ord(Fmt));
    CheckTrue(GetImageFormatInfo(Fmt, Info), Name + ': has info');
    CheckEquals(Ord(Fmt), Ord(Info.Format), Name + ': format in info');
    CheckTrue(Info.Name[0] <> #0, Name + ': has name');

    Name := Info.Name;
    CheckEquals(Name, GetFormatName(Fmt), Name + ': GetFormatName');
    CheckTrue(Assigned(Info.GetPixelsSize), Name + ': GetPixelsSize assigned');
    CheckTrue(Assigned(Info.CheckDimensions), Name + ': CheckDimensions assigned');
    CheckTrue(Info.ChannelCount > 0, Name + ': has channels');
    CheckEquals(Info.IsIndexed, Info.PaletteEntries > 0, Name + ': palette only for indexed');

    if Info.IsSpecial then
    begin
      CheckTrue(GetImageFormatInfo(Info.SpecialNearestFormat, OtherInfo), Name + ': nearest format is valid');
      CheckFalse(OtherInfo.IsSpecial, Name + ': nearest format is not special');
    end
    else
    begin
      CheckTrue(Info.BytesPerPixel > 0, Name + ': has bytes per pixel');
      CheckTrue(Assigned(Info.GetPixel32), Name + ': GetPixel32 assigned');
      CheckTrue(Assigned(Info.SetPixel32), Name + ': SetPixel32 assigned');
      CheckTrue(Assigned(Info.GetPixelFP), Name + ': GetPixelFP assigned');
      CheckTrue(Assigned(Info.SetPixelFP), Name + ': SetPixelFP assigned');
    end;

    if Info.UsePixelFormat then
      CheckTrue(Info.PixelFormat <> nil, Name + ': pixel format assigned');

    if Info.RBSwapFormat <> ifUnknown then
    begin
      CheckTrue(GetImageFormatInfo(Info.RBSwapFormat, OtherInfo), Name + ': RB swap format is valid');
      CheckEquals(Ord(Fmt), Ord(OtherInfo.RBSwapFormat), Name + ': RB swap format points back');
      CheckEquals(Info.BytesPerPixel, OtherInfo.BytesPerPixel, Name + ': RB swap format has the same pixel size');
      CheckTrue(Info.IsRBSwapped <> OtherInfo.IsRBSwapped, Name + ': just one of RB swap pair is swapped');
    end;
  end;

  CheckTrue(Count > 30, 'Most of the formats are valid');
end;

{ TTestPixelsSize }

procedure TTestPixelsSize.CheckSize(const Expected: Int64; AFormat: TImageFormat;
  Width, Height: Integer);
begin
  CheckEquals(Expected, GetPixelsSize(AFormat, Width, Height),
    Format('Size of %dx%d %s', [Width, Height, GetFormatName(AFormat)]));
end;

procedure TTestPixelsSize.TestStandardFormats;
var
  Fmt: TImageFormat;
  Info: TImageFormatInfo;
begin
  for Fmt := Low(TImageFormat) to High(TImageFormat) do
  begin
    if not GetImageFormatInfo(Fmt, Info) or Info.IsSpecial or (Fmt = ifDefault) then
      Continue;

    CheckSize(Info.BytesPerPixel, Fmt, 1, 1);
    CheckSize(7 * 5 * Info.BytesPerPixel, Fmt, 7, 5);
    CheckSize(1024 * 768 * Info.BytesPerPixel, Fmt, 1024, 768);
  end;

  CheckSize(1, ifIndex8, 1, 1);
  CheckSize(2, ifR5G6B5, 1, 1);
  CheckSize(3, ifR8G8B8, 1, 1);
  CheckSize(4, ifA8R8G8B8, 1, 1);
  CheckSize(8, ifA16R16G16B16, 1, 1);
  CheckSize(12, ifR32G32B32F, 1, 1);
  CheckSize(16, ifA32R32G32B32F, 1, 1);
end;

procedure TTestPixelsSize.TestBinary;
begin
  // One bit per pixel, each row is padded to whole bytes
  CheckSize(1, ifBinary, 1, 1);
  CheckSize(3, ifBinary, 7, 3);
  CheckSize(3, ifBinary, 8, 3);
  CheckSize(6, ifBinary, 9, 3);
  CheckSize(6, ifBinary, 16, 3);
  CheckSize(6, ifBinary, 17, 2);
  CheckSize(125 * 1000, ifBinary, 1000, 1000);
end;

procedure TTestPixelsSize.TestBlockCompressed;
begin
  // Blocks of 4x4 pixels, 8 bytes each
  CheckSize(8, ifDXT1, 4, 4);
  CheckSize(32, ifDXT1, 8, 8);
  CheckSize(8, ifATI1N, 4, 4);
  // Blocks of 4x4 pixels, 16 bytes each
  CheckSize(16, ifDXT3, 4, 4);
  CheckSize(16, ifDXT5, 4, 4);
  CheckSize(16, ifATI2N, 4, 4);
  CheckSize(64 * 64, ifDXT5, 64, 64);
  // Blocks of 4x4 pixels, 4 bytes each
  CheckSize(4, ifBTC, 4, 4);
  CheckSize(16, ifBTC, 8, 8);

  // Dimensions are rounded up to whole blocks
  CheckSize(8, ifDXT1, 1, 1);
  CheckSize(8, ifDXT1, 3, 4);
  CheckSize(32, ifDXT1, 5, 5);
  CheckSize(16, ifDXT1, 5, 4);
  CheckSize(16, ifDXT3, 1, 1);
  CheckSize(64, ifDXT5, 5, 7);
  CheckSize(32, ifATI2N, 4, 5);
  CheckSize(16, ifBTC, 5, 5);
end;

procedure TTestPixelsSize.TestOver32Bits;
begin
  // Number of pixels just over 2^31
  CheckSize(Int64(2147488281), ifGray8, 46341, 46341);
  // Exactly 2^32 bytes: wraps to zero when computed in 32 bits
  CheckSize(Int64(4294967296), ifA8R8G8B8, 32768, 32768);
  CheckSize(Int64(4294967296), ifGray8, 65536, 65536);
  CheckSize(Int64(6400000000), ifA8R8G8B8, 40000, 40000);
  CheckSize(Int64(7500000000), ifR8G8B8, 50000, 50000);
  CheckSize(Int64(14400000000), ifA32B32G32R32F, 30000, 30000);
  // Just one of the dimensions is large
  CheckSize(Int64(8000000000), ifA16R16G16B16, 1000000000, 1);
  CheckSize(Int64(8000000000), ifA16R16G16B16, 1, 1000000000);

  CheckSize(Int64(137438953472), ifBinary, 1048576, 1048576);
  CheckSize(Int64(2500000000), ifBinary, 200000, 100000);

  CheckSize(Int64(2147483648), ifDXT1, 65536, 65536);
  CheckSize(Int64(4294967296), ifDXT5, 65536, 65536);
  CheckSize(Int64(4294967296), ifATI2N, 65536, 65536);
  CheckSize(Int64(1073741824), ifBTC, 65536, 65536);
  CheckSize(Int64(5000000000), ifDXT3, 100000, 50000);
end;

procedure TTestPixelsSize.TestMaxDimensions;
begin
  CheckSize(Int64(MaxInt), ifGray8, MaxInt, 1);
  CheckSize(Int64(MaxInt), ifGray8, 1, MaxInt);
  CheckSize(Int64(MaxInt) * 16, ifA32R32G32B32F, MaxInt, 1);
  // Rounding of width up to whole bytes must not overflow
  CheckSize(Int64(268435456), ifBinary, MaxInt, 1);
  CheckSize(Int64(268435456) * 3, ifBinary, MaxInt - 6, 3);
  CheckSize(Int64(MaxInt), ifBinary, 8, MaxInt);
  // The largest dimensions which are multiples of block size
  CheckSize(Int64(MaxInt - 3) * 4, ifDXT5, MaxInt - 3, 4);
  CheckSize(Int64(MaxInt - 3) * 2, ifDXT1, 4, MaxInt - 3);
end;

procedure TTestPixelsSize.TestTestImageOver32Bits;
var
  Image: TImageData;
begin
  // TestImage does not touch the bits so no memory needs to be allocated
  InitImage(Image);
  Image.Width := 40000;
  Image.Height := 40000;
  Image.Format := ifA8R8G8B8;
  Image.Size := Int64(6400000000);
  CheckTrue(TestImage(Image), 'Size over 32 bits is valid');

  // The same size truncated to 32 bits
  Image.Size := Int64(6400000000) and $FFFFFFFF;
  CheckFalse(TestImage(Image), 'Size truncated to 32 bits is not valid');
end;

{ TTestAlphaChecks }

procedure TTestAlphaChecks.Test16Bit;
var
  Pixels: array[0..99] of Word;
  I: Integer;
begin
  for I := 0 to High(Pixels) do
    Pixels[I] := (I * 331) and $7FFF;
  Pixels[3] := $7FFF;

  CheckFalse(Has16BitImageAlpha(Length(Pixels), @Pixels), 'No pixel has the alpha bit');
  CheckFalse(Has16BitImageAlpha(0, @Pixels), 'No pixels');

  Pixels[High(Pixels)] := $8000;
  CheckTrue(Has16BitImageAlpha(Length(Pixels), @Pixels), 'The last pixel has the alpha bit');
  CheckFalse(Has16BitImageAlpha(Length(Pixels) - 1, @Pixels), 'Pixel after the end is not read');
  CheckTrue(Has16BitImageAlpha(1, @Pixels[High(Pixels)]), 'One pixel');

  Pixels[0] := $FFFF;
  CheckTrue(Has16BitImageAlpha(1, @Pixels), 'The first pixel has the alpha bit');
end;

procedure TTestAlphaChecks.Test32Bit;
var
  Pixels: array[0..99] of UInt32;
  I: Integer;
begin
  for I := 0 to High(Pixels) do
    Pixels[I] := (I * 167531) and $00FFFFFF;
  Pixels[3] := $00FFFFFF;

  CheckFalse(Has32BitImageAlpha(Length(Pixels), @Pixels), 'No pixel has alpha');
  CheckFalse(Has32BitImageAlpha(0, @Pixels), 'No pixels');

  Pixels[High(Pixels)] := $01000000;
  CheckTrue(Has32BitImageAlpha(Length(Pixels), @Pixels), 'The last pixel has alpha');
  CheckFalse(Has32BitImageAlpha(Length(Pixels) - 1, @Pixels), 'Pixel after the end is not read');
  CheckTrue(Has32BitImageAlpha(1, @Pixels[High(Pixels)]), 'One pixel');

  Pixels[0] := $FF000000;
  CheckTrue(Has32BitImageAlpha(1, @Pixels), 'The first pixel has alpha');
end;

{ TTestHalfFloat }

const
  HalfMin:     Single = 5.96046448e-08; // Smallest positive half
  HalfMinNorm: Single = 6.10351562e-05; // Smallest positive normalized half
  HalfMax:     Single = 65504.0;        // Largest positive half
  HalfEpsilon: Single = 0.00097656;     // Smallest positive e for which half (1.0 + e) != half (1.0)

procedure TTestHalfFloat.CheckNormalized(F: Single);
var
  RelError: Single;
begin
  RelError := Abs(1.0 - HalfToFloat(FloatToHalf(F)) / F);
  CheckTrue(RelError <= HalfEpsilon * 0.5,
    Format('Normalized %g: relative error %g', [F, RelError]));
end;

procedure TTestHalfFloat.CheckDenormalized(F: Single);
var
  AbsError: Single;
begin
  AbsError := Abs(HalfToFloat(FloatToHalf(F)) - F);
  CheckTrue(AbsError <= HalfMin * 0.5,
    Format('Denormalized %g: absolute error %g', [F, AbsError]));
end;

procedure TTestHalfFloat.TestRoundTrip;
const
  Values: array[0..7] of Single = (0.1, 0.5, 1.0, 2.0, 3.599, 17.0, 1000.25, 65504.0);
var
  I: Integer;
begin
  for I := Low(Values) to High(Values) do
  begin
    CheckNormalized(Values[I]);
    CheckNormalized(-Values[I]);
  end;

  CheckNormalized(HalfMax);
  CheckNormalized(-HalfMax);
  CheckNormalized(HalfMinNorm);
  CheckNormalized(-HalfMinNorm);

  CheckDenormalized(HalfMin);
  CheckDenormalized(-HalfMin);
  CheckDenormalized(HalfMin * 100);
  CheckDenormalized(0.0);

  // Values that halves store exactly
  CheckEquals(1.0, HalfToFloat(FloatToHalf(1.0)), 0.0, 'One');
  CheckEquals(-2.0, HalfToFloat(FloatToHalf(-2.0)), 0.0, 'Minus two');
  CheckEquals(0.5, HalfToFloat(FloatToHalf(0.5)), 0.0, 'Half');
  CheckEquals(1024.0, HalfToFloat(FloatToHalf(1024.0)), 0.0, '1024');
end;

procedure TTestHalfFloat.TestSpecialValues;
const
  HalfZero   = $0000;
  HalfOne    = $3C00;
  HalfNegTwo = $C000;
  HalfPosInf = $7C00;
  HalfNegInf = $FC00;
begin
  CheckEquals(HalfZero, FloatToHalf(0.0), 'Bits of zero');
  CheckEquals(HalfOne, FloatToHalf(1.0), 'Bits of one');
  CheckEquals(HalfNegTwo, FloatToHalf(-2.0), 'Bits of minus two');
  CheckEquals(HalfPosInf, FloatToHalf(Infinity), 'Bits of infinity');
  CheckEquals(HalfNegInf, FloatToHalf(NegInfinity), 'Bits of negative infinity');

  CheckTrue(HalfToFloat(HalfPosInf) = Infinity, 'Infinity from bits');
  CheckTrue(HalfToFloat(HalfNegInf) = NegInfinity, 'Negative infinity from bits');
  CheckTrue(IsNan(HalfToFloat($7E00)), 'NaN from bits');

  // Values out of the range of halves
  CheckTrue(HalfToFloat(FloatToHalf(1e10)) = Infinity, 'Too large becomes infinity');
  CheckTrue(HalfToFloat(FloatToHalf(-1e10)) = NegInfinity, 'Too small becomes negative infinity');
  CheckEquals(0.0, HalfToFloat(FloatToHalf(HalfMin / 10.0)), 0.0, 'Too close to zero becomes zero');
end;

initialization
  RegisterImagingTest(TTestFormatInfos);
  RegisterImagingTest(TTestPixelsSize);
  RegisterImagingTest(TTestAlphaChecks);
  RegisterImagingTest(TTestHalfFloat);
end.
