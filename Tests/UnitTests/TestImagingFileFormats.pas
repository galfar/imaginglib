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

{ Tests of loaders and savers of file formats: values of samples they produce
  from small files built in memory.}
unit TestImagingFileFormats;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, ImagingTypes, Imaging, ImagingUtility, ImagingTestCase,
  ImagingTestPixels;

type
  TImagingFileFormatTestCase = class(TImagingTestCase)
  protected
    FImage: TImageData;
    procedure SetUp; override;
    procedure TearDown; override;
    function LoadFromData(const Data: AnsiString): Boolean;
    procedure CheckLoaded(const Data: AnsiString; Width, Height: Integer;
      Format: TImageFormat; const Msg: string);
    { Saves test image in the given data format to file format given by Ext,
      loads it to FImage and compares channels of all pixels. Only alpha is
      compared when ColorTolerance is negative.}
    procedure CheckRoundTrip(const Ext: string; Format: TImageFormat;
      Width, Height: Integer; const ColorTolerance: Double);
  end;

  { Portable maps (PGM, PPM, PAM) with samples scaled by max value from
    the file header.}
  TTestPortableMaps = class(TImagingFileFormatTestCase)
  private
    procedure CheckGray16(const Expected: array of Word; const Msg: string);
    procedure CheckGray8(const Expected: array of Byte; const Msg: string);
  published
    procedure TestTextMaxValTo16Bits;
    procedure TestBinaryMaxValTo16Bits;
    procedure TestTextMaxValTo8Bits;
    procedure TestBinaryRGBMaxValTo8Bits;
    procedure TestPAMMaxValTo16Bits;
    procedure TestFullRangeIsNotScaled;
    procedure TestMaxVal256IsNotScaled;
    procedure TestInvalidMaxValIsRefused;
    procedure TestBinaryBitmap;
    procedure TestPAMBlackAndWhite;
    procedure TestSavedBinaryFiles;
    procedure TestSavedTextFiles;
  end;

  { Formats where all pixels of the image are processed after loading or
    before saving.}
  TTestPixelLoops = class(TImagingFileFormatTestCase)
  published
    procedure TestJNGAlpha;
    procedure TestXPM;
  end;

  { Targa: raw and RLE compressed images.}
  TTestTarga = class(TImagingFileFormatTestCase)
  private
    procedure CheckBothModes(Format: TImageFormat; Width, Height: Integer);
  published
    procedure TestRoundTrip;
    procedure TestRunsOfPixels;
  end;

  { Radiance HDR: RGBE pixels to floating point values and back.}
  TTestRadiance = class(TImagingFileFormatTestCase)
  published
    procedure TestDecodedValues;
    procedure TestSavedValuesAreKept;
    procedure TestResaveIsStable;
  end;

implementation

const
  LF = #10;

function BE16(Value: Word): AnsiString;
begin
  Result := AnsiChar(Value shr 8) + AnsiChar(Value and $FF);
end;

function Rgbe(R, G, B, E: Byte): AnsiString;
begin
  Result := AnsiChar(R) + AnsiChar(G) + AnsiChar(B) + AnsiChar(E);
end;

{ TImagingFileFormatTestCase }

procedure TImagingFileFormatTestCase.SetUp;
begin
  InitImage(FImage);
end;

procedure TImagingFileFormatTestCase.TearDown;
begin
  FreeImage(FImage);
end;

function TImagingFileFormatTestCase.LoadFromData(const Data: AnsiString): Boolean;
begin
  Result := LoadImageFromMemory(@Data[1], Length(Data), FImage);
end;

procedure TImagingFileFormatTestCase.CheckLoaded(const Data: AnsiString;
  Width, Height: Integer; Format: TImageFormat; const Msg: string);
begin
  CheckTrue(LoadFromData(Data), Msg + ': image is loaded');
  CheckEquals(Width, FImage.Width, Msg + ': width');
  CheckEquals(Height, FImage.Height, Msg + ': height');
  CheckEquals(GetFormatName(Format), GetFormatName(FImage.Format), Msg + ': format');
end;

procedure TImagingFileFormatTestCase.CheckRoundTrip(const Ext: string;
  Format: TImageFormat; Width, Height: Integer; const ColorTolerance: Double);
var
  Original: TImageData;
  Stream: TMemoryStream;
  I: Integer;
  Ch: TPixelChannel;
  Expected, Actual: TIdealPixel;
  Tolerance: Double;
  Msg: string;
begin
  Msg := GetFormatName(Format) + ' as ' + Ext;
  InitImage(Original);
  Stream := TMemoryStream.Create;
  try
    CheckTrue(NewImage(Width, Height, Format, Original), Msg + ': NewImage');
    FillTestImage(Original, 987654321);

    CheckTrue(SaveImageToStream(Ext, Stream, Original), Msg + ': image is saved');
    Stream.Position := 0;
    CheckTrue(LoadImageFromStream(Stream, FImage), Msg + ': image is loaded');
    CheckEquals(Width, FImage.Width, Msg + ': width');
    CheckEquals(Height, FImage.Height, Msg + ': height');

    for I := 0 to Width * Height - 1 do
    begin
      Expected := ReadIdealPixel(Original, I);
      Actual := ReadIdealPixel(FImage, I);

      for Ch := Low(TPixelChannel) to High(TPixelChannel) do
      begin
        Tolerance := 1e-9;
        if Ch <> chAlpha then
        begin
          if ColorTolerance < 0 then
            Continue;
          Tolerance := Tolerance + ColorTolerance;
        end;

        CheckEquals(Expected.Channels[Ch], Actual.Channels[Ch], Tolerance,
                    SysUtils.Format('%s: pixel %d %s', [Msg, I, ChannelNames[Ch]]));
      end;
    end;
  finally
    Stream.Free;
    FreeImage(Original);
  end;
end;

{ TTestPortableMaps }

procedure TTestPortableMaps.CheckGray16(const Expected: array of Word; const Msg: string);
var
  I: Integer;
begin
  for I := 0 to High(Expected) do
    CheckEquals(Expected[I], PWordArray(FImage.Bits)[I], Format('%s: pixel %d', [Msg, I]));
end;

procedure TTestPortableMaps.CheckGray8(const Expected: array of Byte; const Msg: string);
var
  I: Integer;
begin
  for I := 0 to High(Expected) do
    CheckEquals(Expected[I], PBuffer(FImage.Bits)[I], Format('%s: pixel %d', [Msg, I]));
end;

procedure TTestPortableMaps.TestTextMaxValTo16Bits;
begin
  // Samples above 32768 overflowed 32-bit multiplication here
  CheckLoaded('P2' + LF + '5 1' + LF + '40000' + LF + '0 20000 32769 35000 40000' + LF,
              5, 1, ifGray16, 'PGM 40000');
  CheckGray16([0, 32768, 53688, 57343, 65535], 'PGM 40000');

  CheckLoaded('P2' + LF + '4 1' + LF + '4095' + LF + '0 1 2048 4095' + LF,
              4, 1, ifGray16, 'PGM 4095');
  CheckGray16([0, 16, 32776, 65535], 'PGM 4095');
end;

procedure TTestPortableMaps.TestBinaryMaxValTo16Bits;
begin
  // Unix line breaks: samples are big endian
  CheckLoaded('P5' + LF + '4 1' + LF + '40000' + LF +
              BE16(20000) + BE16(35000) + BE16(40000) + BE16(0),
              4, 1, ifGray16, 'PGM 40000');
  CheckGray16([32768, 57343, 65535, 0], 'PGM 40000');
end;

procedure TTestPortableMaps.TestTextMaxValTo8Bits;
begin
  CheckLoaded('P2' + LF + '5 1' + LF + '100' + LF + '0 1 50 99 100' + LF,
              5, 1, ifGray8, 'PGM 100');
  // Rounded to the nearest: 2.55, 127.5, 252.45
  CheckGray8([0, 3, 128, 252, 255], 'PGM 100');

  CheckLoaded('P2' + LF + '3 1' + LF + '15' + LF + '0 7 15' + LF,
              3, 1, ifGray8, 'PGM 15');
  CheckGray8([0, 119, 255], 'PGM 15');
end;

procedure TTestPortableMaps.TestBinaryRGBMaxValTo8Bits;
var
  Pixel: TIdealPixel;
begin
  CheckLoaded('P6' + LF + '2 1' + LF + '15' + LF + #15#5#0 + #1#0#15,
              2, 1, ifR8G8B8, 'PPM 15');

  Pixel := ReadIdealPixel(FImage, 0);
  CheckEquals(1.0, Pixel.R, 0, 'Pixel 0 red');
  CheckEquals(85 / 255, Pixel.G, 1e-9, 'Pixel 0 green');
  CheckEquals(0.0, Pixel.B, 0, 'Pixel 0 blue');

  Pixel := ReadIdealPixel(FImage, 1);
  CheckEquals(17 / 255, Pixel.R, 1e-9, 'Pixel 1 red');
  CheckEquals(0.0, Pixel.G, 0, 'Pixel 1 green');
  CheckEquals(1.0, Pixel.B, 0, 'Pixel 1 blue');
end;

procedure TTestPortableMaps.TestPAMMaxValTo16Bits;
begin
  CheckLoaded('P7' + LF + 'WIDTH 3' + LF + 'HEIGHT 1' + LF + 'DEPTH 1' + LF +
              'MAXVAL 1023' + LF + 'TUPLTYPE GRAYSCALE' + LF + 'ENDHDR' + LF +
              BE16(1023) + BE16(512) + BE16(0),
              3, 1, ifGray16, 'PAM 1023');
  CheckGray16([65535, 32800, 0], 'PAM 1023');
end;

procedure TTestPortableMaps.TestFullRangeIsNotScaled;
begin
  CheckLoaded('P2' + LF + '3 1' + LF + '255' + LF + '0 100 255' + LF,
              3, 1, ifGray8, 'PGM 255');
  CheckGray8([0, 100, 255], 'PGM 255');

  CheckLoaded('P2' + LF + '3 1' + LF + '65535' + LF + '0 1000 65535' + LF,
              3, 1, ifGray16, 'PGM 65535');
  CheckGray16([0, 1000, 65535], 'PGM 65535');
end;

procedure TTestPortableMaps.TestMaxVal256IsNotScaled;
begin
  // Written by programs that take max value as the number of values
  CheckLoaded('P2' + LF + '4 1' + LF + '256' + LF + '0 1 128 255' + LF,
              4, 1, ifGray8, 'PGM 256');
  CheckGray8([0, 1, 128, 255], 'PGM 256');
end;

procedure TTestPortableMaps.TestInvalidMaxValIsRefused;
begin
  CheckFalse(LoadFromData('P2' + LF + '2 1' + LF + '0' + LF + '0 0' + LF), 'Max value 0');
  CheckFalse(LoadFromData('P2' + LF + '2 1' + LF + '70000' + LF + '0 70000' + LF), 'Max value 70000');
end;

procedure TTestPortableMaps.TestBinaryBitmap;
begin
  // One bit per pixel, rows are padded to whole bytes, set bit is black
  CheckLoaded('P4' + LF + '10 2' + LF + #$A5#$40 + #$00#$C0, 10, 2, ifGray8, 'PBM');
  CheckGray8([0, 255, 0, 255, 255, 0, 255, 0, 255, 0,
              255, 255, 255, 255, 255, 255, 255, 255, 0, 0], 'PBM');
end;

procedure TTestPortableMaps.TestPAMBlackAndWhite;
begin
  // Unlike in PBM set sample is white
  CheckLoaded('P7' + LF + 'WIDTH 4' + LF + 'HEIGHT 1' + LF + 'DEPTH 1' + LF +
              'MAXVAL 1' + LF + 'TUPLTYPE BLACKANDWHITE' + LF + 'ENDHDR' + LF + #1#0#0#1,
              4, 1, ifGray8, 'PAM');
  CheckGray8([255, 0, 0, 255], 'PAM');
end;

procedure TTestPortableMaps.TestSavedBinaryFiles;
begin
  CheckRoundTrip('pgm', ifGray8, 13, 7, 0);
  CheckRoundTrip('pgm', ifGray16, 13, 7, 0);
  CheckRoundTrip('ppm', ifR8G8B8, 13, 7, 0);
  CheckRoundTrip('ppm', ifR16G16B16, 13, 7, 0);
  CheckRoundTrip('pam', ifA8Gray8, 13, 7, 0);
  CheckRoundTrip('pam', ifA16Gray16, 13, 7, 0);
  CheckRoundTrip('pam', ifA8R8G8B8, 13, 7, 0);
  CheckRoundTrip('pam', ifA16R16G16B16, 13, 7, 0);
end;

procedure TTestPortableMaps.TestSavedTextFiles;
var
  OldPgm, OldPpm: Integer;
begin
  OldPgm := GetOption(ImagingPGMSaveBinary);
  OldPpm := GetOption(ImagingPPMSaveBinary);
  try
    SetOption(ImagingPGMSaveBinary, 0);
    SetOption(ImagingPPMSaveBinary, 0);

    CheckRoundTrip('pgm', ifGray8, 13, 7, 0);
    CheckRoundTrip('pgm', ifGray16, 13, 7, 0);
    CheckRoundTrip('ppm', ifR8G8B8, 13, 7, 0);
    CheckRoundTrip('ppm', ifR16G16B16, 13, 7, 0);
  finally
    SetOption(ImagingPGMSaveBinary, OldPgm);
    SetOption(ImagingPPMSaveBinary, OldPpm);
  end;
end;

{ TTestPixelLoops }

procedure TTestPixelLoops.TestJNGAlpha;
begin
  // Color is stored as JPEG, alpha is lossless by default and is put back
  // to pixels after loading
  CheckRoundTrip('jng', ifA8R8G8B8, 24, 17, -1);
  CheckEquals(GetFormatName(ifA8R8G8B8), GetFormatName(FImage.Format), 'Format of color image');
  CheckRoundTrip('jng', ifA8Gray8, 24, 17, -1);
  CheckEquals(GetFormatName(ifA8Gray8), GetFormatName(FImage.Format), 'Format of grayscale image');
end;

procedure TTestPixelLoops.TestXPM;
begin
  CheckRoundTrip('xpm', ifR8G8B8, 12, 9, 0);
  CheckRoundTrip('xpm', ifX8R8G8B8, 40, 30, 0);
end;

{ TTestRadiance }

const
  HdrHeader = '#?RADIANCE' + LF + 'FORMAT=32-bit_rle_rgbe' + LF + LF;

  // Nine values so that FillFloatImage gives each pixel another combination
  HdrValues: array[0..8] of Single =
    (0.0, 0.001, 0.3, 0.5, 1.0, 7.7, 1000.0, 0.999, 65000.0);

procedure TTestRadiance.TestDecodedValues;
const
  // Mantissas are fractions of 256: value = mantissa / 256 * 2^(exponent - 128)
  Expected: array[0..4, 0..2] of Single = (
    (1.0, 1.0, 1.0),
    (0.5, 0.25, 0.0),
    (255.0, 1.0, 0.0),
    (0.0, 0.0, 0.0),
    (0.498046875, 0.00390625, 0.25));
var
  I: Integer;
  Pixel: TIdealPixel;
  Msg: string;
begin
  // Lines shorter than 8 pixels are not compressed
  CheckLoaded(HdrHeader + '-Y 1 +X 5' + LF +
              Rgbe(128, 128, 128, 129) +
              Rgbe(128, 64, 0, 128) +
              Rgbe(255, 1, 0, 136) +
              Rgbe(200, 100, 50, 0) +
              Rgbe(255, 2, 128, 127),
              5, 1, ifR32G32B32F, 'HDR');

  for I := 0 to High(Expected) do
  begin
    Pixel := ReadIdealPixel(FImage, I);
    Msg := Format('Pixel %d ', [I]);
    CheckEquals(Expected[I, 0], Pixel.R, 0, Msg + 'red');
    CheckEquals(Expected[I, 1], Pixel.G, 0, Msg + 'green');
    CheckEquals(Expected[I, 2], Pixel.B, 0, Msg + 'blue');
  end;
end;

procedure TTestRadiance.TestSavedValuesAreKept;
const
  Width = 16;
  Height = 3;
var
  Original: TImageData;
  Stream: TMemoryStream;
  I: Integer;
  Ch: TPixelChannel;
  Expected, Actual: TIdealPixel;
  Tolerance: Double;
begin
  InitImage(Original);
  Stream := TMemoryStream.Create;
  try
    NewImage(Width, Height, ifR32G32B32F, Original);
    FillFloatImage(Original, HdrValues);
    // Black pixel
    FillChar(Original.Bits^, SizeOf(TColor96FPRec), 0);

    CheckTrue(SaveImageToStream('hdr', Stream, Original), 'Image is saved');
    Stream.Position := 0;
    CheckTrue(LoadImageFromStream(Stream, FImage), 'Image is loaded');
    CheckEquals(Width, FImage.Width, 'Width');
    CheckEquals(Height, FImage.Height, 'Height');
    CheckEquals(GetFormatName(ifR32G32B32F), GetFormatName(FImage.Format), 'Format');

    for I := 0 to Width * Height - 1 do
    begin
      Expected := ReadIdealPixel(Original, I);
      Actual := ReadIdealPixel(FImage, I);
      // Channels share the exponent of the largest one and have 8 bits
      // of mantissa each
      Tolerance := Expected.R;
      if Expected.G > Tolerance then
        Tolerance := Expected.G;
      if Expected.B > Tolerance then
        Tolerance := Expected.B;
      Tolerance := Tolerance / 256;

      for Ch := chRed to chBlue do
      begin
        CheckEquals(Expected.Channels[Ch], Actual.Channels[Ch], Tolerance,
                    Format('Pixel %d %s', [I, ChannelNames[Ch]]));
      end;
    end;
  finally
    Stream.Free;
    FreeImage(Original);
  end;
end;

procedure TTestRadiance.TestResaveIsStable;
var
  Original: TImageData;
  First, Second: TMemoryStream;
begin
  // Image that was loaded from a file must give the same file when saved:
  // decoding and encoding use the same scale of mantissas
  InitImage(Original);
  First := TMemoryStream.Create;
  Second := TMemoryStream.Create;
  try
    NewImage(16, 3, ifR32G32B32F, Original);
    FillFloatImage(Original, HdrValues);

    CheckTrue(SaveImageToStream('hdr', First, Original), 'Image is saved');
    First.Position := 0;
    CheckTrue(LoadImageFromStream(First, FImage), 'Image is loaded');
    CheckTrue(SaveImageToStream('hdr', Second, FImage), 'Image is saved again');

    CheckEquals(First.Size, Second.Size, 'Size of files');
    CheckSameMem(First.Memory, Second.Memory, First.Size, 'Content of files');
  finally
    Second.Free;
    First.Free;
    FreeImage(Original);
  end;
end;

{ TTestTarga }

procedure TTestTarga.CheckBothModes(Format: TImageFormat; Width, Height: Integer);
var
  OldRle: Integer;
begin
  OldRle := GetOption(ImagingTargaRLE);
  try
    SetOption(ImagingTargaRLE, 0);
    CheckRoundTrip('tga', Format, Width, Height, 0);
    SetOption(ImagingTargaRLE, 1);
    CheckRoundTrip('tga', Format, Width, Height, 0);
  finally
    SetOption(ImagingTargaRLE, OldRle);
  end;
end;

procedure TTestTarga.TestRoundTrip;
begin
  // One to four bytes per pixel
  CheckBothModes(ifGray8, 13, 7);
  CheckBothModes(ifA1R5G5B5, 13, 7);
  CheckBothModes(ifR8G8B8, 13, 7);
  CheckBothModes(ifA8R8G8B8, 13, 7);
end;

procedure TTestTarga.TestRunsOfPixels;
const
  Width = 300;
  Height = 5;
var
  Original: TImageData;
  Stream: TMemoryStream;
  OldRle, X, Y: Integer;
begin
  // Rows with runs longer than one RLE packet (128 pixels) and single pixels between
  InitImage(Original);
  Stream := TMemoryStream.Create;
  OldRle := GetOption(ImagingTargaRLE);
  try
    CheckTrue(NewImage(Width, Height, ifR8G8B8, Original), 'NewImage');
    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
        with PColor24RecArray(Original.Bits)[Y * Width + X] do
        begin
          R := Iff(X mod 97 = 0, X and $FF, 10);
          G := Y * 40;
          B := Iff(X < 200, 0, 255);
        end;

    SetOption(ImagingTargaRLE, 1);
    CheckTrue(SaveImageToStream('tga', Stream, Original), 'Image is saved');
    CheckTrue(Stream.Size < Original.Size div 4, Format('RLE compressed: %d bytes', [Stream.Size]));
    Stream.Position := 0;
    CheckTrue(LoadImageFromStream(Stream, FImage), 'Image is loaded');
    CheckImagesEqual(Original, FImage, 'Loaded image');
  finally
    SetOption(ImagingTargaRLE, OldRle);
    Stream.Free;
    FreeImage(Original);
  end;
end;

initialization
  RegisterImagingTest(TTestPortableMaps);
  RegisterImagingTest(TTestPixelLoops);
  RegisterImagingTest(TTestTarga);
  RegisterImagingTest(TTestRadiance);
end.
