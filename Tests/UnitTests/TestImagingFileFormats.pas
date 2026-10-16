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
  SysUtils, Classes, ImagingTypes, Imaging, ImagingUtility, ImagingZLib,
  ImagingTestCase, ImagingTestPixels;

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
    procedure SaveRle(const Image: TImageData; Stream: TStream);
  published
    procedure TestRoundTrip;
    procedure TestRunsOfPixels;
    procedure TestDataLargerThanReadBlock;
    procedure TestPositionAfterImage;
    procedure TestTruncatedData;
    procedure TestPacketsPastImageEnd;
    procedure TestDimensionsOver32767;
  end;

  { PNG: data formats, filters, interlacing, image data in more chunks,
    damaged and truncated files.}
  TTestPng = class(TImagingFileFormatTestCase)
  private
    procedure NewGray8Pattern(var Image: TImageData; Width, Height: Integer);
    function RawRows(const Image: TImageData): AnsiString;
    function MakePng(const IHDR, Compressed: AnsiString; IDATSize: Integer;
      BadCrcIDAT: Integer = -1; const ExtraChunks: AnsiString = ''): AnsiString;
    procedure CheckFirstRowsAndBlackEnd(const Original: TImageData; GoodRows: Integer;
      const Msg: string);
  published
    procedure TestRoundTrip;
    procedure TestPreFilters;
    procedure TestInterlaced;
    procedure TestManyDataChunks;
    procedure TestTruncatedData;
    procedure TestBadDataChunkCrc;
    procedure TestBadAncillaryChunkCrc;
    procedure TestLargeDataInMoreChunks;
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

function LE16(Value: Word): AnsiString;
begin
  Result := AnsiChar(Value and $FF) + AnsiChar(Value shr 8);
end;

function BE32(Value: UInt32): AnsiString;
begin
  Result := AnsiChar(Value shr 24) + AnsiChar((Value shr 16) and $FF) +
    AnsiChar((Value shr 8) and $FF) + AnsiChar(Value and $FF);
end;

const
  PngSignature: AnsiString = #137'PNG'#13#10#26#10;

function PngChunk(const ChunkType, Data: AnsiString; BadCrc: Boolean = False): AnsiString;
var
  Crc: UInt32;
begin
  Crc := $FFFFFFFF;
  CalcCrc32(Crc, @ChunkType[1], 4);
  CalcCrc32(Crc, PAnsiChar(Data), Length(Data));
  Crc := not Crc;
  if BadCrc then
    Crc := Crc xor 1;
  Result := BE32(Length(Data)) + ChunkType + Data + BE32(Crc);
end;

function PngIHDR(Width, Height: Integer; BitDepth, ColorType, Interlace: Byte): AnsiString;
begin
  Result := PngChunk('IHDR', BE32(Width) + BE32(Height) + AnsiChar(BitDepth) +
    AnsiChar(ColorType) + AnsiChar(0) + AnsiChar(0) + AnsiChar(Interlace));
end;

function ZLibCompress(const Data: AnsiString; Level: Integer): AnsiString;
var
  Buffer: Pointer;
  Size: NativeInt;
begin
  CompressBuf(PAnsiChar(Data), Length(Data), Buffer, Size, Level);
  try
    SetLength(Result, Size);
    Move(Buffer^, Result[1], Size);
  finally
    FreeMem(Buffer);
  end;
end;

function TgaHeader(ImageType: Byte; Width, Height: Word; PixelSize: Byte): AnsiString;
begin
  // No image ID and no color map, rows stored top-down
  Result := AnsiChar(0) + AnsiChar(0) + AnsiChar(ImageType) + AnsiString(#0#0#0#0#0) +
    LE16(0) + LE16(0) + LE16(Width) + LE16(Height) + AnsiChar(PixelSize) + AnsiChar($20);
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

procedure TTestTarga.SaveRle(const Image: TImageData; Stream: TStream);
var
  OldRle: Integer;
begin
  OldRle := GetOption(ImagingTargaRLE);
  try
    SetOption(ImagingTargaRLE, 1);
    CheckTrue(SaveImageToStream('tga', Stream, Image), 'Image is saved');
  finally
    SetOption(ImagingTargaRLE, OldRle);
  end;
end;

procedure TTestTarga.TestDataLargerThanReadBlock;
const
  Formats: array[0..2] of TImageFormat = (ifGray8, ifR8G8B8, ifA8R8G8B8);
var
  Original: TImageData;
  Stream: TMemoryStream;
  F: Integer;
  Name: string;
begin
  // Compressed noise is larger than the 1 MB blocks the data are read and
  // written in, packets are split between the blocks
  InitImage(Original);
  Stream := TMemoryStream.Create;
  try
    for F := Low(Formats) to High(Formats) do
    begin
      Name := GetFormatName(Formats[F]);
      CheckTrue(NewImage(1000, 1100, Formats[F], Original), Name + ': NewImage');
      FillTestImage(Original, 4321);

      Stream.Clear;
      SaveRle(Original, Stream);
      CheckTrue(Stream.Size > 1024 * 1024, Format('%s: compressed size %d bytes', [Name, Stream.Size]));
      Stream.Position := 0;
      CheckTrue(LoadImageFromStream(Stream, FImage), Name + ': image is loaded');
      CheckImagesEqual(Original, FImage, Name + ': loaded image');
    end;
  finally
    Stream.Free;
    FreeImage(Original);
  end;
end;

procedure TTestTarga.TestPositionAfterImage;
var
  First, Second: TImageData;
  Stream: TMemoryStream;
begin
  // Input is read ahead in blocks, the position must be set back to the end
  // of the image so that the next one can be read
  InitImage(First);
  InitImage(Second);
  Stream := TMemoryStream.Create;
  try
    CheckTrue(NewImage(70, 50, ifR8G8B8, First), 'NewImage');
    FillTestImage(First, 11);
    CheckTrue(NewImage(30, 20, ifR8G8B8, Second), 'NewImage');
    FillTestImage(Second, 12);
    SaveRle(First, Stream);
    SaveRle(Second, Stream);

    Stream.Position := 0;
    CheckTrue(LoadImageFromStream(Stream, FImage), 'The first image is loaded');
    CheckImagesEqual(First, FImage, 'The first image');
    CheckTrue(LoadImageFromStream(Stream, FImage), 'The second image is loaded');
    CheckImagesEqual(Second, FImage, 'The second image');
  finally
    Stream.Free;
    FreeImage(First);
    FreeImage(Second);
  end;
end;

procedure TTestTarga.TestTruncatedData;
const
  Width = 64;
  Height = 40;
var
  Original: TImageData;
  Stream: TMemoryStream;
  RowBytes, I: Integer;
begin
  // Pixels after the end of the data are left black
  InitImage(Original);
  Stream := TMemoryStream.Create;
  try
    CheckTrue(NewImage(Width, Height, ifR8G8B8, Original), 'NewImage');
    FillTestImage(Original, 99);
    SaveRle(Original, Stream);

    // Cut in the middle of the data, most likely inside of a packet
    Stream.Size := Stream.Size div 2 + 1;
    Stream.Position := 0;
    CheckTrue(LoadImageFromStream(Stream, FImage), 'Truncated image is loaded');
    CheckEquals(Width, FImage.Width, 'Width');
    CheckEquals(Height, FImage.Height, 'Height');
    CheckEquals(Ord(ifR8G8B8), Ord(FImage.Format), 'Format');

    RowBytes := Width * 3;
    CheckSameMem(Original.Bits, FImage.Bits, RowBytes * (Height div 4), 'The first quarter of rows');
    for I := RowBytes * (Height - 1) to FImage.Size - 1 do
      CheckEquals(0, PBuffer(FImage.Bits)[I], Format('The last row, byte %d', [I]));
  finally
    Stream.Free;
    FreeImage(Original);
  end;
end;

procedure TTestTarga.TestPacketsPastImageEnd;
var
  Raw: AnsiString;
  I: Integer;
begin
  // Packets of 128 pixels in a 4x2 image: only 8 pixels are written
  CheckLoaded(TgaHeader(11, 4, 2, 8) + AnsiChar($FF) + AnsiChar(200), 4, 2, ifGray8, 'Run');
  for I := 0 to 7 do
    CheckEquals(200, PBuffer(FImage.Bits)[I], Format('Run, pixel %d', [I]));

  Raw := '';
  for I := 0 to 127 do
    Raw := Raw + AnsiChar(I + 10);
  CheckLoaded(TgaHeader(11, 4, 2, 8) + AnsiChar($7F) + Raw, 4, 2, ifGray8, 'Raw pixels');
  for I := 0 to 7 do
    CheckEquals(I + 10, PBuffer(FImage.Bits)[I], Format('Raw pixels, pixel %d', [I]));
end;

procedure TTestTarga.TestDimensionsOver32767;
const
  Width = 40000;
var
  Data: AnsiString;
  Row: Integer;
  Image: TImageData;
  Stream: TMemoryStream;
  Saved: Boolean;

  function RleRow(Value: Byte): AnsiString;
  var
    I: Integer;
  begin
    // 312 runs of 128 pixels and one of 64
    Result := '';
    for I := 1 to Width div 128 do
      Result := Result + AnsiChar($FF) + AnsiChar(Value);
    Result := Result + AnsiChar($80 or (Width mod 128 - 1)) + AnsiChar(Value);
  end;

begin
  // Width and height in the header are unsigned 16-bit numbers
  Data := TgaHeader(11, Width, 2, 8) + RleRow(50) + RleRow(150);
  CheckLoaded(Data, Width, 2, ifGray8, 'Loaded');
  for Row := 0 to 1 do
  begin
    CheckEquals(Iff(Row = 0, 50, 150), PBuffer(FImage.Bits)[Row * Width], Format('Row %d, the first pixel', [Row]));
    CheckEquals(Iff(Row = 0, 50, 150), PBuffer(FImage.Bits)[Row * Width + Width - 1], Format('Row %d, the last pixel', [Row]));
  end;

  // Saving and loading again, RLE and raw
  InitImage(Image);
  Stream := TMemoryStream.Create;
  try
    CheckTrue(CloneImage(FImage, Image), 'CloneImage');
    SaveRle(Image, Stream);
    CheckTrue(SaveImageToStream('tga', Stream, Image), 'Raw image is saved');
    Stream.Position := 0;
    CheckTrue(LoadImageFromStream(Stream, FImage), 'RLE image is loaded');
    CheckImagesEqual(Image, FImage, 'RLE image');
    CheckTrue(LoadImageFromStream(Stream, FImage), 'Raw image is loaded');
    CheckImagesEqual(Image, FImage, 'Raw image');
  finally
    Stream.Free;
    FreeImage(Image);
  end;

  // Larger images cannot be saved
  InitImage(Image);
  Stream := TMemoryStream.Create;
  try
    CheckTrue(NewImage(65536, 1, ifGray8, Image), 'NewImage');
    Saved := SaveImageToStream('tga', Stream, Image);
    CheckFalse(Saved, 'Image 65536 pixels wide is refused');
  finally
    Stream.Free;
    FreeImage(Image);
  end;
end;

{ TTestPng }

procedure TTestPng.NewGray8Pattern(var Image: TImageData; Width, Height: Integer);
var
  X, Y: Integer;
begin
  CheckTrue(NewImage(Width, Height, ifGray8, Image), 'NewImage');
  for Y := 0 to Height - 1 do
    for X := 0 to Width - 1 do
      PBuffer(Image.Bits)[Y * Width + X] := Byte(X * 7 + Y * 13 + 1);
end;

function TTestPng.RawRows(const Image: TImageData): AnsiString;
var
  Y, RowBytes: Integer;
begin
  // Rows of PNG data without filtering: filter byte 0 before each row
  RowBytes := Image.Size div Image.Height;
  Result := '';
  for Y := 0 to Image.Height - 1 do
  begin
    SetLength(Result, Length(Result) + RowBytes + 1);
    Result[Length(Result) - RowBytes] := #0;
    Move(PBuffer(Image.Bits)[Y * RowBytes], Result[Length(Result) - RowBytes + 1], RowBytes);
  end;
end;

function TTestPng.MakePng(const IHDR, Compressed: AnsiString; IDATSize: Integer;
  BadCrcIDAT: Integer; const ExtraChunks: AnsiString): AnsiString;
var
  Pos, Index: Integer;
begin
  // Compressed data are split into IDAT chunks of IDATSize bytes
  if IDATSize > Length(Compressed) then
    IDATSize := Length(Compressed);
  Result := PngSignature + IHDR + ExtraChunks;
  Pos := 1;
  Index := 0;
  while Pos <= Length(Compressed) do
  begin
    Result := Result + PngChunk('IDAT', Copy(Compressed, Pos, IDATSize), Index = BadCrcIDAT);
    Inc(Pos, IDATSize);
    Inc(Index);
  end;
  Result := Result + PngChunk('IEND', '');
end;

procedure TTestPng.CheckFirstRowsAndBlackEnd(const Original: TImageData; GoodRows: Integer;
  const Msg: string);
var
  RowBytes, I: Integer;
begin
  CheckEquals(Original.Width, FImage.Width, Msg + ': width');
  CheckEquals(Original.Height, FImage.Height, Msg + ': height');
  RowBytes := Original.Size div Original.Height;
  CheckSameMem(Original.Bits, FImage.Bits, GoodRows * RowBytes, Msg + ': the first rows');
  for I := (Original.Height - 1) * RowBytes to FImage.Size - 1 do
    CheckEquals(0, PBuffer(FImage.Bits)[I], Format('%s: the last row, byte %d', [Msg, I]));
end;

procedure TTestPng.TestRoundTrip;
const
  Formats: array[0..7] of TImageFormat = (ifGray8, ifA8Gray8, ifGray16, ifA16Gray16,
    ifR8G8B8, ifA8R8G8B8, ifR16G16B16, ifA16R16G16B16);
var
  F: Integer;
begin
  for F := Low(Formats) to High(Formats) do
  begin
    CheckRoundTrip('png', Formats[F], 13, 7, 0);
    CheckRoundTrip('png', Formats[F], 1, 1, 0);
  end;
end;

procedure TTestPng.TestPreFilters;
var
  OldFilter, Filter: Integer;
begin
  // 0..4 are filters, 5 is chosen by data format, 6 adaptive (best filter for each row)
  OldFilter := GetOption(ImagingPNGPreFilter);
  try
    for Filter := 0 to 6 do
    begin
      SetOption(ImagingPNGPreFilter, Filter);
      CheckRoundTrip('png', ifR8G8B8, 37, 23, 0);
      CheckRoundTrip('png', ifA16R16G16B16, 11, 9, 0);
    end;
  finally
    SetOption(ImagingPNGPreFilter, OldFilter);
  end;
end;

procedure TTestPng.TestInterlaced;
const
  RowStart: array[0..6] of Integer = (0, 0, 4, 0, 2, 0, 1);
  ColumnStart: array[0..6] of Integer = (0, 4, 0, 2, 0, 1, 0);
  RowIncrement: array[0..6] of Integer = (8, 8, 8, 4, 4, 2, 2);
  ColumnIncrement: array[0..6] of Integer = (8, 8, 4, 4, 2, 2, 1);
  Sizes: array[0..2, 0..1] of Integer = ((13, 11), (3, 2), (1, 9));
var
  Original: TImageData;
  Raw: AnsiString;
  S, Pass, X, Y: Integer;
  Msg: string;
begin
  // Adam7 data are made here pass by pass, without filtering
  InitImage(Original);
  try
    for S := Low(Sizes) to High(Sizes) do
    begin
      Msg := Format('%dx%d', [Sizes[S, 0], Sizes[S, 1]]);
      NewGray8Pattern(Original, Sizes[S, 0], Sizes[S, 1]);
      Raw := '';
      for Pass := 0 to 6 do
      begin
        // Passes without pixels have no rows at all
        if (Original.Width <= ColumnStart[Pass]) or (Original.Height <= RowStart[Pass]) then
          Continue;
        Y := RowStart[Pass];
        while Y < Original.Height do
        begin
          Raw := Raw + #0;
          X := ColumnStart[Pass];
          while X < Original.Width do
          begin
            Raw := Raw + AnsiChar(PBuffer(Original.Bits)[Y * Original.Width + X]);
            Inc(X, ColumnIncrement[Pass]);
          end;
          Inc(Y, RowIncrement[Pass]);
        end;
      end;

      CheckLoaded(MakePng(PngIHDR(Original.Width, Original.Height, 8, 0, 1), ZLibCompress(Raw, 6), MaxInt),
                  Original.Width, Original.Height, ifGray8, Msg);
      CheckImagesEqual(Original, FImage, Msg + ': loaded image');
    end;
  finally
    FreeImage(Original);
  end;
end;

procedure TTestPng.TestManyDataChunks;
var
  Original: TImageData;
  Compressed: AnsiString;
begin
  InitImage(Original);
  try
    NewGray8Pattern(Original, 30, 20);
    Compressed := ZLibCompress(RawRows(Original), 6);
    CheckLoaded(MakePng(PngIHDR(30, 20, 8, 0, 0), Compressed, 7), 30, 20, ifGray8, 'IDAT of 7 bytes');
    CheckImagesEqual(Original, FImage, 'IDAT of 7 bytes');
    CheckLoaded(MakePng(PngIHDR(30, 20, 8, 0, 0), Compressed, 1), 30, 20, ifGray8, 'IDAT of 1 byte');
    CheckImagesEqual(Original, FImage, 'IDAT of 1 byte');
  finally
    FreeImage(Original);
  end;
end;

procedure TTestPng.TestTruncatedData;
var
  Original: TImageData;
  Data: AnsiString;
begin
  // Stored (level 0) data: the first half of the file holds the first rows
  InitImage(Original);
  try
    NewGray8Pattern(Original, 64, 40);
    Data := MakePng(PngIHDR(64, 40, 8, 0, 0), ZLibCompress(RawRows(Original), 0), MaxInt);
    SetLength(Data, Length(Data) div 2);
    CheckLoaded(Data, 64, 40, ifGray8, 'Truncated file');
    CheckFirstRowsAndBlackEnd(Original, 10, 'Truncated file');
  finally
    FreeImage(Original);
  end;
end;

procedure TTestPng.TestBadDataChunkCrc;
var
  Original: TImageData;
  Compressed: AnsiString;
  Half: Integer;
begin
  // Data of the chunk with bad CRC and the following ones are not used
  InitImage(Original);
  try
    NewGray8Pattern(Original, 64, 40);
    Compressed := ZLibCompress(RawRows(Original), 0);
    Half := Length(Compressed) div 2 + 1;

    CheckLoaded(MakePng(PngIHDR(64, 40, 8, 0, 0), Compressed, Half), 64, 40, ifGray8, 'Good CRC');
    CheckImagesEqual(Original, FImage, 'Good CRC');

    CheckLoaded(MakePng(PngIHDR(64, 40, 8, 0, 0), Compressed, Half, 1), 64, 40, ifGray8, 'Bad CRC of the second IDAT');
    CheckFirstRowsAndBlackEnd(Original, 10, 'Bad CRC of the second IDAT');

    CheckLoaded(MakePng(PngIHDR(64, 40, 8, 0, 0), Compressed, Half, 0), 64, 40, ifGray8, 'Bad CRC of the first IDAT');
    CheckFirstRowsAndBlackEnd(Original, 0, 'Bad CRC of the first IDAT');
  finally
    FreeImage(Original);
  end;
end;

procedure TTestPng.TestBadAncillaryChunkCrc;
var
  Original: TImageData;
  Compressed: AnsiString;
begin
  // Gray image with transparent color (tRNS) is loaded with alpha channel,
  // tRNS with bad CRC is skipped
  InitImage(Original);
  try
    NewGray8Pattern(Original, 8, 4);
    Compressed := ZLibCompress(RawRows(Original), 6);
    CheckLoaded(MakePng(PngIHDR(8, 4, 8, 0, 0), Compressed, MaxInt, -1, PngChunk('tRNS', BE16(1))),
                8, 4, ifA8Gray8, 'Good tRNS');
    CheckLoaded(MakePng(PngIHDR(8, 4, 8, 0, 0), Compressed, MaxInt, -1, PngChunk('tRNS', BE16(1), True)),
                8, 4, ifGray8, 'tRNS with bad CRC');
    CheckImagesEqual(Original, FImage, 'tRNS with bad CRC');
  finally
    FreeImage(Original);
  end;
end;

procedure TTestPng.TestLargeDataInMoreChunks;
const
  Width = 2400;
  Height = 2400;
var
  Original: TImageData;
  Stream: TMemoryStream;
  Pos, Count: Int64;
  Size: UInt32;
  ChunkType: AnsiString;
begin
  // Compressed noise is larger than the largest IDAT chunk written (16 MB)
  InitImage(Original);
  Stream := TMemoryStream.Create;
  try
    CheckTrue(NewImage(Width, Height, ifR8G8B8, Original), 'NewImage');
    FillTestImage(Original, 777);
    CheckTrue(SaveImageToStream('png', Stream, Original), 'Image is saved');

    // Walk through the chunks of the file
    Count := 0;
    Pos := Length(PngSignature);
    SetLength(ChunkType, 4);
    while Pos < Stream.Size do
    begin
      Size := (UInt32(PBuffer(Stream.Memory)[Pos]) shl 24) or (UInt32(PBuffer(Stream.Memory)[Pos + 1]) shl 16) or
        (UInt32(PBuffer(Stream.Memory)[Pos + 2]) shl 8) or PBuffer(Stream.Memory)[Pos + 3];
      Move(PBuffer(Stream.Memory)[Pos + 4], ChunkType[1], 4);
      if ChunkType = 'IDAT' then
      begin
        Inc(Count);
        CheckTrue(Size <= 16 * 1024 * 1024, Format('Size of IDAT chunk %d: %d', [Count, Size]));
      end;
      Pos := Pos + 12 + Size;
    end;
    CheckEquals(Stream.Size, Pos, 'The last chunk ends at the end of file');
    CheckTrue(Count >= 2, Format('Number of IDAT chunks: %d', [Count]));

    Stream.Position := 0;
    CheckTrue(LoadImageFromStream(Stream, FImage), 'Image is loaded');
    CheckImagesEqual(Original, FImage, 'Loaded image');
  finally
    Stream.Free;
    FreeImage(Original);
  end;
end;

initialization
  RegisterImagingTest(TTestPortableMaps);
  RegisterImagingTest(TTestPng);
  RegisterImagingTest(TTestPixelLoops);
  RegisterImagingTest(TTestTarga);
  RegisterImagingTest(TTestRadiance);
end.
