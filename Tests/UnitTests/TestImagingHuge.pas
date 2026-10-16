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

{ Tests of images with more than 2^31 pixels: number of pixels does not fit
  into 32-bit integer there.

  These tests are for 64-bit targets only and they are registered only when
  the test program is started with --huge option, so they are never reported
  as passed without being run. They allocate up to 4 bytes per pixel of
  a 46341x46341 image at once (8.6 GB) and some smaller image along it,
  about 10 GB of free memory is needed.
  The whole set takes several minutes, to run just this set:
    ImagingTests --huge --suite=TTestHugeImages   (Free Pascal build)}
unit TestImagingHuge;

{$I ImagingOptions.inc}

{$IF Defined(CPU64) or Defined(CPUX64) or Defined(CPU64BITS)}
  {$DEFINE HUGE_TESTS}
{$IFEND}

interface

uses
  SysUtils, Types, Classes, ImagingTypes, Imaging, ImagingFormats, ImagingUtility,
  ImagingBinary, ImagingCanvases, ImagingZLib, ImagingTestCase, ImagingTestPixels;

{ Returns True when the test program was started with --huge option.}
function HugeTestsRequested: Boolean;

{$IFDEF HUGE_TESTS}
type
  TTestHugeImages = class(TImagingTestCase)
  private
    FImage: TImageData;
    procedure NewHugeImage(Format: TImageFormat; Width, Height: Integer);
    { Creates image where all pixels are white and the last one is black.}
    procedure NewLitImage(Format: TImageFormat);
    { Checks that pixels of the image are not black, apart from the last one.}
    procedure CheckLit(const Msg: string);
    procedure ConvertAndCheck(Format: TImageFormat);
    function AllocHuge(const Size: Int64): Pointer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConvertGray;
    procedure TestConvertChannel;
    procedure TestConvertFloat;
    procedure TestConvertIndexed;
    procedure TestAlphaCheck16;
    procedure TestMemoryHelpers;
    procedure TestRotateMul90;
    procedure TestMakePalette;
    procedure TestOtsu;
    procedure TestCanvas;
    procedure TestDXT;
    procedure TestStreamIO;
    procedure TestMemoryIO;
    procedure TestPng;
    procedure TestZLibStreams;
  end;
{$ENDIF}

implementation

function HugeTestsRequested: Boolean;
begin
  // Second dash is taken as a part of the switch name
  Result := FindCmdLineSwitch('-huge', ['-'], True);
end;

{$IFDEF HUGE_TESTS}

const
  // The smallest square with more than 2^31 pixels (by 4633)
  HugeSide = 46341;

type
  { Stream over a memory block of fixed capacity. TMemoryStream of older
    Delphi cannot grow over 2 GB, this one does not depend on the RTL.
    Read and Write take 32-bit counts like TStream of FPC 3.2.}
  TBlockStream = class(TStream)
  private
    FData: Pointer;
    FCapacity, FSize, FPosition: Int64;
  protected
    function GetSize: Int64; override;
  public
    constructor Create(Data: Pointer; const Capacity: Int64);
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

constructor TBlockStream.Create(Data: Pointer; const Capacity: Int64);
begin
  inherited Create;
  FData := Data;
  FCapacity := Capacity;
end;

function TBlockStream.GetSize: Int64;
begin
  Result := FSize;
end;

function TBlockStream.Read(var Buffer; Count: LongInt): LongInt;
begin
  Result := Count;
  if FPosition + Result > FSize then
    Result := FSize - FPosition;
  if Result <= 0 then
  begin
    Result := 0;
    Exit;
  end;

  Move(PBuffer(FData)[FPosition], Buffer, Result);
  Inc(FPosition, Result);
end;

function TBlockStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  Result := Count;
  if FPosition + Result > FCapacity then
    Result := FCapacity - FPosition;
  if Result <= 0 then
  begin
    Result := 0;
    Exit;
  end;

  Move(Buffer, PBuffer(FData)[FPosition], Result);
  Inc(FPosition, Result);
  if FPosition > FSize then
    FSize := FPosition;
end;

function TBlockStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FPosition := Offset;
    soCurrent:   FPosition := FPosition + Offset;
    soEnd:       FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

function HugePixels: NativeInt;
begin
  Result := NativeInt(HugeSide) * HugeSide;
end;

{ TTestHugeImages }

procedure TTestHugeImages.SetUp;
begin
  InitImage(FImage);
end;

procedure TTestHugeImages.TearDown;
begin
  FreeImage(FImage);
end;

function TTestHugeImages.AllocHuge(const Size: Int64): Pointer;
begin
  Result := nil;
  try
    Result := AllocMem(Size);
  except
    on E: EOutOfMemory do
      Fail(Format('Not enough memory for the test (%d MB needed in one block)', [Size shr 20]));
  end;
end;

procedure TTestHugeImages.NewHugeImage(Format: TImageFormat; Width, Height: Integer);
var
  Created: Boolean;
begin
  Created := False;
  try
    Created := NewImage(Width, Height, Format, FImage);
  except
    on E: Exception do
      Fail('Huge image was not created (not enough memory?): ' + E.Message);
  end;

  CheckTrue(Created, 'NewImage');
  CheckTrue(Int64(FImage.Width) * FImage.Height > MaxInt, 'More than 2^31 pixels');
end;

procedure TTestHugeImages.NewLitImage(Format: TImageFormat);
begin
  CheckTrue(Format in [ifGray8, ifIndex8], 'Format with one byte per pixel');
  NewHugeImage(Format, HugeSide, HugeSide);
  FillChar(FImage.Bits^, FImage.Size, $FF);
  PBuffer(FImage.Bits)[FImage.Size - 1] := 0;

  if Format = ifIndex8 then
  begin
    FImage.Palette[0].Color := $FF000000;
    FImage.Palette[255].Color := $FFFFFFFF;
  end;
end;

procedure TTestHugeImages.CheckLit(const Msg: string);
const
  MinLit = 0.2;
var
  Positions: array[0..5] of NativeInt;
  I: Integer;
  Pixel: TIdealPixel;
begin
  Positions[0] := 0;
  Positions[1] := 1;
  Positions[2] := HugePixels div 2;
  Positions[3] := NativeInt(MaxInt) - 1;
  Positions[4] := NativeInt(MaxInt) + 1;
  Positions[5] := HugePixels - 2;

  for I := 0 to High(Positions) do
  begin
    // Red is the channel that all the formats have
    Pixel := ReadIdealPixel(FImage, Positions[I]);
    if Pixel.R < MinLit then
    begin
      Fail(Format('%s: pixel %d of %d is dark: %s',
        [Msg, Int64(Positions[I]), Int64(HugePixels), IdealPixelToStr(Pixel)]));
    end;
  end;

  Pixel := ReadIdealPixel(FImage, HugePixels - 1);
  CheckEquals(0.0, Pixel.R, 1e-6, Msg + ': the last pixel is black');
end;

procedure TTestHugeImages.ConvertAndCheck(Format: TImageFormat);
var
  Msg: string;
begin
  Msg := GetFormatName(FImage.Format) + ' to ' + GetFormatName(Format);
  CheckTrue(ConvertImage(FImage, Format), Msg + ': ConvertImage');
  CheckEquals(Ord(Format), Ord(FImage.Format), Msg + ': format');
  CheckEquals(HugeSide, FImage.Width, Msg + ': width');
  CheckEquals(HugeSide, FImage.Height, Msg + ': height');
  CheckLit(Msg);
end;

procedure TTestHugeImages.TestConvertGray;
begin
  NewLitImage(ifGray8);
  CheckLit('New image');
  ConvertAndCheck(ifIndex8);
  ConvertAndCheck(ifGray8);
  ConvertAndCheck(ifGray16);
  ConvertAndCheck(ifGray8);
  ConvertAndCheck(ifA8Gray8);
end;

procedure TTestHugeImages.TestConvertChannel;
begin
  NewLitImage(ifGray8);
  ConvertAndCheck(ifR5G6B5);
  ConvertAndCheck(ifGray8);
  // The case reported for 48000x44800 RGB scan
  ConvertAndCheck(ifR8G8B8);
  ConvertAndCheck(ifGray8);
  ConvertAndCheck(ifR5G6B5);
  ConvertAndCheck(ifX1R5G5B5);
end;

procedure TTestHugeImages.TestConvertFloat;
begin
  NewLitImage(ifGray8);
  ConvertAndCheck(ifR16F);
  ConvertAndCheck(ifR5G6B5);
  ConvertAndCheck(ifR16F);
  ConvertAndCheck(ifGray8);
end;

procedure TTestHugeImages.TestConvertIndexed;
var
  Info: TImageFormatInfo;
  Dst: PByte;
  DstPal: array[0..255] of TColor32Rec;
begin
  NewLitImage(ifIndex8);
  ConvertAndCheck(ifR5G6B5);
  FreeImage(FImage);

  NewLitImage(ifIndex8);
  ConvertAndCheck(ifR16F);
  FreeImage(FImage);

  // Conversion that ConvertImage does not get to: there is only one indexed format
  NewLitImage(ifIndex8);
  CheckTrue(GetImageFormatInfo(ifIndex8, Info), 'Index8 info');
  FillChar(DstPal, SizeOf(DstPal), 0);
  Dst := AllocHuge(HugePixels);
  try
    IndexToIndex(HugePixels, FImage.Bits, Dst, @Info, @Info, FImage.Palette, @DstPal);

    CheckEquals(255, PBuffer(Dst)[0], 'IndexToIndex: the first pixel');
    CheckEquals(255, PBuffer(Dst)[HugePixels - 2], 'IndexToIndex: pixel before the last one');
    CheckEquals(0, PBuffer(Dst)[HugePixels - 1], 'IndexToIndex: the last pixel');
    CheckSameMem(FImage.Palette, @DstPal, SizeOf(DstPal), 'IndexToIndex: palette');
  finally
    FreeMem(Dst);
  end;
end;

procedure TTestHugeImages.TestAlphaCheck16;
var
  Data: PByte;
begin
  Data := AllocHuge(HugePixels * SizeOf(Word));
  try
    CheckFalse(Has16BitImageAlpha(HugePixels, PWord(Data)), 'No pixel has the alpha bit');
    // High byte of the last pixel
    PBuffer(Data)[HugePixels * SizeOf(Word) - 1] := $80;
    CheckTrue(Has16BitImageAlpha(HugePixels, PWord(Data)), 'The last pixel has the alpha bit');
    CheckFalse(Has16BitImageAlpha(HugePixels - 1, PWord(Data)), 'Pixel after the end is not read');
  finally
    FreeMem(Data);
  end;
end;

procedure TTestHugeImages.TestMemoryHelpers;
const
  FirstPart = 1 shl 30;
var
  Data: PByte;
  CrcWhole, CrcParts: UInt32;
  LastWord: NativeInt;
begin
  // A few bytes more to see that nothing is written after the end
  Data := AllocHuge(HugePixels * SizeOf(Word) + 16);
  try
    FillMemoryByte(Data, HugePixels, $AB);
    CheckEquals($AB, PBuffer(Data)[0], 'FillMemoryByte: the first byte');
    CheckEquals($AB, PBuffer(Data)[HugePixels - 1], 'FillMemoryByte: the last byte');
    CheckEquals(0, PBuffer(Data)[HugePixels], 'FillMemoryByte: byte after the end');

    PBuffer(Data)[HugePixels - 1] := $CD;
    CrcWhole := $FFFFFFFF;
    CalcCrc32(CrcWhole, Data, HugePixels);
    CrcParts := $FFFFFFFF;
    CalcCrc32(CrcParts, Data, FirstPart);
    CalcCrc32(CrcParts, @PBuffer(Data)[FirstPart], HugePixels - FirstPart);
    CheckEquals(Int64(CrcParts), Int64(CrcWhole), 'CalcCrc32: whole buffer and the same data in two parts');

    LastWord := (HugePixels - 1) * SizeOf(Word);
    FillChar(Data^, HugePixels * SizeOf(Word) + 16, 0);
    PBuffer(Data)[0] := $12;
    PBuffer(Data)[1] := $34;
    PBuffer(Data)[LastWord] := $56;
    PBuffer(Data)[LastWord + 1] := $78;
    PBuffer(Data)[LastWord + 2] := $9A;
    SwapEndianWord(PWord(Data), HugePixels);

    CheckEquals($34, PBuffer(Data)[0], 'SwapEndianWord: the first word, low byte');
    CheckEquals($12, PBuffer(Data)[1], 'SwapEndianWord: the first word, high byte');
    CheckEquals($78, PBuffer(Data)[LastWord], 'SwapEndianWord: the last word, low byte');
    CheckEquals($56, PBuffer(Data)[LastWord + 1], 'SwapEndianWord: the last word, high byte');
    CheckEquals($9A, PBuffer(Data)[LastWord + 2], 'SwapEndianWord: word after the end');
  finally
    FreeMem(Data);
  end;
end;

procedure TTestHugeImages.TestRotateMul90;
const
  Width = 65536;
  Height = 32769;
  TopLeft = 1;
  TopRight = 2;
  BottomLeft = 3;
  BottomRight = 4;

  procedure CheckCorners(ExpWidth, ExpHeight: Integer; ExpTopLeft, ExpTopRight,
    ExpBottomLeft, ExpBottomRight: Byte; const Msg: string);
  var
    LastRow: NativeInt;
  begin
    CheckEquals(ExpWidth, FImage.Width, Msg + ': width');
    CheckEquals(ExpHeight, FImage.Height, Msg + ': height');
    LastRow := NativeInt(FImage.Height - 1) * FImage.Width;
    CheckEquals(ExpTopLeft, PBuffer(FImage.Bits)[0], Msg + ': top left');
    CheckEquals(ExpTopRight, PBuffer(FImage.Bits)[FImage.Width - 1], Msg + ': top right');
    CheckEquals(ExpBottomLeft, PBuffer(FImage.Bits)[LastRow], Msg + ': bottom left');
    CheckEquals(ExpBottomRight, PBuffer(FImage.Bits)[LastRow + FImage.Width - 1], Msg + ': bottom right');
    CheckEquals(0, PBuffer(FImage.Bits)[LastRow div 2 + 7], Msg + ': pixel in the middle');
  end;

begin
  NewHugeImage(ifGray8, Width, Height);
  PBuffer(FImage.Bits)[0] := TopLeft;
  PBuffer(FImage.Bits)[Width - 1] := TopRight;
  PBuffer(FImage.Bits)[NativeInt(Height - 1) * Width] := BottomLeft;
  PBuffer(FImage.Bits)[NativeInt(Height) * Width - 1] := BottomRight;
  CheckCorners(Width, Height, TopLeft, TopRight, BottomLeft, BottomRight, 'New image');

  // Counterclockwise
  RotateImageMul90(FImage, 90);
  CheckCorners(Height, Width, TopRight, BottomRight, TopLeft, BottomLeft, 'By 90');
  // 270 in total
  RotateImageMul90(FImage, 180);
  CheckCorners(Height, Width, BottomLeft, TopLeft, BottomRight, TopRight, 'By 180 more');
  // 540 in total
  RotateImageMul90(FImage, 270);
  CheckCorners(Width, Height, BottomRight, BottomLeft, TopRight, TopLeft, 'By 270 more');
end;

procedure TTestHugeImages.TestMakePalette;
var
  Images: TDynImageDataArray;
  Pal: array[0..255] of TColor32Rec;
  LastPixel: NativeInt;
begin
  // Black image with white last pixel
  NewHugeImage(ifR5G6B5, HugeSide, HugeSide);
  LastPixel := (HugePixels - 1) * SizeOf(Word);
  PBuffer(FImage.Bits)[LastPixel] := $FF;
  PBuffer(FImage.Bits)[LastPixel + 1] := $FF;

  FillChar(Pal, SizeOf(Pal), $55);
  SetLength(Images, 1);
  Images[0] := FImage;
  try
    CheckTrue(MakePaletteForImages(Images, @Pal, 2, False), 'MakePaletteForImages');
  finally
    // Image is still owned by the test case
    Images := nil;
  end;

  CheckTrue((Pal[0].Color = $FFFFFFFF) or (Pal[1].Color = $FFFFFFFF),
    Format('Color of the last pixel is in the palette ($%.8x, $%.8x)', [Pal[0].Color, Pal[1].Color]));
  CheckTrue((Pal[0].Color = $FF000000) or (Pal[1].Color = $FF000000),
    Format('Color of other pixels is in the palette ($%.8x, $%.8x)', [Pal[0].Color, Pal[1].Color]));
end;

procedure TTestHugeImages.TestOtsu;
var
  DarkCount: NativeInt;
  Level: Integer;
begin
  NewHugeImage(ifGray8, HugeSide, HugeSide);
  DarkCount := HugePixels div 2;
  FillChar(FImage.Bits^, DarkCount, 50);
  FillChar(PBuffer(FImage.Bits)[DarkCount], HugePixels - DarkCount, 200);

  Level := OtsuThresholding(FImage, True);
  CheckTrue((Level > 50) and (Level <= 200), Format('Level %d is between the two values', [Level]));
  CheckEquals(0, PBuffer(FImage.Bits)[0], 'The first dark pixel');
  CheckEquals(0, PBuffer(FImage.Bits)[DarkCount - 1], 'The last dark pixel');
  CheckEquals(255, PBuffer(FImage.Bits)[DarkCount], 'The first light pixel');
  CheckEquals(255, PBuffer(FImage.Bits)[HugePixels - 1], 'The last light pixel');
end;

procedure TTestHugeImages.TestCanvas;
const
  // Clipping rectangle is the end of the last two rows. Only the last 4633 pixels
  // of the image have offsets of 2^31 and more.
  ClipWidth = 6;
  ClipHeight = 2;
var
  Canvas, SmallCanvas: TImagingCanvas;
  Small: TImageData;
  Clip: TRect;
  I: Integer;

  procedure CheckClipped(Expected: Byte; const Msg: string);
  var
    X, Y: Integer;
    Offset: NativeInt;
  begin
    for Y := Clip.Top to Clip.Bottom - 1 do
    begin
      Offset := NativeInt(Y) * HugeSide + Clip.Left;
      CheckEquals(0, PBuffer(FImage.Bits)[Offset - 1], Format('%s: pixel before the rectangle in row %d', [Msg, Y]));
      for X := 0 to ClipWidth - 1 do
        CheckEquals(Expected, PBuffer(FImage.Bits)[Offset + X], Format('%s: pixel %d in row %d', [Msg, X, Y]));
    end;

    CheckEquals(0, PBuffer(FImage.Bits)[0], Msg + ': the first pixel of the image');
    CheckEquals(0, PBuffer(FImage.Bits)[NativeInt(Clip.Top) * HugeSide - 1], Msg + ': pixel above the rectangle');
  end;

begin
  NewHugeImage(ifGray8, HugeSide, HugeSide);
  Clip := Rect(HugeSide - ClipWidth, HugeSide - ClipHeight, HugeSide, HugeSide);
  InitImage(Small);
  SmallCanvas := nil;
  Canvas := TImagingCanvas.CreateForData(@FImage);
  try
    Canvas.ClipRect := Clip;
    CheckEqualsRect(Clip, Canvas.ClipRect, 'Clipping rectangle');
    CheckClipped(0, 'New image');

    Canvas.FillColor32 := $FF646464;
    Canvas.FillRectBlend(Rect(0, HugeSide - 10, HugeSide, HugeSide + 10), bfOne, bfZero);
    CheckClipped(100, 'FillRectBlend');

    Canvas.InvertColors;
    CheckClipped(155, 'InvertColors');

    // Change of one channel changes the luminance, the same way as in a small image
    CheckTrue(NewImage(ClipWidth, ClipHeight, ifGray8, Small), 'NewImage');
    FillChar(Small.Bits^, Small.Size, 155);
    SmallCanvas := TImagingCanvas.CreateForData(@Small);

    SmallCanvas.FillChannelFP(ChannelRed, 0.0);
    Canvas.FillChannelFP(ChannelRed, 0.0);
    CheckTrue(PByte(Small.Bits)^ < 155, 'FillChannelFP changes the small image');
    CheckClipped(PByte(Small.Bits)^, 'FillChannelFP');

    SmallCanvas.FillChannel(ChannelGreen, 255);
    Canvas.FillChannel(ChannelGreen, 255);
    CheckTrue(PByte(Small.Bits)^ > 155, 'FillChannel changes the small image');
    CheckClipped(PByte(Small.Bits)^, 'FillChannel');

    // Filters work with copy of the whole image
    Canvas.FillColor32 := $FF505050;
    Canvas.FillRectBlend(Clip, bfOne, bfZero);
    Canvas.ApplyConvolution3x3(FilterSharpen3x3);
    CheckClipped(80, 'ApplyConvolution3x3');
    Canvas.ApplyMaxFilter(3);
    CheckClipped(80, 'ApplyMaxFilter');

    // Ellipse larger than the rectangle
    Canvas.FillColor32 := $FFC8C8C8;
    Canvas.PenColor32 := $FFC8C8C8;
    Canvas.Ellipse(Rect(HugeSide - 100, HugeSide - 100, HugeSide + 100, HugeSide + 100));
    CheckClipped(200, 'Ellipse');

    // Drawing to and from another canvas
    FillChar(Small.Bits^, Small.Size, 30);
    SmallCanvas.DrawBlend(Rect(0, 0, ClipWidth, ClipHeight), Canvas, Clip.Left, Clip.Top, bfOne, bfZero);
    CheckClipped(30, 'DrawBlend to huge image');

    Canvas.InvertColors;
    Canvas.DrawBlend(Clip, SmallCanvas, 0, 0, bfOne, bfZero);
    for I := 0 to Small.Size - 1 do
      CheckEquals(225, PBuffer(Small.Bits)[I], Format('DrawBlend from huge image: pixel %d', [I]));

    FillChar(Small.Bits^, Small.Size, 60);
    SmallCanvas.StretchDrawBlend(Rect(0, 0, ClipWidth, ClipHeight), Canvas, Clip, bfOne, bfZero, rfNearest);
    CheckClipped(60, 'StretchDrawBlend to huge image');
  finally
    SmallCanvas.Free;
    Canvas.Free;
    FreeImage(Small);
  end;
end;

procedure TTestHugeImages.TestDXT;
const
  // Multiple of 4 (size of DXT blocks), more than 2^31 pixels
  Width = 65536;
  Height = 32772;
  Black = TColor32($FF000000);
  White = TColor32($FFFFFFFF);

  procedure CheckDecoded(const Msg: string);
  var
    Pixels: PColor32RecBuffer;
    LastRow: NativeInt;
  begin
    CheckEquals(Ord(ifA8R8G8B8), Ord(FImage.Format), Msg + ': format');
    Pixels := FImage.Bits;
    LastRow := NativeInt(Height - 1) * Width;
    CheckEqualsColor32(Black, Pixels[0].Color, Msg + ': the first pixel');
    CheckEqualsColor32(Black, Pixels[LastRow + Width - 5].Color, Msg + ': pixel left of the last block');
    CheckEqualsColor32(White, Pixels[LastRow + Width - 4].Color,
                       Msg + ': the first pixel of the last block in the last row');
    CheckEqualsColor32(White, Pixels[LastRow + Width - 1].Color, Msg + ': the last pixel');
    CheckEqualsColor32(White, Pixels[LastRow - 3 * Width + Width - 4].Color,
                       Msg + ': the first pixel of the last block');
  end;

var
  LastBlock: PByte;
begin
  // Decoding: blocks of zeros are black, the last block is white
  NewHugeImage(ifDXT1, Width, Height);
  LastBlock := @PBuffer(FImage.Bits)[FImage.Size - 8];
  // Color0 = white in R5G6B5, Color1 = black, all pixels have color 0
  PWord(LastBlock)^ := $FFFF;
  CheckTrue(ConvertImage(FImage, ifA8R8G8B8), 'Decoding of DXT1');
  CheckDecoded('Decoded DXT1');

  // Encoding reads blocks of pixels from the whole image
  CheckTrue(ConvertImage(FImage, ifDXT1), 'Encoding to DXT1');
  CheckEquals(Ord(ifDXT1), Ord(FImage.Format), 'Encoded: format');
  CheckTrue(ConvertImage(FImage, ifA8R8G8B8), 'Decoding of encoded DXT1');
  CheckDecoded('Encoded and decoded DXT1');
end;

procedure TTestHugeImages.TestStreamIO;
const
  // Room for the header and footer of the file
  Extra = 4096;
var
  Data: Pointer;
  Stream: TBlockStream;
  Loaded: TImageData;
  OldRle: Integer;
  Markers: array[0..2] of NativeInt;
  I: Integer;
begin
  // Uncompressed Targa is read and written in one call of the size of the
  // image, stream IO passes it to TStream in smaller blocks
  NewLitImage(ifGray8);
  Markers[0] := NativeInt(1) shl 30 + 5;
  Markers[1] := NativeInt(1) shl 31 + 7;
  Markers[2] := HugePixels - 2;
  for I := Low(Markers) to High(Markers) do
    PBuffer(FImage.Bits)[Markers[I]] := 50 + I;

  InitImage(Loaded);
  Data := AllocHuge(FImage.Size + Extra);
  Stream := TBlockStream.Create(Data, FImage.Size + Extra);
  OldRle := GetOption(ImagingTargaRLE);
  try
    SetOption(ImagingTargaRLE, 0);
    CheckTrue(SaveImageToStream('tga', Stream, FImage), 'Image is saved');
    CheckTrue(Stream.Size > FImage.Size, Format('Size of the file: %d bytes', [Stream.Size]));

    Stream.Position := 0;
    CheckTrue(LoadImageFromStream(Stream, Loaded), 'Image is loaded');
    CheckEquals(HugeSide, Loaded.Width, 'Width');
    CheckEquals(HugeSide, Loaded.Height, 'Height');
    CheckEquals(Ord(ifGray8), Ord(Loaded.Format), 'Format');
    for I := Low(Markers) to High(Markers) do
      CheckEquals(50 + I, PBuffer(Loaded.Bits)[Markers[I]], Format('Marker %d', [I]));
    if not CompareMem(FImage.Bits, Loaded.Bits, FImage.Size) then
      CheckSameMem(FImage.Bits, Loaded.Bits, FImage.Size, 'Loaded image');
  finally
    SetOption(ImagingTargaRLE, OldRle);
    Stream.Free;
    FreeMem(Data);
    FreeImage(Loaded);
  end;
end;

procedure TTestHugeImages.TestMemoryIO;
const
  Extra = 4096;
var
  Data: Pointer;
  Size: Int64;
  Loaded: TImageData;
  OldRle: Integer;
begin
  // Positions in memory over 2 GB
  NewLitImage(ifGray8);
  PBuffer(FImage.Bits)[NativeInt(1) shl 31 + 3] := 77;

  InitImage(Loaded);
  Size := FImage.Size + Extra;
  Data := AllocHuge(Size);
  OldRle := GetOption(ImagingTargaRLE);
  try
    SetOption(ImagingTargaRLE, 0);
    CheckTrue(SaveImageToMemory('tga', Data, Size, FImage), 'Image is saved');
    CheckTrue((Size > FImage.Size) and (Size < FImage.Size + Extra), Format('Size of the file: %d bytes', [Size]));

    CheckTrue(LoadImageFromMemory(Data, Size, Loaded), 'Image is loaded');
    CheckEquals(HugeSide, Loaded.Width, 'Width');
    CheckEquals(HugeSide, Loaded.Height, 'Height');
    CheckEquals(77, PBuffer(Loaded.Bits)[NativeInt(1) shl 31 + 3], 'Marker over 2 GB');
    if not CompareMem(FImage.Bits, Loaded.Bits, FImage.Size) then
      CheckSameMem(FImage.Bits, Loaded.Bits, FImage.Size, 'Loaded image');
  finally
    SetOption(ImagingTargaRLE, OldRle);
    FreeMem(Data);
    FreeImage(Loaded);
  end;
end;

procedure TTestHugeImages.TestPng;
const
  // Compressed data of mostly uniform images are small
  StreamCapacity = 256 * 1024 * 1024;
  RgbWidth = 40000;
  RgbHeight = 36000;

  procedure RoundTrip(const Markers: array of NativeInt; const Msg: string);
  var
    Data: Pointer;
    Stream: TBlockStream;
    Loaded: TImageData;
    I: Integer;
  begin
    for I := Low(Markers) to High(Markers) do
      PBuffer(FImage.Bits)[Markers[I]] := 30 + I;

    InitImage(Loaded);
    Data := AllocHuge(StreamCapacity);
    Stream := TBlockStream.Create(Data, StreamCapacity);
    try
      CheckTrue(SaveImageToStream('png', Stream, FImage), Msg + ': image is saved');
      CheckTrue(Stream.Size < StreamCapacity, Format('%s: size of the file %d bytes', [Msg, Stream.Size]));
      Stream.Position := 0;
      CheckTrue(LoadImageFromStream(Stream, Loaded), Msg + ': image is loaded');
      CheckEquals(FImage.Width, Loaded.Width, Msg + ': width');
      CheckEquals(FImage.Height, Loaded.Height, Msg + ': height');
      CheckEquals(Ord(FImage.Format), Ord(Loaded.Format), Msg + ': format');
      for I := Low(Markers) to High(Markers) do
        CheckEquals(30 + I, PBuffer(Loaded.Bits)[Markers[I]], Format('%s: marker %d', [Msg, I]));
      if not CompareMem(FImage.Bits, Loaded.Bits, FImage.Size) then
        CheckSameMem(FImage.Bits, Loaded.Bits, FImage.Size, Msg + ': loaded image');
    finally
      Stream.Free;
      FreeMem(Data);
      FreeImage(Loaded);
    end;
  end;

begin
  // More than 2^31 pixels
  NewLitImage(ifGray8);
  RoundTrip([NativeInt(1) shl 30 + 5, NativeInt(1) shl 31 + 7, HugePixels - 2], 'Gray8');
  FreeImage(FImage);

  // More than 4 GB of pixel data (fewer than 2^31 pixels, so no NewHugeImage)
  CheckTrue(NewImage(RgbWidth, RgbHeight, ifR8G8B8, FImage), 'NewImage of R8G8B8');
  CheckTrue(FImage.Size > Int64(1) shl 32, 'More than 4 GB');
  FillChar(FImage.Bits^, FImage.Size, $C0);
  RoundTrip([NativeInt(1) shl 31 + 11, NativeInt(1) shl 32 + 13, FImage.Size - 1], 'R8G8B8');
end;

procedure TTestHugeImages.TestZLibStreams;
const
  BlockSize = 64 * 1024 * 1024;
  BlockCount = 80; // 5 GB
var
  Block, Output: TDynByteArray;
  Compressed: TMemoryStream;
  Compression: TCompressionStream;
  Decompression: TDecompressionStream;
  I, Len: Integer;
begin
  // More than 4 GB through zlib streams, compressible data need little memory
  SetLength(Block, BlockSize);
  for I := 0 to BlockSize - 1 do
    Block[I] := Byte(I shr 20);
  SetLength(Output, BlockSize);

  Compressed := TMemoryStream.Create;
  try
    Compression := TCompressionStream.Create(clFastest, Compressed);
    try
      for I := 1 to BlockCount do
        Compression.WriteBuffer(Block[0], BlockSize);
      CheckEquals(Int64(BlockSize) * BlockCount, Compression.Position, 'Position of compression stream');
      CheckTrue(Compression.CompressionRate > 99, Format('Compression rate %.2f', [Compression.CompressionRate]));
    finally
      Compression.Free;
    end;

    Compressed.Position := 0;
    Decompression := TDecompressionStream.Create(Compressed);
    try
      for I := 1 to BlockCount do
      begin
        FillChar(Output[0], BlockSize, 0);
        Decompression.ReadBuffer(Output[0], BlockSize);
        if not CompareMem(Pointer(Block), Pointer(Output), BlockSize) then
          Fail(Format('Decompressed block %d differs', [I]));
      end;
      CheckEquals(Int64(BlockSize) * BlockCount, Decompression.Position, 'Position of decompression stream');
      Len := Decompression.Read(Output[0], 10);
      CheckEquals(0, Len, 'Nothing after the end');
    finally
      Decompression.Free;
    end;
  finally
    Compressed.Free;
  end;
end;

{$ENDIF}

initialization
{$IFDEF HUGE_TESTS}
  if HugeTestsRequested then
    RegisterImagingTest(TTestHugeImages);
{$ENDIF}
end.
