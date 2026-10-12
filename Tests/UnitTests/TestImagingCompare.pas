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

{ Tests of image error metrics in ImagingCompare unit.

  Metrics of test images in all supported data formats are compared with
  the ones computed from pixels decoded by the model in ImagingTestPixels
  unit, the rest are cases with results known in advance.}
unit TestImagingCompare;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, ImagingTypes, Imaging, ImagingFormats, ImagingCompare,
  ImagingTestCase, ImagingTestPixels;

type
  TTestErrorMetrics = class(TImagingTestCase)
  private
    FImage1, FImage2: TImageData;
    FPSNR, FMSE, FRMSE, FPAE, FMAE: Double;
    { Creates both test images with all bytes set to the given values.}
    procedure NewImages(Format: TImageFormat; Width, Height: Integer; Fill1, Fill2: Byte);
    { Sets all samples of floating point image to the given value.}
    procedure FillFloat(var Image: TImageData; Value: Single);
    procedure Compute(const Msg: string);
    { Checks that the value is within the tolerance (relative, with small
      absolute part for zero).}
    procedure CheckMetric(const Expected, Actual: Double; const Msg: string);
    procedure CheckMetrics(const MSE, RMSE, MAE, PAE, PSNR: Double; const Msg: string);
    procedure CheckIdentical(const Msg: string);
    procedure CheckAgainstModel(const Info: TImageFormatInfo);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAllFormatsAgainstModel;
    procedure TestUnsupportedFormats;
    procedure TestMismatchedImages;
    procedure TestIdenticalImages;
    procedure TestConstantDifference;
    procedure TestSingleSample;
    procedure TestUnusedBytesSkipped;
    procedure TestLargeIntegerSamples;
    procedure TestFloatSamples;
    procedure TestFloatPeak;
    procedure TestInfiniteFloatSamples;
    procedure TestManySamples;
  end;

implementation

uses
  Math;

const
  // Typed so the math is done in double precision by all compilers
  RelTolerance: Double = 1e-9;
  AbsTolerance: Double = 1e-12;
  PeakUInt8: Double = 255.0;
  PeakUInt32: Double = 4294967295.0;
  // 20 * Log10(255): all samples of 8-bit image differ by one
  PsnrOneOf255: Double = 48.130803608679103;
  // 20 * Log10(2)
  PsnrHalfOfPeak: Double = 6.020599913279624;

{ Divides in double precision, FPC can fold division of constants in single.}
function Ratio(const A, B: Double): Double;
begin
  Result := A / B;
end;

function IsSupportedFormat(const Info: TImageFormatInfo): Boolean;
begin
  Result := not Info.IsIndexed and not Info.IsSpecial and not Info.UsePixelFormat;
end;

{ Returns True when pixels of the format store the channel as their own sample.}
function IsStoredChannel(const Info: TImageFormatInfo; Channel: TPixelChannel): Boolean;
begin
  case Channel of
    chAlpha: Result := Info.HasAlphaChannel;
    chRed:   Result := True;
  else
    // Gray is in the red channel of the model
    Result := not Info.HasGrayChannel and not IsOneChannelFloat(Info);
  end;
end;

{ TTestErrorMetrics }

procedure TTestErrorMetrics.SetUp;
begin
  InitImage(FImage1);
  InitImage(FImage2);
end;

procedure TTestErrorMetrics.TearDown;
begin
  FreeImage(FImage1);
  FreeImage(FImage2);
end;

procedure TTestErrorMetrics.NewImages(Format: TImageFormat; Width, Height: Integer;
  Fill1, Fill2: Byte);
begin
  FreeImage(FImage1);
  FreeImage(FImage2);
  CheckTrue(NewImage(Width, Height, Format, FImage1), 'NewImage 1 of ' + GetFormatName(Format));
  CheckTrue(NewImage(Width, Height, Format, FImage2), 'NewImage 2 of ' + GetFormatName(Format));
  FillChar(FImage1.Bits^, FImage1.Size, Fill1);
  FillChar(FImage2.Bits^, FImage2.Size, Fill2);
end;

procedure TTestErrorMetrics.FillFloat(var Image: TImageData; Value: Single);
begin
  FillFloatImage(Image, [Value]);
end;

procedure TTestErrorMetrics.Compute(const Msg: string);
begin
  CheckTrue(ComputeErrorMetrics(FImage1, FImage2, FPSNR, FMSE, FRMSE, FPAE, FMAE),
    Msg + ': metrics computed');
end;

procedure TTestErrorMetrics.CheckMetric(const Expected, Actual: Double; const Msg: string);
begin
  CheckEquals(Expected, Actual, Abs(Expected) * RelTolerance + AbsTolerance, Msg);
end;

procedure TTestErrorMetrics.CheckMetrics(const MSE, RMSE, MAE, PAE, PSNR: Double;
  const Msg: string);
begin
  CheckMetric(MSE, FMSE, Msg + ': MSE');
  CheckMetric(RMSE, FRMSE, Msg + ': RMSE');
  CheckMetric(MAE, FMAE, Msg + ': MAE');
  CheckMetric(PAE, FPAE, Msg + ': PAE');
  CheckMetric(PSNR, FPSNR, Msg + ': PSNR');
end;

procedure TTestErrorMetrics.CheckIdentical(const Msg: string);
begin
  CheckTrue(FMSE = 0, Msg + ': MSE is zero');
  CheckTrue(FRMSE = 0, Msg + ': RMSE is zero');
  CheckTrue(FMAE = 0, Msg + ': MAE is zero');
  CheckTrue(FPAE = 0, Msg + ': PAE is zero');
  CheckTrue(IsInfinite(FPSNR) and (FPSNR > 0), Msg + ': PSNR is infinite');
end;

procedure TTestErrorMetrics.CheckAgainstModel(const Info: TImageFormatInfo);
const
  Width = 13;
  Height = 7;
var
  I, SampleCount: Integer;
  Channel: TPixelChannel;
  Pixel1, Pixel2: TIdealPixel;
  Peak, Diff, SumAbs, SumSqr, MaxDiff, MSE: Double;
  Name: string;
begin
  Name := string(Info.Name);
  NewImages(Info.Format, Width, Height, 0, 0);
  FillTestImage(FImage1, 1);
  FillTestImage(FImage2, 2);

  // Model has integer channels scaled to 0..1
  if Info.IsFloatingPoint then
    Peak := 1.0
  else
    Peak := Power(2.0, ChannelBits(Info, chRed)) - 1.0;

  SampleCount := 0;
  SumAbs := 0;
  SumSqr := 0;
  MaxDiff := 0;

  for I := 0 to Width * Height - 1 do
  begin
    Pixel1 := ReadIdealPixel(FImage1, I);
    Pixel2 := ReadIdealPixel(FImage2, I);

    for Channel := Low(TPixelChannel) to High(TPixelChannel) do
    begin
      if IsStoredChannel(Info, Channel) then
      begin
        Diff := Abs(Pixel2.Channels[Channel] - Pixel1.Channels[Channel]) * Peak;
        SumAbs := SumAbs + Diff;
        SumSqr := SumSqr + Diff * Diff;
        if Diff > MaxDiff then
          MaxDiff := Diff;
        Inc(SampleCount);
      end;
    end;
  end;

  CheckEquals(Width * Height * Info.ChannelCount, SampleCount, Name + ': samples of the model');
  CheckTrue(MaxDiff > 0, Name + ': test images differ');
  MSE := SumSqr / SampleCount;

  Compute(Name);
  CheckMetrics(MSE, Sqrt(MSE), SumAbs / SampleCount, MaxDiff,
               20.0 * Log10(Peak / Sqrt(MSE)), Name);
end;

procedure TTestErrorMetrics.TestAllFormatsAgainstModel;
var
  Fmt: TImageFormat;
  Info: TImageFormatInfo;
  Count: Integer;
begin
  Count := 0;

  for Fmt := Low(TImageFormat) to High(TImageFormat) do
  begin
    if (Fmt <> ifDefault) and GetImageFormatInfo(Fmt, Info) and IsSupportedFormat(Info) then
    begin
      CheckAgainstModel(Info);
      Inc(Count);
    end;
  end;

  // 8, 16, 32, and 64-bit integers, half and single floats, gray, RGB, alpha
  CheckTrue(Count >= 20, 'Number of formats checked: ' + IntToStr(Count));
end;

procedure TTestErrorMetrics.TestUnsupportedFormats;
var
  Fmt: TImageFormat;
  Info: TImageFormatInfo;
  Count: Integer;
begin
  Count := 0;

  for Fmt := Low(TImageFormat) to High(TImageFormat) do
  begin
    if (Fmt <> ifDefault) and GetImageFormatInfo(Fmt, Info) and not IsSupportedFormat(Info) then
    begin
      NewImages(Fmt, 8, 8, 0, 255);
      FPSNR := 1;
      FMSE := 1;
      CheckFalse(ComputeErrorMetrics(FImage1, FImage2, FPSNR, FMSE, FRMSE, FPAE, FMAE),
        string(Info.Name) + ': not supported');
      CheckTrue((FPSNR = 0) and (FMSE = 0), string(Info.Name) + ': metrics zeroed');
      Inc(Count);
    end;
  end;

  CheckTrue(Count >= 10, 'Number of formats checked: ' + IntToStr(Count));
end;

procedure TTestErrorMetrics.TestMismatchedImages;
var
  Empty: TImageData;
begin
  NewImages(ifGray8, 8, 8, 0, 0);
  CheckTrue(ComputeErrorMetrics(FImage1, FImage2, FPSNR, FMSE, FRMSE, FPAE, FMAE), 'Same images');

  FreeImage(FImage2);
  NewImage(8, 4, ifGray8, FImage2);
  CheckFalse(ComputeErrorMetrics(FImage1, FImage2, FPSNR, FMSE, FRMSE, FPAE, FMAE),
    'Different height');

  FreeImage(FImage2);
  NewImage(4, 8, ifGray8, FImage2);
  CheckFalse(ComputeErrorMetrics(FImage1, FImage2, FPSNR, FMSE, FRMSE, FPAE, FMAE),
    'Different width');

  // The same size of data
  FreeImage(FImage2);
  NewImage(4, 8, ifA8Gray8, FImage2);
  CheckFalse(ComputeErrorMetrics(FImage1, FImage2, FPSNR, FMSE, FRMSE, FPAE, FMAE),
    'Different format');

  InitImage(Empty);
  CheckFalse(ComputeErrorMetrics(FImage1, Empty, FPSNR, FMSE, FRMSE, FPAE, FMAE),
    'Second image empty');
  CheckFalse(ComputeErrorMetrics(Empty, FImage1, FPSNR, FMSE, FRMSE, FPAE, FMAE),
    'First image empty');

  CheckTrue(ComputeErrorMetrics(FImage1, FImage1, FPSNR, FMSE, FRMSE, FPAE, FMAE, 0.0),
    'Zero peak is ignored for integer formats');
  NewImages(ifR32F, 8, 8, 0, 0);
  CheckFalse(ComputeErrorMetrics(FImage1, FImage2, FPSNR, FMSE, FRMSE, FPAE, FMAE, 0.0),
    'Zero peak of floating point format');
end;

procedure TTestErrorMetrics.TestIdenticalImages;
begin
  NewImages(ifGray8, 9, 5, 77, 77);
  Compute('Gray8');
  CheckIdentical('Gray8');

  NewImages(ifA16R16G16B16, 9, 5, 77, 77);
  Compute('A16R16G16B16');
  CheckIdentical('A16R16G16B16');

  NewImages(ifA32B32G32R32F, 9, 5, 0, 0);
  FillFloat(FImage1, 0.3);
  FillFloat(FImage2, 0.3);
  Compute('A32B32G32R32F');
  CheckIdentical('A32B32G32R32F');
end;

procedure TTestErrorMetrics.TestConstantDifference;
begin
  NewImages(ifGray8, 16, 16, 10, 11);
  Compute('Gray8');
  CheckMetrics(1, 1, 1, 1, PsnrOneOf255, 'Gray8');

  // Order of images does not matter
  NewImages(ifA8R8G8B8, 16, 16, 110, 100);
  Compute('A8R8G8B8');
  CheckMetrics(100, 10, 10, 10, 20.0 * Log10(Ratio(PeakUInt8, 10)), 'A8R8G8B8');

  // Bytes $0A0A and $0B0B: the same part of the range as in Gray8
  NewImages(ifGray16, 16, 16, 10, 11);
  Compute('Gray16');
  CheckMetrics(66049, 257, 257, 257, PsnrOneOf255, 'Gray16');

  NewImages(ifR16G16B16, 5, 3, 11, 10);
  Compute('R16G16B16');
  CheckMetrics(66049, 257, 257, 257, PsnrOneOf255, 'R16G16B16');

  NewImages(ifR8G8B8, 5, 3, 0, 255);
  Compute('R8G8B8');
  CheckMetrics(65025, 255, 255, 255, 0, 'R8G8B8');
end;

procedure TTestErrorMetrics.TestSingleSample;
begin
  // One of 16 samples differs by 16
  NewImages(ifGray8, 4, 4, 100, 100);
  PBuffer(FImage2.Bits)[9] := 116;
  Compute('Gray8');
  CheckMetrics(16, 4, 1, 16, 20.0 * Log10(Ratio(PeakUInt8, 4)), 'Gray8');

  // One of 64 samples (green of the last pixel) differs by 8
  NewImages(ifA8R8G8B8, 4, 4, 100, 100);
  PBuffer(FImage1.Bits)[61] := 92;
  Compute('A8R8G8B8');
  CheckMetrics(1, 1, 0.125, 8, PsnrOneOf255, 'A8R8G8B8');

  // Alpha has the same weight as color channels
  NewImages(ifA8R8G8B8, 4, 4, 100, 100);
  PBuffer(FImage1.Bits)[63] := 92;
  Compute('A8R8G8B8 alpha');
  CheckMetrics(1, 1, 0.125, 8, PsnrOneOf255, 'A8R8G8B8 alpha');
end;

procedure TTestErrorMetrics.TestUnusedBytesSkipped;
var
  I: Integer;
begin
  NewImages(ifX8R8G8B8, 4, 4, 100, 100);
  for I := 0 to 15 do
    PBuffer(FImage2.Bits)[I * 4 + 3] := I;

  Compute('X8R8G8B8 unused bytes');
  CheckIdentical('X8R8G8B8 unused bytes');

  // Blue, green, and red of all pixels differ by 1, 2, and 3
  for I := 0 to 15 do
  begin
    PBuffer(FImage2.Bits)[I * 4] := 101;
    PBuffer(FImage2.Bits)[I * 4 + 1] := 102;
    PBuffer(FImage2.Bits)[I * 4 + 2] := 103;
  end;

  Compute('X8R8G8B8');
  CheckMetrics(Ratio(14, 3), Sqrt(Ratio(14, 3)), 2, 3,
               20.0 * Log10(PeakUInt8 / Sqrt(Ratio(14, 3))), 'X8R8G8B8');
end;

procedure TTestErrorMetrics.TestLargeIntegerSamples;
const
  TwoPow32: Double = 4294967296.0;
var
  Peak64: Double;
begin
  // The largest difference in both directions
  NewImages(ifGray32, 3, 3, $FF, 0);
  Compute('Gray32');
  CheckMetrics(PeakUInt32 * PeakUInt32, PeakUInt32, PeakUInt32, PeakUInt32, 0, 'Gray32');

  NewImages(ifGray32, 3, 3, 0, $FF);
  Compute('Gray32 swapped');
  CheckMetric(PeakUInt32, FPAE, 'Gray32 swapped: PAE');

  NewImages(ifGray32, 3, 3, $80, $80);
  PUInt32(FImage2.Bits)^ := $80808083;
  Compute('Gray32 small');
  CheckMetrics(1, 1, Ratio(1, 3), 3, 20.0 * Log10(PeakUInt32), 'Gray32 small');

  Peak64 := TwoPow32 * TwoPow32;
  NewImages(ifGray64, 3, 3, 0, $FF);
  Compute('Gray64');
  CheckMetrics(Peak64 * Peak64, Peak64, Peak64, Peak64, 0, 'Gray64');

  // Difference of one in the high half, and of one across the halves
  NewImages(ifGray64, 2, 1, 0, 0);
  PUInt32(@PBuffer(FImage1.Bits)[4])^ := 1;
  PUInt32(@PBuffer(FImage2.Bits)[8])^ := $FFFFFFFF;
  PUInt32(@PBuffer(FImage1.Bits)[12])^ := 1;
  Compute('Gray64 halves');
  CheckMetric(TwoPow32, FPAE, 'Gray64 halves: PAE');
  CheckMetric((TwoPow32 + 1) / 2, FMAE, 'Gray64 halves: MAE');
end;

procedure TTestErrorMetrics.TestFloatSamples;
begin
  NewImages(ifR32F, 7, 3, 0, 0);
  FillFloat(FImage1, 0.25);
  FillFloat(FImage2, 0.75);
  Compute('R32F');
  CheckMetrics(0.25, 0.5, 0.5, 0.5, PsnrHalfOfPeak, 'R32F');

  NewImages(ifA16B16G16R16F, 7, 3, 0, 0);
  FillFloat(FImage1, 0.75);
  FillFloat(FImage2, 0.25);
  Compute('A16B16G16R16F');
  CheckMetrics(0.25, 0.5, 0.5, 0.5, PsnrHalfOfPeak, 'A16B16G16R16F');

  // Negative values and values over one
  NewImages(ifR32G32B32F, 7, 3, 0, 0);
  FillFloat(FImage1, -1.5);
  FillFloat(FImage2, 2.5);
  Compute('R32G32B32F');
  CheckMetrics(16, 4, 4, 4, 20.0 * Log10(Ratio(1, 4)), 'R32G32B32F');
end;

procedure TTestErrorMetrics.TestFloatPeak;
begin
  NewImages(ifR32F, 7, 3, 0, 0);
  FillFloat(FImage1, 0.25);
  FillFloat(FImage2, 0.75);
  CheckTrue(ComputeErrorMetrics(FImage1, FImage2, FPSNR, FMSE, FRMSE, FPAE, FMAE, 2.0),
    'R32F: metrics computed');
  CheckMetrics(0.25, 0.5, 0.5, 0.5, 2.0 * PsnrHalfOfPeak, 'R32F peak 2');

  // Peak of integer formats is given by the format
  NewImages(ifGray8, 16, 16, 10, 11);
  CheckTrue(ComputeErrorMetrics(FImage1, FImage2, FPSNR, FMSE, FRMSE, FPAE, FMAE, 1000.0),
    'Gray8: metrics computed');
  CheckMetric(PsnrOneOf255, FPSNR, 'Gray8: PSNR');
end;

procedure TTestErrorMetrics.TestInfiniteFloatSamples;
begin
  NewImages(ifR32F, 4, 2, 0, 0);
  FillFloat(FImage1, 0.5);
  FillFloat(FImage2, 0.5);
  // Bits of positive and negative infinity
  PUInt32(FImage1.Bits)^ := $7F800000;
  PUInt32(FImage2.Bits)^ := $7F800000;
  PUInt32(@PBuffer(FImage1.Bits)[4])^ := $FF800000;
  PUInt32(@PBuffer(FImage2.Bits)[4])^ := $FF800000;
  Compute('R32F');
  CheckIdentical('R32F');

  NewImages(ifR16F, 4, 2, 0, 0);
  PWord(FImage1.Bits)^ := $7C00;
  PWord(FImage2.Bits)^ := $7C00;
  PWord(@PBuffer(FImage2.Bits)[2])^ := $3C00;
  Compute('R16F');
  CheckMetrics(0.125, Sqrt(Ratio(1, 8)), 0.125, 1, 20.0 * Log10(Sqrt(Ratio(8, 1))), 'R16F');
end;

procedure TTestErrorMetrics.TestManySamples;
begin
  // More samples than single precision sum can count (2^24)
  NewImages(ifGray8, 6000, 4000, 10, 11);
  Compute('Gray8');
  CheckMetrics(1, 1, 1, 1, PsnrOneOf255, 'Gray8');

  NewImages(ifR8G8B8, 4000, 2000, 13, 10);
  PBuffer(FImage2.Bits)[FImage2.Size - 1] := 113;
  Compute('R8G8B8');
  CheckMetric(100, FPAE, 'R8G8B8: PAE');
  CheckMetric(3 + Ratio(97, 24000000), FMAE, 'R8G8B8: MAE');
  CheckMetric(9 + Ratio(9991, 24000000), FMSE, 'R8G8B8: MSE');
end;

initialization
  RegisterImagingTest(TTestErrorMetrics);
end.
