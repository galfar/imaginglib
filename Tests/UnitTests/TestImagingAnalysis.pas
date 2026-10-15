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

{ Tests of ImagingAnalysis unit: statistics, content checks, histograms,
  and comparison of images.

  Metrics of test images in all supported data formats are compared with
  the ones computed from pixels decoded by the model in ImagingTestPixels
  unit, the rest are cases with results known in advance.}
unit TestImagingAnalysis;

{$I ImagingOptions.inc}

interface

uses
  Types, SysUtils, Classes, ImagingTypes, Imaging, ImagingFormats, ImagingUtility,
  ImagingAnalysis, ImagingTestCase, ImagingTestPixels;

type
  { Base of the tests with two images that are freed after each test.}
  TAnalysisTestCase = class(TImagingTestCase)
  protected
    FImage1, FImage2: TImageData;
    procedure SetUp; override;
    procedure TearDown; override;
    { Creates both test images with all bytes set to the given values.}
    procedure NewImages(Format: TImageFormat; Width, Height: Integer; Fill1, Fill2: Byte);
    { Sets all samples of floating point image to the given value.}
    procedure FillFloat(var Image: TImageData; Value: Single);
    { Checks that the value is within the tolerance (relative, with small
      absolute part for zero).}
    procedure CheckMetric(const Expected, Actual: Double; const Msg: string);
    { Checks that the value is within the tolerance relative to the larger
      of the expected value and Scale.}
    procedure CheckNear(const Expected, Actual, Scale: Double; const Msg: string);
    procedure CheckCount(Expected, Actual: Int64; const Msg: string);
  end;

  TTestErrorMetrics = class(TAnalysisTestCase)
  private
    FPSNR, FMSE, FRMSE, FPAE, FMAE: Double;
    procedure Compute(const Msg: string);
    procedure CheckMetrics(const MSE, RMSE, MAE, PAE, PSNR: Double; const Msg: string);
    procedure CheckIdentical(const Msg: string);
    procedure CheckAgainstModel(const Info: TImageFormatInfo);
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

  TTestImageComparison = class(TAnalysisTestCase)
  private
    procedure CheckChannel(const Difference: TChannelDifference;
      const MSE, MAE, PAE, MeanError, Peak: Double; DiffCount: Int64; const Msg: string);
    procedure CheckAgainstModel(const Info: TImageFormatInfo);
  published
    procedure TestAllFormatsAgainstModel;
    procedure TestOneChannelDiffers;
    procedure TestMeanErrorSign;
    procedure TestTolerance;
    procedure TestIdenticalImages;
    procedure TestUnsupportedImages;
  end;

  TTestCommonFormat = class(TAnalysisTestCase)
  private
    procedure CheckCommon(Expected, Format1, Format2: TImageFormat);
    { Converts test image to the format and checks that all its channels
      are still there.}
    procedure CheckKeepsPixels(const SrcInfo, DstInfo: TImageFormatInfo);
  published
    procedure TestKnownPairs;
    procedure TestSpecialAndInvalidFormats;
    procedure TestAllPairs;
    procedure TestConvertImages;
  end;

  TTestImageStatistics = class(TAnalysisTestCase)
  private
    procedure CheckChannel(const Stats: TImageStatistics; Channel: TColorChannel;
      const Min, Max, Mean, StdDev, Peak: Double; LowCount, HighCount: Int64;
      const Msg: string);
    procedure CheckAgainstModel(const Info: TImageFormatInfo);
  published
    procedure TestAllFormatsAgainstModel;
    procedure TestKnownImage;
    procedure TestClipping;
    procedure TestFloatRange;
    procedure TestSmallVariance;
    procedure TestPackedFormat;
    procedure TestIndexedFormat;
    procedure TestInvalidImages;
  end;

  TTestContentChecks = class(TAnalysisTestCase)
  published
    procedure TestOpaque;
    procedure TestLuminanceOnly;
    procedure TestColorCount;
    procedure TestColorCountAllFormats;
  end;

  TTestHistogram = class(TAnalysisTestCase)
  private
    procedure CheckHistogram(const Expected, Actual: THistogramArray; const Msg: string);
    function SumOf(const Histogram: THistogramArray): Int64;
  published
    procedure TestKnownColors;
    procedure TestAllFormats;
    procedure TestRectangle;
    procedure TestInvalidImages;
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

{ TAnalysisTestCase }

procedure TAnalysisTestCase.SetUp;
begin
  InitImage(FImage1);
  InitImage(FImage2);
end;

procedure TAnalysisTestCase.TearDown;
begin
  FreeImage(FImage1);
  FreeImage(FImage2);
end;

procedure TAnalysisTestCase.NewImages(Format: TImageFormat; Width, Height: Integer;
  Fill1, Fill2: Byte);
begin
  FreeImage(FImage1);
  FreeImage(FImage2);
  CheckTrue(NewImage(Width, Height, Format, FImage1), 'NewImage 1 of ' + GetFormatName(Format));
  CheckTrue(NewImage(Width, Height, Format, FImage2), 'NewImage 2 of ' + GetFormatName(Format));
  FillChar(FImage1.Bits^, FImage1.Size, Fill1);
  FillChar(FImage2.Bits^, FImage2.Size, Fill2);
end;

procedure TAnalysisTestCase.FillFloat(var Image: TImageData; Value: Single);
begin
  FillFloatImage(Image, [Value]);
end;

procedure TAnalysisTestCase.CheckNear(const Expected, Actual, Scale: Double; const Msg: string);
begin
  CheckEquals(Expected, Actual, (Abs(Expected) + Abs(Scale)) * RelTolerance, Msg);
end;

procedure TAnalysisTestCase.CheckCount(Expected, Actual: Int64; const Msg: string);
begin
  if Expected <> Actual then
    Fail(Format('%s: expected: <%d> but was: <%d>', [Msg, Expected, Actual]));
end;

{ TTestErrorMetrics }

procedure TTestErrorMetrics.Compute(const Msg: string);
begin
  CheckTrue(ComputeErrorMetrics(FImage1, FImage2, FPSNR, FMSE, FRMSE, FPAE, FMAE),
    Msg + ': metrics computed');
end;

procedure TAnalysisTestCase.CheckMetric(const Expected, Actual: Double; const Msg: string);
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

{ Model of channels }

const
  ModelChannels: array[TColorChannel] of TPixelChannel = (chBlue, chGreen, chRed, chAlpha);
  ChannelTitles: array[TColorChannel] of string = ('blue', 'green', 'red', 'alpha');

function IsRegularFormat(Format: TImageFormat; out Info: TImageFormatInfo): Boolean;
begin
  Result := (Format <> ifDefault) and GetImageFormatInfo(Format, Info) and not Info.IsSpecial;
end;

{ Returns True when results for the channel are expected for the format:
  luminance is reported as all three color channels, palettes have alpha.}
function IsReportedChannel(const Info: TImageFormatInfo; Channel: TColorChannel): Boolean;
begin
  case Channel of
    ccAlpha: Result := Info.IsIndexed or (ChannelBits(Info, chAlpha) > 0);
    ccRed:   Result := True;
  else
    Result := not IsOneChannelFloat(Info);
  end;
end;

{ Returns the largest value of the channel in units of stored samples.}
function GetModelPeak(const Info: TImageFormatInfo; Channel: TColorChannel): Double;
begin
  if Info.IsFloatingPoint then
    Result := 1.0
  else if Info.IsIndexed then
    Result := PeakUInt8
  else
    Result := Power(2.0, ChannelBits(Info, ModelChannels[Channel])) - 1.0;
end;

function GetStoredChannelCount(const Info: TImageFormatInfo): Integer;
begin
  if Info.IsIndexed then
    Result := 4
  else
    Result := Info.ChannelCount;
end;

function SameIdealPixels(const A, B: TIdealPixel): Boolean;
begin
  Result := (A.A = B.A) and (A.R = B.R) and (A.G = B.G) and (A.B = B.B);
end;

{ Sets pixel of image with 8-bit blue, green, red, and alpha (or unused) bytes.}
procedure SetPixelBytes(var Image: TImageData; Index: Integer; B, G, R, A: Byte);
begin
  PBuffer(Image.Bits)[Index * 4] := B;
  PBuffer(Image.Bits)[Index * 4 + 1] := G;
  PBuffer(Image.Bits)[Index * 4 + 2] := R;
  PBuffer(Image.Bits)[Index * 4 + 3] := A;
end;

procedure SetPixelWord(var Image: TImageData; Index: Integer; Value: Word);
begin
  PWordBuffer(Image.Bits)[Index] := Value;
end;

procedure SetPaletteEntry(var Image: TImageData; Index: Integer; R, G, B, A: Byte);
begin
  Image.Palette[Index].R := R;
  Image.Palette[Index].G := G;
  Image.Palette[Index].B := B;
  Image.Palette[Index].A := A;
end;

{ TTestImageStatistics }

procedure TTestImageStatistics.CheckChannel(const Stats: TImageStatistics;
  Channel: TColorChannel; const Min, Max, Mean, StdDev, Peak: Double;
  LowCount, HighCount: Int64; const Msg: string);
var
  Name: string;
begin
  Name := Msg + ' ' + ChannelTitles[Channel];
  CheckTrue(Stats.HasChannel[Channel], Name + ': channel reported');
  CheckNear(Min, Stats.Channels[Channel].Min, Peak, Name + ': Min');
  CheckNear(Max, Stats.Channels[Channel].Max, Peak, Name + ': Max');
  CheckNear(Mean, Stats.Channels[Channel].Mean, Peak, Name + ': Mean');
  CheckNear(StdDev, Stats.Channels[Channel].StdDev, Peak, Name + ': StdDev');
  CheckNear(Peak, Stats.Channels[Channel].Peak, Peak, Name + ': Peak');
  CheckCount(LowCount, Stats.Channels[Channel].LowCount, Name + ': LowCount');
  CheckCount(HighCount, Stats.Channels[Channel].HighCount, Name + ': HighCount');
end;

procedure TTestImageStatistics.CheckAgainstModel(const Info: TImageFormatInfo);
const
  Width = 13;
  Height = 7;
  PixelCount = Width * Height;
var
  Stats: TImageStatistics;
  Channel: TColorChannel;
  I: Integer;
  Name: string;
  Peak, V, MinValue, MaxValue, Sum, Mean, SumSqr: Double;
  LowCount, HighCount: Int64;
  Values: array[0..PixelCount - 1] of Double;
begin
  Name := string(Info.Name);
  NewImages(Info.Format, Width, Height, 0, 0);
  FillTestImage(FImage1, 3);

  CheckTrue(ComputeImageStatistics(FImage1, Stats), Name + ': statistics computed');
  CheckCount(PixelCount, Stats.PixelCount, Name + ': PixelCount');
  CheckEquals(GetStoredChannelCount(Info), Stats.ChannelCount, Name + ': ChannelCount');

  for Channel := Low(TColorChannel) to High(TColorChannel) do
  begin
    if not IsReportedChannel(Info, Channel) then
    begin
      CheckFalse(Stats.HasChannel[Channel], Name + ' ' + ChannelTitles[Channel] + ': not reported');
      Continue;
    end;

    Peak := GetModelPeak(Info, Channel);
    MinValue := 0;
    MaxValue := 0;
    Sum := 0;
    LowCount := 0;
    HighCount := 0;

    for I := 0 to PixelCount - 1 do
    begin
      // Model has integer channels scaled to 0..1
      V := ReadIdealPixel(FImage1, I).Channels[ModelChannels[Channel]];
      if V <= 0 then
        Inc(LowCount);
      if V >= 1 then
        Inc(HighCount);

      V := V * Peak;
      Values[I] := V;
      Sum := Sum + V;
      if (I = 0) or (V < MinValue) then
        MinValue := V;
      if (I = 0) or (V > MaxValue) then
        MaxValue := V;
    end;

    Mean := Sum / PixelCount;
    SumSqr := 0;
    for I := 0 to PixelCount - 1 do
      SumSqr := SumSqr + Sqr(Values[I] - Mean);

    CheckChannel(Stats, Channel, MinValue, MaxValue, Mean, Sqrt(SumSqr / PixelCount), Peak,
                 LowCount, HighCount, Name);
  end;
end;

procedure TTestImageStatistics.TestAllFormatsAgainstModel;
var
  Fmt: TImageFormat;
  Info: TImageFormatInfo;
  Count: Integer;
begin
  Count := 0;

  for Fmt := Low(TImageFormat) to High(TImageFormat) do
  begin
    if IsRegularFormat(Fmt, Info) then
    begin
      CheckAgainstModel(Info);
      Inc(Count);
    end;
  end;

  // Also packed and indexed formats
  CheckTrue(Count >= 25, 'Number of formats checked: ' + IntToStr(Count));
end;

procedure TTestImageStatistics.TestKnownImage;
var
  Stats: TImageStatistics;
  StdDev: Double;
begin
  NewImages(ifGray8, 4, 1, 0, 0);
  PBuffer(FImage1.Bits)[1] := 10;
  PBuffer(FImage1.Bits)[2] := 20;
  PBuffer(FImage1.Bits)[3] := 255;
  CheckTrue(ComputeImageStatistics(FImage1, Stats), 'Gray8: statistics computed');

  // Mean is 71.25, squared deviations sum up to 45218.75
  StdDev := Sqrt(Ratio(4521875, 400));
  CheckEquals(1, Stats.ChannelCount, 'Gray8: ChannelCount');
  CheckCount(4, Stats.PixelCount, 'Gray8: PixelCount');
  CheckChannel(Stats, ccRed, 0, 255, 71.25, StdDev, 255, 1, 1, 'Gray8');
  // Luminance is reported as all color channels
  CheckChannel(Stats, ccGreen, 0, 255, 71.25, StdDev, 255, 1, 1, 'Gray8');
  CheckChannel(Stats, ccBlue, 0, 255, 71.25, StdDev, 255, 1, 1, 'Gray8');
  CheckFalse(Stats.HasChannel[ccAlpha], 'Gray8: no alpha');

  // Blue 10, 20; green 30, 60; red 100, 0; alpha 255, 255
  NewImages(ifA8R8G8B8, 2, 1, 0, 0);
  SetPixelBytes(FImage1, 0, 10, 30, 100, 255);
  SetPixelBytes(FImage1, 1, 20, 60, 0, 255);
  CheckTrue(ComputeImageStatistics(FImage1, Stats), 'A8R8G8B8: statistics computed');
  CheckEquals(4, Stats.ChannelCount, 'A8R8G8B8: ChannelCount');
  CheckChannel(Stats, ccBlue, 10, 20, 15, 5, 255, 0, 0, 'A8R8G8B8');
  CheckChannel(Stats, ccGreen, 30, 60, 45, 15, 255, 0, 0, 'A8R8G8B8');
  CheckChannel(Stats, ccRed, 0, 100, 50, 50, 255, 1, 0, 'A8R8G8B8');
  CheckChannel(Stats, ccAlpha, 255, 255, 255, 0, 255, 0, 2, 'A8R8G8B8');

  // Red and blue are stored in the other order
  NewImages(ifA16B16G16R16, 1, 1, 0, 0);
  SetPixelWord(FImage1, 0, 1000);
  SetPixelWord(FImage1, 1, 2000);
  SetPixelWord(FImage1, 2, 3000);
  SetPixelWord(FImage1, 3, 65535);
  CheckTrue(ComputeImageStatistics(FImage1, Stats), 'A16B16G16R16: statistics computed');
  CheckChannel(Stats, ccRed, 1000, 1000, 1000, 0, 65535, 0, 0, 'A16B16G16R16');
  CheckChannel(Stats, ccGreen, 2000, 2000, 2000, 0, 65535, 0, 0, 'A16B16G16R16');
  CheckChannel(Stats, ccBlue, 3000, 3000, 3000, 0, 65535, 0, 0, 'A16B16G16R16');
  CheckChannel(Stats, ccAlpha, 65535, 65535, 65535, 0, 65535, 0, 1, 'A16B16G16R16');
end;

procedure TTestImageStatistics.TestClipping;
var
  Stats: TImageStatistics;
begin
  NewImages(ifR8G8B8, 5, 4, 0, 0);
  CheckTrue(ComputeImageStatistics(FImage1, Stats), 'Black: statistics computed');
  CheckChannel(Stats, ccGreen, 0, 0, 0, 0, 255, 20, 0, 'Black');
  CheckFalse(Stats.HasChannel[ccAlpha], 'Black: no alpha');

  NewImages(ifR8G8B8, 5, 4, 255, 0);
  CheckTrue(ComputeImageStatistics(FImage1, Stats), 'White: statistics computed');
  CheckChannel(Stats, ccBlue, 255, 255, 255, 0, 255, 0, 20, 'White');

  NewImages(ifR8G8B8, 5, 4, 77, 0);
  CheckTrue(ComputeImageStatistics(FImage1, Stats), 'Gray: statistics computed');
  CheckChannel(Stats, ccRed, 77, 77, 77, 0, 255, 0, 0, 'Gray');

  // Unused bytes are not samples of any channel
  NewImages(ifX8R8G8B8, 2, 2, 100, 0);
  PBuffer(FImage1.Bits)[3] := 0;
  PBuffer(FImage1.Bits)[7] := 255;
  CheckTrue(ComputeImageStatistics(FImage1, Stats), 'X8R8G8B8: statistics computed');
  CheckEquals(3, Stats.ChannelCount, 'X8R8G8B8: ChannelCount');
  CheckChannel(Stats, ccRed, 100, 100, 100, 0, 255, 0, 0, 'X8R8G8B8');
  CheckFalse(Stats.HasChannel[ccAlpha], 'X8R8G8B8: no alpha');
end;

procedure TTestImageStatistics.TestFloatRange;
var
  Stats: TImageStatistics;
begin
  // Half of the pixels has each value
  NewImages(ifR32F, 8, 2, 0, 0);
  FillFloatImage(FImage1, [-0.5, 2.5]);
  CheckTrue(ComputeImageStatistics(FImage1, Stats), 'R32F: statistics computed');
  CheckEquals(1, Stats.ChannelCount, 'R32F: ChannelCount');
  CheckChannel(Stats, ccRed, -0.5, 2.5, 1, 1.5, 1, 8, 8, 'R32F');
  CheckFalse(Stats.HasChannel[ccGreen], 'R32F: no green');
  CheckFalse(Stats.HasChannel[ccBlue], 'R32F: no blue');

  CheckTrue(ComputeImageStatistics(FImage1, Stats, 4.0), 'R32F peak 4: statistics computed');
  CheckChannel(Stats, ccRed, -0.5, 2.5, 1, 1.5, 4, 8, 0, 'R32F peak 4');

  NewImages(ifA16B16G16R16F, 3, 3, 0, 0);
  FillFloat(FImage1, 0.25);
  CheckTrue(ComputeImageStatistics(FImage1, Stats), 'A16B16G16R16F: statistics computed');
  CheckChannel(Stats, ccAlpha, 0.25, 0.25, 0.25, 0, 1, 0, 0, 'A16B16G16R16F');
  CheckChannel(Stats, ccBlue, 0.25, 0.25, 0.25, 0, 1, 0, 0, 'A16B16G16R16F');
end;

procedure TTestImageStatistics.TestSmallVariance;
var
  Stats: TImageStatistics;
  I: Integer;
begin
  // Large values that differ by one: needs more than the precision of sums
  // of squares would give
  NewImages(ifGray16, 100, 100, 0, 0);
  for I := 0 to 100 * 100 - 1 do
    SetPixelWord(FImage1, I, 60000 + (I and 1));

  CheckTrue(ComputeImageStatistics(FImage1, Stats), 'Gray16: statistics computed');
  CheckChannel(Stats, ccRed, 60000, 60001, 60000.5, 0.5, 65535, 0, 0, 'Gray16');
  CheckEquals(0.5, Stats.Channels[ccRed].StdDev, 1e-9, 'Gray16: StdDev precision');

  NewImages(ifGray32, 100, 100, 0, 0);
  for I := 0 to 100 * 100 - 1 do
    PUInt32Buffer(FImage1.Bits)[I] := UInt32(4000000000) + UInt32(I and 1);

  CheckTrue(ComputeImageStatistics(FImage1, Stats), 'Gray32: statistics computed');
  CheckEquals(0.5, Stats.Channels[ccRed].StdDev, 1e-9, 'Gray32: StdDev precision');
  CheckEquals(4000000000.5, Stats.Channels[ccRed].Mean, 1e-6, 'Gray32: Mean');
end;

procedure TTestImageStatistics.TestPackedFormat;
var
  Stats: TImageStatistics;
begin
  // White and pure green
  NewImages(ifR5G6B5, 2, 1, 0, 0);
  SetPixelWord(FImage1, 0, $FFFF);
  SetPixelWord(FImage1, 1, $07E0);
  CheckTrue(ComputeImageStatistics(FImage1, Stats), 'R5G6B5: statistics computed');
  CheckEquals(3, Stats.ChannelCount, 'R5G6B5: ChannelCount');
  CheckChannel(Stats, ccRed, 0, 31, 15.5, 15.5, 31, 1, 1, 'R5G6B5');
  CheckChannel(Stats, ccGreen, 63, 63, 63, 0, 63, 0, 2, 'R5G6B5');
  CheckChannel(Stats, ccBlue, 0, 31, 15.5, 15.5, 31, 1, 1, 'R5G6B5');
  CheckFalse(Stats.HasChannel[ccAlpha], 'R5G6B5: no alpha');

  NewImages(ifA1R5G5B5, 2, 1, 0, 0);
  SetPixelWord(FImage1, 0, $8000);
  SetPixelWord(FImage1, 1, $7C00);
  CheckTrue(ComputeImageStatistics(FImage1, Stats), 'A1R5G5B5: statistics computed');
  CheckEquals(4, Stats.ChannelCount, 'A1R5G5B5: ChannelCount');
  CheckChannel(Stats, ccAlpha, 0, 1, 0.5, 0.5, 1, 1, 1, 'A1R5G5B5');
  CheckChannel(Stats, ccRed, 0, 31, 15.5, 15.5, 31, 1, 1, 'A1R5G5B5');
  CheckChannel(Stats, ccBlue, 0, 0, 0, 0, 31, 2, 0, 'A1R5G5B5');
end;

procedure TTestImageStatistics.TestIndexedFormat;
var
  Stats: TImageStatistics;
begin
  // Entries that pixels do not use have no effect
  NewImages(ifIndex8, 2, 1, 0, 0);
  FillChar(FImage1.Palette^, 256 * SizeOf(TColor32Rec), $EE);
  SetPaletteEntry(FImage1, 1, 10, 20, 30, 255);
  SetPaletteEntry(FImage1, 2, 50, 20, 0, 0);
  PBuffer(FImage1.Bits)[0] := 1;
  PBuffer(FImage1.Bits)[1] := 2;

  CheckTrue(ComputeImageStatistics(FImage1, Stats), 'Index8: statistics computed');
  CheckEquals(4, Stats.ChannelCount, 'Index8: ChannelCount');
  CheckChannel(Stats, ccRed, 10, 50, 30, 20, 255, 0, 0, 'Index8');
  CheckChannel(Stats, ccGreen, 20, 20, 20, 0, 255, 0, 0, 'Index8');
  CheckChannel(Stats, ccBlue, 0, 30, 15, 15, 255, 1, 0, 'Index8');
  CheckChannel(Stats, ccAlpha, 0, 255, 127.5, 127.5, 255, 1, 1, 'Index8');
end;

procedure TTestImageStatistics.TestInvalidImages;
var
  Stats: TImageStatistics;
  Empty: TImageData;
begin
  InitImage(Empty);
  Stats.PixelCount := 5;
  CheckFalse(ComputeImageStatistics(Empty, Stats), 'Empty image');
  CheckCount(0, Stats.PixelCount, 'Empty image: result zeroed');

  NewImages(ifDXT1, 8, 8, 0, 0);
  CheckFalse(ComputeImageStatistics(FImage1, Stats), 'Special format');

  NewImages(ifR32F, 8, 8, 0, 0);
  CheckFalse(ComputeImageStatistics(FImage1, Stats, 0.0), 'Zero peak of floating point format');
  NewImages(ifGray8, 8, 8, 0, 0);
  CheckTrue(ComputeImageStatistics(FImage1, Stats, 0.0), 'Zero peak is ignored for integer formats');
end;

{ TTestImageComparison }

procedure TTestImageComparison.CheckChannel(const Difference: TChannelDifference;
  const MSE, MAE, PAE, MeanError, Peak: Double; DiffCount: Int64; const Msg: string);
begin
  CheckNear(MSE, Difference.MSE, Peak * Peak, Msg + ': MSE');
  CheckNear(Sqrt(MSE), Difference.RMSE, Peak, Msg + ': RMSE');
  CheckNear(MAE, Difference.MAE, Peak, Msg + ': MAE');
  CheckNear(PAE, Difference.PAE, Peak, Msg + ': PAE');
  CheckNear(MeanError, Difference.MeanError, Peak, Msg + ': MeanError');
  CheckCount(DiffCount, Difference.DiffCount, Msg + ': DiffCount');

  if MSE = 0 then
    CheckTrue(IsInfinite(Difference.PSNR) and (Difference.PSNR > 0), Msg + ': PSNR is infinite')
  else
    CheckNear(20.0 * Log10(Peak / Sqrt(MSE)), Difference.PSNR, 1, Msg + ': PSNR');
end;

procedure TTestImageComparison.CheckAgainstModel(const Info: TImageFormatInfo);
const
  Width = 13;
  Height = 7;
  PixelCount = Width * Height;
  // No difference of test values is exactly at the tolerance
  ToleranceOfPeak: Double = 0.37;
var
  Diff: TImageDifference;
  Channel: TColorChannel;
  I, X, Y, StoredCount: Integer;
  Name: string;
  Pixel1, Pixel2: TIdealPixel;
  Peak, Tolerance, D: Double;
  SumError, SumAbs, SumSqr, MaxDiff: Double;
  AllError, AllAbs, AllSqr, AllMax: Double;
  DiffCount, AllCount, DiffPixelCount: Int64;
  PixelDiffers: array[0..PixelCount - 1] of Boolean;
  Bounds: TRect;
  PSNR, MSE, RMSE, PAE, MAE: Double;
begin
  Name := string(Info.Name);
  NewImages(Info.Format, Width, Height, 0, 0);
  FillTestImage(FImage1, 1);
  FillTestImage(FImage2, 2);

  Peak := GetModelPeak(Info, ccRed);
  Tolerance := Peak * ToleranceOfPeak;
  CheckTrue(CompareImages(FImage1, FImage2, Diff, Tolerance), Name + ': images compared');
  CheckCount(PixelCount, Diff.PixelCount, Name + ': PixelCount');
  CheckEquals(Info.ChannelCount, Diff.ChannelCount, Name + ': ChannelCount');

  FillChar(PixelDiffers, SizeOf(PixelDiffers), 0);
  StoredCount := 0;
  AllError := 0;
  AllAbs := 0;
  AllSqr := 0;
  AllMax := 0;
  AllCount := 0;

  for Channel := Low(TColorChannel) to High(TColorChannel) do
  begin
    if not IsReportedChannel(Info, Channel) then
    begin
      CheckFalse(Diff.HasChannel[Channel], Name + ' ' + ChannelTitles[Channel] + ': not reported');
      Continue;
    end;

    CheckTrue(Diff.HasChannel[Channel], Name + ' ' + ChannelTitles[Channel] + ': reported');
    SumError := 0;
    SumAbs := 0;
    SumSqr := 0;
    MaxDiff := 0;
    DiffCount := 0;

    for I := 0 to PixelCount - 1 do
    begin
      Pixel1 := ReadIdealPixel(FImage1, I);
      Pixel2 := ReadIdealPixel(FImage2, I);
      D := (Pixel2.Channels[ModelChannels[Channel]] - Pixel1.Channels[ModelChannels[Channel]]) * Peak;
      SumError := SumError + D;
      SumAbs := SumAbs + Abs(D);
      SumSqr := SumSqr + D * D;
      if Abs(D) > MaxDiff then
        MaxDiff := Abs(D);

      if Abs(D) > Tolerance then
      begin
        Inc(DiffCount);
        PixelDiffers[I] := True;
      end;
    end;

    CheckChannel(Diff.Channels[Channel], SumSqr / PixelCount, SumAbs / PixelCount, MaxDiff,
                 SumError / PixelCount, Peak, DiffCount, Name + ' ' + ChannelTitles[Channel]);

    // Luminance is reported three times and stored once
    if IsStoredChannel(Info, ModelChannels[Channel]) then
    begin
      Inc(StoredCount);
      AllError := AllError + SumError;
      AllAbs := AllAbs + SumAbs;
      AllSqr := AllSqr + SumSqr;
      AllCount := AllCount + DiffCount;
      if MaxDiff > AllMax then
        AllMax := MaxDiff;
    end;
  end;

  CheckEquals(Info.ChannelCount, StoredCount, Name + ': channels of the model');
  CheckChannel(Diff.All, AllSqr / (PixelCount * StoredCount), AllAbs / (PixelCount * StoredCount),
               AllMax, AllError / (PixelCount * StoredCount), Peak, AllCount, Name + ' all');

  DiffPixelCount := 0;
  Bounds := Rect(Width, Height, 0, 0);

  for I := 0 to PixelCount - 1 do
  begin
    if PixelDiffers[I] then
    begin
      X := I mod Width;
      Y := I div Width;
      Inc(DiffPixelCount);
      if X < Bounds.Left then
        Bounds.Left := X;
      if X + 1 > Bounds.Right then
        Bounds.Right := X + 1;
      if Y < Bounds.Top then
        Bounds.Top := Y;
      if Y + 1 > Bounds.Bottom then
        Bounds.Bottom := Y + 1;
    end;
  end;

  CheckTrue((DiffPixelCount > 0) and (DiffPixelCount < PixelCount),
    Name + ': some pixels are over the tolerance');
  CheckCount(DiffPixelCount, Diff.DiffPixelCount, Name + ': DiffPixelCount');
  CheckEqualsRect(Bounds, Diff.DiffRect, Name + ': DiffRect');

  // The same values as of the simple function
  CheckTrue(ComputeErrorMetrics(FImage1, FImage2, PSNR, MSE, RMSE, PAE, MAE),
    Name + ': metrics computed');
  CheckTrue((PSNR = Diff.All.PSNR) and (MSE = Diff.All.MSE) and (RMSE = Diff.All.RMSE) and
    (PAE = Diff.All.PAE) and (MAE = Diff.All.MAE), Name + ': the same as ComputeErrorMetrics');
end;

procedure TTestImageComparison.TestAllFormatsAgainstModel;
var
  Fmt: TImageFormat;
  Info: TImageFormatInfo;
  Count: Integer;
begin
  Count := 0;

  for Fmt := Low(TImageFormat) to High(TImageFormat) do
  begin
    if IsRegularFormat(Fmt, Info) and IsSupportedFormat(Info) then
    begin
      CheckAgainstModel(Info);
      Inc(Count);
    end;
  end;

  CheckTrue(Count >= 20, 'Number of formats checked: ' + IntToStr(Count));
end;

procedure TTestImageComparison.TestOneChannelDiffers;
var
  Diff: TImageDifference;
begin
  // Alpha of pixel at 1,1 is lower by 10 in the second image
  NewImages(ifA8R8G8B8, 4, 4, 100, 100);
  PBuffer(FImage2.Bits)[5 * 4 + 3] := 90;
  CheckTrue(CompareImages(FImage1, FImage2, Diff), 'Alpha: images compared');

  CheckEquals(4, Diff.ChannelCount, 'Alpha: ChannelCount');
  CheckChannel(Diff.Channels[ccAlpha], Ratio(100, 16), Ratio(10, 16), 10, Ratio(-10, 16), 255,
               1, 'Alpha: alpha');
  CheckChannel(Diff.Channels[ccRed], 0, 0, 0, 0, 255, 0, 'Alpha: red');
  CheckChannel(Diff.Channels[ccGreen], 0, 0, 0, 0, 255, 0, 'Alpha: green');
  CheckChannel(Diff.Channels[ccBlue], 0, 0, 0, 0, 255, 0, 'Alpha: blue');
  CheckChannel(Diff.All, Ratio(100, 64), Ratio(10, 64), 10, Ratio(-10, 64), 255, 1, 'Alpha: all');
  CheckCount(1, Diff.DiffPixelCount, 'Alpha: DiffPixelCount');
  CheckEqualsRect(Rect(1, 1, 2, 2), Diff.DiffRect, 'Alpha: DiffRect');

  // Red is the first stored channel of this format
  NewImages(ifA16B16G16R16, 3, 1, 0, 0);
  SetPixelWord(FImage2, 2 * 4, 300);
  CheckTrue(CompareImages(FImage1, FImage2, Diff), 'Red: images compared');
  CheckChannel(Diff.Channels[ccRed], 30000, 100, 300, 100, 65535, 1, 'Red: red');
  CheckChannel(Diff.Channels[ccBlue], 0, 0, 0, 0, 65535, 0, 'Red: blue');
  CheckEqualsRect(Rect(2, 0, 3, 1), Diff.DiffRect, 'Red: DiffRect');
end;

procedure TTestImageComparison.TestMeanErrorSign;
var
  Diff: TImageDifference;
begin
  NewImages(ifGray8, 6, 5, 10, 13);
  CheckTrue(CompareImages(FImage1, FImage2, Diff), 'Brighter: images compared');
  CheckEquals(1, Diff.ChannelCount, 'Brighter: ChannelCount');
  CheckChannel(Diff.Channels[ccRed], 9, 3, 3, 3, 255, 30, 'Brighter: red');
  // Luminance is reported as all color channels and counted once
  CheckChannel(Diff.Channels[ccGreen], 9, 3, 3, 3, 255, 30, 'Brighter: green');
  CheckChannel(Diff.Channels[ccBlue], 9, 3, 3, 3, 255, 30, 'Brighter: blue');
  CheckFalse(Diff.HasChannel[ccAlpha], 'Brighter: no alpha');
  CheckChannel(Diff.All, 9, 3, 3, 3, 255, 30, 'Brighter: all');

  CheckTrue(CompareImages(FImage2, FImage1, Diff), 'Darker: images compared');
  CheckChannel(Diff.All, 9, 3, 3, -3, 255, 30, 'Darker: all');

  // Differences in both directions cancel out in the mean error only
  NewImages(ifR32F, 2, 1, 0, 0);
  FillFloat(FImage1, 0.5);
  FillFloatImage(FImage2, [0.25, 0.75]);
  CheckTrue(CompareImages(FImage1, FImage2, Diff), 'Both: images compared');
  CheckChannel(Diff.All, 0.0625, 0.25, 0.25, 0, 1, 2, 'Both: all');
end;

procedure TTestImageComparison.TestTolerance;
var
  Diff: TImageDifference;
begin
  // Pixels at 2,3 and 6,5 and 4,1
  NewImages(ifGray8, 8, 8, 100, 100);
  PBuffer(FImage2.Bits)[3 * 8 + 2] := 102;
  PBuffer(FImage2.Bits)[5 * 8 + 6] := 105;
  PBuffer(FImage2.Bits)[1 * 8 + 4] := 97;

  CheckTrue(CompareImages(FImage1, FImage2, Diff), 'Tolerance 0: images compared');
  CheckCount(64, Diff.PixelCount, 'Tolerance 0: PixelCount');
  CheckCount(3, Diff.DiffPixelCount, 'Tolerance 0: DiffPixelCount');
  CheckCount(3, Diff.All.DiffCount, 'Tolerance 0: DiffCount');
  CheckEqualsRect(Rect(2, 1, 7, 6), Diff.DiffRect, 'Tolerance 0: DiffRect');

  // Difference that equals the tolerance is within it
  CheckTrue(CompareImages(FImage1, FImage2, Diff, 2), 'Tolerance 2: images compared');
  CheckCount(2, Diff.DiffPixelCount, 'Tolerance 2: DiffPixelCount');
  CheckEqualsRect(Rect(4, 1, 7, 6), Diff.DiffRect, 'Tolerance 2: DiffRect');
  CheckNear(5, Diff.All.PAE, 255, 'Tolerance 2: PAE is not affected');

  CheckTrue(CompareImages(FImage1, FImage2, Diff, 4.5), 'Tolerance 4.5: images compared');
  CheckCount(1, Diff.DiffPixelCount, 'Tolerance 4.5: DiffPixelCount');
  CheckEqualsRect(Rect(6, 5, 7, 6), Diff.DiffRect, 'Tolerance 4.5: DiffRect');

  CheckTrue(CompareImages(FImage1, FImage2, Diff, 5), 'Tolerance 5: images compared');
  CheckCount(0, Diff.DiffPixelCount, 'Tolerance 5: DiffPixelCount');
  CheckEqualsRect(Rect(0, 0, 0, 0), Diff.DiffRect, 'Tolerance 5: DiffRect');

  CheckFalse(CompareImages(FImage1, FImage2, Diff, -1), 'Negative tolerance');

  // Two channels of one pixel over the tolerance
  NewImages(ifR8G8B8, 3, 3, 50, 50);
  PBuffer(FImage2.Bits)[4 * 3] := 60;
  PBuffer(FImage2.Bits)[4 * 3 + 2] := 40;
  CheckTrue(CompareImages(FImage1, FImage2, Diff, 1), 'Two channels: images compared');
  CheckCount(1, Diff.DiffPixelCount, 'Two channels: DiffPixelCount');
  CheckCount(2, Diff.All.DiffCount, 'Two channels: DiffCount of all');
  CheckCount(1, Diff.Channels[ccBlue].DiffCount, 'Two channels: DiffCount of blue');
  CheckCount(0, Diff.Channels[ccGreen].DiffCount, 'Two channels: DiffCount of green');
  CheckCount(1, Diff.Channels[ccRed].DiffCount, 'Two channels: DiffCount of red');
end;

procedure TTestImageComparison.TestIdenticalImages;
var
  Diff: TImageDifference;
begin
  NewImages(ifA16Gray16, 5, 3, 0, 0);
  FillTestImage(FImage1, 7);
  FillTestImage(FImage2, 7);
  CheckTrue(CompareImages(FImage1, FImage2, Diff), 'Images compared');
  CheckEquals(2, Diff.ChannelCount, 'ChannelCount');
  CheckChannel(Diff.All, 0, 0, 0, 0, 65535, 0, 'All');
  CheckChannel(Diff.Channels[ccAlpha], 0, 0, 0, 0, 65535, 0, 'Alpha');
  CheckChannel(Diff.Channels[ccGreen], 0, 0, 0, 0, 65535, 0, 'Green');
  CheckCount(0, Diff.DiffPixelCount, 'DiffPixelCount');
  CheckEqualsRect(Rect(0, 0, 0, 0), Diff.DiffRect, 'DiffRect');
end;

procedure TTestImageComparison.TestUnsupportedImages;
var
  Diff: TImageDifference;
begin
  NewImages(ifR5G6B5, 4, 4, 0, 0);
  Diff.PixelCount := 5;
  CheckFalse(CompareImages(FImage1, FImage2, Diff), 'Packed format');
  CheckCount(0, Diff.PixelCount, 'Packed format: result zeroed');

  NewImages(ifIndex8, 4, 4, 0, 0);
  CheckFalse(CompareImages(FImage1, FImage2, Diff), 'Indexed format');

  NewImages(ifGray8, 4, 4, 0, 0);
  FreeImage(FImage2);
  NewImage(4, 5, ifGray8, FImage2);
  CheckFalse(CompareImages(FImage1, FImage2, Diff), 'Different height');
end;

{ TTestCommonFormat }

procedure TTestCommonFormat.CheckCommon(Expected, Format1, Format2: TImageFormat);
var
  Name: string;
begin
  Name := GetFormatName(Format1) + ' and ' + GetFormatName(Format2);
  CheckEquals(GetFormatName(Expected), GetFormatName(FindCommonFormat(Format1, Format2)), Name);
  CheckEquals(GetFormatName(Expected), GetFormatName(FindCommonFormat(Format2, Format1)),
    Name + ' swapped');
end;

procedure TTestCommonFormat.TestKnownPairs;
begin
  // Formats that can be compared are kept
  CheckCommon(ifGray8, ifGray8, ifGray8);
  CheckCommon(ifX8R8G8B8, ifX8R8G8B8, ifX8R8G8B8);
  CheckCommon(ifA16B16G16R16, ifA16B16G16R16, ifA16B16G16R16);
  CheckCommon(ifB32G32R32F, ifB32G32R32F, ifB32G32R32F);
  CheckCommon(ifR8G8B8, ifR5G6B5, ifR5G6B5);
  CheckCommon(ifA8R8G8B8, ifA4R4G4B4, ifA4R4G4B4);
  CheckCommon(ifA8R8G8B8, ifIndex8, ifIndex8);

  // Color, alpha, and size of samples of both
  CheckCommon(ifR8G8B8, ifGray8, ifR8G8B8);
  CheckCommon(ifA8Gray8, ifGray8, ifA8Gray8);
  CheckCommon(ifA8R8G8B8, ifA8Gray8, ifR8G8B8);
  CheckCommon(ifR8G8B8, ifX8R8G8B8, ifR8G8B8);
  CheckCommon(ifR8G8B8, ifX8R8G8B8, ifR5G6B5);
  CheckCommon(ifA8R8G8B8, ifA1R5G5B5, ifGray8);
  CheckCommon(ifA8R8G8B8, ifIndex8, ifGray8);
  CheckCommon(ifGray16, ifGray16, ifGray8);
  CheckCommon(ifA16Gray16, ifGray16, ifA8Gray8);
  CheckCommon(ifR16G16B16, ifR8G8B8, ifGray16);
  CheckCommon(ifA16R16G16B16, ifA8R8G8B8, ifGray16);
  CheckCommon(ifA16R16G16B16, ifA16B16G16R16, ifB16G16R16);
  CheckCommon(ifA16R16G16B16, ifIndex8, ifR16G16B16);
  CheckCommon(ifGray32, ifGray32, ifGray8);
  CheckCommon(ifGray64, ifGray64, ifGray32);

  // Floating point samples are always 32-bit ones, unless both images
  // have the same format already
  CheckCommon(ifR16F, ifR16F, ifR16F);
  CheckCommon(ifA16B16G16R16F, ifA16B16G16R16F, ifA16B16G16R16F);
  CheckCommon(ifR32F, ifR16F, ifGray8);
  CheckCommon(ifR32F, ifR16F, ifGray16);
  CheckCommon(ifR32F, ifR16F, ifR32F);
  CheckCommon(ifR32F, ifR32F, ifGray64);
  CheckCommon(ifR32G32B32F, ifR16F, ifR8G8B8);
  CheckCommon(ifR32G32B32F, ifR32F, ifR8G8B8);
  CheckCommon(ifR32G32B32F, ifR16F, ifR16G16B16);
  CheckCommon(ifR32G32B32F, ifR32G32B32F, ifB32G32R32F);
  CheckCommon(ifA32R32G32B32F, ifR16F, ifA8Gray8);
  CheckCommon(ifA32R32G32B32F, ifR32F, ifA8R8G8B8);
  CheckCommon(ifA32R32G32B32F, ifA16B16G16R16F, ifIndex8);
  CheckCommon(ifA32R32G32B32F, ifA16B16G16R16F, ifA16R16G16B16F);
  CheckCommon(ifA32R32G32B32F, ifA16B16G16R16F, ifR16G16B16);
  CheckCommon(ifA32R32G32B32F, ifA16R16G16B16F, ifB32G32R32F);

  // No format holds these, 32-bit floating point keeps the most
  CheckCommon(ifR32G32B32F, ifGray32, ifR8G8B8);
  CheckCommon(ifA32R32G32B32F, ifGray64, ifA8Gray8);
  CheckCommon(ifR32F, ifGray64, ifR16F);
end;

procedure TTestCommonFormat.TestSpecialAndInvalidFormats;
var
  Info: TImageFormatInfo;
begin
  // Special formats are taken as the formats they are decompressed to
  CheckTrue(GetImageFormatInfo(ifDXT5, Info), 'Info of DXT5');
  CheckCommon(FindCommonFormat(Info.SpecialNearestFormat, ifGray8), ifDXT5, ifGray8);
  CheckCommon(FindCommonFormat(Info.SpecialNearestFormat, Info.SpecialNearestFormat), ifDXT5, ifDXT5);
  CheckCommon(ifGray8, ifBinary, ifBinary);
  CheckCommon(ifGray16, ifBinary, ifGray16);

  CheckCommon(ifUnknown, ifUnknown, ifGray8);
  CheckCommon(ifUnknown, ifUnknown, ifUnknown);
  CheckCommon(ifUnknown, ifLast, ifA8R8G8B8);
end;

procedure TTestCommonFormat.CheckKeepsPixels(const SrcInfo, DstInfo: TImageFormatInfo);
const
  Width = 13;
  Height = 7;
var
  I: Integer;
  Channel: TColorChannel;
  ModelChannel: TPixelChannel;
  SrcPixel, DstPixel: TIdealPixel;
  Name: string;
  Delta: Double;
begin
  Name := string(SrcInfo.Name) + ' in ' + string(DstInfo.Name);
  NewImages(SrcInfo.Format, Width, Height, 0, 0);
  FillTestImage(FImage1, 8);
  CheckTrue(CloneImage(FImage1, FImage2), Name + ': image cloned');
  CheckTrue(ConvertImage(FImage2, DstInfo.Format), Name + ': image converted');

  for I := 0 to Width * Height - 1 do
  begin
    SrcPixel := ReadIdealPixel(FImage1, I);
    DstPixel := ReadIdealPixel(FImage2, I);

    for Channel := Low(TColorChannel) to High(TColorChannel) do
    begin
      ModelChannel := ModelChannels[Channel];

      if not IsReportedChannel(SrcInfo, Channel) then
      begin
        // Alpha that was not there is opaque
        if (Channel = ccAlpha) and IsReportedChannel(DstInfo, Channel) then
          CheckEquals(1.0, DstPixel.A, 1e-9, Format('%s: alpha of pixel %d', [Name, I]));
        Continue;
      end;

      // Luminance is in the red channel of formats with one channel
      if not IsReportedChannel(DstInfo, Channel) then
      begin
        CheckTrue((Channel <> ccAlpha) and (Channel <> ccRed) and SrcInfo.HasGrayChannel,
          Format('%s: %s is kept', [Name, ChannelTitles[Channel]]));
        Continue;
      end;

      // The value must still be closer to the source value than to its neighbors
      Delta := ChannelStep(SrcInfo, ModelChannel, SrcPixel.Channels[ModelChannel]) * 0.49;
      CheckEquals(SrcPixel.Channels[ModelChannel], DstPixel.Channels[ModelChannel], Delta,
        Format('%s: %s of pixel %d', [Name, ChannelTitles[Channel], I]));
    end;
  end;
end;

procedure TTestCommonFormat.TestAllPairs;
var
  Fmt1, Fmt2, Common: TImageFormat;
  Info1, Info2, CommonInfo: TImageFormatInfo;
  Name: string;
  Count: Integer;
begin
  Count := 0;

  for Fmt1 := Low(TImageFormat) to High(TImageFormat) do
  begin
    if not IsRegularFormat(Fmt1, Info1) then
      Continue;

    for Fmt2 := Low(TImageFormat) to High(TImageFormat) do
    begin
      if not IsRegularFormat(Fmt2, Info2) then
        Continue;

      Name := string(Info1.Name) + ' and ' + string(Info2.Name);
      Common := FindCommonFormat(Fmt1, Fmt2);
      CheckTrue(IsRegularFormat(Common, CommonInfo) and IsSupportedFormat(CommonInfo),
        Name + ': can be compared in ' + GetFormatName(Common));
      CheckTrue(Common = FindCommonFormat(Fmt2, Fmt1), Name + ': the same when swapped');
      if (Fmt1 = Fmt2) and IsSupportedFormat(Info1) then
        CheckTrue(Common = Fmt1, Name + ': format is kept');

      // 32 and 64-bit luminance does not fit floating point samples
      if (ChannelBits(Info1, chRed) <= 16) or Info1.IsFloatingPoint or
        not CommonInfo.IsFloatingPoint then
      begin
        CheckKeepsPixels(Info1, CommonInfo);
      end;

      Inc(Count);
    end;
  end;

  CheckTrue(Count >= 25 * 25, 'Number of pairs checked: ' + IntToStr(Count));
end;

procedure TTestCommonFormat.TestConvertImages;
var
  Common1, Common2, Empty: TImageData;
  Diff: TImageDifference;
  I: Integer;
begin
  InitImage(Common1);
  InitImage(Common2);
  InitImage(Empty);
  try
    // Luminance 100 with alpha 200 and color of the same luminance
    NewImages(ifA8Gray8, 5, 3, 100, 0);
    for I := 0 to 5 * 3 - 1 do
      PBuffer(FImage1.Bits)[I * 2 + 1] := 200;
    FreeImage(FImage2);
    CheckTrue(NewImage(5, 3, ifR5G6B5, FImage2), 'NewImage of R5G6B5');
    FillChar(FImage2.Bits^, FImage2.Size, $FF);

    CheckTrue(ConvertToCommonFormat(FImage1, FImage2, Common1, Common2), 'Images converted');
    CheckTrue((FImage1.Format = ifA8Gray8) and (FImage2.Format = ifR5G6B5), 'Sources are kept');
    CheckTrue((Common1.Format = ifA8R8G8B8) and (Common2.Format = ifA8R8G8B8), 'Common format');
    CheckTrue((Common1.Bits <> FImage1.Bits) and (Common2.Bits <> FImage2.Bits), 'Copies');
    CheckEqualsColor32($C8646464, GetPixel32(Common1, 4, 2).Color, 'Pixel of the first image');
    CheckEqualsColor32($FFFFFFFF, GetPixel32(Common2, 4, 2).Color, 'Pixel of the second image');

    CheckFalse(CompareImages(FImage1, FImage2, Diff), 'Sources cannot be compared');
    CheckTrue(CompareImages(Common1, Common2, Diff), 'Copies can be compared');
    CheckCount(15, Diff.DiffPixelCount, 'DiffPixelCount');
    CheckEquals(155.0, Diff.Channels[ccRed].MeanError, 1e-9, 'MeanError of red');
    CheckEquals(55.0, Diff.Channels[ccAlpha].MeanError, 1e-9, 'MeanError of alpha');

    // Images held by the results are replaced, the same format is only copied
    NewImages(ifGray16, 4, 4, 7, 9);
    CheckTrue(ConvertToCommonFormat(FImage1, FImage2, Common1, Common2), 'Gray16: images converted');
    CheckTrue((Common1.Format = ifGray16) and (Common2.Format = ifGray16) and
      (Common1.Width = 4), 'Gray16: common format');
    CheckImagesEqual(FImage1, Common1, 'Gray16: the first image');
    CheckImagesEqual(FImage2, Common2, 'Gray16: the second image');
    CheckTrue(Common1.Bits <> FImage1.Bits, 'Gray16: copy');

    // Special format is decompressed, dimensions do not have to be the same
    NewImages(ifGray8, 8, 8, 0, 0);
    FillTestImage(FImage1, 2);
    CheckTrue(ConvertImage(FImage1, ifDXT1), 'Conversion to DXT1');
    FreeImage(FImage2);
    CheckTrue(NewImage(3, 2, ifR32F, FImage2), 'NewImage of R32F');
    CheckTrue(ConvertToCommonFormat(FImage1, FImage2, Common1, Common2), 'DXT1: images converted');
    CheckTrue(FImage1.Format = ifDXT1, 'DXT1: source is kept');
    CheckTrue((Common1.Format = ifA32R32G32B32F) and (Common2.Format = ifA32R32G32B32F),
      'DXT1: common format');
    CheckTrue((Common1.Width = 8) and (Common2.Width = 3), 'DXT1: dimensions');

    CheckFalse(ConvertToCommonFormat(FImage1, Empty, Common1, Common2), 'Empty image');
    CheckTrue((Common1.Bits = nil) and (Common2.Bits = nil), 'Empty image: results are freed');
    CheckFalse(ConvertToCommonFormat(Empty, FImage1, Common1, Common2), 'Empty first image');
  finally
    FreeImage(Common1);
    FreeImage(Common2);
  end;
end;

{ TTestContentChecks }

procedure TTestContentChecks.TestOpaque;
var
  Empty: TImageData;
begin
  NewImages(ifR8G8B8, 4, 4, 10, 0);
  CheckTrue(IsImageOpaque(FImage1), 'R8G8B8: no alpha channel');

  NewImages(ifA8R8G8B8, 4, 4, 255, 0);
  CheckTrue(IsImageOpaque(FImage1), 'A8R8G8B8: opaque');
  PBuffer(FImage1.Bits)[15 * 4 + 3] := 254;
  CheckFalse(IsImageOpaque(FImage1), 'A8R8G8B8: alpha of the last pixel');
  CheckFalse(IsImageOpaque(FImage2), 'A8R8G8B8: transparent');

  // Unused byte is not alpha
  NewImages(ifX8R8G8B8, 4, 4, 0, 0);
  CheckTrue(IsImageOpaque(FImage1), 'X8R8G8B8: no alpha channel');

  NewImages(ifA16Gray16, 4, 4, 255, 0);
  CheckTrue(IsImageOpaque(FImage1), 'A16Gray16: opaque');
  SetPixelWord(FImage1, 2 * 2 + 1, $FFFE);
  CheckFalse(IsImageOpaque(FImage1), 'A16Gray16: alpha of one pixel');
  SetPixelWord(FImage1, 2 * 2 + 1, $FFFF);
  SetPixelWord(FImage1, 2 * 2, 0);
  CheckTrue(IsImageOpaque(FImage1), 'A16Gray16: luminance does not matter');

  // Alpha is the last of four values
  NewImages(ifA32B32G32R32F, 2, 2, 0, 0);
  FillFloat(FImage1, 1.0);
  CheckTrue(IsImageOpaque(FImage1), 'A32B32G32R32F: opaque');
  PSingle(@PBuffer(FImage1.Bits)[12])^ := 2.0;
  CheckTrue(IsImageOpaque(FImage1), 'A32B32G32R32F: alpha over one');
  PSingle(@PBuffer(FImage1.Bits)[12])^ := 0.5;
  CheckFalse(IsImageOpaque(FImage1), 'A32B32G32R32F: alpha under one');
  PSingle(@PBuffer(FImage1.Bits)[12])^ := 1.0;
  PSingle(@PBuffer(FImage1.Bits)[8])^ := 0.5;
  CheckTrue(IsImageOpaque(FImage1), 'A32B32G32R32F: color does not matter');

  NewImages(ifA1R5G5B5, 2, 2, 255, 0);
  CheckTrue(IsImageOpaque(FImage1), 'A1R5G5B5: opaque');
  SetPixelWord(FImage1, 3, $7FFF);
  CheckFalse(IsImageOpaque(FImage1), 'A1R5G5B5: alpha of one pixel');
  NewImages(ifX1R5G5B5, 2, 2, 0, 0);
  CheckTrue(IsImageOpaque(FImage1), 'X1R5G5B5: no alpha channel');

  // Only alpha of palette entries that pixels use matters
  NewImages(ifIndex8, 2, 2, 3, 0);
  FillChar(FImage1.Palette^, 256 * SizeOf(TColor32Rec), 0);
  SetPaletteEntry(FImage1, 3, 10, 20, 30, 255);
  CheckTrue(IsImageOpaque(FImage1), 'Index8: opaque');
  PBuffer(FImage1.Bits)[2] := 4;
  CheckFalse(IsImageOpaque(FImage1), 'Index8: transparent entry used');

  InitImage(Empty);
  CheckFalse(IsImageOpaque(Empty), 'Empty image');
  NewImages(ifDXT5, 8, 8, 255, 0);
  CheckFalse(IsImageOpaque(FImage1), 'Special format');
end;

procedure TTestContentChecks.TestLuminanceOnly;
var
  Empty: TImageData;
begin
  NewImages(ifGray8, 4, 4, 0, 0);
  FillTestImage(FImage1, 1);
  CheckTrue(IsImageLuminanceOnly(FImage1), 'Gray8: luminance format');
  NewImages(ifA16Gray16, 4, 4, 0, 0);
  FillTestImage(FImage1, 1);
  CheckTrue(IsImageLuminanceOnly(FImage1), 'A16Gray16: luminance format');
  NewImages(ifR32F, 4, 4, 0, 0);
  FillTestImage(FImage1, 1);
  CheckTrue(IsImageLuminanceOnly(FImage1), 'R32F: one channel');

  NewImages(ifR8G8B8, 4, 4, 77, 0);
  CheckTrue(IsImageLuminanceOnly(FImage1), 'R8G8B8: the same channels');
  PBuffer(FImage1.Bits)[15 * 3 + 1] := 78;
  CheckFalse(IsImageLuminanceOnly(FImage1), 'R8G8B8: green of the last pixel');
  PBuffer(FImage1.Bits)[15 * 3 + 1] := 77;
  PBuffer(FImage1.Bits)[0] := 76;
  CheckFalse(IsImageLuminanceOnly(FImage1), 'R8G8B8: blue of the first pixel');

  // Alpha and unused bytes do not matter
  NewImages(ifA8R8G8B8, 2, 2, 50, 0);
  PBuffer(FImage1.Bits)[3] := 200;
  CheckTrue(IsImageLuminanceOnly(FImage1), 'A8R8G8B8: alpha differs');
  PBuffer(FImage1.Bits)[2] := 200;
  CheckFalse(IsImageLuminanceOnly(FImage1), 'A8R8G8B8: red differs');
  NewImages(ifX8R8G8B8, 2, 2, 50, 0);
  PBuffer(FImage1.Bits)[7] := 200;
  CheckTrue(IsImageLuminanceOnly(FImage1), 'X8R8G8B8: unused byte differs');

  NewImages(ifA16B16G16R16, 2, 2, 50, 0);
  CheckTrue(IsImageLuminanceOnly(FImage1), 'A16B16G16R16: the same channels');
  SetPixelWord(FImage1, 2, 1);
  CheckFalse(IsImageLuminanceOnly(FImage1), 'A16B16G16R16: blue differs');

  NewImages(ifR32G32B32F, 2, 2, 0, 0);
  FillFloat(FImage1, 0.3);
  CheckTrue(IsImageLuminanceOnly(FImage1), 'R32G32B32F: the same channels');
  PSingle(@PBuffer(FImage1.Bits)[4])^ := 0.31;
  CheckFalse(IsImageLuminanceOnly(FImage1), 'R32G32B32F: green differs');

  // White, black, and red
  NewImages(ifR5G6B5, 2, 1, 255, 0);
  SetPixelWord(FImage1, 1, 0);
  CheckTrue(IsImageLuminanceOnly(FImage1), 'R5G6B5: white and black');
  SetPixelWord(FImage1, 1, $F800);
  CheckFalse(IsImageLuminanceOnly(FImage1), 'R5G6B5: red');

  // Only palette entries that pixels use matter
  NewImages(ifIndex8, 2, 2, 3, 0);
  FillChar(FImage1.Palette^, 256 * SizeOf(TColor32Rec), 0);
  SetPaletteEntry(FImage1, 3, 90, 90, 90, 10);
  SetPaletteEntry(FImage1, 4, 10, 20, 30, 255);
  CheckTrue(IsImageLuminanceOnly(FImage1), 'Index8: entry without color');
  PBuffer(FImage1.Bits)[1] := 4;
  CheckFalse(IsImageLuminanceOnly(FImage1), 'Index8: entry with color used');

  InitImage(Empty);
  CheckFalse(IsImageLuminanceOnly(Empty), 'Empty image');
  NewImages(ifDXT1, 8, 8, 0, 0);
  CheckFalse(IsImageLuminanceOnly(FImage1), 'Special format');
end;

procedure TTestContentChecks.TestColorCount;
var
  I: Integer;
  Empty: TImageData;
begin
  NewImages(ifGray8, 4, 4, 5, 0);
  CheckCount(1, CountImageColors(FImage1), 'Gray8: one value');
  PBuffer(FImage1.Bits)[3] := 6;
  PBuffer(FImage1.Bits)[9] := 6;
  PBuffer(FImage1.Bits)[15] := 255;
  CheckCount(3, CountImageColors(FImage1), 'Gray8: three values');

  // Alpha is a part of the color
  NewImages(ifA8Gray8, 4, 4, 5, 0);
  PBuffer(FImage1.Bits)[1] := 6;
  PBuffer(FImage1.Bits)[2] := 6;
  CheckCount(3, CountImageColors(FImage1), 'A8Gray8: alpha and luminance');

  NewImages(ifGray16, 4, 4, 5, 0);
  SetPixelWord(FImage1, 7, $0506);
  SetPixelWord(FImage1, 8, $0605);
  CheckCount(3, CountImageColors(FImage1), 'Gray16');

  // All pixels have different colors, more than the first size of the table
  NewImages(ifR8G8B8, 40, 40, 0, 0);
  for I := 0 to 40 * 40 - 1 do
  begin
    PBuffer(FImage1.Bits)[I * 3] := I and $FF;
    PBuffer(FImage1.Bits)[I * 3 + 1] := I shr 8;
  end;

  CheckCount(1600, CountImageColors(FImage1), 'R8G8B8: all different');
  CheckCount(1600, CountImageColors(FImage1, 1600), 'R8G8B8: at the limit');
  CheckCount(257, CountImageColors(FImage1, 256), 'R8G8B8: over the limit');
  CheckCount(2, CountImageColors(FImage1, 1), 'R8G8B8: over the limit of one');

  // Ten colors repeated
  for I := 0 to 40 * 40 - 1 do
  begin
    PBuffer(FImage1.Bits)[I * 3] := I mod 10;
    PBuffer(FImage1.Bits)[I * 3 + 1] := 0;
    PBuffer(FImage1.Bits)[I * 3 + 2] := (I mod 10) * 20;
  end;

  CheckCount(10, CountImageColors(FImage1), 'R8G8B8: ten colors');
  CheckCount(10, CountImageColors(FImage1, 256), 'R8G8B8: under the limit');

  // Unused bytes and bits are not a part of the color
  NewImages(ifX8R8G8B8, 4, 4, 9, 0);
  for I := 0 to 15 do
    PBuffer(FImage1.Bits)[I * 4 + 3] := I;
  CheckCount(1, CountImageColors(FImage1), 'X8R8G8B8: unused bytes differ');
  PBuffer(FImage1.Bits)[5 * 4 + 2] := 10;
  CheckCount(2, CountImageColors(FImage1), 'X8R8G8B8: red differs');

  NewImages(ifX1R5G5B5, 4, 1, 0, 0);
  SetPixelWord(FImage1, 0, $8000);
  SetPixelWord(FImage1, 2, $0001);
  CheckCount(2, CountImageColors(FImage1), 'X1R5G5B5: unused bit differs');
  NewImages(ifA1R5G5B5, 4, 1, 0, 0);
  SetPixelWord(FImage1, 0, $8000);
  SetPixelWord(FImage1, 2, $0001);
  CheckCount(3, CountImageColors(FImage1), 'A1R5G5B5: alpha bit differs');

  NewImages(ifA32B32G32R32F, 3, 3, 0, 0);
  FillFloat(FImage1, 0.5);
  CheckCount(1, CountImageColors(FImage1), 'A32B32G32R32F: one color');
  PSingle(@PBuffer(FImage1.Bits)[4 * 16 + 12])^ := 0.25;
  PSingle(@PBuffer(FImage1.Bits)[7 * 16])^ := 0.75;
  CheckCount(3, CountImageColors(FImage1), 'A32B32G32R32F: three colors');

  // Colors of palette entries are counted, not the indices
  NewImages(ifIndex8, 4, 1, 0, 0);
  FillChar(FImage1.Palette^, 256 * SizeOf(TColor32Rec), 0);
  SetPaletteEntry(FImage1, 1, 10, 20, 30, 255);
  SetPaletteEntry(FImage1, 2, 10, 20, 30, 255);
  SetPaletteEntry(FImage1, 3, 10, 20, 30, 254);
  PBuffer(FImage1.Bits)[0] := 1;
  PBuffer(FImage1.Bits)[1] := 2;
  PBuffer(FImage1.Bits)[2] := 3;
  PBuffer(FImage1.Bits)[3] := 1;
  CheckCount(2, CountImageColors(FImage1), 'Index8: entries with the same color');

  InitImage(Empty);
  CheckCount(-1, CountImageColors(Empty), 'Empty image');
  NewImages(ifDXT1, 8, 8, 0, 0);
  CheckCount(-1, CountImageColors(FImage1), 'Special format');
end;

procedure TTestContentChecks.TestColorCountAllFormats;
const
  Width = 13;
  Height = 7;
  PixelCount = Width * Height;
var
  Fmt: TImageFormat;
  Info: TImageFormatInfo;
  I, J, Expected, Count: Integer;
  Pixels: array[0..PixelCount - 1] of TIdealPixel;
  IsNew: Boolean;
begin
  Count := 0;

  for Fmt := Low(TImageFormat) to High(TImageFormat) do
  begin
    if not IsRegularFormat(Fmt, Info) then
      Continue;

    // Some of the pixels are repeated
    NewImages(Fmt, Width, Height, 0, 0);
    FillTestImage(FImage1, 5);
    Move(FImage1.Bits^, PBuffer(FImage1.Bits)[40 * Info.BytesPerPixel], 30 * Info.BytesPerPixel);
    Expected := 0;

    for I := 0 to PixelCount - 1 do
    begin
      Pixels[I] := ReadIdealPixel(FImage1, I);
      IsNew := True;
      for J := 0 to I - 1 do
        if SameIdealPixels(Pixels[I], Pixels[J]) then
          IsNew := False;

      if IsNew then
        Inc(Expected);
    end;

    CheckTrue(Expected < PixelCount, string(Info.Name) + ': some colors are repeated');
    CheckCount(Expected, CountImageColors(FImage1), string(Info.Name) + ': number of colors');
    // Some formats cannot have more colors than the limit
    if Expected > 10 then
      Expected := 11;
    CheckCount(Expected, CountImageColors(FImage1, 10), string(Info.Name) + ': with limit');
    Inc(Count);
  end;

  CheckTrue(Count >= 25, 'Number of formats checked: ' + IntToStr(Count));
end;

{ TTestHistogram }

procedure TTestHistogram.CheckHistogram(const Expected, Actual: THistogramArray;
  const Msg: string);
var
  I: Integer;
begin
  for I := 0 to 255 do
    CheckCount(Expected[I], Actual[I], Format('%s[%d]', [Msg, I]));
end;

function TTestHistogram.SumOf(const Histogram: THistogramArray): Int64;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to 255 do
    Result := Result + Histogram[I];
end;

procedure TTestHistogram.TestKnownColors;
var
  Red, Green, Blue, Alpha, Luma: THistogramArray;
begin
  // White, transparent black, red, green, and blue
  NewImages(ifA8R8G8B8, 5, 1, 0, 0);
  SetPixelBytes(FImage1, 0, 255, 255, 255, 255);
  SetPixelBytes(FImage1, 1, 0, 0, 0, 0);
  SetPixelBytes(FImage1, 2, 0, 0, 255, 255);
  SetPixelBytes(FImage1, 3, 0, 255, 0, 255);
  SetPixelBytes(FImage1, 4, 255, 0, 0, 255);
  CheckTrue(ComputeHistogram(FImage1, Red, Green, Blue, Alpha, Luma), 'Histogram computed');

  CheckCount(2, Red[255], 'Red[255]');
  CheckCount(3, Red[0], 'Red[0]');
  CheckCount(2, Green[255], 'Green[255]');
  CheckCount(3, Green[0], 'Green[0]');
  CheckCount(2, Blue[255], 'Blue[255]');
  CheckCount(3, Blue[0], 'Blue[0]');
  CheckCount(4, Alpha[255], 'Alpha[255]');
  CheckCount(1, Alpha[0], 'Alpha[0]');
  CheckCount(5, SumOf(Red), 'Sum of red');
  CheckCount(5, SumOf(Alpha), 'Sum of alpha');

  // Luma weights are 0.299, 0.587, and 0.114
  CheckCount(1, Luma[255], 'Luma of white');
  CheckCount(1, Luma[0], 'Luma of black');
  CheckCount(1, Luma[76], 'Luma of red');
  CheckCount(1, Luma[150], 'Luma of green');
  CheckCount(1, Luma[29], 'Luma of blue');
  CheckCount(5, SumOf(Luma), 'Sum of luma');
end;

procedure TTestHistogram.TestAllFormats;
const
  Width = 13;
  Height = 7;
var
  Fmt: TImageFormat;
  Info: TImageFormatInfo;
  X, Y, Count: Integer;
  Color: TColor32Rec;
  Red, Green, Blue, Alpha, Luma: THistogramArray;
  ExpRed, ExpGreen, ExpBlue, ExpAlpha, ExpLuma: THistogramArray;
  Name: string;
begin
  Count := 0;

  for Fmt := Low(TImageFormat) to High(TImageFormat) do
  begin
    if not IsRegularFormat(Fmt, Info) then
      Continue;

    Name := string(Info.Name);
    NewImages(Fmt, Width, Height, 0, 0);
    FillTestImage(FImage1, 4);
    FillChar(ExpRed, SizeOf(ExpRed), 0);
    FillChar(ExpGreen, SizeOf(ExpGreen), 0);
    FillChar(ExpBlue, SizeOf(ExpBlue), 0);
    FillChar(ExpAlpha, SizeOf(ExpAlpha), 0);
    FillChar(ExpLuma, SizeOf(ExpLuma), 0);

    // Conversion to 8 bits is the one of pixel access functions
    for Y := 0 to Height - 1 do
    begin
      for X := 0 to Width - 1 do
      begin
        Color := GetPixel32(FImage1, X, Y);
        Inc(ExpRed[Color.R]);
        Inc(ExpGreen[Color.G]);
        Inc(ExpBlue[Color.B]);
        Inc(ExpAlpha[Color.A]);
        Inc(ExpLuma[Round(GrayConv.R * Color.R + GrayConv.G * Color.G + GrayConv.B * Color.B)]);
      end;
    end;

    CheckTrue(ComputeHistogram(FImage1, Red, Green, Blue, Alpha, Luma), Name + ': histogram computed');
    CheckHistogram(ExpRed, Red, Name + ': Red');
    CheckHistogram(ExpGreen, Green, Name + ': Green');
    CheckHistogram(ExpBlue, Blue, Name + ': Blue');
    CheckHistogram(ExpAlpha, Alpha, Name + ': Alpha');
    CheckHistogram(ExpLuma, Luma, Name + ': Luma');
    CheckCount(Width * Height, SumOf(Luma), Name + ': sum of luma');
    Inc(Count);
  end;

  CheckTrue(Count >= 25, 'Number of formats checked: ' + IntToStr(Count));
end;

procedure TTestHistogram.TestRectangle;
var
  I: Integer;
  Red, Green, Blue, Alpha, Luma: THistogramArray;
begin
  // Value of pixel is its index
  NewImages(ifGray8, 8, 8, 0, 0);
  for I := 0 to 63 do
    PBuffer(FImage1.Bits)[I] := I;

  CheckTrue(ComputeHistogram(FImage1, Rect(2, 1, 5, 3), Red, Green, Blue, Alpha, Luma),
    'Rectangle: histogram computed');
  CheckCount(6, SumOf(Red), 'Rectangle: sum of red');
  CheckCount(6, SumOf(Luma), 'Rectangle: sum of luma');
  CheckCount(6, Alpha[255], 'Rectangle: alpha');

  for I := 0 to 63 do
  begin
    if (I mod 8 >= 2) and (I mod 8 < 5) and (I div 8 >= 1) and (I div 8 < 3) then
      CheckCount(1, Green[I], Format('Rectangle: Green[%d]', [I]))
    else
      CheckCount(0, Green[I], Format('Rectangle: Green[%d]', [I]));
  end;

  CheckTrue(ComputeHistogram(FImage1, Rect(-5, -5, 100, 100), Red, Green, Blue, Alpha, Luma),
    'Clipped: histogram computed');
  CheckCount(64, SumOf(Blue), 'Clipped: sum of blue');
  CheckCount(1, Blue[63], 'Clipped: last pixel');

  CheckTrue(ComputeHistogram(FImage1, Rect(6, 7, 100, 100), Red, Green, Blue, Alpha, Luma),
    'Corner: histogram computed');
  CheckCount(2, SumOf(Red), 'Corner: sum of red');
  CheckCount(1, Red[62], 'Corner: Red[62]');
  CheckCount(1, Red[63], 'Corner: Red[63]');

  CheckTrue(ComputeHistogram(FImage1, Rect(3, 3, 3, 6), Red, Green, Blue, Alpha, Luma),
    'Empty: histogram computed');
  CheckCount(0, SumOf(Red), 'Empty: sum of red');
  CheckTrue(ComputeHistogram(FImage1, Rect(9, 9, 12, 12), Red, Green, Blue, Alpha, Luma),
    'Outside: histogram computed');
  CheckCount(0, SumOf(Red), 'Outside: sum of red');
end;

procedure TTestHistogram.TestInvalidImages;
var
  Empty: TImageData;
  Red, Green, Blue, Alpha, Luma: THistogramArray;
begin
  InitImage(Empty);
  Red[7] := 5;
  CheckFalse(ComputeHistogram(Empty, Red, Green, Blue, Alpha, Luma), 'Empty image');
  CheckCount(0, Red[7], 'Empty image: result zeroed');

  NewImages(ifDXT1, 8, 8, 0, 0);
  CheckFalse(ComputeHistogram(FImage1, Red, Green, Blue, Alpha, Luma), 'Special format');
end;

initialization
  RegisterImagingTest(TTestErrorMetrics);
  RegisterImagingTest(TTestImageComparison);
  RegisterImagingTest(TTestCommonFormat);
  RegisterImagingTest(TTestImageStatistics);
  RegisterImagingTest(TTestContentChecks);
  RegisterImagingTest(TTestHistogram);
end.
