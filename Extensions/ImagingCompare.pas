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

{
  This unit contains various image comparing functions and image difference
  computations.
}
unit ImagingCompare;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, Math, ImagingTypes, Imaging, ImagingFormats, ImagingUtility;

{ Computes error metrics of two images with the same dimensions and data format:
    MSE  - mean squared error,
    RMSE - root of mean squared error,
    MAE  - mean absolute error,
    PAE  - peak absolute error (the largest difference of two samples),
    PSNR - peak signal to noise ratio in dB, infinity for identical images.

  Errors are in units of stored samples (0..255 for 8-bit channels, 0..65535
  for 16-bit ones, the stored value for floating point channels) and are
  computed over samples of all channels together, alpha has the same weight
  as color channels. Unused bytes of pixels (X8R8G8B8) are skipped.

  Peak of PSNR is the largest value sample of integer formats can hold,
  for floating point formats it is given by FloatPeak. Floating point samples
  must be finite numbers, unless they are stored identically in both images.

  All formats with channels stored as whole 1, 2, 4, or 8 byte samples are
  supported. Returns False and zero metrics for other formats (indexed,
  special, and packed ones like R5G6B5), for invalid images, and for images
  that differ in dimensions or format.}
function ComputeErrorMetrics(const Image1, Image2: TImageData;
  var PSNR, MSE, RMSE, PAE, MAE: Double; const FloatPeak: Double = 1.0): Boolean;

implementation

type
  TSampleKind = (skUInt8, skUInt16, skUInt32, skUInt64, skHalf, skSingle);

const
  // Typed so the math is done in double precision by all compilers
  Ln10: Double = 2.302585092994046;
  TwoPow32: Double = 4294967296.0;
  PeakUInt8: Double = 255.0;
  PeakUInt16: Double = 65535.0;
  PeakUInt32: Double = 4294967295.0;
  PeakUInt64: Double = 18446744073709551615.0;

{ Finds out how channels of the format are stored. Returns False for formats
  that are not made of whole samples of the same size.}
function GetSampleKind(const Info: TImageFormatInfo; out Kind: TSampleKind;
  out SampleSize: Integer): Boolean;
begin
  Result := False;
  Kind := skUInt8;
  SampleSize := 0;

  if Info.IsIndexed or Info.IsSpecial or Info.UsePixelFormat or (Info.ChannelCount = 0) then
    Exit;

  // Pixels can have unused bytes after the channels (X8R8G8B8)
  SampleSize := Info.BytesPerPixel div Info.ChannelCount;

  if Info.IsFloatingPoint then
  begin
    case SampleSize of
      2: Kind := skHalf;
      4: Kind := skSingle;
    else
      Exit;
    end;
  end
  else
  begin
    case SampleSize of
      1: Kind := skUInt8;
      2: Kind := skUInt16;
      4: Kind := skUInt32;
      8: Kind := skUInt64;
    else
      Exit;
    end;
  end;

  Result := True;
end;

function ComputeErrorMetrics(const Image1, Image2: TImageData;
  var PSNR, MSE, RMSE, PAE, MAE: Double; const FloatPeak: Double): Boolean;
var
  Info: TImageFormatInfo;
  Kind: TSampleKind;
  SampleSize, Padding, X, Y, Channel: Integer;
  P1, P2: PByte;
  V1, V2, Diff, Peak, SampleCount: Double;
  RowAbs, RowSqr, SumAbs, SumSqr: Double;
begin
  Result := False;
  PSNR := 0;
  MSE  := 0;
  RMSE := 0;
  PAE  := 0;
  MAE  := 0;

  if not TestImage(Image1) or not TestImage(Image2) then
    Exit;
  if (Image1.Width <> Image2.Width) or (Image1.Height <> Image2.Height) or
    (Image1.Format <> Image2.Format) then
    Exit;
  if not GetImageFormatInfo(Image1.Format, Info) or not GetSampleKind(Info, Kind, SampleSize) then
    Exit;

  case Kind of
    skUInt8:  Peak := PeakUInt8;
    skUInt16: Peak := PeakUInt16;
    skUInt32: Peak := PeakUInt32;
    skUInt64: Peak := PeakUInt64;
  else
    Peak := FloatPeak;
  end;

  if Peak <= 0 then
    Exit;

  P1 := Image1.Bits;
  P2 := Image2.Bits;
  Padding := Info.BytesPerPixel - Info.ChannelCount * SampleSize;
  SumAbs := 0;
  SumSqr := 0;
  Diff := 0;

  for Y := 0 to Image1.Height - 1 do
  begin
    // Rows are summed separately so small differences are not lost
    // in the large sums of huge images
    RowAbs := 0;
    RowSqr := 0;

    for X := 0 to Image1.Width - 1 do
    begin
      for Channel := 0 to Info.ChannelCount - 1 do
      begin
        case Kind of
          skUInt8:
            Diff := Abs(Integer(P2^) - Integer(P1^));
          skUInt16:
            Diff := Abs(Integer(PWord(P2)^) - Integer(PWord(P1)^));
          skUInt32:
            begin
              V1 := PUInt32(P1)^;
              V2 := PUInt32(P2)^;
              Diff := Abs(V2 - V1);
            end;
          skUInt64:
            begin
              // Differences of low and high halves are exact
              V1 := PUInt32(@PBuffer(P1)[4])^;
              V2 := PUInt32(@PBuffer(P2)[4])^;
              Diff := (V2 - V1) * TwoPow32;
              V1 := PUInt32(P1)^;
              V2 := PUInt32(P2)^;
              Diff := Abs(Diff + (V2 - V1));
            end;
          skHalf:
            begin
              // Samples with the same bits are not subtracted,
              // they can be infinite
              if PWord(P1)^ = PWord(P2)^ then
                Diff := 0
              else
              begin
                V1 := HalfToFloat(PWord(P1)^);
                V2 := HalfToFloat(PWord(P2)^);
                Diff := Abs(V2 - V1);
              end;
            end;
          skSingle:
            begin
              if PUInt32(P1)^ = PUInt32(P2)^ then
                Diff := 0
              else
              begin
                V1 := PSingle(P1)^;
                V2 := PSingle(P2)^;
                Diff := Abs(V2 - V1);
              end;
            end;
        end;

        RowAbs := RowAbs + Diff;
        RowSqr := RowSqr + Diff * Diff;
        if Diff > PAE then
          PAE := Diff;

        Inc(P1, SampleSize);
        Inc(P2, SampleSize);
      end;

      Inc(P1, Padding);
      Inc(P2, Padding);
    end;

    SumAbs := SumAbs + RowAbs;
    SumSqr := SumSqr + RowSqr;
  end;

  SampleCount := Image1.Width;
  SampleCount := SampleCount * Image1.Height * Info.ChannelCount;
  MAE := SumAbs / SampleCount;
  MSE := SumSqr / SampleCount;
  RMSE := Sqrt(MSE);

  if MSE = 0 then
    PSNR := Math.Infinity
  else
    PSNR := 20 * Ln(Peak / RMSE) / Ln10;

  Result := True;
end;

end.
