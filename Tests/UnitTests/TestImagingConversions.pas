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

{ Tests of conversions between image data formats (ConvertImage).

  Each regular data format is converted to each other one and the result is
  compared with values expected by the model of pixels in ImagingTestPixels
  unit, which does not use the conversion code of the library.

  Conversions that are known to give imprecise results are described by
  rules in GetKnownDeviation function. A conversion that deviates without
  a rule fails the tests, and so does a conversion that has a rule and
  does not deviate anymore: the rule is to be removed then.

  Results for all pairs of formats are written to ConversionMatrix.txt
  in the test output directory.}
unit TestImagingConversions;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, ImagingTypes, Imaging, ImagingFormats, ImagingUtility,
  ImagingTestCase, ImagingTestPixels;

type
  TConversionCheck = (ccValues, ccExtremes, ccRoundTrip);

  TTestConversionMatrix = class(TImagingTestCase)
  private
    procedure CheckMatrix(Check: TConversionCheck; const Title: string);
  published
    procedure TestPixelModel;
    procedure TestInvariants;
    procedure TestValuesWithinOneStep;
    procedure TestExtremesAreExact;
    procedure TestRoundTripsAreLossless;
    procedure TestRBSwapPairsAreExact;
    procedure TestOddSizedImage;
    procedure TestSameFormatIsNoOp;
    procedure TestDefaultFormat;
    procedure TestFloatClamping;
  end;

  { Conversions to indexed format. Conversions from it are in the matrix.}
  TTestConversionIndexed = class(TImagingTestCase)
  published
    procedure TestGrayToIndexed;
    procedure TestFewColorsToIndexed;
    procedure TestArbitraryColorsToIndexed;
    procedure TestManyColorsToIndexed;
  end;

  { Conversions from and to special formats: block compressed and binary.}
  TTestConversionSpecial = class(TImagingTestCase)
  published
    procedure TestInvariants;
    procedure TestBinaryKeepsTwoLevels;
    procedure TestColorBlockFormatsKeepFlatBlocks;
    procedure TestGrayBlockFormatsKeepFlatBlocks;
  end;

implementation

const
  MatrixWidth = 16;
  MatrixHeight = 8;
  TestSeed = 20261012;
  MaxReportedFailures = 30;
  ExactEpsilon = 1e-12;
  ReportFileName = 'ConversionMatrix.txt';

type
  TKnownDeviation = (kdNone, kdAlways, kdSometimes);

  TPairResult = record
    Checked: Boolean;
    InvariantError: string;
    // Largest difference from expected value, in units of allowed tolerance
    MaxError: Double;
    ValueDetail: string;
    ExtremesOk: Boolean;
    ExtremesDetail: string;
    RoundTripChecked: Boolean;
    RoundTripOk: Boolean;
    RoundTripDetail: string;
  end;

var
  Formats: array of TImageFormat;
  Infos: array of TImageFormatInfo;
  Matrix: array of array of TPairResult;
  MatrixReady: Boolean = False;

function Fmt(const S: string; const Args: array of const): string;
begin
  // Numbers are written the same way regardless of system locale
  Result := Format(S, Args, GetFormatSettingsForFloats);
end;

function IsPackedFormat(const Info: TImageFormatInfo): Boolean;
begin
  Result := Info.UsePixelFormat;
end;

{ Format with unpacked integer channels: 8 or 16 bits each.}
function IsChannelFormat(const Info: TImageFormatInfo): Boolean;
begin
  Result := not (Info.IsIndexed or Info.HasGrayChannel or Info.IsFloatingPoint or
    Info.UsePixelFormat);
end;

{ Grayscale format with more than 16 bits: Gray32, Gray64.}
function IsWideGrayFormat(const Info: TImageFormatInfo): Boolean;
begin
  Result := Info.HasGrayChannel and (ChannelBits(Info, chRed) > 16);
end;

function IsSingleFloatFormat(const Info: TImageFormatInfo): Boolean;
begin
  Result := Info.IsFloatingPoint and (ChannelBits(Info, chRed) = SinglePrecisionBits);
end;

function IsHalfFloatFormat(const Info: TImageFormatInfo): Boolean;
begin
  Result := Info.IsFloatingPoint and (ChannelBits(Info, chRed) = HalfPrecisionBits);
end;

{ Rules for conversions known to give imprecise results, see the unit header.
  Names in comments are routines of ImagingFormats unit where it happens.}
function GetKnownDeviation(Check: TConversionCheck; const SrcInfo, DstInfo: TImageFormatInfo): TKnownDeviation;
begin
  Result := kdNone;

  case Check of
    ccExtremes:
      begin
        // Channels of packed formats are widened to 16 bits by shl 8 so full
        // channel is 65280 and not 65535: white gets darker, opaque gets
        // slightly transparent (ChannelGetSrcPixel). Packed destinations
        // take just the high bits back so they are not affected.
        if IsPackedFormat(SrcInfo) and not IsPackedFormat(DstInfo) then
          Result := kdAlways;

        // The same widening of palette colors (IndexToChannel, only R8G8B8
        // and A8R8G8B8 have own code) and of palette alpha (IndexToGray)
        if SrcInfo.IsIndexed then
        begin
          if IsChannelFormat(DstInfo) and not (DstInfo.Format in [ifR8G8B8, ifA8R8G8B8]) then
            Result := kdAlways;
          if DstInfo.HasGrayChannel and DstInfo.HasAlphaChannel then
            Result := kdAlways;
        end;

        // The same widening in special code for this pair (GrayToGray)
        if (SrcInfo.Format = ifGray8) and (DstInfo.Format = ifGray16) then
          Result := kdAlways;

        // Gray32 and Gray64 get only the highest 16 bits of gray value,
        // lower bits are zero when the source is grayscale...
        if IsWideGrayFormat(DstInfo) and SrcInfo.HasGrayChannel and
          (ChannelBits(SrcInfo, chRed) < ChannelBits(DstInfo, chRed)) then
          Result := kdAlways;
        // ... and not initialized when it is indexed or floating point
        // (IndexToGray, FloatToGray), so the result differs from run to run.
        // Lower bits from color formats are copies of color channels
        // (ChannelToGray) which happens to be right for black and white.
        if IsWideGrayFormat(DstInfo) and (SrcInfo.IsIndexed or SrcInfo.IsFloatingPoint) then
          Result := kdSometimes;
      end;
    ccValues:
      begin
        // Gray32 and Gray64 are converted from and to floating point with
        // 16 bits of precision only (GrayToFloat, FloatToGray). Visible
        // just against 32-bit floats, half floats are less precise than that.
        if (IsWideGrayFormat(SrcInfo) and IsSingleFloatFormat(DstInfo)) or
          (IsSingleFloatFormat(SrcInfo) and IsWideGrayFormat(DstInfo)) then
          Result := kdAlways;
      end;
    ccRoundTrip:
      begin
        // Narrowing from 16 to 8 bits truncates (MulDiv in ChannelSetDstPixel
        // and GraySetDstPixel). Values rounded by half floats end up
        // one lower on the way back.
        if not (SrcInfo.IsFloatingPoint or IsPackedFormat(SrcInfo)) and
          (ChannelBits(SrcInfo, chRed) = 8) and IsHalfFloatFormat(DstInfo) then
          Result := kdAlways;

        // Channels of 5 and 6 bits are scaled to 8 bits, widened by shl 8 and
        // narrowed again with truncation: the 8-bit value is one lower
        // than it should be and does not give the original bits back.
        if IsPackedFormat(SrcInfo) and IsChannelFormat(DstInfo) and
          (ChannelBits(DstInfo, chRed) = 8) and (ChannelBits(SrcInfo, chRed) >= 5) then
          Result := kdAlways;
      end;
  end;
end;

procedure CollectFormats;
var
  Fmt: TImageFormat;
  Info: TImageFormatInfo;
  Count: Integer;
begin
  if Length(Formats) > 0 then
    Exit;

  Count := 0;
  SetLength(Formats, Ord(High(TImageFormat)) + 1);
  SetLength(Infos, Length(Formats));

  for Fmt := Low(TImageFormat) to High(TImageFormat) do
  begin
    if (Fmt <> ifDefault) and GetImageFormatInfo(Fmt, Info) and not Info.IsSpecial then
    begin
      Formats[Count] := Fmt;
      Infos[Count] := Info;
      Inc(Count);
    end;
  end;

  SetLength(Formats, Count);
  SetLength(Infos, Count);
end;

function PairName(const SrcInfo, DstInfo: TImageFormatInfo): string;
begin
  Result := string(SrcInfo.Name) + ' -> ' + string(DstInfo.Name);
end;

function MaxDouble(const A, B: Double): Double;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Clamp01(const Value: Double): Double;
begin
  if Value < 0.0 then
    Result := 0.0
  else if Value > 1.0 then
    Result := 1.0
  else
    Result := Value;
end;

{ Returns pixel expected in destination format for given pixel of source format.}
function ExpectedPixel(const Src: TIdealPixel; const SrcInfo, DstInfo: TImageFormatInfo): TIdealPixel;
var
  Ch: TPixelChannel;
  Gray: Double;
begin
  Result := Src;

  // Values out of range can be kept only by floating point formats
  if SrcInfo.IsFloatingPoint and not DstInfo.IsFloatingPoint then
  begin
    for Ch := Low(TPixelChannel) to High(TPixelChannel) do
      Result.Channels[Ch] := Clamp01(Result.Channels[Ch]);
  end;

  if DstInfo.HasGrayChannel then
  begin
    if SrcInfo.HasGrayChannel then
      Gray := Result.R
    else
      Gray := 0.299 * Result.R + 0.587 * Result.G + 0.114 * Result.B;
    Result.R := Gray;
    Result.G := Gray;
    Result.B := Gray;
  end;

  if IsOneChannelFloat(DstInfo) then
  begin
    Result.G := 0.0;
    Result.B := 0.0;
  end;

  if not DstInfo.HasAlphaChannel then
    Result.A := 1.0;
end;

{ Returns allowed difference of channel in destination format from the expected
  value: one step of the less precise of the two formats.}
function GetTolerance(const SrcInfo, DstInfo: TImageFormatInfo; Ch: TPixelChannel;
  const SrcPixel, Expected: TIdealPixel): Double;
var
  SrcStep: Double;
  C: TPixelChannel;
begin
  if (Ch <> chAlpha) and DstInfo.HasGrayChannel and not SrcInfo.HasGrayChannel then
  begin
    // Gray is computed from all color channels of source
    SrcStep := 0.0;
    for C := chRed to chBlue do
      SrcStep := MaxDouble(SrcStep, ChannelStep(SrcInfo, C, SrcPixel.Channels[C]));
  end
  else
    SrcStep := ChannelStep(SrcInfo, Ch, SrcPixel.Channels[Ch]);

  Result := MaxDouble(SrcStep, ChannelStep(DstInfo, Ch, Expected.Channels[Ch])) + ExactEpsilon;
end;

{ Returns True when all pixels of image in the first format can be stored in
  the second format and read back without any loss.}
function CanHold(const A, B: TImageFormatInfo): Boolean;
var
  Ch: TPixelChannel;
  BitsA, BitsB: Integer;
begin
  Result := False;

  if A.IsIndexed or B.IsIndexed or (A.Format = B.Format) then
    Exit;
  if A.IsFloatingPoint and not B.IsFloatingPoint then
    Exit;
  // Gray computed from colors cannot give the colors back
  if B.HasGrayChannel and not A.HasGrayChannel then
    Exit;

  for Ch := Low(TPixelChannel) to High(TPixelChannel) do
  begin
    BitsA := ChannelBits(A, Ch);
    BitsB := ChannelBits(B, Ch);

    if BitsA = 0 then
      Continue;
    if BitsB = 0 then
      Exit;

    if B.IsFloatingPoint and not A.IsFloatingPoint then
    begin
      // Integer values scaled to 0..1 need some more bits to round back safely
      if BitsA > BitsB - 1 then
        Exit;
    end
    else if BitsA > BitsB then
      Exit;
  end;

  Result := True;
end;

procedure NewTestImage(Format: TImageFormat; Width, Height: Integer; var Image: TImageData);
begin
  InitImage(Image);
  if not NewImage(Width, Height, Format, Image) then
    raise Exception.Create('NewImage failed for ' + GetFormatName(Format));
  FillTestImage(Image, TestSeed);
end;

function CheckConvertedImage(const Image: TImageData; const DstInfo: TImageFormatInfo;
  Width, Height: Integer): string;
begin
  Result := '';
  if (Image.Width <> Width) or (Image.Height <> Height) then
    Result := Fmt('dimensions changed to %dx%d', [Image.Width, Image.Height])
  else if Image.Format <> DstInfo.Format then
    Result := 'format is ' + GetFormatName(Image.Format)
  else if Image.Size <> GetPixelsSize(DstInfo.Format, Width, Height) then
    Result := Fmt('size is %d', [Image.Size])
  else if not TestImage(Image) then
    Result := 'TestImage failed'
  else if Image.Bits = nil then
    Result := 'no bits'
  // Not checked the other way: ConvertImage allocates palette with zero
  // entries for not indexed formats, which is not nil pointer in FPC
  else if DstInfo.IsIndexed and (Image.Palette = nil) then
    Result := 'no palette';
end;

procedure AnalyzePair(const SrcInfo, DstInfo: TImageFormatInfo; Width, Height: Integer;
  var R: TPairResult);
var
  Src, Dst: TImageData;
  I, PixelCount: Integer;
  Ch: TPixelChannel;
  SrcPixel, Expected, Actual, Back: TIdealPixel;
  Error, Tolerance: Double;
begin
  R.Checked := True;
  R.InvariantError := '';
  R.MaxError := 0.0;
  R.ValueDetail := '';
  R.ExtremesOk := True;
  R.ExtremesDetail := '';
  R.RoundTripChecked := False;
  R.RoundTripOk := True;
  R.RoundTripDetail := '';
  PixelCount := Width * Height;

  NewTestImage(SrcInfo.Format, Width, Height, Src);
  InitImage(Dst);
  try
    try
      if not CloneImage(Src, Dst) then
        R.InvariantError := 'CloneImage failed'
      else if not ConvertImage(Dst, DstInfo.Format) then
        R.InvariantError := 'ConvertImage returned False'
      else
        R.InvariantError := CheckConvertedImage(Dst, DstInfo, Width, Height);
    except
      on E: Exception do
        R.InvariantError := E.ClassName + ': ' + E.Message;
    end;

    if R.InvariantError <> '' then
      Exit;

    for I := 0 to PixelCount - 1 do
    begin
      SrcPixel := ReadIdealPixel(Src, I);
      Expected := ExpectedPixel(SrcPixel, SrcInfo, DstInfo);
      Actual := ReadIdealPixel(Dst, I);

      for Ch := Low(TPixelChannel) to High(TPixelChannel) do
      begin
        Tolerance := GetTolerance(SrcInfo, DstInfo, Ch, SrcPixel, Expected);
        Error := Abs(Actual.Channels[Ch] - Expected.Channels[Ch]) / Tolerance;

        if Error > R.MaxError then
        begin
          R.MaxError := Error;
          R.ValueDetail := Fmt('pixel %d channel %s: source %s expected %.6f but was %.6f',
            [I, ChannelNames[Ch], IdealPixelToStr(SrcPixel), Expected.Channels[Ch], Actual.Channels[Ch]]);
        end;

        // The smallest and the largest values must be converted exactly
        if R.ExtremesOk and (I < ExtremePixelCount) and
          ((Abs(Expected.Channels[Ch]) < 1e-9) or (Abs(Expected.Channels[Ch] - 1.0) < 1e-9)) and
          (Abs(Actual.Channels[Ch] - Expected.Channels[Ch]) > ExactEpsilon) then
        begin
          R.ExtremesOk := False;
          R.ExtremesDetail := Fmt('pixel %d channel %s: expected %.0f but was %.6f',
            [I, ChannelNames[Ch], Expected.Channels[Ch], Actual.Channels[Ch]]);
        end;
      end;
    end;

    if CanHold(SrcInfo, DstInfo) then
    begin
      R.RoundTripChecked := True;
      try
        if not ConvertImage(Dst, SrcInfo.Format) then
        begin
          R.RoundTripOk := False;
          R.RoundTripDetail := 'conversion back returned False';
        end;
      except
        on E: Exception do
        begin
          R.RoundTripOk := False;
          R.RoundTripDetail := E.ClassName + ': ' + E.Message;
        end;
      end;

      if R.RoundTripOk then
      begin
        for I := 0 to PixelCount - 1 do
        begin
          SrcPixel := ReadIdealPixel(Src, I);
          Back := ReadIdealPixel(Dst, I);

          for Ch := Low(TPixelChannel) to High(TPixelChannel) do
          begin
            if R.RoundTripOk and (SrcPixel.Channels[Ch] <> Back.Channels[Ch]) then
            begin
              R.RoundTripOk := False;
              R.RoundTripDetail := Fmt('pixel %d channel %s: %.9f came back as %.9f',
                [I, ChannelNames[Ch], SrcPixel.Channels[Ch], Back.Channels[Ch]]);
            end;
          end;
        end;
      end;
    end;
  finally
    FreeImage(Src);
    FreeImage(Dst);
  end;
end;

function PairDeviates(const R: TPairResult; Check: TConversionCheck): Boolean;
begin
  case Check of
    ccValues:    Result := R.MaxError > 1.0;
    ccExtremes:  Result := not R.ExtremesOk;
  else
    Result := R.RoundTripChecked and not R.RoundTripOk;
  end;
end;

function PairDetail(const R: TPairResult; Check: TConversionCheck): string;
begin
  case Check of
    ccValues:    Result := Fmt('%.2f steps off, %s', [R.MaxError, R.ValueDetail]);
    ccExtremes:  Result := R.ExtremesDetail;
  else
    Result := R.RoundTripDetail;
  end;
end;

function StatusText(const R: TPairResult; Check: TConversionCheck;
  const SrcInfo, DstInfo: TImageFormatInfo): string;
var
  Known: TKnownDeviation;
begin
  Known := GetKnownDeviation(Check, SrcInfo, DstInfo);

  if (Check = ccRoundTrip) and not R.RoundTripChecked then
    Result := '-'
  else if PairDeviates(R, Check) then
    Result := Iff(Known <> kdNone, 'known', 'FAILED')
  else if Known = kdSometimes then
    Result := 'ok*'
  else
    Result := Iff(Known = kdAlways, 'FIXED', 'ok');
end;

procedure WriteMatrixReport;
var
  Lines: TStringList;
  S, D: Integer;
  R: TPairResult;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('Conversions between regular data formats, ' +
      Fmt('test image %dx%d', [MatrixWidth, MatrixHeight]));
    Lines.Add('error: the largest difference from expected value in steps of the less precise format');
    Lines.Add('known: known deviation, FAILED: deviation without a rule, FIXED: rule without a deviation,');
    Lines.Add('ok*: no deviation in this run but the result is not stable');
    Lines.Add('');
    Lines.Add(Fmt('%-34s %8s  %-8s %-8s %-8s  %s',
      ['conversion', 'error', 'values', 'extremes', 'roundtrip', 'details']));

    for S := 0 to High(Formats) do
    begin
      for D := 0 to High(Formats) do
      begin
        R := Matrix[S, D];
        if not R.Checked then
          Continue;

        if R.InvariantError <> '' then
        begin
          Lines.Add(Fmt('%-34s FAILED: %s', [PairName(Infos[S], Infos[D]), R.InvariantError]));
          Continue;
        end;

        Lines.Add(TrimRight(Fmt('%-34s %8.3f  %-8s %-8s %-8s  %s',
          [PairName(Infos[S], Infos[D]), R.MaxError,
          StatusText(R, ccValues, Infos[S], Infos[D]),
          StatusText(R, ccExtremes, Infos[S], Infos[D]),
          StatusText(R, ccRoundTrip, Infos[S], Infos[D]),
          Iff(PairDeviates(R, ccExtremes), 'extremes: ' + R.ExtremesDetail + '; ', '') +
          Iff(PairDeviates(R, ccRoundTrip), 'roundtrip: ' + R.RoundTripDetail, '')])));
      end;
    end;

    Lines.SaveToFile(GetTestOutDir + ReportFileName);
  finally
    Lines.Free;
  end;
end;

{ Converts all pairs of formats with not indexed destination. Results are kept
  for all test methods.}
procedure EnsureMatrix;
var
  S, D: Integer;
begin
  if MatrixReady then
    Exit;

  CollectFormats;
  SetLength(Matrix, Length(Formats), Length(Formats));

  for S := 0 to High(Formats) do
  begin
    for D := 0 to High(Formats) do
    begin
      Matrix[S, D].Checked := False;
      if (S <> D) and not Infos[D].IsIndexed then
        AnalyzePair(Infos[S], Infos[D], MatrixWidth, MatrixHeight, Matrix[S, D]);
    end;
  end;

  MatrixReady := True;
  WriteMatrixReport;
end;

function JoinFailures(Failures: TStrings; const Title: string): string;
var
  I: Integer;
begin
  Result := Fmt('%s: %d conversions', [Title, Failures.Count]);
  for I := 0 to Failures.Count - 1 do
  begin
    if I = MaxReportedFailures then
    begin
      Result := Result + sLineBreak + Fmt('  ... and %d more, see %s',
        [Failures.Count - I, ReportFileName]);
      Break;
    end;
    Result := Result + sLineBreak + '  ' + Failures[I];
  end;
end;

{ TTestConversionMatrix }

procedure TTestConversionMatrix.CheckMatrix(Check: TConversionCheck; const Title: string);
var
  Failures: TStringList;
  S, D: Integer;
  R: TPairResult;
  Known: TKnownDeviation;
begin
  EnsureMatrix;
  Failures := TStringList.Create;
  try
    for S := 0 to High(Formats) do
    begin
      for D := 0 to High(Formats) do
      begin
        R := Matrix[S, D];
        if not R.Checked or (R.InvariantError <> '') then
          Continue;
        if (Check = ccRoundTrip) and not R.RoundTripChecked then
          Continue;

        Known := GetKnownDeviation(Check, Infos[S], Infos[D]);
        if PairDeviates(R, Check) and (Known = kdNone) then
          Failures.Add(PairName(Infos[S], Infos[D]) + ': ' + PairDetail(R, Check))
        else if (Known = kdAlways) and not PairDeviates(R, Check) then
          Failures.Add(PairName(Infos[S], Infos[D]) + ': known deviation is gone, remove its rule');
      end;
    end;

    if Failures.Count > 0 then
      Fail(JoinFailures(Failures, Title));
  finally
    Failures.Free;
  end;
end;

procedure TTestConversionMatrix.TestPixelModel;
var
  Image: TImageData;
  Pixel: TIdealPixel;
  Info: TImageFormatInfo;

  procedure CheckPixel(const Msg: string; const A, R, G, B: Double);
  begin
    CheckEquals(A, Pixel.A, 1e-12, Msg + ': alpha');
    CheckEquals(R, Pixel.R, 1e-12, Msg + ': red');
    CheckEquals(G, Pixel.G, 1e-12, Msg + ': green');
    CheckEquals(B, Pixel.B, 1e-12, Msg + ': blue');
  end;

begin
  // The model must be right before it is used to judge the library:
  // pixels written here byte by byte and expected values counted by hand
  InitImage(Image);
  try
    CheckTrue(NewImage(2, 1, ifA8R8G8B8, Image), 'NewImage A8R8G8B8');
    PUInt32Array(Image.Bits)[1] := $80FF4000;
    Pixel := ReadIdealPixel(Image, 1);
    CheckPixel('A8R8G8B8', 128 / 255, 1.0, 64 / 255, 0.0);

    CheckTrue(NewImage(2, 1, ifX8R8G8B8, Image), 'NewImage X8R8G8B8');
    PUInt32Array(Image.Bits)[1] := $00FF4000;
    Pixel := ReadIdealPixel(Image, 1);
    CheckPixel('X8R8G8B8', 1.0, 1.0, 64 / 255, 0.0);

    CheckTrue(NewImage(2, 1, ifR8G8B8, Image), 'NewImage R8G8B8');
    PBuffer(Image.Bits)[3] := 51;   // blue
    PBuffer(Image.Bits)[4] := 102;  // green
    PBuffer(Image.Bits)[5] := 255;  // red
    Pixel := ReadIdealPixel(Image, 1);
    CheckPixel('R8G8B8', 1.0, 1.0, 0.4, 0.2);

    CheckTrue(NewImage(2, 1, ifR5G6B5, Image), 'NewImage R5G6B5');
    PWordArray(Image.Bits)[1] := $F81F;
    Pixel := ReadIdealPixel(Image, 1);
    CheckPixel('R5G6B5 magenta', 1.0, 1.0, 0.0, 1.0);
    PWordArray(Image.Bits)[1] := $0400;
    Pixel := ReadIdealPixel(Image, 1);
    CheckPixel('R5G6B5 green', 1.0, 0.0, 32 / 63, 0.0);

    CheckTrue(NewImage(2, 1, ifA1R5G5B5, Image), 'NewImage A1R5G5B5');
    PWordArray(Image.Bits)[1] := $8000 or (31 shl 10) or (10 shl 5) or 1;
    Pixel := ReadIdealPixel(Image, 1);
    CheckPixel('A1R5G5B5', 1.0, 1.0, 10 / 31, 1 / 31);

    CheckTrue(NewImage(2, 1, ifA4R4G4B4, Image), 'NewImage A4R4G4B4');
    PWordArray(Image.Bits)[1] := $5F30;
    Pixel := ReadIdealPixel(Image, 1);
    CheckPixel('A4R4G4B4', 5 / 15, 1.0, 3 / 15, 0.0);

    CheckTrue(NewImage(2, 1, ifR3G3B2, Image), 'NewImage R3G3B2');
    PBuffer(Image.Bits)[1] := $E0 or (2 shl 2) or 1;
    Pixel := ReadIdealPixel(Image, 1);
    CheckPixel('R3G3B2', 1.0, 1.0, 2 / 7, 1 / 3);

    CheckTrue(NewImage(2, 1, ifGray8, Image), 'NewImage Gray8');
    PBuffer(Image.Bits)[1] := 51;
    Pixel := ReadIdealPixel(Image, 1);
    CheckPixel('Gray8', 1.0, 0.2, 0.2, 0.2);

    CheckTrue(NewImage(2, 1, ifA8Gray8, Image), 'NewImage A8Gray8');
    PBuffer(Image.Bits)[2] := 51;   // gray
    PBuffer(Image.Bits)[3] := 102;  // alpha
    Pixel := ReadIdealPixel(Image, 1);
    CheckPixel('A8Gray8', 0.4, 0.2, 0.2, 0.2);

    CheckTrue(NewImage(2, 1, ifGray16, Image), 'NewImage Gray16');
    PWordArray(Image.Bits)[1] := 13107;
    Pixel := ReadIdealPixel(Image, 1);
    CheckPixel('Gray16', 1.0, 0.2, 0.2, 0.2);

    CheckTrue(NewImage(2, 1, ifA16Gray16, Image), 'NewImage A16Gray16');
    PWordArray(Image.Bits)[2] := 13107;  // gray
    PWordArray(Image.Bits)[3] := 65535;  // alpha
    Pixel := ReadIdealPixel(Image, 1);
    CheckPixel('A16Gray16', 1.0, 0.2, 0.2, 0.2);

    CheckTrue(NewImage(2, 1, ifGray32, Image), 'NewImage Gray32');
    PUInt32Array(Image.Bits)[1] := $FFFFFFFF;
    Pixel := ReadIdealPixel(Image, 1);
    CheckPixel('Gray32', 1.0, 1.0, 1.0, 1.0);

    CheckTrue(NewImage(2, 1, ifGray64, Image), 'NewImage Gray64');
    PUInt32Array(Image.Bits)[2] := $FFFFFFFF;
    PUInt32Array(Image.Bits)[3] := $FFFFFFFF;
    Pixel := ReadIdealPixel(Image, 1);
    CheckPixel('Gray64', 1.0, 1.0, 1.0, 1.0);

    CheckTrue(NewImage(2, 1, ifA16R16G16B16, Image), 'NewImage A16R16G16B16');
    PWordArray(Image.Bits)[4] := 13107;  // blue
    PWordArray(Image.Bits)[5] := 0;      // green
    PWordArray(Image.Bits)[6] := 65535;  // red
    PWordArray(Image.Bits)[7] := 26214;  // alpha
    Pixel := ReadIdealPixel(Image, 1);
    CheckPixel('A16R16G16B16', 0.4, 1.0, 0.0, 0.2);

    CheckTrue(NewImage(2, 1, ifA16B16G16R16, Image), 'NewImage A16B16G16R16');
    PWordArray(Image.Bits)[4] := 13107;  // red
    PWordArray(Image.Bits)[6] := 65535;  // blue
    Pixel := ReadIdealPixel(Image, 1);
    CheckPixel('A16B16G16R16', 0.0, 0.2, 0.0, 1.0);

    CheckTrue(NewImage(2, 1, ifR32F, Image), 'NewImage R32F');
    PSingle(@PBuffer(Image.Bits)[4])^ := 0.25;
    Pixel := ReadIdealPixel(Image, 1);
    CheckPixel('R32F', 1.0, 0.25, 0.0, 0.0);

    CheckTrue(NewImage(2, 1, ifA32B32G32R32F, Image), 'NewImage A32B32G32R32F');
    PSingle(@PBuffer(Image.Bits)[16])^ := 0.25;  // red
    PSingle(@PBuffer(Image.Bits)[20])^ := 0.5;   // green
    PSingle(@PBuffer(Image.Bits)[24])^ := 2.0;   // blue
    PSingle(@PBuffer(Image.Bits)[28])^ := 0.75;  // alpha
    Pixel := ReadIdealPixel(Image, 1);
    CheckPixel('A32B32G32R32F', 0.75, 0.25, 0.5, 2.0);

    CheckTrue(NewImage(2, 1, ifA16R16G16B16F, Image), 'NewImage A16R16G16B16F');
    PWordArray(Image.Bits)[4] := $3C00;  // blue 1.0
    PWordArray(Image.Bits)[5] := $3800;  // green 0.5
    PWordArray(Image.Bits)[6] := $C000;  // red -2.0
    PWordArray(Image.Bits)[7] := $0000;  // alpha 0.0
    Pixel := ReadIdealPixel(Image, 1);
    CheckPixel('A16R16G16B16F', 0.0, -2.0, 0.5, 1.0);

    CheckTrue(NewImage(2, 1, ifIndex8, Image), 'NewImage Index8');
    Image.Palette[7].Color := $66FF0033;
    PBuffer(Image.Bits)[1] := 7;
    Pixel := ReadIdealPixel(Image, 1);
    CheckPixel('Index8', 0.4, 1.0, 0.0, 0.2);
  finally
    FreeImage(Image);
  end;

  // Sizes of channels
  CheckTrue(GetImageFormatInfo(ifR5G6B5, Info), 'Info R5G6B5');
  CheckEquals(0, ChannelBits(Info, chAlpha), 'R5G6B5 alpha bits');
  CheckEquals(5, ChannelBits(Info, chRed), 'R5G6B5 red bits');
  CheckEquals(6, ChannelBits(Info, chGreen), 'R5G6B5 green bits');
  CheckEquals(1 / 63, ChannelStep(Info, chGreen, 0.5), 1e-12, 'R5G6B5 green step');
  CheckTrue(GetImageFormatInfo(ifA16Gray16, Info), 'Info A16Gray16');
  CheckEquals(16, ChannelBits(Info, chAlpha), 'A16Gray16 alpha bits');
  CheckEquals(16, ChannelBits(Info, chBlue), 'A16Gray16 gray bits');
  CheckTrue(GetImageFormatInfo(ifX8R8G8B8, Info), 'Info X8R8G8B8');
  CheckEquals(0, ChannelBits(Info, chAlpha), 'X8R8G8B8 alpha bits');
  CheckEquals(8, ChannelBits(Info, chRed), 'X8R8G8B8 red bits');
  CheckTrue(GetImageFormatInfo(ifR16F, Info), 'Info R16F');
  CheckEquals(HalfPrecisionBits, ChannelBits(Info, chRed), 'R16F red bits');
  CheckEquals(0, ChannelBits(Info, chGreen), 'R16F green bits');
end;

procedure TTestConversionMatrix.TestInvariants;
var
  Failures: TStringList;
  S, D: Integer;
begin
  EnsureMatrix;
  CheckTrue(Length(Formats) > 25, 'Most of the formats are tested');

  Failures := TStringList.Create;
  try
    for S := 0 to High(Formats) do
    begin
      for D := 0 to High(Formats) do
      begin
        if Matrix[S, D].Checked and (Matrix[S, D].InvariantError <> '') then
          Failures.Add(PairName(Infos[S], Infos[D]) + ': ' + Matrix[S, D].InvariantError);
      end;
    end;

    if Failures.Count > 0 then
      Fail(JoinFailures(Failures, 'Conversion failed or gave invalid image'));
  finally
    Failures.Free;
  end;
end;

procedure TTestConversionMatrix.TestValuesWithinOneStep;
begin
  CheckMatrix(ccValues, 'Converted values differ from expected ones by more than one step');
end;

procedure TTestConversionMatrix.TestExtremesAreExact;
begin
  CheckMatrix(ccExtremes, 'The smallest or the largest channel values are not converted exactly');
end;

procedure TTestConversionMatrix.TestRoundTripsAreLossless;
begin
  CheckMatrix(ccRoundTrip, 'Conversion to more precise format and back changed the pixels');
end;

procedure TTestConversionMatrix.TestRBSwapPairsAreExact;
var
  S, D, Count: Integer;
begin
  EnsureMatrix;
  Count := 0;

  for S := 0 to High(Formats) do
  begin
    for D := 0 to High(Formats) do
    begin
      if Infos[S].RBSwapFormat = Formats[D] then
      begin
        Inc(Count);
        CheckEquals('', Matrix[S, D].InvariantError, PairName(Infos[S], Infos[D]) + ': conversion');
        CheckEquals(0.0, Matrix[S, D].MaxError, 0.0, PairName(Infos[S], Infos[D]) + ': ' + Matrix[S, D].ValueDetail);
        CheckTrue(Matrix[S, D].RoundTripChecked and Matrix[S, D].RoundTripOk,
          PairName(Infos[S], Infos[D]) + ': ' + Matrix[S, D].RoundTripDetail);
      end;
    end;
  end;

  CheckTrue(Count >= 8, 'Formats with swapped red and blue found');
end;

procedure TTestConversionMatrix.TestOddSizedImage;
const
  Width = 17;
  Height = 5;
var
  Failures: TStringList;
  S, D: Integer;
  R: TPairResult;
begin
  // Statuses must be the same as for the image used for the matrix
  EnsureMatrix;
  Failures := TStringList.Create;
  try
    for S := 0 to High(Formats) do
    begin
      for D := 0 to High(Formats) do
      begin
        if not Matrix[S, D].Checked then
          Continue;

        AnalyzePair(Infos[S], Infos[D], Width, Height, R);
        if R.InvariantError <> '' then
          Failures.Add(PairName(Infos[S], Infos[D]) + ': ' + R.InvariantError)
        else if PairDeviates(R, ccValues) <> PairDeviates(Matrix[S, D], ccValues) then
          Failures.Add(PairName(Infos[S], Infos[D]) + ': values: ' + PairDetail(R, ccValues))
        else if PairDeviates(R, ccRoundTrip) <> PairDeviates(Matrix[S, D], ccRoundTrip) then
          Failures.Add(PairName(Infos[S], Infos[D]) + ': round trip: ' + PairDetail(R, ccRoundTrip));
      end;
    end;

    if Failures.Count > 0 then
      Fail(JoinFailures(Failures, Fmt('Different results for image %dx%d', [Width, Height])));
  finally
    Failures.Free;
  end;
end;

procedure TTestConversionMatrix.TestSameFormatIsNoOp;
var
  I: Integer;
  Image, Copy: TImageData;
begin
  CollectFormats;

  for I := 0 to High(Formats) do
  begin
    NewTestImage(Formats[I], MatrixWidth, MatrixHeight, Image);
    InitImage(Copy);
    try
      CheckTrue(CloneImage(Image, Copy), 'CloneImage');
      CheckTrue(ConvertImage(Image, Formats[I]), GetFormatName(Formats[I]) + ': ConvertImage');
      CheckImagesEqual(Copy, Image, GetFormatName(Formats[I]) + ' to the same format');
    finally
      FreeImage(Image);
      FreeImage(Copy);
    end;
  end;
end;

procedure TTestConversionMatrix.TestDefaultFormat;
var
  Image, Copy: TImageData;
begin
  NewTestImage(ifR8G8B8, MatrixWidth, MatrixHeight, Image);
  InitImage(Copy);
  try
    CheckTrue(CloneImage(Image, Copy), 'CloneImage');
    CheckTrue(ConvertImage(Image, ifDefault), 'ConvertImage to ifDefault');
    CheckEquals(Ord(ifA8R8G8B8), Ord(Image.Format), 'Default format');

    CheckTrue(ConvertImage(Copy, ifA8R8G8B8), 'ConvertImage to A8R8G8B8');
    CheckImagesEqual(Copy, Image, 'Conversion to ifDefault and to the default format');
  finally
    FreeImage(Image);
    FreeImage(Copy);
  end;
end;

procedure TTestConversionMatrix.TestFloatClamping;
const
  Values: array[0..6] of Single = (-0.5, 1.5, 100.0, -3.0, 0.5, 2.0, 0.0);
  SrcFormats: array[0..2] of TImageFormat = (ifA32R32G32B32F, ifA16B16G16R16F, ifR32F);
  DstFormats: array[0..6] of TImageFormat = (ifA8R8G8B8, ifA16R16G16B16, ifR5G6B5,
    ifGray8, ifA16Gray16, ifA16R16G16B16F, ifA32B32G32R32F);
var
  S, D, I: Integer;
  Ch: TPixelChannel;
  Src, Dst: TImageData;
  SrcInfo, DstInfo: TImageFormatInfo;
  SrcPixel, Expected, Actual: TIdealPixel;
  Msg: string;
begin
  for S := Low(SrcFormats) to High(SrcFormats) do
  begin
    for D := Low(DstFormats) to High(DstFormats) do
    begin
      CheckTrue(GetImageFormatInfo(SrcFormats[S], SrcInfo), 'Source format info');
      CheckTrue(GetImageFormatInfo(DstFormats[D], DstInfo), 'Destination format info');

      InitImage(Src);
      InitImage(Dst);
      try
        CheckTrue(NewImage(MatrixWidth, MatrixHeight, SrcFormats[S], Src), 'NewImage');
        FillFloatImage(Src, Values);
        CheckTrue(CloneImage(Src, Dst), 'CloneImage');
        CheckTrue(ConvertImage(Dst, DstFormats[D]), PairName(SrcInfo, DstInfo) + ': ConvertImage');

        for I := 0 to MatrixWidth * MatrixHeight - 1 do
        begin
          SrcPixel := ReadIdealPixel(Src, I);
          // Values out of 0..1 are clamped for integer formats and kept for float ones
          Expected := ExpectedPixel(SrcPixel, SrcInfo, DstInfo);
          Actual := ReadIdealPixel(Dst, I);

          for Ch := Low(TPixelChannel) to High(TPixelChannel) do
          begin
            Msg := Fmt('%s pixel %d channel %s, source %s', [PairName(SrcInfo, DstInfo), I,
              ChannelNames[Ch], IdealPixelToStr(SrcPixel)]);
            CheckEquals(Expected.Channels[Ch], Actual.Channels[Ch],
              GetTolerance(SrcInfo, DstInfo, Ch, SrcPixel, Expected), Msg);
          end;
        end;
      finally
        FreeImage(Src);
        FreeImage(Dst);
      end;
    end;
  end;
end;

{ TTestConversionIndexed }

procedure TTestConversionIndexed.TestGrayToIndexed;
var
  Failures: TStringList;
  F, I: Integer;
  Ch: TPixelChannel;
  Src, Dst: TImageData;
  IndexInfo: TImageFormatInfo;
  SrcPixel, Actual: TIdealPixel;
  GrayTolerance, Tolerance, Expected: Double;
  FormatFailed: Boolean;
begin
  // Grayscale images get palette with shades of gray, alpha is not kept
  CollectFormats;
  CheckTrue(GetImageFormatInfo(ifIndex8, IndexInfo), 'Index8 info');
  Failures := TStringList.Create;
  try
    for F := 0 to High(Formats) do
    begin
      if not Infos[F].HasGrayChannel then
        Continue;

      NewTestImage(Formats[F], MatrixWidth, MatrixHeight, Src);
      InitImage(Dst);
      try
        CheckTrue(CloneImage(Src, Dst), 'CloneImage');
        CheckTrue(ConvertImage(Dst, ifIndex8), PairName(Infos[F], IndexInfo) + ': ConvertImage');
        CheckEquals('', CheckConvertedImage(Dst, IndexInfo, MatrixWidth, MatrixHeight),
          PairName(Infos[F], IndexInfo) + ': converted image');

        FormatFailed := False;
        for I := 0 to MatrixWidth * MatrixHeight - 1 do
        begin
          SrcPixel := ReadIdealPixel(Src, I);
          Actual := ReadIdealPixel(Dst, I);
          GrayTolerance := MaxDouble(ChannelStep(Infos[F], chRed, SrcPixel.R),
            ChannelStep(IndexInfo, chRed, SrcPixel.R)) + ExactEpsilon;
          // The same number of levels: palette index is the gray value
          if ChannelBits(Infos[F], chRed) = 8 then
            GrayTolerance := ExactEpsilon;

          for Ch := Low(TPixelChannel) to High(TPixelChannel) do
          begin
            Expected := SrcPixel.R;
            Tolerance := GrayTolerance;
            if Ch = chAlpha then
            begin
              Expected := 1.0;
              Tolerance := ExactEpsilon;
            end;

            if not FormatFailed and (Abs(Actual.Channels[Ch] - Expected) > Tolerance) then
            begin
              FormatFailed := True;
              Failures.Add(Fmt('%s: pixel %d channel %s: expected %.6f but was %.6f',
                [PairName(Infos[F], IndexInfo), I, ChannelNames[Ch], Expected, Actual.Channels[Ch]]));
            end;
          end;
        end;
      finally
        FreeImage(Src);
        FreeImage(Dst);
      end;
    end;

    if Failures.Count > 0 then
      Fail(JoinFailures(Failures, 'Grayscale images converted to indexed format differ'));
  finally
    Failures.Free;
  end;
end;

procedure TTestConversionIndexed.TestFewColorsToIndexed;
var
  Failures: TStringList;
  F, I, Bpp: Integer;
  Ch: TPixelChannel;
  Src, Dst: TImageData;
  IndexInfo: TImageFormatInfo;
  SrcPixel, Expected, Actual: TIdealPixel;
  PairFailed: Boolean;
begin
  // Image with less colors than palette entries: all the colors must be kept
  // (with precision of 8-bit palette entries)
  CollectFormats;
  CheckTrue(GetImageFormatInfo(ifIndex8, IndexInfo), 'Index8 info');
  Failures := TStringList.Create;
  try
    for F := 0 to High(Formats) do
    begin
      if Infos[F].HasGrayChannel or Infos[F].IsIndexed then
        Continue;

      NewTestImage(Formats[F], MatrixWidth, MatrixHeight, Src);
      InitImage(Dst);
      try
        // Only the pixels with combinations of extreme values are used
        Bpp := Infos[F].BytesPerPixel;
        for I := ExtremePixelCount to MatrixWidth * MatrixHeight - 1 do
        begin
          Move(PBuffer(Src.Bits)[(I mod ExtremePixelCount) * Bpp],
               PBuffer(Src.Bits)[I * Bpp], Bpp);
        end;

        CheckTrue(CloneImage(Src, Dst), 'CloneImage');
        CheckTrue(ConvertImage(Dst, ifIndex8), PairName(Infos[F], IndexInfo) + ': ConvertImage');
        CheckEquals('', CheckConvertedImage(Dst, IndexInfo, MatrixWidth, MatrixHeight),
          PairName(Infos[F], IndexInfo) + ': converted image');

        PairFailed := False;
        for I := 0 to MatrixWidth * MatrixHeight - 1 do
        begin
          SrcPixel := ReadIdealPixel(Src, I);
          Expected := ExpectedPixel(SrcPixel, Infos[F], IndexInfo);
          Actual := ReadIdealPixel(Dst, I);

          for Ch := Low(TPixelChannel) to High(TPixelChannel) do
          begin
            if not PairFailed and (Abs(Actual.Channels[Ch] - Expected.Channels[Ch]) >
              GetTolerance(Infos[F], IndexInfo, Ch, SrcPixel, Expected)) then
            begin
              PairFailed := True;
              Failures.Add(Fmt('%s: pixel %d channel %s: source %s expected %.6f but was %.6f',
                [PairName(Infos[F], IndexInfo), I, ChannelNames[Ch], IdealPixelToStr(SrcPixel),
                Expected.Channels[Ch], Actual.Channels[Ch]]));
            end;
          end;
        end;
      finally
        FreeImage(Src);
        FreeImage(Dst);
      end;
    end;

    if Failures.Count > 0 then
      Fail(JoinFailures(Failures, 'Images with few colors converted to indexed format differ'));
  finally
    Failures.Free;
  end;
end;

procedure TTestConversionIndexed.TestArbitraryColorsToIndexed;
const
  // As many pixels as palette entries, so each color can have its own entry
  Width = 16;
  Height = 16;
var
  Failures: TStringList;
  F, I: Integer;
  Ch: TPixelChannel;
  Src, Dst: TImageData;
  IndexInfo: TImageFormatInfo;
  SrcPixel, Expected, Actual: TIdealPixel;
  PairFailed: Boolean;
begin
  // Arbitrary colors and alphas, not more of them than palette entries:
  // all must be kept (with precision of 8-bit palette entries)
  CollectFormats;
  CheckTrue(GetImageFormatInfo(ifIndex8, IndexInfo), 'Index8 info');
  CheckEquals(Width * Height, IndexInfo.PaletteEntries, 'Pixels of test image and palette entries');
  CheckEquals($FF, GetOption(ImagingColorReductionMask), 'Color reduction uses all bits by default');
  Failures := TStringList.Create;
  try
    for F := 0 to High(Formats) do
    begin
      if Infos[F].HasGrayChannel or Infos[F].IsIndexed then
        Continue;

      NewTestImage(Formats[F], Width, Height, Src);
      InitImage(Dst);
      try
        CheckTrue(CloneImage(Src, Dst), 'CloneImage');
        CheckTrue(ConvertImage(Dst, ifIndex8), PairName(Infos[F], IndexInfo) + ': ConvertImage');
        CheckEquals('', CheckConvertedImage(Dst, IndexInfo, Width, Height),
          PairName(Infos[F], IndexInfo) + ': converted image');

        PairFailed := False;
        for I := 0 to Width * Height - 1 do
        begin
          SrcPixel := ReadIdealPixel(Src, I);
          Expected := ExpectedPixel(SrcPixel, Infos[F], IndexInfo);
          Actual := ReadIdealPixel(Dst, I);

          for Ch := Low(TPixelChannel) to High(TPixelChannel) do
          begin
            if not PairFailed and (Abs(Actual.Channels[Ch] - Expected.Channels[Ch]) >
              GetTolerance(Infos[F], IndexInfo, Ch, SrcPixel, Expected)) then
            begin
              PairFailed := True;
              Failures.Add(Fmt('%s: pixel %d channel %s: source %s expected %.6f but was %.6f',
                [PairName(Infos[F], IndexInfo), I, ChannelNames[Ch], IdealPixelToStr(SrcPixel),
                Expected.Channels[Ch], Actual.Channels[Ch]]));
            end;
          end;
        end;
      finally
        FreeImage(Src);
        FreeImage(Dst);
      end;
    end;

    if Failures.Count > 0 then
      Fail(JoinFailures(Failures, 'Images with up to 256 colors converted to indexed format differ'));
  finally
    Failures.Free;
  end;
end;

procedure TTestConversionIndexed.TestManyColorsToIndexed;
const
  Width = 64;
  Height = 64;
  // Coarse limit: colors are random so the palette cannot fit them well
  MaxMeanError = 0.1;
var
  Src, Dst: TImageData;
  IndexInfo: TImageFormatInfo;
  I: Integer;
  Ch: TPixelChannel;
  SrcPixel, Actual: TIdealPixel;
  ErrorSum: Double;
begin
  CheckTrue(GetImageFormatInfo(ifIndex8, IndexInfo), 'Index8 info');
  NewTestImage(ifR8G8B8, Width, Height, Src);
  InitImage(Dst);
  try
    CheckTrue(CloneImage(Src, Dst), 'CloneImage');
    CheckTrue(ConvertImage(Dst, ifIndex8), 'ConvertImage');
    CheckEquals('', CheckConvertedImage(Dst, IndexInfo, Width, Height), 'Converted image');

    ErrorSum := 0.0;
    for I := 0 to Width * Height - 1 do
    begin
      SrcPixel := ReadIdealPixel(Src, I);
      Actual := ReadIdealPixel(Dst, I);
      for Ch := chRed to chBlue do
        ErrorSum := ErrorSum + Abs(Actual.Channels[Ch] - SrcPixel.Channels[Ch]);
      CheckEquals(1.0, Actual.A, 0.0, 'Opaque pixels stay opaque, pixel ' + IntToStr(I));
    end;

    CheckTrue(ErrorSum / (Width * Height * 3) < MaxMeanError,
      Fmt('Mean error of channels %.4f is over %.4f', [ErrorSum / (Width * Height * 3), MaxMeanError]));
  finally
    FreeImage(Src);
    FreeImage(Dst);
  end;
end;

{ TTestConversionSpecial }

type
  TSpecialFormats = array of TImageFormat;

function CollectSpecialFormats: TSpecialFormats;
var
  Fmt: TImageFormat;
  Info: TImageFormatInfo;
begin
  SetLength(Result, 0);
  for Fmt := Low(TImageFormat) to High(TImageFormat) do
  begin
    if GetImageFormatInfo(Fmt, Info) and Info.IsSpecial then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Fmt;
    end;
  end;
end;

{ Returns error message when Image is not valid image in given format for
  source image of given dimensions.}
function CheckSpecialImage(const Image: TImageData; Format: TImageFormat;
  SrcWidth, SrcHeight: Integer): string;
var
  Width, Height: Integer;
begin
  Width := SrcWidth;
  Height := SrcHeight;
  // Block compressed formats store whole blocks of 4x4 pixels
  if Format in [ifDXT1, ifDXT3, ifDXT5, ifBTC, ifATI1N, ifATI2N] then
  begin
    Width := (Width + 3) div 4 * 4;
    Height := (Height + 3) div 4 * 4;
  end;

  Result := '';
  if Image.Format <> Format then
    Result := 'format is ' + GetFormatName(Image.Format)
  else if (Image.Width <> Width) or (Image.Height <> Height) then
    Result := Fmt('dimensions are %dx%d and not %dx%d', [Image.Width, Image.Height, Width, Height])
  else if Image.Size <> GetPixelsSize(Format, Width, Height) then
    Result := Fmt('size is %d', [Image.Size])
  else if not TestImage(Image) then
    Result := 'TestImage failed'
  else if Image.Bits = nil then
    Result := 'no bits';
end;

procedure TTestConversionSpecial.TestInvariants;
const
  Widths: array[0..1] of Integer = (16, 17);
  Heights: array[0..1] of Integer = (8, 5);
var
  Failures: TStringList;
  Specials: TSpecialFormats;
  Size, F, S, S2: Integer;
  Image: TImageData;
  Name, Error: string;

  procedure Convert(Format: TImageFormat; SrcWidth, SrcHeight: Integer);
  begin
    if Error <> '' then
      Exit;
    try
      if not ConvertImage(Image, Format) then
        Error := 'ConvertImage to ' + GetFormatName(Format) + ' returned False'
      else
        Error := CheckSpecialImage(Image, Format, SrcWidth, SrcHeight);
    except
      on E: Exception do
        Error := E.ClassName + ': ' + E.Message;
    end;
    if Error <> '' then
      Error := 'after conversion to ' + GetFormatName(Format) + ': ' + Error;
  end;

begin
  CollectFormats;
  Specials := CollectSpecialFormats;
  CheckTrue(Length(Specials) >= 7, 'Special formats found');
  Failures := TStringList.Create;
  try
    for Size := Low(Widths) to High(Widths) do
    begin
      for S := 0 to High(Specials) do
      begin
        // Each regular format to special one and back
        for F := 0 to High(Formats) do
        begin
          Name := Fmt('%s -> %s -> %s (%dx%d)', [GetFormatName(Formats[F]),
            GetFormatName(Specials[S]), GetFormatName(Formats[F]), Widths[Size], Heights[Size]]);
          Error := '';
          NewTestImage(Formats[F], Widths[Size], Heights[Size], Image);
          try
            Convert(Specials[S], Widths[Size], Heights[Size]);
            // Dimensions enlarged to whole blocks are kept
            Convert(Formats[F], Image.Width, Image.Height);
            if Error <> '' then
              Failures.Add(Name + ': ' + Error);
          finally
            FreeImage(Image);
          end;
        end;

        // Special format to another special format
        for S2 := 0 to High(Specials) do
        begin
          if S = S2 then
            Continue;

          Name := Fmt('%s -> %s (%dx%d)', [GetFormatName(Specials[S]),
            GetFormatName(Specials[S2]), Widths[Size], Heights[Size]]);
          Error := '';
          NewTestImage(ifA8R8G8B8, Widths[Size], Heights[Size], Image);
          try
            Convert(Specials[S], Widths[Size], Heights[Size]);
            Convert(Specials[S2], Image.Width, Image.Height);
            if Error <> '' then
              Failures.Add(Name + ': ' + Error);
          finally
            FreeImage(Image);
          end;
        end;
      end;
    end;

    if Failures.Count > 0 then
      Fail(JoinFailures(Failures, 'Conversion with special format failed or gave invalid image'));
  finally
    Failures.Free;
  end;
end;

procedure TTestConversionSpecial.TestBinaryKeepsTwoLevels;
const
  // Width that is not a multiple of 8: rows of binary image are padded
  Width = 17;
  Height = 5;
var
  Image, Original: TImageData;
  I: Integer;
begin
  InitImage(Image);
  InitImage(Original);
  try
    CheckTrue(NewImage(Width, Height, ifGray8, Image), 'NewImage');
    for I := 0 to Width * Height - 1 do
    begin
      if (I * 7 + I div 3) mod 5 < 2 then
        PBuffer(Image.Bits)[I] := 255;
    end;
    CheckTrue(CloneImage(Image, Original), 'CloneImage');

    CheckTrue(ConvertImage(Image, ifBinary), 'ConvertImage to Binary');
    CheckEquals('', CheckSpecialImage(Image, ifBinary, Width, Height), 'Binary image');
    CheckEquals(Int64(3 * Height), Image.Size, 'Size of binary image');

    CheckTrue(ConvertImage(Image, ifGray8), 'ConvertImage back to Gray8');
    CheckImagesEqual(Original, Image, 'Black and white image after conversion to Binary and back');
  finally
    FreeImage(Image);
    FreeImage(Original);
  end;
end;

procedure TTestConversionSpecial.TestColorBlockFormatsKeepFlatBlocks;
const
  Width = 16;
  Height = 8;
  BlockColors: array[0..7] of UInt32 = ($FFFFFFFF, $FF000000, $FFFF0000, $FF00FF00,
    $FF0000FF, $FF808080, $FF204060, $FFC0A0E0);
  BlockAlphas: array[0..7] of Byte = ($FF, $00, $80, $40, $C0, $FF, $10, $E0);
  TestFormats: array[0..2] of TImageFormat = (ifDXT1, ifDXT3, ifDXT5);
  // Colors are stored as R5G6B5, alpha in 4 bits by DXT3 and in 8 bits by DXT5.
  // One more step of 8 bits is for rounding on the way.
  ColorTolerances: array[TPixelChannel] of Double = (0.0, 1 / 31 + 1 / 255, 1 / 63 + 1 / 255, 1 / 31 + 1 / 255);
  AlphaTolerances: array[0..2] of Double = (0.0, 1 / 15 + 1 / 255, 2 / 255);
var
  F, X, Y, Block: Integer;
  Ch: TPixelChannel;
  Image, Original: TImageData;
  Color: TColor32Rec;
  Expected, Actual: TIdealPixel;
  Tolerance: Double;
  Name: string;
begin
  for F := Low(TestFormats) to High(TestFormats) do
  begin
    Name := GetFormatName(TestFormats[F]);
    InitImage(Image);
    InitImage(Original);
    try
      CheckTrue(NewImage(Width, Height, ifA8R8G8B8, Image), 'NewImage');
      for Y := 0 to Height - 1 do
      begin
        for X := 0 to Width - 1 do
        begin
          Block := (Y div 4) * (Width div 4) + X div 4;
          Color.Color := BlockColors[Block];
          // DXT1 is tested with opaque pixels only
          if TestFormats[F] <> ifDXT1 then
            Color.A := BlockAlphas[Block];
          PUInt32Array(Image.Bits)[Y * Width + X] := Color.Color;
        end;
      end;
      CheckTrue(CloneImage(Image, Original), 'CloneImage');

      CheckTrue(ConvertImage(Image, TestFormats[F]), Name + ': ConvertImage');
      CheckEquals('', CheckSpecialImage(Image, TestFormats[F], Width, Height), Name + ': compressed image');
      CheckTrue(ConvertImage(Image, ifA8R8G8B8), Name + ': ConvertImage back');
      CheckEquals(Width, Image.Width, Name + ': width after decompression');
      CheckEquals(Height, Image.Height, Name + ': height after decompression');

      for Y := 0 to Width * Height - 1 do
      begin
        Expected := ReadIdealPixel(Original, Y);
        Actual := ReadIdealPixel(Image, Y);

        for Ch := Low(TPixelChannel) to High(TPixelChannel) do
        begin
          if Ch = chAlpha then
            Tolerance := AlphaTolerances[F]
          else
            Tolerance := ColorTolerances[Ch];

          CheckEquals(Expected.Channels[Ch], Actual.Channels[Ch], Tolerance + ExactEpsilon,
            Fmt('%s: pixel %d channel %s of %s', [Name, Y, ChannelNames[Ch], IdealPixelToStr(Expected)]));
        end;
      end;
    finally
      FreeImage(Image);
      FreeImage(Original);
    end;
  end;
end;

procedure TTestConversionSpecial.TestGrayBlockFormatsKeepFlatBlocks;
const
  Width = 16;
  Height = 8;
  BlockGrays: array[0..7] of Byte = (0, 255, 128, 64, 200, 17, 90, 231);
  TestFormats: array[0..1] of TImageFormat = (ifATI1N, ifBTC);
  Tolerance = 2 / 255;
var
  F, X, Y: Integer;
  Image, Original: TImageData;
  Name: string;
begin
  for F := Low(TestFormats) to High(TestFormats) do
  begin
    Name := GetFormatName(TestFormats[F]);
    InitImage(Image);
    InitImage(Original);
    try
      CheckTrue(NewImage(Width, Height, ifGray8, Image), 'NewImage');
      for Y := 0 to Height - 1 do
      begin
        for X := 0 to Width - 1 do
          PBuffer(Image.Bits)[Y * Width + X] := BlockGrays[(Y div 4) * (Width div 4) + X div 4];
      end;
      CheckTrue(CloneImage(Image, Original), 'CloneImage');

      CheckTrue(ConvertImage(Image, TestFormats[F]), Name + ': ConvertImage');
      CheckEquals('', CheckSpecialImage(Image, TestFormats[F], Width, Height), Name + ': compressed image');
      CheckTrue(ConvertImage(Image, ifGray8), Name + ': ConvertImage back');

      for Y := 0 to Width * Height - 1 do
      begin
        CheckEquals(ReadIdealPixel(Original, Y).R, ReadIdealPixel(Image, Y).R, Tolerance + ExactEpsilon,
          Fmt('%s: pixel %d', [Name, Y]));
      end;
    finally
      FreeImage(Image);
      FreeImage(Original);
    end;
  end;
end;

initialization
  RegisterImagingTest(TTestConversionMatrix);
  RegisterImagingTest(TTestConversionIndexed);
  RegisterImagingTest(TTestConversionSpecial);
end.
