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
  This unit contains functions that measure images: statistics of channels,
  checks of image content, histograms, and comparison of two images
  (with search for the data format they can be compared in).

  Values are in units of samples stored in the image (0..255 for 8-bit
  channels, 0..65535 for 16-bit ones, the stored value for floating point
  channels), images are not converted to other data formats.

  Results for channels are indexed by TColorChannel. Formats that store
  luminance (ifGray8, ifA16Gray16, ...) report it as red, green, and blue
  alike, the way pixel access functions do, ChannelCount of the result is
  the number of channels the format really stores.
}
unit ImagingAnalysis;

{$I ImagingOptions.inc}

interface

uses
  Types, SysUtils, Math, ImagingTypes, Imaging, ImagingFormats, ImagingUtility;

type
  { Number of pixels for each value of 8-bit channel.}
  THistogramArray = array[Byte] of Int64;

  { Statistics of samples of one channel.}
  TChannelStatistics = record
    Min: Double;       // The smallest sample
    Max: Double;       // The largest sample
    Mean: Double;      // Average of samples
    StdDev: Double;    // Standard deviation of samples (of population)
    Peak: Double;      // The largest value channel can hold, for floating
                       // point formats the one given by caller
    LowCount: Int64;   // Number of samples that are zero or less
    HighCount: Int64;  // Number of samples that are Peak or more
  end;

  TImageStatistics = record
    PixelCount: Int64;
    ChannelCount: Integer; // Number of channels stored by the data format
    HasChannel: array[TColorChannel] of Boolean;
    Channels: array[TColorChannel] of TChannelStatistics;
  end;

  { Differences of samples of one channel (or of all channels together)
    of two images.}
  TChannelDifference = record
    MSE: Double;       // Mean squared error
    RMSE: Double;      // Root of mean squared error
    MAE: Double;       // Mean absolute error
    PAE: Double;       // Peak absolute error (the largest difference)
    PSNR: Double;      // Peak signal to noise ratio in dB, infinity when
                       // there is no difference
    MeanError: Double; // Average of signed differences (second image minus
                       // the first one), shows shift of brightness
    DiffCount: Int64;  // Number of samples that differ by more than tolerance
  end;

  TImageDifference = record
    PixelCount: Int64;
    DiffPixelCount: Int64; // Number of pixels where some channel differs
                           // by more than tolerance
    DiffRect: TRect;       // Bounds of these pixels, empty when there are none
    ChannelCount: Integer; // Number of channels stored by the data format
    HasChannel: array[TColorChannel] of Boolean;
    Channels: array[TColorChannel] of TChannelDifference;
    All: TChannelDifference; // Stored samples of all channels together
  end;

{ Computes statistics of channels of the image. Works for all data formats but
  the special ones. Channels of indexed formats are taken from the palette,
  channels of packed formats (ifR5G6B5, ...) are in units of their bit depth.

  LowCount and HighCount tell how many samples are clipped at the limits. Peak
  of channels of floating point formats is FloatPeak, their Min and Max give
  the range of values that is really used. Floating point samples must be
  finite numbers, 64-bit samples are processed with the precision of Double.}
function ComputeImageStatistics(const Image: TImageData; out Stats: TImageStatistics;
  const FloatPeak: Double = 1.0): Boolean;

{ Compares two images with the same dimensions and data format. Samples that
  differ by more than Tolerance are counted for channels (DiffCount) and
  pixels (DiffPixelCount), DiffRect gives bounds of such pixels.

  All formats with channels stored as whole 1, 2, 4, or 8 byte samples are
  supported, unused bytes of pixels (ifX8R8G8B8) are skipped. Returns False
  and zeroed result for other formats (indexed, special, and packed ones like
  ifR5G6B5), for invalid images, and for images that differ in dimensions
  or format.

  Peak of PSNR is the largest value sample of integer formats can hold,
  for floating point formats it is given by FloatPeak. Floating point samples
  must be finite numbers, unless they are stored identically in both images.}
function CompareImages(const Image1, Image2: TImageData; out Diff: TImageDifference;
  const Tolerance: Double = 0.0; const FloatPeak: Double = 1.0): Boolean;

{ Computes error metrics of two images over samples of all their channels
  together (alpha has the same weight as color channels):
    MSE  - mean squared error,
    RMSE - root of mean squared error,
    MAE  - mean absolute error,
    PAE  - peak absolute error (the largest difference of two samples),
    PSNR - peak signal to noise ratio in dB, infinity for identical images.

  This is TImageDifference.All of CompareImages, look there for supported
  formats and other details. Returns False and zero metrics when the images
  cannot be compared.}
function ComputeErrorMetrics(const Image1, Image2: TImageData;
  var PSNR, MSE, RMSE, PAE, MAE: Double; const FloatPeak: Double = 1.0): Boolean;

{ Returns data format that images in the two given formats can be converted to
  for comparison with the least loss of information. It is always a format
  CompareImages supports and it has:
    - color channels when one of the formats has them (indexed and packed
      formats do), luminance only otherwise,
    - alpha channel when one of the formats has it (indexed formats do),
    - 32-bit floating point samples when one of the formats has floating
      point ones (16-bit floating point formats are never chosen),
    - integer samples large enough for both formats otherwise.

  Format that both images already have is returned as it is when CompareImages
  supports it. Special formats are taken as the regular formats they are
  decompressed to. Some combinations have no format that holds everything:
  32 and 64-bit luminance with color, alpha, or floating point samples gives
  32-bit floating point format, which keeps 24 bits of the luminance.
  Returns ifUnknown when one of the formats is not valid.}
function FindCommonFormat(Format1, Format2: TImageFormat): TImageFormat;
{ Creates copies of the two images converted to the data format given by
  FindCommonFormat, so they can be passed to CompareImages. The source images
  are not changed, Common1 and Common2 must be other variables than them
  and are to be freed by the caller (images they hold on entry are freed).
  Returns False and frees both copies when the images are not valid
  or cannot be converted.}
function ConvertToCommonFormat(const Image1, Image2: TImageData;
  var Common1, Common2: TImageData): Boolean;

{ Returns True when the image has no alpha channel or alpha of all its pixels
  is at the maximum (1.0 or more in floating point formats). False also
  for invalid images and special data formats.}
function IsImageOpaque(const Image: TImageData): Boolean;
{ Returns True when the image has no color: its data format has one color
  channel (luminance, or red only in floating point formats), or red, green,
  and blue are the same in all its pixels. Pixels of packed formats (ifR5G6B5,
  ...) are compared after conversion to 8 bits per channel. False also for
  invalid images and special data formats.}
function IsImageLuminanceOnly(const Image: TImageData): Boolean;
{ Returns number of different colors (alpha included) used by pixels of the
  image, or -1 for invalid images and special data formats. When Limit is
  greater than zero the counting stops as soon as there are more colors and
  Limit + 1 is returned: use it to find out cheaply if the image fits
  a palette. Counting of all colors needs up to 16 bytes of memory
  for each pixel.}
function CountImageColors(const Image: TImageData; Limit: Integer = 0): Int64;

{ Computes histograms of channels and of luma of pixels inside the given
  rectangle. Pixels are converted to 8 bits per channel, luma is the weighted
  sum of red, green, and blue. Works for all data formats but
  the special ones.}
function ComputeHistogram(const Image: TImageData; const Rect: TRect;
  out Red, Green, Blue, Alpha, Luma: THistogramArray): Boolean; overload;
{ Computes histograms of channels and of luma of all pixels of the image.}
function ComputeHistogram(const Image: TImageData;
  out Red, Green, Blue, Alpha, Luma: THistogramArray): Boolean; overload;

implementation

type
  TSampleKind = (skUInt8, skUInt16, skUInt32, skUInt64, skHalf, skSingle,
    skPacked, skIndexed);

  { Values and counters for slots of TPixelLayout, only the first SlotCount
    items are used.}
  TSlotValues = array[0..3] of Double;
  TSlotCounts = array[0..3] of Int64;

  { Describes how pixels of data format are stored.

    A slot is one value that a pixel of the format really stores. Slots are
    numbered from zero in the order ReadPixel returns them, which is not
    the order of TColorChannel:
      - formats made of whole samples have one slot for each sample, in the
        order the samples lie in memory: ifA8R8G8B8 has slots blue, green,
        red, alpha, the formats with red and blue swapped (ifA16B16G16R16)
        red, green, blue, alpha. Unused bytes (ifX8R8G8B8) have no slot,
      - luminance formats have slot 0 for luminance and slot 1 for alpha when
        they have it (ifA8Gray8). Luminance slot has channel ccRed, the same
        as the only slot of floating point formats with red channel only,
      - packed formats (ifR5G6B5) have slots blue, green, red, alpha for
        the channels that have some bits, read by SlotMask and SlotShift,
      - indexed formats have slots blue, green, red, alpha of the palette
        entry the pixel points to.

    Functions do their sums for slots and map them to channels of results
    by SlotChannel at the end. This is where luminance (IsLuminance) is
    copied to green and blue too, so it is processed once and reported
    as all three color channels.}
  TPixelLayout = record
    Info: TImageFormatInfo;
    Kind: TSampleKind;
    BytesPerPixel: Integer;
    SampleSize: Integer;     // Bytes of one slot, zero for packed and
                             // indexed formats
    SlotCount: Integer;      // Number of stored channels, 1 to 4
    SlotChannel: array[0..3] of TColorChannel; // Channel the slot holds
    SlotPeak: TSlotValues;   // The largest value the slot can hold: by size
                             // of the sample or number of bits, peak given
                             // by caller for floating point formats
    SlotMask: array[0..3] of UInt32;  // Bits of the slot in pixel of packed
                                      // format
    SlotShift: array[0..3] of Byte;   // Position of the lowest of these bits
    PackedMask: UInt32;      // Bits of all slots of packed format (pixel
                             // without unused bits)
    IsLuminance: Boolean;    // Slot with ccRed holds luminance
  end;

  TColorKey = array[0..15] of Byte;
  TDynNativeIntArray = array of NativeInt;

const
  // Typed so the math is done in double precision by all compilers
  Ln10: Double = 2.302585092994046;
  TwoPow32: Double = 4294967296.0;
  PeakUInt8: Double = 255.0;
  PeakUInt16: Double = 65535.0;
  PeakUInt32: Double = 4294967295.0;
  PeakUInt64: Double = 18446744073709551615.0;

  NoSlot = -1;
  FnvOffset = UInt32($811C9DC5);
  FnvPrime = 16777619;
  // Colors with keys up to this size are counted by flags, not by hashing
  MaxDirectKeySize = 2;
  InitialTableSize = 1024;

{ Finds out how pixels of the format are stored. Returns False for formats
  the functions of this unit cannot read.}
function GetPixelLayout(const Info: TImageFormatInfo; const FloatPeak: Double;
  out Layout: TPixelLayout): Boolean;

  { Adds the next slot: slots must be added in the order ReadPixel reads them.}
  procedure AddSlot(Channel: TColorChannel; const Peak: Double);
  begin
    Layout.SlotChannel[Layout.SlotCount] := Channel;
    Layout.SlotPeak[Layout.SlotCount] := Peak;
    Inc(Layout.SlotCount);
  end;

  procedure AddPackedSlot(Channel: TColorChannel; BitCount: Integer; Mask: UInt32;
    Shift: Byte);
  begin
    if BitCount = 0 then
      Exit;

    Layout.SlotMask[Layout.SlotCount] := Mask;
    Layout.SlotShift[Layout.SlotCount] := Shift;
    Layout.PackedMask := Layout.PackedMask or Mask;
    AddSlot(Channel, (Int64(1) shl BitCount) - 1);
  end;

var
  Peak: Double;
begin
  Result := False;
  FillChar(Layout, SizeOf(Layout), 0);

  if Info.IsSpecial or (Info.BytesPerPixel = 0) or (Info.ChannelCount = 0) then
    Exit;

  Layout.Info := Info;
  Layout.BytesPerPixel := Info.BytesPerPixel;

  if Info.IsIndexed then
  begin
    if Info.BytesPerPixel <> 1 then
      Exit;

    // Channels of palette entries
    Layout.Kind := skIndexed;
    AddSlot(ccBlue, PeakUInt8);
    AddSlot(ccGreen, PeakUInt8);
    AddSlot(ccRed, PeakUInt8);
    AddSlot(ccAlpha, PeakUInt8);
  end
  else if Info.UsePixelFormat then
  begin
    if (Info.PixelFormat = nil) or (Info.BytesPerPixel > SizeOf(UInt32)) then
      Exit;

    Layout.Kind := skPacked;
    AddPackedSlot(ccBlue, Info.PixelFormat.BBitCount, Info.PixelFormat.BBitMask,
                  Info.PixelFormat.BShift);
    AddPackedSlot(ccGreen, Info.PixelFormat.GBitCount, Info.PixelFormat.GBitMask,
                  Info.PixelFormat.GShift);
    AddPackedSlot(ccRed, Info.PixelFormat.RBitCount, Info.PixelFormat.RBitMask,
                  Info.PixelFormat.RShift);
    AddPackedSlot(ccAlpha, Info.PixelFormat.ABitCount, Info.PixelFormat.ABitMask,
                  Info.PixelFormat.AShift);

    if Layout.SlotCount = 0 then
      Exit;
  end
  else
  begin
    // Pixels can have unused bytes after the channels (X8R8G8B8)
    Layout.SampleSize := Info.BytesPerPixel div Info.ChannelCount;

    if Info.IsFloatingPoint then
    begin
      if FloatPeak <= 0 then
        Exit;

      Peak := FloatPeak;
      case Layout.SampleSize of
        2: Layout.Kind := skHalf;
        4: Layout.Kind := skSingle;
      else
        Exit;
      end;
    end
    else
    begin
      case Layout.SampleSize of
        1:
          begin
            Layout.Kind := skUInt8;
            Peak := PeakUInt8;
          end;
        2:
          begin
            Layout.Kind := skUInt16;
            Peak := PeakUInt16;
          end;
        4:
          begin
            Layout.Kind := skUInt32;
            Peak := PeakUInt32;
          end;
        8:
          begin
            Layout.Kind := skUInt64;
            Peak := PeakUInt64;
          end;
      else
        Exit;
      end;
    end;

    // Samples in the order they are stored
    if Info.HasGrayChannel then
    begin
      Layout.IsLuminance := True;
      AddSlot(ccRed, Peak);
      if Info.HasAlphaChannel then
        AddSlot(ccAlpha, Peak);
    end
    else if Info.ChannelCount = 1 then
      AddSlot(ccRed, Peak)
    else if Info.ChannelCount in [3, 4] then
    begin
      if Info.IsRBSwapped then
        AddSlot(ccRed, Peak)
      else
        AddSlot(ccBlue, Peak);

      AddSlot(ccGreen, Peak);

      if Info.IsRBSwapped then
        AddSlot(ccBlue, Peak)
      else
        AddSlot(ccRed, Peak);

      if Info.ChannelCount = 4 then
        AddSlot(ccAlpha, Peak);
    end;

    if Layout.SlotCount <> Info.ChannelCount then
      Exit;
  end;

  Result := True;
end;

{ Checks the image and finds out how its pixels are stored.}
function GetImageLayout(const Image: TImageData; const FloatPeak: Double;
  out Layout: TPixelLayout): Boolean;
var
  Info: TImageFormatInfo;
begin
  Result := TestImage(Image) and (Image.Width > 0) and (Image.Height > 0) and
    GetImageFormatInfo(Image.Format, Info) and GetPixelLayout(Info, FloatPeak, Layout) and
    ((Layout.Kind <> skIndexed) or (Image.Palette <> nil));
end;

{ Returns index of the slot that holds the channel, or NoSlot when the format
  does not store it. Luminance is found as ccRed.}
function FindSlot(const Layout: TPixelLayout; Channel: TColorChannel): Integer;
var
  I: Integer;
begin
  Result := NoSlot;

  for I := 0 to Layout.SlotCount - 1 do
  begin
    if Layout.SlotChannel[I] = Channel then
      Result := I;
  end;
end;

{ Reads values of all slots of one pixel in units of stored samples.
  Items of Values after SlotCount are not touched.}
procedure ReadPixel(P: PByte; const Layout: TPixelLayout; Palette: PPalette32;
  var Values: TSlotValues);
var
  I: Integer;
  Packed32, High32, Low32: UInt32;
begin
  case Layout.Kind of
    skUInt8:
      for I := 0 to Layout.SlotCount - 1 do
        Values[I] := PBuffer(P)[I];
    skUInt16:
      for I := 0 to Layout.SlotCount - 1 do
        Values[I] := PWordBuffer(P)[I];
    skUInt32:
      for I := 0 to Layout.SlotCount - 1 do
        Values[I] := PUInt32Buffer(P)[I];
    skUInt64:
      for I := 0 to Layout.SlotCount - 1 do
      begin
        Low32 := PUInt32Buffer(P)[I * 2];
        High32 := PUInt32Buffer(P)[I * 2 + 1];
        Values[I] := High32 * TwoPow32 + Low32;
      end;
    skHalf:
      for I := 0 to Layout.SlotCount - 1 do
        Values[I] := HalfToFloat(PWordBuffer(P)[I]);
    skSingle:
      for I := 0 to Layout.SlotCount - 1 do
        Values[I] := PSingleBuffer(P)[I];
    skPacked:
      begin
        Packed32 := 0;
        Move(P^, Packed32, Layout.BytesPerPixel);
        for I := 0 to Layout.SlotCount - 1 do
          Values[I] := (Packed32 and Layout.SlotMask[I]) shr Layout.SlotShift[I];
      end;
    skIndexed:
      begin
        Values[0] := Palette[P^].B;
        Values[1] := Palette[P^].G;
        Values[2] := Palette[P^].R;
        Values[3] := Palette[P^].A;
      end;
  end;
end;

{ Returns difference of two stored samples (the second minus the first).}
function SampleDifference(P1, P2: PByte; Kind: TSampleKind): Double; {$IFDEF USE_INLINE}inline;{$ENDIF}
var
  V1, V2: Double;
begin
  Result := 0;

  case Kind of
    skUInt8:
      Result := Integer(P2^) - Integer(P1^);
    skUInt16:
      Result := Integer(PWord(P2)^) - Integer(PWord(P1)^);
    skUInt32:
      begin
        V1 := PUInt32(P1)^;
        V2 := PUInt32(P2)^;
        Result := V2 - V1;
      end;
    skUInt64:
      begin
        // Differences of low and high halves are exact
        V1 := PUInt32Buffer(P1)[1];
        V2 := PUInt32Buffer(P2)[1];
        Result := (V2 - V1) * TwoPow32;
        V1 := PUInt32(P1)^;
        V2 := PUInt32(P2)^;
        Result := Result + (V2 - V1);
      end;
    skHalf:
      begin
        // Samples with the same bits are not subtracted,
        // they can be infinite
        if PWord(P1)^ <> PWord(P2)^ then
        begin
          V1 := HalfToFloat(PWord(P1)^);
          V2 := HalfToFloat(PWord(P2)^);
          Result := V2 - V1;
        end;
      end;
    skSingle:
      begin
        if PUInt32(P1)^ <> PUInt32(P2)^ then
        begin
          V1 := PSingle(P1)^;
          V2 := PSingle(P2)^;
          Result := V2 - V1;
        end;
      end;
  end;
end;

function ComputeImageStatistics(const Image: TImageData; out Stats: TImageStatistics;
  const FloatPeak: Double): Boolean;
var
  Layout: TPixelLayout;
  X, Y, I: Integer;
  P: PByte;
  Values, Shift, MinValue, MaxValue: TSlotValues;
  RowSum, RowSqr, Sum, Sqr: TSlotValues;
  LowCount, HighCount: TSlotCounts;
  V, PixelCount, Mean, Variance: Double;
  Channel: TColorChannel;
begin
  Result := False;
  FillChar(Stats, SizeOf(Stats), 0);

  if not GetImageLayout(Image, FloatPeak, Layout) then
    Exit;

  // Values are summed as differences from the first pixel, sums of large
  // values with small variance would lose precision otherwise
  P := Image.Bits;
  FillChar(Shift, SizeOf(Shift), 0);
  FillChar(Values, SizeOf(Values), 0);
  ReadPixel(P, Layout, Image.Palette, Shift);
  MinValue := Shift;
  MaxValue := Shift;
  FillChar(Sum, SizeOf(Sum), 0);
  FillChar(Sqr, SizeOf(Sqr), 0);
  FillChar(LowCount, SizeOf(LowCount), 0);
  FillChar(HighCount, SizeOf(HighCount), 0);

  for Y := 0 to Image.Height - 1 do
  begin
    // Rows are summed separately so small values are not lost
    // in the large sums of huge images
    FillChar(RowSum, SizeOf(RowSum), 0);
    FillChar(RowSqr, SizeOf(RowSqr), 0);

    for X := 0 to Image.Width - 1 do
    begin
      ReadPixel(P, Layout, Image.Palette, Values);

      for I := 0 to Layout.SlotCount - 1 do
      begin
        V := Values[I];

        if V < MinValue[I] then
          MinValue[I] := V;
        if V > MaxValue[I] then
          MaxValue[I] := V;
        if V <= 0 then
          Inc(LowCount[I]);
        if V >= Layout.SlotPeak[I] then
          Inc(HighCount[I]);

        V := V - Shift[I];
        RowSum[I] := RowSum[I] + V;
        RowSqr[I] := RowSqr[I] + V * V;
      end;

      Inc(P, Layout.BytesPerPixel);
    end;

    for I := 0 to Layout.SlotCount - 1 do
    begin
      Sum[I] := Sum[I] + RowSum[I];
      Sqr[I] := Sqr[I] + RowSqr[I];
    end;
  end;

  Stats.PixelCount := GetPixelCount(Image);
  Stats.ChannelCount := Layout.SlotCount;
  PixelCount := Stats.PixelCount;

  for I := 0 to Layout.SlotCount - 1 do
  begin
    Channel := Layout.SlotChannel[I];
    Mean := Sum[I] / PixelCount;
    Variance := Sqr[I] / PixelCount - Mean * Mean;
    if Variance < 0 then
      Variance := 0;

    Stats.HasChannel[Channel] := True;
    Stats.Channels[Channel].Min := MinValue[I];
    Stats.Channels[Channel].Max := MaxValue[I];
    Stats.Channels[Channel].Mean := Shift[I] + Mean;
    Stats.Channels[Channel].StdDev := Sqrt(Variance);
    Stats.Channels[Channel].Peak := Layout.SlotPeak[I];
    Stats.Channels[Channel].LowCount := LowCount[I];
    Stats.Channels[Channel].HighCount := HighCount[I];
  end;

  if Layout.IsLuminance then
  begin
    Stats.HasChannel[ccGreen] := True;
    Stats.HasChannel[ccBlue] := True;
    Stats.Channels[ccGreen] := Stats.Channels[ccRed];
    Stats.Channels[ccBlue] := Stats.Channels[ccRed];
  end;

  Result := True;
end;

{ Computes metrics from sums of differences of SampleCount samples.}
procedure SetChannelDifference(var Difference: TChannelDifference;
  const SumError, SumAbs, SumSqr, PeakError, SampleCount, Peak: Double; DiffCount: Int64);
begin
  Difference.MeanError := SumError / SampleCount;
  Difference.MAE := SumAbs / SampleCount;
  Difference.MSE := SumSqr / SampleCount;
  Difference.RMSE := Sqrt(Difference.MSE);
  Difference.PAE := PeakError;
  Difference.DiffCount := DiffCount;

  if Difference.MSE = 0 then
    Difference.PSNR := Math.Infinity
  else
    Difference.PSNR := 20 * Ln(Peak / Difference.RMSE) / Ln10;
end;

function CompareImages(const Image1, Image2: TImageData; out Diff: TImageDifference;
  const Tolerance, FloatPeak: Double): Boolean;
var
  Layout: TPixelLayout;
  X, Y, I, Padding: Integer;
  MinX, MinY, MaxX, MaxY: Integer;
  P1, P2: PByte;
  D, AbsD, PixelCount: Double;
  RowError, RowAbs, RowSqr, SumError, SumAbs, SumSqr, PeakError: TSlotValues;
  DiffCount: TSlotCounts;
  AllError, AllAbs, AllSqr, AllPeak: Double;
  AllCount: Int64;
  PixelDiffers: Boolean;
  Channel: TColorChannel;
begin
  Result := False;
  FillChar(Diff, SizeOf(Diff), 0);

  if Tolerance < 0 then
    Exit;
  if not TestImage(Image2) or (Image1.Width <> Image2.Width) or
    (Image1.Height <> Image2.Height) or (Image1.Format <> Image2.Format) then
    Exit;
  if not GetImageLayout(Image1, FloatPeak, Layout) or (Layout.Kind in [skPacked, skIndexed]) then
    Exit;

  P1 := Image1.Bits;
  P2 := Image2.Bits;
  Padding := Layout.BytesPerPixel - Layout.SlotCount * Layout.SampleSize;
  MinX := Image1.Width;
  MinY := Image1.Height;
  MaxX := -1;
  MaxY := -1;
  FillChar(SumError, SizeOf(SumError), 0);
  FillChar(SumAbs, SizeOf(SumAbs), 0);
  FillChar(SumSqr, SizeOf(SumSqr), 0);
  FillChar(PeakError, SizeOf(PeakError), 0);
  FillChar(DiffCount, SizeOf(DiffCount), 0);

  for Y := 0 to Image1.Height - 1 do
  begin
    // Rows are summed separately so small differences are not lost
    // in the large sums of huge images
    FillChar(RowError, SizeOf(RowError), 0);
    FillChar(RowAbs, SizeOf(RowAbs), 0);
    FillChar(RowSqr, SizeOf(RowSqr), 0);

    for X := 0 to Image1.Width - 1 do
    begin
      PixelDiffers := False;

      for I := 0 to Layout.SlotCount - 1 do
      begin
        D := SampleDifference(P1, P2, Layout.Kind);
        AbsD := Abs(D);
        RowError[I] := RowError[I] + D;
        RowAbs[I] := RowAbs[I] + AbsD;
        RowSqr[I] := RowSqr[I] + AbsD * AbsD;

        if AbsD > PeakError[I] then
          PeakError[I] := AbsD;

        if AbsD > Tolerance then
        begin
          Inc(DiffCount[I]);
          PixelDiffers := True;
        end;

        Inc(P1, Layout.SampleSize);
        Inc(P2, Layout.SampleSize);
      end;

      if PixelDiffers then
      begin
        Inc(Diff.DiffPixelCount);
        if X < MinX then
          MinX := X;
        if X > MaxX then
          MaxX := X;
        if Y < MinY then
          MinY := Y;
        MaxY := Y;
      end;

      Inc(P1, Padding);
      Inc(P2, Padding);
    end;

    for I := 0 to Layout.SlotCount - 1 do
    begin
      SumError[I] := SumError[I] + RowError[I];
      SumAbs[I] := SumAbs[I] + RowAbs[I];
      SumSqr[I] := SumSqr[I] + RowSqr[I];
    end;
  end;

  Diff.PixelCount := GetPixelCount(Image1);
  Diff.ChannelCount := Layout.SlotCount;
  if Diff.DiffPixelCount > 0 then
    Diff.DiffRect := Rect(MinX, MinY, MaxX + 1, MaxY + 1);

  PixelCount := Diff.PixelCount;
  AllError := 0;
  AllAbs := 0;
  AllSqr := 0;
  AllPeak := 0;
  AllCount := 0;

  for I := 0 to Layout.SlotCount - 1 do
  begin
    Channel := Layout.SlotChannel[I];
    Diff.HasChannel[Channel] := True;
    SetChannelDifference(Diff.Channels[Channel], SumError[I], SumAbs[I], SumSqr[I],
                         PeakError[I], PixelCount, Layout.SlotPeak[I], DiffCount[I]);

    AllError := AllError + SumError[I];
    AllAbs := AllAbs + SumAbs[I];
    AllSqr := AllSqr + SumSqr[I];
    AllCount := AllCount + DiffCount[I];
    if PeakError[I] > AllPeak then
      AllPeak := PeakError[I];
  end;

  // All slots of formats made of whole samples have the same peak
  SetChannelDifference(Diff.All, AllError, AllAbs, AllSqr, AllPeak,
                       PixelCount * Layout.SlotCount, Layout.SlotPeak[0], AllCount);

  if Layout.IsLuminance then
  begin
    Diff.HasChannel[ccGreen] := True;
    Diff.HasChannel[ccBlue] := True;
    Diff.Channels[ccGreen] := Diff.Channels[ccRed];
    Diff.Channels[ccBlue] := Diff.Channels[ccRed];
  end;

  Result := True;
end;

function ComputeErrorMetrics(const Image1, Image2: TImageData;
  var PSNR, MSE, RMSE, PAE, MAE: Double; const FloatPeak: Double): Boolean;
var
  Diff: TImageDifference;
begin
  // Result is zeroed when the images cannot be compared
  Result := CompareImages(Image1, Image2, Diff, 0.0, FloatPeak);
  PSNR := Diff.All.PSNR;
  MSE  := Diff.All.MSE;
  RMSE := Diff.All.RMSE;
  PAE  := Diff.All.PAE;
  MAE  := Diff.All.MAE;
end;

type
  { What data format needs to keep everything pixels of some format hold.}
  TFormatNeeds = record
    HasColor: Boolean;
    HasAlpha: Boolean;
    IsFloat: Boolean;
    SampleBits: Integer;  // The largest number of bits of a channel
  end;

{ Finds out what pixels of the format hold. Returns False for invalid formats.}
function GetFormatNeeds(Format: TImageFormat; out Needs: TFormatNeeds): Boolean;
var
  Info: TImageFormatInfo;
begin
  Result := False;
  FillChar(Needs, SizeOf(Needs), 0);

  if not GetImageFormatInfo(Format, Info) then
    Exit;

  if Info.IsSpecial then
  begin
    // Pixels are available only after decompression to this format
    if not GetImageFormatInfo(Info.SpecialNearestFormat, Info) or Info.IsSpecial then
      Exit;
  end;

  if (Info.BytesPerPixel = 0) or (Info.ChannelCount = 0) then
    Exit;

  if Info.IsIndexed then
  begin
    // Palette entries
    Needs.HasColor := True;
    Needs.HasAlpha := True;
    Needs.SampleBits := 8;
  end
  else if Info.UsePixelFormat then
  begin
    if Info.PixelFormat = nil then
      Exit;

    Needs.HasColor := True;
    Needs.HasAlpha := Info.PixelFormat.ABitCount > 0;
    Needs.SampleBits := Info.PixelFormat.ABitCount;
    if Info.PixelFormat.RBitCount > Needs.SampleBits then
      Needs.SampleBits := Info.PixelFormat.RBitCount;
    if Info.PixelFormat.GBitCount > Needs.SampleBits then
      Needs.SampleBits := Info.PixelFormat.GBitCount;
    if Info.PixelFormat.BBitCount > Needs.SampleBits then
      Needs.SampleBits := Info.PixelFormat.BBitCount;
  end
  else
  begin
    // Formats with one channel hold luminance, also the floating point ones
    Needs.HasColor := not Info.HasGrayChannel and (Info.ChannelCount > 1);
    Needs.HasAlpha := Info.HasAlphaChannel;
    Needs.IsFloat := Info.IsFloatingPoint;
    Needs.SampleBits := (Info.BytesPerPixel div Info.ChannelCount) * 8;
  end;

  Result := True;
end;

function FindCommonFormat(Format1, Format2: TImageFormat): TImageFormat;
var
  Needs1, Needs2: TFormatNeeds;
  Info: TImageFormatInfo;
  Layout: TPixelLayout;
  HasColor, HasAlpha, IsFloat: Boolean;
  IntegerBits: Integer;
begin
  Result := ifUnknown;
  if not GetFormatNeeds(Format1, Needs1) or not GetFormatNeeds(Format2, Needs2) then
    Exit;

  // Nothing to convert when images in the format can be compared directly
  if (Format1 = Format2) and GetImageFormatInfo(Format1, Info) and
    GetPixelLayout(Info, 1.0, Layout) and not (Layout.Kind in [skPacked, skIndexed]) then
  begin
    Result := Format1;
    Exit;
  end;

  HasColor := Needs1.HasColor or Needs2.HasColor;
  HasAlpha := Needs1.HasAlpha or Needs2.HasAlpha;
  IsFloat := Needs1.IsFloat or Needs2.IsFloat;
  IntegerBits := 0;

  if not Needs1.IsFloat then
    IntegerBits := Needs1.SampleBits;
  if not Needs2.IsFloat and (Needs2.SampleBits > IntegerBits) then
    IntegerBits := Needs2.SampleBits;

  // There are no integer formats with color or alpha and more than 16 bits
  // per channel, floating point samples keep the most of them
  if (IntegerBits > 16) and (HasColor or HasAlpha) then
    IsFloat := True;

  if IsFloat then
  begin
    if not HasColor and not HasAlpha then
      Result := ifR32F
    else if HasAlpha then
      Result := ifA32R32G32B32F
    else
      Result := ifR32G32B32F;
  end
  else if not HasColor then
  begin
    if HasAlpha then
    begin
      if IntegerBits <= 8 then
        Result := ifA8Gray8
      else
        Result := ifA16Gray16;
    end
    else if IntegerBits <= 8 then
      Result := ifGray8
    else if IntegerBits <= 16 then
      Result := ifGray16
    else if IntegerBits <= 32 then
      Result := ifGray32
    else
      Result := ifGray64;
  end
  else if IntegerBits <= 8 then
  begin
    if HasAlpha then
      Result := ifA8R8G8B8
    else
      Result := ifR8G8B8;
  end
  else if HasAlpha then
    Result := ifA16R16G16B16
  else
    Result := ifR16G16B16;
end;

function ConvertToCommonFormat(const Image1, Image2: TImageData;
  var Common1, Common2: TImageData): Boolean;
var
  Format: TImageFormat;
begin
  Result := False;
  FreeImage(Common1);
  FreeImage(Common2);

  if not TestImage(Image1) or not TestImage(Image2) then
    Exit;

  Format := FindCommonFormat(Image1.Format, Image2.Format);
  if Format = ifUnknown then
    Exit;

  Result := CloneImage(Image1, Common1) and CloneImage(Image2, Common2) and
    ((Common1.Format = Format) or ConvertImage(Common1, Format)) and
    ((Common2.Format = Format) or ConvertImage(Common2, Format));

  if not Result then
  begin
    FreeImage(Common1);
    FreeImage(Common2);
  end;
end;

function IsImageOpaque(const Image: TImageData): Boolean;
var
  Layout: TPixelLayout;
  X, Y, AlphaSlot: Integer;
  P: PByte;
  Values: TSlotValues;
begin
  Result := False;
  if not GetImageLayout(Image, 1.0, Layout) then
    Exit;

  Result := True;
  AlphaSlot := FindSlot(Layout, ccAlpha);
  if AlphaSlot = NoSlot then
    Exit;

  P := Image.Bits;
  FillChar(Values, SizeOf(Values), 0);

  for Y := 0 to Image.Height - 1 do
  begin
    for X := 0 to Image.Width - 1 do
    begin
      ReadPixel(P, Layout, Image.Palette, Values);

      if Values[AlphaSlot] < Layout.SlotPeak[AlphaSlot] then
      begin
        Result := False;
        Exit;
      end;

      Inc(P, Layout.BytesPerPixel);
    end;
  end;
end;

function IsImageLuminanceOnly(const Image: TImageData): Boolean;
var
  Layout: TPixelLayout;
  X, Y, RedSlot, GreenSlot, BlueSlot: Integer;
  P: PByte;
  Values: TSlotValues;
  Color32: TColor32Rec;
  HasColor: Boolean;
begin
  Result := False;
  if not GetImageLayout(Image, 1.0, Layout) then
    Exit;

  Result := True;
  RedSlot := FindSlot(Layout, ccRed);
  GreenSlot := FindSlot(Layout, ccGreen);
  BlueSlot := FindSlot(Layout, ccBlue);
  if (RedSlot = NoSlot) or (GreenSlot = NoSlot) or (BlueSlot = NoSlot) then
    Exit;

  P := Image.Bits;
  FillChar(Values, SizeOf(Values), 0);

  for Y := 0 to Image.Height - 1 do
  begin
    for X := 0 to Image.Width - 1 do
    begin
      if Layout.Kind = skPacked then
      begin
        // Channels can have different numbers of bits
        Color32 := Layout.Info.GetPixel32(P, @Layout.Info, Image.Palette);
        HasColor := (Color32.R <> Color32.G) or (Color32.G <> Color32.B);
      end
      else
      begin
        ReadPixel(P, Layout, Image.Palette, Values);
        HasColor := (Values[RedSlot] <> Values[GreenSlot]) or
          (Values[GreenSlot] <> Values[BlueSlot]);
      end;

      if HasColor then
      begin
        Result := False;
        Exit;
      end;

      Inc(P, Layout.BytesPerPixel);
    end;
  end;
end;

{ Returns size of the key that tells colors of pixels apart.}
function GetColorKeySize(const Layout: TPixelLayout): Integer;
begin
  if Layout.Kind in [skPacked, skIndexed] then
    Result := SizeOf(UInt32)
  else
    Result := Layout.SlotCount * Layout.SampleSize;
end;

{ Gets key of color of the pixel: its bytes without the unused ones,
  palette entry for indexed formats.}
procedure GetColorKey(P: PByte; const Layout: TPixelLayout; Palette: PPalette32;
  KeySize: Integer; var Key: TColorKey);
var
  Packed32: UInt32;
begin
  case Layout.Kind of
    skPacked:
      begin
        Packed32 := 0;
        Move(P^, Packed32, Layout.BytesPerPixel);
        Packed32 := Packed32 and Layout.PackedMask;
        Move(Packed32, Key, SizeOf(Packed32));
      end;
    skIndexed:
      Move(Palette[P^], Key, SizeOf(TColor32Rec));
  else
    Move(P^, Key, KeySize);
  end;
end;

function HashColorKey(const Key: TColorKey; KeySize: Integer): UInt32;
var
  I: Integer;
begin
  // FNV-1a, multiplied in 64 bits so it does not overflow
  Result := FnvOffset;
  for I := 0 to KeySize - 1 do
    Result := UInt32((Int64(Result xor Key[I]) * FnvPrime) and $FFFFFFFF);
end;

function CountImageColors(const Image: TImageData; Limit: Integer): Int64;
var
  Layout: TPixelLayout;
  KeySize: Integer;
  PixelCount, PixelIndex: NativeInt;
  P: PByte;
  Key, OtherKey: TColorKey;
  Seen: array of Boolean;
  // Indices of pixels with different colors plus one, zero for free entry
  Table: TDynNativeIntArray;
  TableMask: NativeInt;
  Count: Int64;

  function GetPixelPointer(Index: NativeInt): PByte;
  begin
    Result := @PBuffer(Image.Bits)[Index * Layout.BytesPerPixel];
  end;

  { Returns index of table entry for the key: the one that has it
    or the free one where it belongs.}
  function FindEntry(const AKey: TColorKey; CompareKeys: Boolean): NativeInt;
  begin
    Result := NativeInt(HashColorKey(AKey, KeySize)) and TableMask;

    while Table[Result] <> 0 do
    begin
      if CompareKeys then
      begin
        GetColorKey(GetPixelPointer(Table[Result] - 1), Layout, Image.Palette, KeySize, OtherKey);
        if CompareMem(@AKey, @OtherKey, KeySize) then
          Exit;
      end;

      Result := (Result + 1) and TableMask;
    end;
  end;

  procedure GrowTable;
  var
    OldTable: TDynNativeIntArray;
    I: NativeInt;
    EntryKey: TColorKey;
  begin
    OldTable := Table;
    Table := nil;
    SetLength(Table, Length(OldTable) * 2);
    TableMask := Length(Table) - 1;
    FillChar(EntryKey, SizeOf(EntryKey), 0);

    for I := 0 to Length(OldTable) - 1 do
    begin
      if OldTable[I] <> 0 then
      begin
        // All colors in the table are different
        GetColorKey(GetPixelPointer(OldTable[I] - 1), Layout, Image.Palette, KeySize, EntryKey);
        Table[FindEntry(EntryKey, False)] := OldTable[I];
      end;
    end;
  end;

var
  Entry: NativeInt;
  DirectIndex: Integer;
begin
  Result := -1;
  if not GetImageLayout(Image, 1.0, Layout) then
    Exit;

  Count := 0;
  KeySize := GetColorKeySize(Layout);
  PixelCount := GetPixelCount(Image);
  P := Image.Bits;
  FillChar(Key, SizeOf(Key), 0);
  FillChar(OtherKey, SizeOf(OtherKey), 0);

  if KeySize <= MaxDirectKeySize then
  begin
    // One flag for each possible color
    SetLength(Seen, 1 shl (KeySize * 8));

    for PixelIndex := 0 to PixelCount - 1 do
    begin
      GetColorKey(P, Layout, Image.Palette, KeySize, Key);
      DirectIndex := Key[0] or (Key[1] shl 8);

      if not Seen[DirectIndex] then
      begin
        Seen[DirectIndex] := True;
        Inc(Count);
        if (Limit > 0) and (Count > Limit) then
          Break;
      end;

      Inc(P, Layout.BytesPerPixel);
    end;
  end
  else
  begin
    // Hash table with open addressing, it is kept at most half full
    SetLength(Table, InitialTableSize);
    TableMask := Length(Table) - 1;

    for PixelIndex := 0 to PixelCount - 1 do
    begin
      GetColorKey(P, Layout, Image.Palette, KeySize, Key);
      Entry := FindEntry(Key, True);

      if Table[Entry] = 0 then
      begin
        Table[Entry] := PixelIndex + 1;
        Inc(Count);
        if (Limit > 0) and (Count > Limit) then
          Break;
        if Count * 2 > Length(Table) then
          GrowTable;
      end;

      Inc(P, Layout.BytesPerPixel);
    end;
  end;

  Result := Count;
end;

function ComputeHistogram(const Image: TImageData; const Rect: TRect;
  out Red, Green, Blue, Alpha, Luma: THistogramArray): Boolean;
var
  Info: TImageFormatInfo;
  X, Y, Left, Top, Right, Bottom: Integer;
  P: PByte;
  Color32: TColor32Rec;
begin
  Result := False;
  FillChar(Red,   SizeOf(Red), 0);
  FillChar(Green, SizeOf(Green), 0);
  FillChar(Blue,  SizeOf(Blue), 0);
  FillChar(Alpha, SizeOf(Alpha), 0);
  FillChar(Luma,  SizeOf(Luma), 0);

  if not TestImage(Image) or not GetImageFormatInfo(Image.Format, Info) then
    Exit;
  if Info.IsSpecial or (Info.BytesPerPixel = 0) or not Assigned(Info.GetPixel32) then
    Exit;

  Left := Rect.Left;
  Top := Rect.Top;
  Right := Rect.Right;
  Bottom := Rect.Bottom;
  if Left < 0 then
    Left := 0;
  if Top < 0 then
    Top := 0;
  if Right > Image.Width then
    Right := Image.Width;
  if Bottom > Image.Height then
    Bottom := Image.Height;

  for Y := Top to Bottom - 1 do
  begin
    if Left >= Right then
      Break;
    P := @PBuffer(Image.Bits)[(NativeInt(Y) * Image.Width + Left) * Info.BytesPerPixel];

    for X := Left to Right - 1 do
    begin
      Color32 := Info.GetPixel32(P, @Info, Image.Palette);

      Inc(Red[Color32.R]);
      Inc(Green[Color32.G]);
      Inc(Blue[Color32.B]);
      Inc(Alpha[Color32.A]);
      Inc(Luma[Round(GrayConv.R * Color32.R + GrayConv.G * Color32.G + GrayConv.B * Color32.B)]);

      Inc(P, Info.BytesPerPixel);
    end;
  end;

  Result := True;
end;

function ComputeHistogram(const Image: TImageData;
  out Red, Green, Blue, Alpha, Luma: THistogramArray): Boolean;
begin
  Result := ComputeHistogram(Image, Rect(0, 0, Image.Width, Image.Height),
                             Red, Green, Blue, Alpha, Luma);
end;

end.
