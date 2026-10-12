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

{ Independent model of pixels in memory for the tests.

  Pixels of images in regular data formats are decoded here to channel values
  only by the memory layout of the format (channel order, bit depths and bit
  masks). No pixel access or conversion function of the library is called, so
  the values can be used to check those functions. Also fills test images with
  defined content.}
unit ImagingTestPixels;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, ImagingTypes, Imaging, ImagingFormats, ImagingUtility;

type
  TPixelChannel = (chAlpha, chRed, chGreen, chBlue);

  { Channel values of one pixel. Integer channels are scaled to 0..1 where 1 is
    the largest value the channel can hold, floating point channels have their
    stored value. Alpha that is not stored is 1, pixels of grayscale formats
    have the gray value in all color channels, pixels of floating point formats
    with one channel have zero green and blue.}
  TIdealPixel = record
    case Integer of
      0: (A, R, G, B: Double);
      1: (Channels: array[TPixelChannel] of Double);
  end;

const
  ChannelNames: array[TPixelChannel] of string = ('A', 'R', 'G', 'B');

  { Number of pixels at the start of images filled by FillTestImage that hold
    all combinations of the smallest and the largest values of channels.}
  ExtremePixelCount = 16;

  { Bits of precision reported for channels of floating point formats.}
  SinglePrecisionBits = 24;
  HalfPrecisionBits = 11;

{ Returns number of bits the channel is stored in. It is zero for channels the
  format does not store. For color channels of grayscale formats it is the
  size of the gray channel, for all channels of indexed formats size of the
  palette entry channel, for floating point formats bits of precision.}
function ChannelBits(const Info: TImageFormatInfo; Channel: TPixelChannel): Integer;
{ Returns the smallest difference of two values the channel can hold around
  the given value (in units of TIdealPixel). Zero for channels the format
  does not store.}
function ChannelStep(const Info: TImageFormatInfo; Channel: TPixelChannel;
  const Value: Double): Double;
{ Returns True for floating point formats with red channel only.}
function IsOneChannelFloat(const Info: TImageFormatInfo): Boolean;

{ Decodes pixel with given index (Y * Width + X) of image in regular
  (not special) data format.}
function ReadIdealPixel(const Image: TImageData; Index: Integer): TIdealPixel;
function IdealPixelToStr(const Pixel: TIdealPixel): string;

{ Fills image in regular data format with test content given by Seed. The first
  ExtremePixelCount pixels have all channels (or palette entries they point to)
  at the smallest or the largest value in all combinations: 0 or 1 in floating
  point formats. Other pixels have pseudo-random bits in integer formats
  and pseudo-random picks from a fixed list of values between 0 and 1
  in floating point formats.}
procedure FillTestImage(var Image: TImageData; Seed: UInt32);
{ Fills all channels of image in floating point format with the given values,
  each pixel gets a different combination.}
procedure FillFloatImage(var Image: TImageData; const Values: array of Single);

implementation

const
  // Typed so divisions are done in double precision by all compilers.
  // FPC computes with single precision when a literal fits Single: intended
  // behavior, changed only by -CF64 option or $MINFPCONSTPREC directive
  // (see the note in ImagingUtility.ScaleValue).
  ByteMax: Double = 255.0;
  TwoPow32: Double = 4294967296.0;

  DefaultFloatValues: array[0..8] of Single =
    (0.0, 1.0, 0.5, 0.25, 0.75, 0.125, 0.003, 0.9, 0.3333);

type
  TElementKind = (ekPacked, ekInteger, ekSingle, ekHalf);

function NextRandomByte(var State: UInt32): Byte;
begin
  // Linear congruential generator, computed in 64 bits so it does not overflow
  State := UInt32((Int64(State) * 1664525 + 1013904223) and $FFFFFFFF);
  Result := (State shr 16) and $FF;
end;

{ Returns byte with all bits set when Bit is not zero.}
function ExtremeByte(Bit: Integer): Byte;
begin
  if Bit <> 0 then
    Result := $FF
  else
    Result := 0;
end;

function Pow2(Exponent: Integer): Double;
var
  I: Integer;
begin
  Result := 1.0;
  for I := 1 to Exponent do
    Result := Result * 2.0;
end;

function GetElementKind(const Info: TImageFormatInfo): TElementKind;
begin
  if Info.UsePixelFormat then
    Result := ekPacked
  else if not Info.IsFloatingPoint then
    Result := ekInteger
  else if Info.BytesPerPixel in [2, 8] then
    Result := ekHalf
  else
    Result := ekSingle;
end;

{ Returns size in bytes of one element (channel) of pixel, the whole pixel for
  packed formats.}
function GetElementSize(const Info: TImageFormatInfo): Integer;
begin
  case GetElementKind(Info) of
    ekPacked: Result := Info.BytesPerPixel;
    ekSingle: Result := SizeOf(Single);
    ekHalf:   Result := SizeOf(THalfFloat);
  else
    if Info.IsIndexed then
      Result := Info.BytesPerPixel
    else if Info.HasGrayChannel then
      Result := Info.BytesPerPixel div Info.ChannelCount
    else if Info.BytesPerPixel <= 4 then
      Result := 1
    else
      Result := 2;
  end;
end;

function IsOneChannelFloat(const Info: TImageFormatInfo): Boolean;
begin
  Result := Info.IsFloatingPoint and (Info.ChannelCount = 1);
end;

function ChannelBits(const Info: TImageFormatInfo; Channel: TPixelChannel): Integer;
begin
  Result := 0;

  if (Channel = chAlpha) and not Info.HasAlphaChannel then
    Exit;
  if (Channel in [chGreen, chBlue]) and IsOneChannelFloat(Info) then
    Exit;

  case GetElementKind(Info) of
    ekPacked:
      case Channel of
        chAlpha: Result := Info.PixelFormat.ABitCount;
        chRed:   Result := Info.PixelFormat.RBitCount;
        chGreen: Result := Info.PixelFormat.GBitCount;
        chBlue:  Result := Info.PixelFormat.BBitCount;
      end;
    ekSingle: Result := SinglePrecisionBits;
    ekHalf:   Result := HalfPrecisionBits;
  else
    if Info.IsIndexed then
      Result := 8
    else
      Result := GetElementSize(Info) * 8;
  end;
end;

function ChannelStep(const Info: TImageFormatInfo; Channel: TPixelChannel;
  const Value: Double): Double;
var
  Bits: Integer;
  Magnitude: Double;
begin
  Bits := ChannelBits(Info, Channel);

  if Bits = 0 then
    Result := 0.0
  else if Info.IsFloatingPoint then
  begin
    // A few units in the last place of values with magnitude of one or more
    Magnitude := Abs(Value);
    if Magnitude < 1.0 then
      Magnitude := 1.0;
    Result := Magnitude * 4.0 / Pow2(Bits);
  end
  else
    Result := 1.0 / (Pow2(Bits) - 1.0);
end;

function ReadUnsigned(P: PByte; Size: Integer): Double;
var
  B: Byte;
  W: Word;
  D: array[0..1] of UInt32;
begin
  case Size of
    1:
      begin
        Move(P^, B, SizeOf(B));
        Result := B;
      end;
    2:
      begin
        Move(P^, W, SizeOf(W));
        Result := W;
      end;
    4:
      begin
        Move(P^, D[0], SizeOf(UInt32));
        Result := D[0];
      end;
    8:
      begin
        Move(P^, D, SizeOf(D));
        Result := D[1] * TwoPow32 + D[0];
      end;
  else
    raise Exception.CreateFmt('Unsupported element size: %d', [Size]);
  end;
end;

function ReadFloat(P: PByte; Kind: TElementKind): Double;
var
  S: Single;
  H: THalfFloat;
begin
  if Kind = ekSingle then
  begin
    Move(P^, S, SizeOf(S));
    Result := S;
  end
  else
  begin
    Move(P^, H, SizeOf(H));
    Result := HalfToFloat(H);
  end;
end;

function ReadIdealPixel(const Image: TImageData; Index: Integer): TIdealPixel;
var
  Info: TImageFormatInfo;
  Kind: TElementKind;
  P: PByte;
  ElemSize, ElemCount, I: Integer;
  MaxValue, Tmp: Double;
  Elems: array[0..3] of Double;
  Packed32: UInt32;
  PalEntry: TColor32Rec;
begin
  if not GetImageFormatInfo(Image.Format, Info) or Info.IsSpecial then
    raise Exception.Create('ReadIdealPixel: image is not in regular data format');

  P := @PBuffer(Image.Bits)[PtrInt(Index) * Info.BytesPerPixel];
  Kind := GetElementKind(Info);
  ElemSize := GetElementSize(Info);
  Result.A := 1.0;
  Result.R := 0.0;
  Result.G := 0.0;
  Result.B := 0.0;

  if Info.IsIndexed then
  begin
    PalEntry := Image.Palette[Round(ReadUnsigned(P, ElemSize))];
    Result.A := PalEntry.A / ByteMax;
    Result.R := PalEntry.R / ByteMax;
    Result.G := PalEntry.G / ByteMax;
    Result.B := PalEntry.B / ByteMax;
    Exit;
  end;

  if Kind = ekPacked then
  begin
    Packed32 := Round(ReadUnsigned(P, ElemSize));
    with Info.PixelFormat^ do
    begin
      if ABitCount > 0 then
        Result.A := ((Packed32 and ABitMask) shr AShift) / (Pow2(ABitCount) - 1.0);
      Result.R := ((Packed32 and RBitMask) shr RShift) / (Pow2(RBitCount) - 1.0);
      Result.G := ((Packed32 and GBitMask) shr GShift) / (Pow2(GBitCount) - 1.0);
      Result.B := ((Packed32 and BBitMask) shr BShift) / (Pow2(BBitCount) - 1.0);
    end;
    Exit;
  end;

  // Formats made of elements of the same size: read them in memory order
  ElemCount := Info.BytesPerPixel div ElemSize;
  MaxValue := Pow2(ElemSize * 8) - 1.0;

  for I := 0 to ElemCount - 1 do
  begin
    if Kind = ekInteger then
      Elems[I] := ReadUnsigned(@PBuffer(P)[I * ElemSize], ElemSize) / MaxValue
    else
      Elems[I] := ReadFloat(@PBuffer(P)[I * ElemSize], Kind);
  end;

  if Info.HasGrayChannel then
  begin
    // Gray, then alpha
    Result.R := Elems[0];
    Result.G := Elems[0];
    Result.B := Elems[0];
    if Info.HasAlphaChannel then
      Result.A := Elems[1];
  end
  else if ElemCount = 1 then
  begin
    // Red only
    Result.R := Elems[0];
  end
  else
  begin
    // Blue, green, red, then alpha or unused element
    Result.B := Elems[0];
    Result.G := Elems[1];
    Result.R := Elems[2];
    if Info.HasAlphaChannel then
      Result.A := Elems[3];

    if Info.IsRBSwapped then
    begin
      Tmp := Result.R;
      Result.R := Result.B;
      Result.B := Tmp;
    end;
  end;
end;

function IdealPixelToStr(const Pixel: TIdealPixel): string;
begin
  Result := Format('(A=%.6f R=%.6f G=%.6f B=%.6f)', [Pixel.A, Pixel.R, Pixel.G, Pixel.B],
    GetFormatSettingsForFloats);
end;

procedure WriteFloat(P: PByte; Kind: TElementKind; Value: Single);
var
  H: THalfFloat;
begin
  if Kind = ekSingle then
    Move(Value, P^, SizeOf(Value))
  else
  begin
    H := FloatToHalf(Value);
    Move(H, P^, SizeOf(H));
  end;
end;

procedure FillFloatImage(var Image: TImageData; const Values: array of Single);
var
  Info: TImageFormatInfo;
  Kind: TElementKind;
  ElemSize, ElemCount, I, E: Integer;
begin
  if not GetImageFormatInfo(Image.Format, Info) or not Info.IsFloatingPoint then
    raise Exception.Create('FillFloatImage: image is not in floating point format');

  Kind := GetElementKind(Info);
  ElemSize := GetElementSize(Info);
  ElemCount := Info.BytesPerPixel div ElemSize;

  for I := 0 to Image.Width * Image.Height - 1 do
  begin
    for E := 0 to ElemCount - 1 do
    begin
      WriteFloat(@PBuffer(Image.Bits)[(PtrInt(I) * ElemCount + E) * ElemSize], Kind,
                 Values[(I * 7 + E * 3) mod Length(Values)]);
    end;
  end;
end;

procedure FillTestImage(var Image: TImageData; Seed: UInt32);
var
  Info: TImageFormatInfo;
  Kind: TElementKind;
  State: UInt32;
  ElemSize, ElemCount, PixelCount, I, E, J: Integer;
  P: PByte;
  Packed32: UInt32;
  W: Word;
  B: Byte;
begin
  if not GetImageFormatInfo(Image.Format, Info) or Info.IsSpecial then
    raise Exception.Create('FillTestImage: image is not in regular data format');

  State := Seed;
  Kind := GetElementKind(Info);
  ElemSize := GetElementSize(Info);
  ElemCount := Info.BytesPerPixel div ElemSize;
  PixelCount := Image.Width * Image.Height;

  if Info.IsFloatingPoint then
  begin
    for I := 0 to PixelCount * ElemCount - 1 do
    begin
      WriteFloat(@PBuffer(Image.Bits)[PtrInt(I) * ElemSize], Kind,
                 DefaultFloatValues[NextRandomByte(State) mod Length(DefaultFloatValues)]);
    end;
  end
  else
  begin
    for I := 0 to Image.Size - 1 do
      PBuffer(Image.Bits)[I] := NextRandomByte(State);
  end;

  if Info.IsIndexed then
  begin
    for I := 0 to Info.PaletteEntries - 1 do
    begin
      Image.Palette[I].B := NextRandomByte(State);
      Image.Palette[I].G := NextRandomByte(State);
      Image.Palette[I].R := NextRandomByte(State);
      Image.Palette[I].A := NextRandomByte(State);
    end;
  end;

  // Combinations of the smallest and the largest values of channels
  for I := 0 to ExtremePixelCount - 1 do
  begin
    if I >= PixelCount then
      Break;
    P := @PBuffer(Image.Bits)[PtrInt(I) * Info.BytesPerPixel];

    if Info.IsIndexed then
    begin
      P^ := I;
      Image.Palette[I].A := ExtremeByte(I and 1);
      Image.Palette[I].R := ExtremeByte(I and 2);
      Image.Palette[I].G := ExtremeByte(I and 4);
      Image.Palette[I].B := ExtremeByte(I and 8);
    end
    else if Kind = ekPacked then
    begin
      Packed32 := 0;
      with Info.PixelFormat^ do
      begin
        if (I and 1) <> 0 then
          Packed32 := Packed32 or ABitMask;
        if (I and 2) <> 0 then
          Packed32 := Packed32 or RBitMask;
        if (I and 4) <> 0 then
          Packed32 := Packed32 or GBitMask;
        if (I and 8) <> 0 then
          Packed32 := Packed32 or BBitMask;
      end;

      if ElemSize = 1 then
      begin
        B := Byte(Packed32);
        Move(B, P^, SizeOf(B));
      end
      else
      begin
        W := Word(Packed32);
        Move(W, P^, SizeOf(W));
      end;
    end
    else
    begin
      for E := 0 to ElemCount - 1 do
      begin
        if Kind = ekInteger then
        begin
          for J := 0 to ElemSize - 1 do
            PBuffer(P)[E * ElemSize + J] := ExtremeByte((I shr E) and 1);
        end
        else
          WriteFloat(@PBuffer(P)[E * ElemSize], Kind, (I shr E) and 1);
      end;
    end;
  end;
end;

end.
