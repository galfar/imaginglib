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

{ Tests of TIFF loading and saving (ImagingTiff) with each available codec
  (ImagingTiffBackend): PasTiff, and libtiff when it is compiled in
  (TIFF_USE_LIBTIFF_DYNLIB) and found at run time. Files with unusual sample
  layouts are built in memory.}
unit TestImagingTiff;

{$I ImagingOptions.inc}

// Same condition as for PasTiff in ImagingTiffBackend: when it is not
// compiled in, only layouts that need no conversion are loaded as they are,
// others come through libtiff's RGBA interface
{$IF not Defined(TIFF_NO_BUNDLED) and
  ((Defined(DCC) and (CompilerVersion >= 20)) or Defined(FPC))}
  {$DEFINE TIFF_CONVERTS_LAYOUTS}
{$IFEND}

interface

uses
  SysUtils, Classes, ImagingTypes, Imaging, ImagingUtility,
  ImagingExtFileFormats, ImagingTiff, ImagingTiffBackend,
  ImagingTestCase, ImagingTestPixels;

type
  { Tests run with the backend given by Backend, they are skipped when it is
    not available.}
  TTiffTestCase = class(TImagingTestCase)
  private
    FOldBackend: TTiffBackend;
    FOldCompression: Integer;
    FOldBigTiffMode: Integer;
    FOldPredictor: Integer;
    FImage: TImageData;
    procedure SaveToStream(const Image: TImageData; Stream: TStream);
    function LoadFromStream(Stream: TStream): Boolean;
    function LoadFromData(const Data: AnsiString): Boolean;
    procedure CheckRoundTrip(Format: TImageFormat; Compression, Predictor: Integer;
      const ColorTolerance: Double);
    function SavedTag(const Image: TImageData; Tag: Word): Integer;
    procedure CheckGray8(const Expected: array of Byte; const Msg: string);
    procedure CheckRgb8(const Expected: array of Byte; const Msg: string);
  protected
    function Backend: TTiffBackend; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRoundTripLossless;
    procedure TestRoundTripJpeg;
    procedure TestCompressionChoice;
    procedure TestBigTiffMode;
    procedure TestMultiPage;
    procedure TestResolution;
    procedure TestSavedImageIsNotChanged;
    procedure TestTigers;
    procedure TestOrientation;
    procedure TestOrientationSaved;
    procedure TestPredictorOption;
    procedure TestMinIsWhite;
    procedure TestBigEndian16;
    procedure TestTiles;
    procedure TestOddLayouts;
    procedure TestTruncatedData;
    procedure TestGarbage;
  end;

  TTestTiffPasTiff = class(TTiffTestCase)
  protected
    function Backend: TTiffBackend; override;
  end;

  TTestTiffLibTiff = class(TTiffTestCase)
  protected
    function Backend: TTiffBackend; override;
  end;

implementation

const
  TagWidth = 256;
  TagHeight = 257;
  TagBitsPerSample = 258;
  TagCompression = 259;
  TagPhotometric = 262;
  TagStripOffsets = 273;
  TagOrientation = 274;
  TagPredictor = 317;
  TagSamplesPerPixel = 277;
  TagRowsPerStrip = 278;
  TagStripByteCounts = 279;
  TagPlanarConfig = 284;
  TagTileWidth = 322;
  TagTileLength = 323;
  TagTileOffsets = 324;
  TagTileByteCounts = 325;
  TagInkSet = 332;
  TagYCbCrSubSampling = 530;

  TypeShort = 3;
  TypeLong = 4;

type
  { Builds a classic TIFF with one page in memory: header, directory, values
    that don't fit into the directory, then pixel data. Strip or tile
    offsets and byte counts are added for the chunks given to Build.}
  TTiffBuilder = class
  private
    FBigEndian: Boolean;
    FTags: array of record
      Tag, DataType: Word;
      Values: array of UInt32;
    end;
    function U16(Value: Word): AnsiString;
    function U32(Value: UInt32): AnsiString;
    procedure AddTag(Tag, DataType: Word; const Values: array of UInt32);
  public
    constructor Create(BigEndian: Boolean = False);
    procedure Short(Tag: Word; const Values: array of UInt32);
    procedure Long(Tag: Word; const Values: array of UInt32);
    { Chunks are strips, or tiles when Tiled is set.}
    function Build(const Chunks: array of AnsiString; Tiled: Boolean = False): AnsiString;
  end;

constructor TTiffBuilder.Create(BigEndian: Boolean);
begin
  inherited Create;
  FBigEndian := BigEndian;
end;

function TTiffBuilder.U16(Value: Word): AnsiString;
begin
  if FBigEndian then
    Result := AnsiChar(Value shr 8) + AnsiChar(Value and $FF)
  else
    Result := AnsiChar(Value and $FF) + AnsiChar(Value shr 8);
end;

function TTiffBuilder.U32(Value: UInt32): AnsiString;
begin
  if FBigEndian then
    Result := U16(Value shr 16) + U16(Value and $FFFF)
  else
    Result := U16(Value and $FFFF) + U16(Value shr 16);
end;

procedure TTiffBuilder.AddTag(Tag, DataType: Word; const Values: array of UInt32);
var
  I, J: Integer;
begin
  // Tags must be sorted, insert at the right place
  I := Length(FTags);
  SetLength(FTags, I + 1);
  while (I > 0) and (FTags[I - 1].Tag > Tag) do
  begin
    FTags[I] := FTags[I - 1];
    Dec(I);
  end;

  FTags[I].Tag := Tag;
  FTags[I].DataType := DataType;
  SetLength(FTags[I].Values, Length(Values));
  for J := 0 to High(Values) do
    FTags[I].Values[J] := Values[J];
end;

procedure TTiffBuilder.Short(Tag: Word; const Values: array of UInt32);
begin
  AddTag(Tag, TypeShort, Values);
end;

procedure TTiffBuilder.Long(Tag: Word; const Values: array of UInt32);
begin
  AddTag(Tag, TypeLong, Values);
end;

function TTiffBuilder.Build(const Chunks: array of AnsiString; Tiled: Boolean): AnsiString;
var
  Offsets, Counts: array of UInt32;
  I, J, DirSize: Integer;
  Pos: UInt32;
  Dir, Extra, Value, Pixels: AnsiString;
begin
  SetLength(Offsets, Length(Chunks));
  SetLength(Counts, Length(Chunks));
  for I := 0 to High(Chunks) do
    Counts[I] := Length(Chunks[I]);
  // Offsets are filled in when the size of the directory is known
  if Tiled then
  begin
    Long(TagTileOffsets, Offsets);
    Long(TagTileByteCounts, Counts);
  end
  else
  begin
    Long(TagStripOffsets, Offsets);
    Long(TagStripByteCounts, Counts);
  end;

  DirSize := 2 + 12 * Length(FTags) + 4;
  // Size of the values stored outside of the directory
  Pos := 8 + DirSize;
  for I := 0 to High(FTags) do
    if Length(FTags[I].Values) * Iff(FTags[I].DataType = TypeShort, 2, 4) > 4 then
      Inc(Pos, Length(FTags[I].Values) * Iff(FTags[I].DataType = TypeShort, 2, 4));

  Pixels := '';
  for I := 0 to High(Chunks) do
  begin
    Offsets[I] := Pos + UInt32(Length(Pixels));
    Pixels := Pixels + Chunks[I];
  end;
  for I := 0 to High(FTags) do
    if (FTags[I].Tag = TagStripOffsets) or (FTags[I].Tag = TagTileOffsets) then
      for J := 0 to High(Offsets) do
        FTags[I].Values[J] := Offsets[J];

  Dir := U16(Length(FTags));
  Extra := '';
  for I := 0 to High(FTags) do
  begin
    Value := '';
    for J := 0 to High(FTags[I].Values) do
      if FTags[I].DataType = TypeShort then
        Value := Value + U16(FTags[I].Values[J])
      else
        Value := Value + U32(FTags[I].Values[J]);

    Dir := Dir + U16(FTags[I].Tag) + U16(FTags[I].DataType) + U32(Length(FTags[I].Values));
    if Length(Value) <= 4 then
    begin
      // Values that fit are stored in the entry, left aligned
      while Length(Value) < 4 do
        Value := Value + #0;
      Dir := Dir + Value;
    end
    else
    begin
      Dir := Dir + U32(8 + DirSize + Length(Extra));
      Extra := Extra + Value;
    end;
  end;
  Dir := Dir + U32(0);

  if FBigEndian then
    Result := 'MM' + U16(42) + U32(8)
  else
    Result := 'II' + U16(42) + U32(8);
  Result := Result + Dir + Extra + Pixels;
end;

{ FillTestImage for all formats TIFF saves: ifBinary gets a byte pattern,
  bits after the last pixel of a row are zero (they are not stored).}
procedure FillImage(var Image: TImageData; Seed: UInt32);
var
  I: NativeInt;
  RowBytes: Integer;
begin
  if Image.Format = ifBinary then
  begin
    RowBytes := (Image.Width + 7) div 8;
    for I := 0 to Image.Size - 1 do
    begin
      PBuffer(Image.Bits)[I] := Byte(I * 37 + Integer(Seed and $FF));
      if (I mod RowBytes = RowBytes - 1) and (Image.Width mod 8 <> 0) then
        PBuffer(Image.Bits)[I] := PBuffer(Image.Bits)[I] and Byte($FF00 shr (Image.Width mod 8));
    end;
  end
  else
    FillTestImage(Image, Seed);
end;

function Bytes(const Values: array of Byte): AnsiString;
var
  I: Integer;
begin
  SetLength(Result, Length(Values));
  for I := 0 to High(Values) do
    Result[I + 1] := AnsiChar(Values[I]);
end;

{ TTiffTestCase }

procedure TTiffTestCase.SetUp;
begin
  InitImage(FImage);
  FOldBackend := TiffBackend;
  FOldCompression := GetOption(ImagingTiffCompression);
  FOldBigTiffMode := GetOption(ImagingTiffBigTiffWriteMode);
  FOldPredictor := GetOption(ImagingTiffPredictor);
  if not SelectTiffBackend(Backend) then
    Fail('TIFF backend is not available');
end;

procedure TTiffTestCase.TearDown;
begin
  SelectTiffBackend(FOldBackend);
  SetOption(ImagingTiffCompression, FOldCompression);
  SetOption(ImagingTiffBigTiffWriteMode, FOldBigTiffMode);
  SetOption(ImagingTiffPredictor, FOldPredictor);
  GlobalMetadata.ClearMetaItemsForSaving;
  FreeImage(FImage);
end;

procedure TTiffTestCase.SaveToStream(const Image: TImageData; Stream: TStream);
begin
  CheckTrue(SaveImageToStream('tif', Stream, Image), 'image is saved');
  Stream.Position := 0;
end;

function TTiffTestCase.LoadFromStream(Stream: TStream): Boolean;
begin
  FreeImage(FImage);
  try
    Result := LoadImageFromStream(Stream, FImage);
  except
    Result := False;
  end;
end;

function TTiffTestCase.LoadFromData(const Data: AnsiString): Boolean;
begin
  FreeImage(FImage);
  try
    Result := LoadImageFromMemory(@Data[1], Length(Data), FImage);
  except
    Result := False;
  end;
end;

procedure TTiffTestCase.CheckRoundTrip(Format: TImageFormat; Compression, Predictor: Integer;
  const ColorTolerance: Double);
var
  Original: TImageData;
  Stream: TMemoryStream;
  I: Integer;
  Channel: TPixelChannel;
  Expected, Actual: TIdealPixel;
  Info: TImageFormatInfo;
  Msg: string;
begin
  Msg := SysUtils.Format('%s compression %d predictor %d', [GetFormatName(Format), Compression, Predictor]);
  InitImage(Original);
  Stream := TMemoryStream.Create;
  try
    NewImage(37, 21, Format, Original);
    FillImage(Original, 123456789);
    SetOption(ImagingTiffCompression, Compression);
    SetOption(ImagingTiffPredictor, Predictor);
    SaveToStream(Original, Stream);
    CheckTrue(LoadFromStream(Stream), Msg + ': image is loaded');
    CheckEquals(GetFormatName(Format), GetFormatName(FImage.Format), Msg + ': format');

    GetImageFormatInfo(Format, Info);
    if Format = ifBinary then
    begin
      CheckSameMem(Original.Bits, FImage.Bits, Original.Size, Msg + ': bits');
      Exit;
    end;
    if Info.IsIndexed then
    begin
      CheckSameMem(Original.Bits, FImage.Bits, Original.Size, Msg + ': indices');
      for I := 0 to Info.PaletteEntries - 1 do
      begin
        // Alpha of palette is not stored
        CheckEqualsColor32(Original.Palette[I].Color or $FF000000,
          FImage.Palette[I].Color, Msg + ': palette');
      end;
      Exit;
    end;

    for I := 0 to Original.Width * Original.Height - 1 do
    begin
      Expected := ReadIdealPixel(Original, I);
      Actual := ReadIdealPixel(FImage, I);
      for Channel := Low(TPixelChannel) to High(TPixelChannel) do
        CheckEquals(Expected.Channels[Channel], Actual.Channels[Channel], ColorTolerance + 1e-9,
          SysUtils.Format('%s: pixel %d %s', [Msg, I, ChannelNames[Channel]]));
    end;
  finally
    Stream.Free;
    FreeImage(Original);
  end;
end;

procedure TTiffTestCase.CheckGray8(const Expected: array of Byte; const Msg: string);
var
  I: Integer;
begin
  CheckEquals(GetFormatName(ifGray8), GetFormatName(FImage.Format), Msg + ': format');
  for I := 0 to High(Expected) do
    CheckEquals(Expected[I], PBuffer(FImage.Bits)[I], Format('%s: pixel %d', [Msg, I]));
end;

procedure TTiffTestCase.CheckRgb8(const Expected: array of Byte; const Msg: string);
var
  I: Integer;
  Pixel: TColor32Rec;
begin
  // R8G8B8 with converting backend, A8R8G8B8 from libtiff's RGBA interface
  CheckTrue(FImage.Format in [ifR8G8B8, ifA8R8G8B8], Msg + ': format');
  for I := 0 to High(Expected) div 3 do
  begin
    Pixel := GetPixel32(FImage, I mod FImage.Width, I div FImage.Width);
    CheckEquals(Expected[I * 3], Pixel.R, Format('%s: pixel %d red', [Msg, I]));
    CheckEquals(Expected[I * 3 + 1], Pixel.G, Format('%s: pixel %d green', [Msg, I]));
    CheckEquals(Expected[I * 3 + 2], Pixel.B, Format('%s: pixel %d blue', [Msg, I]));
  end;
end;

procedure TTiffTestCase.TestRoundTripLossless;
var
  Format: TImageFormat;
  Compression, Predictor: Integer;
begin
  for Format := Low(TImageFormat) to High(TImageFormat) do
    if Format in TiffSupportedFormats then
      for Compression := TiffCompressionOptionNone to TiffCompressionOptionGroup4 do
      begin
        // JPEG falls back to LZW for these, except 8-bit gray and RGB
        if (Compression = TiffCompressionOptionJpeg) and (Format in [ifGray8, ifR8G8B8]) then
          Continue;
        if Compression in [TiffCompressionOptionLzw, TiffCompressionOptionDeflate] then
        begin
          for Predictor := TiffPredictorOptionAuto to TiffPredictorOptionOff do
            CheckRoundTrip(Format, Compression, Predictor, 0);
        end
        else
          CheckRoundTrip(Format, Compression, TiffPredictorOptionAuto, 0);
      end;
end;

{ Saves Image as TIFF and returns the value of SHORT tag Tag of its first
  page, -1 when the page does not have it. Both backends write classic
  little endian TIFF for small images.}
function TTiffTestCase.SavedTag(const Image: TImageData; Tag: Word): Integer;
var
  Stream: TMemoryStream;
  Data: PBuffer;
  DirOffset, Entry: NativeInt;
  I, Count: Integer;
begin
  Result := -1;
  Stream := TMemoryStream.Create;
  try
    SaveToStream(Image, Stream);
    Data := Stream.Memory;
    CheckEquals(Ord('I'), Data[0], 'little endian TIFF');
    CheckEquals(42, Data[2], 'classic TIFF');
    DirOffset := PUInt32(@Data[4])^;
    Count := PWord(@Data[DirOffset])^;
    for I := 0 to Count - 1 do
    begin
      Entry := DirOffset + 2 + I * 12;
      if PWord(@Data[Entry])^ = Tag then
      begin
        CheckEquals(TypeShort, PWord(@Data[Entry + 2])^, 'tag type');
        Result := PWord(@Data[Entry + 8])^;
      end;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TTiffTestCase.TestRoundTripJpeg;
var
  Original: TImageData;
  Stream: TMemoryStream;
  X, Y: Integer;
  Color: TColor32Rec;
  Format: TImageFormat;
  Msg: string;
begin
  InitImage(Original);
  Stream := TMemoryStream.Create;
  try
    for Format := ifGray8 to ifR8G8B8 do
    begin
      if not (Format in [ifGray8, ifR8G8B8]) then
        Continue;
      Msg := GetFormatName(Format) + ' JPEG';
      // Smooth content, JPEG is meant for it
      NewImage(48, 40, ifA8R8G8B8, Original);
      for Y := 0 to Original.Height - 1 do
        for X := 0 to Original.Width - 1 do
        begin
          Color.A := 255;
          Color.R := X * 5;
          Color.G := Y * 6;
          Color.B := 128;
          SetPixel32(Original, X, Y, Color);
        end;
      ConvertImage(Original, Format);

      SetOption(ImagingTiffCompression, TiffCompressionOptionJpeg);
      Stream.Clear;
      SaveToStream(Original, Stream);
      CheckTrue(LoadFromStream(Stream), Msg + ': image is loaded');
      CheckEquals('JPEG', string(GlobalMetadata.MetaItems[SMetaTiffCompressionName]), Msg + ': compression');
      CheckEquals(GetFormatName(Format), GetFormatName(FImage.Format), Msg + ': format');
      for Y := 0 to Original.Height - 1 do
        for X := 0 to Original.Width - 1 do
        begin
          CheckEquals(GetPixel32(Original, X, Y).R, GetPixel32(FImage, X, Y).R, 12,
            SysUtils.Format('%s: red at %d,%d', [Msg, X, Y]));
          CheckEquals(GetPixel32(Original, X, Y).G, GetPixel32(FImage, X, Y).G, 12,
            SysUtils.Format('%s: green at %d,%d', [Msg, X, Y]));
        end;
    end;
  finally
    Stream.Free;
    FreeImage(Original);
  end;
end;

procedure TTiffTestCase.TestCompressionChoice;

  procedure Check(Format: TImageFormat; Compression: Integer; const Expected: string);
  var
    Image: TImageData;
    Stream: TMemoryStream;
  begin
    InitImage(Image);
    Stream := TMemoryStream.Create;
    try
      NewImage(16, 16, Format, Image);
      SetOption(ImagingTiffCompression, Compression);
      SaveToStream(Image, Stream);
      CheckTrue(LoadFromStream(Stream), 'image is loaded');
      CheckEquals(Expected, string(GlobalMetadata.MetaItems[SMetaTiffCompressionName]),
        SysUtils.Format('%s with compression option %d', [GetFormatName(Format), Compression]));
    finally
      Stream.Free;
      FreeImage(Image);
    end;
  end;

begin
  Check(ifGray8, TiffCompressionOptionNone, 'None');
  Check(ifGray8, TiffCompressionOptionLzw, 'LZW');
  Check(ifGray8, TiffCompressionOptionPackbitsRle, 'Packbits RLE');
  Check(ifGray8, TiffCompressionOptionDeflate, 'Deflate');
  Check(ifR8G8B8, TiffCompressionOptionJpeg, 'JPEG');
  // JPEG only for 8-bit gray and RGB, Group 4 only for binary
  Check(ifGray16, TiffCompressionOptionJpeg, 'LZW');
  Check(ifA8R8G8B8, TiffCompressionOptionJpeg, 'LZW');
  Check(ifIndex8, TiffCompressionOptionJpeg, 'LZW');
  Check(ifGray8, TiffCompressionOptionGroup4, 'LZW');
  // Binary images get Group 4 whenever compression is asked for
  Check(ifBinary, TiffCompressionOptionGroup4, 'CCITT Group 4 Fax');
  Check(ifBinary, TiffCompressionOptionLzw, 'CCITT Group 4 Fax');
  Check(ifBinary, TiffCompressionOptionNone, 'None');
  // Out of range option means default (LZW)
  Check(ifGray8, 17, 'LZW');
end;

procedure TTiffTestCase.TestBigTiffMode;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    NewImage(20, 10, ifR8G8B8, FImage);
    FillTestImage(FImage, 42);

    SetOption(ImagingTiffBigTiffWriteMode, TiffBigTiffWriteModeIfNeeded);
    SaveToStream(FImage, Stream);
    CheckEquals(42, PBuffer(Stream.Memory)[2], 'IfNeeded: classic TIFF for small image');

    Stream.Clear;
    SetOption(ImagingTiffBigTiffWriteMode, TiffBigTiffWriteModeAlways);
    SaveToStream(FImage, Stream);
    CheckEquals(43, PBuffer(Stream.Memory)[2], 'Always: BigTIFF');
    CheckTrue(LoadFromStream(Stream), 'BigTIFF is loaded');
    CheckEquals(20, FImage.Width, 'BigTIFF: width');
  finally
    Stream.Free;
  end;
end;

procedure TTiffTestCase.TestMultiPage;
var
  Images, Loaded: TDynImageDataArray;
  Stream: TMemoryStream;
  I: Integer;
begin
  Stream := TMemoryStream.Create;
  try
    SetLength(Images, 3);
    NewImage(10, 12, ifGray8, Images[0]);
    NewImage(30, 5, ifR8G8B8, Images[1]);
    NewImage(7, 7, ifBinary, Images[2]);
    for I := 0 to High(Images) do
      FillImage(Images[I], I + 1);

    CheckTrue(SaveMultiImageToStream('tif', Stream, Images), 'images are saved');
    Stream.Position := 0;
    CheckTrue(LoadMultiImageFromStream(Stream, Loaded), 'images are loaded');
    CheckEquals(3, Length(Loaded), 'number of pages');
    for I := 0 to High(Images) do
      CheckImagesEqual(Images[I], Loaded[I], 'page ' + IntToStr(I));

    // Only the first page is loaded as single image
    Stream.Position := 0;
    CheckTrue(LoadFromStream(Stream), 'first page is loaded');
    CheckImagesEqual(Images[0], FImage, 'first page');
  finally
    FreeImagesInArray(Images);
    FreeImagesInArray(Loaded);
    Stream.Free;
  end;
end;

procedure TTiffTestCase.TestResolution;
var
  Stream: TMemoryStream;
  XDpi, YDpi: Double;
begin
  Stream := TMemoryStream.Create;
  try
    NewImage(8, 8, ifGray8, FImage);

    // Centimeters are written by default
    GlobalMetadata.SetPhysicalPixelSize(ruDpi, 300, 150, True);
    SaveToStream(FImage, Stream);
    CheckTrue(LoadFromStream(Stream), 'image is loaded');
    CheckEquals(3, Integer(GlobalMetadata.MetaItems[SMetaTiffResolutionUnit]), 'unit is centimeter');
    CheckTrue(GlobalMetadata.GetPhysicalPixelSize(ruDpi, XDpi, YDpi), 'resolution is loaded');
    CheckEquals(300, XDpi, 0.01, 'X dpi');
    CheckEquals(150, YDpi, 0.01, 'Y dpi');

    // Inches when asked for by the resolution unit metadata
    GlobalMetadata.SetMetaItemForSaving(SMetaTiffResolutionUnit, 2);
    Stream.Clear;
    SaveToStream(FImage, Stream);
    CheckTrue(LoadFromStream(Stream), 'image is loaded');
    CheckEquals(2, Integer(GlobalMetadata.MetaItems[SMetaTiffResolutionUnit]), 'unit is inch');
    CheckTrue(GlobalMetadata.GetPhysicalPixelSize(ruDpi, XDpi, YDpi), 'resolution is loaded');
    CheckEquals(300, XDpi, 0.001, 'X dpi');
    CheckEquals(150, YDpi, 0.001, 'Y dpi');
  finally
    Stream.Free;
  end;
end;

procedure TTiffTestCase.TestSavedImageIsNotChanged;
var
  Before: TImageData;
  Stream: TMemoryStream;
  Format: TImageFormat;
begin
  InitImage(Before);
  Stream := TMemoryStream.Create;
  try
    // Channels were swapped in place while saving before
    for Format := Low(TImageFormat) to High(TImageFormat) do
      if Format in [ifR8G8B8, ifA8R8G8B8, ifA8Gray8, ifA16R16G16B16] then
      begin
        NewImage(9, 4, Format, FImage);
        FillTestImage(FImage, 7);
        CloneImage(FImage, Before);
        Stream.Clear;
        SaveToStream(FImage, Stream);
        CheckImagesEqual(Before, FImage, GetFormatName(Format) + ': image given to saving');
      end;
  finally
    Stream.Free;
    FreeImage(Before);
  end;
end;

procedure TTiffTestCase.TestTigers;
var
  Images: TDynImageDataArray;
begin
  try
    CheckTrue(LoadMultiImageFromFile(GetTestDataFile('Tigers.tif'), Images), 'file is loaded');
    CheckEquals(2, Length(Images), 'number of pages');
    CheckEquals(256, Images[0].Width, 'page 0 width');
    CheckEquals(GetFormatName(ifA16R16G16B16F), GetFormatName(Images[0].Format), 'page 0 format');
    CheckEquals(641, Images[1].Width, 'page 1 width');
    CheckEquals(472, Images[1].Height, 'page 1 height');
    CheckEquals('JPEG', string(GlobalMetadata.MetaItemsMulti[SMetaTiffCompressionName, 1]), 'page 1 compression');
  finally
    FreeImagesInArray(Images);
  end;
end;

procedure TTiffTestCase.TestOrientation;
var
  Builder: TTiffBuilder;
begin
  Builder := TTiffBuilder.Create;
  try
    Builder.Long(TagWidth, [3]);
    Builder.Long(TagHeight, [1]);
    Builder.Short(TagBitsPerSample, [8]);
    Builder.Short(TagPhotometric, [1]);
    Builder.Short(TagOrientation, [3]);
    CheckTrue(LoadFromData(Builder.Build([Bytes([10, 20, 30])])), 'image is loaded');
    // Orientation is reported, pixels stay as stored
    CheckEquals(3, Integer(GlobalMetadata.MetaItems[SMetaTiffOrientation]), 'orientation');
    CheckGray8([10, 20, 30], 'pixels');
  finally
    Builder.Free;
  end;
end;

procedure TTiffTestCase.TestOrientationSaved;
var
  Images, Loaded: TDynImageDataArray;
  Stream: TMemoryStream;
begin
  NewImage(5, 4, ifGray8, FImage);
  // As stored unless metadata says otherwise, invalid values too
  CheckEquals(1, SavedTag(FImage, TagOrientation), 'default');
  GlobalMetadata.SetMetaItemForSaving(SMetaTiffOrientation, 6);
  CheckEquals(6, SavedTag(FImage, TagOrientation), 'from metadata');
  GlobalMetadata.SetMetaItemForSaving(SMetaTiffOrientation, 9);
  CheckEquals(1, SavedTag(FImage, TagOrientation), 'invalid value');
  GlobalMetadata.ClearMetaItemsForSaving;

  // Per page: a page without its own value gets that of page 0
  Stream := TMemoryStream.Create;
  try
    SetLength(Images, 3);
    NewImage(5, 4, ifGray8, Images[0]);
    NewImage(5, 4, ifGray8, Images[1]);
    NewImage(5, 4, ifGray8, Images[2]);
    GlobalMetadata.SetMetaItemForSaving(SMetaTiffOrientation, 3, 0);
    GlobalMetadata.SetMetaItemForSaving(SMetaTiffOrientation, 8, 2);
    CheckTrue(SaveMultiImageToStream('tif', Stream, Images), 'images are saved');
    Stream.Position := 0;
    CheckTrue(LoadMultiImageFromStream(Stream, Loaded), 'images are loaded');
    CheckEquals(3, Integer(GlobalMetadata.MetaItemsMulti[SMetaTiffOrientation, 0]), 'page 0');
    CheckEquals(3, Integer(GlobalMetadata.MetaItemsMulti[SMetaTiffOrientation, 1]), 'page 1');
    CheckEquals(8, Integer(GlobalMetadata.MetaItemsMulti[SMetaTiffOrientation, 2]), 'page 2');
  finally
    FreeImagesInArray(Images);
    FreeImagesInArray(Loaded);
    Stream.Free;
  end;
end;

procedure TTiffTestCase.TestPredictorOption;
const
  NoPredictor = -1;
var
  Smooth, Dithered: TImageData;
  X, Y, Compression: Integer;
  State: UInt32;
  Msg: string;

  procedure Check(const Image: TImageData; Predictor, Expected: Integer; const What: string);
  begin
    SetOption(ImagingTiffPredictor, Predictor);
    CheckEquals(Expected, SavedTag(Image, TagPredictor),
      SysUtils.Format('%s: %s with predictor option %d', [Msg, What, Predictor]));
  end;

begin
  InitImage(Smooth);
  InitImage(Dithered);
  try
    // Smooth content gains from the predictor, two levels at random
    // (dithered) lose: differences take three values instead of two
    NewImage(128, 96, ifGray8, Smooth);
    NewImage(128, 96, ifGray8, Dithered);
    State := 12345;
    for Y := 0 to Smooth.Height - 1 do
      for X := 0 to Smooth.Width - 1 do
      begin
        PBuffer(Smooth.Bits)[Y * Smooth.Width + X] := (X + Y) mod 256;
        State := UInt32((Int64(State) * 1103515245 + 12345) and $FFFFFFFF);
        PBuffer(Dithered.Bits)[Y * Dithered.Width + X] := 255 * ((State shr 16) and 1);
      end;

    for Compression := TiffCompressionOptionLzw to TiffCompressionOptionDeflate do
    begin
      if Compression = TiffCompressionOptionPackbitsRle then
        Continue;
      Msg := 'compression option ' + IntToStr(Compression);
      SetOption(ImagingTiffCompression, Compression);
    {$IFDEF TIFF_CONVERTS_LAYOUTS}
      Check(Smooth, TiffPredictorOptionAuto, 2, 'smooth image');
    {$ELSE}
      // Without PasTiff's encoders Auto uses it for color images only
      Check(Smooth, TiffPredictorOptionAuto, NoPredictor, 'smooth image');
    {$ENDIF}
      Check(Smooth, TiffPredictorOptionOn, 2, 'smooth image');
      Check(Smooth, TiffPredictorOptionOff, NoPredictor, 'smooth image');
      Check(Dithered, TiffPredictorOptionAuto, NoPredictor, 'dithered image');
      Check(Dithered, TiffPredictorOptionOn, 2, 'dithered image');
      // Out of range option means Auto
      Check(Dithered, 7, NoPredictor, 'dithered image');

      // Never for palette indices and floating point
      ConvertImage(Dithered, ifIndex8);
      Check(Dithered, TiffPredictorOptionOn, NoPredictor, 'palette image');
      ConvertImage(Dithered, ifR32F);
      Check(Dithered, TiffPredictorOptionOn, NoPredictor, 'float image');
      ConvertImage(Dithered, ifGray8);
    end;

    // Only with LZW and Deflate
    Msg := 'no compression';
    SetOption(ImagingTiffCompression, TiffCompressionOptionNone);
    Check(Smooth, TiffPredictorOptionOn, NoPredictor, 'smooth image');
  finally
    FreeImage(Smooth);
    FreeImage(Dithered);
  end;
end;

procedure TTiffTestCase.TestMinIsWhite;
var
  Builder: TTiffBuilder;
begin
  Builder := TTiffBuilder.Create;
  try
    Builder.Long(TagWidth, [4]);
    Builder.Long(TagHeight, [1]);
    Builder.Short(TagBitsPerSample, [8]);
    Builder.Short(TagPhotometric, [0]);
    CheckTrue(LoadFromData(Builder.Build([Bytes([0, 1, 200, 255])])), '8-bit is loaded');
    CheckGray8([255, 254, 55, 0], '8-bit');
  finally
    Builder.Free;
  end;

  Builder := TTiffBuilder.Create;
  try
    Builder.Long(TagWidth, [10]);
    Builder.Long(TagHeight, [1]);
    Builder.Short(TagBitsPerSample, [1]);
    Builder.Short(TagPhotometric, [0]);
    CheckTrue(LoadFromData(Builder.Build([Bytes([$F0, $C0])])), '1-bit is loaded');
    CheckEquals(GetFormatName(ifBinary), GetFormatName(FImage.Format), '1-bit: format');
    // 1 is white in ifBinary, the bits after the 10 pixels are not compared
    CheckEquals($0F, PBuffer(FImage.Bits)[0], '1-bit: first byte');
    CheckEquals($3F, PBuffer(FImage.Bits)[1] or $3F, '1-bit: second byte');
  finally
    Builder.Free;
  end;
end;

procedure TTiffTestCase.TestBigEndian16;
var
  Builder: TTiffBuilder;
begin
  Builder := TTiffBuilder.Create(True);
  try
    Builder.Long(TagWidth, [2]);
    Builder.Long(TagHeight, [1]);
    Builder.Short(TagBitsPerSample, [16]);
    Builder.Short(TagPhotometric, [1]);
    CheckTrue(LoadFromData(Builder.Build([Bytes([$12, $34, $FF, $01])])), 'image is loaded');
    CheckEquals(GetFormatName(ifGray16), GetFormatName(FImage.Format), 'format');
    CheckEquals($1234, PWordArray(FImage.Bits)[0], 'pixel 0');
    CheckEquals($FF01, PWordArray(FImage.Bits)[1], 'pixel 1');
  finally
    Builder.Free;
  end;
end;

procedure TTiffTestCase.TestTiles;
var
  Builder: TTiffBuilder;
  Tiles: array[0..3] of AnsiString;
  Expected: array[0..20 * 18 - 1] of Byte;
  I, X, Y: Integer;
begin
  // 20x18 image in 16x16 tiles: 2x2 tiles, those on the right and the
  // bottom are partly outside of the image
  for I := 0 to High(Tiles) do
  begin
    SetLength(Tiles[I], 16 * 16);
    for Y := 0 to 15 do
      for X := 0 to 15 do
        Tiles[I][Y * 16 + X + 1] := AnsiChar(I * 60 + Y * 3 + X);
  end;
  for Y := 0 to 17 do
    for X := 0 to 19 do
      Expected[Y * 20 + X] := (Y div 16 * 2 + X div 16) * 60 + (Y mod 16) * 3 + X mod 16;

  Builder := TTiffBuilder.Create;
  try
    Builder.Long(TagWidth, [20]);
    Builder.Long(TagHeight, [18]);
    Builder.Short(TagBitsPerSample, [8]);
    Builder.Short(TagPhotometric, [1]);
    Builder.Long(TagTileWidth, [16]);
    Builder.Long(TagTileLength, [16]);
    CheckTrue(LoadFromData(Builder.Build(Tiles, True)), 'image is loaded');
    CheckGray8(Expected, 'tiled');
  finally
    Builder.Free;
  end;
end;

procedure TTiffTestCase.TestOddLayouts;
var
  Builder: TTiffBuilder;
begin
  // 4-bit gray: scaled to 8 bits
  Builder := TTiffBuilder.Create;
  try
    Builder.Long(TagWidth, [4]);
    Builder.Long(TagHeight, [1]);
    Builder.Short(TagBitsPerSample, [4]);
    Builder.Short(TagPhotometric, [1]);
    CheckTrue(LoadFromData(Builder.Build([Bytes([$0F, $8A])])), '4-bit gray is loaded');
  {$IFDEF TIFF_CONVERTS_LAYOUTS}
    CheckGray8([0, 255, 136, 170], '4-bit gray');
  {$ELSE}
    CheckRgb8([0, 0, 0, 255, 255, 255, 136, 136, 136, 170, 170, 170], '4-bit gray');
  {$ENDIF}
  finally
    Builder.Free;
  end;

  // RGB in separate planes
  Builder := TTiffBuilder.Create;
  try
    Builder.Long(TagWidth, [2]);
    Builder.Long(TagHeight, [1]);
    Builder.Short(TagBitsPerSample, [8, 8, 8]);
    Builder.Short(TagPhotometric, [2]);
    Builder.Short(TagSamplesPerPixel, [3]);
    Builder.Short(TagPlanarConfig, [2]);
    CheckTrue(LoadFromData(Builder.Build([Bytes([1, 2]), Bytes([3, 4]), Bytes([5, 6])])), 'planar RGB is loaded');
    CheckRgb8([1, 3, 5, 2, 4, 6], 'planar RGB');
  finally
    Builder.Free;
  end;

  // CMYK
  Builder := TTiffBuilder.Create;
  try
    Builder.Long(TagWidth, [2]);
    Builder.Long(TagHeight, [1]);
    Builder.Short(TagBitsPerSample, [8, 8, 8, 8]);
    Builder.Short(TagPhotometric, [5]);
    Builder.Short(TagSamplesPerPixel, [4]);
    Builder.Short(TagInkSet, [1]);
    CheckTrue(LoadFromData(Builder.Build([Bytes([0, 255, 255, 0, 0, 0, 0, 255])])), 'CMYK is loaded');
    CheckRgb8([255, 0, 0, 0, 0, 0], 'CMYK');
  finally
    Builder.Free;
  end;

  // YCbCr subsampled 2x2, uncompressed: one block of four lumas and neutral
  // chroma is gray
  Builder := TTiffBuilder.Create;
  try
    Builder.Long(TagWidth, [2]);
    Builder.Long(TagHeight, [2]);
    Builder.Short(TagBitsPerSample, [8, 8, 8]);
    Builder.Short(TagPhotometric, [6]);
    Builder.Short(TagSamplesPerPixel, [3]);
    Builder.Short(TagYCbCrSubSampling, [2, 2]);
    CheckTrue(LoadFromData(Builder.Build([Bytes([10, 20, 30, 40, 128, 128])])), 'YCbCr is loaded');
    CheckRgb8([10, 10, 10, 20, 20, 20, 30, 30, 30, 40, 40, 40], 'YCbCr');
  finally
    Builder.Free;
  end;
end;

procedure TTiffTestCase.TestTruncatedData;
var
  Builder: TTiffBuilder;
  Data: AnsiString;
begin
  Builder := TTiffBuilder.Create;
  try
    Builder.Long(TagWidth, [4]);
    Builder.Long(TagHeight, [4]);
    Builder.Short(TagBitsPerSample, [8]);
    Builder.Short(TagPhotometric, [1]);
    Builder.Long(TagRowsPerStrip, [2]);
    Data := Builder.Build([Bytes([1, 2, 3, 4, 5, 6, 7, 8]), Bytes([9, 10, 11, 12, 13, 14, 15, 16])]);
  finally
    Builder.Free;
  end;

  // The second strip is missing: what is there is loaded, the rest is black
  SetLength(Data, Length(Data) - 8);
  CheckTrue(LoadFromData(Data), 'truncated file is loaded');
  CheckGray8([1, 2, 3, 4, 5, 6, 7, 8, 0, 0, 0, 0, 0, 0, 0, 0], 'truncated file');
end;

procedure TTiffTestCase.TestGarbage;
begin
  CheckFalse(LoadFromData('II*'#0#8#0#0#0#$FF#$FF#1#2#3#4#5#6#7#8#9), 'garbage after header');
  CheckFalse(LoadFromData('II*'#0#$F0#0#0#0), 'directory beyond the end');
end;

{ TTestTiffPasTiff }

function TTestTiffPasTiff.Backend: TTiffBackend;
begin
  Result := tbPasTiff;
end;

{ TTestTiffLibTiff }

function TTestTiffLibTiff.Backend: TTiffBackend;
begin
  Result := tbLibTiff;
end;

var
  Saved: TTiffBackend;

initialization
  // Each backend that can be used here gets its run of the tests
  Saved := TiffBackend;
  if SelectTiffBackend(tbPasTiff) then
    RegisterImagingTest(TTestTiffPasTiff);
  if SelectTiffBackend(tbLibTiff) then
    RegisterImagingTest(TTestTiffLibTiff);
  SelectTiffBackend(Saved);
end.
