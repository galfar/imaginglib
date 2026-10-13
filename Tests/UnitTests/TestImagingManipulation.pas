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

{ Tests of image manipulation functions that move pixels without changing
  them: resizing with nearest filter, rotations by right angles, and mapping
  of images to palettes.}
unit TestImagingManipulation;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, ImagingTypes, Imaging, ImagingFormats, ImagingUtility,
  ImagingTestCase, ImagingTestPixels;

type
  { Resizing with nearest filter: every pixel of the result is a copy of the
    source pixel given by 16.16 fixed point steps.}
  TTestResizeNearest = class(TImagingTestCase)
  private
    procedure CheckResize(Format: TImageFormat; SrcWidth, SrcHeight, DstWidth,
      DstHeight: Integer; Filter: TResizeFilter);
  published
    procedure TestAllFormats;
    procedure TestVariousSizes;
    procedure TestWideImage;
    procedure TestTallImage;
    procedure TestIndexedIgnoresFilter;
  end;

  { Rotations by multiples of 90 degrees (counterclockwise).}
  TTestRotate = class(TImagingTestCase)
  private
    procedure CheckRotation(Format: TImageFormat; Angle: Integer; UseRotateImage: Boolean);
  published
    procedure TestRotateImageMul90;
    procedure TestRotateImageRightAngles;
    procedure TestFullTurnsChangeNothing;
  end;

  TTestPalettes = class(TImagingTestCase)
  published
    procedure TestMapImageToPalette;
    procedure TestMakePaletteForImages;
    procedure TestMakePaletteAndConvertImages;
  end;

implementation

function PixelPtr(const Image: TImageData; X, Y, Bpp: Integer): PByte;
begin
  Result := @PBuffer(Image.Bits)[(PtrInt(Y) * Image.Width + X) * Bpp];
end;

{ TTestResizeNearest }

procedure TTestResizeNearest.CheckResize(Format: TImageFormat; SrcWidth, SrcHeight,
  DstWidth, DstHeight: Integer; Filter: TResizeFilter);
var
  Src, Dst: TImageData;
  Info: TImageFormatInfo;
  X, Y, SrcX, SrcY, Bpp: Integer;
  ScaleX, ScaleY: Int64;
  Msg: string;
begin
  CheckTrue(GetImageFormatInfo(Format, Info), 'Format info');
  Bpp := Info.BytesPerPixel;
  Msg := SysUtils.Format('%s %dx%d to %dx%d', [Info.Name, SrcWidth, SrcHeight, DstWidth, DstHeight]);

  InitImage(Src);
  InitImage(Dst);
  try
    CheckTrue(NewImage(SrcWidth, SrcHeight, Format, Src), Msg + ': NewImage');
    FillTestImage(Src, 20261013);
    CheckTrue(CloneImage(Src, Dst), Msg + ': CloneImage');

    CheckTrue(ResizeImage(Dst, DstWidth, DstHeight, Filter), Msg + ': ResizeImage');
    CheckEquals(DstWidth, Dst.Width, Msg + ': width');
    CheckEquals(DstHeight, Dst.Height, Msg + ': height');
    CheckEquals(Ord(Format), Ord(Dst.Format), Msg + ': format');
    CheckEquals(Int64(DstWidth) * DstHeight * Bpp, Dst.Size, Msg + ': size');

    if Info.PaletteEntries > 0 then
    begin
      CheckSameMem(Src.Palette, Dst.Palette, Info.PaletteEntries * SizeOf(TColor32Rec),
                   Msg + ': palette');
    end;

    // Steps in source image are truncated to 16 fractional bits
    ScaleX := (Int64(SrcWidth) shl 16) div DstWidth;
    ScaleY := (Int64(SrcHeight) shl 16) div DstHeight;

    for Y := 0 to DstHeight - 1 do
    begin
      SrcY := (Y * ScaleY) shr 16;

      for X := 0 to DstWidth - 1 do
      begin
        SrcX := (X * ScaleX) shr 16;
        if not CompareMem(PixelPtr(Src, SrcX, SrcY, Bpp), PixelPtr(Dst, X, Y, Bpp), Bpp) then
        begin
          Fail(SysUtils.Format('%s: pixel [%d, %d] is not a copy of source pixel [%d, %d]',
            [Msg, X, Y, SrcX, SrcY]));
        end;
      end;
    end;
  finally
    FreeImage(Src);
    FreeImage(Dst);
  end;
end;

procedure TTestResizeNearest.TestAllFormats;
var
  Fmt: TImageFormat;
  Info: TImageFormatInfo;
begin
  for Fmt := Low(TImageFormat) to High(TImageFormat) do
  begin
    if not GetImageFormatInfo(Fmt, Info) or Info.IsSpecial or (Fmt = ifDefault) then
      Continue;

    CheckResize(Fmt, 40, 30, 17, 23, rfNearest);
    CheckResize(Fmt, 17, 23, 40, 30, rfNearest);
  end;
end;

procedure TTestResizeNearest.TestVariousSizes;
begin
  CheckResize(ifGray8, 100, 100, 37, 37, rfNearest);
  CheckResize(ifGray8, 37, 37, 100, 100, rfNearest);
  CheckResize(ifR8G8B8, 1000, 3, 999, 3, rfNearest);
  CheckResize(ifR8G8B8, 3, 1000, 3, 999, rfNearest);
  CheckResize(ifA8R8G8B8, 999, 2, 1000, 5, rfNearest);
  CheckResize(ifA8R8G8B8, 64, 64, 1, 1, rfNearest);
  CheckResize(ifA8R8G8B8, 1, 1, 64, 64, rfNearest);
  CheckResize(ifA16R16G16B16, 300, 7, 7, 300, rfNearest);
  // Only one of the dimensions changes
  CheckResize(ifGray16, 50, 20, 50, 33, rfNearest);
  CheckResize(ifGray16, 50, 20, 21, 20, rfNearest);
end;

procedure TTestResizeNearest.TestWideImage;
begin
  // Positions in sources with 32768 pixels or more do not fit
  // into 32-bit 16.16 fixed point numbers
  CheckResize(ifGray8, 32768, 2, 16384, 2, rfNearest);
  CheckResize(ifGray8, 40000, 3, 20000, 3, rfNearest);
  CheckResize(ifGray8, 40000, 3, 80000, 3, rfNearest);
  CheckResize(ifGray8, 70000, 2, 35000, 2, rfNearest);
  CheckResize(ifGray8, 70000, 2, 140000, 2, rfNearest);
  CheckResize(ifGray8, 70000, 2, 1234, 1, rfNearest);
  CheckResize(ifR8G8B8, 40000, 3, 20000, 2, rfNearest);
  CheckResize(ifR8G8B8, 70000, 2, 99999, 3, rfNearest);
  CheckResize(ifA16R16G16B16, 70000, 1, 65537, 1, rfNearest);
end;

procedure TTestResizeNearest.TestTallImage;
begin
  CheckResize(ifGray8, 2, 32768, 2, 16384, rfNearest);
  CheckResize(ifGray8, 3, 40000, 3, 20000, rfNearest);
  CheckResize(ifGray8, 3, 40000, 3, 80000, rfNearest);
  CheckResize(ifGray8, 2, 70000, 2, 35000, rfNearest);
  CheckResize(ifGray8, 2, 70000, 2, 140000, rfNearest);
  CheckResize(ifR8G8B8, 3, 40000, 2, 20000, rfNearest);
  CheckResize(ifR8G8B8, 2, 70000, 3, 99999, rfNearest);
end;

procedure TTestResizeNearest.TestIndexedIgnoresFilter;
begin
  // Indices cannot be interpolated so indexed images are always resized
  // with nearest filter
  CheckResize(ifIndex8, 40, 30, 17, 23, rfBilinear);
  CheckResize(ifIndex8, 17, 23, 40, 30, rfBicubic);
  CheckResize(ifIndex8, 40000, 2, 20000, 3, rfLanczos);
end;

{ TTestRotate }

procedure TTestRotate.CheckRotation(Format: TImageFormat; Angle: Integer;
  UseRotateImage: Boolean);
const
  Width = 7;
  Height = 4;
var
  Src, Dst: TImageData;
  Info: TImageFormatInfo;
  X, Y, SrcX, SrcY, Bpp, NormAngle: Integer;
  Msg: string;
begin
  CheckTrue(GetImageFormatInfo(Format, Info), 'Format info');
  Bpp := Info.BytesPerPixel;
  Msg := SysUtils.Format('%s by %d', [Info.Name, Angle]);
  NormAngle := ((Angle mod 360) + 360) mod 360;

  InitImage(Src);
  InitImage(Dst);
  try
    CheckTrue(NewImage(Width, Height, Format, Src), Msg + ': NewImage');
    FillTestImage(Src, 4711);
    CheckTrue(CloneImage(Src, Dst), Msg + ': CloneImage');

    if UseRotateImage then
      RotateImage(Dst, Angle)
    else
      RotateImageMul90(Dst, Angle);

    CheckEquals(Ord(Format), Ord(Dst.Format), Msg + ': format');
    CheckTrue(TestImage(Dst), Msg + ': image is valid');

    if (NormAngle = 90) or (NormAngle = 270) then
    begin
      CheckEquals(Height, Dst.Width, Msg + ': width');
      CheckEquals(Width, Dst.Height, Msg + ': height');
    end
    else
    begin
      CheckEquals(Width, Dst.Width, Msg + ': width');
      CheckEquals(Height, Dst.Height, Msg + ': height');
    end;

    if Info.PaletteEntries > 0 then
    begin
      CheckSameMem(Src.Palette, Dst.Palette, Info.PaletteEntries * SizeOf(TColor32Rec),
                   Msg + ': palette');
    end;

    for Y := 0 to Dst.Height - 1 do
    begin
      for X := 0 to Dst.Width - 1 do
      begin
        // Positive angles rotate counterclockwise
        case NormAngle of
          90:
            begin
              SrcX := Width - 1 - Y;
              SrcY := X;
            end;
          180:
            begin
              SrcX := Width - 1 - X;
              SrcY := Height - 1 - Y;
            end;
          270:
            begin
              SrcX := Y;
              SrcY := Height - 1 - X;
            end;
        else
          SrcX := X;
          SrcY := Y;
        end;

        if not CompareMem(PixelPtr(Src, SrcX, SrcY, Bpp), PixelPtr(Dst, X, Y, Bpp), Bpp) then
        begin
          Fail(SysUtils.Format('%s: pixel [%d, %d] is not a copy of source pixel [%d, %d]',
            [Msg, X, Y, SrcX, SrcY]));
        end;
      end;
    end;
  finally
    FreeImage(Src);
    FreeImage(Dst);
  end;
end;

const
  RotatedFormats: array[0..5] of TImageFormat = (ifIndex8, ifGray8, ifR5G6B5,
    ifR8G8B8, ifA16R16G16B16, ifA32R32G32B32F);
  RightAngles: array[0..5] of Integer = (90, 180, 270, -90, -180, 450);

procedure TTestRotate.TestRotateImageMul90;
var
  F, A: Integer;
begin
  for F := Low(RotatedFormats) to High(RotatedFormats) do
    for A := Low(RightAngles) to High(RightAngles) do
      CheckRotation(RotatedFormats[F], RightAngles[A], False);
end;

procedure TTestRotate.TestRotateImageRightAngles;
var
  F, A: Integer;
begin
  // Rotation by any angle does the right angles by moving of pixels too
  for F := Low(RotatedFormats) to High(RotatedFormats) do
    for A := Low(RightAngles) to High(RightAngles) do
      CheckRotation(RotatedFormats[F], RightAngles[A], True);
end;

procedure TTestRotate.TestFullTurnsChangeNothing;
begin
  CheckRotation(ifR8G8B8, 0, False);
  CheckRotation(ifR8G8B8, 360, False);
  CheckRotation(ifR8G8B8, -720, False);
  CheckRotation(ifIndex8, 0, False);
  CheckRotation(ifR8G8B8, 0, True);
  CheckRotation(ifR8G8B8, 360, True);
  CheckRotation(ifIndex8, 720, True);
end;

{ TTestPalettes }

const
  // Blue, green, red, alpha in memory
  PaletteColors: array[0..4] of TColor32 = ($FF000000, $FFFF0000, $FF00FF00,
    $FF0000FF, $FFFFFFFF);

procedure TTestPalettes.TestMapImageToPalette;
const
  Width = 9;
  Height = 5;
var
  Image: TImageData;
  Pal: array[0..High(PaletteColors)] of TColor32Rec;
  I: Integer;
begin
  for I := 0 to High(Pal) do
    Pal[I].Color := PaletteColors[I];

  InitImage(Image);
  try
    CheckTrue(NewImage(Width, Height, ifA8R8G8B8, Image), 'NewImage');
    for I := 0 to Width * Height - 1 do
      PUInt32Array(Image.Bits)[I] := PaletteColors[(I * 3) mod Length(Pal)];
    // Colors that are not in the palette get the nearest one
    PUInt32Array(Image.Bits)[1] := $FFF0F5FA;
    PUInt32Array(Image.Bits)[2] := $FF100005;

    CheckTrue(MapImageToPalette(Image, @Pal, Length(Pal)), 'MapImageToPalette');
    CheckEquals(Ord(ifIndex8), Ord(Image.Format), 'Format');
    CheckEquals(Width, Image.Width, 'Width');
    CheckEquals(Height, Image.Height, 'Height');
    CheckSameMem(@Pal, Image.Palette, SizeOf(Pal), 'Palette');

    for I := 0 to Width * Height - 1 do
    begin
      case I of
        1: CheckEquals(4, PBuffer(Image.Bits)[I], 'Index of nearly white pixel');
        2: CheckEquals(0, PBuffer(Image.Bits)[I], 'Index of nearly black pixel');
      else
        CheckEquals((I * 3) mod Length(Pal), PBuffer(Image.Bits)[I], Format('Index of pixel %d', [I]));
      end;
    end;
  finally
    FreeImage(Image);
  end;
end;

procedure NewImagesForPalette(var Images: TDynImageDataArray);
var
  I, J: Integer;
begin
  SetLength(Images, 2);

  for I := 0 to High(Images) do
  begin
    InitImage(Images[I]);
    NewImage(6 + I, 4, ifA8R8G8B8, Images[I]);
    // The first image has the first three colors, the second one the last three
    for J := 0 to Images[I].Width * Images[I].Height - 1 do
      PUInt32Array(Images[I].Bits)[J] := PaletteColors[I * 2 + (J * 5) mod 3];
  end;
end;

function FindInPalette(Pal: PPalette32; Entries: Integer; Color: TColor32): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to Entries - 1 do
  begin
    if Pal[I].Color = Color then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

procedure TTestPalettes.TestMakePaletteForImages;
var
  Images, Originals: TDynImageDataArray;
  Pal: array[0..255] of TColor32Rec;
  I: Integer;
begin
  NewImagesForPalette(Images);
  NewImagesForPalette(Originals);
  try
    FillChar(Pal, SizeOf(Pal), $55);
    CheckTrue(MakePaletteForImages(Images, @Pal, 256, False), 'MakePaletteForImages');

    for I := 0 to High(PaletteColors) do
    begin
      CheckTrue(FindInPalette(@Pal, 256, PaletteColors[I]) >= 0,
        Format('Color %d of images is in the palette', [I]));
    end;

    for I := 0 to High(Images) do
      CheckImagesEqual(Originals[I], Images[I], Format('Image %d is not changed', [I]));
  finally
    FreeImagesInArray(Images);
    FreeImagesInArray(Originals);
  end;
end;

procedure TTestPalettes.TestMakePaletteAndConvertImages;
var
  Images, Originals: TDynImageDataArray;
  Pal: array[0..255] of TColor32Rec;
  I, J: Integer;
begin
  NewImagesForPalette(Images);
  NewImagesForPalette(Originals);
  try
    CheckTrue(MakePaletteForImages(Images, @Pal, 256, True), 'MakePaletteForImages');

    for I := 0 to High(Images) do
    begin
      CheckEquals(Ord(ifIndex8), Ord(Images[I].Format), Format('Image %d: format', [I]));
      CheckEquals(Originals[I].Width, Images[I].Width, Format('Image %d: width', [I]));
      CheckEquals(Originals[I].Height, Images[I].Height, Format('Image %d: height', [I]));
      CheckSameMem(@Pal, Images[I].Palette, SizeOf(Pal), Format('Image %d: palette', [I]));

      // All images share the palette and keep their colors
      for J := 0 to Images[I].Width * Images[I].Height - 1 do
      begin
        CheckEqualsColor32(PUInt32Array(Originals[I].Bits)[J],
          Images[I].Palette[PBuffer(Images[I].Bits)[J]].Color,
          Format('Image %d: color of pixel %d', [I, J]));
      end;
    end;
  finally
    FreeImagesInArray(Images);
    FreeImagesInArray(Originals);
  end;
end;

initialization
  RegisterImagingTest(TTestResizeNearest);
  RegisterImagingTest(TTestRotate);
  RegisterImagingTest(TTestPalettes);
end.
