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
  Functions and classes for interoperability between Imaging and
  FireMonkey framework.

  Currently has conversion functions between FMX TBitmap (RGBA and BGRA pixel formats)
  and Imaging's TImageData and TBaseImage.
}
unit ImagingFmx;

{$I ImagingOptions.inc}

{$IF not Defined (DCC) or (CompilerVersion < 25.0)}
  {$MESSAGE FATAL 'Needs at least Delphi XE4, probably higher'}
{$IFEND}

interface

uses
  Types,
  SysUtils,
  ImagingTypes,
  Imaging,
  ImagingFormats,
  ImagingClasses,
  ImagingUtility,
  UITypes,
  UIConsts,
  FMX.Types,
  FMX.Utils,
  FMX.Graphics;

{ Converts image from TImageData record to FMX bitmap. Bitmap must be already instantiated.}
procedure ConvertImageDataToFmxBitmap(const Image: TImageData; Bitmap: TBitmap);
{ Converts FMX bitmap to TImageData. Image must be already initialized.}
procedure ConvertFmxBitmapToImageData(const Bitmap: TBitmap; Image: TImageData);

{ Converts image from TBaseImage instance to FMX bitmap. Bitmap must be already instantiated.}
procedure ConvertImageToFmxBitmap(Image: TBaseImage; Bitmap: TBitmap);
{ Converts FMX bitmap to TBaseImage. Image must be already instantiated.}
procedure ConvertFmxBitmapToImage(const Bitmap: TBitmap; Image: TBaseImage);

{ Copies rectangular area of pixels from TImageData record to existing FMX bitmap.}
procedure CopyRectToFmxBitmap(const Image: TImageData; Bitmap: TBitmap;
  SrcX, SrcY, Width, Height, DstX, DstY: LongInt); overload;
{ Copies rectangular area of pixels from TBaseImage instance to existing FMX bitmap.}
procedure CopyRectToFmxBitmap(Image: TBaseImage; Bitmap: TBitmap;
  SrcX, SrcY, Width, Height, DstX, DstY: LongInt); overload;

implementation

procedure ConvertFmxBitmapToImageData(const Bitmap: TBitmap; Image: TImageData);
var
  Color32: TColor32Rec;
  MapData: TBitmapData;
  SourceData: PAlphaColorRec;
  TargetData: PByte;
  X, Y, Bpp, SrcWidthBytes: Integer;
  TargetInfo: TImageFormatInfo;
begin
  Assert(TestImage(Image) and not Bitmap.IsEmpty);

  Bitmap.Map(TMapAccess.Read, MapData);
  GetImageFormatInfo(Image.Format, TargetInfo);

  Bpp := TargetInfo.BytesPerPixel;
  SrcWidthBytes := Image.Width * Bpp;
  TargetData := @PByteArray(Image.Bits)[0];

  for Y := 0 to Bitmap.Height - 1 do
   for X := 0 to Bitmap.Width - 1 do
    begin
      SourceData:= @PAlphaColorRecArray(MapData.Data)[Y * (MapData.Pitch div 4) + X];
      case TargetInfo.Format of
        ifIndex8:
          begin
            Image.Palette[TargetData^].R := SourceData^.R;
            Image.Palette[TargetData^].G := SourceData^.G;
            Image.Palette[TargetData^].B := SourceData^.B;
            Image.Palette[TargetData^].A := SourceData^.A;
          end;
        ifGray8:
            TargetData^ := SourceData.R;
        ifA8Gray8:
          begin
            TargetData^ := SourceData.R;
            PWordRec(TargetData).High := SourceData.A;
          end;
        ifGray16:
            PWord(TargetData)^ := SourceData.R;
        ifR8G8B8:
          begin
            PColor24Rec(TargetData)^.R := SourceData.R;
            PColor24Rec(TargetData)^.G := SourceData.G;
            PColor24Rec(TargetData)^.B := SourceData.B;
          end;
        ifA8R8G8B8:
          begin
            PColor32Rec(TargetData)^.A := SourceData^.B;
            PColor32Rec(TargetData)^.G := SourceData^.R;
            PColor32Rec(TargetData)^.R := SourceData^.G;
            PColor32Rec(TargetData)^.B := SourceData^.A;
          end;
        ifR16G16B16:
          begin
            PColor48Rec(TargetData).R := Round(SourceData.R * $FFFF / 255);
            PColor48Rec(TargetData).G := Round(SourceData.G * $FFFF / 255);
            PColor48Rec(TargetData).B := Round(SourceData.B * $FFFF / 255);
          end;
        ifA16R16G16B16:
          begin
            PColor64Rec(TargetData).R  := Round(SourceData.R * $FFFF / 255);
            PColor64Rec(TargetData).G  := Round(SourceData.G * $FFFF / 255);
            PColor64Rec(TargetData).B  := Round(SourceData.B * $FFFF / 255);
            PColor64Rec(TargetData).A  := Round(SourceData.A * $FFFF / 255);
          end;
      else
        Color32.R := SourceData^.R;
        Color32.G := SourceData^.G;
        Color32.B := SourceData^.B;
        Color32.A := SourceData^.A;
        TargetInfo.SetPixel32(TargetData,@TargetInfo, Image.Palette,Color32);
      end;
      Inc(TargetData, Bpp);
    end;
  Bitmap.Unmap(MapData);
end;

procedure ConvertFmxBitmapToImage(const Bitmap: TBitmap; Image: TBaseImage);
begin
  ConvertFmxBitmapToImageData(Bitmap, Image.ImageDataPointer^);
end;

procedure ConvertImageDataToFmxBitmap(const Image: TImageData; Bitmap: TBitmap);
begin
  Assert(TestImage(Image));
  Bitmap.SetSize(Image.Width, Image.Height);
  CopyRectToFmxBitmap(Image, Bitmap, 0, 0, Image.Width, Image.Height, 0, 0);
end;

procedure ConvertImageToFmxBitmap(Image: TBaseImage; Bitmap: TBitmap);
begin
  ConvertImageDataToFmxBitmap(Image.ImageDataPointer^, Bitmap);
end;

procedure ConvertToAlphaColorRec(SrcPix: PByte; DestPix: PAlphaColorRec;
  const SrcInfo: TImageFormatInfo; SrcPalette: PPalette32);
var
  Color32: TColor32Rec;
begin
  case SrcInfo.Format of
    ifIndex8:
      begin
        DestPix.R := SrcPalette[SrcPix^].R;
        DestPix.G := SrcPalette[SrcPix^].G;
        DestPix.B := SrcPalette[SrcPix^].B;
        DestPix.A := SrcPalette[SrcPix^].A;
      end;
    ifGray8:
      begin
        DestPix.R := SrcPix^;
        DestPix.G := SrcPix^;
        DestPix.B := SrcPix^;
        DestPix.A := 255;
      end;
    ifR8G8B8:
      begin
        PColor24Rec(DestPix)^ := PColor24Rec(SrcPix)^;
        DestPix.A := 255;
      end;
    ifA8R8G8B8:
      PColor32Rec(DestPix)^ := PColor32Rec(SrcPix)^;
  else
    PColor32Rec(DestPix)^ := SrcInfo.GetPixel32(SrcPix, @SrcInfo, SrcPalette);
  end;
end;

procedure CopyRectToFmxBitmap(const Image: TImageData; Bitmap: TBitmap;
  SrcX, SrcY, Width, Height, DstX, DstY: LongInt);
var
  TempImage: TImageData;
  X, Y, Bpp, SrcLineBytes: Integer;
  SrcPtr: PByte;
  Info: TImageFormatInfo;
  MapData: TBitmapData;
  DstPtr: PAlphaColorRec;
  BGRA: TAlphaColorRec;
begin
  Assert(TestImage(Image) and not Bitmap.IsEmpty);

  if not (Bitmap.PixelFormat in [TPixelFormat.RGBA, TPixelFormat.BGRA]) then
      raise Exception.CreateFmt('Unsupported FMX TBitmap pixel format "%s"', [PixelFormatToString(Bitmap.PixelFormat)]);

  ClipCopyBounds(SrcX, SrcY, Width, Height, DstX, DstY, Image.Width,
    Image.Height, Rect(0, 0, Bitmap.Width, Bitmap.Height));
  GetImageFormatInfo(Image.Format, Info);

  if not Info.IsSpecial then
  begin
    Bpp := Info.BytesPerPixel;
    SrcLineBytes := Image.Width * Bpp;
    Bitmap.Map(TMapAccess.Write, MapData);

    for Y := 0 to Height - 1 do
    begin
      SrcPtr := @PByteArray(Image.Bits)[(SrcY + Y) * SrcLineBytes + SrcX * Bpp];
      DstPtr := @PAlphaColorRecArray(MapData.GetScanline(DstY + Y))[DstX];

      for X := 0 to Width - 1 do
      begin
        if Info.Format = ifA8R8G8B8 then
          BGRA := PAlphaColorRec(SrcPtr)^
        else
          ConvertToAlphaColorRec(SrcPtr, @BGRA, Info, Image.Palette);

        if MapData.PixelFormat = TPixelFormat.RGBA then
          SwapValues(BGRA.R, BGRA.B);

        // Alpha premultiplication is needed for FMX bitmaps to display correctly in TImage etc.
        DstPtr.Color := PremultiplyAlpha(BGRA.Color);

        Inc(SrcPtr, Bpp);
        Inc(DstPtr);
      end;
    end;
  end
  else
  begin
    InitImage(TempImage);
    CloneImage(Image, TempImage);
    ConvertImage(TempImage, ifA8R8G8B8);
    try
      CopyRectToFmxBitmap(TempImage, Bitmap, SrcX, SrcY, Width, Height, DstX, DstY);
    finally
      FreeImage(TempImage);
    end;
  end;
  Bitmap.Unmap(MapData);
end;

procedure CopyRectToFmxBitmap(Image: TBaseImage; Bitmap: TBitmap;
  SrcX, SrcY, Width, Height, DstX, DstY: LongInt);
begin
  CopyRectToFmxBitmap(Image.ImageDataPointer^, Bitmap,
    SrcX, SrcY, Width, Height, DstX, DstY);
end;

end.
