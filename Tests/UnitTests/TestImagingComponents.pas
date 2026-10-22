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

{ Tests of conversions between TImageData and VCL TBitmap (ImagingComponents
  unit). Delphi only: TBitmap of LCL needs a widget set, LCL conversions and
  drawing are tested by Tests/DisplayTest for each widget set.}
unit TestImagingComponents;

{$I ImagingOptions.inc}

interface

{$IFNDEF FPC}
uses
  SysUtils, Types, Classes, Graphics, ImagingTypes, Imaging, ImagingComponents,
  ImagingTestCase;

type
  { Conversions between TImageData and VCL TBitmap, size limits of TBitmap.}
  TTestVclBitmapConversions = class(TImagingTestCase)
  private
    { Image of Width x Height pixels with colors depending on position, in
      given data format.}
    procedure MakeImage(Width, Height: Integer; Format: TImageFormat; var Image: TImageData);
    procedure CheckRoundTrip(DataFormat: TImageFormat; Tolerance: Integer);
    procedure CheckBitmapToData(PF: TPixelFormat; ExpectedFormat: TImageFormat; Tolerance: Integer);
    procedure CheckLimit(Width, Height: Integer; DataFormat: TImageFormat; Expected: Boolean);
  published
    procedure TestOpaqueFormatsRoundTrip;
    procedure TestX8R8G8B8UnusedByte;
    procedure TestAlphaRoundTrip;
    procedure TestIndexedTransparentPalette;
    procedure TestBitmapFormatsToData;
    procedure TestBitmapIsNotChanged;
    procedure TestSizeLimits;
  end;
{$ENDIF}

implementation

{$IFNDEF FPC}

{ TTestVclBitmapConversions }

procedure TTestVclBitmapConversions.MakeImage(Width, Height: Integer; Format: TImageFormat;
  var Image: TImageData);
var
  X, Y: Integer;
  P: PColor32Rec;
begin
  NewImage(Width, Height, ifA8R8G8B8, Image);
  for Y := 0 to Height - 1 do
    for X := 0 to Width - 1 do
    begin
      P := GetPixelPointer(Image, X, Y, 4);
      P.R := (X * 37 + Y * 11) and $FF;
      P.G := (X * 13 + Y * 51) and $FF;
      P.B := (X * 7 + Y * 23 + 90) and $FF;
      P.A := 255;
    end;
  ConvertImage(Image, Format);
end;

procedure TTestVclBitmapConversions.CheckRoundTrip(DataFormat: TImageFormat; Tolerance: Integer);
var
  Image, Back: TImageData;
  Bitmap: TBitmap;
  X, Y: Integer;
  C1, C2: TColor32Rec;
  Name: string;
begin
  InitImage(Image);
  InitImage(Back);
  Bitmap := TBitmap.Create;
  try
    MakeImage(16, 8, DataFormat, Image);
    Name := GetFormatName(DataFormat);
    ConvertDataToBitmap(Image, Bitmap);
    ConvertBitmapToData(Bitmap, Back);
    CheckEquals(16, Back.Width, Name + ' width');
    CheckEquals(8, Back.Height, Name + ' height');

    for Y := 0 to Image.Height - 1 do
      for X := 0 to Image.Width - 1 do
      begin
        C1 := GetPixel32(Image, X, Y);
        C2 := GetPixel32(Back, X, Y);
        CheckEquals(255, C2.A, Format('%s alpha at [%d,%d]', [Name, X, Y]));
        CheckTrue((Abs(C1.R - C2.R) <= Tolerance) and (Abs(C1.G - C2.G) <= Tolerance) and
                  (Abs(C1.B - C2.B) <= Tolerance),
                  Format('%s at [%d,%d]: %d,%d,%d came back as %d,%d,%d',
                         [Name, X, Y, C1.R, C1.G, C1.B, C2.R, C2.G, C2.B]));
      end;
  finally
    Bitmap.Free;
    FreeImage(Image);
    FreeImage(Back);
  end;
end;

procedure TTestVclBitmapConversions.TestOpaqueFormatsRoundTrip;
begin
  // Formats with TBitmap counterpart are copied as they are
  CheckRoundTrip(ifR8G8B8, 0);
  CheckRoundTrip(ifR5G6B5, 0);
  CheckRoundTrip(ifIndex8, 0);
  // Other ones are converted to the nearest bitmap pixel format
  CheckRoundTrip(ifX1R5G5B5, 4);
  CheckRoundTrip(ifGray8, 0);
  CheckRoundTrip(ifGray16, 1);
  CheckRoundTrip(ifR16G16B16, 1);
  CheckRoundTrip(ifA16R16G16B16, 1);
end;

procedure TTestVclBitmapConversions.TestX8R8G8B8UnusedByte;
var
  Image, Back: TImageData;
  Bitmap: TBitmap;
  I: Integer;
  P: PColor32Rec;
  C: TColor32Rec;
begin
  InitImage(Image);
  InitImage(Back);
  Bitmap := TBitmap.Create;
  try
    NewImage(4, 4, ifX8R8G8B8, Image);
    for I := 0 to 15 do
    begin
      P := @PColor32RecBuffer(Image.Bits)[I];
      P.R := 200;
      P.G := 100;
      P.B := 50;
      P.A := 0; // Unused byte must not become alpha
    end;

    ConvertDataToBitmap(Image, Bitmap);
    CheckTrue(Bitmap.PixelFormat = pf32bit, 'pixel format of bitmap');
{$IF CompilerVersion >= 20.0}
    CheckTrue(Bitmap.AlphaFormat = afIgnored, 'bitmap has no alpha');
{$IFEND}
    CheckEquals(TColor($3264C8), ColorToRGB(Bitmap.Canvas.Pixels[1, 1]), 'color in bitmap');

    // Alpha 0 everywhere is taken as bitmap without alpha data
    ConvertBitmapToData(Bitmap, Back);
    CheckEquals(Ord(ifX8R8G8B8), Ord(Back.Format), 'data format back');
    C := GetPixel32(Back, 1, 1);
    CheckTrue((C.A = 255) and (C.R = 200) and (C.G = 100) and (C.B = 50),
              Format('color back %d,%d,%d,%d', [C.A, C.R, C.G, C.B]));
  finally
    Bitmap.Free;
    FreeImage(Image);
    FreeImage(Back);
  end;
end;

procedure TTestVclBitmapConversions.TestAlphaRoundTrip;
const
  Alphas: array[0..3] of Byte = (0, 1, 128, 255);
var
  Image, Back: TImageData;
  Bitmap: TBitmap;
  I: Integer;
  P: PColor32Rec;
  C: TColor32Rec;
begin
  InitImage(Image);
  InitImage(Back);
  Bitmap := TBitmap.Create;
  try
    NewImage(4, 1, ifA8R8G8B8, Image);
    for I := 0 to 3 do
    begin
      P := @PColor32RecBuffer(Image.Bits)[I];
      P.R := 200;
      P.G := 100;
      P.B := 50;
      P.A := Alphas[I];
    end;

    ConvertDataToBitmap(Image, Bitmap);
    ConvertBitmapToData(Bitmap, Back);
    CheckEquals(Ord(ifA8R8G8B8), Ord(Back.Format), 'data format back');

    for I := 0 to 3 do
    begin
      C := GetPixel32(Back, I, 0);
      CheckEquals(Alphas[I], C.A, Format('alpha %d', [Alphas[I]]));
      // Colors with low alpha are lost by premultiplication in VCL bitmap
      if Alphas[I] >= 128 then
      begin
        CheckTrue((Abs(C.R - 200) <= 2) and (Abs(C.G - 100) <= 2) and (Abs(C.B - 50) <= 2),
                  Format('color with alpha %d came back as %d,%d,%d', [Alphas[I], C.R, C.G, C.B]));
      end;
    end;
  finally
    Bitmap.Free;
    FreeImage(Image);
    FreeImage(Back);
  end;
end;

procedure TTestVclBitmapConversions.TestIndexedTransparentPalette;
var
  Image, Back: TImageData;
  Bitmap: TBitmap;
  C: TColor32Rec;
begin
  InitImage(Image);
  InitImage(Back);
  Bitmap := TBitmap.Create;
  try
    NewImage(2, 1, ifIndex8, Image);
    Image.Palette[0].A := 0;
    Image.Palette[0].R := 10;
    Image.Palette[0].G := 20;
    Image.Palette[0].B := 30;
    Image.Palette[1].A := 255;
    Image.Palette[1].R := 200;
    Image.Palette[1].G := 100;
    Image.Palette[1].B := 50;
    PByte(Image.Bits)^ := 0;
    PBuffer(Image.Bits)[1] := 1;

    // Alpha of palette is kept, the bitmap gets 32 bits with alpha
    ConvertDataToBitmap(Image, Bitmap);
    CheckTrue(Bitmap.PixelFormat = pf32bit, 'pixel format of bitmap');
    ConvertBitmapToData(Bitmap, Back);
    CheckEquals(Ord(ifA8R8G8B8), Ord(Back.Format), 'data format back');
    CheckEquals(0, GetPixel32(Back, 0, 0).A, 'transparent palette entry');
    C := GetPixel32(Back, 1, 0);
    CheckTrue((C.A = 255) and (C.R = 200) and (C.G = 100) and (C.B = 50),
              Format('opaque palette entry %d,%d,%d,%d', [C.A, C.R, C.G, C.B]));
  finally
    Bitmap.Free;
    FreeImage(Image);
    FreeImage(Back);
  end;
end;

procedure TTestVclBitmapConversions.CheckBitmapToData(PF: TPixelFormat; ExpectedFormat: TImageFormat;
  Tolerance: Integer);
var
  Bitmap: TBitmap;
  Image: TImageData;
  C: TColor32Rec;
  Drawn: TColor;
  Name: string;
begin
  InitImage(Image);
  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := PF;
    Bitmap.Width := 4;
    Bitmap.Height := 4;
    Bitmap.Canvas.Brush.Color := $325AC8; // R=200, G=90, B=50
    Bitmap.Canvas.FillRect(Rect(0, 0, 4, 4));
    // Palette bitmaps get the nearest palette color
    Drawn := ColorToRGB(Bitmap.Canvas.Pixels[1, 1]);
    Name := Format('pixel format %d', [Ord(PF)]);

    ConvertBitmapToData(Bitmap, Image);
    CheckEquals(Ord(ExpectedFormat), Ord(Image.Format), Name + ' data format');
    C := GetPixel32(Image, 1, 1);
    CheckEquals(255, C.A, Name + ' alpha');
    CheckTrue((Abs(C.R - (Drawn and $FF)) <= Tolerance) and
              (Abs(C.G - ((Drawn shr 8) and $FF)) <= Tolerance) and
              (Abs(C.B - ((Drawn shr 16) and $FF)) <= Tolerance),
              Format('%s: %d,%d,%d, bitmap has %d,%d,%d', [Name, C.R, C.G, C.B,
                     Drawn and $FF, (Drawn shr 8) and $FF, (Drawn shr 16) and $FF]));
  finally
    Bitmap.Free;
    FreeImage(Image);
  end;
end;

procedure TTestVclBitmapConversions.TestBitmapFormatsToData;
begin
  CheckBitmapToData(pf1bit, ifIndex8, 0);
  CheckBitmapToData(pf4bit, ifIndex8, 0);
  CheckBitmapToData(pf8bit, ifIndex8, 0);
  CheckBitmapToData(pf15bit, ifR8G8B8, 8); // VCL reports it as pfCustom
  CheckBitmapToData(pf16bit, ifR5G6B5, 8);
  CheckBitmapToData(pf24bit, ifR8G8B8, 0);
  // GDI drawing leaves alpha 0, it is not taken as transparent image
  CheckBitmapToData(pf32bit, ifX8R8G8B8, 0);
  CheckBitmapToData(pfDevice, ifR8G8B8, 0);
end;

procedure TTestVclBitmapConversions.TestBitmapIsNotChanged;
const
  Formats: array[0..4] of TPixelFormat = (pf1bit, pf4bit, pf15bit, pfDevice, pf32bit);
var
  Bitmap: TBitmap;
  Image: TImageData;
  I: Integer;
  OldPF: TPixelFormat;
begin
  InitImage(Image);
  for I := Low(Formats) to High(Formats) do
  begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.PixelFormat := Formats[I];
      Bitmap.Width := 4;
      Bitmap.Height := 4;
      OldPF := Bitmap.PixelFormat;
      ConvertBitmapToData(Bitmap, Image);
      CheckTrue(Bitmap.PixelFormat = OldPF, Format('pixel format %d changed to %d',
                [Ord(OldPF), Ord(Bitmap.PixelFormat)]));
    finally
      Bitmap.Free;
      FreeImage(Image);
    end;
  end;
end;

procedure TTestVclBitmapConversions.CheckLimit(Width, Height: Integer; DataFormat: TImageFormat;
  Expected: Boolean);
var
  Image: TImageData;
begin
  // Image record without pixel data is enough for the check
  InitImage(Image);
  Image.Width := Width;
  Image.Height := Height;
  Image.Format := DataFormat;
  CheckEquals(Expected, CanConvertDataToBitmap(Image),
              Format('%dx%d %s', [Width, Height, GetFormatName(DataFormat)]));
end;

procedure TTestVclBitmapConversions.TestSizeLimits;
begin
  CheckLimit(1000, 1000, ifA8R8G8B8, True);
  CheckLimit(0, 1000, ifA8R8G8B8, False);
  // DIB sections must have less than 2 GB, rows aligned to 4 bytes
  CheckLimit(23170, 23170, ifA8R8G8B8, True);
  CheckLimit(23171, 23171, ifA8R8G8B8, False);
  CheckLimit(26000, 26000, ifX8R8G8B8, False);
  CheckLimit(26000, 26000, ifR8G8B8, True);
  CheckLimit(26800, 26800, ifR8G8B8, False);
  CheckLimit(46340, 46340, ifGray8, True);
  CheckLimit(46400, 46400, ifGray8, False);
  CheckLimit(100000, 1, ifA8R8G8B8, True);
end;

{$ENDIF}

initialization
{$IFNDEF FPC}
  RegisterImagingTest(TTestVclBitmapConversions);
{$ENDIF}

end.
