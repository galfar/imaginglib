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

{ Tests of canvas classes (ImagingCanvases unit).}
unit TestImagingCanvases;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Types, Classes, ImagingTypes, Imaging, ImagingFormats, ImagingClasses,
  ImagingCanvases, ImagingUtility, ImagingTestCase, ImagingTestPixels;

type
  { Drawing and filters of canvases: results, clipping, and sizes where
    32-bit intermediate values used to overflow.}
  TTestCanvases = class(TImagingTestCase)
  private
    FImage: TImageData;
    FCanvas: TImagingCanvas;
    { Creates image with all bytes set to zero and canvas for it.}
    procedure NewCanvas(Width, Height: Integer; Format: TImageFormat);
    function GetByte(X, Y: Integer): Byte;
    procedure SetByte(X, Y: Integer; Value: Byte);
    procedure FillBytes(Value: Byte);
    function GetColor(X, Y: Integer): TColor32;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEllipseIsClipped;
    procedure TestEllipseOutsideOfCanvas;
    procedure TestLargeEllipse;
    procedure TestFillRectBlend;
    procedure TestDrawBlend;
    procedure TestPointTransformInClipRect;
    procedure TestFillChannel;
    procedure TestConvolution;
    procedure TestNonLinearFilters;
    procedure TestStretchDrawAlphaLongSource;
    procedure TestStretchDrawClipped;
  end;

implementation

{ TTestCanvases }

procedure TTestCanvases.SetUp;
begin
  InitImage(FImage);
  FCanvas := nil;
end;

procedure TTestCanvases.TearDown;
begin
  FreeAndNil(FCanvas);
  FreeImage(FImage);
end;

procedure TTestCanvases.NewCanvas(Width, Height: Integer; Format: TImageFormat);
begin
  FreeAndNil(FCanvas);
  CheckTrue(NewImage(Width, Height, Format, FImage), 'NewImage');
  FillChar(FImage.Bits^, FImage.Size, 0);
  FCanvas := FindBestCanvasForImage(FImage).CreateForData(@FImage);
end;

function TTestCanvases.GetByte(X, Y: Integer): Byte;
begin
  // For images with one byte per pixel
  Result := PBuffer(FImage.Bits)[NativeInt(Y) * FImage.Width + X];
end;

procedure TTestCanvases.SetByte(X, Y: Integer; Value: Byte);
begin
  PBuffer(FImage.Bits)[NativeInt(Y) * FImage.Width + X] := Value;
end;

procedure TTestCanvases.FillBytes(Value: Byte);
begin
  FillChar(FImage.Bits^, FImage.Size, Value);
end;

function TTestCanvases.GetColor(X, Y: Integer): TColor32;
begin
  Result := GetPixel32(FImage, X, Y).Color;
end;

procedure TTestCanvases.TestEllipseIsClipped;
const
  Black = TColor32($FF000000);
  White = TColor32($FFFFFFFF);
  Formats: array[0..3] of TImageFormat = (ifGray8, ifA1R5G5B5, ifR8G8B8, ifA8R8G8B8);
var
  F, X, Y: Integer;
  Clip: TRect;
  Name: string;
begin
  // Formats with 1 to 4 bytes per pixel, the fill is done differently for them
  Clip := Rect(5, 5, 25, 25);

  for F := Low(Formats) to High(Formats) do
  begin
    Name := GetFormatName(Formats[F]);
    NewCanvas(40, 30, Formats[F]);
    FCanvas.FillColor32 := Black;
    FCanvas.Clear;

    FCanvas.ClipRect := Clip;
    FCanvas.FillColor32 := White;
    FCanvas.PenColor32 := White;
    // The right part of the ellipse is outside of the clipping rectangle
    FCanvas.Ellipse(Rect(10, 8, 34, 22));

    CheckEqualsColor32(White, GetColor(22, 15), Name + ': center of the ellipse');
    CheckEqualsColor32(White, GetColor(Clip.Right - 1, 15), Name + ': the last column inside');
    CheckEqualsColor32(White, GetColor(10, 15), Name + ': the left end');
    CheckEqualsColor32(Black, GetColor(9, 15), Name + ': left of the ellipse');
    CheckEqualsColor32(Black, GetColor(11, 9), Name + ': corner of the bounding rectangle');

    for Y := 0 to FImage.Height - 1 do
      for X := 0 to FImage.Width - 1 do
      begin
        if (X < Clip.Left) or (X >= Clip.Right) or (Y < Clip.Top) or (Y >= Clip.Bottom) then
        begin
          if GetColor(X, Y) <> Black then
            Fail(Format('%s: pixel [%d, %d] outside of the clipping rectangle was drawn', [Name, X, Y]));
        end;
      end;
  end;
end;

procedure TTestCanvases.TestEllipseOutsideOfCanvas;
const
  Formats: array[0..3] of TImageFormat = (ifGray8, ifGray16, ifR8G8B8, ifA8R8G8B8);
var
  F: Integer;
  I: NativeInt;
  Name: string;
begin
  for F := Low(Formats) to High(Formats) do
  begin
    Name := GetFormatName(Formats[F]);
    NewCanvas(20, 20, Formats[F]);
    FCanvas.FillColor32 := $FFFFFFFF;
    FCanvas.PenColor32 := $FFFFFFFF;
    // Nothing of these is inside of the image
    FCanvas.Ellipse(Rect(30, 5, 50, 15));
    FCanvas.Ellipse(Rect(-40, 5, -20, 15));
    FCanvas.Ellipse(Rect(5, 30, 15, 50));
    FCanvas.Ellipse(Rect(5, -40, 15, -20));

    for I := 0 to FImage.Size - 1 do
    begin
      if PBuffer(FImage.Bits)[I] <> 0 then
        Fail(Format('%s: byte %d of the image was changed', [Name, I]));
    end;
  end;
end;

procedure TTestCanvases.TestLargeEllipse;
const
  Side = 4200;
  Border = 100;
  Radius = (Side - 2 * Border) div 2;
var
  I, Count: NativeInt;
  Expected: Double;
begin
  // Radius * Radius * Radius does not fit into 32 bits from radius of 1291 pixels
  NewCanvas(Side, Side, ifGray8);
  FCanvas.FillColor32 := $FFFFFFFF;
  FCanvas.PenColor32 := $FFFFFFFF;
  FCanvas.Ellipse(Rect(Border, Border, Side - Border, Side - Border));

  CheckEquals(255, GetByte(Side div 2, Side div 2), 'Center');
  CheckEquals(255, GetByte(Border + 2, Side div 2), 'Left end');
  CheckEquals(255, GetByte(Side - Border - 2, Side div 2), 'Right end');
  CheckEquals(255, GetByte(Side div 2, Border + 2), 'Top end');
  CheckEquals(255, GetByte(Side div 2, Side - Border - 2), 'Bottom end');
  CheckEquals(0, GetByte(Border + 200, Border + 200), 'Corner of the bounding rectangle');
  CheckEquals(0, GetByte(Border - 2, Side div 2), 'Left of the circle');
  CheckEquals(0, GetByte(Side div 2, Border - 2), 'Above the circle');
  // Point on the diagonal, 0.7 of radius from the center in both directions (inside)
  CheckEquals(255, GetByte(Side div 2 + Radius * 7 div 10, Side div 2 + Radius * 7 div 10), 'Inside on the diagonal');
  CheckEquals(0, GetByte(Side div 2 + Radius * 72 div 100, Side div 2 + Radius * 72 div 100), 'Outside on the diagonal');

  Count := 0;
  for I := 0 to FImage.Size - 1 do
  begin
    if PBuffer(FImage.Bits)[I] <> 0 then
      Inc(Count);
  end;

  Expected := Pi * Radius * Radius;
  CheckTrue(Abs(Count - Expected) < Expected * 0.005,
            Format('Area of the circle: %d pixels, expected about %.0f', [Count, Expected]));
end;

procedure TTestCanvases.TestFillRectBlend;
const
  Back = TColor32($FF102030);
  Fill = TColor32($FF4080C0);
var
  X, Y: Integer;
  Inside: Boolean;
begin
  NewCanvas(16, 12, ifA8R8G8B8);
  FCanvas.FillColor32 := Back;
  FCanvas.Clear;
  FCanvas.FillColor32 := Fill;
  // The rectangle is clipped at the right and bottom edges of the image
  FCanvas.FillRectBlend(Rect(4, 3, 20, 15), bfOne, bfZero);

  for Y := 0 to FImage.Height - 1 do
    for X := 0 to FImage.Width - 1 do
    begin
      Inside := (X >= 4) and (Y >= 3);
      if Inside then
        CheckEqualsColor32(Fill, GetColor(X, Y), Format('Pixel [%d, %d] inside', [X, Y]))
      else
        CheckEqualsColor32(Back, GetColor(X, Y), Format('Pixel [%d, %d] outside', [X, Y]));
    end;
end;

procedure TTestCanvases.TestDrawBlend;
const
  DestX = 15;
  DestY = 16;
var
  Src: TImageData;
  SrcCanvas: TImagingCanvas;
  X, Y: Integer;
  SrcColor: TColor32;
begin
  NewCanvas(20, 20, ifR8G8B8);
  InitImage(Src);
  SrcCanvas := nil;
  try
    CheckTrue(NewImage(8, 8, ifR8G8B8, Src), 'NewImage');
    FillTestImage(Src, 31);
    SrcCanvas := FindBestCanvasForImage(Src).CreateForData(@Src);
    // Only 5x4 pixels of the source fit into the destination
    SrcCanvas.DrawBlend(Rect(0, 0, 8, 8), FCanvas, DestX, DestY, bfOne, bfZero);

    for Y := 0 to FImage.Height - 1 do
      for X := 0 to FImage.Width - 1 do
      begin
        if (X >= DestX) and (Y >= DestY) then
          SrcColor := GetPixel32(Src, X - DestX, Y - DestY).Color
        else
          SrcColor := $FF000000;
        CheckEqualsColor32(SrcColor, GetColor(X, Y), Format('Pixel [%d, %d]', [X, Y]));
      end;
  finally
    SrcCanvas.Free;
    FreeImage(Src);
  end;
end;

procedure TTestCanvases.TestPointTransformInClipRect;
var
  X, Y, Value: Integer;
  Clip: TRect;
begin
  NewCanvas(12, 10, ifGray8);
  for Y := 0 to FImage.Height - 1 do
    for X := 0 to FImage.Width - 1 do
      SetByte(X, Y, Y * FImage.Width + X);

  Clip := Rect(2, 3, 9, 8);
  FCanvas.ClipRect := Clip;
  FCanvas.InvertColors;

  for Y := 0 to FImage.Height - 1 do
    for X := 0 to FImage.Width - 1 do
    begin
      Value := Y * FImage.Width + X;
      if (X >= Clip.Left) and (X < Clip.Right) and (Y >= Clip.Top) and (Y < Clip.Bottom) then
        Value := 255 - Value;
      CheckEquals(Value, GetByte(X, Y), Format('Pixel [%d, %d]', [X, Y]));
    end;
end;

procedure TTestCanvases.TestFillChannel;
var
  Original: TImageData;
  X, Y: Integer;
  Clip: TRect;
  Before, After: TColor32Rec;
  Msg: string;
begin
  NewCanvas(10, 10, ifA8R8G8B8);
  FillTestImage(FImage, 7);
  InitImage(Original);
  try
    CheckTrue(CloneImage(FImage, Original), 'CloneImage');
    Clip := Rect(1, 2, 6, 9);
    FCanvas.ClipRect := Clip;
    FCanvas.FillChannel(ChannelAlpha, 77);
    FCanvas.FillChannelFP(ChannelRed, 1.0);

    for Y := 0 to FImage.Height - 1 do
      for X := 0 to FImage.Width - 1 do
      begin
        Msg := Format('Pixel [%d, %d]', [X, Y]);
        Before := GetPixel32(Original, X, Y);
        After := GetPixel32(FImage, X, Y);
        if (X >= Clip.Left) and (X < Clip.Right) and (Y >= Clip.Top) and (Y < Clip.Bottom) then
        begin
          CheckEquals(77, After.A, Msg + ': alpha');
          CheckEquals(255, After.R, Msg + ': red');
          CheckEquals(Before.G, After.G, Msg + ': green');
          CheckEquals(Before.B, After.B, Msg + ': blue');
        end
        else
          CheckEqualsColor32(Before.Color, After.Color, Msg + ' outside');
      end;
  finally
    FreeImage(Original);
  end;
end;

procedure TTestCanvases.TestConvolution;
var
  I: NativeInt;
begin
  NewCanvas(9, 7, ifGray8);

  // Kernels with negative values: sum of Laplace kernel is zero, of sharpening one
  FillBytes(100);
  FCanvas.ApplyConvolution3x3(FilterLaplace3x3);
  for I := 0 to FImage.Size - 1 do
    CheckEquals(0, PBuffer(FImage.Bits)[I], Format('Laplace of flat image, pixel %d', [I]));

  FillBytes(100);
  FCanvas.ApplyConvolution3x3(FilterSharpen3x3);
  for I := 0 to FImage.Size - 1 do
    CheckEquals(100, PBuffer(FImage.Bits)[I], Format('Sharpening of flat image, pixel %d', [I]));

  FillBytes(100);
  FCanvas.ApplyConvolution5x5(FilterLaplace5x5);
  for I := 0 to FImage.Size - 1 do
    CheckEquals(0, PBuffer(FImage.Bits)[I], Format('Laplace 5x5 of flat image, pixel %d', [I]));

  // One light pixel is spread by the weights of the kernel
  FillBytes(0);
  SetByte(4, 3, 160);
  FCanvas.ApplyConvolution3x3(FilterGaussian3x3);
  CheckEquals(40, GetByte(4, 3), 'Gaussian: center');
  CheckEquals(20, GetByte(3, 3), 'Gaussian: left');
  CheckEquals(20, GetByte(4, 4), 'Gaussian: below');
  CheckEquals(10, GetByte(5, 2), 'Gaussian: corner');
  CheckEquals(0, GetByte(6, 3), 'Gaussian: two pixels away');
  CheckEquals(0, GetByte(4, 1), 'Gaussian: two rows away');
end;

procedure TTestCanvases.TestNonLinearFilters;
var
  X, Y, Expected: Integer;
begin
  NewCanvas(9, 7, ifGray8);
  SetByte(4, 3, 200);

  FCanvas.ApplyMaxFilter(3);
  for Y := 0 to FImage.Height - 1 do
    for X := 0 to FImage.Width - 1 do
    begin
      Expected := Iff((Abs(X - 4) <= 1) and (Abs(Y - 3) <= 1), 200, 0);
      CheckEquals(Expected, GetByte(X, Y), Format('Max filter, pixel [%d, %d]', [X, Y]));
    end;

  FCanvas.ApplyMinFilter(3);
  for Y := 0 to FImage.Height - 1 do
    for X := 0 to FImage.Width - 1 do
    begin
      Expected := Iff((X = 4) and (Y = 3), 200, 0);
      CheckEquals(Expected, GetByte(X, Y), Format('Min filter, pixel [%d, %d]', [X, Y]));
    end;

  FCanvas.ApplyMedianFilter(3);
  for Y := 0 to FImage.Height - 1 do
    for X := 0 to FImage.Width - 1 do
      CheckEquals(0, GetByte(X, Y), Format('Median filter, pixel [%d, %d]', [X, Y]));
end;

procedure TTestCanvases.TestStretchDrawAlphaLongSource;
const
  // Source positions of 32768 and more do not fit into 16.16 fixed point numbers
  SrcWidth = 40000;
  DestWidth = 400;
  BlockWidth = SrcWidth div DestWidth;
  // Alpha blending of the fast canvas is not exact even for opaque pixels
  Tolerance = 2;

  function BlockColor(Block: Integer): TColor32Rec;
  begin
    Result.A := 255;
    Result.R := (Block * 37) mod 251;
    Result.G := (Block * 91) mod 251;
    Result.B := (Block * 13) mod 251;
  end;

var
  Dest: TImageData;
  DestCanvas: TImagingCanvas;

  procedure CheckDest(const Msg: string);
  var
    X, Y: Integer;
    Expected, Actual: TColor32Rec;
  begin
    for Y := 0 to Dest.Height - 1 do
      for X := 0 to Dest.Width - 1 do
      begin
        Expected := BlockColor(X);
        Actual := PColor32RecArray(Dest.Bits)[Y * Dest.Width + X];
        if (Abs(Expected.R - Actual.R) > Tolerance) or (Abs(Expected.G - Actual.G) > Tolerance) or
          (Abs(Expected.B - Actual.B) > Tolerance) or (Actual.A <> 255) then
        begin
          Fail(Format('%s: pixel [%d, %d] is %.8x, expected %.8x', [Msg, X, Y, Actual.Color, Expected.Color]));
        end;
      end;
  end;

var
  X, Y: Integer;
begin
  // Each pixel of destination corresponds to block of pixels of one color in the source
  NewCanvas(SrcWidth, 2, ifA8R8G8B8);
  CheckTrue(FCanvas is TFastARGB32Canvas, 'Fast canvas is used');
  for Y := 0 to FImage.Height - 1 do
    for X := 0 to FImage.Width - 1 do
      PColor32RecArray(FImage.Bits)[Y * FImage.Width + X] := BlockColor(X div BlockWidth);

  InitImage(Dest);
  DestCanvas := nil;
  try
    CheckTrue(NewImage(DestWidth, 2, ifA8R8G8B8, Dest), 'NewImage');
    DestCanvas := FindBestCanvasForImage(Dest).CreateForData(@Dest);

    FillChar(Dest.Bits^, Dest.Size, 0);
    FCanvas.StretchDrawAlpha(Rect(0, 0, SrcWidth, 2), DestCanvas, Rect(0, 0, DestWidth, 2), rfNearest);
    CheckDest('Nearest');

    FillChar(Dest.Bits^, Dest.Size, 0);
    FCanvas.StretchDrawAlpha(Rect(0, 0, SrcWidth, 2), DestCanvas, Rect(0, 0, DestWidth, 2), rfBilinear);
    CheckDest('Bilinear');
  finally
    DestCanvas.Free;
    FreeImage(Dest);
  end;
end;

procedure TTestCanvases.TestStretchDrawClipped;
const
  SrcWidth = 30;
  SrcHeight = 20;
  DestWidth = 220;
  DestHeight = 160;

  function IsZero(P: PByte; Count: Integer): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := 0 to Count - 1 do
      if PBuffer(P)[I] <> 0 then
        Result := False;
  end;

  procedure CheckDraw(DataFormat: TImageFormat; Filter: TResizeFilter; const DestRect, Clip: TRect);
  var
    Full, Clipped: TImageData;
    FullCanvas, ClippedCanvas: TImagingCanvas;
    Info: TImageFormatInfo;
    X, Y, Bpp: Integer;
    Inside: Boolean;
    FullPix, ClippedPix: PByte;
    Msg: string;
  begin
    GetImageFormatInfo(DataFormat, Info);
    Bpp := Info.BytesPerPixel;
    Msg := Format('%s filter %d to [%d, %d, %d, %d] clip [%d, %d, %d, %d]', [Info.Name, Ord(Filter),
      DestRect.Left, DestRect.Top, DestRect.Right, DestRect.Bottom, Clip.Left, Clip.Top, Clip.Right, Clip.Bottom]);

    NewCanvas(SrcWidth, SrcHeight, DataFormat);
    FillTestImage(FImage, 20261021);
    InitImage(Full);
    InitImage(Clipped);
    FullCanvas := nil;
    ClippedCanvas := nil;
    try
      CheckTrue(NewImage(DestWidth, DestHeight, DataFormat, Full), 'NewImage');
      CheckTrue(NewImage(DestWidth, DestHeight, DataFormat, Clipped), 'NewImage');
      FillChar(Full.Bits^, Full.Size, 0);
      FillChar(Clipped.Bits^, Clipped.Size, 0);
      FullCanvas := FindBestCanvasForImage(Full).CreateForData(@Full);
      ClippedCanvas := FindBestCanvasForImage(Clipped).CreateForData(@Clipped);
      ClippedCanvas.ClipRect := Clip;

      FCanvas.StretchDrawAlpha(Rect(0, 0, SrcWidth, SrcHeight), FullCanvas, DestRect, Filter);
      FCanvas.StretchDrawAlpha(Rect(0, 0, SrcWidth, SrcHeight), ClippedCanvas, DestRect, Filter);

      // Inside of the clip the pixels are the same as without it, outside stay zero
      for Y := 0 to DestHeight - 1 do
        for X := 0 to DestWidth - 1 do
        begin
          Inside := (X >= Clip.Left) and (X < Clip.Right) and (Y >= Clip.Top) and (Y < Clip.Bottom);
          FullPix := @PBuffer(Full.Bits)[(PtrInt(Y) * DestWidth + X) * Bpp];
          ClippedPix := @PBuffer(Clipped.Bits)[(PtrInt(Y) * DestWidth + X) * Bpp];

          if Inside and not CompareMem(FullPix, ClippedPix, Bpp) then
            Fail(Format('%s: pixel [%d, %d] differs from unclipped drawing', [Msg, X, Y]));
          if not Inside and not IsZero(ClippedPix, Bpp) then
            Fail(Format('%s: pixel [%d, %d] outside of the clip is drawn', [Msg, X, Y]));
        end;
    finally
      FullCanvas.Free;
      ClippedCanvas.Free;
      FreeImage(Full);
      FreeImage(Clipped);
    end;
  end;

const
  // TFastARGB32Canvas (own nearest and bilinear, bicubic by the base class)
  // and the base canvas class
  Formats: array[0..1] of TImageFormat = (ifA8R8G8B8, ifR8G8B8);
  Filters: array[0..2] of TResizeFilter = (rfNearest, rfBilinear, rfBicubic);
var
  I, J: Integer;
begin
  for I := Low(Formats) to High(Formats) do
    for J := Low(Filters) to High(Filters) do
    begin
      // 10x zoom starting before the image, clip in the middle
      CheckDraw(Formats[I], Filters[J], Rect(-15, -7, 285, 193), Rect(13, 9, 170, 120));
      // 7.3x zoom, clip cuts the destination rectangle on all sides
      CheckDraw(Formats[I], Filters[J], Rect(5, 3, 224, 149), Rect(41, 37, 150, 100));
      // Downscale
      CheckDraw(Formats[I], Filters[J], Rect(10, 10, 21, 18), Rect(13, 11, 20, 17));
    end;
end;

initialization
  RegisterImagingTest(TTestCanvases);

end.
