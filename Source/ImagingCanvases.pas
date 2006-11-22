{
  $Id: ImagingCanvases.pas,v 1.5 2006/10/26 13:29:28 galfar Exp $
  Vampyre Imaging Library
  by Marek Mauder (pentar@seznam.cz)
  http://imaginglib.sourceforge.net

  The contents of this file are used with permission, subject to the Mozilla
  Public License Version 1.1 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  Alternatively, the contents of this file may be used under the terms of the
  GNU Lesser General Public License (the  "LGPL License"), in which case the
  provisions of the LGPL License are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the LGPL License and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting  the provisions above and
  replace  them with the notice and other provisions required by the LGPL
  License.  If you do not delete the provisions above, a recipient may use
  your version of this file under either the MPL or the LGPL License.

  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
}

{
  This unit contains canvas classes for drawing and applying effects.
}
unit ImagingCanvases;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Types, Classes, ImagingTypes, Imaging, ImagingClasses,
  ImagingFormats, ImagingUtility;

const
  { Color constants in ifA8R8G8B8 format.}
  pcClear   = $00000000;
  pcBlack   = $FF000000;
  pcWhite   = $FFFFFFFF;
  pcMaroon  = $FF800000;
  pcGreen   = $FF008000;
  pcOlive   = $FF808000;
  pcNavy    = $FF000080;
  pcPurple  = $FF800080;
  pcTeal    = $FF008080;
  pcGray    = $FF808080;
  pcSilver  = $FFC0C0C0;
  pcRed     = $FFFF0000;
  pcLime    = $FF00FF00;
  pcYellow  = $FFFF00FF;
  pcBlue    = $FF0000FF;
  pcFuchsia = $FFFF00FF;
  pcAqua    = $FF00FFFF;
  pcLtGray  = $FFC0C0C0;
  pcDkGray  = $FF808080;

type
  EImagingCanvasError = class(EImagingError);

  { Represents 3x3 convolution filter kernel.}
  TConvolutionFilter3x3 = record
    Kernel: array[0..2, 0..2] of LongInt;
    Divisor: LongInt;
  end;

  { Represents 5x5 convolution filter kernel.}
  TConvolutionFilter5x5 = record
    Kernel: array[0..4, 0..4] of LongInt;
    Divisor: LongInt;
  end;

  { Base canvas class for drawing objects, applying effects, and other.
    Constructor takes TBaseImage (or pointer to TImageData). Source image
    bits are not copied but referenced so all canvas functions affect
    source image and vice versa. When you change format or resolution of
    source image you must call UpdateCanvasState method (so canvas could
    recompute some data size related stuff).

    TImagingCanvas works for all image data formats except special ones
    (compressed). Because of this its methods are quite slow (they work
    with colors in ifA32R32G32B32F format). If you want fast drawing you
    can use one of fast canvas clases. These descendants of TImagingCanvas
    work only for few select formats (or only one) but they are optimized thus
    much faster.

    --
    Canvas in this Imaging version (0.20) is very basic and its purpose is to
    act like sort of a preview of things to come.
  }
  TImagingCanvas = class(TObject)
  private
    FPData: PImageData;
    FClipRect: TRect;
    FFillColorFP: TColorFPRec;
    FPenColorFP: TColorFPRec;
    FFillColor32: TColor32;
    FPenColor32: TColor32;
    FNativeColor: TColorFPRec;
    FFormatInfo: TImageFormatInfo;
    FDataSizeOnUpdate: LongInt;

    function GetPixel32(X, Y: LongInt): TColor32; virtual;
    function GetPixelFP(X, Y: LongInt): TColorFPRec; virtual;
    function GetFormatInfo: TImageFormatInfo;
    function GetValid: Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetPixel32(X, Y: LongInt; const Value: TColor32); virtual;
    procedure SetPixelFP(X, Y: LongInt; const Value: TColorFPRec); virtual;
    procedure SetFillColor32(const Value: TColor32); {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetFillColorFP(const Value: TColorFPRec); {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetPenColor32(const Value: TColor32); {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetPenColorFP(const Value: TColorFPRec); {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetClipRect(const Value: TRect);

    { Returns pointer to pixel at given position.}
    function GetPixelPointer(X, Y: LongInt): Pointer; {$IFDEF USE_INLINE}inline;{$ENDIF}
    { Translates given FP color to native format of canvas and stores it
      in FNativeColor field (its bit copy).}
    procedure TranslateFPToNative(const Color: TColorFPRec); {$IFDEF USE_INLINE}inline;{$ENDIF}
    { Clipping function used by horizontal and vertical line drawing functions.}
    function ClipAxisParallelLine(var A1, A2, B: LongInt;
      AStart, AStop, BStart, BStop: LongInt): Boolean;
  public
    constructor CreateForData(ImageDataPointer: PImageData);
    constructor CreateForImage(Image: TBaseImage);
    destructor Destroy; override;

    { Call this method when you change size or format of image this canvas
      operates on (like calling ResizeImage, ConvertImage, or changing Format
      property of TBaseImage descendants).}
    procedure UpdateCanvasState; virtual;
    { Resets clipping rectangle to Rect(0, 0, ImageWidth, ImageHeight).}
    procedure ResetClipRect;

    { Clears entire canvas with current fill color (ignores clipping rectangle).}
    procedure Clear;

    { Draws horizontal line with current pen color.}
    procedure HorzLine(X1, X2, Y: LongInt); virtual;
    { Draws vertical line with current pen color.}
    procedure VertLine(X, Y1, Y2: LongInt); virtual;
    { Fills given rectangle with current fill color.}
    procedure FillRect(const Rect: TRect); virtual;

    { Convolves canvas' image with given 3x3 filter kernel. You can use
      predefined filter kernels or define your own.}
    procedure ApplyConvolution3x3(const Filter: TConvolutionFilter3x3);
    { Convolves canvas' image with given 5x5 filter kernel. You can use
      predefined filter kernels or define your own.}
    procedure ApplyConvolution5x5(const Filter: TConvolutionFilter5x5);
    { Computes 2D convolution of canvas' image and given filter kernel.
      Kernel is in row format and KernelSize must be odd number >= 3. Divisor
      is normalizing value based on Kernel (usually sum of all kernel's cells).
      The Bias number shifts each color value by a fixed amount. If
      ClampChannels is True all color values are clamped to [0, 1]. You can use
      predefined filter kernels or define your own.}
    procedure ApplyConvolution(Kernel: PLongInt; KernelSize, Divisor: LongInt;
      Bias: Single = 0.0; ClampChannels: Boolean = True); virtual;

    { Color used for filling when drawing various objects.}
    property FillColor32: TColor32 read FFillColor32 write SetFillColor32;
    { Color used for filling when drawing various objects.}
    property FillColorFP: TColorFPRec read FFillColorFP write SetFillColorFP;
    { Color used when drawing lines, frames, and outlines of objects.}
    property PenColor32: TColor32 read FPenColor32 write SetPenColor32;
    { Color used when drawing lines, frames, and outlines of objects.}
    property PenColorFP: TColorFPRec read FPenColorFP write SetPenColorFP;
    { Specifies the current color of the pixels of canvas. Native pixel is
      read from canvas and then translated to 32bit ARGB. Reverse operation
      is made when setting pixel color.}
    property Pixels32[X, Y: LongInt]: TColor32 read GetPixel32 write SetPixel32;
    { Specifies the current color of the pixels of canvas. Native pixel is
      read from canvas and then translated to FP ARGB. Reverse operation
      is made when setting pixel color.}
    property PixelsFP[X, Y: LongInt]: TColorFPRec read GetPixelFP write SetPixelFP;
    { Clipping rectangle of this canvas. No pixels outside this rectangle are
      altered by canvas methods.}
    property ClipRect: TRect read FClipRect write SetClipRect;
    { Extended format information.}
    property FormatInfo: TImageFormatInfo read FFormatInfo;
    { Indicates that this canvas is in valid state. If False canvas oprations
      may crash.}
    property Valid: Boolean read GetValid;

    { Returns all formats supported by this canvas class.}
    class function GetSupportedFormats: TImageFormats; virtual;
  end;

  TImagingCanvasClass = class of TImagingCanvas;

  TScanlineArray = array[0..MaxInt div SizeOf(Pointer) - 1] of PColor32RecArray;
  PScanlineArray = ^TScanlineArray;

  { Fast canvas class for ifA8R8G8B8 format images.}
  TFastARGB32Canvas = class(TImagingCanvas)
  protected
    FScanlines: PScanlineArray;
    function GetPixel32(X, Y: LongInt): TColor32; override;
    procedure SetPixel32(X, Y: LongInt; const Value: TColor32); override;
  public
    destructor Destroy; override;

    procedure UpdateCanvasState; override;

    procedure HorzLine(X1, X2, Y: LongInt); override;
    procedure VertLine(X, Y1, Y2: LongInt); override;

    property Scanlines: PScanlineArray read FScanlines;

    class function GetSupportedFormats: TImageFormats; override;
  end;

const
  { Kernel for 3x3 average smoothing filter.}
  FilterAverage3x3: TConvolutionFilter3x3 = (
    Kernel: ((1, 1, 1),
             (1, 1, 1),
             (1, 1, 1));
    Divisor: 9);

  { Kernel for 5x5 average smoothing filter.}
  FilterAverage5x5: TConvolutionFilter5x5 = (
    Kernel: ((1, 1, 1, 1, 1),
             (1, 1, 1, 1, 1),
             (1, 1, 1, 1, 1),
             (1, 1, 1, 1, 1),
             (1, 1, 1, 1, 1));
    Divisor: 25);

  { Kernel for 3x3 Gaussian smoothing filter.}
  FilterGaussian3x3: TConvolutionFilter3x3 = (
    Kernel: ((1, 2, 1),
             (2, 4, 2),
             (1, 2, 1));
    Divisor: 16);

  { Kernel for 5x5 Gaussian smoothing filter.}
  FilterGaussian5x5: TConvolutionFilter5x5 = (
    Kernel: ((1,  4,  6,  4, 1),
             (4, 16, 24, 16, 4),
             (6, 24, 36, 24, 6),
             (4, 16, 24, 16, 4),
             (1,  4,  6,  4, 1));
    Divisor: 256);

  { Kernel for 3x3 Sobel horizontal edge detection filter (1st derivative approximation).}
  FilterSobelHorz3x3: TConvolutionFilter3x3 = (
    Kernel: (( 1,  2,  1),
             ( 0,  0,  0),
             (-1, -2, -1));
    Divisor: 1);

  { Kernel for 3x3 Sobel vertical edge detection filter (1st derivative approximation).}
  FilterSobelVert3x3: TConvolutionFilter3x3 = (
    Kernel: ((-1, 0, 1),
             (-2, 0, 2),
             (-1, 0, 1));
    Divisor: 1);

  { Kernel for 3x3 Laplace edge detection filter (2nd derivative approximation).}
  FilterLaplace3x3: TConvolutionFilter3x3 = (
    Kernel: ((-1, -1, -1),
             (-1,  8, -1),
             (-1, -1, -1));
    Divisor: 1);

  { Kernel for 5x5 Laplace edge detection filter (2nd derivative approximation).}
  FilterLaplace5x5: TConvolutionFilter5x5 = (
    Kernel: ((-1, -1, -1, -1, -1),
             (-1, -1, -1, -1, -1),
             (-1, -1, 24, -1, -1),
             (-1, -1, -1, -1, -1),
             (-1, -1, -1, -1, -1));
    Divisor: 1);

  { Kernel for 3x3 spharpening filter (Laplacian + original color).}
  FilterSharpen3x3: TConvolutionFilter3x3 = (
    Kernel: ((-1, -1, -1),
             (-1,  9, -1),
             (-1, -1, -1));
    Divisor: 1);

  { Kernel for 5x5 spharpening filter (Laplacian + original color).}
  FilterSharpen5x5: TConvolutionFilter5x5 = (
    Kernel: ((-1, -1, -1, -1, -1),
             (-1, -1, -1, -1, -1),
             (-1, -1, 25, -1, -1),
             (-1, -1, -1, -1, -1),
             (-1, -1, -1, -1, -1));
    Divisor: 1);

{ You can register your own canvas class. List of registered canvases is used
  by FindBestCanvasForImage functions to find best canvas for given image.
  If two different canvases which support the same image data format are
  registered then the one that was registered later is returned (so you can
  override builtin Imaging canvases).}
procedure RegisterCanvas(CanvasClass: TImagingCanvasClass);
{ Returns best canvas for given TImageFormat.}
function FindBestCanvasForImage(ImageFormat: TImageFormat): TImagingCanvasClass; overload;
{ Returns best canvas for given TImageData.}
function FindBestCanvasForImage(const ImageData: TImageData): TImagingCanvasClass; overload;
{ Returns best canvas for given TBaseImage.}
function FindBestCanvasForImage(Image: TBaseImage): TImagingCanvasClass; overload;

implementation

resourcestring
  SConstructorInvalidPointer = 'Invalid pointer (%p) to TImageData passed to TImagingCanvas constructor.';
  SConstructorInvalidImage = 'Invalid image data passed to TImagingCanvas constructor (%s).';
  SConstructorUnsupportedFormat = 'Image passed to TImagingCanvas constructor is in unsupported format (%s)';

var
  // list with all registered TImagingCanvas classes
  CanvasClasses: TList = nil;

procedure RegisterCanvas(CanvasClass: TImagingCanvasClass);
begin
  Assert(CanvasClass <> nil);
  if CanvasClasses = nil then
    CanvasClasses := TList.Create;
  if CanvasClasses.IndexOf(CanvasClass) < 0 then
    CanvasClasses.Add(CanvasClass);
end;

function FindBestCanvasForImage(ImageFormat: TImageFormat): TImagingCanvasClass; overload;
var
  I: LongInt;
begin
  for I := CanvasClasses.Count - 1 downto 0 do
  begin
    if ImageFormat in TImagingCanvasClass(CanvasClasses[I]).GetSupportedFormats then
    begin
      Result := TImagingCanvasClass(CanvasClasses[I]);
      Exit;
    end;
  end;
  Result := TImagingCanvas;
end;

function FindBestCanvasForImage(const ImageData: TImageData): TImagingCanvasClass;
begin
  Result := FindBestCanvasForImage(ImageData.Format);
end;

function FindBestCanvasForImage(Image: TBaseImage): TImagingCanvasClass;
begin
  Result := FindBestCanvasForImage(Image.Format);
end;

{ TImagingCanvas }

constructor TImagingCanvas.CreateForData(ImageDataPointer: PImageData);
begin
  if ImageDataPointer = nil then
    raise EImagingCanvasError.CreateFmt(SConstructorInvalidPointer, [ImageDataPointer]);

  if not TestImage(ImageDataPointer^) then
    raise EImagingCanvasError.CreateFmt(SConstructorInvalidImage, [Imaging.ImageToStr(ImageDataPointer^)]);

  if not (ImageDataPointer.Format in GetSupportedFormats) then
    raise EImagingCanvasError.CreateFmt(SConstructorUnsupportedFormat, [Imaging.ImageToStr(ImageDataPointer^)]);

  FPData := ImageDataPointer;
  UpdateCanvasState;
end;

constructor TImagingCanvas.CreateForImage(Image: TBaseImage);
begin
  CreateForData(Image.ImageDataPointer);
end;

destructor TImagingCanvas.Destroy;
begin
  inherited Destroy;
end;

function TImagingCanvas.GetPixel32(X, Y: LongInt): TColor32;
begin
  Result := Imaging.GetPixel32(FPData^, X, Y).Color;
end;

function TImagingCanvas.GetPixelFP(X, Y: LongInt): TColorFPRec;
begin
  Result := Imaging.GetPixelFP(FPData^, X, Y);
end;

function TImagingCanvas.GetFormatInfo: TImageFormatInfo;
begin
  Imaging.GetImageFormatInfo(FPData.Format, Result);
end;

function TImagingCanvas.GetValid: Boolean;
begin
  Result := (FPData <> nil) and (FDataSizeOnUpdate = FPData.Size);
end;

procedure TImagingCanvas.SetPenColor32(const Value: TColor32);
begin
  FPenColor32 := Value;
  TranslatePixel(@FPenColor32, @FPenColorFP, ifA8R8G8B8, ifA32R32G32B32F, nil, nil);
end;

procedure TImagingCanvas.SetPenColorFP(const Value: TColorFPRec);
begin
  FPenColorFP := Value;
  TranslatePixel(@FPenColorFP, @FPenColor32, ifA32R32G32B32F, ifA8R8G8B8, nil, nil);
end;

procedure TImagingCanvas.SetFillColor32(const Value: TColor32);
begin
  FFillColor32 := Value;
  TranslatePixel(@FFillColor32, @FFillColorFP, ifA8R8G8B8, ifA32R32G32B32F, nil, nil);
end;

procedure TImagingCanvas.SetFillColorFP(const Value: TColorFPRec);
begin
  FFillColorFP := Value;
  TranslatePixel(@FFillColorFP, @FFillColor32, ifA32R32G32B32F, ifA8R8G8B8, nil, nil);
end;

procedure TImagingCanvas.SetPixel32(X, Y: LongInt; const Value: TColor32);
begin
  if (X >= FClipRect.Left) and (Y >= FClipRect.Top) and
    (X < FClipRect.Right) and (Y < FClipRect.Bottom) then
  begin
    Imaging.SetPixel32(FPData^, X, Y, TColor32Rec(Value));
  end;
end;

procedure TImagingCanvas.SetPixelFP(X, Y: LongInt; const Value: TColorFPRec);
begin
  if (X >= FClipRect.Left) and (Y >= FClipRect.Top) and
    (X < FClipRect.Right) and (Y < FClipRect.Bottom) then
  begin
    Imaging.SetPixelFP(FPData^, X, Y, TColorFPRec(Value));
  end;
end;

procedure TImagingCanvas.SetClipRect(const Value: TRect);
begin
  FClipRect := Value;
  IntersectRect(FClipRect, FClipRect, Rect(0, 0, FPData.Width, FPData.Height));
end;

function TImagingCanvas.GetPixelPointer(X, Y: LongInt): Pointer;
begin
  Result := @PByteArray(FPData.Bits)[(Y * FPData.Width + X) * FFormatInfo.BytesPerPixel]
end;

procedure TImagingCanvas.TranslateFPToNative(const Color: TColorFPRec);
begin
  ImagingFormats.TranslatePixel(@Color, @FNativeColor, ifA32R32G32B32F,
    FPData.Format, nil, FPData.Palette);
end;

procedure TImagingCanvas.UpdateCanvasState;
begin
  FDataSizeOnUpdate := FPData.Size;
  ResetClipRect;
  Imaging.GetImageFormatInfo(FPData.Format, FFormatInfo)
end;

procedure TImagingCanvas.ResetClipRect;
begin
  FClipRect := Rect(0, 0, FPData.Width, FPData.Height)
end;

procedure TImagingCanvas.Clear;
begin
  TranslateFPToNative(FFillColorFP);
  Imaging.FillRect(FPData^, 0, 0, FPData.Width, FPData.Height, @FNativeColor);
end;

function TImagingCanvas.ClipAxisParallelLine(var A1, A2, B: LongInt;
  AStart, AStop, BStart, BStop: LongInt): Boolean;
begin
  if (B >= BStart) and (B < BStop) then
  begin
    SwapMin(A1, A2);
    if A1 < AStart then A1 := AStart;
    if A2 >= AStop then A2 := AStop - 1;
    Result := True;
  end
  else
    Result := False;
end;

procedure TImagingCanvas.HorzLine(X1, X2, Y: LongInt);
var
  I, Bpp: LongInt;
  PixelPtr: PByte;
begin
  if ClipAxisParallelLine(X1, X2, Y, FClipRect.Left, FClipRect.Right,
    FClipRect.Top, FClipRect.Bottom) then
  begin
    TranslateFPToNative(FPenColorFP);
    PixelPtr := GetPixelPointer(X1, Y);
    Bpp := FFormatInfo.BytesPerPixel;

    for I := X1 to X2 do
    begin
      ImagingFormats.CopyPixel(@FNativeColor, PixelPtr, Bpp);
      Inc(PixelPtr, Bpp);
    end;
  end;
end;

procedure TImagingCanvas.VertLine(X, Y1, Y2: LongInt);
var
  I, Bpp, WidthBytes: LongInt;
  PixelPtr: PByte;
begin
  if ClipAxisParallelLine(Y1, Y2, X, FClipRect.Top, FClipRect.Bottom,
    FClipRect.Left, FClipRect.Right) then
  begin
    TranslateFPToNative(FPenColorFP);
    PixelPtr := GetPixelPointer(X, Y1);
    Bpp := FFormatInfo.BytesPerPixel;
    WidthBytes := FPData.Width * Bpp;

    for I := Y1 to Y2 do
    begin
      ImagingFormats.CopyPixel(@FNativeColor, PixelPtr, Bpp);
      Inc(PixelPtr, WidthBytes);
    end;
  end;
end;

procedure TImagingCanvas.FillRect(const Rect: TRect);
var
  DstRect: TRect;
begin
  if IntersectRect(DstRect, Rect, FClipRect) then
  begin
    TranslateFPToNative(FFillColorFP);
    Imaging.FillRect(FPData^, DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left,
      DstRect.Bottom - DstRect.Top, @FNativeColor);
  end;
end;

procedure TImagingCanvas.ApplyConvolution(Kernel: PLongInt; KernelSize,
  Divisor: LongInt; Bias: Single; ClampChannels: Boolean);
var
  X, Y, I, J, PosY, PosX, SizeDiv2, KernelValue, WidthBytes, Bpp: LongInt;
  R, G, B, DivFloat: Single;
  Pixel: TColorFPRec;
  TempImage: TImageData;
  DstPointer, SrcPointer: PByte;
  Info: TImageFormatInfo;
begin
  SizeDiv2 := KernelSize div 2;
  DivFloat := IffFloat(Divisor > 1, 1.0 / Divisor, 1.0);
  Info := GetFormatInfo;
  Bpp := Info.BytesPerPixel;
  WidthBytes := FPData.Width * Bpp;

  InitImage(TempImage);
  CloneImage(FPData^, TempImage);

  try
    // For every pixel in clip rect
    for Y := FClipRect.Top to FClipRect.Bottom - 1 do
    begin
      DstPointer := @PByteArray(FPData.Bits)[Y * WidthBytes + FClipRect.Left * Bpp];

      for X := FClipRect.Left to FClipRect.Right - 1 do
      begin
        // Reset accumulators
        R := 0.0;
        G := 0.0;
        B := 0.0;

        for J := 0 to KernelSize - 1 do
        begin
          PosY := ClampInt(Y + J - SizeDiv2, FClipRect.Top, FClipRect.Bottom);

          for I := 0 to KernelSize - 1 do
          begin
            PosX := ClampInt(X + I - SizeDiv2, FClipRect.Left, FClipRect.Right);
            SrcPointer := @PByteArray(TempImage.Bits)[PosY * WidthBytes + PosX * Bpp];

            // Get pixels from neighbourhood of current pixel and add their
            // colors to accumulators weighted by filter kernel values
            Pixel := Info.GetPixelFP(SrcPointer, @Info, TempImage.Palette);
            KernelValue := PLongIntArray(Kernel)[J * KernelSize + I];

            R := R + Pixel.R * KernelValue;
            G := G + Pixel.G * KernelValue;
            B := B + Pixel.B * KernelValue;
          end;
        end;

        Pixel := Info.GetPixelFP(DstPointer, @Info, FPData.Palette);

        Pixel.R := R * DivFloat + Bias;
        Pixel.G := G * DivFloat + Bias;
        Pixel.B := B * DivFloat + Bias;

        if ClampChannels then
          ClampFloatPixel(Pixel);

        // Set resulting pixel color
        Info.SetPixelFP(DstPointer, @Info, FPData.Palette, Pixel);

        Inc(DstPointer, Info.BytesPerPixel);
      end;
    end;

  finally
    FreeImage(TempImage);
  end;
end;

procedure TImagingCanvas.ApplyConvolution3x3(const Filter: TConvolutionFilter3x3);
begin
  ApplyConvolution(@Filter.Kernel, 3, Filter.Divisor, 0, True);
end;

procedure TImagingCanvas.ApplyConvolution5x5(const Filter: TConvolutionFilter5x5);
begin
  ApplyConvolution(@Filter.Kernel, 5, Filter.Divisor, 0, True);
end;

class function TImagingCanvas.GetSupportedFormats: TImageFormats;
begin
  Result := [ifIndex8..Pred(ifDXT1)];
end;


{ TFastARGB32Canvas }

destructor TFastARGB32Canvas.Destroy;
begin
  FreeMem(FScanlines);
  inherited Destroy;
end;

function TFastARGB32Canvas.GetPixel32(X, Y: LongInt): TColor32;
begin
  Result := FScanlines[Y, X].Color;
end;

procedure TFastARGB32Canvas.SetPixel32(X, Y: LongInt; const Value: TColor32);
begin
  FScanlines[Y, X].Color := Value;
end;

procedure TFastARGB32Canvas.UpdateCanvasState;
var
  I: LongInt;
  ScanPos: PLongWord;
begin
  inherited UpdateCanvasState;

  // Realloc and update scanline array
  ReallocMem(FScanlines, FPData.Height * SizeOf(PColor32RecArray));
  ScanPos := FPData.Bits;

  for I := 0 to FPData.Height - 1 do
  begin
    FScanlines[I] := PColor32RecArray(ScanPos);
    Inc(ScanPos, FPData.Width);
  end;
end;

procedure TFastARGB32Canvas.HorzLine(X1, X2, Y: LongInt);
begin
  if ClipAxisParallelLine(X1, X2, Y, FClipRect.Left, FClipRect.Right,
    FClipRect.Top, FClipRect.Bottom) then
  begin
    FillMemoryLongWord(@FScanlines[Y, X1], (X2 - X1 + 1) * 4, FPenColor32);
  end;
end;

procedure TFastARGB32Canvas.VertLine(X, Y1, Y2: LongInt);
var
  I: LongInt;
begin
  if ClipAxisParallelLine(Y1, Y2, X, FClipRect.Top, FClipRect.Bottom,
    FClipRect.Left, FClipRect.Right) then
  begin
    for I := Y1 to Y2 do
      FScanlines[I, X].Color := FPenColor32;
  end;
end;

class function TFastARGB32Canvas.GetSupportedFormats: TImageFormats;
begin
  Result := [ifA8R8G8B8];
end;

initialization
  RegisterCanvas(TFastARGB32Canvas);

finalization
  FreeAndNil(CanvasClasses);

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - more more more ... 
    - add pen width
    - add blending (image and object drawing)
    - add image drawing
    - more objects (rect, framerect)

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - added TFastARGB32Canvas
    - added convolutions, hline, vline
    - unit created, intial stuff added

}

end.

