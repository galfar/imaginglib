{
  $Id$
  Vampyre Imaging Library
  by Marek Mauder
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

{ Unit functions for converting and copying images between Imaging and Graphics32 library.}
unit ImagingGraphics32;

{$I ImagingOptions.inc}

interface

uses
  Types, ImagingTypes, Imaging, ImagingFormats, ImagingUtility, ImagingClasses, GR32;


procedure ConvertImageDataToBitmap32(const Image: TImageData; Bitmap32: TCustomBitmap32);
procedure ConvertImageToBitmap32(Image: TBaseImage; Bitmap32: TCustomBitmap32);

procedure ConvertBitmap32ToImageData(Bitmap32: TCustomBitmap32; var Image: TImageData);
procedure ConvertBitmap32ToImage(Bitmap32: TCustomBitmap32; Image: TBaseImage);

procedure CopyImageDataToBitmap32(const Image: TImageData; Bitmap32: TCustomBitmap32);
procedure CopyImageToBitmap32(Image: TBaseImage; Bitmap32: TCustomBitmap32);

procedure CopyRectToBitmap32(const Image: TImageData; Bitmap32: TCustomBitmap32;
  SrcX, SrcY, Width, Height, DstX, DstY: Integer); overload;
procedure CopyRectToBitmap32(Image: TBaseImage; Bitmap32: TCustomBitmap32;
  SrcX, SrcY, Width, Height, DstX, DstY: Integer); overload;

procedure MapBitmap32ToImageData(Bitmap32: TCustomBitmap32; var Image: TImageData);

implementation

procedure ConvertImageDataToBitmap32(const Image: TImageData; Bitmap32: TCustomBitmap32);
begin
  Assert(TestImage(Image));
  Bitmap32.SetSize(Image.Width, Image.Height);
  CopyImageDataToBitmap32(Image, Bitmap32);
end;

procedure ConvertImageToBitmap32(Image: TBaseImage; Bitmap32: TCustomBitmap32);
begin
  ConvertImageDataToBitmap32(Image.ImageDataPointer^, Bitmap32);
end;

procedure ConvertBitmap32ToImageData(Bitmap32: TCustomBitmap32; var Image: TImageData);
begin
  Assert(not Bitmap32.Empty);
  NewImage(Bitmap32.Width, Bitmap32.Height, ifA8R8G8B8, Image);
  Move(Bitmap32.Bits^, Image.Bits^, Image.Size);
end;

procedure ConvertBitmap32ToImage(Bitmap32: TCustomBitmap32; Image: TBaseImage);
begin
  ConvertBitmap32ToImageData(Bitmap32, Image.ImageDataPointer^);
end;

procedure CopyImageDataToBitmap32(const Image: TImageData; Bitmap32: TCustomBitmap32);
begin
  Assert(TestImage(Image) and (Image.Width = Bitmap32.Width) and (Image.Height = Bitmap32.Height));
  CopyRectToBitmap32(Image, Bitmap32, 0, 0, Image.Width, Image.Height, 0, 0);
end;

procedure CopyImageToBitmap32(Image: TBaseImage; Bitmap32: TCustomBitmap32);
begin
  CopyImageDataToBitmap32(Image.ImageDataPointer^, Bitmap32);
end;

procedure CopyRectToBitmap32(const Image: TImageData; Bitmap32: TCustomBitmap32;
  SrcX, SrcY, Width, Height, DstX, DstY: Integer);
var
  TempImage: TImageData;
  X, Y, Bpp, SrcWidthBytes, DstWidth, MoveBytes: Integer;
  DstPtr: PColor32Rec;
  SrcPtr: PByte;
  Info: TImageFormatInfo;
begin
  Assert(TestImage(Image) and not Bitmap32.Empty);

  ClipCopyBounds(SrcX, SrcY, Width, Height, DstX, DstY, Image.Width, Image.Height,
    Rect(0, 0, Bitmap32.Width, Bitmap32.Height));

  if Image.Format in [ifIndex8, ifGray8, ifGray16, ifR8G8B8, ifA8R8G8B8,
    ifR16G16B16, ifA16R16G16B16] then
  begin
    GetImageFormatInfo(Image.Format, Info);
    Bpp := Info.BytesPerPixel;
    SrcWidthBytes := Image.Width * Bpp;
    DstWidth := Bitmap32.Width;
    MoveBytes := Width * Bpp;
    SrcPtr := @PByteArray(Image.Bits)[SrcY * SrcWidthBytes +  SrcX * Bpp];
    DstPtr := @PColor32RecArray(Bitmap32.Bits)[DstY * DstWidth +  DstX];

    for Y := 0 to Height - 1 do
    begin
      case Image.Format of
        ifIndex8:
          for X := 0 to Width - 1 do
          begin
            DstPtr^ := Image.Palette[SrcPtr^];
            Inc(DstPtr);
            Inc(SrcPtr, Bpp);
          end;
        ifGray8:
          for X := 0 to Width - 1 do
          begin
            DstPtr.R := SrcPtr^;
            DstPtr.G := SrcPtr^;
            DstPtr.B := SrcPtr^;
            DstPtr.A := 255;
            Inc(DstPtr);
            Inc(SrcPtr, Bpp);
          end;
        ifGray16:
          for X := 0 to Width - 1 do
          begin
            DstPtr.R := PWord(SrcPtr)^ shr 8;
            DstPtr.G := DstPtr.R;
            DstPtr.B := DstPtr.R;
            DstPtr.A := 255;
            Inc(DstPtr);
            Inc(SrcPtr, Bpp);
          end;
        ifR8G8B8:
          for X := 0 to Width - 1 do
          begin
            DstPtr.Color24Rec := PColor24Rec(SrcPtr)^;
            DstPtr.A := 255;
            Inc(DstPtr);
            Inc(SrcPtr, Bpp);
          end;
        ifA8R8G8B8:
          begin
            Move(SrcPtr^, DstPtr^, MoveBytes);
          end;
        ifR16G16B16:
          for X := 0 to Width - 1 do
          begin
            DstPtr.R := PColor48Rec(SrcPtr).R shr 8;
            DstPtr.G := PColor48Rec(SrcPtr).G shr 8;
            DstPtr.B := PColor48Rec(SrcPtr).B shr 8;
            DstPtr.A := 255;
            Inc(DstPtr);
            Inc(SrcPtr, Bpp);
          end;
        ifA16R16G16B16:
          for X := 0 to Width - 1 do
          begin
            DstPtr.R := PColor64Rec(SrcPtr).R shr 8;
            DstPtr.G := PColor64Rec(SrcPtr).G shr 8;
            DstPtr.B := PColor64Rec(SrcPtr).B shr 8;
            DstPtr.A := PColor64Rec(SrcPtr).A shr 8;
            Inc(DstPtr);
            Inc(SrcPtr, Bpp);
          end;
      end;

      Inc(SrcPtr, SrcWidthBytes - MoveBytes);
      Inc(DstPtr, DstWidth - Width);
    end;
  end
  else
  begin
    InitImage(TempImage);
    CloneImage(Image, TempImage);
    ConvertImage(TempImage, ifA8R8G8B8);
    try
      CopyRectToBitmap32(Image, Bitmap32, SrcX, SrcY, Width, Height, DstX, DstY);
      ConvertImageDataToBitmap32(TempImage, Bitmap32);
    finally
      FreeImage(TempImage);
    end;
  end;
end;

procedure CopyRectToBitmap32(Image: TBaseImage; Bitmap32: TCustomBitmap32;
  SrcX, SrcY, Width, Height, DstX, DstY: Integer);
begin
  CopyRectToBitmap32(Image.ImageDataPointer^, Bitmap32,
    SrcX, SrcY, Width, Height, DstX, DstY);
end;

procedure MapBitmap32ToImageData(Bitmap32: TCustomBitmap32; var Image: TImageData);
begin
  Assert(not Bitmap32.Empty);
  FreeImage(Image);

  Image.Width := Bitmap32.Width;
  Image.Height := Bitmap32.Height;
  Image.Format := ifA8R8G8B8;

  Image.Bits := Bitmap32.Bits;
end;

end.
