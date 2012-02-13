{
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

{ Functions and classes for interoperability between Imaging and
  FireMonkey framework.}
unit ImagingFmx;

{$I ImagingOptions.inc}

{$IF not Defined(DCC) or (CompilerVersion < 23)}
  {$MESSAGE FATAL 'FMX needs Delphi XE2+'}
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
  Fmx.Types;

{ Converts image from TImageData record to FMX bitmap. Bitmap must be already instantiated.}
procedure ConvertImageDataToFmxBitmap(const Image: TImageData; Bitmap: TBitmap);
{ Converts image from TBaseImage instance to FMX bitmap. Bitmap must be already instantiated.}
procedure ConvertImageToFmxBitmap(Image: TBaseImage; Bitmap: TBitmap);

{ Copies rectangular area of pixels from TImageData record to existing FMX bitmap.}
procedure CopyRectToFmxBitmap(const Image: TImageData; Bitmap: TBitmap;
  SrcX, SrcY, Width, Height, DstX, DstY: Integer); overload;
{ Copies rectangular area of pixels from TBaseImage instance to existing FMX bitmap.}
procedure CopyRectToFmxBitmap(Image: TBaseImage; Bitmap: TBitmap;
  SrcX, SrcY, Width, Height, DstX, DstY: Integer); overload;

implementation

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

procedure CopyRectToFmxBitmap(const Image: TImageData; Bitmap: TBitmap;
  SrcX, SrcY, Width, Height, DstX, DstY: Integer);
var
  TempImage: TImageData;
  X, Y, Bpp, SrcWidthBytes, MoveBytes: Integer;
  DstPtr: PColor32Rec;
  SrcPtr: PByte;
  Info: TImageFormatInfo;
begin
  Assert(TestImage(Image) and not Bitmap.IsEmpty);

  ClipCopyBounds(SrcX, SrcY, Width, Height, DstX, DstY, Image.Width, Image.Height,
    Rect(0, 0, Bitmap.Width, Bitmap.Height));
  GetImageFormatInfo(Image.Format, Info);

  if not Info.IsSpecial then
  begin
    Bpp := Info.BytesPerPixel;
    SrcWidthBytes := Image.Width * Bpp;
    MoveBytes := Width * Bpp;
    SrcPtr := @PByteArray(Image.Bits)[SrcY * SrcWidthBytes + SrcX * Bpp];

    for Y := 0 to Height - 1 do
    begin
      DstPtr := PColor32Rec(@Bitmap.ScanLine[Y][DstX]);

      if Info.Format = ifA8R8G8B8 then
      begin
        Move(SrcPtr^, DstPtr^, MoveBytes);
        Inc(SrcPtr, MoveBytes);
      end
      else
      begin
        for X := 0 to Width - 1 do
        begin
          ConvertToPixel32(SrcPtr, DstPtr, Info, Image.Palette);
          Inc(DstPtr);
          Inc(SrcPtr, Bpp);
        end;
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

  Bitmap.UpdateHandles;
  Bitmap.BitmapChanged;
end;

procedure CopyRectToFmxBitmap(Image: TBaseImage; Bitmap: TBitmap;
  SrcX, SrcY, Width, Height, DstX, DstY: Integer);
begin
  CopyRectToFmxBitmap(Image.ImageDataPointer^, Bitmap,
    SrcX, SrcY, Width, Height, DstX, DstY);
end;

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.77 Changes/Bug Fixes -----------------------------------
    - Unit created with initial stuff.
 }

end.
