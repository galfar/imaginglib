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

{ Tests of image classes (TBaseImage and its descendants in ImagingClasses unit).}
unit TestImagingClasses;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, ImagingTypes, Imaging, ImagingClasses, ImagingUtility,
  ImagingTestCase;

type
  { Pointers to pixels and scanlines of images.}
  TTestImagePixelAccess = class(TImagingTestCase)
  published
    procedure TestScanlineAndPixelPointers;
    procedure TestSpecialFormatsAreRefused;
  end;

implementation

{ TTestImagePixelAccess }

procedure TTestImagePixelAccess.TestScanlineAndPixelPointers;
var
  Image: TSingleImage;
begin
  Image := TSingleImage.CreateFromParams(9, 6, ifR8G8B8);
  try
    CheckTrue(Image.Scanline[0] = Image.Bits, 'The first scanline');
    CheckTrue(Image.Scanline[4] = @PBuffer(Image.Bits)[4 * 9 * 3], 'Scanline 4');
    CheckTrue(Image.PixelPointer[0, 0] = Image.Bits, 'The first pixel');
    CheckTrue(Image.PixelPointer[7, 5] = @PBuffer(Image.Bits)[(5 * 9 + 7) * 3], 'Pixel [7, 5]');
    CheckTrue(Image.Scanline[5] = GetScanlinePointer(Image.ImageDataPointer^, 5), 'The same as GetScanlinePointer');
    CheckTrue(Image.PixelPointer[2, 3] = GetPixelPointer(Image.ImageDataPointer^, 2, 3), 'The same as GetPixelPointer');
  finally
    Image.Free;
  end;
end;

procedure TTestImagePixelAccess.TestSpecialFormatsAreRefused;
const
  Formats: array[0..2] of TImageFormat = (ifDXT1, ifBTC, ifBinary);
var
  Image: TSingleImage;
  F: Integer;
  P: Pointer;
  Raised: Boolean;
  Name: string;
begin
  // Images in special formats have no pixels or scanlines that can be pointed to
  for F := Low(Formats) to High(Formats) do
  begin
    Name := GetFormatName(Formats[F]);
    Image := TSingleImage.CreateFromParams(16, 8, Formats[F]);
    try
      CheckTrue(Image.Valid, Name + ': image is valid');

      Raised := False;
      try
        P := Image.Scanline[0];
        CheckTrue(P = nil, Name + ': no scanline');
      except
        on E: EImagingError do
        begin
          Raised := True;
          CheckTrue(Pos(Name, E.Message) > 0, Name + ': message of Scanline names the format: ' + E.Message);
        end;
      end;
      CheckTrue(Raised, Name + ': Scanline raises');

      Raised := False;
      try
        P := Image.PixelPointer[0, 0];
        CheckTrue(P = nil, Name + ': no pixel');
      except
        on E: EImagingError do
          Raised := True;
      end;
      CheckTrue(Raised, Name + ': PixelPointer raises');
    finally
      Image.Free;
    end;
  end;
end;

initialization
  RegisterImagingTest(TTestImagePixelAccess);
end.
