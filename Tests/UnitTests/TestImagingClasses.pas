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
  ImagingTestCase, ImagingTestPixels;

type
  { Pointers to pixels and scanlines of images.}
  TTestImagePixelAccess = class(TImagingTestCase)
  published
    procedure TestScanlineAndPixelPointers;
    procedure TestSpecialFormatsAreRefused;
  end;

  { Conversion of an image into another one (TBaseImage.ConvertInto).}
  TTestImageConvertInto = class(TImagingTestCase)
  private
    FSizeChanges: Integer;
    FLastSender: TObject;
    procedure DataSizeChanged(Sender: TObject);
    { Image of the given size and format filled with test content.}
    function MakeImage(AWidth, AHeight: Integer; AFormat: TImageFormat; Seed: UInt32): TSingleImage;
    { Converted copy of Image made by the procedural API, what ConvertInto must give.}
    procedure MakeExpected(const Image: TImageData; DstFormat: TImageFormat; var Expected: TImageData);
  published
    procedure TestConvertsIntoOtherImage;
    procedure TestSameFormatGivesCopy;
    procedure TestInPlace;
    procedure TestMultiImages;
    procedure TestNothingToConvert;
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

{ TTestImageConvertInto }

procedure TTestImageConvertInto.DataSizeChanged(Sender: TObject);
begin
  Inc(FSizeChanges);
  FLastSender := Sender;
end;

function TTestImageConvertInto.MakeImage(AWidth, AHeight: Integer;
  AFormat: TImageFormat; Seed: UInt32): TSingleImage;
begin
  Result := TSingleImage.CreateFromParams(AWidth, AHeight, AFormat);
  FillTestImage(Result.ImageDataPointer^, Seed);
end;

procedure TTestImageConvertInto.MakeExpected(const Image: TImageData;
  DstFormat: TImageFormat; var Expected: TImageData);
begin
  CheckTrue(CloneImage(Image, Expected), 'Clone for the expected result');
  CheckTrue(ConvertImage(Expected, DstFormat), 'Conversion for the expected result');
end;

procedure TTestImageConvertInto.TestConvertsIntoOtherImage;
const
  DstFormats: array[0..3] of TImageFormat = (ifGray8, ifA8R8G8B8, ifR16G16B16, ifA32R32G32B32F);
var
  Src, Dst: TSingleImage;
  SrcCopy, Expected: TImageData;
  F: Integer;
  Name: string;
begin
  InitImage(SrcCopy);
  InitImage(Expected);
  Src := MakeImage(13, 7, ifR8G8B8, 11);
  // Destination holds an image of other size and format that gets replaced
  Dst := MakeImage(5, 9, ifGray16, 12);
  try
    CloneImage(Src.ImageDataPointer^, SrcCopy);
    Dst.OnDataSizeChanged := DataSizeChanged;

    for F := Low(DstFormats) to High(DstFormats) do
    begin
      Name := GetFormatName(DstFormats[F]);
      MakeExpected(Src.ImageDataPointer^, DstFormats[F], Expected);
      FSizeChanges := 0;
      FLastSender := nil;

      Src.ConvertInto(Dst, DstFormats[F]);

      CheckImagesEqual(Expected, Dst.ImageDataPointer^, Name + ': converted image');
      CheckImagesEqual(SrcCopy, Src.ImageDataPointer^, Name + ': source is not changed');
      CheckEquals(1, FSizeChanges, Name + ': OnDataSizeChanged of the destination');
      CheckTrue(FLastSender = Dst, Name + ': sender of OnDataSizeChanged');
    end;
  finally
    FreeImage(SrcCopy);
    FreeImage(Expected);
    Src.Free;
    Dst.Free;
  end;
end;

procedure TTestImageConvertInto.TestSameFormatGivesCopy;
var
  Src, Dst: TSingleImage;
begin
  Src := MakeImage(10, 4, ifA8R8G8B8, 21);
  Dst := TSingleImage.Create;
  try
    Src.ConvertInto(Dst, ifA8R8G8B8);
    CheckImagesEqual(Src.ImageDataPointer^, Dst.ImageDataPointer^, 'Copy of the image');
    CheckTrue(Dst.Bits <> Src.Bits, 'Destination has its own pixels');
  finally
    Src.Free;
    Dst.Free;
  end;
end;

procedure TTestImageConvertInto.TestInPlace;
var
  Image: TSingleImage;
  Expected: TImageData;
begin
  InitImage(Expected);
  Image := MakeImage(8, 6, ifR8G8B8, 31);
  try
    MakeExpected(Image.ImageDataPointer^, ifGray16, Expected);
    Image.OnDataSizeChanged := DataSizeChanged;
    FSizeChanges := 0;

    Image.ConvertInto(Image, ifGray16);

    CheckImagesEqual(Expected, Image.ImageDataPointer^, 'Converted in place');
    CheckEquals(1, FSizeChanges, 'OnDataSizeChanged');
  finally
    FreeImage(Expected);
    Image.Free;
  end;
end;

procedure TTestImageConvertInto.TestMultiImages;
var
  Multi, Other: TMultiImage;
  Single: TSingleImage;
  Expected, Untouched: TImageData;
begin
  InitImage(Expected);
  InitImage(Untouched);
  Multi := TMultiImage.CreateFromParams(6, 5, ifR8G8B8, 3);
  Other := TMultiImage.CreateFromParams(4, 4, ifGray8, 2);
  Single := TSingleImage.Create;
  try
    FillTestImage(Multi.DataArray[0], 41);
    FillTestImage(Multi.DataArray[1], 42);
    FillTestImage(Multi.DataArray[2], 43);
    FillTestImage(Other.DataArray[0], 44);
    FillTestImage(Other.DataArray[1], 45);

    // Current image of the source is converted
    Multi.ActiveImage := 1;
    MakeExpected(Multi.DataArray[1], ifA8R8G8B8, Expected);
    Multi.ConvertInto(Single, ifA8R8G8B8);
    CheckImagesEqual(Expected, Single.ImageDataPointer^, 'Current image of multi image into single image');

    // It replaces only the current image of the destination
    CloneImage(Other.DataArray[0], Untouched);
    Other.ActiveImage := 1;
    Multi.ConvertInto(Other, ifA8R8G8B8);
    CheckEquals(2, Other.ImageCount, 'Number of images of the destination');
    CheckImagesEqual(Expected, Other.DataArray[1], 'Current image of the destination is replaced');
    CheckImagesEqual(Untouched, Other.DataArray[0], 'Other image of the destination is kept');

    // Another image of the same multi image
    MakeExpected(Multi.DataArray[2], ifGray8, Expected);
    Multi.ActiveImage := 2;
    Multi.ConvertInto(Multi, ifGray8);
    CheckImagesEqual(Expected, Multi.DataArray[2], 'Multi image converted in place');
    CheckEquals(3, Multi.ImageCount, 'Number of images of the source');
  finally
    FreeImage(Expected);
    FreeImage(Untouched);
    Multi.Free;
    Other.Free;
    Single.Free;
  end;
end;

procedure TTestImageConvertInto.TestNothingToConvert;
var
  Src, Dst: TSingleImage;
  Empty: TMultiImage;
  DstCopy: TImageData;
begin
  InitImage(DstCopy);
  Src := TSingleImage.Create;
  Dst := MakeImage(7, 3, ifGray8, 51);
  Empty := TMultiImage.Create;
  try
    CloneImage(Dst.ImageDataPointer^, DstCopy);
    Dst.OnDataSizeChanged := DataSizeChanged;
    FSizeChanges := 0;

    // Source without an image: destination stays as it is
    CheckFalse(Src.Valid, 'Source is empty');
    Src.ConvertInto(Dst, ifR8G8B8);
    CheckImagesEqual(DstCopy, Dst.ImageDataPointer^, 'Destination is not changed');
    CheckEquals(0, FSizeChanges, 'No OnDataSizeChanged');

    // Destination with no images to replace, and no destination at all
    Dst.ConvertInto(Empty, ifR8G8B8);
    CheckEquals(0, Empty.ImageCount, 'Empty multi image gets no image');
    Dst.ConvertInto(nil, ifR8G8B8);
    CheckImagesEqual(DstCopy, Dst.ImageDataPointer^, 'Source is not changed');
  finally
    FreeImage(DstCopy);
    Src.Free;
    Dst.Free;
    Empty.Free;
  end;
end;

initialization
  RegisterImagingTest(TTestImagePixelAccess);
  RegisterImagingTest(TTestImageConvertInto);
end.
