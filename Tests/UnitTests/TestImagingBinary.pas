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

{ Tests of functions for binary images (ImagingBinary unit).}
unit TestImagingBinary;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, ImagingTypes, Imaging, ImagingUtility, ImagingBinary,
  ImagingTestCase;

type
  TTestOtsuThresholding = class(TImagingTestCase)
  private
    FImage: TImageData;
    { Creates Gray8 image where the first DarkCount pixels have Dark value
      and the rest Light value.}
    procedure NewTwoLevelImage(Width, Height: Integer; DarkCount: NativeInt; Dark, Light: Byte);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTwoLevels;
    procedure TestThreeLevels;
    procedure TestImageIsKeptWithoutBinarization;
    procedure TestLargeFlatAreas;
  end;

implementation

{ TTestOtsuThresholding }

procedure TTestOtsuThresholding.SetUp;
begin
  InitImage(FImage);
end;

procedure TTestOtsuThresholding.TearDown;
begin
  FreeImage(FImage);
end;

procedure TTestOtsuThresholding.NewTwoLevelImage(Width, Height: Integer;
  DarkCount: NativeInt; Dark, Light: Byte);
begin
  CheckTrue(NewImage(Width, Height, ifGray8, FImage), 'NewImage');
  FillChar(FImage.Bits^, FImage.Size, Light);
  FillChar(FImage.Bits^, DarkCount, Dark);
end;

procedure TTestOtsuThresholding.TestTwoLevels;
var
  Level: Integer;
begin
  NewTwoLevelImage(20, 10, 80, 50, 200);
  Level := OtsuThresholding(FImage, True);

  CheckTrue((Level > 50) and (Level <= 200), Format('Level %d is between the two values', [Level]));
  CheckEquals(0, PBuffer(FImage.Bits)[0], 'The first dark pixel');
  CheckEquals(0, PBuffer(FImage.Bits)[79], 'The last dark pixel');
  CheckEquals(255, PBuffer(FImage.Bits)[80], 'The first light pixel');
  CheckEquals(255, PBuffer(FImage.Bits)[199], 'The last light pixel');
end;

procedure TTestOtsuThresholding.TestThreeLevels;
var
  Level, I: Integer;
begin
  // Two dark values close to each other and a light one
  NewTwoLevelImage(30, 10, 100, 20, 230);
  FillChar(PBuffer(FImage.Bits)[100], 50, 40);
  Level := OtsuThresholding(FImage, True);

  CheckTrue((Level > 40) and (Level <= 230), Format('Level %d splits dark and light values', [Level]));

  for I := 0 to 149 do
    CheckEquals(0, PBuffer(FImage.Bits)[I], Format('Dark pixel %d', [I]));
  for I := 150 to 299 do
    CheckEquals(255, PBuffer(FImage.Bits)[I], Format('Light pixel %d', [I]));
end;

procedure TTestOtsuThresholding.TestImageIsKeptWithoutBinarization;
var
  Original: TImageData;
  Level: Integer;
begin
  NewTwoLevelImage(16, 16, 100, 10, 100);
  InitImage(Original);
  try
    CheckTrue(CloneImage(FImage, Original), 'CloneImage');
    Level := OtsuThresholding(FImage);

    CheckTrue((Level > 10) and (Level <= 100), Format('Level %d is between the two values', [Level]));
    CheckImagesEqual(Original, FImage, 'Image');
  finally
    FreeImage(Original);
  end;
end;

procedure TTestOtsuThresholding.TestLargeFlatAreas;
const
  Side = 6000;
  DarkCount = 2000000;
var
  Level: Integer;
begin
  // More pixels of one value than single precision numbers can count (2^24)
  NewTwoLevelImage(Side, Side, DarkCount, 50, 200);
  Level := OtsuThresholding(FImage, True);

  CheckTrue((Level > 50) and (Level <= 200), Format('Level %d is between the two values', [Level]));
  CheckEquals(0, PBuffer(FImage.Bits)[DarkCount - 1], 'The last dark pixel');
  CheckEquals(255, PBuffer(FImage.Bits)[DarkCount], 'The first light pixel');
  CheckEquals(255, PBuffer(FImage.Bits)[Side * Side - 1], 'The last light pixel');
end;

initialization
  RegisterImagingTest(TTestOtsuThresholding);
end.
