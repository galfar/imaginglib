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

{ Tests of color space conversions in ImagingColors unit.}
unit TestImagingColors;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, ImagingTypes, ImagingColors, ImagingTestCase;

type
  TColorSpace = (csCMY, csCMYK, csYCbCr, csYUV, csYCoCg);

  TTestColorSpaces = class(TImagingTestCase)
  private
    { Converts all 8-bit RGB colors to given color space and back, and checks
      that no channel differs by more than MaxDiff.}
    procedure CheckRoundTrip(Space: TColorSpace; MaxDiff: Integer; const Name: string);
  published
    procedure TestKnownColors;
    procedure TestRoundTripCMY;
    procedure TestRoundTripCMYK;
    procedure TestRoundTripYCbCr;
    procedure TestRoundTripYUV;
    procedure TestRoundTripYCoCg;
  end;

implementation

{ TTestColorSpaces }

procedure TTestColorSpaces.CheckRoundTrip(Space: TColorSpace; MaxDiff: Integer;
  const Name: string);
var
  R, G, B, Diff, WorstDiff: Integer;
  WorstR, WorstG, WorstB: Integer;
  C1, C2, C3, C4, OutR, OutG, OutB: Byte;
begin
  WorstDiff := 0;
  WorstR := 0;
  WorstG := 0;
  WorstB := 0;
  C4 := 0;

  for R := 0 to 255 do
  begin
    for G := 0 to 255 do
    begin
      for B := 0 to 255 do
      begin
        case Space of
          csCMY:
            begin
              RGBToCMY(R, G, B, C1, C2, C3);
              CMYToRGB(C1, C2, C3, OutR, OutG, OutB);
            end;
          csCMYK:
            begin
              RGBToCMYK(R, G, B, C1, C2, C3, C4);
              CMYKToRGB(C1, C2, C3, C4, OutR, OutG, OutB);
            end;
          csYCbCr:
            begin
              RGBToYCbCr(R, G, B, C1, C2, C3);
              YCbCrToRGB(C1, C2, C3, OutR, OutG, OutB);
            end;
          csYUV:
            begin
              RGBToYUV(R, G, B, C1, C2, C3);
              YUVToRGB(C1, C2, C3, OutR, OutG, OutB);
            end;
          csYCoCg:
            begin
              RGBToYCoCg(R, G, B, C1, C2, C3);
              YCoCgToRGB(C1, C2, C3, OutR, OutG, OutB);
            end;
        end;

        Diff := Abs(R - OutR);
        if Abs(G - OutG) > Diff then
          Diff := Abs(G - OutG);
        if Abs(B - OutB) > Diff then
          Diff := Abs(B - OutB);

        if Diff > WorstDiff then
        begin
          WorstDiff := Diff;
          WorstR := R;
          WorstG := G;
          WorstB := B;
        end;
      end;
    end;
  end;

  CheckTrue(WorstDiff <= MaxDiff,
    Format('%s round trip: channel differs by %d (allowed %d) for RGB (%d, %d, %d)',
    [Name, WorstDiff, MaxDiff, WorstR, WorstG, WorstB]));
end;

procedure TTestColorSpaces.TestKnownColors;
var
  C, M, Y, K, Cb, Cr, R, G, B: Byte;
begin
  RGBToCMY(255, 0, 64, C, M, Y);
  CheckEquals(0, C, 'CMY cyan');
  CheckEquals(255, M, 'CMY magenta');
  CheckEquals(191, Y, 'CMY yellow');

  RGBToCMYK(0, 0, 0, C, M, Y, K);
  CheckEquals(0, C, 'CMYK black: cyan');
  CheckEquals(0, M, 'CMYK black: magenta');
  CheckEquals(0, Y, 'CMYK black: yellow');
  CheckEquals(255, K, 'CMYK black: key');

  RGBToCMYK(255, 255, 255, C, M, Y, K);
  CheckEquals(0, C + M + Y + K, 'CMYK white has no ink');

  RGBToCMYK(255, 0, 0, C, M, Y, K);
  CheckEquals(0, C, 'CMYK red: cyan');
  CheckEquals(255, M, 'CMYK red: magenta');
  CheckEquals(255, Y, 'CMYK red: yellow');
  CheckEquals(0, K, 'CMYK red: key');

  // Full range YCbCr as used by JPEG: grays have neutral chroma
  RGBToYCbCr(0, 0, 0, Y, Cb, Cr);
  CheckEquals(0, Y, 'YCbCr black: luma');
  CheckEquals(128, Cb, 'YCbCr black: Cb');
  CheckEquals(128, Cr, 'YCbCr black: Cr');

  RGBToYCbCr(255, 255, 255, Y, Cb, Cr);
  CheckEquals(255, Y, 'YCbCr white: luma');
  CheckEquals(128, Cb, 'YCbCr white: Cb');
  CheckEquals(128, Cr, 'YCbCr white: Cr');

  YCbCrToRGB(128, 128, 128, R, G, B);
  CheckEquals(128, R, 'YCbCr gray: red');
  CheckEquals(128, G, 'YCbCr gray: green');
  CheckEquals(128, B, 'YCbCr gray: blue');

  // YUV with limited range luma (16..235)
  RGBToYUV(0, 0, 0, Y, Cb, Cr);
  CheckEquals(16, Y, 'YUV black: luma');
  CheckEquals(128, Cb, 'YUV black: U');
  CheckEquals(128, Cr, 'YUV black: V');

  RGBToYUV(255, 255, 255, Y, Cb, Cr);
  CheckEquals(235, Y, 'YUV white: luma');

  RGBToYCoCg(128, 128, 128, Y, Cb, Cr);
  CheckEquals(128, Y, 'YCoCg gray: luma');
  CheckEquals(128, Cb, 'YCoCg gray: Co');
  CheckEquals(128, Cr, 'YCoCg gray: Cg');
end;

procedure TTestColorSpaces.TestRoundTripCMY;
begin
  CheckRoundTrip(csCMY, 0, 'CMY');
end;

procedure TTestColorSpaces.TestRoundTripCMYK;
begin
  CheckRoundTrip(csCMYK, 1, 'CMYK');
end;

procedure TTestColorSpaces.TestRoundTripYCbCr;
begin
  CheckRoundTrip(csYCbCr, 1, 'YCbCr');
end;

procedure TTestColorSpaces.TestRoundTripYUV;
begin
  CheckRoundTrip(csYUV, 2, 'YUV');
end;

procedure TTestColorSpaces.TestRoundTripYCoCg;
begin
  CheckRoundTrip(csYCoCg, 2, 'YCoCg');
end;

initialization
  RegisterImagingTest(TTestColorSpaces);
end.
