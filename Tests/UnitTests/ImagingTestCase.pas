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

{ Base test case class and helpers shared by all unit tests.

  Tests are built on FPCUnit in Free Pascal and on DUnit in Delphi. FPCUnit has
  DUnit compatible checks built in (Check, CheckTrue, CheckEquals, Fail, ...),
  so test methods use only these and this unit is the only place where
  the two frameworks differ.}
unit ImagingTestCase;

{$I ImagingOptions.inc}

interface

uses
  Types, SysUtils, Classes,
{$IFDEF FPC}
  fpcunit, testregistry,
{$ELSE}
  TestFramework,
{$ENDIF}
  ImagingTypes, Imaging, ImagingUtility;

type
  TImagingTestCase = class(TTestCase)
  protected
    { Fails when the two memory blocks of Size bytes differ.}
    procedure CheckSameMem(Expected, Actual: Pointer; Size: NativeInt; const Msg: string);
    procedure CheckEqualsRect(const Expected, Actual: TRect; const Msg: string);
    procedure CheckEqualsColor32(Expected, Actual: TColor32; const Msg: string);
    { Fails when the images differ in dimensions, data format, bits, or palette.}
    procedure CheckImagesEqual(const Expected, Actual: TImageData; const Msg: string);
  end;

  TImagingTestCaseClass = class of TImagingTestCase;

{ Registers all published methods of the class as tests.}
procedure RegisterImagingTest(TestClass: TImagingTestCaseClass);

{ Returns directory with test images (Demos/Data of the repository) with
  trailing path delimiter. It is looked for in parent directories
  of the test executable so tests can be run from any output directory.}
function GetTestDataDir: string;
{ Returns full path to file in test images directory.}
function GetTestDataFile(const FileName: string): string;
{ Returns directory for files written by tests with trailing path delimiter
  (TestOut next to the test executable). It is created when it does not exist.}
function GetTestOutDir: string;

implementation

procedure RegisterImagingTest(TestClass: TImagingTestCaseClass);
begin
{$IFDEF FPC}
  testregistry.RegisterTest(TestClass);
{$ELSE}
  TestFramework.RegisterTest(TestClass.Suite);
{$ENDIF}
end;

function GetTestDataDir: string;
const
  MaxLevelsUp = 6;
var
  Dir, Parent: string;
  I: Integer;
begin
  Dir := GetAppDir;

  for I := 0 to MaxLevelsUp do
  begin
    Result := Dir + PathDelim + 'Demos' + PathDelim + 'Data' + PathDelim;
    if DirectoryExists(Result) then
      Exit;

    Parent := ExtractFileDir(Dir);
    if Parent = Dir then
      Break;
    Dir := Parent;
  end;

  raise Exception.Create('Test data directory (Demos/Data) not found above ' + GetAppDir);
end;

function GetTestDataFile(const FileName: string): string;
begin
  Result := GetTestDataDir + FileName;
end;

function GetTestOutDir: string;
begin
  Result := GetAppDir + PathDelim + 'TestOut' + PathDelim;
  if not ForceDirectories(Result) then
    raise Exception.Create('Cannot create test output directory ' + Result);
end;

{ TImagingTestCase }

procedure TImagingTestCase.CheckSameMem(Expected, Actual: Pointer; Size: NativeInt;
  const Msg: string);
var
  I: NativeInt;
begin
  for I := 0 to Size - 1 do
  begin
    if PBuffer(Expected)[I] <> PBuffer(Actual)[I] then
    begin
      Fail(Format('%s: memory differs at offset %d of %d, expected: <$%.2x> but was: <$%.2x>',
        [Msg, Int64(I), Int64(Size), PBuffer(Expected)[I], PBuffer(Actual)[I]]));
    end;
  end;
end;

procedure TImagingTestCase.CheckEqualsRect(const Expected, Actual: TRect; const Msg: string);
begin
  if (Expected.Left <> Actual.Left) or (Expected.Top <> Actual.Top) or
    (Expected.Right <> Actual.Right) or (Expected.Bottom <> Actual.Bottom) then
  begin
    Fail(Format('%s: expected: <%d,%d,%d,%d> but was: <%d,%d,%d,%d>',
      [Msg, Expected.Left, Expected.Top, Expected.Right, Expected.Bottom,
      Actual.Left, Actual.Top, Actual.Right, Actual.Bottom]));
  end;
end;

procedure TImagingTestCase.CheckEqualsColor32(Expected, Actual: TColor32; const Msg: string);
begin
  if Expected <> Actual then
    Fail(Format('%s: expected: <$%.8x> but was: <$%.8x>', [Msg, Expected, Actual]));
end;

procedure TImagingTestCase.CheckImagesEqual(const Expected, Actual: TImageData;
  const Msg: string);
var
  Info: TImageFormatInfo;
begin
  CheckEquals(Expected.Width, Actual.Width, Msg + ': width');
  CheckEquals(Expected.Height, Actual.Height, Msg + ': height');
  CheckEquals(Ord(Expected.Format), Ord(Actual.Format), Msg + ': data format');
  CheckEquals(Expected.Size, Actual.Size, Msg + ': size');
  CheckSameMem(Expected.Bits, Actual.Bits, Expected.Size, Msg + ': bits');

  CheckTrue(GetImageFormatInfo(Expected.Format, Info), Msg + ': format info');
  if Info.PaletteEntries > 0 then
  begin
    CheckSameMem(Expected.Palette, Actual.Palette,
                 Info.PaletteEntries * SizeOf(TColor32Rec), Msg + ': palette');
  end;
end;

end.
