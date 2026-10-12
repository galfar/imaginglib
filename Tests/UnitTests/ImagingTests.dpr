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

{ Unit tests of Imaging library.

  Console program that runs all registered tests and returns non-zero exit
  code when any of them fails. Uses FPCUnit when compiled with Free Pascal
  and DUnit when compiled with Delphi (see ImagingTestCase unit).

  Free Pascal build takes options of FPCUnit console runner, for instance:
    ImagingTests --list
    ImagingTests --suite=TTestPixelsSize
    ImagingTests --format=xml --file=results.xml
    ImagingTests --pause    waits for Enter key after the tests finish, so the
                            console window started by IDE does not close
                            (in Lazarus put it to Run - Run Parameters)

  Delphi build takes these optional switches:
    --pause  waits for Enter key after the tests finish
    --gui    shows DUnit GUI runner instead of running the tests in console
             (Windows only), tests can be selected and run repeatedly there
    --leaks  turns on reporting of memory leaks at exit (the report is shown
             in a message box)}
program ImagingTests;

{$I ImagingOptions.inc}

{$IFDEF MSWINDOWS}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$IF Defined(DCC) and Defined(MSWINDOWS)}
  {$DEFINE HAS_GUI_RUNNER}
{$IFEND}

uses
{$IFDEF FPC}
  Classes,
  consoletestrunner,
{$ELSE}
  SysUtils,
  TestFramework,
  TextTestRunner,
  {$IFDEF HAS_GUI_RUNNER}
  GUITestRunner,
  {$ENDIF}
{$ENDIF}
  ImagingTestCase in 'ImagingTestCase.pas',
  ImagingTestPixels in 'ImagingTestPixels.pas',
  TestImagingUtility in 'TestImagingUtility.pas',
  TestImagingFormats in 'TestImagingFormats.pas',
  TestImagingBasics in 'TestImagingBasics.pas',
  TestImagingColors in 'TestImagingColors.pas',
  TestImagingConversions in 'TestImagingConversions.pas',
  TestImagingFileFormats in 'TestImagingFileFormats.pas';

{$IFDEF FPC}
type
  { Console runner of FPCUnit that knows also --pause option.}
  TImagingTestRunner = class(TTestRunner)
  protected
    procedure AppendLongOpts; override;
  end;

procedure TImagingTestRunner.AppendLongOpts;
begin
  inherited AppendLongOpts;
  LongOpts.Add('pause');
end;
{$ENDIF}

var
  PauseAtExit: Boolean;
{$IFDEF FPC}
  Runner: TImagingTestRunner;
{$ELSE}
  TestResult: TTestResult;
  Success: Boolean;
{$ENDIF}

{$IFNDEF FPC}
{ Returns True when the program was started with --Name option.}
function HasOption(const Name: string): Boolean;
begin
  // Second dash is taken as a part of the switch name
  Result := FindCmdLineSwitch('-' + Name, ['-'], True);
end;
{$ENDIF}

procedure WaitForEnter;
begin
  WriteLn;
  WriteLn('Press Enter to exit');
  ReadLn;
end;

begin
{$IFDEF FPC}
  // Run all tests and print readable results when there are no parameters
  DefaultRunAllTests := True;
  DefaultFormat := fPlain;

  Runner := TImagingTestRunner.Create(nil);
  try
    Runner.Initialize;
    Runner.Title := 'Imaging Unit Tests';
    Runner.Run;
    PauseAtExit := Runner.HasOption('pause');
  finally
    Runner.Free;
  end;

  // Exit code was set by the runner
  if PauseAtExit then
    WaitForEnter;
{$ELSE}
  {$IF CompilerVersion >= 18.0}
  ReportMemoryLeaksOnShutdown := HasOption('leaks');
  {$IFEND}

  {$IFDEF HAS_GUI_RUNNER}
  if HasOption('gui') then
  begin
    GUITestRunner.RunRegisteredTests;
    Exit;
  end;
  {$ENDIF}

  PauseAtExit := HasOption('pause');

  TestResult := TextTestRunner.RunRegisteredTests(rxbContinue);
  try
    Success := TestResult.WasSuccessful;
  finally
    TestResult.Free;
  end;

  if PauseAtExit then
    WaitForEnter;
  if not Success then
    Halt(1);
{$ENDIF}
end.
