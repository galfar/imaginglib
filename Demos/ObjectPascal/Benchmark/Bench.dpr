program Bench;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  DemoUnit in 'DemoUnit.pas',
  ImagingTypes in '..\..\..\Source\ImagingTypes.pas',
  Imaging in '..\..\..\Source\Imaging.pas',
  ImagingFormats in '..\..\..\Source\ImagingFormats.pas';

begin
  RunDemo;
end.


