program D3DDemo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  DemoUnit in 'DemoUnit.pas',
  Imaging in '..\..\..\Source\Imaging.pas',
  ImagingDirect3D9 in '..\..\..\Extensions\ImagingDirect3D9.pas';

begin
  RunDemo;
end.


