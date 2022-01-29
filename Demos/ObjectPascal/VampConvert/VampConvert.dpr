program VampConvert;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$APPTYPE CONSOLE}
{$ENDIF}  

uses
  DemoUnit in 'DemoUnit.pas',
  Imaging in '..\..\..\Source\Imaging.pas';

begin
  RunDemo;
end.

