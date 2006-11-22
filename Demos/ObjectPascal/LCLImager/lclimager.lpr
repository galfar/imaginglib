program LCLImager;

{$IFDEF MSWINDOWS}
  {$APPTYPE GUI}
  {$R *.res}
{$ENDIF}

{$R *.res}

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, MainUnit, AboutUnit;

begin
  Application.Title := 'LCL Imager';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.Run;
end.

