program LCLImager;

{$IFDEF MSWINDOWS}
  {$APPTYPE GUI}
  {$IFDEF WIN32}
    {$R *.res}
  {$ENDIF}
{$ENDIF}

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

