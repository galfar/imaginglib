program LCLImager;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms, vampyreimagingpackage
  { add your units here },
  MainUnit, AboutUnit;

{$R *.res}

begin
  Application.Title := 'LCL Imager';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.Run;
end.

