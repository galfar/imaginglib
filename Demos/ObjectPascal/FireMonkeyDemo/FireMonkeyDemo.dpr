program FireMonkeyDemo;

uses
  FMX.Forms,
  FMX.Types,
  DemoUtils in '..\Common\DemoUtils.pas',
  MainForm in 'MainForm.pas' {FormMain},
  AboutForm in 'AboutForm.pas' {FormAbout},
  ImagingFmx in '..\..\..\Extensions\ImagingFmx.pas';

{$R *.res}

begin
  //FMX.Types.GlobalUseDirect2DSoftware := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.Run;
end.
