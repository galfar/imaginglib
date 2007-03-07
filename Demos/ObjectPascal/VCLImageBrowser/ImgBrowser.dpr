program ImgBrowser;

uses
  Forms,
  Main,
  ImagingGif in '..\..\..\Source\ImagingGif.pas',
  ImagingPsd in '..\..\..\Extras\Extensions\ImagingPsd.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'VCL Image Browser';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
