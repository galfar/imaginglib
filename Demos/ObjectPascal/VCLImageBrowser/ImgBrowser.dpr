program ImgBrowser;

uses
  Forms,
  Main in 'Main.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'VCL Image Browser';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
