program ImgBrowser;

uses
  Forms,
{$IFDEF LCL}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
{$ENDIF}
  Main;

{$R *.res}

begin
{$IFDEF LCL}
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
{$ENDIF}
  Application.Title:='Image Browser';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
