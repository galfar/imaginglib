program ImgBrowser;

uses
  Forms,
  {$IFDEF LCL}
  Interfaces,
  {$ENDIF}
  Main in 'Main.pas',
  Imaging in '..\..\..\Source\Imaging.pas',
  ImagingCanvases in '..\..\..\Source\ImagingCanvases.pas',
  ImagingClasses in '..\..\..\Source\ImagingClasses.pas';

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
