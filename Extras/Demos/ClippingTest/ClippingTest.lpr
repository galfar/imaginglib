program ClippingTest;

uses
 {$IFDEF FPC}
  Interfaces, LCLVersion,
 {$ENDIF}
  Forms,
  ClipForm in 'ClipForm.pas' {MainForm},
  ResultsForm in 'ResultsForm.pas' {ResultForm};

{$R *.res}

begin
{$IFDEF FPC}
 {$IF LCL_FULLVERSION >= 1080000}
  Application.Scaled := True;
 {$ENDIF}
{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TResultForm, ResultForm);
  Application.Run;
end.
