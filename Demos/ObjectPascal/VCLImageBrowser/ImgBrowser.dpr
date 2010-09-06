// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program ImgBrowser;

uses
  Forms,
  Main;

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'VCL Image Browser';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
