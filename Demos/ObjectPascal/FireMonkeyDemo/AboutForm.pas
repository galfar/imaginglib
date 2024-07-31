unit AboutForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Ani, FMX.Layouts,
  FMX.Filter.Effects, FMX.Effects, FMX.StdCtrls, FMX.Controls.Presentation,

  Imaging,
  DemoUtils;

type
  TFormAbout = class(TForm)
    ImgLogo: TImage;
    PanelBack: TPanel;
    BtnOk: TButton;
    LabVersion: TLabel;
    LabWebsite: TLabel;
    LabImaging: TLabel;
    FlowLayout: TFlowLayout;
    LabGitHub: TLabel;
    Effect: TPixelateEffect;
    EffectAnimation: TFloatAnimation;
    procedure FormCreate(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure EffectAnimationFinish(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.fmx}

procedure TFormAbout.EffectAnimationFinish(Sender: TObject);
begin
  Effect.Enabled := False;
  Close;
end;

procedure TFormAbout.FormCreate(Sender: TObject);
var
  LogoPath: string;
begin
  LogoPath := GetDataDir + PathDelim + 'Logo.png';
  if FileExists(LogoPath) then
    ImgLogo.Bitmap.LoadFromFile(LogoPath);
  LabVersion.Text := LabVersion.Text + GetVersionStr;
end;

procedure TFormAbout.BtnOkClick(Sender: TObject);
begin
  if Effect.Enabled then
    Exit;
  Effect.Enabled := True;
  EffectAnimation.Start;
end;

end.
