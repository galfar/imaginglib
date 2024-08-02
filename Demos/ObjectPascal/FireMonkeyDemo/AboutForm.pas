unit AboutForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Ani, FMX.Layouts,
  FMX.Filter.Effects, FMX.Effects, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Graphics,

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

uses
  ImagingClasses,
  ImagingFmx;

{$R *.fmx}

procedure LoadFmxBitmapFromResourceWithImaging(const ResName: string; Bitmap: TBitmap);
var
  ResourceStream: TResourceStream;
  Image: TSingleImage;
begin
  // Could read from stream directly to FMX Bitmap but let's also show
  // reading from resources to TSingleImage.
  ResourceStream := TResourceStream.Create(HInstance, ResName, RT_RCDATA);
  Image := TSingleImage.Create;
  try
    Image.LoadFromStream(ResourceStream);
    ImagingFmx.ConvertImageToFmxBitmap(Image, Bitmap);
  finally
    ResourceStream.Free;
    Image.Free;
  end;
end;

procedure TFormAbout.EffectAnimationFinish(Sender: TObject);
begin
  Effect.Enabled := False;
  Close;
end;

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  LoadFmxBitmapFromResourceWithImaging('LOGO', ImgLogo.Bitmap);
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
