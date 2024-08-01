unit AboutUnit;

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, StdCtrls, Imaging, DemoUtils;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    BitBtn1: TBitBtn;
    ImageLogo: TImage;
    ImageLaz: TImage;
    LabGitHub: TLabel;
    LabImaging: TLabel;
    LabWeb: TLabel;
    LabVersion: TLabel;
    LabDemo: TLabel;
    procedure FormShow(Sender: TObject);
    procedure LabLinkClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  AboutForm: TAboutForm;

implementation

uses
  LCLIntf,
  ImagingComponents;

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.FormShow(Sender: TObject);
begin
  LabVersion.Caption := 'version ' + Imaging.GetVersionStr;
  if ImageLogo.Picture.Graphic = nil then
  begin
    ImageLogo.Picture.LoadFromResourceName(HInstance, 'LOGO', ImagingComponents.TImagingPNG);
  end;
end;

procedure TAboutForm.LabLinkClick(Sender: TObject);
begin
  OpenURL(TLabel(Sender).Caption);
end;

end.

