unit AboutUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, StdCtrls, Imaging, DemoUtils;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    BitBtn1: TBitBtn;
    Image: TImage;
    Image2: TImage;
    ImLabel: TLabel;
    ImLabel1: TLabel;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  AboutForm: TAboutForm;

implementation

{ TAboutForm }

procedure TAboutForm.FormShow(Sender: TObject);
begin
  ImLabel.Caption := 'Vampyre Imaging Library version ' + Imaging.GetVersionStr;
  if Image.Picture.Graphic = nil then
    Image.Picture.LoadFromFile(GetRootDir + PathDelim + 'Doc' + PathDelim +
      'Common' + PathDelim + 'logo.png');
end;

initialization
  {$I aboutunit.lrs}

end.

