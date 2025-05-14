program FireMonkeyDemo;

{$R *.dres}

uses
  FMX.Forms,
  FMX.Types,
  DemoUtils in '..\Common\DemoUtils.pas',
  MainForm in 'MainForm.pas' {FormMain},
  AboutForm in 'AboutForm.pas' {FormAbout},
  Imaging in '..\..\..\Source\Imaging.pas',
  ImagingTypes in '..\..\..\Source\ImagingTypes.pas',
  ImagingCanvases in '..\..\..\Source\ImagingCanvases.pas',
  ImagingClasses in '..\..\..\Source\ImagingClasses.pas',
  ImagingFormats in '..\..\..\Source\ImagingFormats.pas',
  ImagingIO in '..\..\..\Source\ImagingIO.pas',
  ImagingUtility in '..\..\..\Source\ImagingUtility.pas',
  ImagingFmx in '..\..\..\Extensions\ImagingFmx.pas';

{$R *.res}

begin
  //FMX.Types.GlobalUseDirect2DSoftware := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.Run;
end.
