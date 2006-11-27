{
  Vampyre Imaging Library Demo
  VCL Image Browser (ObjectPascal, high level/component sets/canvas, Win32)
  tested in Delphi 7/10
  written by Marek Mauder

  This simple viewer application shows usage of high level class interface
  to Imaging library and also drawing images onto standard VCL TCanvas.
  TImagingCanvas class is also used here.

  In the left part of the window is shell tree view component. Here you can
  select files located in your computer. If the selected file is in one of the
  supported formats (JPG, BMP, TGA, DDS, PNG, MNG, JNG) it is displayed in the viewer
  area and some information about the file is displayed in the info area.
  If image file contains subimages you can view them too. Select active subimage
  by clicking on buttons with arrows (Previous/Next).

  When supported file is selected in shell tree view it is loaded to
  TMultiImage and converted to ifA8R8G8B8
  data format. Active subimage is then drawn TPainBox component's
  client area using DisplayImage procedure (direct bit copy, no need to
  convert Imaging's data to TGraphic).
}

unit Main;

{$I ImagingOptions.inc}

{$IF not Defined(COMPONENT_SET_VCL) or not Defined(DELPHI)}
  {$MESSAGE ERROR 'This program requires Delphi with VCL'}
{$IFEND}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ShellCtrls, ExtCtrls, StdCtrls, Buttons,
  ImagingTypes,
  Imaging,
  ImagingClasses,
  ImagingComponents,
  ImagingCanvases,
  { Uncomment this to get support for file formats declared in Extras package }
  //ImagingExtraFileFormats,
  ImagingUtility;

type
  TMainForm = class(TForm)
    LeftPanel: TPanel;
    RightPanel: TPanel;
    InfoPanel: TPanel;
    LabDataFormat: TLabel;
    LabFileFormat: TLabel;
    LabDim: TLabel;
    LabFileName: TLabel;
    Label3: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    Lab1: TLabel;
    ViewPanel: TPanel;
    PaintBox: TPaintBox;
    Tree: TShellTreeView;
    Splitter1: TSplitter;
    Label4: TLabel;
    LabActImage: TLabel;
    StatusBar: TStatusBar;
    BtnPrev: TSpeedButton;
    BtnNext: TSpeedButton;
    procedure PaintBoxPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure BtnPrevClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
  private
    FImg: TMultiImage; // Class that hold multiple images (load from MNG or DDS files for instance)
    FFileName: string;
    FLastTime: LongInt;
  public
    procedure SetSupported;
    procedure SetUnsupported;
    procedure LoadFile;
    procedure FillDefault;
  end;

const
  FillColor = $00FFFFA6;
  CheckersDensity = 8;
  SUnsupportedFormat = 'Selected item format not supported';

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.LoadFile;
var
  T: Int64;
begin
  try
    // DetermineFileFormat reads file header and returns image
    // file format identifier (like 'jpg', 'tga') if file is valid,
    // otherwise empty string is returned
    if Imaging.DetermineFileFormat(FFileName) <> '' then
    begin
      // Load all subimages in file
      T := ImagingUtility.GetTimeMicroseconds;
      FImg.LoadMultiFromFile(FFileName);
      FLastTime := (ImagingUtility.GetTimeMicroseconds - T) div 1000;
      StatusBar.SimpleText := Format('Last image loaded in: %.0n ms', [FLastTime * 1.0]);
      SetSupported;
      // Convert images to 32bit ARGB format for easier drawing later
      FImg.ConvertImages(ifA8R8G8B8);
      PaintBox.Repaint;
    end
    else
      SetUnsupported;
  except
    SetUnsupported;
  end;
end;

procedure TMainForm.SetSupported;
begin
  // Update image info and enable previous/next buttons
  LabDim.Caption := Format('%dx%d pixels', [FImg.Width, FImg.Height]);
  LabFileFormat.Caption := Imaging.FindImageFileFormat(GetFileExt(FFileName)).Name;
  LabDataFormat.Caption := Imaging.GetFormatName(FImg.Format);
  LabActImage.Caption := Format('%d/%d', [FImg.ActiveImage + 1, FImg.ImageCount]);
  BtnPrev.Enabled := True;
  BtnNext.Enabled := True;
end;

procedure TMainForm.SetUnsupported;
var
  ImgCanvas: TImagingCanvas;
  X, Y, Step: LongInt;
begin
  // Set info texts to 'unsupported' and create default image to show
  LabDim.Caption := SUnsupportedFormat;
  LabFileFormat.Caption := SUnsupportedFormat;
  LabDataFormat.Caption := SUnsupportedFormat;
  LabActImage.Caption := '0/0';
  StatusBar.SimpleText := 'No image loaded';
  BtnPrev.Enabled := False;
  BtnNext.Enabled := False;

  if Assigned(FImg) then
  begin
    FImg.CreateFromParams(CheckersDensity, CheckersDensity, ifA8R8G8B8, 1);

    // Create canvas for image and draw checker board
    ImgCanvas := ImagingCanvases.FindBestCanvasForImage(FImg).CreateForImage(FImg);

    Step := FImg.Width div CheckersDensity;
    for Y := 0 to CheckersDensity - 1 do
      for X := 0 to CheckersDensity - 1 do
      begin
        ImgCanvas.FillColor32 := IffUnsigned((Odd(X) and not Odd(Y)) or (not Odd(X) and Odd(Y)),
          pcWhite, pcBlack);
        ImgCanvas.FillRect(Rect(X * Step, Y * Step, (X + 1) * Step, (Y + 1) * Step));
      end;

    ImgCanvas.Free;
  end;

  PaintBox.Repaint;
end;

procedure TMainForm.BtnPrevClick(Sender: TObject);
begin
  FImg.ActiveImage := FImg.ActiveImage - 1;
  SetSupported;
  PaintBox.Repaint;
end;

procedure TMainForm.BtnNextClick(Sender: TObject);
begin
  FImg.ActiveImage := FImg.ActiveImage + 1;
  SetSupported;
  PaintBox.Repaint;
end;

procedure TMainForm.TreeChange(Sender: TObject; Node: TTreeNode);
begin
  // Selected item in the shell tree view has been changed
  // we check whether the selected item is valid file in one of the
  // supported formats
  FFileName := Tree.Path;
  LabFileName.Caption := ExtractFileName(FFileName);
  if FileExists(FFileName) and Assigned(Imaging.FindImageFileFormat(GetFileExt(FFileName))) then
    LoadFile
  else
    SetUnsupported;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FImg.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := Caption + ' version ' + Imaging.GetVersionStr;
  FImg := TMultiImage.Create;
  SetUnsupported;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  R: TRect;
begin
  FillDefault;
  if (FImg.Width > 0) and (FImg.Height > 0) and (FImg.Format = ifA8R8G8B8) then
  begin
    // Scale image to fit the paint box
    R := ImagingUtility.ScaleRectToRect(FImg.BoundsRect, PaintBox.ClientRect);
    // Draw image to canvas (without conversion) using OS drawing functions.
    // Note that DisplayImage only supports images in ifA8R8G8B8 format so
    // if you have image in different format you must convert it or
    // create standard TBitmap by calling ImagingComponents.ConvertImageToBitmap
    ImagingComponents.DisplayImage(PaintBox.Canvas, R, FImg);
  end;
end;

procedure TMainForm.FillDefault;
begin
  PaintBox.Canvas.Brush.Color := FillColor;
  PaintBox.Canvas.FillRect(PaintBox.ClientRect);
end;

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - add save image in desired format
    - add first/last image buttons
    - use left/right arrows to change active subimage
    - data format always shows A8R8G8B8 for multimages at index 1+

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - added canvas usage too
    - added support for viewing multiimages (like MNG)
    - change drawing to use stuff from ImagingComponents unit instead of
      converting to TBitmap
    - changed demo to use high level interface instead of low level

}

end.
