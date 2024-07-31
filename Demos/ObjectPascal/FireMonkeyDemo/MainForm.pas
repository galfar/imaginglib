{
  Vampyre Imaging Library Demo
  FireMonkey Demo (class api, fmx interaction)

  This demo is a simple image viewer. On the left of the window is a list box with
  information and thumbnail of images loaded from file. Selecting item in
  list box displays the image in image viewer component that fills the rest of
  the app window. Loaded image can be saved back to disk in one the supported
  file formats.

  Demo uses ImagingFmx extension to convert between Imaging's and FireMonkey's
  image classes.

  Image is loaded from the file in a background thread while the UI shows
  progress animation.

  Note: tested only in Delphi 11
}
unit MainForm;

{$IF not Defined (DCC) or (CompilerVersion < 25.0)}
  {$MESSAGE FATAL 'Needs at least Delphi XE4'}
{$IFEND}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Filter.Effects, FMX.Graphics,
  FMX.Layouts, FMX.ListBox, FMX.ExtCtrls, FMX.Objects, FMX.StdCtrls, FMX.Effects,
  FMX.Controls.Presentation, System.ImageList, FMX.ImgList, FMX.ComboEdit, FMX.Edit,

  ImagingTypes,
  Imaging,
  ImagingClasses,
  ImagingUtility,
  ImagingFmx, Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components;

type
  TFormMain = class(TForm)
    Splitter: TSplitter;
    ToolBar: TToolBar;
    ListImages: TListBox;
    BtnOpenImage: TSpeedButton;
    ImageViewer: TImageViewer;
    StyleBook: TStyleBook;
    PanelBack: TPanel;
    AniIndicator: TAniIndicator;
    OpenDialog: TOpenDialog;
    BtnSaveImage: TSpeedButton;
    BtnAbout: TSpeedButton;
    SaveDialog: TSaveDialog;
    ImageList: TImageList;
    DropTarget: TDropTarget;
    BlurEffect: TGaussianBlurEffect;
    LayoutCombo: TLayout;
    ComboScale: TComboEdit;
    LayoutSide: TLayout;
    HueAdjustEffect: THueAdjustEffect;
    LineSep: TLine;
    BindingsList: TBindingsList;
    LinkControlToPropertyHue: TLinkControlToProperty;
    LayoutTrack: TLayout;
    HueTrackBar: TTrackBar;
    procedure BtnOpenImageClick(Sender: TObject);
    procedure BtnAboutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnSaveImageClick(Sender: TObject);
    procedure ListImagesChange(Sender: TObject);
    procedure DropTargetDropped(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure DropTargetDragOver(Sender: TObject; const Data: TDragObject;
      const Point: TPointF; var Operation: TDragOperation);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure ComboScaleClosePopup(Sender: TObject);
    procedure ComboScaleKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure ImageViewerCalcContentBounds(Sender: TObject;
      var ContentBounds: TRectF);
  private
    FImage: TMultiImage;
    FFileName: string;
    FLoaderThread: TThread;
    procedure OpenFile(const FileName: string);
    procedure LoadingFinished(Success: Boolean; const ErrorMsg: string);
    procedure FillListBox(Image: TMultiImage);
    procedure SelectImage(Index: Integer);
    procedure UpdateScaleText;
  end;

var
  FormMain: TFormMain;

implementation

uses
  AboutForm;

{$R *.fmx}

const
  ThumbMaxX = 106;
  ThumbMaxY = 92;

type
  TImgLoaderThread = class(TThread)
  private
    type
      TFinishedHandler = reference to procedure(Success: Boolean; const ErrorMsg: string);
  private
    FFileName: string;
    FImageRef: TMultiImage;
    FFinishedHandler: TFinishedHandler;
  protected
    procedure Execute; override;
  public
    constructor Create(const FileName: string; ImageRef: TMultiImage;
      FinishedHandler: TFinishedHandler);
  end;

procedure ClearImagesAndThumbs(Img: TMultiImage);
var
  I: Integer;
begin
  for I := 0 to Img.ImageCount - 1 do
  begin
    if Img.DataArray[I].Tag <> nil then
      TObject(Img.DataArray[I].Tag).Free;
  end;
  Img.ClearAll;
end;

{ TImgLoaderThread }

constructor TImgLoaderThread.Create(const FileName: string;
  ImageRef: TMultiImage; FinishedHandler: TFinishedHandler);
begin
  FFileName := FileName;
  FImageRef := ImageRef;
  FFinishedHandler := FinishedHandler;
  FreeOnTerminate := True;

  inherited Create(False);
end;

procedure TImgLoaderThread.Execute;
var
  I: Integer;
  Success: Boolean;
  ErrorMsg: string;
  Thumb: TSingleImage;
begin
  TThread.NameThreadForDebugging('ImageLoaderThread');
  ErrorMsg := '';

  // Delete old images and thumbnails
  ClearImagesAndThumbs(FImageRef);

  try
    // Load image from file
    FImageRef.LoadMultiFromFile(FFileName);
    Success := FImageRef.AllImagesValid;

    // Generate thumbnails for subimages
    for I := 0 to FImageRef.ImageCount - 1 do
    begin
      Thumb := TSingleImage.Create;
      FImageRef.ActiveImage := I;
      FImageRef.ResizeToFit(ThumbMaxX, ThumbMaxY, rfBilinear, Thumb);
      FImageRef.DataArray[I].Tag := Thumb;
    end;
  except
    on E: Exception do
    begin
      Success := False;
      ErrorMsg := E.Message;
    end;
  end;

  Synchronize(
    procedure
    begin
      FFinishedHandler(Success, ErrorMsg);
    end);
end;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Caption := Caption + ' - ' + Imaging.SImagingLibTitle + ' ' + Imaging.GetVersionStr;
  FImage := TMultiImage.Create;
  // For panning the scaled up image with a mouse drag
  ImageViewer.AniCalculations.TouchTracking := [ttVertical, ttHorizontal];

  if (ParamCount > 0) and FileExists(ParamStr(1)) then
    OpenFile(ParamStr(1));
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  ListImages.Clear;
  ClearImagesAndThumbs(FImage);
  FImage.Free;
end;

procedure TFormMain.FillListBox(Image: TMultiImage);
var
  Item: TListBoxItem;
  I, ImgSize: Integer;
  Data: TImageData;
  Bmp: TBitmap;
begin
  ListImages.Clear;
  Bmp := TBitmap.Create(0, 0);

  try
    for I := 0 to FImage.ImageCount - 1 do
    begin
      Data := FImage.DataArray[I];

      Item := TListBoxItem.Create(ListImages);
      Item.Parent := ListImages;
      Item.StyleLookup := 'ListBoxItem';

      ImgSize := Data.Size;
      if ImgSize > 8192 then
        ImgSize := ImgSize div 1024;

      ImagingFmx.ConvertImageToFmxBitmap(TSingleImage(Data.Tag), Bmp);

      Item.StylesData['ImgThumb'] := Bmp;
      Item.StylesData['TextTitle'] := Format('Image %d/%d', [I + 1, FImage.ImageCount]);
      Item.StylesData['TextInfo'] :=
        Format('Resolution: %dx%d', [Data.Width, Data.Height]) + sLineBreak +
        Format('Format: %s', [GetFormatName(Data.Format)]) + sLineBreak +
        Format('Size: %.0n %s', [ImgSize + 0.0, Iff(ImgSize = Data.Size, 'B', 'KiB')]);
    end;
  finally
    Bmp.Free;
  end;
end;

procedure TFormMain.UpdateScaleText;
begin
  ComboScale.Text := FloatToStrFmt(ImageViewer.BitmapScale * 100, 0) + ' %';
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if ssCtrl in Shift then
  begin
    case Key of
      vkAdd: ImageViewer.BitmapScale := ImageViewer.BitmapScale * 1.1;
      vkSubtract: ImageViewer.BitmapScale := ImageViewer.BitmapScale * 0.9;
    end;
  end;
end;

procedure TFormMain.ImageViewerCalcContentBounds(Sender: TObject;
  var ContentBounds: TRectF);
begin
  UpdateScaleText;
end;

procedure TFormMain.ListImagesChange(Sender: TObject);
begin
  if ListImages.ItemIndex >= 0 then
    SelectImage(ListImages.ItemIndex);
end;

procedure TFormMain.ComboScaleClosePopup(Sender: TObject);
const
  Scales: array[0..5] of Double = (0.25, 0.5, 1.0, 1.5, 2.0, 4.0);
begin
  if ComboScale.ItemIndex <= 5 then
    ImageViewer.BitmapScale := Scales[ComboScale.ItemIndex]
  else if ComboScale.ItemIndex = ComboScale.Items.Count - 1 then
    ImageViewer.BestFit;

  ComboScale.ResetFocus;
  UpdateScaleText;
end;

procedure TFormMain.ComboScaleKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
var
  Scale: Integer;
begin
  if Key = vkReturn then
  begin
    var Text := ComboScale.Text.Trim([' ', '%']);
    if TryStrToInt(Text, Scale) then
    begin
      if (Scale >= 1) and (Scale <= 1000) then
        ImageViewer.BitmapScale := Scale / 100;
    end;

    ComboScale.ResetFocus;
    UpdateScaleText;
  end;
end;

procedure TFormMain.LoadingFinished(Success: Boolean; const ErrorMsg: string);
begin
  if Success then
  begin
    FillListBox(FImage);
    ListImages.ItemIndex := 0;
  end
  else
  begin
    MessageDlg('Error loading image: ' + ErrorMsg, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
    FImage.ClearAll;
  end;

  AniIndicator.Visible := False;
  BlurEffect.Enabled := False;
  ToolBar.Enabled := True;
end;

procedure TFormMain.SelectImage(Index: Integer);
begin
  FImage.ActiveImage := Index;

  ImageViewer.BeginUpdate;
  try
    ImagingFmx.ConvertImageToFmxBitmap(FImage, ImageViewer.Bitmap);
    ImageViewer.BestFit;

    ComboScale.Enabled := True;
    ImageViewer.DisableMouseWheel := False;
    HueTrackBar.Enabled := True;
  finally
    ImageViewer.EndUpdate;
  end;
end;

procedure TFormMain.BtnSaveImageClick(Sender: TObject);
begin
  if not FImage.AllImagesValid then
  begin
    MessageDlg('No image is loaded.', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
    Exit;
  end;

  SaveDialog.Filter := GetImageFileFormatsFilter(False);
  SaveDialog.FileName := ChangeFileExt(ExtractFileName(FFileName), '');
  SaveDialog.FilterIndex := GetFileNameFilterIndex(FFileName, False);

  if SaveDialog.Execute then
  begin
    FFileName := ChangeFileExt(SaveDialog.FileName, '.' + GetFilterIndexExtension(SaveDialog.FilterIndex, False));
    FImage.SaveMultiToFile(FFileName);
  end;
end;

procedure TFormMain.DropTargetDragOver(Sender: TObject; const Data: TDragObject;
  const Point: TPointF; var Operation: TDragOperation);
begin
  if (Length(Data.Files) > 0) and Imaging.IsFileFormatSupported(Data.Files[0]) then
  begin
    Operation := TDragOperation.Copy;
  end;
end;

procedure TFormMain.OpenFile(const FileName: string);
begin
  FFileName := FileName;

  ListImages.Clear;
  ImageViewer.Bitmap.SetSize(0, 0);
  ToolBar.Enabled := False;
  AniIndicator.Visible := True;
  BlurEffect.Enabled := True;

  FLoaderThread := TImgLoaderThread.Create(FFileName, FImage, LoadingFinished);
end;

procedure TFormMain.DropTargetDropped(Sender: TObject; const Data: TDragObject;
  const Point: TPointF);
begin
  if Length(Data.Files) > 0 then
  begin
    OpenFile(Data.Files[0]);
  end;
end;

procedure TFormMain.BtnOpenImageClick(Sender: TObject);
begin
  OpenDialog.Filter := Imaging.GetImageFileFormatsFilter(True);
  if OpenDialog.Execute then
    OpenFile(OpenDialog.FileName);
end;

procedure TFormMain.BtnAboutClick(Sender: TObject);
var
  X, Y: Integer;
begin
  // Place it manually - poMainFormCenter etc. doesn't really work
  // when main form has poScreenCenter
  X := Left + (Width - FormAbout.Width) div 2;
  Y := Top + (Height - FormAbout.Height) div 2;
  FormAbout.SetBounds(X, Y, FormAbout.Width, FormAbout.Height);
  FormAbout.ShowModal;
end;


end.
