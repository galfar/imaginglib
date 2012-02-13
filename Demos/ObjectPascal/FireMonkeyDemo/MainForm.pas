unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Filter.Effects,
  FMX.Layouts, FMX.ListBox, FMX.ExtCtrls, FMX.Objects,

  ImagingTypes,
  Imaging,
  ImagingClasses,
  ImagingUtility,
  ImagingFmx;

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
    EmbossEffect: TEmbossEffect;
    SaveDialog: TSaveDialog;
    procedure BtnOpenImageClick(Sender: TObject);
    procedure BtnToolbarApplyStyleLookup(Sender: TObject);
    procedure BtnAboutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnSaveImageClick(Sender: TObject);
    procedure ListImagesChange(Sender: TObject);
  private
    FImage: TMultiImage;
    FFileName: string;
    FLoaderThread: TThread;
    function FindImage(const Name: string): TBitmap;
    procedure LoadingFinished(Success: Boolean; const ErrorMsg: string);
    procedure FillListBox(Image: TMultiImage);
    procedure SelectImage(Index: Integer);
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
  ErrorMsg := '';

  // Delete old images and thumbmails
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

      Item.Binding['ImgThumb'] := ObjectToVariant(Bmp);
      Item.Binding['TextTitle'] := Format('Image %d/%d', [I + 1, FImage.ImageCount]);
      Item.Binding['TextInfo'] :=
        Format('Resolution: %dx%d', [Data.Width, Data.Height]) + sLineBreak +
        Format('Format: %s', [GetFormatName(Data.Format)]) + sLineBreak +
        Format('Size: %.0n %s', [ImgSize + 0.0, Iff(ImgSize = Data.Size, 'B', 'KiB')]);
    end;
  finally
    Bmp.Free;
  end;
end;

function TFormMain.FindImage(const Name: string): TBitmap;
begin
  Result := (StyleBook.Root.FindStyleResource(Name) as TImage).Bitmap;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Caption := Caption + ' - ' + Imaging.SImagingLibTitle + ' ' + Imaging.GetVersionStr;
  FImage := TMultiImage.Create;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  ClearImagesAndThumbs(FImage);
  FImage.Free;
end;

procedure TFormMain.ListImagesChange(Sender: TObject);
begin
  if ListImages.ItemIndex >= 0 then
    SelectImage(ListImages.ItemIndex);
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
  EmbossEffect.Enabled := False;
  ToolBar.Enabled := True;
end;

procedure TFormMain.SelectImage(Index: Integer);
begin
  FImage.ActiveImage := Index;

  ImageViewer.BeginUpdate;
  try
    ImagingFmx.ConvertImageToFmxBitmap(FImage, ImageViewer.Bitmap);
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

procedure TFormMain.BtnToolbarApplyStyleLookup(Sender: TObject);
var
  FmxObj: TFmxObject;
begin
  FmxObj := Sender as TFmxObject;
  // Sets the toolbar icon bitmap, does the following line seem insane?
  FmxObj.Binding['icon'] := ObjectToVariant(FindImage(FmxObj.BindingName));
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

procedure TFormMain.BtnOpenImageClick(Sender: TObject);
begin
  OpenDialog.Filter := Imaging.GetImageFileFormatsFilter(True);
  if OpenDialog.Execute then
  begin
    FFileName := OpenDialog.FileName;

    ListImages.Clear;
    ImageViewer.Bitmap.SetSize(0, 0);
    ToolBar.Enabled := False;
    AniIndicator.Visible := True;
    EmbossEffect.Enabled := True;

    FLoaderThread := TImgLoaderThread.Create(FFileName, FImage, LoadingFinished);
  end;
end;

end.
