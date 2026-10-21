{
  Vampyre Imaging Library
  by Marek Mauder
  https://github.com/galfar/imaginglib
  https://imaginglib.sourceforge.io
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.
}

{ This unit contains VCL/LCL TGraphic descendant which uses Imaging library
  for saving and loading.}
unit ImagingComponents;

{$I ImagingOptions.inc}

interface

{$IF Defined(FPC) and Defined(LCL)}
  {$DEFINE COMPONENT_SET_LCL}
{$ELSEIF Defined(DELPHI)}
  {$DEFINE COMPONENT_SET_VCL}
{$IFEND}

{$IF not Defined(COMPONENT_SET_LCL) and not Defined(COMPONENT_SET_VCL)}
// If no component sets should be used just include empty unit.
implementation
{$ELSE}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Types, Classes,
{$IFDEF COMPONENT_SET_VCL}
  Graphics,
{$ENDIF}
{$IFDEF COMPONENT_SET_LCL}
  GraphType,
  Graphics,
  LCLType,
  LCLIntf,
{$ENDIF}
  ImagingTypes, Imaging, ImagingClasses;

type
  { Graphic class which uses Imaging to load images.
    It has standard TBitmap class as ancestor and it can
    Assign also to/from TImageData structures and TBaseImage
    classes. If you want to perfectly preserve the original pixel format
    of the source image then these classes may not for you.

    This class is automatically registered to TPicture for all
    file extensions supported by Imaging (useful only for loading).
    If you just want to load images in various formats you can use this
    class or simply use TPicture.LoadFromXXX which will create this class
    automatically.

    For saving it always uses PNG fallback.
    For TGraphic classes that save in different formats look
    at TImagingGraphicForSave class.}
  TImagingGraphic = class(TBitmap)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    { Called by TFiler when reading and writing TPicture.Data property.
      We need to override ReadData+WriteData otherwise inherited ones from
      TBitmap would be called resulting in errors.}
    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
  public
    constructor Create; override;

    { Loads new image from the stream. It can load all image
      file formats supported by Imaging (and enabled of course)
      even though it is called by descendant class capable of
      saving only one file format.}
    procedure LoadFromStream(Stream: TStream); override;
    { Always saves as PNG.}
    procedure SaveToStream(Stream: TStream); override;
    { Copies the image contained in Source to this graphic object.
      Supports also TBaseImage descendants from ImagingClasses unit. }
    procedure Assign(Source: TPersistent); override;
    { Copies the image contained in TBaseImage to this graphic object.}
    procedure AssignFromImage(Image: TBaseImage);
    { Copies the current image to TBaseImage object.}
    procedure AssignToImage(Image: TBaseImage);
    { Copies the image contained in TImageData structure to this graphic object.}
    procedure AssignFromImageData(const ImageData: TImageData);
    { Copies the current image to TImageData structure.}
    procedure AssignToImageData(var ImageData: TImageData);

  {$IFDEF COMPONENT_SET_LCL}
    { Needed for TGraphic.LoadFromResourceName() to work.
      We return RT_RCDATA here. Also for TImagingBitmap since
      RT_BITMAP is stored differently than bitmap on disk (no BITMAPFILEHEADER).}
    function GetResourceType: TResourceType; override;
    { Used by TPicture.LoadFromStream to find the right TGraphic class for streams. }
    class function IsStreamFormatSupported(Stream: TStream): boolean; override;
  {$ENDIF}
  end;

  TImagingGraphicClass = class of TImagingGraphic;

  { Base (abstract) class for file format specific TGraphic classes that use
    Imaging for saving. Each descendant class can load all file formats
    supported by Imaging but save only one format (TImagingBitmap
    for *.bmp, TImagingJpeg for *.jpg). The image is saved in this one file
    format regardless of the extension you request).

    Format specific classes also allow easy access to Imaging options that
    affect saving of files (they are properties here).}
  TImagingGraphicForSave = class(TImagingGraphic)
  protected
    FDefaultFileExt: string;
    FSavingFormat: TImageFormat;
    procedure WriteData(Stream: TStream); override;
  public
    constructor Create; override;
    { Saves the current image to the stream. It is saved in the
      file format according to the DefaultFileExt property.
      So each descendant class can save some other file format.}
    procedure SaveToStream(Stream: TStream); override;
    { Returns TImageFileFormat descendant for this graphic class.}
    class function GetFileFormat: TImageFileFormat; virtual; abstract;
  {$IFDEF COMPONENT_SET_LCL}
    { Returns file extensions of this graphic class.}
    class function GetFileExtensions: string; override;
    { Returns default MIME type of this graphic class.}
    function GetMimeType: string; override;
  {$ENDIF}
    { Default (the most common) file extension of this graphic class.}
    property DefaultFileExt: string read FDefaultFileExt;
  end;

  TImagingGraphicForSaveClass = class of TImagingGraphicForSave;

{$IFNDEF DONT_LINK_BITMAP}
  { TImagingGraphic descendant for loading/saving Windows bitmaps.
    VCL/LCL both have native support for bitmaps so you might
    want to disable this class (although you can save bitmaps with
    RLE compression with this class).}
  TImagingBitmap = class(TImagingGraphicForSave)
  protected
    FUseRLE: Boolean;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
    { See ImagingBitmapRLE option for details.}
    property UseRLE: Boolean read FUseRLE write FUseRLE;
  end;
{$ENDIF}

{$IFNDEF DONT_LINK_JPEG}
  { TImagingGraphic descendant for loading/saving JPEG images.}
  TImagingJpeg = class(TImagingGraphicForSave)
  protected
    FQuality: LongInt;
    FProgressive: Boolean;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
  {$IFDEF COMPONENT_SET_LCL}
    function GetMimeType: string; override;
  {$ENDIF}
    { See ImagingJpegQuality option for details.}
    property Quality: LongInt read FQuality write FQuality;
    { See ImagingJpegProgressive option for details.}
    property Progressive: Boolean read FProgressive write FProgressive;
  end;
{$ENDIF}

{$IFNDEF DONT_LINK_PNG}
  { TImagingGraphic descendant for loading/saving PNG images.}
  TImagingPNG = class(TImagingGraphicForSave)
  protected
    FPreFilter: LongInt;
    FCompressLevel: LongInt;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
    { See ImagingPNGPreFilter option for details.}
    property PreFilter: LongInt read FPreFilter write FPreFilter;
    { See ImagingPNGCompressLevel option for details.}
    property CompressLevel: LongInt read FCompressLevel write FCompressLevel;
  end;
{$ENDIF}

{$IFNDEF DONT_LINK_GIF}
  { TImagingGraphic descendant for loading/saving GIF images.}
  TImagingGIF = class(TImagingGraphicForSave)
  public
    class function GetFileFormat: TImageFileFormat; override;
  end;
{$ENDIF}

{$IFNDEF DONT_LINK_TARGA}
  { TImagingGraphic descendant for loading/saving Targa images.}
  TImagingTarga = class(TImagingGraphicForSave)
  protected
    FUseRLE: Boolean;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
    { See ImagingTargaRLE option for details.}
    property UseRLE: Boolean read FUseRLE write FUseRLE;
  end;
{$ENDIF}

{$IFNDEF DONT_LINK_DDS}
  { Compression type used when saving DDS files by TImagingDds.}
  TDDSCompression = (dcNone, dcDXT1, dcDXT3, dcDXT5);

  { TImagingGraphic descendant for loading/saving DDS images.}
  TImagingDDS = class(TImagingGraphicForSave)
  protected
    FCompression: TDDSCompression;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
    { You can choose compression type used when saving DDS file.
      dcNone means that file will be saved in the current bitmaps pixel format.}
    property Compression: TDDSCompression read FCompression write FCompression;
  end;
{$ENDIF}

{$IFNDEF DONT_LINK_MNG}
  { TImagingGraphic descendant for loading/saving MNG images.}
  TImagingMNG = class(TImagingGraphicForSave)
  protected
    FLossyCompression: Boolean;
    FLossyAlpha: Boolean;
    FPreFilter: LongInt;
    FCompressLevel: LongInt;
    FQuality: LongInt;
    FProgressive: Boolean;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
  {$IFDEF COMPONENT_SET_LCL}
    function GetMimeType: string; override;
  {$ENDIF}
    { See ImagingMNGLossyCompression option for details.}
    property LossyCompression: Boolean read FLossyCompression write FLossyCompression;
    { See ImagingMNGLossyAlpha option for details.}
    property LossyAlpha: Boolean read FLossyAlpha write FLossyAlpha;
    { See ImagingMNGPreFilter option for details.}
    property PreFilter: LongInt read FPreFilter write FPreFilter;
    { See ImagingMNGCompressLevel option for details.}
    property CompressLevel: LongInt read FCompressLevel write FCompressLevel;
    { See ImagingMNGQuality option for details.}
    property Quality: LongInt read FQuality write FQuality;
    { See ImagingMNGProgressive option for details.}
    property Progressive: Boolean read FProgressive write FProgressive;
  end;
{$ENDIF}

{$IFNDEF DONT_LINK_JNG}
  { TImagingGraphic descendant for loading/saving JNG images.}
  TImagingJNG = class(TImagingGraphicForSave)
  protected
    FLossyAlpha: Boolean;
    FAlphaPreFilter: LongInt;
    FAlphaCompressLevel: LongInt;
    FQuality: LongInt;
    FProgressive: Boolean;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
    { See ImagingJNGLossyAlpha option for details.}
    property LossyAlpha: Boolean read FLossyAlpha write FLossyAlpha;
    { See ImagingJNGPreFilter option for details.}
    property AlphaPreFilter: LongInt read FAlphaPreFilter write FAlphaPreFilter;
    { See ImagingJNGCompressLevel option for details.}
    property AlphaCompressLevel: LongInt read FAlphaCompressLevel write FAlphaCompressLevel;
    { See ImagingJNGQuality option for details.}
    property Quality: LongInt read FQuality write FQuality;
    { See ImagingJNGProgressive option for details.}
    property Progressive: Boolean read FProgressive write FProgressive;
  end;
{$ENDIF}

{ Returns bitmap pixel format with the closest match with given data format.}
function DataFormatToPixelFormat(Format: TImageFormat): TPixelFormat;
{ Returns data format with closest match with given bitmap pixel format.}
function PixelFormatToDataFormat(Format: TPixelFormat): TImageFormat;

{ Converts TImageData structure to VCL/CLX/LCL bitmap.}
procedure ConvertDataToBitmap(const Data: TImageData; Bitmap: TBitmap);
{ Converts VCL/CLX/LCL bitmap to TImageData structure.}
procedure ConvertBitmapToData(Bitmap: TBitmap; var Data: TImageData);
{ Converts TBaseImage instance to VCL/CLX/LCL bitmap.}
procedure ConvertImageToBitmap(Image: TBaseImage; Bitmap: TBitmap);
{ Converts VCL/CLX/LCL bitmap to TBaseImage. Image must exist before
  procedure is called. It overwrites its current image data.
  When Image is TMultiImage only the current image level is overwritten.}
procedure ConvertBitmapToImage(Bitmap: TBitmap; Image: TBaseImage);

{ Displays image onto TCanvas to rectangle DstRect.
  See the overload with SrcRect for details.}
procedure DisplayImageData(DstCanvas: TCanvas; const DstRect: TRect; const ImageData: TImageData); overload;
{ Displays part SrcRect of image stored in TImageData structure onto TCanvas
  to rectangle DstRect, scaled with nearest neighbor sampling. Dest and Src
  rectangles represent coordinates in the form (X1, Y1, X2, Y2).
  Images in [ifA8R8G8B8, ifX8R8G8B8] formats are drawn directly from their
  memory without any copy or conversion to TBitmap, so this is fast
  also for images that change frequently (e.g. view buffer of image viewer).
  Images in other formats get only their SrcRect part converted before drawing.
  Alpha channel is not used, the image is drawn opaque.
  Implemented for VCL and LCL Win32, GTK2, GTK3, Qt5, Qt6, and Cocoa widget sets,
  other LCL widget sets use slower drawing through TBitmap.}
procedure DisplayImageData(DstCanvas: TCanvas; const DstRect: TRect; const ImageData: TImageData; const SrcRect: TRect); overload;
{ Displays image onto TCanvas at position [DstX, DstY].
  Behaviour is the same as of DisplayImageData.}
procedure DisplayImage(DstCanvas: TCanvas; DstX, DstY: LongInt; Image: TBaseImage); overload;
{ Displays image onto TCanvas to rectangle DstRect.
  Behaviour is the same as of DisplayImageData.}
procedure DisplayImage(DstCanvas: TCanvas; const DstRect: TRect; Image: TBaseImage); overload;
{ Displays part of the image specified by SrcRect onto TCanvas to rectangle DstRect.
  Behaviour is the same as of DisplayImageData.}
procedure DisplayImage(DstCanvas: TCanvas; const DstRect: TRect; Image: TBaseImage; const SrcRect: TRect); overload;

{$IFDEF MSWINDOWS}
{ Displays image stored in TImageData structure onto Windows device context.
  Behaviour is the same as of DisplayImageData.}
procedure DisplayImageDataOnDC(DC: HDC; const DstRect: TRect; const ImageData: TImageData; const SrcRect: TRect);
{$ENDIF}

procedure RegisterTypes;

implementation

uses
{$IF Defined(LCL)}
  InterfaceBase,
  {$IF Defined(LCLGTK2)}
    GDK2, GTK2Def, Cairo,
  {$ELSEIF Defined(LCLGTK3)}
    LazCairo1, Gtk3Objects,
  {$ELSEIF Defined(LCLqt5)}
    Qt5, qtobjects,
  {$ELSEIF Defined(LCLqt6)}
    Qt6, qtobjects,
  {$ELSEIF Defined(LCLcocoa)}
    MacOSAll, CocoaGDIObjects,
  {$IFEND}
{$IFEND}
{$IFNDEF DONT_LINK_BITMAP}
  ImagingBitmap,
{$ENDIF}
{$IFNDEF DONT_LINK_JPEG}
  ImagingJpeg,
{$ENDIF}
{$IFNDEF DONT_LINK_GIF}
  ImagingGif,
{$ENDIF}
{$IFNDEF DONT_LINK_TARGA}
  ImagingTarga,
{$ENDIF}
{$IFNDEF DONT_LINK_DDS}
  ImagingDds,
{$ENDIF}
{$IF not Defined(DONT_LINK_PNG) or not Defined(DONT_LINK_MNG) or not Defined(DONT_LINK_JNG)}
  ImagingNetworkGraphics,
{$IFEND}
  ImagingFormats, ImagingUtility;

resourcestring
  SBadFormatDataToBitmap = 'Cannot find compatible bitmap format for image %s';
  SBadFormatBitmapToData = 'Cannot find compatible data format for bitmap %p';
  SBadFormatDisplay = 'Unsupported image format passed';
  SFailedBitmapFromData = 'Failed to create TBitmap from TImageData';
  SImagingGraphicName = 'Imaging Graphic AllInOne';

var
  RegisteredFormats: TList;
  RegisteredGraphicsClasses: Boolean = False;

{ Registers types to VCL/LCL.
  In some cases (base+ext package installed in Lazarus) RegisterTypes can be
  called twice so must keep track of which formats were already registered. }
procedure RegisterTypes;
var
  I: LongInt;

  procedure RegisterFileFormatAllInOne(Format: TImageFileFormat);
  var
    I: LongInt;
  begin
    if RegisteredFormats.IndexOf(Format) >= 0 then
      Exit;

    for I := 0 to Format.Extensions.Count - 1 do
    begin
      TPicture.RegisterFileFormat(Format.Extensions[I], SImagingGraphicName,
        TImagingGraphic);
    end;
    RegisteredFormats.Add(Format);
  end;

  procedure RegisterFileFormat(AClass: TImagingGraphicForSaveClass);
  var
    I: LongInt;
  begin
    for I := 0 to AClass.GetFileFormat.Extensions.Count - 1 do
      TPicture.RegisterFileFormat(AClass.GetFileFormat.Extensions[I],
        AClass.GetFileFormat.Name, AClass);
  end;

begin
  for I := Imaging.GetFileFormatCount - 1 downto 0 do
    RegisterFileFormatAllInOne(Imaging.GetFileFormatAtIndex(I));
  Classes.RegisterClass(TImagingGraphic);

  if RegisteredGraphicsClasses then
    Exit;

{$IFNDEF DONT_LINK_TARGA}
  RegisterFileFormat(TImagingTarga);
  Classes.RegisterClass(TImagingTarga);
{$ENDIF}
{$IFNDEF DONT_LINK_DDS}
  RegisterFileFormat(TImagingDDS);
  Classes.RegisterClass(TImagingDDS);
{$ENDIF}
{$IFNDEF DONT_LINK_JNG}
  RegisterFileFormat(TImagingJNG);
  Classes.RegisterClass(TImagingJNG);
{$ENDIF}
{$IFNDEF DONT_LINK_MNG}
  RegisterFileFormat(TImagingMNG);
  Classes.RegisterClass(TImagingMNG);
{$ENDIF}
{$IFNDEF DONT_LINK_GIF}
  RegisterFileFormat(TImagingGIF);
  Classes.RegisterClass(TImagingGIF);
{$ENDIF}
{$IFNDEF DONT_LINK_PNG}
  RegisterFileFormat(TImagingPNG);
  Classes.RegisterClass(TImagingPNG);
{$ENDIF}
{$IFNDEF DONT_LINK_JPEG}
  RegisterFileFormat(TImagingJpeg);
  Classes.RegisterClass(TImagingJpeg);
{$ENDIF}
{$IFNDEF DONT_LINK_BITMAP}
  RegisterFileFormat(TImagingBitmap);
  Classes.RegisterClass(TImagingBitmap);
{$ENDIF}

  RegisteredGraphicsClasses := True;
end;

{ Unregisters types from VCL/LCL.}
procedure UnRegisterTypes;
begin
{$IFNDEF DONT_LINK_BITMAP}
  TPicture.UnregisterGraphicClass(TImagingBitmap);
  Classes.UnRegisterClass(TImagingBitmap);
{$ENDIF}
{$IFNDEF DONT_LINK_JPEG}
  TPicture.UnregisterGraphicClass(TImagingJpeg);
  Classes.UnRegisterClass(TImagingJpeg);
{$ENDIF}
{$IFNDEF DONT_LINK_PNG}
  TPicture.UnregisterGraphicClass(TImagingPNG);
  Classes.UnRegisterClass(TImagingPNG);
{$ENDIF}
{$IFNDEF DONT_LINK_GIF}
  TPicture.UnregisterGraphicClass(TImagingGIF);
  Classes.UnRegisterClass(TImagingGIF);
{$ENDIF}
{$IFNDEF DONT_LINK_TARGA}
  TPicture.UnregisterGraphicClass(TImagingTarga);
  Classes.UnRegisterClass(TImagingTarga);
{$ENDIF}
{$IFNDEF DONT_LINK_DDS}
  TPicture.UnregisterGraphicClass(TImagingDDS);
  Classes.UnRegisterClass(TImagingDDS);
{$ENDIF}
  TPicture.UnregisterGraphicClass(TImagingGraphic);
  Classes.UnRegisterClass(TImagingGraphic);
end;

function DataFormatToPixelFormat(Format: TImageFormat): TPixelFormat;
begin
  case Format of
{$IFDEF COMPONENT_SET_VCL}
    ifIndex8: Result := pf8bit;
    ifR5G6B5: Result := pf16bit;
    ifR8G8B8: Result := pf24bit;
{$ENDIF}
    ifA8R8G8B8,
    ifX8R8G8B8: Result := pf32bit;
  else
    Result := pfCustom;
  end;
end;

function PixelFormatToDataFormat(Format: TPixelFormat): TImageFormat;
begin
  case Format of
    pf8bit: Result := ifIndex8;
    pf15bit: Result := ifA1R5G5B5;
    pf16bit: Result := ifR5G6B5;
    pf24bit: Result := ifR8G8B8;
    pf32bit: Result := ifA8R8G8B8;
  else
    Result := ifUnknown;
  end;
end;

procedure ConvertDataToBitmap(const Data: TImageData; Bitmap: TBitmap);
var
  PF: TPixelFormat;
  Info: TImageFormatInfo;
  WorkData: TImageData;
{$IFDEF COMPONENT_SET_VCL}
  I: NativeInt;
  LineBytes: LongInt;
  LogPalette: TMaxLogPalette;
{$ENDIF}
{$IFDEF COMPONENT_SET_LCL}
  RawImage: TRawImage;
  ImgHandle, ImgMaskHandle: HBitmap;
{$ENDIF}
begin
  PF := DataFormatToPixelFormat(Data.Format);
  GetImageFormatInfo(Data.Format, Info);

  if (PF = pf8bit) and PaletteHasAlpha(Data.Palette, Info.PaletteEntries) then
  begin
    // Some indexed images may have valid alpha data, don't lose it!
    // (e.g. transparent 8bit PNG or GIF images)
    PF := pfCustom;
  end;

  if PF = pfCustom then
  begin
    // Convert from formats not supported by Graphics unit
    Imaging.InitImage(WorkData);
    Imaging.CloneImage(Data, WorkData);
    if Info.IsFloatingPoint or Info.HasAlphaChannel or Info.IsSpecial then
      Imaging.ConvertImage(WorkData, ifA8R8G8B8)
    else
    begin
{$IFDEF COMPONENT_SET_VCL}
      if Info.IsIndexed or Info.HasGrayChannel then
        Imaging.ConvertImage(WorkData, ifIndex8)
      else if Info.UsePixelFormat then
        Imaging.ConvertImage(WorkData, ifR5G6B5)
      else
        Imaging.ConvertImage(WorkData, ifR8G8B8);
{$ELSE}
        Imaging.ConvertImage(WorkData, ifA8R8G8B8);
{$ENDIF}
    end;

    PF := DataFormatToPixelFormat(WorkData.Format);
    GetImageFormatInfo(WorkData.Format, Info);
  end
  else
    WorkData := Data;
    
  if PF = pfCustom then
    RaiseImaging(SBadFormatDataToBitmap, [ImageToStr(WorkData)]);

{$IFDEF COMPONENT_SET_VCL}
  Bitmap.Width := WorkData.Width;
  Bitmap.Height := WorkData.Height;
  Bitmap.PixelFormat := PF;

  if (PF = pf8bit) and (WorkData.Palette <> nil) then
  begin
    // Copy palette, this must be done before copying bits
    FillChar(LogPalette, SizeOf(LogPalette), 0);
    LogPalette.palVersion := $300;
    LogPalette.palNumEntries := Info.PaletteEntries;
    for I := 0 to Info.PaletteEntries - 1 do
    with LogPalette do
    begin
      palPalEntry[I].peRed := WorkData.Palette[I].R;
      palPalEntry[I].peGreen := WorkData.Palette[I].G;
      palPalEntry[I].peBlue := WorkData.Palette[I].B;
    end;
    Bitmap.Palette := CreatePalette(PLogPalette(@LogPalette)^);
  end;

  // Copy scanlines
  LineBytes := WorkData.Width * Info.BytesPerPixel;
  for I := 0 to WorkData.Height - 1 do
    Move(PBuffer(WorkData.Bits)[I * LineBytes], Bitmap.Scanline[I]^, LineBytes);

  // Delphi 2009 and newer support alpha transparency for TBitmap
{$IF Defined(DELPHI) and (CompilerVersion >= 20.0)}
  if Bitmap.PixelFormat = pf32bit then
    Bitmap.AlphaFormat := afDefined;
{$IFEND}
{$ENDIF}

{$IFDEF COMPONENT_SET_LCL}
  // Create 32bit raw image from image data
  FillChar(RawImage, SizeOf(RawImage), 0);
  with RawImage.Description do
  begin
    Width := WorkData.Width;
    Height := WorkData.Height;
    BitsPerPixel := 32;
    Format := ricfRGBA;
    LineEnd := rileDWordBoundary;
    BitOrder := riboBitsInOrder;
    ByteOrder := riboLSBFirst;
    LineOrder := riloTopToBottom;
    AlphaPrec := 8;
    RedPrec := 8;
    GreenPrec := 8;
    BluePrec := 8;
    AlphaShift := 24;
    RedShift := 16;
    GreenShift := 8;
    BlueShift := 0;
    Depth := 32; // Must be 32 for alpha blending (and for working in MacOSX Carbon)
  end;
  RawImage.Data := WorkData.Bits;
  RawImage.DataSize := WorkData.Size;

  Bitmap.Clear;
  // Create bitmap from raw image
  if RawImage_CreateBitmaps(RawImage, ImgHandle, ImgMaskHandle) then
  begin
    Bitmap.Handle := ImgHandle;
    Bitmap.MaskHandle := ImgMaskHandle;
  end;
{$ENDIF}

  if WorkData.Bits <> Data.Bits then
    Imaging.FreeImage(WorkData);

  if Bitmap.Empty then
    RaiseImaging(SFailedBitmapFromData, []);
end;

procedure ConvertBitmapToData(Bitmap: TBitmap; var Data: TImageData);
var
  I: NativeInt;
  LineBytes: LongInt;
  Format: TImageFormat;
  Info: TImageFormatInfo;
{$IFDEF COMPONENT_SET_VCL}
  Colors: Word;
  LogPalette: TMaxLogPalette;
{$ENDIF}
{$IFDEF COMPONENT_SET_LCL}
  RawImage: TRawImage;
  LineLazBytes: LongInt;
{$ENDIF}
begin
  Format := ifUnknown;
{$IFDEF COMPONENT_SET_LCL}
  // In the current Lazarus 0.9.10 Bitmap.PixelFormat property is useless.
  // We cannot change bitmap's format by changing it (it will just release
  // old image but not convert it to new format) nor we can determine bitmaps's
  // current format (it is usually set to pfDevice). So bitmap's format is obtained
  // trough RawImage api and cannot be changed to mirror some Imaging format
  // (so formats with no corresponding Imaging format cannot be saved now).

  if RawImage_DescriptionFromBitmap(Bitmap.Handle, RawImage.Description) then
    case RawImage.Description.BitsPerPixel of
      8: Format := ifIndex8;
      16:
        if RawImage.Description.Depth = 15 then
          Format := ifA1R5G5B5
        else
          Format := ifR5G6B5;
      24: Format := ifR8G8B8;
      32: Format := ifA8R8G8B8;
      48: Format := ifR16G16B16;
      64: Format := ifA16R16G16B16;
    end;
{$ELSE}
  Format := PixelFormatToDataFormat(Bitmap.PixelFormat);
  if Format = ifUnknown then
  begin
    // Convert from formats not supported by Imaging (1/4 bit)
    if Bitmap.PixelFormat < pf8bit then
       Bitmap.PixelFormat := pf8bit
    else
      Bitmap.PixelFormat := pf32bit;
    Format := PixelFormatToDataFormat(Bitmap.PixelFormat);
  end;
{$ENDIF}

  if Format = ifUnknown then
    RaiseImaging(SBadFormatBitmapToData, []);

  Imaging.NewImage(Bitmap.Width, Bitmap.Height, Format, Data);
  GetImageFormatInfo(Data.Format, Info);
  LineBytes := Data.Width * Info.BytesPerPixel;

{$IFDEF COMPONENT_SET_VCL}
  if (Format = ifIndex8) and (GetObject(Bitmap.Palette, SizeOf(Colors),
    @Colors) <> 0) then
  begin
    // Copy palette
    GetPaletteEntries(Bitmap.Palette, 0, Colors, LogPalette.palPalEntry);
    if Colors > Info.PaletteEntries  then
      Colors := Info.PaletteEntries;
    for I := 0 to Colors - 1 do
    with LogPalette do
    begin
      Data.Palette[I].A := $FF;
      Data.Palette[I].R := palPalEntry[I].peRed;
      Data.Palette[I].G := palPalEntry[I].peGreen;
      Data.Palette[I].B := palPalEntry[I].peBlue;
    end;
  end;
  // Copy scanlines
  for I := 0 to Data.Height - 1 do
    Move(Bitmap.ScanLine[I]^, PBuffer(Data.Bits)[I * LineBytes], LineBytes);
{$ENDIF}
{$IFDEF COMPONENT_SET_LCL}
  // Get raw image from bitmap (mask handle must be 0 or expect violations)
  if RawImage_FromBitmap(RawImage, Bitmap.Handle, 0, nil) then
  begin
    LineLazBytes := GetBytesPerLine(Data.Width, RawImage.Description.BitsPerPixel,
      RawImage.Description.LineEnd);
    // Copy scanlines
    for I := 0 to Data.Height - 1 do
    begin
      Move(PBuffer(RawImage.Data)[I * LineLazBytes], PBuffer(Data.Bits)[I * LineBytes], LineBytes);
    end;
    // May need to swap RB order, depends on widget set
    if RawImage.Description.BlueShift > RawImage.Description.RedShift then
      SwapChannels(Data, ChannelRed, ChannelBlue);

    RawImage.FreeData;
  end;
{$ENDIF}
end;

procedure ConvertImageToBitmap(Image: TBaseImage; Bitmap: TBitmap);
begin
  ConvertDataToBitmap(Image.ImageDataPointer^, Bitmap);
end;

procedure ConvertBitmapToImage(Bitmap: TBitmap; Image: TBaseImage);
begin
  ConvertBitmapToData(Bitmap, Image.ImageDataPointer^);
end;

const
  // Data formats that are drawn directly from image memory
  DirectDisplayFormats = [ifA8R8G8B8, ifX8R8G8B8];

{ Clips the rectangles and returns image in one of DirectDisplayFormats to be drawn.
  That is ImageData itself, or Region with converted SrcRect part of it (SrcRect
  is then moved to Region coordinates). Returns nil when there is nothing to draw.}
function PrepareDisplayData(const ImageData: TImageData; var DstRect, SrcRect: TRect;
  var Region: TImageData): PImageData;
var
  Info: TImageFormatInfo;
  DrawnRect, SrcValidRect: TRect;
begin
  Result := nil;
  // Source is clipped to the image here (destination pixels whose centers map
  // outside of it are left out), the platform clips the destination
  if not TestImage(ImageData) or
     not ClipStretchRects(SrcRect, DstRect, ImageData.Width, ImageData.Height, DstRect,
                          DrawnRect, SrcValidRect) then
    Exit;

  DstRect := DrawnRect;
  SrcRect := SrcValidRect;

  if ImageData.Format in DirectDisplayFormats then
  begin
    Result := @ImageData;
    Exit;
  end;

  if not GetImageFormatInfo(ImageData.Format, Info) then
    raise EImagingError.Create(SBadFormatDisplay);

  if Info.IsSpecial then
  begin
    // Compressed formats cannot be copied by pixel rectangles
    CloneImage(ImageData, Region);
  end
  else
  begin
    NewImage(RectWidth(SrcRect), RectHeight(SrcRect), ImageData.Format, Region);
    CopyRect(ImageData, SrcRect.Left, SrcRect.Top, RectWidth(SrcRect), RectHeight(SrcRect),
             Region, 0, 0);
    if Info.IsIndexed then
      CopyPalette(ImageData.Palette, Region.Palette, 0, 0, Info.PaletteEntries);
    SrcRect := Types.Rect(0, 0, Region.Width, Region.Height);
  end;

  if not ConvertImage(Region, ifX8R8G8B8) then
    raise EImagingError.Create(SBadFormatDisplay);
  Result := @Region;
end;

{$IF Defined(MSWINDOWS) or Defined(LCLGTK2) or Defined(LCLGTK3)}
const
  // GDI StretchDIBits draws nothing for source rectangles over 32767 pixels (and
  // reports success). Cairo cannot create larger image surfaces and draws
  // nothing when scaling surfaces a bit smaller than that (about 32600 pixels).
  MaxDisplayTileSize = 16384;

type
  { Draws SrcRect of image in one of DirectDisplayFormats to DstRect of Target,
    SrcRect is not larger than MaxDisplayTileSize.}
  TDrawDisplayTileProc = procedure(Target: Pointer; const DstRect: TRect;
    const ImageData: TImageData; const SrcRect: TRect);

{ Returns the first destination pixel (relative to destination rectangle) whose
  center maps to source position SrcPos or after it (relative to source
  rectangle, Scale is destination/source size). Center of destination pixel D
  maps to (D + 0.5) / Scale, so it is the smallest D >= SrcPos * Scale - 0.5.}
function FirstDstPixelFrom(SrcPos: Integer; Scale: Double): Integer;
var
  Value: Double;
begin
  Value := SrcPos * Scale - 0.5;
  Result := Trunc(Value);
  if Result < Value then
    Inc(Result);
end;

{ Draws image using DrawTile, source rectangles larger than MaxDisplayTileSize
  are split to tiles. Destination rectangle of each tile has the pixels whose
  centers map into the tile, so the tiles cover DstRect without gaps and overlaps
  (edge shared by two tiles is computed once by the same rounding) and each
  destination pixel is sampled from the same source pixel as it would be without
  tiles (within one pixel, every tile has its own scale from rounded edges).}
procedure DrawDisplayTiles(DrawTile: TDrawDisplayTileProc; Target: Pointer; const DstRect: TRect;
  const ImageData: TImageData; const SrcRect: TRect);
var
  ScaleX, ScaleY: Double;
  TileSrc, TileDst: TRect;
begin
  if (RectWidth(SrcRect) <= MaxDisplayTileSize) and (RectHeight(SrcRect) <= MaxDisplayTileSize) then
  begin
    DrawTile(Target, DstRect, ImageData, SrcRect);
    Exit;
  end;

  ScaleX := RectWidth(DstRect) / RectWidth(SrcRect);
  ScaleY := RectHeight(DstRect) / RectHeight(SrcRect);

  TileSrc.Top := SrcRect.Top;
  while TileSrc.Top < SrcRect.Bottom do
  begin
    TileSrc.Bottom := Min(SrcRect.Bottom, TileSrc.Top + MaxDisplayTileSize);
    TileDst.Top := DstRect.Top + FirstDstPixelFrom(TileSrc.Top - SrcRect.Top, ScaleY);
    TileDst.Bottom := DstRect.Top + FirstDstPixelFrom(TileSrc.Bottom - SrcRect.Top, ScaleY);

    TileSrc.Left := SrcRect.Left;
    while TileSrc.Left < SrcRect.Right do
    begin
      TileSrc.Right := Min(SrcRect.Right, TileSrc.Left + MaxDisplayTileSize);
      TileDst.Left := DstRect.Left + FirstDstPixelFrom(TileSrc.Left - SrcRect.Left, ScaleX);
      TileDst.Right := DstRect.Left + FirstDstPixelFrom(TileSrc.Right - SrcRect.Left, ScaleX);

      if (TileDst.Right > TileDst.Left) and (TileDst.Bottom > TileDst.Top) then
        DrawTile(Target, TileDst, ImageData, TileSrc);

      TileSrc.Left := TileSrc.Right;
    end;
    TileSrc.Top := TileSrc.Bottom;
  end;
end;
{$IFEND}

{$IFDEF MSWINDOWS}
{ Draws tile of image to device context given as Target.}
procedure DrawDisplayTileOnDC(Target: Pointer; const DstRect: TRect; const ImageData: TImageData;
  const SrcRect: TRect);
var
  DC: HDC;
  OldMode, Lines, SrcWidth, SrcHeight, Y: Integer;
  BitmapInfo: Windows.TBitmapInfo;
  DibBits: Pointer;
  Dib: HBITMAP;
  MemDC: HDC;
  OldObject: HGDIOBJ;
begin
  DC := HDC(Target);
  SrcWidth := RectWidth(SrcRect);
  SrcHeight := RectHeight(SrcRect);

  FillChar(BitmapInfo, SizeOf(BitmapInfo), 0);
  with BitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
    // Top-down DIB starting at the first row of SrcRect, its size stays
    // small even when the image is huge and only a part of it is drawn
    biWidth := ImageData.Width;
    biHeight := -SrcHeight;
  end;

  OldMode := Windows.SetStretchBltMode(DC, COLORONCOLOR);
  try
    Lines := Windows.StretchDIBits(DC, DstRect.Left, DstRect.Top, RectWidth(DstRect),
                                   RectHeight(DstRect), SrcRect.Left, 0, SrcWidth, SrcHeight,
                                   GetScanlinePointer(ImageData, SrcRect.Top, 4), BitmapInfo,
                                   DIB_RGB_COLORS, SRCCOPY);
    if Lines <= 0 then
    begin
      // StretchDIBits may fail on some occasions (error 487, http://support.microsoft.com/kb/269585).
      // This fallback is slower but works every time. Thanks to Sergey Galezdinov for the fix.
      // Only SrcRect part of the image is copied to the DIB section.
      BitmapInfo.bmiHeader.biWidth := SrcWidth;
      Dib := Windows.CreateDIBSection(DC, BitmapInfo, DIB_RGB_COLORS, DibBits, 0, 0);
      if Dib = 0 then
        Exit;

      try
        for Y := 0 to SrcHeight - 1 do
        begin
          Move(GetPixelPointer(ImageData, SrcRect.Left, SrcRect.Top + Y, 4)^,
               PColor32RecBuffer(DibBits)[PtrInt(Y) * SrcWidth], SrcWidth * 4);
        end;

        MemDC := Windows.CreateCompatibleDC(DC);
        OldObject := Windows.SelectObject(MemDC, Dib);
        try
          Windows.StretchBlt(DC, DstRect.Left, DstRect.Top, RectWidth(DstRect), RectHeight(DstRect),
                             MemDC, 0, 0, SrcWidth, SrcHeight, SRCCOPY);
        finally
          Windows.SelectObject(MemDC, OldObject);
          Windows.DeleteDC(MemDC);
        end;
      finally
        Windows.DeleteObject(Dib);
      end;
    end;
  finally
    Windows.SetStretchBltMode(DC, OldMode);
  end;
end;

procedure DisplayImageDataOnDC(DC: HDC; const DstRect: TRect; const ImageData: TImageData; const SrcRect: TRect);
var
  Dst, Src: TRect;
  Region: TImageData;
  Data: PImageData;
begin
  Dst := DstRect;
  Src := SrcRect;
  InitImage(Region);
  try
    Data := PrepareDisplayData(ImageData, Dst, Src, Region);
    if Data <> nil then
      DrawDisplayTiles(DrawDisplayTileOnDC, Pointer(DC), Dst, Data^, Src);
  finally
    FreeImage(Region);
  end;
end;
{$ENDIF}

{$IF Defined(LCLGTK2) or Defined(LCLGTK3)}
{ Draws tile of image to Cairo context given as Target. Cairo image
  surface is made directly over image memory, no copy is made.}
procedure DrawDisplayTileOnCairo(Target: Pointer; const DstRect: TRect; const ImageData: TImageData;
  const SrcRect: TRect);
var
  Cairo: Pcairo_t;
  Surface: Pcairo_surface_t;
begin
  Cairo := Target;
  // RGB24 format is BGRX in memory and its unused byte is ignored
  Surface := cairo_image_surface_create_for_data(GetPixelPointer(ImageData, SrcRect.Left, SrcRect.Top, 4),
                                                 CAIRO_FORMAT_RGB24, RectWidth(SrcRect),
                                                 RectHeight(SrcRect), ImageData.Width * 4);
  cairo_save(Cairo);
  try
    cairo_rectangle(Cairo, DstRect.Left, DstRect.Top, RectWidth(DstRect), RectHeight(DstRect));
    cairo_clip(Cairo);
    cairo_translate(Cairo, DstRect.Left, DstRect.Top);
    cairo_scale(Cairo, RectWidth(DstRect) / RectWidth(SrcRect), RectHeight(DstRect) / RectHeight(SrcRect));
    cairo_set_source_surface(Cairo, Surface, 0, 0);
    cairo_pattern_set_filter(cairo_get_source(Cairo), CAIRO_FILTER_NEAREST);
    cairo_paint(Cairo);
  finally
    cairo_restore(Cairo);
    cairo_surface_destroy(Surface);
  end;
end;
{$IFEND}

{$IF Defined(LCLcocoa)}
type
  TCocoaContextAccess = class(TCocoaContext);
{$IFEND}

{ Draws image in one of DirectDisplayFormats, rectangles are already clipped.}
procedure DrawDisplayData(DstCanvas: TCanvas; const DstRect: TRect; const ImageData: TImageData;
  const SrcRect: TRect);
{$IF Defined(DCC) or Defined(LCLWIN32)} // Delphi or LCL Win32
begin
  DrawDisplayTiles(DrawDisplayTileOnDC, Pointer(DstCanvas.Handle), DstRect, ImageData, SrcRect);
end;
{$ELSEIF Defined(LCLGTK2)}
var
  Context: TGtkDeviceContext;
  Cairo: Pcairo_t;
  Clip: TRect;
  ClipKind: Integer;
begin
  Context := TGtkDeviceContext(DstCanvas.Handle);
  ClipKind := WidgetSet.GetClipBox(DstCanvas.Handle, @Clip);
  if ClipKind = NULLREGION then
    Exit;

  Cairo := gdk_cairo_create(Context.Drawable);
  try
    cairo_translate(Cairo, Context.Offset.X, Context.Offset.Y);
    if ClipKind <> ERROR then
    begin
      cairo_rectangle(Cairo, Clip.Left, Clip.Top, RectWidth(Clip), RectHeight(Clip));
      cairo_clip(Cairo);
    end;
    DrawDisplayTiles(DrawDisplayTileOnCairo, Cairo, DstRect, ImageData, SrcRect);
  finally
    cairo_destroy(Cairo);
  end;
end;
{$ELSEIF Defined(LCLGTK3)}
begin
  // Cairo context of the DC has its clipping and origin already set
  DrawDisplayTiles(DrawDisplayTileOnCairo, TGtk3DeviceContext(DstCanvas.Handle).pcr, DstRect,
                   ImageData, SrcRect);
end;
{$ELSEIF Defined(LCLqt5) or Defined(LCLqt6)}
var
  QImage: TQtImage;
  Context: TQtDeviceContext;
  QSrcRect: TRect;
begin
  Context := TQtDeviceContext(DstCanvas.Handle);
  // QImage uses image memory directly starting at the first row of SrcRect, no copy is made.
  // RGB32 format is BGRX in memory and its unused byte is ignored.
  QImage := TQtImage.Create(GetScanlinePointer(ImageData, SrcRect.Top, 4), ImageData.Width,
                            RectHeight(SrcRect), ImageData.Width * 4, QImageFormat_RGB32, False);
  try
    QSrcRect := Rect(SrcRect.Left, 0, SrcRect.Right, RectHeight(SrcRect));
    QPainter_drawImage(Context.Widget, PRect(@DstRect), QImage.Handle, @QSrcRect, QtAutoColor);
  finally
    QImage.Free;
  end;
end;
{$ELSEIF Defined(LCLcocoa)}
var
  Context: TCocoaContext;
  CG: CGContextRef;
  SrcWidth, SrcHeight: Integer;
  ColorSpace: CGColorSpaceRef;
  Provider: CGDataProviderRef;
  Image: CGImageRef;
begin
  Context := TCocoaGDIUtil.CheckDC(DstCanvas.Handle);
  if Context = nil then
    Exit;

  CG := Context.CGContext;
  SrcWidth := RectWidth(SrcRect);
  SrcHeight := RectHeight(SrcRect);

  // Data provider uses image memory directly starting at the first
  // pixel of SrcRect, no copy is made
  Provider := CGDataProviderCreateWithData(nil, GetPixelPointer(ImageData, SrcRect.Left, SrcRect.Top, 4),
                                           PtrInt(SrcHeight - 1) * ImageData.Width * 4 + SrcWidth * 4, nil);
  ColorSpace := CGColorSpaceCreateDeviceRGB;
  // BGRX in memory, the unused byte is ignored
  Image := CGImageCreate(SrcWidth, SrcHeight, 8, 32, ImageData.Width * 4, ColorSpace,
                         kCGImageAlphaNoneSkipFirst or kCGBitmapByteOrder32Little, Provider,
                         nil, 0, kCGRenderingIntentDefault);
  try
    CGContextSaveGState(CG);
    try
      CGContextSetInterpolationQuality(CG, kCGInterpolationNone);
      // LCL context has Y axis going down, flip it around the destination
      // rectangle so the image is not drawn upside down
      CGContextTranslateCTM(CG, 0, DstRect.Top + DstRect.Bottom);
      CGContextScaleCTM(CG, 1, -1);
      CGContextDrawImage(CG, CGRectMake(DstRect.Left, DstRect.Top, RectWidth(DstRect),
                         RectHeight(DstRect)), Image);
    finally
      CGContextRestoreGState(CG);
    end;
  finally
    CGImageRelease(Image);
    CGColorSpaceRelease(ColorSpace);
    CGDataProviderRelease(Provider);
  end;

  // Canvas of TBitmap needs to know its pixels have changed
  TCocoaContextAccess(Context).AttachedBitmap_SetModified;
end;
{$ELSE}
var
  Region: TImageData;
  Bitmap: TBitmap;
begin
  // Slower fallback for other widget sets, only SrcRect part
  // of the image is converted to TBitmap
  InitImage(Region);
  Bitmap := TBitmap.Create;
  try
    NewImage(RectWidth(SrcRect), RectHeight(SrcRect), ImageData.Format, Region);
    CopyRect(ImageData, SrcRect.Left, SrcRect.Top, Region.Width, Region.Height, Region, 0, 0);
    // Opaque format so the unused byte of X8R8G8B8 is not taken as alpha
    ConvertImage(Region, ifR8G8B8);
    ConvertDataToBitmap(Region, Bitmap);
    DstCanvas.StretchDraw(DstRect, Bitmap);
  finally
    Bitmap.Free;
    FreeImage(Region);
  end;
end;
{$IFEND}

procedure DisplayImageData(DstCanvas: TCanvas; const DstRect: TRect; const ImageData: TImageData);
begin
  DisplayImageData(DstCanvas, DstRect, ImageData, Types.Rect(0, 0, ImageData.Width, ImageData.Height));
end;

procedure DisplayImageData(DstCanvas: TCanvas; const DstRect: TRect; const ImageData: TImageData; const SrcRect: TRect);
var
  Dst, Src: TRect;
  Region: TImageData;
  Data: PImageData;
begin
  Dst := DstRect;
  Src := SrcRect;
  InitImage(Region);
  try
    Data := PrepareDisplayData(ImageData, Dst, Src, Region);
    if Data <> nil then
      DrawDisplayData(DstCanvas, Dst, Data^, Src);
  finally
    FreeImage(Region);
  end;
end;

procedure DisplayImage(DstCanvas: TCanvas; DstX, DstY: LongInt; Image: TBaseImage);
begin
  DisplayImageData(DstCanvas, BoundsToRect(DstX, DstY, Image.Width, Image.Height),
    Image.ImageDataPointer^, Image.BoundsRect);
end;

procedure DisplayImage(DstCanvas: TCanvas; const DstRect: TRect; Image: TBaseImage);
begin
  DisplayImageData(DstCanvas, DstRect, Image.ImageDataPointer^, Image.BoundsRect);
end;

procedure DisplayImage(DstCanvas: TCanvas; const DstRect: TRect; Image: TBaseImage; const SrcRect: TRect);
begin
  DisplayImageData(DstCanvas, DstRect, Image.ImageDataPointer^, SrcRect);
end;


{ TImagingGraphic class implementation }

constructor TImagingGraphic.Create;
begin
  inherited Create;
  PixelFormat := pf24Bit;
end;

procedure TImagingGraphic.ReadData(Stream: TStream);
begin
  // Here we need to skip ReadData+WriteData of TBitmap (and LCL TRasterBitmap)
  // and go to the basics in TGraphic's ReadData+WriteData with just LoadFromStream
  // and SaveToStream.
  // Some VCL/LCL TGraphic classes also store size of the written data
  // before the stream contents. However, the stream passed here
  // from TReader.DefineBinaryProperty is already
  // a memory stream capped to the size of binary property data.
  // Picture.Data = <vaBinary><Size(TWriter)><TGraphicClassName(TPicture)><ImageBits(TImagingGraphicForSave)>
  LoadFromStream(Stream);
end;

procedure TImagingGraphic.WriteData(Stream: TStream);
begin
  // This can happen when streaming some of the formats that don't have
  // TImagingGraphicForSave descendant.
  SaveToStream(Stream);
end;

procedure TImagingGraphic.LoadFromStream(Stream: TStream);
var
  Image: TSingleImage;
begin
  Image := TSingleImage.Create;
  try
    Image.LoadFromStream(Stream);
    Assign(Image);
  finally
    Image.Free;
  end;
end;

procedure TImagingGraphic.SaveToStream(Stream: TStream);
var
  Image: TSingleImage;
begin
  Image := TSingleImage.Create;
  try
    Image.Assign(Self);
    Image.SaveToStream('png', Stream);
  finally
    Image.Free;
  end;
end;

procedure TImagingGraphic.AssignTo(Dest: TPersistent);
var
  Arr: TDynImageDataArray;
begin
  if Dest is TSingleImage then
  begin
    AssignToImage(TSingleImage(Dest))
  end
  else if Dest is TMultiImage then
  begin
    SetLength(Arr, 1);
    AssignToImageData(Arr[0]);
    TMultiImage(Dest).CreateFromArray(Arr);
    Imaging.FreeImagesInArray(Arr);
  end
  else
    inherited AssignTo(Dest);
end;

{$IFDEF COMPONENT_SET_LCL}
function TImagingGraphic.GetResourceType: TResourceType;
begin
  Result := RT_RCDATA;
end;

class function TImagingGraphic.IsStreamFormatSupported(Stream: TStream): Boolean;
begin
  Result := DetermineStreamFormat(Stream) <> '';
end;
{$ENDIF}

procedure TImagingGraphic.Assign(Source: TPersistent);
begin
  if Source is TBaseImage then
    AssignFromImage(TBaseImage(Source))
  else
    inherited Assign(Source);
end;

procedure TImagingGraphic.AssignFromImage(Image: TBaseImage);
begin
  if (Image <> nil) and Image.Valid then
    AssignFromImageData(Image.ImageDataPointer^);
end;

procedure TImagingGraphic.AssignToImage(Image: TBaseImage);
begin
  if (Image <> nil) and (Image.ImageDataPointer <> nil) then
    AssignToImageData(Image.ImageDataPointer^);
end;

procedure TImagingGraphic.AssignFromImageData(const ImageData: TImageData);
begin
  if Imaging.TestImage(ImageData) then
    ConvertDataToBitmap(ImageData, Self);
end;

procedure TImagingGraphic.AssignToImageData(var ImageData: TImageData);
begin
  Imaging.FreeImage(ImageData);
  ConvertBitmapToData(Self, ImageData);
end;

{ TImagingGraphicForSave class implementation }

constructor TImagingGraphicForSave.Create;
begin
  inherited Create;
  FDefaultFileExt := GetFileFormat.Extensions[0];
  FSavingFormat := ifUnknown;
  GetFileFormat.CheckOptionsValidity;
end;

procedure TImagingGraphicForSave.WriteData(Stream: TStream);
begin
  SaveToStream(Stream);
end;

procedure TImagingGraphicForSave.SaveToStream(Stream: TStream);
var
  Image: TSingleImage;
begin
  if FDefaultFileExt <> '' then
  begin
    Image := TSingleImage.Create;
    try
      Image.Assign(Self);
      if FSavingFormat <> ifUnknown then
        Image.Format := FSavingFormat;
      Image.SaveToStream(FDefaultFileExt, Stream);
    finally
      Image.Free;
    end;
  end;
end;

{$IFDEF COMPONENT_SET_LCL}
class function TImagingGraphicForSave.GetFileExtensions: string;
begin
  Result := StringReplace(GetFileFormat.Extensions.CommaText, ',', ';', [rfReplaceAll]);
end;

function TImagingGraphicForSave.GetMimeType: string;
begin
  Result := 'image/' + FDefaultFileExt;
end;
{$ENDIF}

{$IFNDEF DONT_LINK_BITMAP}
constructor TImagingBitmap.Create;
begin
  inherited Create;
  FUseRLE := (GetFileFormat as TBitmapFileFormat).UseRLE;
end;

class function TImagingBitmap.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TBitmapFileFormat);
end;

procedure TImagingBitmap.SaveToStream(Stream: TStream);
begin
  Imaging.PushOptions;
  Imaging.SetOption(ImagingBitmapRLE, Ord(FUseRLE));
  inherited SaveToStream(Stream);
  Imaging.PopOptions;
end;
{$ENDIF}

{$IFNDEF DONT_LINK_JPEG}
constructor TImagingJpeg.Create;
begin
  inherited Create;
  FQuality := (GetFileFormat as TJpegFileFormat).Quality;
  FProgressive := (GetFileFormat as TJpegFileFormat).Progressive;
end;

class function TImagingJpeg.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TJpegFileFormat);
end;

{$IFDEF COMPONENT_SET_LCL}
function TImagingJpeg.GetMimeType: string;
begin
  Result := 'image/jpeg';
end;
{$ENDIF}

procedure TImagingJpeg.SaveToStream(Stream: TStream);
begin
  Imaging.PushOptions;
  Imaging.SetOption(ImagingJpegQuality, FQuality);
  Imaging.SetOption(ImagingJpegProgressive, Ord(FProgressive));
  inherited SaveToStream(Stream);
  Imaging.PopOptions;
end;

{$ENDIF}

{$IFNDEF DONT_LINK_PNG}
constructor TImagingPNG.Create;
begin
  inherited Create;
  FPreFilter := (GetFileFormat as TPNGFileFormat).PreFilter;
  FCompressLevel := (GetFileFormat as TPNGFileFormat).CompressLevel;
end;

class function TImagingPNG.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TPNGFileFormat);
end;

procedure TImagingPNG.SaveToStream(Stream: TStream);
begin
  Imaging.PushOptions;
  Imaging.SetOption(ImagingPNGPreFilter, FPreFilter);
  Imaging.SetOption(ImagingPNGCompressLevel, FCompressLevel);
  inherited SaveToStream(Stream);
  Imaging.PopOptions;
end;
{$ENDIF}

{$IFNDEF DONT_LINK_GIF}
class function TImagingGIF.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TGIFFileFormat);
end;
{$ENDIF}

{$IFNDEF DONT_LINK_TARGA}
constructor TImagingTarga.Create;
begin
  inherited Create;
  FUseRLE := (GetFileFormat as TTargaFileFormat).UseRLE;
end;

class function TImagingTarga.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TTargaFileFormat);
end;

procedure TImagingTarga.SaveToStream(Stream: TStream);
begin
  Imaging.PushOptions;
  Imaging.SetOption(ImagingTargaRLE, Ord(FUseRLE));
  inherited SaveToStream(Stream);
  Imaging.PopOptions;
end;
{$ENDIF}

{$IFNDEF DONT_LINK_DDS}
constructor TImagingDDS.Create;
begin
  inherited Create;
  FCompression := dcNone;
end;

class function TImagingDDS.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TDDSFileFormat);
end;

procedure TImagingDDS.SaveToStream(Stream: TStream);
begin
  case FCompression of
    dcNone: FSavingFormat := ifUnknown;
    dcDXT1: FSavingFormat := ifDXT1;
    dcDXT3: FSavingFormat := ifDXT3;
    dcDXT5: FSavingFormat := ifDXT5;
  end;
  Imaging.PushOptions;
  Imaging.SetOption(ImagingDDSSaveCubeMap, Ord(False));
  Imaging.SetOption(ImagingDDSSaveVolume, Ord(False));
  Imaging.SetOption(ImagingDDSSaveMipMapCount, 1);
  Imaging.SetOption(ImagingDDSSaveDepth, 1);
  inherited SaveToStream(Stream);
  Imaging.PopOptions;
end;
{$ENDIF}

{$IFNDEF DONT_LINK_MNG}
constructor TImagingMNG.Create;
begin
  inherited Create;
  FLossyCompression := (GetFileFormat as TMNGFileFormat).LossyCompression;
  FLossyAlpha := (GetFileFormat as TMNGFileFormat).LossyAlpha;
  FPreFilter := (GetFileFormat as TMNGFileFormat).PreFilter;
  FCompressLevel := (GetFileFormat as TMNGFileFormat).CompressLevel;
  FQuality := (GetFileFormat as TMNGFileFormat).Quality;
  FProgressive := (GetFileFormat as TMNGFileFormat).Progressive;
end;

class function TImagingMNG.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TMNGFileFormat);
end;

{$IFDEF COMPONENT_SET_LCL}
function TImagingMNG.GetMimeType: string;
begin
  Result := 'video/mng';
end;
{$ENDIF}

procedure TImagingMNG.SaveToStream(Stream: TStream);
begin
  Imaging.PushOptions;
  Imaging.SetOption(ImagingMNGLossyCompression, Ord(FLossyCompression));
  Imaging.SetOption(ImagingMNGLossyAlpha, Ord(FLossyAlpha));
  Imaging.SetOption(ImagingMNGPreFilter, FPreFilter);
  Imaging.SetOption(ImagingMNGCompressLevel, FCompressLevel);
  Imaging.SetOption(ImagingMNGQuality, FQuality);
  Imaging.SetOption(ImagingMNGProgressive, Ord(FProgressive));
  inherited SaveToStream(Stream);
  Imaging.PopOptions;
end;
{$ENDIF}

{$IFNDEF DONT_LINK_JNG}
constructor TImagingJNG.Create;
begin
  inherited Create;
  FLossyAlpha := (GetFileFormat as TJNGFileFormat).LossyAlpha;
  FAlphaPreFilter := (GetFileFormat as TJNGFileFormat).PreFilter;
  FAlphaCompressLevel := (GetFileFormat as TJNGFileFormat).CompressLevel;
  FQuality := (GetFileFormat as TJNGFileFormat).Quality;
  FProgressive := (GetFileFormat as TJNGFileFormat).Progressive;
end;

class function TImagingJNG.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TJNGFileFormat);
end;

procedure TImagingJNG.SaveToStream(Stream: TStream);
begin
  Imaging.PushOptions;
  Imaging.SetOption(ImagingJNGLossyALpha, Ord(FLossyAlpha));
  Imaging.SetOption(ImagingJNGAlphaPreFilter, FAlphaPreFilter);
  Imaging.SetOption(ImagingJNGAlphaCompressLevel, FAlphaCompressLevel);
  Imaging.SetOption(ImagingJNGQuality, FQuality);
  Imaging.SetOption(ImagingJNGProgressive, Ord(FProgressive));
  inherited SaveToStream(Stream);
  Imaging.PopOptions;
end;
{$ENDIF}

initialization
  RegisteredFormats := TList.Create;
  RegisterTypes;
finalization
  UnRegisterTypes;
  RegisteredFormats.Free;

{$IFEND} // {$IF not Defined(COMPONENT_SET_LCL) and not Defined(COMPONENT_SET_VCL)}

{
  File Notes:

  -- 0.77.1 ---------------------------------------------------
    - Fixed bug in ConvertBitmapToData causing images from GTK2 bitmaps
      to have swapped RB channels.
    - LCL: Removed GTK1 support (deprecated).

  -- 0.26.3 Changes/Bug Fixes ---------------------------------
    - Transparency of 8bit images (like loaded from 8bit PNG or GIF) is
      kept intact during conversion to TBitmap in ConvertDataToBitmap
      (32bit bitmap is created).

  -- 0.26.3 Changes/Bug Fixes ---------------------------------
    - Setting AlphaFormat property of TBitmap in ConvertDataToBitmap
      when using Delphi 2009+.
    - Fixed garbled LCL TBitmaps created by ConvertDataToBitmap
      in Mac OS X (Carbon).

  -- 0.26.1 Changes/Bug Fixes ---------------------------------
    - Added some more IFDEFs for Lazarus widget sets.
    - Removed CLX code.
    - GTK version of Unix DisplayImageData only used with LCL GTK so the
      the rest of the unit can be used with Qt or other LCL interfaces. 
    - Fallback mechanism for DisplayImageDataOnDC, it may fail on occasions.
    - Changed file format conditional compilation to reflect changes
      in LINK symbols.
    - Lazarus 0.9.26 compatibility changes.

  -- 0.24.1 Changes/Bug Fixes ---------------------------------
    - Fixed wrong IFDEF causing that Imaging wouldn't compile in Lazarus
      with GTK2 target.
    - Added comments with code for Lazarus rev. 11861+ regarding
      RawImage interface. Replace current code with that in comments
      if you use Lazarus from SVN. New RawImage interface will be used by
      default after next Lazarus release. 

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Added TImagingGIF. 

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Uses only high level interface now (except for saving options).
    - Slightly changed class hierarchy. TImagingGraphic is now only for loading
      and base class for savers is new TImagingGraphicForSave. Also
      TImagingGraphic is now registered with all supported file formats
      by TPicture's format support.

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - added DisplayImage procedures (thanks to Paul Michell, modified)
    - removed RegisterTypes and UnRegisterTypes from interface section,
      they are called automatically
    - added procedures: ConvertImageToBitmap and ConvertBitmapToImage

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - LCL data to bitmap conversion didn't work in Linux, fixed
    - added MNG file format
    - added JNG file format

  -- 0.15 Changes/Bug Fixes -----------------------------------
    - made it LCL compatible
    - made it CLX compatible
    - added all initial stuff
}

end.

