{
  $Id$
  Vampyre Imaging Library
  by Marek Mauder 
  http://imaginglib.sourceforge.net

  The contents of this file are used with permission, subject to the Mozilla
  Public License Version 1.1 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  Alternatively, the contents of this file may be used under the terms of the
  GNU Lesser General Public License (the  "LGPL License"), in which case the
  provisions of the LGPL License are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the LGPL License and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting  the provisions above and
  replace  them with the notice and other provisions required by the LGPL
  License.  If you do not delete the provisions above, a recipient may use
  your version of this file under either the MPL or the LGPL License.

  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
}

{ This unit contains VCL/CLX/LCL TGraphic descendant which uses Imaging library
  for saving and loading.}
unit ImagingComponents;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Types, Classes,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF COMPONENT_SET_VCL}
  Graphics,
{$ENDIF}
{$IFDEF COMPONENT_SET_CLX}
  Qt,
  QGraphics,
{$ENDIF}
{$IFDEF COMPONENT_SET_LCL}
  InterfaceBase,
  GraphType,
  Graphics,
  LCLType,
  LCLIntf,
{$ENDIF}
  ImagingTypes, Imaging, ImagingClasses;

type
  { Base graphic class which uses Imaging to load and save
    images. It has standard TBitmap class as ancestor and it can
    Assign also to/from TImageData structres and TBaseImage
    classes. Each descendant class can load all file formats
    supported by Imaging but save only one format (TImagingBitmap
    for *.bmp, TImagingJpeg for *.jpg).                           }
  TImagingGraphic = class(TBitmap)
  protected
    FDefaultFileExt: string;
    FSavingFormat: TImageFormat;
    procedure ReadDataFromStream(Stream: TStream); virtual;
    procedure WriteDataToStream(Stream: TStream); virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    { Loads new image from the stream. It can load all image
      file formats supported by Imaging (and enabled of course)
      even though it is called by descendant class capable of
      saving only one file format.}
    procedure LoadFromStream(Stream: TStream); override;
    { Saves the current image to the stream. It is saved in the
      file format according to the DefaultFileExt property.
      So each descendant class can save some other file format.}
    procedure SaveToStream(Stream: TStream); override;
    { Returns TImageFileFormat descendant for this graphic class.}
    class function GetFileFormat: TImageFileFormat; virtual; abstract;

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
    { Returns file extensions of this graphic class.}
    class function GetFileExtensions: string; override;
    { Returns default MIME type of this graphic class.}
    function GetDefaultMimeType: string; override;
{$ENDIF}
    { Default (the most common) file extension of this graphic class.}
    property DefaultFileExt: string read FDefaultFileExt;
  end;

  TImagingGraphicClass = class of TImagingGraphic;

{$IFDEF LINK_BITMAP}
  { TImagingGraphic descendant for loading/saving Windows bitmaps.
    VCL/CLX/LCL all have native support for bitmaps so you might
    want to disable this class (although you can save bitmaps with
    RLE compression with this class).}
  TImagingBitmap = class(TImagingGraphic)
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

{$IFDEF LINK_JPEG}
  { TImagingGraphic descendant for loading/saving JPEG images.}
  TImagingJpeg = class(TImagingGraphic)
  protected
    FQuality: LongInt;
    FProgressive: Boolean;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
{$IFDEF COMPONENT_SET_LCL}
    function GetDefaultMimeType: string; override;
{$ENDIF}
    { See ImagingJpegQuality option for details.}
    property Quality: LongInt read FQuality write FQuality;
    { See ImagingJpegProgressive option for details.}
    property Progressive: Boolean read FProgressive write FProgressive;
  end;
{$ENDIF}

{$IFDEF LINK_PNG}
  { TImagingGraphic descendant for loading/saving PNG images.}
  TImagingPNG = class(TImagingGraphic)
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

{$IFDEF LINK_TARGA}
  { TImagingGraphic descendant for loading/saving Targa images.}
  TImagingTarga = class(TImagingGraphic)
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

{$IFDEF LINK_DDS}
  { Compresssion type used when saving DDS files by TImagingDds.}
  TDDSCompresion = (dcNone, dcDXT1, dcDXT3, dcDXT5);

  { TImagingGraphic descendant for loading/saving DDS images.}
  TImagingDDS = class(TImagingGraphic)
  protected
    FCompression: TDDSCompresion;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetFileFormat: TImageFileFormat; override;
    { You can choose compression type used when saving DDS file.
      dcNone means that file will be saved in the current bitmaps pixel format.}
    property Compression: TDDSCompresion read FCompression write FCompression;
  end;
{$ENDIF}

{$IFDEF LINK_MNG}
  { TImagingGraphic descendant for loading/saving MNG images.}
  TImagingMNG = class(TImagingGraphic)
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
    function GetDefaultMimeType: string; override;
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

{$IFDEF LINK_JNG}
  { TImagingGraphic descendant for loading/saving JNG images.}
  TImagingJNG = class(TImagingGraphic)
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

{ Displays image stored in TImageData structure onto TCanvas. This procedure
  draws image without converting from Imaging format to TBitmap.
  Only [ifA8R8G8B8, ifX8R8G8B8] image formats are supported. Use this
  when you want displaying images that change frequently (because converting to
  TBitmap by ConvertImageDataToBitmap is generally slow). Dest and Src
  rectangles represent coordinates in the form (X1, Y1, X2, Y2).}
procedure DisplayImageData(DstCanvas: TCanvas; const DstRect: TRect; const ImageData: TImageData; const SrcRect: TRect);
{ Displays image onto TCanvas at position [DstX, DstY]. This procedure
  draws image without converting from Imaging format to TBitmap.
  Only [ifA8R8G8B8, ifX8R8G8B8] image formats are supported. Use this
  when you want displaying images that change frequently (because converting to
  TBitmap by ConvertImageDataToBitmap is generally slow).}
procedure DisplayImage(DstCanvas: TCanvas; DstX, DstY: LongInt; Image: TBaseImage); overload;
{ Displays image onto TCanvas to rectangle DstRect. This procedure
  draws image without converting from Imaging format to TBitmap.
  Only [ifA8R8G8B8, ifX8R8G8B8] image formats are supported. Use this
  when you want displaying images that change frequently (because converting to
  TBitmap by ConvertImageDataToBitmap is generally slow).}
procedure DisplayImage(DstCanvas: TCanvas; const DstRect: TRect; Image: TBaseImage); overload;
{ Displays part of the image specified by SrcRect onto TCanvas to rectangle DstRect.
  This procedure draws image without converting from Imaging format to TBitmap.
  Only [ifA8R8G8B8, ifX8R8G8B8] image formats are supported. Use this
  when you want displaying images that change frequently (because converting to
  TBitmap by ConvertImageDataToBitmap is generally slow).}
procedure DisplayImage(DstCanvas: TCanvas; const DstRect: TRect; Image: TBaseImage; const SrcRect: TRect); overload;

{$IFDEF MSWINDOWS}
{ Displays image stored in TImageData structure onto Windows device context.
  Behaviour is the same as of DisplayImageData.}
procedure DisplayImageDataOnDC(DC: HDC; const DstRect: TRect; const ImageData: TImageData; const SrcRect: TRect);
{$ENDIF}


implementation

uses
{$IF Defined(UNIX) and Defined(COMPONENT_SET_LCL)}
  {$IFDEF GTK2}
    GLib2, GDK2, GTK2, GTKDef, GTKProc,
  {$ELSE}
    GDK, GTK, GTKDef, GTKProc,
  {$ENDIF}
{$IFEND}
{$IFDEF LINK_BITMAP}
  ImagingBitmap,
{$ENDIF}
{$IFDEF LINK_JPEG}
  ImagingJpeg,
{$ENDIF}
{$IFDEF LINK_TARGA}
  ImagingTarga,
{$ENDIF}
{$IFDEF LINK_DDS}
  ImagingDds,
{$ENDIF}
{$IF Defined(LINK_PNG) or Defined(LINK_MNG) or Defined(LINK_JNG)}
  ImagingNetworkGraphics,
{$IFEND}
  ImagingUtility;

resourcestring
  SBadFormatDataToBitmap = 'Cannot find compatible bitmap format for image %s';
  SBadFormatBitmapToData = 'Cannot find compatible data format for bitmap %p';
  SBadFormatDisplay = 'Unsupported image format passed';

{ Registers types to VCL/CLX/LCL.}
procedure RegisterTypes;

  procedure RegisterFileFormat(AClass: TImagingGraphicClass);
  var
    Inst: TImageFileFormat;
    I: LongInt;
  begin
    Inst := AClass.GetFileFormat;
    if Inst <> nil then
      for I := 0 to Inst.Extensions.Count - 1 do
        TPicture.RegisterFileFormat(Inst.Extensions[I], Inst.Name, AClass);
  end;

begin
{$IFDEF LINK_TARGA}
  RegisterFileFormat(TImagingTarga);
  {$IFNDEF COMPONENT_SET_CLX}Classes.RegisterClass(TImagingTarga);{$ENDIF}
{$ENDIF}
{$IFDEF LINK_DDS}
  RegisterFileFormat(TImagingDDS);
  {$IFNDEF COMPONENT_SET_CLX}Classes.RegisterClass(TImagingDDS);{$ENDIF}
{$ENDIF}
{$IFDEF LINK_JNG}
  RegisterFileFormat(TImagingJNG);
  {$IFNDEF COMPONENT_SET_CLX}Classes.RegisterClass(TImagingJNG);{$ENDIF}
{$ENDIF}
{$IFDEF LINK_MNG}
  RegisterFileFormat(TImagingMNG);
  {$IFNDEF COMPONENT_SET_CLX}Classes.RegisterClass(TImagingMNG);{$ENDIF}
{$ENDIF}
{$IFDEF LINK_PNG}
  {$IFDEF COMPONENT_SET_LCL}
    // Unregister Lazarus´ default PNG loader which crashes on some PNG files
    TPicture.UnregisterGraphicClass(TPortableNetworkGraphic);
  {$ENDIF}
  RegisterFileFormat(TImagingPNG);
  {$IFNDEF COMPONENT_SET_CLX}Classes.RegisterClass(TImagingPNG);{$ENDIF}
{$ENDIF}
{$IFDEF LINK_JPEG}
  RegisterFileFormat(TImagingJpeg);
  {$IFNDEF COMPONENT_SET_CLX}Classes.RegisterClass(TImagingJpeg);{$ENDIF}
{$ENDIF}
{$IFDEF LINK_BITMAP}
  RegisterFileFormat(TImagingBitmap);
  {$IFNDEF COMPONENT_SET_CLX}Classes.RegisterClass(TImagingBitmap);{$ENDIF}
{$ENDIF}
end;

{ Unregisters types from VCL/CLX/LCL.}
procedure UnRegisterTypes;
begin
{$IFDEF LINK_BITMAP}
  TPicture.UnregisterGraphicClass(TImagingBitmap);
  {$IFNDEF COMPONENT_SET_CLX}Classes.UnRegisterClass(TImagingBitmap);{$ENDIF}
{$ENDIF}
{$IFDEF LINK_JPEG}
  TPicture.UnregisterGraphicClass(TImagingJpeg);
  {$IFNDEF COMPONENT_SET_CLX}Classes.UnRegisterClass(TImagingJpeg);{$ENDIF}
{$ENDIF}
{$IFDEF LINK_PNG}
  TPicture.UnregisterGraphicClass(TImagingPNG);
  {$IFNDEF COMPONENT_SET_CLX}Classes.UnRegisterClass(TImagingPNG);{$ENDIF}
{$ENDIF}
{$IFDEF LINK_TARGA}
  TPicture.UnregisterGraphicClass(TImagingTarga);
  {$IFNDEF COMPONENT_SET_CLX}Classes.UnRegisterClass(TImagingTarga);{$ENDIF}
{$ENDIF}
{$IFDEF LINK_DDS}
  TPicture.UnregisterGraphicClass(TImagingDDS);
  {$IFNDEF COMPONENT_SET_CLX}Classes.UnRegisterClass(TImagingDDS);{$ENDIF}
{$ENDIF}
end;

function DataFormatToPixelFormat(Format: TImageFormat): TPixelFormat;
begin
  case Format of
{$IFNDEF COMPONENT_SET_LCL}
    ifIndex8: Result := pf8bit;
{$ENDIF}
{$IF (not Defined(COMPONENT_SET_CLX)) and (not Defined(COMPONENT_SET_LCL))}
    ifR5G6B5: Result := pf16bit;
    ifR8G8B8: Result := pf24bit;
{$IFEND}
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
{$IFNDEF COMPONENT_SET_CLX}
    pf15bit: Result := ifA1R5G5B5;
    pf16bit: Result := ifR5G6B5;
    pf24bit: Result := ifR8G8B8;
{$ENDIF}
    pf32bit: Result := ifA8R8G8B8;
  else
    Result := ifUnknown;
  end;
end;

procedure ConvertDataToBitmap(const Data: TImageData; Bitmap: TBitmap);
var
  I, LineBytes: LongInt;
  PF: TPixelFormat;
  Info: TImageFormatInfo;
  WorkData: TImageData;
{$IFDEF COMPONENT_SET_VCL}
  LogPalette: TMaxLogPalette;
{$ENDIF}
{$IFDEF COMPONENT_SET_CLX}
  ColorTable: PPalette32;
{$ENDIF}
{$IFDEF COMPONENT_SET_LCL}
  RawImage: TRawImage;
  ImgHandle, ImgMaskHandle: HBitmap;
{$ENDIF}
begin
  PF := DataFormatToPixelFormat(Data.Format);
  GetImageFormatInfo(Data.Format, Info);
  if PF = pfCustom then
  begin
    // convert from formats not supported by Graphics unit
    Imaging.InitImage(WorkData);
    Imaging.CloneImage(Data, WorkData);
    if Info.IsFloatingPoint or Info.HasAlphaChannel or Info.IsSpecial then
      Imaging.ConvertImage(WorkData, ifA8R8G8B8)
    else
{$IFNDEF COMPONENT_SET_LCL}
      if Info.IsIndexed or Info.HasGrayChannel then
        Imaging.ConvertImage(WorkData, ifIndex8)
      else
{$ENDIF}
{$IF (not Defined(COMPONENT_SET_CLX)) and (not Defined(COMPONENT_SET_LCL))}
        if Info.UsePixelFormat then
          Imaging.ConvertImage(WorkData, ifR5G6B5)
        else
          Imaging.ConvertImage(WorkData, ifR8G8B8);
{$ELSE}
        Imaging.ConvertImage(WorkData, ifA8R8G8B8);
{$IFEND}

    PF := DataFormatToPixelFormat(WorkData.Format);
    GetImageFormatInfo(WorkData.Format, Info);
  end
  else
    WorkData := Data;
    
  if PF = pfCustom then
    RaiseImaging(SBadFormatDataToBitmap, [ImageToStr(WorkData)]);
      
  LineBytes := WorkData.Width * Info.BytesPerPixel;

{$IFDEF COMPONENT_SET_VCL}
  Bitmap.Width := WorkData.Width;
  Bitmap.Height := WorkData.Height;
  Bitmap.PixelFormat := PF;

  if (PF = pf8bit) and (WorkData.Palette <> nil) then
  begin
    // copy palette, this must be done before copying bits
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
  // copy scanlines
  for I := 0 to WorkData.Height - 1 do
    Move(PByteArray(WorkData.Bits)[I * LineBytes], Bitmap.Scanline[I]^, LineBytes);
{$ENDIF}
{$IFDEF COMPONENT_SET_CLX}
  Bitmap.Width := WorkData.Width;
  Bitmap.Height := WorkData.Height;
  Bitmap.PixelFormat := PF;

  if (PF = pf8bit) and (WorkData.Palette <> nil) then
  begin
    // copy palette
    ColorTable := Bitmap.ColorTable;
    for I := 0 to Info.PaletteEntries - 1 do
    with ColorTable[I] do
    begin
      R := WorkData.Palette[I].R;
      G := WorkData.Palette[I].G;
      B := WorkData.Palette[I].B;
    end;
  end;
  // copy scanlines
  for I := 0 to WorkData.Height - 1 do
    Move(PByteArray(WorkData.Bits)[I * LineBytes], Bitmap.Scanline[I]^, LineBytes);
{$ENDIF}
{$IFDEF COMPONENT_SET_LCL}
  // create 32bit raw image from image data
  FillChar(RawImage, SizeOf(RawImage), 0);
  with RawImage.Description do
  begin
    Width := WorkData.Width;
    Height := WorkData.Height;
    BitsPerPixel := Info.BytesPerPixel * 8;
    Format := ricfRGBA;
    LineEnd := rileByteBoundary;
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
    Depth := 24;
  end;
  RawImage.Data := WorkData.Bits;
  RawImage.DataSize := WorkData.Size;

  // create bitmap from raw image
  if CreateBitmapFromRawImage(RawImage, ImgHandle, ImgMaskHandle, False) then
  begin
    Bitmap.Handle := ImgHandle;
    Bitmap.MaskHandle := ImgMaskHandle;
  end;
{$ENDIF}
  if WorkData.Bits <> Data.Bits then
    Imaging.FreeImage(WorkData);
end;

procedure ConvertBitmapToData(Bitmap: TBitmap; var Data: TImageData);
var
  I, LineBytes: LongInt;
  Format: TImageFormat;
  Info: TImageFormatInfo;
{$IFDEF COMPONENT_SET_VCL}
  Colors: Word;
  LogPalette: TMaxLogPalette;
{$ENDIF}
{$IFDEF COMPONENT_SET_CLX}
  ColorTable: PPalette32;
{$ENDIF}
{$IFDEF COMPONENT_SET_LCL}
  RawImage: TRawImage;
  LineLazBytes: LongInt;
{$ENDIF}
begin
{$IFDEF COMPONENT_SET_LCL}
  // In the current Lazarus 0.9.10 Bitmap.PixelFormat property is useless.
  // We cannot change bitmap's format by changing it (it will just release
  // old image but not convert it to new format) nor we can determine bitmaps's
  // current format (it is usually set to pfDevice). So bitmap's format is obtained
  // trough RawImage api and cannot be changed to mirror some Imaging format
  // (so formats with no coresponding Imaging format cannot be saved now).
  if GetBitmapRawImageDescription(Bitmap.Handle, @RawImage.Description) then
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
    else
      Format := ifUnknown;
    end;
{$ELSE}
  Format := PixelFormatToDataFormat(Bitmap.PixelFormat);
  if Format = ifUnknown then
  begin
    // convert from formats not supported by Imaging (1/4 bit)
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
    // copy palette
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
  // copy scanlines
  for I := 0 to Data.Height - 1 do
    Move(Bitmap.ScanLine[I]^, PByteArray(Data.Bits)[I * LineBytes], LineBytes);
{$ENDIF}
{$IFDEF COMPONENT_SET_CLX}
  if Format = ifIndex8 then
  begin
    // copy palette
    ColorTable := Bitmap.ColorTable;
    for I := 0 to Info.PaletteEntries - 1 do
    with ColorTable[I] do
    begin
      Data.Palette[I].A := $FF;
      Data.Palette[I].R := R;
      Data.Palette[I].G := G;
      Data.Palette[I].B := B;
    end;
  end;
  // copy scanlines
  for I := 0 to Data.Height - 1 do
    Move(Bitmap.ScanLine[I]^, PByteArray(Data.Bits)[I * LineBytes], LineBytes);
{$ENDIF}
{$IFDEF COMPONENT_SET_LCL}
  // get raw image from bitmap (mask handle must be 0 or expect violations)
  if GetRawImageFromBitmap(Bitmap.Handle, 0, Classes.Rect(0, 0, Data.Width, Data.Height), RawImage) then
  begin
    LineLazBytes := GetBytesPerLine(Data.Width, RawImage.Description.BitsPerPixel,
      RawImage.Description.LineEnd);
    // copy scanlines
    for I := 0 to Data.Height - 1 do
      Move(PByteArray(RawImage.Data)[I * LineLazBytes],
        PByteArray(Data.Bits)[I * LineBytes], LineBytes);
    FreeRawImageData(@RawImage);
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

{$IFDEF MSWINDOWS}
procedure DisplayImageDataOnDC(DC: HDC; const DstRect: TRect; const ImageData: TImageData; const SrcRect: TRect);
var
  OldMode: Integer;
  BitmapInfo: Windows.TBitmapInfo;
begin
  if TestImage(ImageData) then
  begin
    Assert(ImageData.Format in [ifA8R8G8B8, ifX8R8G8B8], SBadFormatDisplay);
    OldMode := Windows.SetStretchBltMode(DC, COLORONCOLOR);

    with BitmapInfo.bmiHeader do
    begin
      biSize := SizeOf(TBitmapInfoHeader);
      biPlanes := 1;
      biBitCount := 32;
      biCompression := BI_RGB;
      biWidth := ImageData.Width;
      biHeight := -ImageData.Height;
      biSizeImage := ImageData.Size;
      biXPelsPerMeter := 0;
      biYPelsPerMeter := 0;
      biClrUsed := 0;
      biClrImportant := 0;
    end;

    try
      with SrcRect, ImageData do
        Windows.StretchDIBits(DC, DstRect.Left, DstRect.Top,
          DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top, Left,
          Top, Right - Left, Bottom - Top, Bits, BitmapInfo, DIB_RGB_COLORS, SRCCOPY);
    finally
      Windows.SetStretchBltMode(DC, OldMode);
    end;
  end;  
end;
{$ENDIF}

procedure DisplayImageData(DstCanvas: TCanvas; const DstRect: TRect; const ImageData: TImageData; const SrcRect: TRect);
{$IF Defined(MSWINDOWS) and not Defined(COMPONENT_SET_CLX)}
begin
  DisplayImageDataOnDC(DstCanvas.Handle, DstRect, ImageData, SrcRect);
end;
{$ELSEIF Defined(COMPONENT_SET_CLX)}
var
  Bitmap: TBitmap;
  //Handle: LongWord;
begin
  (*
  // It would be nice if this worked:
  DstCanvas.Start;
  Handle := QPainter_handle(DstCanvas.Handle);
  {$IFDEF MSWINDOWS}
  DisplayImageDataOnDC(Handle, DstRect, ImageData, SrcRect);
  {$ELSE}
  DisplayImageDataOnX(Handle, DstRect, ImageData, SrcRect);
  {$ENDIF}
  DstCanvas.Stop;
  *)
  Bitmap := TBitmap.Create;
  try
    ConvertDataToBitmap(ImageData, Bitmap);
    DstCanvas.CopyRect(DstRect, Bitmap.Canvas, SrcRect);
  finally
    Bitmap.Free;
  end;  
end;
{$ELSEIF Defined(UNIX) and Defined(COMPONENT_SET_LCL)}

  procedure GDKDrawBitmap(Dest: HDC; DstX, DstY: Integer; SrcX, SrcY,
    SrcWidth, SrcHeight: Integer; ImageData: TImageData);
  var
    P: TPoint;
  begin
    P := GetDCOffset(TDeviceContext(Dest));
    Inc(DstX, P.X);
    Inc(DstY, P.Y);
    gdk_draw_rgb_32_image(TDeviceContext(Dest).Drawable, TDeviceContext(Dest).GC,
      DstX, DstY, SrcWidth, SrcHeight, GDK_RGB_DITHER_NONE,
      @PLongWordArray(ImageData.Bits)[SrcY * ImageData.Width + SrcX], ImageData.Width * 4);
  end;
  
var
  DisplayImage: TImageData;
  NewWidth, NewHeight: Integer;
  SrcBounds, DstBounds, DstClip: TRect;
begin
  if TestImage(ImageData) then
  begin
    Assert(ImageData.Format in [ifA8R8G8B8, ifX8R8G8B8], SBadFormatDisplay);
    InitImage(DisplayImage);

    SrcBounds := RectToBounds(SrcRect);
    DstBounds := RectToBounds(DstRect);
    WidgetSet.GetClipBox(DstCanvas.Handle, @DstClip);

    ClipStretchBounds(SrcBounds.Left, SrcBounds.Top, SrcBounds.Right, SrcBounds.Bottom,
      DstBounds.Left, DstBounds.Top, DstBounds.Right, DstBounds.Bottom, ImageData.Width,
      ImageData.Height, DstClip);

    NewWidth := DstBounds.Right;
    NewHeight := DstBounds.Bottom;

    if (NewWidth > 0) and (NewHeight > 0) then
    begin
      if (SrcBounds.Right = NewWidth) and (SrcBounds.Bottom = NewHeight) then
      try
        CloneImage(ImageData, DisplayImage);
        // Swap R-B channels for GTK display compatability!
        SwapChannels(DisplayImage, ChannelRed, ChannelBlue);
        GDKDrawBitmap(DstCanvas.Handle, DstBounds.Left, DstBounds.Top,
          SrcBounds.Left, SrcBounds.Top, NewWidth, NewHeight, DisplayImage);
      finally
        FreeImage(DisplayImage);
      end
      else
      try
        // Create new image with desired dimensions
        NewImage(NewWidth, NewHeight, ImageData.Format, DisplayImage);
        // Stretch pixels from old image to new one  TResizeFilter = (rfNearest, rfBilinear, rfBicubic);
        StretchRect(ImageData, SrcBounds.Left, SrcBounds.Top, SrcBounds.Right,
          SrcBounds.Bottom, DisplayImage, 0, 0, NewWidth, NewHeight, rfNearest);
        // Swap R-B channels for GTK display compatability!
        SwapChannels(DisplayImage, ChannelRed, ChannelBlue);
        GDKDrawBitmap(DstCanvas.Handle, DstBounds.Left, DstBounds.Top, 0, 0,
          NewWidth, NewHeight, DisplayImage);
       finally
        FreeImage(DisplayImage);
      end
    end;
  end;
end;
{$IFEND}

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
  FDefaultFileExt := GetFileFormat.Extensions[0];
  FSavingFormat := ifUnknown;
end;

procedure TImagingGraphic.SaveToStream(Stream: TStream);
begin
  WriteDataToStream(Stream);
end;

procedure TImagingGraphic.LoadFromStream(Stream: TStream);
begin
  ReadDataFromStream(Stream);
end;

destructor TImagingGraphic.Destroy;
begin
  inherited Destroy;
end;

procedure TImagingGraphic.ReadDataFromStream(Stream: TStream);
var
  Data: TImageData;
begin
  Imaging.InitImage(Data);
  if Imaging.LoadImageFromStream(Stream, Data) then
  try
    AssignFromImageData(Data);
  finally
    Imaging.FreeImage(Data);
  end;
end;

procedure TImagingGraphic.WriteDataToStream(Stream: TStream);
var
  Data: TImageData;
begin
  Imaging.InitImage(Data);
  try
    AssignToImageData(Data);
    if FSavingFormat <> ifUnknown then
      Imaging.ConvertImage(Data, FSavingFormat);
    Imaging.SaveImageToStream(FDefaultFileExt, Stream, Data);
  finally
    Imaging.FreeImage(Data);
  end;
end;

procedure TImagingGraphic.AssignTo(Dest: TPersistent);
var
  Arr: TDynImageDataArray;
begin
  if Dest is TSingleImage then
    AssignToImage(TSingleImage(Dest))
  else
  if Dest is TMultiImage then
  begin
    SetLength(Arr, 1);
    AssignToImageData(Arr[0]);
    TMultiImage(Dest).CreateFromArray(Arr);
    Imaging.FreeImagesInArray(Arr);
  end
  else
    inherited AssignTo(Dest);
end;

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

{$IFDEF COMPONENT_SET_LCL}
class function TImagingGraphic.GetFileExtensions: string;
begin
  Result := StringReplace(GetFileFormat.Extensions.CommaText, ',', ';', [rfReplaceAll]);
end;

function TImagingGraphic.GetDefaultMimeType: string;
begin
  Result := 'image/' + FDefaultFileExt;
end;
{$ENDIF}

{$IFDEF LINK_BITMAP}

{ TImagingBitmap class implementation }

constructor TImagingBitmap.Create;
begin
  inherited Create;
  FUseRLE := BitmapDefaultRLE;
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

{$IFDEF LINK_JPEG}

{ TImagingJpeg class implementation }

constructor TImagingJpeg.Create;
begin
  inherited Create;
  FQuality := JpegDefaultQuality;
  FProgressive := JpegDefaultProgressive;
end;

class function TImagingJpeg.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TJpegFileFormat);
end;

{$IFDEF COMPONENT_SET_LCL}
function TImagingJpeg.GetDefaultMimeType: string;
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

{$IFDEF LINK_PNG}

{ TImagingPNG class implementation }

constructor TImagingPNG.Create;
begin
  inherited Create;
  FPreFilter := NGDefaultPreFilter;
  FCompressLevel := NGDefaultCompressLevel;
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

{$IFDEF LINK_TARGA}

{ TImagingTarga class implementation }

constructor TImagingTarga.Create;
begin
  inherited Create;
  FUseRLE := TargaDefaultRLE;
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

{$IFDEF LINK_DDS}

{ TImagingDDS class implementation }

constructor TImagingDDS.Create;
begin
  inherited Create;
  FCompression := dcNone;
end;

class function TImagingDDS.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TDdsFileFormat);
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

{$IFDEF LINK_MNG}

{ TImagingMNG class implementation }

constructor TImagingMNG.Create;
begin
  inherited Create;
  FLossyCompression := NGDefaultLossyCompression;
  FLossyAlpha := NGDefaultLossyAlpha;
  FPreFilter := NGDefaultPreFilter;
  FCompressLevel := NGDefaultCompressLevel;
  FQuality := NGDefaultQuality;
  FProgressive := NGDefaultProgressive;
end;

class function TImagingMNG.GetFileFormat: TImageFileFormat;
begin
  Result := FindImageFileFormatByClass(TMNGFileFormat);
end;

{$IFDEF COMPONENT_SET_LCL}
function TImagingMNG.GetDefaultMimeType: string;
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

{$IFDEF LINK_JNG}

{ TImagingJNG class implementation }

constructor TImagingJNG.Create;
begin
  inherited Create;
  FLossyAlpha := NGDefaultLossyAlpha;
  FAlphaPreFilter := NGDefaultPreFilter;
  FAlphaCompressLevel := NGDefaultCompressLevel;
  FQuality := NGDefaultQuality;
  FProgressive := NGDefaultProgressive;
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
  RegisterTypes;
finalization
  UnRegisterTypes;

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - test all, check loading more images from single stream
    - add filtered resize to TImagingGraphic

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - added DisplayImage procedures (thanks to Paul Michell, modified)
    - removed RegisterTypes and UnRegisterTypes from interface section,
      they are called automatically
    - added procedures: ConvertImageToBitmap and ConvertBitmapToImage

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - LCL data to bitmap conversion didn´t work in Linux, fixed
    - added MNG file format
    - added JNG file format

  -- 0.15 Changes/Bug Fixes -----------------------------------
    - made it LCL compatible
    - made it CLX compatible
    - added all initial stuff
}

end.

