{ 
  This unit contains image format loader for textures in NIF model files.
  Works for NIF version 3 (StarTrek Bridge Commander, ...).
  Author: Delfi
}

unit ImagingNIF;

{$I ImagingOptions.inc}

interface

uses
  ImagingTypes, Imaging, ImagingFormats, ImagingUtility;

type
  { Class for loading and saving NIF images. It can load 24 bit RGB and 32 bit RGBA images}
  TNIFFileFormat = class(TImageFileFormat)
  protected
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); override;
  public
    constructor Create; override;
    function TestFormat(Handle: TImagingHandle): Boolean; override;
  published  
  end;

implementation

const
  SNIFFormatName = 'NetImmerse Image';
  SNIFMasks      = '*.nif';
  NIFSupportedFormats: TImageFormats = [ifR8G8B8, ifA8R8G8B8];

type
  { NIF file header.}
  TNIFHeader = packed record
    Width: LongWord;
    Height: LongWord;
    PixelFmt: LongWord;
  end;

{ TNIFFileFormat class implementation }

constructor TNIFFileFormat.Create;
begin
  inherited Create;
  FName := SNIFFormatName;
  FCanLoad := True;
  FCanSave := False;
  FIsMultiImageFormat := False;
  FSupportedFormats := NIFSupportedFormats;

  AddMasks(SNIFMasks);
end;

function TNIFFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Hdr: TNIFHeader;
  I, PSize, PalSize: LongWord;
  Pal: Pointer;
  FmtInfo: TImageFormatInfo;
  WordValue: Word;
begin
  SetLength(Images, 1);
  with GetIO, Images[0] do
  begin
    // Read NIF header

    Seek(Handle, 170, smFromBeginning);

    Read(Handle, @Hdr.Width, SizeOf(Hdr.Width));
    Read(Handle, @Hdr.Height, SizeOf(Hdr.Height));
    Read(Handle, @Hdr.PixelFmt, SizeOf(Hdr.PixelFmt));

    Seek(Handle, 182, smFromBeginning);

    // Determine image format
    Format := ifR8G8B8;

    if Hdr.PixelFmt = 2 then
      Format := ifA8R8G8B8;

    NewImage(Hdr.Width, Hdr.Height, Format, Images[0]);
    FmtInfo := GetFormatInfo(Format);

    Read(Handle, Bits, Size);

    SwapChannels(Images[0], ChannelRed, ChannelBlue);

    Result := True;
  end;
end;

function TNIFFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
begin
// not impl.
end;

procedure TNIFFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
var
  ConvFormat: TImageFormat;
begin
  if Info.HasGrayChannel then
    // Convert all grayscale images to Gray8 (preserve alpha of AxGrayx formats)
    ConvFormat := IffFormat(not Info.HasAlphaChannel, ifGray8, ifA8R8G8B8)
  else if Info.IsIndexed then
    // Convert all indexed images to Index8
    ConvFormat := ifIndex8
  else if Info.HasAlphaChannel then
    // Convert images with alpha channel to A8R8G8B8
    ConvFormat := ifA8R8G8B8
  else if Info.UsePixelFormat then
    // Convert 16bit images (without alpha channel) to A1R5G5B5
    ConvFormat := ifA1R5G5B5
  else
    // Convert all other formats to R8G8B8
    ConvFormat := ifR8G8B8;

  ConvertImage(Image, ConvFormat);
end;

function TNIFFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Hdr: longword;
  ReadCount: LongInt;
begin
  Result := False;
  if Handle <> nil then
  begin
    ReadCount := GetIO.Read(Handle, @Hdr, SizeOf(Hdr));
    if Hdr = 1232364878 then Result := True;
    GetIO.Seek(Handle, -ReadCount, smFromCurrent);
  end;
end;

initialization
  RegisterImageFileFormat(TNIFFileFormat);

end.

