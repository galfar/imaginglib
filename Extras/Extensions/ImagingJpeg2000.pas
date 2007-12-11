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

{ This unit contains image format loader/saver for Jpeg 2000 images.}
unit ImagingJpeg2000;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, ImagingTypes, Imaging, ImagingColors, ImagingIO, ImagingUtility,
  ImagingExtras, OpenJpeg;

type
  { Type Jpeg 2000 file (needed for OpenJPEG codec settings).}
  TJpeg2000FileType = (jtInvalid, jtJP2, jtJ2K, jtJPT);

  { Class for loading/saving Jpeg 2000 images. It uses OpenJPEG library
    compiled to object files and linked to Object Pascal program. Jpeg 2000
    supports wide variety of data formats. You can have arbitrary number
    of components/channels, each with different bitdepth and optional
    "signedness". Jpeg 2000 images can be lossy or lossless compressed.

    Imaging can load most data formats (except images
    with componenet bitdepth > 16 => no Imaging data format equivalents).
    Components with sample separation are loaded correctly, ICC profiles
    or palettes are not used, YCbCr images are translated to RGB.

    You can set various options when saving Jpeg-2000 images. Look at
    properties of TJpeg2000FileFormat for details.}
  TJpeg2000FileFormat = class(TImageFileFormat)
  protected
    FQuality: LongInt;
    FCodeStreamOnly: LongBool;
    FLosslessCompression: LongBool;
    function GetFileType(Handle: TImagingHandle): TJpeg2000FileType;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); override;
  public
    constructor Create; override;
    function TestFormat(Handle: TImagingHandle): Boolean; override;
    procedure CheckOptionsValidity; override;
  published  
    { Controls JPEG 2000 lossy compression quality. It is number in range 1..100.
      1 means small/ugly file, 100 means large/nice file. Accessible trough
      ImagingJpeg2000Quality option. Default value is 80.}
    property Quality: LongInt read FQuality write FQuality;
    { Controls whether JPEG 2000 image is saved with full file headers or just
      as code stream. Default value is False. Accessible trough
      ImagingJpeg2000CodeStreamOnly option.}
    property CodeStreamOnly: LongBool read FCodeStreamOnly write FCodeStreamOnly;
    { Specifies JPEG 2000 image compression type. If True, saved JPEG 2000 files
      will be losslessly compressed. Otherwise lossy compression is used.
      Default value is False. Accessible trough
      ImagingJpeg2000LosslessCompression option.}
    property LosslessCompression: LongBool read FLosslessCompression write FLosslessCompression;
  end;

implementation

const
  SJpeg2000FormatName = 'JPEG 2000 Image';
  SJpeg2000Masks      = '*.jp2,*.j2k,*.j2c,*.jpx,*.jpc';
  Jpeg2000SupportedFormats: TImageFormats = [ifGray8, ifGray16,
    ifA8Gray8, ifA16Gray16, ifR8G8B8, ifR16G16B16, ifA8R8G8B8, ifA16R16G16B16];
  Jpeg2000DefaultQuality = 80;
  Jpeg2000DefaultCodeStreamOnly = False;
  Jpeg2000DefaultLosslessCompression = False;

const
  JP2Signature: TChar8 = #0#0#0#$0C#$6A#$50#$20#$20;
  J2KSignature: TChar4 = #$FF#$4F#$FF#$51;

constructor TJpeg2000FileFormat.Create;
begin
  inherited Create;
  FName := SJpeg2000FormatName;
  FCanLoad := True;
  FCanSave := True;
  FIsMultiImageFormat := False;
  FSupportedFormats := Jpeg2000SupportedFormats;

  FQuality := Jpeg2000DefaultQuality;
  FCodeStreamOnly := Jpeg2000DefaultCodeStreamOnly;
  FLosslessCompression := Jpeg2000DefaultLosslessCompression;

  AddMasks(SJpeg2000Masks);
  RegisterOption(ImagingJpeg2000Quality, @FQuality);
  RegisterOption(ImagingJpeg2000CodeStreamOnly, @FCodeStreamOnly);
  RegisterOption(ImagingJpeg2000LosslessCompression, @FLosslessCompression);
end;

procedure TJpeg2000FileFormat.CheckOptionsValidity;
begin
  // Check if option values are valid
  if not (FQuality in [1..100]) then
    FQuality := Jpeg2000DefaultQuality;
end;

function TJpeg2000FileFormat.GetFileType(Handle: TImagingHandle): TJpeg2000FileType;
var
  ReadCount: LongInt;
  Id: TChar8;
begin
  Result := jtInvalid;
  with GetIO do
  begin
    ReadCount := Read(Handle, @Id, SizeOf(Id));
    if ReadCount = SizeOf(Id) then
    begin
      // Check if we have full JP2 file format or just J2K code stream
      if CompareMem(@Id, @JP2Signature, SizeOf(JP2Signature)) then
        Result := jtJP2
      else if CompareMem(@Id, @J2KSignature, SizeOf(J2KSignature)) then
        Result := jtJ2K;
    end;
    Seek(Handle, -ReadCount, smFromCurrent);
  end;
end;

function TJpeg2000FileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  FileType: TJpeg2000FileType;
  Buffer, Pix, PixUp: PByte;
  X, Y, Z, InvZ, SX, SY, WidthBytes, BufferSize, ChannelSize, Channel,
    CY, CB, CR: LongInt;
  Info: TImageFormatInfo;
  Signed: Boolean;
  Col24: PColor24Rec;
  Col48: PColor48Rec;
  dinfo: popj_dinfo_t;
  parameters: opj_dparameters_t;
  cio: popj_cio_t;
  image: popj_image_t;
  StartPos: Int64;
begin
  Result := False;
  image := nil;
  cio := nil;
  opj_set_default_decoder_parameters(@parameters);
  // Determine which codec to use
  FileType := GetFileType(Handle);
  case FileType of
    jtJP2: dinfo := opj_create_decompress(CODEC_JP2);
    jtJ2K: dinfo := opj_create_decompress(CODEC_J2K);
    jtJPT: dinfo := opj_create_decompress(CODEC_JPT);
  else
    Exit;
  end;
  // Set event manager to nil to avoid getting messages
  dinfo.event_mgr := nil;
  // Currently OpenJPEG can load images only from memory so we have to
  // preload whole input to mem buffer. Not good but no other way now.
  // At least we set stream pos to end of JP2 data after loading (we will now
  // the exact size by then).
  StartPos := GetIO.Tell(Handle);
  BufferSize := ImagingIO.GetInputSize(GetIO, Handle);
  GetMem(Buffer, BufferSize);

  SetLength(Images, 1);
  with GetIO, Images[0] do
  try
    Read(Handle, Buffer, BufferSize);
    cio := opj_cio_open(opj_common_ptr(dinfo), Buffer, BufferSize);
    opj_setup_decoder(dinfo, @parameters);
    // Decode image
    image := opj_decode(dinfo, cio);
    if image = nil then
      Exit;

    // Determine which Imaging data format to use accorsing to
    // decoded image components
    case image.numcomps of
      2: case image.comps[0].prec of
            1..8: Format := ifA8Gray8;
           9..16: Format := ifA16Gray16;
         end;
      3: case image.comps[0].prec of
            1..8: Format := ifR8G8B8;
           9..16: Format := ifR16G16B16;
         end;
      4: case image.comps[0].prec of
            1..8: Format := ifA8R8G8B8;
           9..16: Format := ifA16R16G16B16;
         end;
    else
      // There is only one component or there is more than four =>
      // just load the first one as gray
      case image.comps[0].prec of
           1..8: Format := ifGray8;
          9..16: Format := ifGray16;
         17..32: Format := ifGray32;
       end;
    end;
    // Exit if no compatible format was found
    if Format = ifUnknown then
      Exit;

    NewImage(image.x1, image.y1, Format, Images[0]);
    Info := GetFormatInfo(Format);
    ChannelSize := Info.BytesPerPixel div Info.ChannelCount;

    // Images components are stored separately in JP2, each can have
    // different dimensions, bitdepth, ...
    for Channel := 0 to Info.ChannelCount - 1 do
    begin
      // Z and InvZ are used as component indices to output image channels and
      // decoded image components. Following settings prevent later need for
      // Red/Blue switch. Alpha channel is special case, channel orders
      // are ARGB <-> ABGR (Channel at the lowest address of output image is Blue
      // where as decoded image component at the lowest index is Red).   
      Z := Channel;
      InvZ := Info.ChannelCount - 1 - Z;
      if Info.HasAlphaChannel then
      begin
        if Channel = Info.ChannelCount - 1 then
          InvZ := Z
        else
          InvZ := Info.ChannelCount - 2 - Z;
      end;
      // Signed componets must be scaled to [0, 1] (later)
      Signed := image.comps[Z].sgnd = 1;
      if (image.comps[Z].dx = 1) and (image.comps[Z].dy = 1) then
      begin
        // X and Y sample separation is 1 so just need to assign component values
        // to image pixels one by one
        Pix := @PByteArray(Bits)[InvZ * ChannelSize];
        for Y := 0 to Height - 1 do
          for X := 0 to Width - 1 do
            begin
            case ChannelSize of
              1: Pix^ := image.comps[Z].data[Y * Width + X] + Iff(Signed, $80, 0);
              2: PWord(Pix)^ := image.comps[Z].data[Y * Width + X] + Iff(Signed, $8000, 0);
              4: PLongWord(Pix)^ := image.comps[Z].data[Y * Width + X] + IffUnsigned(Signed, $80000000, 0);
            end;
            Inc(Pix, Info.BytesPerPixel);
          end;
      end
      else
      begin
        // Sample separation is active - component is sub-sampled. Real component
        // dimensions are [image.comps[Z].w * image.comps[Z].dx,
        // image.comps[Z].h * image.comps[Z].dy
        WidthBytes := Width * Info.BytesPerPixel;
        for Y := 0 to image.comps[Z].h - 1 do
        begin
          Pix := @PByteArray(Bits)[Y * image.comps[Z].dy * WidthBytes + InvZ * ChannelSize];
          for X := 0 to image.comps[Z].w - 1 do
            for SX := 0 to image.comps[Z].dx - 1 do
            begin
              // Replicate pixels on line
              case ChannelSize of
                1: Pix^ := image.comps[Z].data[Y * image.comps[Z].w + X] + Iff(Signed, $80, 0);
                2: PWord(Pix)^ := image.comps[Z].data[Y * image.comps[Z].w + X] + Iff(Signed, $8000, 0);
                4: PLongWord(Pix)^ := image.comps[Z].data[Y * image.comps[Z].w + X] + IffUnsigned(Signed, $80000000, 0);
              end;
              Inc(Pix, Info.BytesPerPixel);
            end;

          for SY := 1 to image.comps[Z].dy - 1 do
          begin
            // Replicate lines
            PixUp := @PByteArray(Bits)[Y * image.comps[Z].dy * WidthBytes + InvZ * ChannelSize];
            Pix := @PByteArray(Bits)[(Y * image.comps[Z].dy + SY) * WidthBytes + InvZ * ChannelSize];
            for X := 0 to Width - 1 do
            begin
              case ChannelSize of
                1: Pix^ := PixUp^;
                2: PWord(Pix)^ := PWord(PixUp)^;
                4: PLongWord(Pix)^ := PLongWord(PixUp)^;
              end;
              Inc(Pix, Info.BytesPerPixel);
              Inc(PixUp, Info.BytesPerPixel);
            end;
          end;
        end;
      end;
    end;

    if (Info.ChannelCount = 3) and (image.color_space = CLRSPC_SYCC) then
    begin
      // Convert image from YCbCr colorspace to RGB if needed.
      // Not exactly sure which channel is Y (OpenJpeg's fault - no "cdef" detection).
      Pix := Bits;
      if Info.BytesPerPixel = 3 then
      begin
        for X := 0 to Width * Height - 1 do
        with PColor24Rec(Pix)^ do
        begin
          CY := R;
          CB := G;
          CR := B;
          YCbCrToRGB(CY, CB, CR, R, G, B);
          Inc(Pix, Info.BytesPerPixel);
        end;
      end
      else
      begin
        for X := 0 to Width * Height - 1 do
        with PColor48Rec(Pix)^ do
        begin
          CY := R;
          CB := G;
          CR := B;
          YCbCrToRGB16(CY, CB, CR, R, G, B);
          Inc(Pix, Info.BytesPerPixel);
        end;
      end;
    end;
    // Set the input position just after end of image
    Seek(Handle, StartPos + Cardinal(cio.bp) - Cardinal(cio.start), smFromBeginning);

    Result := True;
  finally
    opj_image_destroy(image);
    opj_destroy_decompress(dinfo);
    opj_cio_close(cio);
    FreeMem(Buffer);
  end;
end;

function TJpeg2000FileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  TargetSize, Rate: Single;
  ImageToSave: TImageData;
  MustBeFreed: Boolean;
  Info: TImageFormatInfo;
  I, Z, InvZ, Channel, ChannelSize, NumPixels: LongInt;
  Pix: PByte;
  image: popj_image_t;
  cio: popj_cio_t;
  cinfo: popj_cinfo_t;
  parameters: opj_cparameters_t;
  compparams: popj_image_cmptparm_array;
begin
  Result := False;
  image := nil;
  compparams := nil;
  cinfo := nil;
  cio := nil;
  // Makes image to save compatible with Jpeg 2000 saving capabilities
  if MakeCompatible(Images[Index], ImageToSave, MustBeFreed) then
  with GetIO, ImageToSave do
  try
    Info := GetFormatInfo(Format);
    ChannelSize := Info.BytesPerPixel div Info.ChannelCount;

    // Fill component info structures and then create OpenJPEG image
    GetMem(compparams, Info.ChannelCount * SizeOf(opj_image_comptparm));
    for I := 0 to Info.ChannelCount - 1 do
    with compparams[I] do
    begin
      dx := 1;
      dy := 1;
      w  := Width;
      h  := Height;
      bpp := (Info.BytesPerPixel div Info.ChannelCount) * 8;
      prec := bpp;
      sgnd := 0;
      x0 := 0;
      y0 := 0;
    end;
    image := opj_image_create(Info.ChannelCount, @compparams[0], CLRSPC_SRGB);
    if image = nil then Exit;
    image.x1 := Width;
    image.y1 := Height;

    if FCodeStreamOnly then
      cinfo := opj_create_compress(CODEC_J2K)
    else
      cinfo := opj_create_compress(CODEC_JP2);

    // Set event manager to nil to avoid getting messages
    cinfo.event_mgr := nil;  
    // Set compression parameters based current file format properties
    opj_set_default_encoder_parameters(@parameters);
    parameters.cod_format := Iff(FCodeStreamOnly, 0, 1);
    parameters.numresolution := 6;
    parameters.tcp_numlayers := 1;
    parameters.cp_disto_alloc := 1;
    if FLosslessCompression then
    begin
      // Set rate to 0 -> lossless
      parameters.tcp_rates[0] := 0;
    end
    else
    begin
      // Quality -> Rate computation taken from ImageMagick
      Rate := 100.0 / Sqr(115 - FQuality);
      NumPixels := Width * Height * Info.BytesPerPixel;
      TargetSize := (NumPixels * Rate) + 550 + (Info.ChannelCount - 1) * 142;
      parameters.tcp_rates[0] := 1.0 / (TargetSize / NumPixels);
    end;
    // Setup encoder
    opj_setup_encoder(cinfo, @parameters, image);

    // Fill component samples in data with values taken from
    // image pixels
    for Channel := 0 to Info.ChannelCount - 1 do
    begin
      Z := Channel;
      InvZ := Info.ChannelCount - 1 - Z;
      if Info.HasAlphaChannel then
      begin
        if Channel = Info.ChannelCount - 1 then
          InvZ := Z
        else
          InvZ := Info.ChannelCount - 2 - Z;
      end;
      Pix := @PByteArray(Bits)[InvZ * ChannelSize];
      for I := 0 to Width * Height - 1 do
      begin
        case ChannelSize of
          1: image.comps[Z].data[I] := Pix^;
          2: image.comps[Z].data[I] := PWord(Pix)^;
          4: LongWord(image.comps[Z].data[I]) := PLongWord(Pix)^;
        end;
        Inc(Pix, Info.BytesPerPixel);
      end;
    end;

    // Open OpenJPEG output
    cio := opj_cio_open(opj_common_ptr(cinfo), nil, 0);
    // Try to encode the image
    if not opj_encode(cinfo, cio, image, nil) then
      Exit;
    // Finally write buffer with encoded image to output
    Write(Handle, cio.buffer, cio_tell(cio));

    Result := True;
  finally
    if MustBeFreed then
      FreeImage(ImageToSave);
    opj_destroy_compress(cinfo);
    opj_image_destroy(image);
    opj_cio_close(cio);
    FreeMem(compparams);
  end;
end;

procedure TJpeg2000FileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
var
  ConvFormat: TImageFormat;
begin
  if Info.IsFloatingPoint then
    ConvFormat := IffFormat(Info.ChannelCount = 1, ifGray16, ifA16R16G16B16)
  else if Info.HasGrayChannel then
    ConvFormat := IffFormat(Info.HasAlphaChannel, ifA16Gray16, ifGray16)
  else if Info.IsIndexed then
    ConvFormat := ifA8R8G8B8
  else if Info.BytesPerPixel div Info.ChannelCount > 1 then
    ConvFormat := IffFormat(Info.HasAlphaChannel, ifA16R16G16B16, ifR16G16B16)
  else
    ConvFormat := IffFormat(Info.HasAlphaChannel, ifA8R8G8B8, ifR8G8B8);

  ConvertImage(Image, ConvFormat);
end;

function TJpeg2000FileFormat.TestFormat(Handle: TImagingHandle): Boolean;
begin
  Result := False;
  if Handle <> nil then
    Result := GetFileType(Handle) <> jtInvalid;
end;

initialization
  RegisterImageFileFormat(TJpeg2000FileFormat);

{
  File Notes:

 -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Removed ifGray32 from supported formats, OpenJPEG crashes when saving them.
    - Added Seek after loading to set input pos to the end of image.
    - Saving added losy/lossless, quality option added.
    - Initial loading-only version created.

}

end.
