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

{ This unit contains image format loader/saver for WebP images.
  WebP codec used for the actual encoding and decoding is selected
  in ImagingLibWebP unit. }
unit ImagingWebP;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, ImagingTypes, Imaging, ImagingUtility;

type
  { Class for loading and saving WebP images (lossy and lossless, with or
    without alpha channel).
    Works when libwebp shared library is found or when the Pascal codec is
    compiled in (FPC, Delphi 2009+).
    Animations are loaded as one image for each frame with delays of frames
    and number of loops in metadata (SMetaFrameDelay, SMetaAnimationLoops);
    more images are saved as an animation. See ImagingWebPLoadAnimated option.
    Note that WebP has a maximum size of 16383 x 16383 pixel.}
  TWebPFileFormat = class(TImageFileFormat)
  protected
    FQuality: LongInt;
    FLossless: LongBool;
    FEffort: LongInt;
    FLoadAnimated: LongBool;
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
    function GetConversionFormat(const Info: TImageFormatInfo): TImageFormat; override;
  public
    function TestFormat(Handle: TImagingHandle): Boolean; override;
    procedure CheckOptionsValidity; override;
    { Updates loading and saving capabilities according to currently
      available WebP backend - can call it again after loading libwebp from
      a custom location (TryLoadWebPLibrary in ImagingLibWebP unit). }
    procedure RefreshBackend;
  published
    { Quality of lossy compression used when saving in range 0..100,
      higher value means larger file with better quality.
      Accessible trough ImagingWebPQuality option. }
    property Quality: LongInt read FQuality write FQuality;
    { If True images are saved using lossless compression.
      Accessible trough ImagingWebPLossless option. }
    property Lossless: LongBool read FLossless write FLossless;
    { Time spent to make the files smaller: 0 (fast) to 9 (smallest
      files, slow). Used by the Pascal encoders only.
      Accessible trough ImagingWebPEffort option. }
    property Effort: LongInt read FEffort write FEffort;
    { If True frames of animations are loaded as they look when shown: each
      one is the whole canvas with the frame drawn over the previous ones.
      If False they are loaded as stored in the file (often only the part
      of the canvas that changes), to be put together by the caller.
      Accessible trough ImagingWebPLoadAnimated option. }
    property LoadAnimated: LongBool read FLoadAnimated write FLoadAnimated;
  end;

implementation

uses
  ImagingLibWebP, ImagingExtFileFormats;

const
  SWebPFormatName = 'WebP Image';
  SWebPMasks = '*.webp';
  WebPSupportedFormats: TImageFormats = [ifR8G8B8, ifA8R8G8B8];
  WebPDefaultQuality = 85;
  WebPDefaultLossless = False;
  WebPDefaultEffort = 6;
  WebPMaxEffort = 9;
  WebPDefaultLoadAnimated = True;
  WebPDefaultFrameDelay = 100;

resourcestring
  SWebPDecodingFailed = 'WebP image could not be decoded';

type
  { Start of every WebP file. }
  TWebPRiffHeader = packed record
    Riff: TChar4;     // 'RIFF'
    Size: UInt32;     // Size of the rest of the file
    WebP: TChar4;     // 'WEBP'
  end;

const
  RiffMagic: TChar4 = 'RIFF';
  WebPMagic: TChar4 = 'WEBP';

function IsWebPHeader(const Hdr: TWebPRiffHeader): Boolean;
begin
  Result := (Hdr.Riff = RiffMagic) and (Hdr.WebP = WebPMagic);
end;

{ TWebPFileFormat class implementation }

procedure TWebPFileFormat.Define;
begin
  FName := SWebPFormatName;
  FSupportedFormats := WebPSupportedFormats;
  RefreshBackend;

  FQuality := WebPDefaultQuality;
  FLossless := WebPDefaultLossless;
  FEffort := WebPDefaultEffort;
  FLoadAnimated := WebPDefaultLoadAnimated;

  AddMasks(SWebPMasks);
  RegisterOption(ImagingWebPQuality, @FQuality);
  RegisterOption(ImagingWebPLossless, @FLossless);
  RegisterOption(ImagingWebPEffort, @FEffort);
  RegisterOption(ImagingWebPLoadAnimated, @FLoadAnimated);
end;

procedure TWebPFileFormat.RefreshBackend;
begin
  FFeatures := [ffMultiImage];
  // WebP backend is selected at this point (if not already) as WebPCanLoad
  // triggers backend setup.
  if WebPCanLoad then
    Include(FFeatures, ffLoad);
  if WebPCanSave then
    Include(FFeatures, ffSave);
end;

procedure TWebPFileFormat.CheckOptionsValidity;
begin
  if (FQuality < 0) or (FQuality > 100) then
    FQuality := WebPDefaultQuality;
  if (FEffort < 0) or (FEffort > WebPMaxEffort) then
    FEffort := WebPDefaultEffort;
end;

function TWebPFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Hdr: TWebPRiffHeader;
  Data: PBuffer;
  DataSize, RestSize: Int64;
  Info: TWebPImageInfo;
  Anim: TWebPAnimInfo;
  I, MaxFrames: Integer;
begin
  Result := False;
  SetLength(Images, 1);

  with GetIO do
  begin
    if (Read(Handle, @Hdr, SizeOf(Hdr)) <> SizeOf(Hdr)) or not IsWebPHeader(Hdr) then
      Exit;

    // Codecs want the whole file in memory. Size in the header says how much
    // to read so only this one image is consumed from the input.
    RestSize := Int64(Hdr.Size) - SizeOf(Hdr.WebP);

    if RestSize <= 0 then
      Exit;

    DataSize := SizeOf(Hdr) + RestSize;
    GetMem(Data, DataSize);

    try
      Move(Hdr, Data^, SizeOf(Hdr));
      // File can be shorter than the header says, codec deals with that
      RestSize := Read(Handle, @Data[SizeOf(Hdr)], RestSize);
      if RestSize < 0 then
        RestSize := 0;
      DataSize := SizeOf(Hdr) + RestSize;

      if WebPGetImageInfo(Data, DataSize, Info) and Info.Animated then
      begin
        MaxFrames := 0;
        if OnlyFirstLevel then
          MaxFrames := 1;
        Result := WebPLoadAnimation(Data, DataSize, FLoadAnimated, MaxFrames, Images, Anim);

        if Result then
        begin
          FMetadata.SetMetaItem(SMetaAnimationLoops, Anim.LoopCount);
          for I := 0 to High(Anim.Frames) do
            FMetadata.SetMetaItem(SMetaFrameDelay, Anim.Frames[I].Duration, I);
        end;
      end
      else
        Result := WebPLoadImage(Data, DataSize, Images[0], Info);

      if not Result then
        RaiseImaging(SWebPDecodingFailed);
    finally
      FreeMem(Data);
    end;
  end;
end;

function TWebPFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  ImageToSave: TImageData;
  MustBeFreed: Boolean;
  Params: TWebPSaveParams;
  Data: TDynByteArray;
  Frames: TDynImageDataArray;
  Owned: array of Boolean;
  Delays: array of Integer;
  I, Count, Loops: Integer;
begin
  Result := False;
  FillChar(Params, SizeOf(Params), 0);
  Params.Quality := FQuality;
  Params.Lossless := FLossless;
  Params.Effort := FEffort;

  if FLastIdx > FFirstIdx then
  begin
    // More images: animation
    Count := FLastIdx - FFirstIdx + 1;
    SetLength(Frames, Count);
    SetLength(Owned, Count);
    SetLength(Delays, Count);

    try
      for I := 0 to Count - 1 do
      begin
        if not MakeCompatible(Images[FFirstIdx + I], Frames[I], Owned[I]) then
          Exit;

        Delays[I] := WebPDefaultFrameDelay;
        if FMetadata.HasMetaItemForSaving(SMetaFrameDelay, FFirstIdx + I) then
          Delays[I] := FMetadata.MetaItemsForSavingMulti[SMetaFrameDelay, FFirstIdx + I];
      end;

      Loops := 0;
      if FMetadata.HasMetaItemForSaving(SMetaAnimationLoops) then
        Loops := FMetadata.MetaItemsForSaving[SMetaAnimationLoops];

      if WebPSaveAnimation(Frames, 0, Count - 1, Delays, Loops, Params, Data) then
        Result := GetIO.Write(Handle, Pointer(Data), Length(Data)) = Length(Data);
    finally
      for I := 0 to Count - 1 do
        if Owned[I] then
          FreeImage(Frames[I]);
    end;
  end
  else if MakeCompatible(Images[Index], ImageToSave, MustBeFreed) then
  try
    if WebPSaveImage(ImageToSave, Params, Data) then
      Result := GetIO.Write(Handle, Pointer(Data), Length(Data)) = Length(Data);
  finally
    if MustBeFreed then
      FreeImage(ImageToSave);
  end;
end;

function TWebPFileFormat.GetConversionFormat(const Info: TImageFormatInfo): TImageFormat;
begin
  if Info.HasAlphaChannel then
    Result := ifA8R8G8B8
  else
    Result := ifR8G8B8;
end;

function TWebPFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Hdr: TWebPRiffHeader;
  ReadCount: Int64;
begin
  Result := False;
  if Handle <> nil then
  with GetIO do
  begin
    FillChar(Hdr, SizeOf(Hdr), 0);
    ReadCount := Read(Handle, @Hdr, SizeOf(Hdr));
    Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount = SizeOf(Hdr)) and IsWebPHeader(Hdr);
  end;
end;

initialization
  RegisterImageFileFormat(TWebPFileFormat);

end.
