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

{ This unit selects the WebP codec used by Imaging and wraps it in a small
  API working with Imaging's image data and memory buffers.

  Two implementations are available and the choice is made at run time:

  - libwebp shared library (DLL/SO/dylib) is used when it is found. It decodes
    and encodes (lossy and lossless) and is the only option for Delphi older
    than 2009. Define WEBP_NO_DYNLIB to never look for it.

  - PasWebP, pure Pascal codec: decoder PasWebPDec (fork of WebPDec by
    Xelitan, which is a port of libwebp decoder) and encoders PasWebPLossless
    and PasWebPLossy. Used when libwebp is not found.
    Needs FPC or Delphi 2009+. Define WEBP_NO_BUNDLED to leave it out.

  In Delphi older than 2009, if libwebp is not found, there is no WebP support.

  The defines are listed in ImagingOptions.inc, set them there, in project
  options, or in ImagingUserOptions.inc.

  Animated WebP: frames are found here (RIFF chunks), decoded and encoded
  as still images by the codec in use and put together here, so animations
  work the same with both codecs. }
unit ImagingLibWebP;

{$I ImagingOptions.inc}

// LibWebPDynLib unit can be used only on targets with dynamic libraries
{$IF not Defined(WEBP_NO_DYNLIB) and Defined(HAS_DYNLIBS)}
  {$DEFINE WEBP_USE_DYNLIB}
{$IFEND}

{$IF not Defined(WEBP_NO_BUNDLED) and
  ((Defined(DCC) and (CompilerVersion >= 20)) or Defined(FPC))}
  {$DEFINE WEBP_USE_BUNDLED}
{$IFEND}

interface

uses
  ImagingTypes, ImagingUtility;

type
  { WebP codec implementation in use. }
  TWebPBackend = (
    wbNone,     // nothing available, WebP can't be loaded nor saved
    wbPascal,   // Pascal codec
    wbLibWebP   // libwebp shared library, loading and saving
  );

  TWebPImageInfo = record
    Width: Integer;
    Height: Integer;
    HasAlpha: Boolean;
    { File has an animation: Width and Height are of its canvas. }
    Animated: Boolean;
  end;

  { Frame of an animation as the file has it: a picture put on the canvas. }
  TWebPFrameInfo = record
    X, Y, Width, Height: Integer;
    Duration: Integer;             // milliseconds
    { Frame is alpha blended over what the canvas has; otherwise it replaces
      the rectangle with its alpha. }
    Blend: Boolean;
    { Rectangle of the frame is cleared to transparent before the next
      frame is drawn. }
    DisposeToBackground: Boolean;
  end;

  TWebPAnimInfo = record
    CanvasWidth, CanvasHeight: Integer;
    LoopCount: Integer;            // 0 = forever
    HasAlpha: Boolean;
    Frames: array of TWebPFrameInfo;
  end;

  TWebPSaveParams = record
    Quality: Integer;  // Quality of lossy compression, 0..100
    Lossless: Boolean;
    { Time spent to make the file smaller, 0 (fast) .. 9 (smallest).
      Used by the Pascal encoders only. }
    Effort: Integer;
  end;

{ Returns WebP implementation in use. Looks for libwebp (in default locations)
  when called for the first time. }
function WebPBackend: TWebPBackend;
function WebPCanLoad: Boolean;
function WebPCanSave: Boolean;
{ Makes the given backend current (e.g. the Pascal codec although libwebp is
  loaded). Returns False and keeps the current one if it is not available. }
function SelectWebPBackend(ABackend: TWebPBackend): Boolean;

{ Loads libwebp from the given file and uses it from now on (instead of
  looking for it in the default locations) - updates the current backend.
  Returns False and falls back to the Pascal codec (if available)
  when the library can't be loaded. }
function TryLoadWebPLibrary(const FileName: string): Boolean;

function WebPGetImageInfo(Data: Pointer; Size: Int64; out Info: TWebPImageInfo): Boolean;
{ Decodes WebP file stored in memory. New image is created in Image: ifA8R8G8B8
  if the file has alpha channel, ifR8G8B8 otherwise. For animated files the
  first frame is loaded (placed on the canvas of the animation). }
function WebPLoadImage(Data: Pointer; Size: Int64; var Image: TImageData;
  out Info: TWebPImageInfo): Boolean;
{ Encodes Image (ifR8G8B8 or ifA8R8G8B8) to WebP file in memory. }
function WebPSaveImage(const Image: TImageData; const Params: TWebPSaveParams;
  out Data: TDynByteArray): Boolean;

{ Loads frames of an animated WebP file stored in memory, all of them or
  MaxFrames first ones (when not zero).
  With Compose each image is the whole canvas as it looks when the frame is
  shown (frames drawn over the previous ones the way the file says):
  ifA8R8G8B8 when the file says it has alpha, ifR8G8B8 otherwise.
  Without it images are the frames as stored, of their own sizes; where
  they belong is in Anim.Frames. }
function WebPLoadAnimation(Data: Pointer; Size: Int64; Compose: Boolean; MaxFrames: Integer;
  var Images: TDynImageDataArray; out Anim: TWebPAnimInfo): Boolean;
{ Encodes images First..Last (ifR8G8B8 or ifA8R8G8B8) as animated WebP file
  in memory. Canvas has the size of the largest image, smaller ones are in
  its top left corner with the rest transparent. Delays are times the
  images are shown for in milliseconds (the first one is of image First),
  LoopCount number of repetitions (0 = forever).
  Lossless animations of images of one size store only the rectangle
  that changed from the previous image; an image that is the same as the
  one before it takes a few bytes in lossy animations too. }
function WebPSaveAnimation(const Images: TDynImageDataArray; First, Last: Integer;
  const Delays: array of Integer; LoopCount: Integer; const Params: TWebPSaveParams;
  out Data: TDynByteArray): Boolean;

implementation

uses
{$IFDEF WEBP_USE_DYNLIB}
  LibWebPDynLib,
{$ENDIF}
{$IFDEF WEBP_USE_BUNDLED}
  PasWebPDec, PasWebPLossless, PasWebPLossy,
{$ENDIF}
  SysUtils, Imaging;

const
  RiffTag = $46464952;  // 'RIFF'
  WebPTag = $50424557;  // 'WEBP'
  VP8XTag = $58385056;  // 'VP8X'
  VP8Tag  = $20385056;  // 'VP8 '
  VP8LTag = $4C385056;  // 'VP8L'
  ALPHTag = $48504C41;  // 'ALPH'
  ANIMTag = $4D494E41;  // 'ANIM'
  ANMFTag = $464D4E41;  // 'ANMF'

  RiffHeaderSize = 12;
  ChunkHeaderSize = 8;
  VP8XChunkSize = 10;
  VP8XFlagAnimation = $02;
  VP8XFlagAlpha = $10;
  ANIMChunkSize = 6;
  FrameHeaderSize = 16;
  FrameFlagDispose = $01;
  FrameFlagNoBlend = $02;
  MaxUInt24 = $FFFFFF;

var
  Backend: TWebPBackend = wbNone;
  BackendDetected: Boolean = False;

function ReadByte(Data: PBuffer; Offset: Int64): Byte;
begin
  Result := Data[Offset];
end;

function ReadUInt32(Data: PBuffer; Offset: Int64): UInt32;
begin
  Result := PUInt32(@Data[Offset])^;
end;

{ There is no 3 byte type to read and reading 4 bytes could reach
  beyond the end of the data. }
function ReadUInt24(Data: PBuffer; Offset: Int64): UInt32;
begin
  Result := Data[Offset] or (UInt32(Data[Offset + 1]) shl 8) or (UInt32(Data[Offset + 2]) shl 16);
end;

procedure WriteUInt32(Data: PBuffer; Offset: Int64; Value: UInt32);
begin
  PUInt32(@Data[Offset])^ := Value;
end;

procedure SelectBackend;
begin
  Backend := wbNone;
{$IFDEF WEBP_USE_BUNDLED}
  Backend := wbPascal;
{$ENDIF}
{$IFDEF WEBP_USE_DYNLIB}
  if IsLibWebPLoaded then
    Backend := wbLibWebP;
{$ENDIF}
  BackendDetected := True;
end;

function WebPBackend: TWebPBackend;
begin
  if not BackendDetected then
  begin
  {$IFDEF WEBP_USE_DYNLIB}
    LoadLibWebP;
  {$ENDIF}
    SelectBackend;
  end;
  Result := Backend;
end;

function WebPCanLoad: Boolean;
begin
  Result := WebPBackend <> wbNone;
end;

function WebPCanSave: Boolean;
begin
  Result := WebPBackend <> wbNone;
end;

function SelectWebPBackend(ABackend: TWebPBackend): Boolean;
begin
  WebPBackend;
  case ABackend of
  {$IFDEF WEBP_USE_BUNDLED}
    wbPascal: Result := True;
  {$ENDIF}
  {$IFDEF WEBP_USE_DYNLIB}
    wbLibWebP: Result := IsLibWebPLoaded;
  {$ENDIF}
  else
    Result := False;
  end;

  if Result then
    Backend := ABackend;
end;

function TryLoadWebPLibrary(const FileName: string): Boolean;
begin
{$IFDEF WEBP_USE_DYNLIB}
  Result := LoadLibWebPFromFile(FileName);
{$ELSE}
  Result := False;
{$ENDIF}
  SelectBackend;
end;

{$IFDEF WEBP_USE_BUNDLED}
{ Finds out what the Pascal decoder doesn't tell: presence of alpha channel
  and animation. Walks RIFF chunks up to the image data. }
function ParseWebPContainer(Data: PBuffer; Size: Int64; out Info: TWebPImageInfo): Boolean;
var
  Pos: Int64;
  Tag, ChunkSize: UInt32;
begin
  Result := False;
  FillChar(Info, SizeOf(Info), 0);

  if (Size < RiffHeaderSize + ChunkHeaderSize) or (ReadUInt32(Data, 0) <> RiffTag) or
     (ReadUInt32(Data, 8) <> WebPTag) then
  begin
    Exit;
  end;

  Pos := RiffHeaderSize;

  while Pos + ChunkHeaderSize <= Size do
  begin
    Tag := ReadUInt32(Data, Pos);
    ChunkSize := ReadUInt32(Data, Pos + 4);

    if Tag = VP8XTag then
    begin
      if Pos + ChunkHeaderSize + VP8XChunkSize > Size then
        Exit;

      if ReadByte(Data, Pos + 8) and VP8XFlagAlpha <> 0 then
        Info.HasAlpha := True;
      if ReadByte(Data, Pos + 8) and VP8XFlagAnimation <> 0 then
        Info.Animated := True;
      // Canvas size, the only size info for animated files
      Info.Width := 1 + ReadUInt24(Data, Pos + 12);
      Info.Height := 1 + ReadUInt24(Data, Pos + 15);
    end
    else if (Tag = ANIMTag) or (Tag = ANMFTag) then
    begin
      Info.Animated := True;
      Result := True;
      Exit;
    end
    else if Tag = ALPHTag then
    begin
      Info.HasAlpha := True;
    end
    else if Tag = VP8Tag then
    begin
      Result := True;
      Exit;
    end
    else if Tag = VP8LTag then
    begin
      // Signature byte, 14 bits width, 14 bits height, 1 bit alpha
      if (Pos + 13 > Size) or (ReadByte(Data, Pos + 8) <> $2F) then
        Exit;

      if ReadUInt32(Data, Pos + 9) and (1 shl 28) <> 0 then
        Info.HasAlpha := True;
      Result := True;
      Exit;
    end;

    // Chunks are padded to even size
    Pos := Pos + ChunkHeaderSize + ChunkSize + (ChunkSize and 1);
  end;
end;

function PascalGetImageInfo(Data: Pointer; Size: Int64; out Info: TWebPImageInfo): Boolean;
begin
  Result := ParseWebPContainer(Data, Size, Info);
  if Result and not Info.Animated then
    Result := PasWebPGetInfo(Data, Size, Info.Width, Info.Height);
end;

function PascalLoadImage(Data: Pointer; Size: Int64; var Image: TImageData;
  const Info: TWebPImageInfo): Boolean;
begin
  Result := False;

  // Decoder writes straight to the memory of the image
  if Info.HasAlpha then
  begin
    if NewImage(Info.Width, Info.Height, ifA8R8G8B8, Image) then
      Result := PasWebPDecodeInto(Data, Size, Image.Bits, NativeInt(Info.Width) * 4,
                                  Info.Width, Info.Height, True);
  end
  else
  begin
    if NewImage(Info.Width, Info.Height, ifR8G8B8, Image) then
      Result := PasWebPDecodeInto(Data, Size, Image.Bits, NativeInt(Info.Width) * 3,
                                  Info.Width, Info.Height, False);
  end;

  if not Result then
    FreeImage(Image);
end;
{$ENDIF}

{$IFDEF WEBP_USE_DYNLIB}
function LibGetImageInfo(Data: Pointer; Size: Int64; out Info: TWebPImageInfo): Boolean;
var
  Features: TWebPBitstreamFeatures;
begin
  FillChar(Info, SizeOf(Info), 0);
  FillChar(Features, SizeOf(Features), 0);
  Result := WebPGetFeaturesInternal(Data, Size, Features, WEBP_DECODER_ABI_VERSION) = 0;

  if Result then
  begin
    Info.Width := Features.Width;
    Info.Height := Features.Height;
    Info.HasAlpha := Features.HasAlpha <> 0;
    Info.Animated := Features.HasAnimation <> 0;
  end;
end;

function LibLoadImage(Data: Pointer; Size: Int64; var Image: TImageData;
  const Info: TWebPImageInfo): Boolean;
begin
  Result := False;

  if Info.HasAlpha then
  begin
    if NewImage(Info.Width, Info.Height, ifA8R8G8B8, Image) then
      Result := WebPDecodeBGRAInto(Data, Size, Image.Bits, Image.Size, Info.Width * 4) <> nil;
  end
  else
  begin
    if NewImage(Info.Width, Info.Height, ifR8G8B8, Image) then
      Result := WebPDecodeBGRInto(Data, Size, Image.Bits, Image.Size, Info.Width * 3) <> nil;
  end;

  if not Result then
    FreeImage(Image);
end;
{$ENDIF}

function WebPGetImageInfo(Data: Pointer; Size: Int64; out Info: TWebPImageInfo): Boolean;
begin
  Result := False;
  FillChar(Info, SizeOf(Info), 0);
  if (Data = nil) or (Size <= 0) then
    Exit;

{$IFDEF WEBP_USE_DYNLIB}
  if WebPBackend = wbLibWebP then
    Result := LibGetImageInfo(Data, Size, Info);
{$ENDIF}
{$IFDEF WEBP_USE_BUNDLED}
  if WebPBackend = wbPascal then
    Result := PascalGetImageInfo(Data, Size, Info);
{$ENDIF}
end;

function LoadStillImage(Data: Pointer; Size: Int64; var Image: TImageData;
  const Info: TWebPImageInfo): Boolean;
begin
  Result := False;
  if Info.Animated or (Info.Width <= 0) or (Info.Height <= 0) then
    Exit;

{$IFDEF WEBP_USE_DYNLIB}
  if WebPBackend = wbLibWebP then
    Result := LibLoadImage(Data, Size, Image, Info);
{$ENDIF}
{$IFDEF WEBP_USE_BUNDLED}
  if WebPBackend = wbPascal then
    Result := PascalLoadImage(Data, Size, Image, Info);
{$ENDIF}
end;

type
  { Where the image chunks of a frame are in the file. }
  TFrameChunks = record
    Pos, Size: Int64;
  end;
  TFrameChunksArray = array of TFrameChunks;

{ Finds the frames of an animation. A file that ends in the middle
  of a frame gives the frames before it. }
function ParseAnimation(Data: PBuffer; Size: Int64; out Anim: TWebPAnimInfo;
  out Chunks: TFrameChunksArray): Boolean;
var
  Pos: Int64;
  Tag, ChunkSize: UInt32;
  Flags, N: Integer;
begin
  Result := False;
  Anim.CanvasWidth := 0;
  Anim.CanvasHeight := 0;
  Anim.LoopCount := 0;
  Anim.HasAlpha := False;
  Anim.Frames := nil;
  Chunks := nil;

  if (Size < RiffHeaderSize + ChunkHeaderSize) or (ReadUInt32(Data, 0) <> RiffTag) or
     (ReadUInt32(Data, 8) <> WebPTag) then
    Exit;

  Pos := RiffHeaderSize;

  while Pos + ChunkHeaderSize <= Size do
  begin
    Tag := ReadUInt32(Data, Pos);
    ChunkSize := ReadUInt32(Data, Pos + 4);

    if (Tag = VP8XTag) and (Pos + ChunkHeaderSize + VP8XChunkSize <= Size) then
    begin
      Anim.HasAlpha := ReadByte(Data, Pos + 8) and VP8XFlagAlpha <> 0;
      Anim.CanvasWidth := 1 + ReadUInt24(Data, Pos + 12);
      Anim.CanvasHeight := 1 + ReadUInt24(Data, Pos + 15);
    end
    else if (Tag = ANIMTag) and (Pos + ChunkHeaderSize + ANIMChunkSize <= Size) then
    begin
      // Background color is not used: canvas starts transparent (as in
      // libwebp and browsers)
      Anim.LoopCount := ReadByte(Data, Pos + 12) or (ReadByte(Data, Pos + 13) shl 8);
    end
    else if Tag = ANMFTag then
    begin
      if (ChunkSize <= FrameHeaderSize) or (Pos + ChunkHeaderSize + ChunkSize > Size) then
        Break;

      N := Length(Anim.Frames);
      SetLength(Anim.Frames, N + 1);
      SetLength(Chunks, N + 1);

      // Offsets are stored divided by two, sizes minus one
      with Anim.Frames[N] do
      begin
        X := 2 * ReadUInt24(Data, Pos + 8);
        Y := 2 * ReadUInt24(Data, Pos + 11);
        Width := 1 + ReadUInt24(Data, Pos + 14);
        Height := 1 + ReadUInt24(Data, Pos + 17);
        Duration := ReadUInt24(Data, Pos + 20);
        Flags := ReadByte(Data, Pos + 23);
        Blend := Flags and FrameFlagNoBlend = 0;
        DisposeToBackground := Flags and FrameFlagDispose <> 0;
      end;

      Chunks[N].Pos := Pos + ChunkHeaderSize + FrameHeaderSize;
      Chunks[N].Size := Int64(ChunkSize) - FrameHeaderSize;
    end;

    Pos := Pos + ChunkHeaderSize + ChunkSize + (ChunkSize and 1);
  end;

  Result := (Anim.CanvasWidth > 0) and (Anim.CanvasHeight > 0) and (Length(Anim.Frames) > 0);
end;

{ Frames of animations are not files that still image decoders could
  decode: they are image chunks inside ANMF chunks. This builds a still WebP
  file of the chunks of one frame. }
procedure BuildStillFile(Data: PBuffer; const Chunks: TFrameChunks; Width, Height: Integer;
  out FileData: TDynByteArray);
var
  SubPos, DestPos: Int64;
  ChunkSize: UInt32;
  HasAlphaChunk: Boolean;
  Dest: PBuffer;
begin
  // Lossy frame with separate alpha data needs extended file header
  HasAlphaChunk := False;
  SubPos := Chunks.Pos;

  while SubPos + ChunkHeaderSize <= Chunks.Pos + Chunks.Size do
  begin
    if ReadUInt32(Data, SubPos) = ALPHTag then
      HasAlphaChunk := True;
    ChunkSize := ReadUInt32(Data, SubPos + 4);
    SubPos := SubPos + ChunkHeaderSize + ChunkSize + (ChunkSize and 1);
  end;

  DestPos := RiffHeaderSize + Chunks.Size;
  if HasAlphaChunk then
    DestPos := DestPos + ChunkHeaderSize + VP8XChunkSize;
  SetLength(FileData, DestPos);
  Dest := Pointer(FileData);
  WriteUInt32(Dest, 0, RiffTag);
  WriteUInt32(Dest, 4, Length(FileData) - 8);
  WriteUInt32(Dest, 8, WebPTag);
  DestPos := RiffHeaderSize;

  if HasAlphaChunk then
  begin
    WriteUInt32(Dest, DestPos, VP8XTag);
    WriteUInt32(Dest, DestPos + 4, VP8XChunkSize);
    WriteUInt32(Dest, DestPos + 8, VP8XFlagAlpha);  // flags + 3 reserved bytes
    WriteUInt32(Dest, DestPos + 12, Width - 1);     // 3 bytes, 4th overwritten below
    WriteUInt32(Dest, DestPos + 15, Height - 1);    // 3 bytes + 1 byte beyond the chunk
    DestPos := DestPos + ChunkHeaderSize + VP8XChunkSize;
  end;

  // Last WriteUInt32 above touched the first payload byte, payload goes over it
  Move(Data[Chunks.Pos], Dest[DestPos], Chunks.Size);
end;

function DecodeFrame(Data: PBuffer; const Chunks: TFrameChunks; const Frame: TWebPFrameInfo;
  var Image: TImageData): Boolean;
var
  FileData: TDynByteArray;
  Info: TWebPImageInfo;
begin
  BuildStillFile(Data, Chunks, Frame.Width, Frame.Height, FileData);
  Result := WebPGetImageInfo(Pointer(FileData), Length(FileData), Info) and
            LoadStillImage(Pointer(FileData), Length(FileData), Image, Info);
end;

{ Pixel of a frame over a pixel of the canvas, both with alpha that colors
  are not multiplied by. The integer arithmetic of libwebp (anim_decode.c),
  so the frames are the same as it gives. }
function BlendPixel(Src, Dst: UInt32): UInt32;
var
  SrcA, DstA, BlendA, Scale: UInt32;

  function Channel(Shift: Integer): UInt32;
  begin
    Result := ((((Src shr Shift) and $FF) * SrcA + ((Dst shr Shift) and $FF) * DstA) * Scale) shr 24;
  end;

begin
  SrcA := Src shr 24;
  if SrcA = 0 then
  begin
    Result := Dst;
    Exit;
  end;

  // What is left of the canvas pixel: its alpha * (255 - SrcA) / 255
  DstA := ((Dst shr 24) * (256 - SrcA)) shr 8;
  BlendA := SrcA + DstA;
  Scale := (1 shl 24) div BlendA;
  Result := Channel(0) or (Channel(8) shl 8) or (Channel(16) shl 16) or (BlendA shl 24);
end;

function WebPLoadAnimation(Data: Pointer; Size: Int64; Compose: Boolean; MaxFrames: Integer;
  var Images: TDynImageDataArray; out Anim: TWebPAnimInfo): Boolean;
var
  Chunks: TFrameChunksArray;
  Canvas, Frame: TImageData;
  I, X, Y, X0, Y0, CopyWidth, CopyHeight, Count: Integer;
  ClearX0, ClearY0, ClearX1, ClearY1: Integer;
  FrameHasAlpha, IsKey, PrevIsKey, InClearRows: Boolean;
  Src, Dst: PUInt32;

  function IsFullFrame(const Info: TWebPFrameInfo): Boolean;
  begin
    Result := (Info.Width = Anim.CanvasWidth) and (Info.Height = Anim.CanvasHeight);
  end;

  { Part of the rectangle that is on the canvas. }
  procedure ClipToCanvas(const Info: TWebPFrameInfo; Width, Height: Integer);
  begin
    X0 := Info.X;
    Y0 := Info.Y;
    CopyWidth := Width;
    CopyHeight := Height;
    if X0 + CopyWidth > Anim.CanvasWidth then
      CopyWidth := Anim.CanvasWidth - X0;
    if Y0 + CopyHeight > Anim.CanvasHeight then
      CopyHeight := Anim.CanvasHeight - Y0;
  end;

begin
  Result := False;
  FreeImagesInArray(Images);
  Images := nil;
  if not ParseAnimation(Data, Size, Anim, Chunks) then
    Exit;

  Count := Length(Anim.Frames);
  if (MaxFrames > 0) and (Count > MaxFrames) then
  begin
    Count := MaxFrames;
    SetLength(Anim.Frames, Count);
  end;

  SetLength(Images, Count);
  for I := 0 to Count - 1 do
    InitImage(Images[I]);
  InitImage(Canvas);
  InitImage(Frame);
  PrevIsKey := False;

  try
    if Compose then
    begin
      // Canvas starts transparent
      if not NewImage(Anim.CanvasWidth, Anim.CanvasHeight, ifA8R8G8B8, Canvas) then
        Exit;
      FillChar(Canvas.Bits^, Canvas.Size, 0);
    end;

    for I := 0 to Count - 1 do
    begin
      if not Compose then
      begin
        if not DecodeFrame(Data, Chunks[I], Anim.Frames[I], Images[I]) then
          Exit;
        Continue;
      end;

      // What the previous frame asked to be done after it
      if (I > 0) and Anim.Frames[I - 1].DisposeToBackground then
      begin
        ClipToCanvas(Anim.Frames[I - 1], Anim.Frames[I - 1].Width, Anim.Frames[I - 1].Height);
        for Y := 0 to CopyHeight - 1 do
          if CopyWidth > 0 then
            FillChar(PColor32RecBuffer(Canvas.Bits)[NativeInt(Y0 + Y) * Canvas.Width + X0],
                     CopyWidth * 4, 0);
      end;

      FreeImage(Frame);
      if not DecodeFrame(Data, Chunks[I], Anim.Frames[I], Frame) then
        Exit;
      FrameHasAlpha := Frame.Format = ifA8R8G8B8;
      if not ConvertImage(Frame, ifA8R8G8B8) then
        Exit;

      // Key frame does not depend on the frames before it: the canvas is
      // empty under it or it covers all of it (rules of libwebp, its frames
      // are matched exactly)
      IsKey := (I = 0) or
               ((not FrameHasAlpha or not Anim.Frames[I].Blend) and IsFullFrame(Anim.Frames[I])) or
               (Anim.Frames[I - 1].DisposeToBackground and (IsFullFrame(Anim.Frames[I - 1]) or PrevIsKey));
      PrevIsKey := IsKey;
      if IsKey then
        FillChar(Canvas.Bits^, Canvas.Size, 0);

      // Where the previous frame was cleared away nothing is to blend with
      // (blending with transparent pixels would only round the colors)
      ClearX0 := 0;
      ClearX1 := -1;
      ClearY0 := 0;
      ClearY1 := -1;
      if (I > 0) and Anim.Frames[I - 1].DisposeToBackground then
      begin
        ClearX0 := Anim.Frames[I - 1].X;
        ClearY0 := Anim.Frames[I - 1].Y;
        ClearX1 := ClearX0 + Anim.Frames[I - 1].Width - 1;
        ClearY1 := ClearY0 + Anim.Frames[I - 1].Height - 1;
      end;

      ClipToCanvas(Anim.Frames[I], Frame.Width, Frame.Height);

      for Y := 0 to CopyHeight - 1 do
      begin
        Src := @PColor32RecBuffer(Frame.Bits)[NativeInt(Y) * Frame.Width];
        Dst := @PColor32RecBuffer(Canvas.Bits)[NativeInt(Y0 + Y) * Canvas.Width + X0];

        if CopyWidth <= 0 then
          Break;

        if Anim.Frames[I].Blend and not IsKey then
        begin
          InClearRows := (Y0 + Y >= ClearY0) and (Y0 + Y <= ClearY1);
          for X := 0 to CopyWidth - 1 do
          begin
            if (Src^ shr 24 = 255) or (InClearRows and (X0 + X >= ClearX0) and (X0 + X <= ClearX1)) then
              Dst^ := Src^
            else
              Dst^ := BlendPixel(Src^, Dst^);
            Inc(Src);
            Inc(Dst);
          end;
        end
        else
          Move(Src^, Dst^, CopyWidth * 4);
      end;

      // The canvas as it is now is the image of this frame
      if Anim.HasAlpha then
      begin
        if not CloneImage(Canvas, Images[I]) then
          Exit;
      end
      else if not ConvertImageInto(Canvas, Images[I], ifR8G8B8) then
        Exit;
    end;

    Result := True;
  finally
    FreeImage(Canvas);
    FreeImage(Frame);
    if not Result then
    begin
      FreeImagesInArray(Images);
      Images := nil;
    end;
  end;
end;

function WebPLoadImage(Data: Pointer; Size: Int64; var Image: TImageData;
  out Info: TWebPImageInfo): Boolean;
var
  Images: TDynImageDataArray;
  Anim: TWebPAnimInfo;
begin
  Result := False;
  if not WebPGetImageInfo(Data, Size, Info) then
    Exit;

  if Info.Animated then
  begin
    // The first frame on the canvas
    Images := nil;
    Result := WebPLoadAnimation(Data, Size, True, 1, Images, Anim);
    if Result then
    begin
      Image := Images[0];
      Info.Width := Image.Width;
      Info.Height := Image.Height;
      Info.HasAlpha := Image.Format = ifA8R8G8B8;
    end;
  end
  else
    Result := LoadStillImage(Data, Size, Image, Info);
end;

{$IFDEF WEBP_USE_DYNLIB}
function LibSaveImage(const Image: TImageData; const Params: TWebPSaveParams;
  out Data: TDynByteArray): Boolean;
var
  Output: PByte;
  Size: Int64;
begin
  Result := False;
  Output := nil;

  if Image.Format = ifA8R8G8B8 then
  begin
    if Params.Lossless then
      Size := WebPEncodeLosslessBGRA(Image.Bits, Image.Width, Image.Height,
                                     Image.Width * 4, @Output)
    else
      Size := WebPEncodeBGRA(Image.Bits, Image.Width, Image.Height,
                             Image.Width * 4, Params.Quality, @Output);
  end
  else
  begin
    if Params.Lossless then
      Size := WebPEncodeLosslessBGR(Image.Bits, Image.Width, Image.Height,
                                    Image.Width * 3, @Output)
    else
      Size := WebPEncodeBGR(Image.Bits, Image.Width, Image.Height,
                            Image.Width * 3, Params.Quality, @Output);
  end;

  // Data of the library is copied so the caller needs not know who made it
  if Output <> nil then
  try
    if Size > 0 then
    begin
      SetLength(Data, Size);
      Move(Output^, Pointer(Data)^, Size);
      Result := True;
    end;
  finally
    LibWebPDynLib.WebPFree(Output);
  end;
end;
{$ENDIF}

function WebPSaveImage(const Image: TImageData; const Params: TWebPSaveParams;
  out Data: TDynByteArray): Boolean;
begin
  Result := False;
  Data := nil;
  if not (Image.Format in [ifR8G8B8, ifA8R8G8B8]) then
    Exit;

{$IFDEF WEBP_USE_DYNLIB}
  if WebPBackend = wbLibWebP then
    Result := LibSaveImage(Image, Params, Data);
{$ENDIF}
{$IFDEF WEBP_USE_BUNDLED}
  if WebPBackend = wbPascal then
  begin
    if Params.Lossless then
      Result := PasWebPEncodeLossless(Image.Bits, GetPixelsSize(Image.Format, Image.Width, 1),
                                      Image.Width, Image.Height, Image.Format = ifA8R8G8B8,
                                      Params.Effort, TPasWebPBytes(Data))
    else
      Result := PasWebPEncodeLossy(Image.Bits, GetPixelsSize(Image.Format, Image.Width, 1),
                                   Image.Width, Image.Height, Image.Format = ifA8R8G8B8,
                                   Params.Quality, Params.Effort, TPasWebPBytes(Data));
  end;
{$ENDIF}
end;

function WebPSaveAnimation(const Images: TDynImageDataArray; First, Last: Integer;
  const Delays: array of Integer; LoopCount: Integer; const Params: TWebPSaveParams;
  out Data: TDynByteArray): Boolean;
const
  DefaultDelay = 100;
var
  CanvasWidth, CanvasHeight, I, X, Y, X0, Y0, X1, Y1, Flags, Delay: Integer;
  SameSize, HasAlpha, Partial, Blend, AllOpaque: Boolean;
  Current, Previous, Part: TImageData;
  FileData: TDynByteArray;
  Size, Pos, SubPos, FramePos, PayloadSize: Int64;
  Tag, ChunkSize, Pixel: UInt32;
  Cur, Prev, Dst: PUInt32;

  procedure Reserve(Count: Int64);
  begin
    if Size + Count > Length(Data) then
      SetLength(Data, (Size + Count) * 3 div 2 + 4096);
  end;

  procedure PutUInt24(Offset: Int64; Value: UInt32);
  begin
    Data[Offset] := Byte(Value);
    Data[Offset + 1] := Byte(Value shr 8);
    Data[Offset + 2] := Byte(Value shr 16);
  end;

begin
  Result := False;
  Data := nil;
  if (First < 0) or (Last > High(Images)) or (First > Last) then
    Exit;

  // Canvas takes the largest image
  CanvasWidth := 0;
  CanvasHeight := 0;
  for I := First to Last do
  begin
    if not (Images[I].Format in [ifR8G8B8, ifA8R8G8B8]) then
      Exit;
    if Images[I].Width > CanvasWidth then CanvasWidth := Images[I].Width;
    if Images[I].Height > CanvasHeight then CanvasHeight := Images[I].Height;
  end;

  SameSize := True;
  for I := First to Last do
    if (Images[I].Width <> CanvasWidth) or (Images[I].Height <> CanvasHeight) then
      SameSize := False;

  // Only what changed is stored when it can be told exactly what the
  // canvas has: lossless images of one size
  Partial := SameSize and Params.Lossless;
  // The canvas outside of smaller images is transparent
  HasAlpha := not SameSize;

  // File header, VP8X and ANIM chunks are filled in at the end
  Size := RiffHeaderSize + ChunkHeaderSize + VP8XChunkSize + ChunkHeaderSize + ANIMChunkSize;
  Reserve(0);
  InitImage(Current);
  InitImage(Previous);
  InitImage(Part);

  try
    for I := First to Last do
    begin
      X0 := 0;
      Y0 := 0;
      Blend := False;
      FreeImage(Part);

      if Partial then
      begin
        if not ConvertImageInto(Images[I], Current, ifA8R8G8B8) then
          Exit;

        if I = First then
        begin
          if not CloneImage(Current, Part) then
            Exit;
        end
        else
        begin
          // Rectangle of the pixels that differ from the previous image
          X0 := CanvasWidth;
          Y0 := CanvasHeight;
          X1 := -1;
          Y1 := -1;
          AllOpaque := True;
          Cur := Current.Bits;
          Prev := Previous.Bits;

          for Y := 0 to CanvasHeight - 1 do
            for X := 0 to CanvasWidth - 1 do
            begin
              if Cur^ <> Prev^ then
              begin
                if X < X0 then X0 := X;
                if X > X1 then X1 := X;
                if Y < Y0 then Y0 := Y;
                if Y > Y1 then Y1 := Y;
                if Cur^ shr 24 <> 255 then
                  AllOpaque := False;
              end;
              Inc(Cur);
              Inc(Prev);
            end;

          if X1 < 0 then
          begin
            // Nothing changed: the smallest frame there can be
            X0 := 0;
            Y0 := 0;
            X1 := 0;
            Y1 := 0;
          end;

          // Frames are placed at even positions
          X0 := X0 and not 1;
          Y0 := Y0 and not 1;
          if not NewImage(X1 - X0 + 1, Y1 - Y0 + 1, ifA8R8G8B8, Part) then
            Exit;

          // When all new pixels are opaque the frame can be blended: pixels
          // that stay are transparent in it, which compresses well.
          // Otherwise the frame replaces its rectangle.
          Blend := AllOpaque;

          for Y := Y0 to Y1 do
          begin
            Cur := @PColor32RecBuffer(Current.Bits)[NativeInt(Y) * CanvasWidth + X0];
            Prev := @PColor32RecBuffer(Previous.Bits)[NativeInt(Y) * CanvasWidth + X0];
            Dst := @PColor32RecBuffer(Part.Bits)[NativeInt(Y - Y0) * Part.Width];

            for X := X0 to X1 do
            begin
              Pixel := Cur^;
              if Blend and (Pixel = Prev^) then
                Pixel := 0;
              Dst^ := Pixel;
              Inc(Cur);
              Inc(Prev);
              Inc(Dst);
            end;
          end;
        end;

        // Current image is the canvas for the next one
        FreeImage(Previous);
        Previous := Current;
        InitImage(Current);

        if not WebPSaveImage(Part, Params, FileData) then
          Exit;
      end
      else if SameSize and (I > First) and (Images[I].Format = Images[I - 1].Format) and
              CompareMem(Images[I].Bits, Images[I - 1].Bits, Images[I].Size) then
      begin
        // The same image again: a transparent pixel blended over the canvas
        // leaves it as it is
        if not NewImage(1, 1, ifA8R8G8B8, Part) then
          Exit;
        FillChar(Part.Bits^, Part.Size, 0);
        Blend := True;
        if not WebPSaveImage(Part, Params, FileData) then
          Exit;
      end
      else if not WebPSaveImage(Images[I], Params, FileData) then
        Exit;

      // Image chunks of the still file go to the frame
      FramePos := Size;
      Reserve(ChunkHeaderSize + FrameHeaderSize + Length(FileData));
      Size := Size + ChunkHeaderSize + FrameHeaderSize;
      PayloadSize := 0;
      SubPos := RiffHeaderSize;

      while SubPos + ChunkHeaderSize <= Length(FileData) do
      begin
        Tag := ReadUInt32(Pointer(FileData), SubPos);
        ChunkSize := ReadUInt32(Pointer(FileData), SubPos + 4);
        ChunkSize := ChunkHeaderSize + ChunkSize + (ChunkSize and 1);
        if SubPos + ChunkSize > Length(FileData) then
          Break;

        if (Tag = ALPHTag) or (Tag = VP8Tag) or (Tag = VP8LTag) then
        begin
          Move(FileData[SubPos], Data[Size], ChunkSize);
          Size := Size + ChunkSize;
          PayloadSize := PayloadSize + ChunkSize;
          // Alpha: its chunk of a lossy image, a bit of the lossless header
          if (Tag = ALPHTag) or ((Tag = VP8LTag) and
             (ReadUInt32(Pointer(FileData), SubPos + ChunkHeaderSize + 1) and (1 shl 28) <> 0)) then
            HasAlpha := True;
        end;

        SubPos := SubPos + ChunkSize;
      end;

      if PayloadSize = 0 then
        Exit;

      if Part.Bits <> nil then
      begin
        X := Part.Width;
        Y := Part.Height;
      end
      else
      begin
        X := Images[I].Width;
        Y := Images[I].Height;
      end;

      Delay := DefaultDelay;
      if I - First <= High(Delays) then
        Delay := Delays[I - First];
      if Delay < 0 then Delay := 0;
      if Delay > MaxUInt24 then Delay := MaxUInt24;

      // Whole images replace what is there; those smaller than the canvas
      // must also be cleared away for the next one
      Flags := 0;
      if not Blend then
        Flags := Flags or FrameFlagNoBlend;
      if not SameSize then
        Flags := Flags or FrameFlagDispose;

      WriteUInt32(Pointer(Data), FramePos, ANMFTag);
      WriteUInt32(Pointer(Data), FramePos + 4, FrameHeaderSize + PayloadSize);
      PutUInt24(FramePos + 8, X0 div 2);
      PutUInt24(FramePos + 11, Y0 div 2);
      PutUInt24(FramePos + 14, X - 1);
      PutUInt24(FramePos + 17, Y - 1);
      PutUInt24(FramePos + 20, Delay);
      Data[FramePos + 23] := Flags;
    end;

    SetLength(Data, Size);
    Pos := 0;
    WriteUInt32(Pointer(Data), Pos, RiffTag);
    WriteUInt32(Pointer(Data), Pos + 4, Size - 8);
    WriteUInt32(Pointer(Data), Pos + 8, WebPTag);
    Pos := RiffHeaderSize;

    Flags := VP8XFlagAnimation;
    if HasAlpha then
      Flags := Flags or VP8XFlagAlpha;
    WriteUInt32(Pointer(Data), Pos, VP8XTag);
    WriteUInt32(Pointer(Data), Pos + 4, VP8XChunkSize);
    WriteUInt32(Pointer(Data), Pos + 8, Flags);
    PutUInt24(Pos + 12, CanvasWidth - 1);
    PutUInt24(Pos + 15, CanvasHeight - 1);
    Pos := Pos + ChunkHeaderSize + VP8XChunkSize;

    if LoopCount < 0 then LoopCount := 0;
    if LoopCount > $FFFF then LoopCount := $FFFF;
    WriteUInt32(Pointer(Data), Pos, ANIMTag);
    WriteUInt32(Pointer(Data), Pos + 4, ANIMChunkSize);
    WriteUInt32(Pointer(Data), Pos + 8, 0);  // background color, not used by players
    Data[Pos + 12] := Byte(LoopCount);
    Data[Pos + 13] := Byte(LoopCount shr 8);
    Result := True;
  finally
    FreeImage(Current);
    FreeImage(Previous);
    FreeImage(Part);
    if not Result then
      Data := nil;
  end;
end;

end.
