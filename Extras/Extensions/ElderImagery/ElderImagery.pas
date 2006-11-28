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

{ This is basic unit of Elder Imagery extension for Vampyre Imaging Library.
  It adds support for loading and saving of images and textures from older
  Bethesda games (like TES2: Daggerfall, Redguard, Terminator: FS, TES: Arena, ...).
  This unit registers all file formats declared in additional ElderImagery units
  so its the only unit you need to add to uses clause of your project
  for Imaging to be able to load/save these new formats using standard
  loading/saving functions.}
unit ElderImagery;

{$I ImagingOptions.inc}

interface

uses
  ImagingTypes, Imaging;

type
  { Palette format used by 8bit Bethesda images (its BGR not RGB used by Imaging).}
  TElderPalette = array[0..255] of TColor24Rec;

  TElderFileFormat = class;
  TDaggerfallFileFormatClass = class of TElderFileFormat;

  { Used to hold information about some special images without headers.}
  TNoHeaderFileInfo = record
    Size: LongInt;
    Width: LongInt;
    Height: LongInt;
  end;

  { Basic class for image formats used mainly in TES2: Daggerfall.}
  TElderFileFormat = class(TImageFileFormat)
  protected
    FPalette: TElderPalette;
    FARGBPalette: PPalette32;
    { Decodes RLE compressed data.}
    procedure DagRLEDecode(InData: Pointer; InSize, OutSize: LongInt; out OutData: Pointer);
    function FindNoHeaderInfo(Size: LongInt; Infos: array of TNoHeaderFileInfo): LongInt;
    function TestNoHeaderFormat(Handle: TImagingHandle): TDaggerfallFileFormatClass;
    procedure ConvertPalette(const ElderPal: TElderPalette; ARGBPal: PPalette32);
    function GetInputSize(Handle: TImagingHandle): LongInt;
    procedure SetPalette(const Value: TElderPalette);
    function GetSupportedFormats: TImageFormats; override;
    function MakeCompatible(const Image: TImageData; var Comp: TImageData;
      out MustBeFreed: Boolean): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function TestFormat(Handle: TImagingHandle): Boolean; override;
    { Current palette used when loading and saving images. Nearly all images
      in Daggerfall use external palettes. Change this property if you want
      images that don't use default palette to load correctly.}
    property Palette: TElderPalette read FPalette write SetPalette;
  end;

  { Header of IMG and CIF files.}
  TImgHeader = packed record
    XOff: Word;
    YOff: Word;
    Width: Word;
    Height: Word;
    Unk: Word;        // Might indicate compressed data or not
    ImageSize: Word;  // Size of Image data (but not always)
  end;

const
  { This is default Daggerfall's palette (C:\Dagger\Arena2\pal.pal).
    Every Daggerfall module loads this pal in constructor.}
  DaggerfallPalette: TElderPalette = (
    (B:   0; G:   0; R:   0), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255),
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255),
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255),
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255),
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255),
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255),
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255),
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255),
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255),
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255),
    (B: 255; G:   0; R: 255), (B: 255; G:   0; R: 255), (B: 244; G: 202; R: 167),
    (B: 227; G: 180; R: 144), (B: 207; G: 152; R: 118), (B: 193; G: 133; R: 100),
    (B: 180; G: 113; R:  80), (B: 165; G: 100; R:  70), (B: 152; G:  93; R:  63),
    (B: 140; G:  86; R:  55), (B: 129; G:  79; R:  48), (B: 122; G:  75; R:  43),
    (B: 112; G:  70; R:  40), (B: 103; G:  64; R:  39), (B:  91; G:  67; R:  38),
    (B:  79; G:  63; R:  43), (B:  66; G:  54; R:  41), (B:  54; G:  50; R:  40),
    (B: 232; G: 196; R: 196), (B: 220; G: 177; R: 177), (B: 204; G: 157; R: 157),
    (B: 188; G: 138; R: 138), (B: 175; G: 122; R: 122), (B: 155; G: 105; R: 106),
    (B: 143; G:  94; R:  97), (B: 126; G:  81; R:  89), (B: 109; G:  72; R:  88),
    (B: 101; G:  68; R:  85), (B:  86; G:  61; R:  77), (B:  75; G:  55; R:  71),
    (B:  67; G:  51; R:  63), (B:  63; G:  47; R:  56), (B:  56; G:  45; R:  52),
    (B:  46; G:  44; R:  46), (B: 245; G: 212; R: 172), (B: 229; G: 193; R: 150),
    (B: 213; G: 174; R: 128), (B: 196; G: 154; R: 105), (B: 183; G: 140; R:  88),
    (B: 173; G: 127; R:  78), (B: 160; G: 118; R:  74), (B: 151; G: 110; R:  69),
    (B: 134; G: 103; R:  65), (B: 123; G:  92; R:  60), (B: 109; G:  85; R:  54),
    (B:  96; G:  76; R:  51), (B:  83; G:  71; R:  44), (B:  69; G:  63; R:  42),
    (B:  61; G:  54; R:  38), (B:  50; G:  45; R:  34), (B: 205; G: 205; R: 224),
    (B: 188; G: 188; R: 199), (B: 165; G: 165; R: 174), (B: 145; G: 145; R: 159),
    (B: 135; G: 135; R: 149), (B: 122; G: 122; R: 137), (B: 114; G: 114; R: 127),
    (B: 103; G: 103; R: 116), (B:  94; G:  94; R: 109), (B:  85; G:  85; R:  96),
    (B:  75; G:  75; R:  85), (B:  68; G:  68; R:  80), (B:  61; G:  61; R:  67),
    (B:  53; G:  53; R:  59), (B:  48; G:  48; R:  50), (B:  44; G:  44; R:  45),
    (B: 176; G: 205; R: 255), (B: 147; G: 185; R: 244), (B: 123; G: 164; R: 230),
    (B: 104; G: 152; R: 217), (B:  87; G: 137; R: 205), (B:  68; G: 124; R: 192),
    (B:  68; G: 112; R: 179), (B:  62; G: 105; R: 167), (B:  55; G:  97; R: 154),
    (B:  49; G:  90; R: 142), (B:  45; G:  82; R: 122), (B:  51; G:  77; R: 102),
    (B:  52; G:  69; R:  87), (B:  50; G:  62; R:  73), (B:  47; G:  59; R:  60),
    (B:  44; G:  48; R:  49), (B: 220; G: 220; R: 220), (B: 197; G: 197; R: 197),
    (B: 185; G: 185; R: 185), (B: 174; G: 174; R: 174), (B: 162; G: 162; R: 162),
    (B: 147; G: 147; R: 147), (B: 132; G: 132; R: 132), (B: 119; G: 119; R: 119),
    (B: 110; G: 110; R: 110), (B:  99; G:  99; R:  99), (B:  87; G:  87; R:  87),
    (B:  78; G:  78; R:  78), (B:  67; G:  67; R:  67), (B:  58; G:  58; R:  58),
    (B:  51; G:  51; R:  51), (B:  44; G:  44; R:  44), (B: 182; G: 218; R: 227),
    (B: 158; G: 202; R: 202), (B: 134; G: 187; R: 187), (B: 109; G: 170; R: 170),
    (B:  87; G: 154; R: 154), (B:  77; G: 142; R: 142), (B:  70; G: 135; R: 135),
    (B:  62; G: 124; R: 124), (B:  54; G: 112; R: 112), (B:  46; G: 103; R: 103),
    (B:  39; G:  91; R:  91), (B:  40; G:  83; R:  83), (B:  45; G:  72; R:  72),
    (B:  47; G:  63; R:  63), (B:  50; G:  55; R:  55), (B:  45; G:  48; R:  48),
    (B: 255; G: 246; R: 103), (B: 241; G: 238; R:  45), (B: 226; G: 220; R:   0),
    (B: 212; G: 203; R:   0), (B: 197; G: 185; R:   0), (B: 183; G: 168; R:   0),
    (B: 168; G: 150; R:   0), (B: 154; G: 133; R:   0), (B: 139; G: 115; R:   0),
    (B: 127; G: 106; R:   4), (B: 116; G:  97; R:   7), (B: 104; G:  87; R:  11),
    (B:  93; G:  78; R:  14), (B:  81; G:  69; R:  18), (B:  69; G:  60; R:  21),
    (B:  58; G:  51; R:  25), (B: 202; G: 221; R: 196), (B: 175; G: 200; R: 168),
    (B: 148; G: 176; R: 141), (B: 123; G: 156; R: 118), (B: 107; G: 144; R: 109),
    (B:  93; G: 130; R:  94), (B:  82; G: 116; R:  86), (B:  77; G: 110; R:  78),
    (B:  68; G:  99; R:  67), (B:  61; G:  89; R:  53), (B:  52; G:  77; R:  45),
    (B:  46; G:  68; R:  37), (B:  39; G:  60; R:  39), (B:  30; G:  55; R:  30),
    (B:  34; G:  51; R:  34), (B:  40; G:  47; R:  40), (B: 179; G: 107; R:  83),
    (B: 175; G:  95; R:  75), (B: 175; G:  87; R:  67), (B: 163; G:  79; R:  59),
    (B: 155; G:  75; R:  51), (B: 147; G:  71; R:  47), (B: 155; G:  91; R:  47),
    (B: 139; G:  83; R:  43), (B: 127; G:  75; R:  39), (B: 115; G:  67; R:  35),
    (B:  99; G:  63; R:  31), (B:  87; G:  55; R:  27), (B:  75; G:  47; R:  23),
    (B:  59; G:  39; R:  19), (B:  47; G:  31; R:  15), (B:  35; G:  23; R:  11),
    (B: 216; G: 227; R: 162), (B: 185; G: 205; R: 127), (B: 159; G: 183; R: 101),
    (B: 130; G: 162; R:  77), (B: 109; G: 146; R:  66), (B: 101; G: 137; R:  60),
    (B:  92; G: 127; R:  54), (B:  84; G: 118; R:  48), (B:  76; G: 108; R:  42),
    (B:  65; G:  98; R:  37), (B:  53; G:  87; R:  34), (B:  51; G:  75; R:  35),
    (B:  45; G:  64; R:  37), (B:  43; G:  56; R:  39), (B:  38; G:  51; R:  40),
    (B:  43; G:  46; R:  45), (B: 179; G: 115; R:  79), (B: 175; G: 111; R:  75),
    (B: 171; G: 107; R:  71), (B: 167; G: 103; R:  67), (B: 159; G:  99; R:  63),
    (B: 155; G:  95; R:  59), (B: 151; G:  91; R:  55), (B: 143; G:  87; R:  51),
    (B:  40; G:  40; R:  40), (B:  38; G:  38; R:  38), (B:  35; G:  35; R:  35),
    (B:  31; G:  31; R:  31), (B:  27; G:  27; R:  27), (B:  23; G:  23; R:  23),
    (B:  19; G:  19; R:  19), (B:  15; G:  15; R:  15), (B: 254; G: 255; R: 199),
    (B: 254; G: 245; R: 185), (B: 254; G: 235; R: 170), (B: 254; G: 225; R: 156),
    (B: 255; G: 215; R: 141), (B: 255; G: 205; R: 127), (B: 255; G: 195; R: 112),
    (B: 255; G: 185; R:  98), (B: 255; G: 175; R:  83), (B: 241; G: 167; R:  54),
    (B: 234; G: 155; R:  50), (B: 226; G: 143; R:  46), (B: 219; G: 131; R:  43),
    (B: 212; G: 119; R:  39), (B: 205; G: 107; R:  35), (B: 198; G:  95; R:  31),
    (B: 190; G:  84; R:  27), (B: 183; G:  72; R:  23), (B: 176; G:  60; R:  19),
    (B: 169; G:  48; R:  15), (B: 162; G:  36; R:  12), (B: 154; G:  24; R:   8),
    (B: 147; G:  12; R:   4), (B: 130; G:  22; R:   0), (B: 111; G:  34; R:   0),
    (B: 102; G:  33; R:   1), (B:  92; G:  33; R:   3), (B:  83; G:  32; R:  10),
    (B:  74; G:  39; R:  27), (B:  65; G:  41; R:  33), (B:  57; G:  43; R:  39),
    (B:  48; G:  45; R:  45));

implementation

uses
  ImagingUtility, 
  ElderImageryBsi,
  ElderImageryCif,
  ElderImageryImg,
  ElderImageryTexture;

{ TDaggerfallFileFormat class implementation }

constructor TElderFileFormat.Create;
begin
  inherited Create;
  FCanLoad := True;
  FCanSave := True;
  FIsMultiImageFormat := True;

  GetMem(FARGBPalette, Length(FPalette) * SizeOf(TColor32Rec));
  SetPalette(DaggerfallPalette);
end;

destructor TElderFileFormat.Destroy;
begin
  FreeMem(FARGBPalette);
  inherited Destroy;
end;

procedure TElderFileFormat.DagRLEDecode(InData: Pointer; InSize, OutSize: LongInt;
  out OutData: Pointer);
var
  I, Pos, CByte: LongInt;
  Rle, B: Byte;
begin
  Pos := 0;
  CByte := 0;
  while Pos < OutSize do
  begin
    Rle := PByteArray(InData)[CByte];
    CByte := CByte + 1;
    if Rle < 128 then
    begin
      Rle := Rle + 1;
      Move(PByteArray(InData)[CByte], PByteArray(OutData)[Pos], Rle);
      CByte := CByte + Rle;
      Pos := Pos + Rle;
    end
    else
    begin
      Rle := Rle - 127;
      B := PByteArray(InData)[CByte];;
      CByte := CByte + 1;
      for I := 0 to Rle - 1 do
      begin
        PByteArray(OutData)[Pos] := B;
        Pos := Pos + 1;
      end;
    end;
  end;
end;

function TElderFileFormat.FindNoHeaderInfo(Size: LongInt;
  Infos: array of TNoHeaderFileInfo): LongInt;
var
  I: LongInt;
begin
  for I := Low(Infos) to High(Infos) do
  begin
    if Size = Infos[I].Size then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function TElderFileFormat.TestNoHeaderFormat(Handle: TImagingHandle): TDaggerfallFileFormatClass;
var
  InputSize, I: LongInt;
begin
  Result := nil;
  if Handle <> nil then
  begin
    InputSize := GetInputSize(Handle);
    // Check special IMG files
    I := FindNoHeaderInfo(InputSize, NoHeaderIMGInfos);
    if I >= 0 then
    begin
      Result := TIMGFileFormat;
      Exit;
    end;
    // Check special CIF files
    I := FindNoHeaderInfo(InputSize, NoHeaderCIFInfos);
    if I >= 0 then
    begin
      Result := TCIFFileFormat;
      Exit;
    end;
  end;
end;

procedure TElderFileFormat.ConvertPalette(const ElderPal: TElderPalette;
  ARGBPal: PPalette32);
var
  I: LongInt;
begin
  for I := Low(ElderPal) to High(ElderPal) do
  begin
    ARGBPal[I].A := $FF;
    ARGBPal[I].R := FPalette[I].B;
    ARGBPal[I].G := FPalette[I].G;
    ARGBPal[I].B := FPalette[I].R;
  end;
  // Palette index 0 represents transparent color
  ARGBPal[0].A := 0;
end;

function TElderFileFormat.GetInputSize(Handle: TImagingHandle): LongInt;
var
  OldPos: LongInt;
begin
  OldPos := GetIO.Tell(Handle);
  GetIO.Seek(Handle, 0, smFromEnd);
  Result := GetIO.Tell(Handle);
  GetIO.Seek(Handle, OldPos, smFromBeginning);
end;

procedure TElderFileFormat.SetPalette(const Value: TElderPalette);
begin
  FPalette := Value;
  ConvertPalette(FPalette, FARGBPalette);
end;

function TElderFileFormat.GetSupportedFormats: TImageFormats;
begin
  Result := [];
end;

function TElderFileFormat.MakeCompatible(const Image: TImageData;
  var Comp: TImageData; out MustBeFreed: Boolean): Boolean;
begin
  inherited MakeCompatible(Image, Comp, MustBeFreed);
  MapImageToPalette(Comp, FARGBPalette, Length(FPalette));
  MustBeFreed := True;
  Result := True;
end;

function TElderFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Hdr: TImgHeader;
  DagClass: TDaggerfallFileFormatClass;
  ReadCount: LongInt;
begin
  // TestFormat for both IMG and CIF formats
  Result := False;
  DagClass := TestNoHeaderFormat(Handle);
  if (DagClass = nil) and (Handle <> nil) then
  begin
    // Check ordinary IMG/CIF files with header
    ReadCount := GetIO.Read(Handle, @Hdr, SizeOf(Hdr));
    GetIO.Seek(Handle, -ReadCount, smFromCurrent);
    Result := (Hdr.ImageSize <= Hdr.Width * Hdr.Height) and
      (Hdr.Width * Hdr.Height <= High(Word)) and (Hdr.ImageSize <> 0) and
      (Hdr.Width <> 0) and (Hdr.Height <> 0);
    if FIsMultiImageFormat then
      Result := Result and (GetInputSize(Handle) > Hdr.ImageSize + SizeOf(Hdr))
    else
      Result := Result and (GetInputSize(Handle) = Hdr.ImageSize + SizeOf(Hdr));
  end
  else if DagClass = Self.ClassType then
    Result := True;
end;

initialization
  RegisterImageFileFormat(TBSIFileFormat);
  RegisterImageFileFormat(TCIFFileFormat);
  RegisterImageFileFormat(TIMGFileFormat);
  RegisterImageFileFormat(TTextureFileFormat);

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Added transparency to Daggerfall palettes.
    - Initial version created based on my older code.
}

end.
