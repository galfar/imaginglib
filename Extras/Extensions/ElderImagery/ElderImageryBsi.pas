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

{ This unit contains image format loader/saver NOT READY YET!!!!!!!!.}
unit ElderImageryBsi;

{$I ImagingOptions.inc}

interface

uses
  ImagingTypes, Imaging, ElderImagery, ImagingUtility;

type
  TBSIFileFormat = class(TElderFileFormat)
  private
    function IsMultiBSI(Handle: TImagingHandle): Boolean;
  protected
    function GetSupportedFormats: TImageFormats; override;
    procedure LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean); override;
    function MakeCompatible(const Image: TImageData; var Comp: TImageData;
      out MustBeFreed: Boolean): Boolean; override;
  public
    constructor Create; override;
    function TestFormat(Handle: TImagingHandle): Boolean; override;
  end;

const
  SBSIFormatName = 'Bethesda Image';
  SBSIMasks      = '*.bsi,texbsi.*';
  BSISupportedFormats: TImageFormats = [ifIndex8];

implementation

resourcestring
  SBadHeader = 'Invalid header chunk. IFHD expected.';
  SErrorLoadingChunk = 'Error when reading %s chunk data.';

type
  TChunk = packed record
    ChunkID: TChar4;
    DataSize: LongWord;
  end;

  TTextureBSIHeader = packed record
    Name: array[0..8] of Char;
    ImageSize: LongInt;
  end;

  TBHDRChunk = packed record
    OffsetX: Word;
    OffsetY: Word;
    Width: Word;
    Height: Word;
    Unk1, Unk2: Byte;
    Unk3, Unk4, Frames, Unk6, Unk7, Unk8: Word;
    Unk9, Unk10: Byte;
    Unk11: Word;
  end;

const
  IFHDSignature: TChar4 = 'IFHD';
  BSIFSignature: TChar4 = 'BSIF';
  BHDRSignature: TChar4 = 'BHDR';
  CMAPSignature: TChar4 = 'CMAP';
  HICLSignature: TChar4 = 'HICL';
  HTBLSignature: TChar4 = 'HTBL';
  DATASignature: TChar4 = 'DATA';
  ENDSignature:  TChar4 = 'END ';

  RedguardPalette : array[0..767] of Byte = (
    $00,$00,$00,$FF,$00,$FF,$FF,
    $00,$FF,$FF,$00,$FF,$FF,$00,$FF,$FF,$00,$FF,$FF,$00,$FF,$FF,
    $00,$FF,$FF,$00,$FF,$FF,$00,$FF,$FF,$00,$FF,$FF,$00,$FF,$FF,
    $00,$FF,$FF,$00,$FF,$FF,$00,$FF,$FF,$00,$FF,$85,$C4,$B7,$64,
    $B5,$99,$42,$A5,$7C,$21,$96,$5E,$1F,$8B,$57,$1C,$7F,$50,$1A,
    $74,$49,$18,$69,$42,$15,$5D,$3B,$13,$52,$34,$11,$47,$2D,$0E,
    $3B,$26,$0C,$30,$1F,$0A,$25,$18,$07,$19,$11,$05,$0E,$0A,$E6,
    $B3,$8E,$D8,$9B,$7F,$C7,$97,$88,$CD,$86,$76,$C7,$83,$67,$BF,
    $82,$6C,$CA,$71,$5F,$B4,$70,$5E,$C5,$5F,$4E,$B7,$6A,$4E,$AE,
    $60,$4B,$A0,$5B,$3F,$A6,$54,$37,$97,$5B,$36,$98,$4B,$31,$8E,
    $51,$33,$D8,$E3,$A2,$9B,$D4,$6D,$5F,$C6,$39,$22,$B7,$04,$20,
    $A9,$04,$1D,$9B,$04,$1B,$8D,$04,$19,$7F,$04,$16,$71,$04,$14,
    $64,$04,$12,$56,$03,$0F,$48,$03,$0D,$3A,$03,$0B,$2C,$03,$08,
    $1E,$03,$06,$10,$03,$86,$48,$39,$84,$47,$2F,$7A,$4B,$33,$7B,
    $3D,$2C,$77,$3B,$25,$67,$37,$29,$68,$2F,$1F,$62,$2F,$1B,$5B,
    $2D,$21,$53,$2A,$22,$4B,$28,$18,$50,$21,$16,$3F,$1D,$18,$42,
    $18,$10,$33,$1B,$18,$28,$18,$18,$FF,$F6,$67,$F1,$EE,$2D,$EB,
    $F7,$00,$E4,$E4,$03,$CC,$CF,$01,$BD,$BB,$02,$AD,$A6,$02,$9E,
    $92,$03,$8E,$7E,$03,$7F,$6A,$04,$72,$61,$09,$60,$51,$07,$4B,
    $3F,$06,$35,$2F,$06,$23,$1F,$06,$13,$12,$06,$B8,$74,$53,$AF,
    $60,$39,$A6,$4B,$1E,$9D,$37,$04,$91,$33,$04,$85,$2F,$04,$7A,
    $2B,$04,$6E,$27,$03,$62,$23,$03,$56,$1F,$03,$4A,$1A,$03,$3E,
    $16,$03,$33,$12,$03,$27,$0E,$02,$1B,$0A,$02,$0F,$06,$02,$FF,
    $FF,$B8,$FF,$F1,$89,$FF,$E2,$5A,$FF,$D4,$2B,$F0,$BD,$27,$E1,
    $A6,$23,$D3,$90,$1E,$C4,$79,$1A,$B5,$62,$16,$A3,$5C,$14,$7F,
    $49,$0F,$69,$3C,$0D,$53,$2E,$0C,$3D,$21,$0A,$27,$14,$08,$1A,
    $0F,$09,$FC,$CB,$B3,$F5,$BD,$9E,$DE,$A7,$85,$C4,$93,$6F,$BA,
    $86,$5B,$AE,$7D,$51,$A1,$76,$4E,$93,$6E,$48,$88,$66,$41,$7A,
    $5D,$3B,$6E,$55,$37,$62,$4F,$35,$55,$45,$2E,$42,$36,$25,$2E,
    $28,$1D,$1B,$19,$14,$E4,$85,$85,$E1,$60,$5E,$DE,$3A,$37,$DB,
    $15,$10,$CA,$14,$0F,$B9,$12,$0E,$A7,$11,$0D,$96,$10,$0C,$85,
    $0E,$0B,$74,$0D,$0B,$62,$0C,$0A,$51,$0A,$09,$40,$09,$08,$2F,
    $08,$07,$1D,$06,$06,$0C,$05,$05,$FF,$FF,$FF,$F0,$F0,$F0,$DC,
    $DC,$DC,$C9,$C9,$C9,$B5,$B5,$B5,$A2,$A2,$A2,$94,$94,$94,$87,
    $87,$87,$79,$79,$79,$6C,$6C,$6C,$5A,$5A,$5A,$4A,$4A,$4A,$3A,
    $3A,$3A,$2A,$2A,$2A,$16,$16,$16,$08,$08,$08,$68,$96,$E9,$5D,
    $7D,$F2,$52,$62,$F9,$48,$48,$FF,$30,$30,$FF,$19,$19,$FE,$07,
    $07,$F6,$07,$07,$DC,$06,$06,$C2,$06,$06,$A9,$05,$05,$8F,$05,
    $05,$75,$04,$04,$5B,$04,$04,$42,$03,$03,$28,$03,$03,$0E,$BF,
    $58,$75,$B4,$3F,$61,$A9,$26,$4E,$9F,$0E,$38,$93,$0D,$34,$87,
    $0C,$30,$7B,$0C,$2C,$6F,$0B,$28,$63,$0A,$24,$57,$09,$20,$4B,
    $08,$1B,$3F,$07,$17,$33,$07,$13,$27,$06,$0F,$1B,$05,$0B,$0F,
    $04,$07,$87,$E0,$FF,$5B,$D5,$FF,$2E,$C5,$FF,$02,$B8,$FF,$02,
    $AA,$EB,$02,$9C,$D7,$02,$8D,$C3,$02,$7F,$AF,$02,$71,$9B,$03,
    $63,$88,$03,$54,$74,$03,$46,$60,$03,$38,$4C,$03,$2A,$38,$03,
    $1B,$24,$03,$0D,$10,$FE,$FF,$C7,$FE,$EB,$AA,$FF,$D7,$8D,$FF,
    $CD,$7F,$FF,$C3,$70,$FF,$AF,$53,$EA,$9B,$32,$DB,$83,$2B,$CD,
    $6B,$23,$BE,$54,$1B,$B0,$3C,$13,$9B,$18,$0A,$82,$15,$09,$69,
    $13,$08,$50,$10,$07,$37,$0D,$06,$C5,$D7,$FF,$B5,$C4,$E9,$A5,
    $B1,$D4,$95,$9E,$BE,$8A,$92,$B0,$7E,$86,$A2,$73,$7A,$93,$67,
    $6E,$85,$5C,$62,$77,$51,$57,$69,$45,$4B,$5A,$3A,$3F,$4C,$2E,
    $33,$3E,$23,$27,$30,$17,$1B,$21,$0C,$0F,$13);

  BattleSpirePalette : array[0..767] of Byte = (
    $00,$00,$00,$3F,$39,$20,$3F,$33,$1A,$3F,$33,$18,$3D,$33,$1C,
    $3F,$33,$16,$3D,$33,$1A,$3B,$33,$1C,$39,$33,$1E,$3F,$31,$18,
    $3F,$31,$15,$39,$31,$1E,$37,$31,$20,$3D,$2F,$13,$34,$2E,$21,
    $39,$2C,$14,$2E,$2B,$24,$2C,$29,$25,$33,$27,$12,$2C,$28,$1E,
    $29,$27,$27,$2E,$25,$13,$28,$24,$1F,$29,$23,$17,$29,$20,$10,
    $23,$20,$1D,$22,$1E,$17,$21,$1D,$1A,$21,$1C,$14,$22,$1C,$10,
    $1D,$1A,$17,$1C,$17,$12,$3D,$32,$29,$38,$2D,$24,$33,$26,$1D,
    $30,$21,$19,$2D,$1C,$14,$29,$19,$11,$26,$17,$0F,$23,$15,$0D,
    $20,$13,$0C,$1E,$12,$0A,$1C,$11,$0A,$19,$10,$09,$16,$10,$09,
    $13,$0F,$0A,$10,$0D,$0A,$0D,$0C,$0A,$3A,$2F,$32,$37,$29,$2F,
    $33,$24,$2A,$2F,$1F,$27,$2B,$1B,$24,$26,$18,$20,$23,$15,$1D,
    $1F,$13,$1A,$1B,$11,$19,$19,$10,$18,$15,$0E,$13,$12,$0D,$11,
    $10,$0C,$0F,$0F,$0B,$0E,$0E,$0B,$0D,$0B,$0B,$0B,$3D,$35,$2B,
    $39,$30,$25,$35,$2B,$20,$31,$26,$1A,$2D,$23,$16,$2B,$1F,$13,
    $28,$1D,$12,$25,$1B,$11,$21,$19,$10,$1E,$17,$0F,$1B,$15,$0D,
    $18,$13,$0C,$14,$11,$0B,$11,$0F,$0A,$0F,$0D,$09,$0C,$0B,$08,
    $33,$33,$38,$2F,$2F,$31,$29,$29,$2B,$24,$24,$27,$21,$21,$25,
    $1E,$1E,$22,$1C,$1C,$1F,$19,$19,$1D,$17,$17,$1B,$15,$15,$18,
    $12,$12,$15,$11,$11,$14,$0F,$0F,$10,$0D,$0D,$0E,$0C,$0C,$0C,
    $0B,$0B,$0B,$2C,$33,$3F,$24,$2E,$3D,$1E,$29,$39,$1A,$26,$36,
    $15,$22,$33,$11,$1F,$30,$11,$1C,$2C,$0F,$1A,$29,$0D,$18,$26,
    $0C,$16,$23,$0B,$14,$1E,$0C,$13,$19,$0D,$11,$15,$0C,$0F,$12,
    $0B,$0E,$0F,$0B,$0C,$0C,$37,$37,$37,$31,$31,$31,$2E,$2E,$2E,
    $2B,$2B,$2B,$28,$28,$28,$24,$24,$24,$21,$21,$21,$1D,$1D,$1D,
    $1B,$1B,$1B,$18,$18,$18,$15,$15,$15,$13,$13,$13,$10,$10,$10,
    $0E,$0E,$0E,$0C,$0C,$0C,$0B,$0B,$0B,$2D,$36,$38,$27,$32,$32,
    $21,$2E,$2E,$1B,$2A,$2A,$15,$26,$26,$13,$23,$23,$11,$21,$21,
    $0F,$1F,$1F,$0D,$1C,$1C,$0B,$19,$19,$09,$16,$16,$0A,$14,$14,
    $0B,$12,$12,$0B,$0F,$0F,$0C,$0D,$0D,$0B,$0C,$0C,$3F,$3D,$19,
    $3C,$3B,$0B,$38,$37,$00,$35,$32,$00,$31,$2E,$00,$2D,$2A,$00,
    $2A,$25,$00,$26,$21,$00,$22,$1C,$00,$1F,$1A,$01,$1D,$18,$01,
    $1A,$15,$02,$17,$13,$03,$14,$11,$04,$11,$0F,$05,$0E,$0C,$06,
    $32,$37,$31,$2B,$32,$2A,$25,$2C,$23,$1E,$27,$1D,$1A,$24,$1B,
    $17,$20,$17,$14,$1D,$15,$13,$1B,$13,$11,$18,$10,$0F,$16,$0D,
    $0D,$13,$0B,$0B,$11,$09,$09,$0F,$09,$07,$0D,$07,$08,$0C,$08,
    $0A,$0B,$0A,$2C,$1A,$14,$2B,$17,$12,$2B,$15,$10,$28,$13,$0E,
    $26,$12,$0C,$24,$11,$0B,$26,$16,$0B,$22,$14,$0A,$1F,$12,$09,
    $1C,$10,$08,$18,$0F,$07,$15,$0D,$06,$12,$0B,$05,$0E,$09,$04,
    $0B,$07,$03,$08,$05,$02,$36,$38,$28,$2E,$33,$1F,$27,$2D,$19,
    $20,$28,$13,$1B,$24,$10,$19,$22,$0F,$17,$1F,$0D,$15,$1D,$0C,
    $13,$1B,$0A,$10,$18,$09,$0D,$15,$08,$0C,$12,$08,$0B,$10,$09,
    $0A,$0E,$09,$09,$0C,$0A,$0A,$0B,$0B,$2C,$1C,$13,$2B,$1B,$12,
    $2A,$1A,$11,$29,$19,$10,$27,$18,$0F,$26,$17,$0E,$25,$16,$0D,
    $23,$15,$0C,$0A,$0A,$0A,$09,$09,$09,$08,$08,$08,$07,$07,$07,
    $06,$06,$06,$05,$05,$05,$04,$04,$04,$03,$03,$03,$3F,$3F,$31,
    $3F,$3D,$2E,$3F,$3A,$2A,$3F,$38,$27,$3F,$35,$23,$3F,$33,$1F,
    $3F,$30,$1C,$3F,$2E,$18,$3F,$2B,$14,$3C,$29,$0D,$3A,$26,$0C,
    $38,$23,$0B,$36,$20,$0A,$35,$1D,$09,$33,$1A,$08,$31,$17,$07,
    $2F,$15,$06,$2D,$12,$05,$2C,$0F,$04,$2A,$0C,$03,$28,$09,$03,
    $26,$06,$02,$24,$03,$01,$20,$05,$00,$1B,$08,$00,$19,$08,$00,
    $17,$08,$00,$14,$08,$02,$12,$09,$06,$10,$0A,$08,$0E,$0A,$09,
    $00,$00,$00);


{ TPngFileFormat class implementation }

constructor TBSIFileFormat.Create;
begin
  inherited Create;
  FName := SBSIFormatName;
  FCanLoad := True;
  FCanSave := False;
  FIsMultiImageFormat := True;

  AddMasks(SBSIMasks);
  SetPalette(TElderPalette(RedguardPalette));
end;

function TBSIFileFormat.IsMultiBSI(Handle: TImagingHandle): Boolean;
var
  ReadCount, StartPos: LongInt;
  Sig: TChar4;
begin
  Result := False;
  if Handle <> nil then
  with GetIO do
  begin
    StartPos := Tell(Handle);
    // Redguard textures have 13 byte tex header and then IFHD or BSIF
    Seek(Handle, SizeOf(TTextureBSIHeader), smFromCurrent);
    ReadCount := Read(Handle, @Sig, SizeOf(Sig));
    Seek(Handle, StartPos, smFromBeginning);
    Result := Result or ((ReadCount = SizeOf(Sig)) and
      ((Sig = IFHDSignature) or (Sig = BSIFSignature)));
  end;
end;

function TBSIFileFormat.GetSupportedFormats: TImageFormats;
begin
  Result := BSISupportedFormats;
end;

procedure TBSIFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean);
var
  Chunk: TChunk;
  ChunkData: Pointer;
  DATASize: LongInt;
  BHDR: TBHDRChunk;
  PalLoaded: TElderPalette;
  HICL: PByteArray;
  HTBL: PWordArray;
  IsMulti: Boolean;
  TextureHdr: TTextureBSIHeader;
  PaletteFound: Boolean;

  procedure ReadChunk;
  begin
    GetIO.Read(Handle, @Chunk, SizeOf(Chunk));
    Chunk.DataSize := SwapEndianLongWord(Chunk.DataSize);
  end;

  procedure ReadChunkData;
  var
    ReadBytes: LongInt;
  begin
    FreeMemNil(ChunkData);
    GetMem(ChunkData, Chunk.DataSize);
    ReadBytes := GetIO.Read(Handle, ChunkData, Chunk.DataSize);
    if ReadBytes <> Chunk.DataSize then
      raise EImagingError.CreateFmt(SErrorLoadingChunk, [Chunk.ChunkID]);
  end;

  procedure SkipChunkData;
  begin
    GetIO.Seek(Handle, Chunk.DataSize, smFromCurrent);
  end;

  procedure LoadBHDR;
  begin
    ReadChunkData;
    if Chunk.DataSize <> SizeOf(BHDR) then
      raise EImagingError.CreateFmt(SErrorLoadingChunk, [Chunk.ChunkID]);
    BHDR := TBHDRChunk(ChunkData^);
  end;

  procedure LoadHICL;
  begin
    ReadChunkData;
    GetMem(HICL, Chunk.DataSize);
    Move(ChunkData^, HICL[0], Chunk.DataSize);
  end;

  procedure LoadHTBL;
  begin
    ReadChunkData;
//    if Chunk.DataSize <> SizeOf(BHDR) then
//      raise EImagingError.CreateFmt(SErrorLoadingChunk, [Chunk.ChunkID]);
    GetMem(HTBL, Chunk.DataSize);
    Move(ChunkData^, HTBL[0], Chunk.DataSize);
  end;

  procedure LoadCMAP;
  begin
    ReadChunkData;
    if Chunk.DataSize <> SizeOf(PalLoaded) then
      raise EImagingError.CreateFmt(SErrorLoadingChunk, [Chunk.ChunkID]);
    Move(ChunkData^, PalLoaded, Chunk.DataSize);
    PaletteFound := True;
  end;

  procedure LoadDATA;
  begin
    ReadChunkData;
    DATASize := Chunk.DataSize;
  end;

  function AddImage(Width, Height: LongInt): LongInt;
  begin
    Result := Length(Images);
    SetLength(Images, Length(Images) + 1);
    NewImage(Width, Height, ifIndex8, Images[Result]);
    if not PaletteFound then
      Move(FARGBPalette[0], Images[Result].Palette[0], Length(FPalette) * SizeOf(TColor32Rec))
    else
      ConvertPalette(PalLoaded, Images[Result].Palette);
  end;

  function AddImageHiColor(Width, Height: LongInt): LongInt;
  begin
    Result := Length(Images);
    SetLength(Images, Length(Images) + 1);
    NewImage(Width, Height, ifX1R5G5B5, Images[Result]);
{    if not PaletteFound then
      Move(FARGBPalette[0], Images[Result].Palette[0], Length(FPalette) * SizeOf(TColor32Rec))
    else
      ConvertPalette(PalLoaded, Images[Result].Palette);
 } end;

  procedure Reconstruct;
  var
    Index, I, J: LongInt;
    RowOffsets: PLongWordArray;
    PW: PWord;
    PB: PByte;
  begin
    if HTBL = nil then
    begin
      if BHDR.Frames = 1 then
      begin
        // Load simple image
        Index := AddImage(BHDR.Width, BHDR.Height);
        Move(ChunkData^, Images[Index].Bits^, Images[Index].Size);
      end
      else
      begin
        // Load animated image:
        // At the beggining of the chunk data there is BHDR.Height * BHDR.Frames
        // 32bit offsets. Each BHDR.Height offsets point to rows of the current frame
        RowOffsets := @PByteArray(ChunkData)[0];

        for I := 0 to BHDR.Frames - 1 do
        begin
          Index := AddImage(BHDR.Width, BHDR.Height);
          for J := 0 to BHDR.Height - 1 do
          begin
            Move(PByteArray(ChunkData)[RowOffsets[I * BHDR.Height + J]],
              PByteArray(Images[Index].Bits)[J * Images[Index].Width], Images[Index].Width);
          end;
        end;


      end;
    end
    else
    begin
      ConvertPalette(PalLoaded, FARGBPalette);

      Index := AddImageHiColor(BHDR.Width, BHDR.Height);
      with Images[Index] do
      for I := 0 to DATASize - 1 do
      begin
        PWordArray(Bits)[I] := HTBL[HICL[PByteArray(ChunkData)[I]] * 15 + 10];
//        PPalette24(Bits)[I] :=
//          FARGBPalette[HTBL[8*256 + PByteArray(ChunkData)[I]]].Color24Rec;
//        HTBL[8*256 + FARGBPalette[PByteArray(ChunkData)[I]]];
      end;

    end;

{    if Pal <> nil then
      for I := 0 to CMAPSize div 3 - 1 do
      with  Images[0].Palette[I] do
      begin
        R := Pal[I].B;
        G := Pal[I].G;
        B := Pal[I].R;

        R := I;
        G := I;
        B := I;

{        R := HICL[Pal[I].R];
        G := HICL[Pal[I].G];
        B := HICL[Pal[I].B];
}    //  end;
   {   PW := Images[0].Bits;
      PB := Chunkdata;
      for I := 0 to Images[0].Size div 2 - 1 do
      with Images[0].Palette[I] do
      begin


        //PW^ := PWordArray(HICL)[3840+pb^];

        PW^ := {Pal[PB^].R *Pal[HICL[pb^]].R ;// or
        //Pal[PB^].g *HICL[pb^] or Pal[PB^].b *HICL[pb^] ;


        Inc(PB);
        Inc(PW);
      end;  }
  end;

  procedure ReadTextureHeader;
  begin
    FillChar(TextureHdr, SizeOf(TextureHdr), 0);
    if IsMulti then
      GetIO.Read(Handle, @TextureHdr, SizeOf(TextureHdr))
    else if Length(Images) = 0 then
      // Ensure that while loop that reads chunks is executed for
      // single-image files
      TextureHdr.ImageSize := 1;
  end;

begin
  ChunkData := nil;
  HICL := nil;
  HTBL := nil;
  SetLength(Images, 0);
  IsMulti := IsMultiBSI(Handle);
  if not IsMulti then
    // Use Battlespire palette as default for single-image BSIs
    ConvertPalette(TElderPalette(BattleSpirePalette), FARGBPalette);
  with GetIO do
  begin
    ReadTextureHeader;

    while TextureHdr.ImageSize > 0 do
    try
      PaletteFound := False;
      ReadChunk;
      SkipChunkData;

      repeat
        ReadChunk;
        if Chunk.ChunkID = BHDRSignature then
          LoadBHDR
        else if Chunk.ChunkID = HICLSignature then
          LoadHICL
        else if Chunk.ChunkID = HTBLSignature then
          LoadHTBL
        else if Chunk.ChunkID = CMAPSignature then
          LoadCMAP
        else if Chunk.ChunkID = DATASignature then
          LoadDATA
        else
          SkipChunkData;
      until Eof(Handle) or (Chunk.ChunkID = ENDSignature);

      Reconstruct;

      ReadTextureHeader;
    finally
      FreeMemNil(ChunkData);
      FreeMemNil(HICL);
      FreeMemNil(HTBL);
    end;
  end;
  if not IsMulti then
    // Copy current palette back to ARGB format 
    ConvertPalette(FPalette, FARGBPalette);
end;

function TBSIFileFormat.MakeCompatible(const Image: TImageData;
  var Comp: TImageData; out MustBeFreed: Boolean): Boolean;
begin
  if not inherited MakeCompatible(Image, Comp, MustBeFreed) then
    ConvertImage(Comp, ifIndex8);
  Result := Comp.Format in GetSupportedFormats;
end;

function TBSIFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  ReadCount: LongInt;
  Sig: TChar4;
begin
  // First check if have multi-image BSI file (Redguard textures)
  Result := IsMultiBSI(Handle);
  if not Result and (Handle <> nil) then
  with GetIO do
  begin
    // Check standard Bettlespire images with IFHD chunk at
    // the beginning of the file
    ReadCount := Read(Handle, @Sig, SizeOf(Sig));
    Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount = SizeOf(Sig)) and (Sig = IFHDSignature);
  end;
end;


{
  Changes/Bug Fixes:

  -- 0.21 -----------------------------------------------------
    - Added support for animated Redguard textures.
    - Added support for Redguard textures (Battlespire images still don't figured out).
    - Updated to current Imaging version.

  -- 0.13 -----------------------------------------------------
    - TBSIFileFormat class added

}

end.
