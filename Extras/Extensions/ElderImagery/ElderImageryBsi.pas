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
  SysUtils, ImagingTypes, Imaging, ImagingUtility;

type
  TBSIFileFormat = class(TImageFileFormat)
  protected
    function GetSupportedFormats: TImageFormats; override;
    procedure LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean); override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
    function MakeCompatible(const Image: TImageData; var Comp: TImageData;
      out MustBeFreed: Boolean): Boolean; override;
  public
    constructor Create; override;
    function TestFormat(Handle: TImagingHandle): Boolean; override;
  end;

const
  sBSIExtensions = 'bsi';
  sBSIFormatName = 'Bethesda Image';
  BSISupportedFormats: TImageFormats = [ifIndex8];

implementation

resourcestring
  sBadHeader = 'Invalid header chunk. IFHD expected.';
  sErrorLoadingChunk = 'Error when reading %s chunk data.';

type
  TChar4 = array[0..3] of Char;

  TChunk = packed record
    ChunkID: TChar4;
    DataSize: LongWord;
  end;

  TBHDRChunk = packed record
    OffsetX: Word;
    OffsetY: Word;
    Width: Word;
    Height: Word;
    Unk1, Unk2: Byte;
    Unk3, Unk4, Unk5, Unk6, Unk7, Unk8: Word;
    Unk9, Unk10: Byte;
    Unk11: Word;
  end;

const
  IFHDSignature: TChar4 = 'IFHD';
  EndSignature: TChar4 = 'END ';
  BHDRSignature: TChar4 = 'BHDR';
  CMAPSignature: TChar4 = 'CMAP';
  DATASignature: TChar4 = 'DATA';

  CMAPSize = 768;


{ TPngFileFormat class implementation }

constructor TBSIFileFormat.Create;
begin
  inherited Create;
  FName := sBSIFormatName;
  FCanLoad := True;
  FCanSave := not True;
  FIsMultiImageFormat := False;

  AddExtensions(sBSIExtensions);
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
  Pal: PPalette24;
  HICL: PByteArray;
  HTBL: PByteArray;

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
      raise EImagingError.CreateFmt(sErrorLoadingChunk, [Chunk.ChunkID]);
  end;

  procedure SkipChunkData;
  begin
    GetIO.Seek(Handle, Chunk.DataSize, smFromCurrent);
  end;

  procedure LoadBHDR;
  begin
    ReadChunkData;
    if Chunk.DataSize <> SizeOf(BHDR) then
      raise EImagingError.CreateFmt(sErrorLoadingChunk, [Chunk.ChunkID]);
    BHDR := TBHDRChunk(ChunkData^);
  end;

  procedure LoadHICL;
  begin
    ReadChunkData;
    GetMem(HICL, Chunk.DataSize);
    Move(ChunkData^, HICL^, Chunk.DataSize);
  end;

  procedure LoadCMAP;
  begin
    ReadChunkData;
    if Chunk.DataSize <> CMAPSize then
      raise EImagingError.CreateFmt(sErrorLoadingChunk, [Chunk.ChunkID]);
    GetMem(Pal, Chunk.DataSize);
    Move(ChunkData^, Pal^, Chunk.DataSize);
  end;

  procedure LoadDATA;
  begin
    ReadChunkData;
    DATASize := Chunk.DataSize;
  end;

  procedure Reconstruct;
  var
    I: LongInt;
    PW: PWord;
    PB: PByte;
  begin
    NewImage(BHDR.Width, BHDR.Height, ifIndex8, Images[0]);
    Move(ChunkData^, Images[0].Bits^, Images[0].Width * Images[0].Height);
    if Pal <> nil then
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
}      end;
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

begin
  ChunkData := nil;
  Pal := nil;
  HICL := nil;
  SetLength(Images, 1);
  with GetIO, Images[0] do
  try
    ReadChunk;
    SkipChunkData;
    if (Chunk.ChunkID <> IFHDSignature) then
      raise EImagingError.Create(sBadHeader);

    repeat
      ReadChunk;
      if Chunk.ChunkID = BHDRSignature then
        LoadBHDR
      else
      if Chunk.ChunkID = {HICLSignature}'HICL' then
        LoadHICL
      else
      if Chunk.ChunkID = CMAPSignature then
        LoadCMAP
      else
      if Chunk.ChunkID = DATASignature then
        LoadDATA
      else
        SkipChunkData;
    until Eof(Handle) or (Chunk.ChunkID = ENDSignature);
    Reconstruct;
    
  finally
    FreeMem(ChunkData);
    FreeMem(Pal);
    FreeMem(HICL);
  end;
end;

function TBSIFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
begin
//  inherited;

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
  Result := False;
  if Handle <> nil then
    with GetIO do
    begin
      FillChar(Sig, SizeOf(Sig), 0);
      ReadCount := Read(Handle, @Sig, SizeOf(Sig));
      Seek(Handle, -ReadCount, smFromCurrent);
      Result := (ReadCount = SizeOf(Sig)) and (Sig = IFHDSignature);
    end;
end;


{
  Changes/Bug Fixes:

  -- 0.13 -----------------------------------------------------
    - TBSIFileFormat class added

}

end.
