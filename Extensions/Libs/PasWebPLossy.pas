{
  PasWebP - pure Pascal WebP codec
  Part of Vampyre Imaging Library
  by Marek Mauder
  https://github.com/galfar/imaginglib
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.

  The encoder is written for Imaging. It follows the lossy encoder of libwebp
  in what the format prescribes and in several choices (forward transforms,
  quantization, mapping of quality, the way probabilities are updated) and
  uses its constants:

    Copyright (c) 2010, Google Inc. All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:

      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in
        the documentation and/or other materials provided with the
        distribution.

      * Neither the name of Google nor the names of its contributors may
        be used to endorse or promote products derived from this software
        without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}

{ Lossy WebP (VP8 key frame) encoder.

  The image is converted to Y, U, V planes (chroma at half resolution) and
  coded in macroblocks of 16x16 pixels. Each one is predicted from pixels
  already coded: luma as a whole (4 modes) or in 4x4 blocks (10 modes each),
  chroma by one of 4 modes; the choice minimizes distortion + lambda * bits.
  Differences from the prediction go through DCT, are quantized and coded by
  the boolean arithmetic coder with probabilities fitted to the image.
  The decoder smooths edges of blocks by the loop filter, its strength
  is set by the quantizer.

  Pixels of a macroblock are made by the routines of the decoder
  (PasWebPDec), so the encoder predicts from exactly what will be decoded.

  One segment and one partition are used: no adaptive quantization.
  Alpha channel is stored lossless in ALPH chunk (PasWebPLossless).

  Format: RFC 6386, https://developers.google.com/speed/webp/docs/riff_container

  FPC and Delphi 2009+. }
unit PasWebPLossy;

{$I PasWebP.inc}

interface

uses
  PasWebPLossless;

{ Encodes image to lossy WebP file in memory. Rows of Width pixels are
  Stride bytes apart, pixels are B, G, R, A bytes when WithAlpha is True
  and B, G, R bytes otherwise. Quality is 0..100 (as of libwebp: 75 is
  the usual choice), Effort 0..9 trades time for size.
  Alpha is kept exactly; when all pixels are opaque no alpha is stored.
  Returns False for wrong parameters and for images whose first partition
  would not fit the format's limit of 512 KB (hundreds of megapixels). }
function PasWebPEncodeLossy(Pixels: PByte; Stride: NativeInt; Width, Height: Integer;
  WithAlpha: Boolean; Quality, Effort: Integer; out Data: TPasWebPBytes): Boolean;

implementation

uses
  PasWebPDec;

const
  NUM_PROBA_IDS = NUM_TYPES * NUM_BANDS * NUM_CTX * NUM_PROBAS;
  MAX_LEVEL = 2047;
  QFIX = 17;
  // Tokens: bit 15 is the coded bit, bit 14 says the rest is a probability
  // (otherwise it is an index to the probabilities of coefficients)
  TOKEN_BIT = $8000;
  TOKEN_CONST = $4000;
  MAX_PARTITION0_SIZE = 1 shl 19;

  // Types of coefficient blocks
  TYPE_I16_AC = 0;   // luma of a 16x16 predicted macroblock, without DC
  TYPE_I16_DC = 1;   // its DC coefficients, transformed once more
  TYPE_CHROMA = 2;
  TYPE_I4 = 3;       // luma of 4x4 predicted blocks

type
  TLevels = array[0..15] of Int16;  // quantized coefficients in zigzag order
  TCoeffs = array[0..15] of Int16;  // coefficients as the transforms have them

  TQuant = record
    Q: array[0..1] of Integer;      // DC, AC step
    IQ: array[0..1] of Integer;     // reciprocals
    Bias: array[0..1] of Integer;   // rounding
    MaxLevel: array[0..1] of Integer;
  end;

  TMBInfo = packed record
    IsI4: Boolean;
    Skip: Boolean;
    YMode: Byte;
    UVMode: Byte;
    Modes: array[0..15] of Byte;  // of 4x4 blocks
  end;

  { Boolean arithmetic encoder (VP8BitWriter of libwebp). }
  TBoolWriter = record
    Buf: TPasWebPBytes;
    Pos: NativeInt;
    Range: Integer;   // minus one
    Value: Integer;
    Run: Integer;     // bytes $FF waiting for a possible carry
    NbBits: Integer;
  end;

  TNzContext = array[0..8] of Byte;  // 4 luma, 2 + 2 chroma, DC block

  TEncoder = record
    Width, Height, MbW, MbH: Integer;
    Y, U, V: TPasWebPBytes;       // source planes, sizes are whole macroblocks
    YStride, UVStride: Integer;
    QIndex: Integer;
    Y1, Y2, UV: TQuant;
    LambdaY, LambdaUV: Double;
    Candidates: Integer;          // 4x4 modes that are tried by coding the block
    TryI4: Boolean;
    FilterLevel: Integer;
    // Macroblock being coded with its surroundings, as the decoder has it
    YuvBuf: array[0..YUV_SIZE - 1] of Byte;
    TopY, TopU, TopV: TPasWebPBytes;
    // Which neighbor blocks have coefficients, modes of neighbor 4x4 blocks
    TopNz: array of TNzContext;
    LeftNz: TNzContext;
    IntraT: TPasWebPBytes;
    IntraL: array[0..3] of Byte;
    MBs: array of TMBInfo;
    NumSkipped: Integer;
    // Coefficients as bits with their probabilities, written at the end
    Tokens: array of Word;
    NumTokens: NativeInt;
    Stats: array[0..NUM_PROBA_IDS - 1, 0..1] of Cardinal;  // zeros, ones
    Probas: array[0..NUM_PROBA_IDS - 1] of Byte;
  end;

var
  // Cost of a bit coded with given probability of zero, in 1/256 of a bit
  BitCost: array[Boolean, 0..255] of Integer;
  // Tables of the format as flat arrays indexed by ProbaId + node
  DefaultProbas, UpdateProbas: array[0..NUM_PROBA_IDS - 1] of Byte;
  NormShift: array[0..127] of Byte;

procedure InitTables;
const
  InvLn2: Double = 1.4426950408889634;
  Scale: Double = 256.0;
var
  P, T, B, C, I, S: Integer;
begin
  BitCost[False, 0] := 16 * 256;
  BitCost[True, 0] := 0;
  for P := 1 to 255 do
  begin
    BitCost[False, P] := Round(-Scale * Ln(P / Scale) * InvLn2);
    BitCost[True, P] := Round(-Scale * Ln((Scale - P) / Scale) * InvLn2);
  end;

  for T := 0 to NUM_TYPES - 1 do
    for B := 0 to NUM_BANDS - 1 do
      for C := 0 to NUM_CTX - 1 do
        for I := 0 to NUM_PROBAS - 1 do
        begin
          P := NUM_PROBAS * (C + NUM_CTX * (B + NUM_BANDS * T)) + I;
          DefaultProbas[P] := CoeffsProba0[T, B, C, I];
          UpdateProbas[P] := CoeffsUpdateProba[T, B, C, I];
        end;

  for I := 0 to 127 do
  begin
    S := 0;
    while ((I + 1) shl S) < 128 do
      Inc(S);
    NormShift[I] := S;
  end;
end;

function ProbaId(BlockType, Band, Ctx: Integer): Integer; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := NUM_PROBAS * (Ctx + NUM_CTX * (Band + NUM_BANDS * BlockType));
end;

{ Arithmetic shift right of a signed value. }
function SarLong(V, N: Integer): Integer; {$IFDEF FPC}inline;{$ENDIF}
begin
{$IFDEF FPC}
  Result := SarLongint(V, N);
{$ELSE}
  if V >= 0 then Result := V shr N
  else Result := not (not V shr N);
{$ENDIF}
end;

{ Boolean encoder }

procedure BoolInit(var BW: TBoolWriter; Capacity: NativeInt);
begin
  SetLength(BW.Buf, Capacity + 16);
  BW.Pos := 0;
  BW.Range := 255 - 1;
  BW.Value := 0;
  BW.Run := 0;
  BW.NbBits := -8;
end;

procedure BoolFlush(var BW: TBoolWriter);
var
  S, Bits, Fill: Integer;
begin
  S := 8 + BW.NbBits;
  Bits := BW.Value shr S;
  BW.Value := BW.Value - (Bits shl S);
  Dec(BW.NbBits, 8);

  if (Bits and $FF) <> $FF then
  begin
    if BW.Pos + BW.Run + 2 > Length(BW.Buf) then
      SetLength(BW.Buf, Length(BW.Buf) + Length(BW.Buf) shr 1 + BW.Run + 1024);

    // Carry goes over the waiting $FF bytes to the byte before them
    if (Bits and $100) <> 0 then
      if BW.Pos > 0 then
        Inc(BW.Buf[BW.Pos - 1]);

    if BW.Run > 0 then
    begin
      if (Bits and $100) <> 0 then Fill := 0 else Fill := $FF;
      while BW.Run > 0 do
      begin
        BW.Buf[BW.Pos] := Fill;
        Inc(BW.Pos);
        Dec(BW.Run);
      end;
    end;

    BW.Buf[BW.Pos] := Bits and $FF;
    Inc(BW.Pos);
  end
  else
    Inc(BW.Run);
end;

procedure PutBit(var BW: TBoolWriter; Bit: Boolean; Prob: Integer); {$IFDEF FPC}inline;{$ENDIF}
var
  Split, Shift: Integer;
begin
  Split := (BW.Range * Prob) shr 8;
  if Bit then
  begin
    Inc(BW.Value, Split + 1);
    Dec(BW.Range, Split + 1);
  end
  else
    BW.Range := Split;

  if BW.Range < 127 then
  begin
    Shift := NormShift[BW.Range];
    BW.Range := ((BW.Range + 1) shl Shift) - 1;
    BW.Value := BW.Value shl Shift;
    Inc(BW.NbBits, Shift);
    if BW.NbBits > 0 then
      BoolFlush(BW);
  end;
end;

{ Count bits of Value with both values equally likely, the highest first. }
procedure PutBits(var BW: TBoolWriter; Value, Count: Integer);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    PutBit(BW, (Value shr I) and 1 <> 0, 128);
end;

function BoolFinish(var BW: TBoolWriter): NativeInt;
begin
  PutBits(BW, 0, 9 - BW.NbBits);
  BW.NbBits := 0;
  BoolFlush(BW);
  Result := BW.Pos;
end;

{ Transforms and quantization }

{ DCT of the difference of a 4x4 block and its prediction (FTransform of libwebp). }
procedure ForwardDCT(Src: PByte; SrcStride: Integer; Pred: PByte; PredStride: Integer;
  var Coeffs: TCoeffs);
var
  I, D0, D1, D2, D3, A0, A1, A2, A3: Integer;
  Tmp: array[0..15] of Integer;
begin
  for I := 0 to 3 do
  begin
    D0 := Src[0] - Pred[0];
    D1 := Src[1] - Pred[1];
    D2 := Src[2] - Pred[2];
    D3 := Src[3] - Pred[3];
    A0 := D0 + D3;
    A1 := D1 + D2;
    A2 := D1 - D2;
    A3 := D0 - D3;
    Tmp[0 + I * 4] := (A0 + A1) * 8;
    Tmp[1 + I * 4] := SarLong(A2 * 2217 + A3 * 5352 + 1812, 9);
    Tmp[2 + I * 4] := (A0 - A1) * 8;
    Tmp[3 + I * 4] := SarLong(A3 * 2217 - A2 * 5352 + 937, 9);
    Inc(Src, SrcStride);
    Inc(Pred, PredStride);
  end;

  for I := 0 to 3 do
  begin
    A0 := Tmp[0 + I] + Tmp[12 + I];
    A1 := Tmp[4 + I] + Tmp[8 + I];
    A2 := Tmp[4 + I] - Tmp[8 + I];
    A3 := Tmp[0 + I] - Tmp[12 + I];
    Coeffs[0 + I] := SarLong(A0 + A1 + 7, 4);
    Coeffs[4 + I] := SarLong(A2 * 2217 + A3 * 5352 + 12000, 16) + Ord(A3 <> 0);
    Coeffs[8 + I] := SarLong(A0 - A1 + 7, 4);
    Coeffs[12 + I] := SarLong(A3 * 2217 - A2 * 5352 + 51000, 16);
  end;
end;

{ Walsh-Hadamard transform of DC coefficients of the 16 blocks of a macroblock. }
procedure ForwardWHT(const DC: TCoeffs; var Coeffs: TCoeffs);
var
  I, A0, A1, A2, A3, B0, B1, B2, B3: Integer;
  Tmp: array[0..15] of Integer;
begin
  for I := 0 to 3 do
  begin
    A0 := DC[I * 4 + 0] + DC[I * 4 + 2];
    A1 := DC[I * 4 + 1] + DC[I * 4 + 3];
    A2 := DC[I * 4 + 1] - DC[I * 4 + 3];
    A3 := DC[I * 4 + 0] - DC[I * 4 + 2];
    Tmp[0 + I * 4] := A0 + A1;
    Tmp[1 + I * 4] := A3 + A2;
    Tmp[2 + I * 4] := A3 - A2;
    Tmp[3 + I * 4] := A0 - A1;
  end;

  for I := 0 to 3 do
  begin
    A0 := Tmp[0 + I] + Tmp[8 + I];
    A1 := Tmp[4 + I] + Tmp[12 + I];
    A2 := Tmp[4 + I] - Tmp[12 + I];
    A3 := Tmp[0 + I] - Tmp[8 + I];
    B0 := A0 + A1;
    B1 := A3 + A2;
    B2 := A3 - A2;
    B3 := A0 - A1;
    Coeffs[0 + I] := SarLong(B0, 1);
    Coeffs[4 + I] := SarLong(B1, 1);
    Coeffs[8 + I] := SarLong(B2, 1);
    Coeffs[12 + I] := SarLong(B3, 1);
  end;
end;

procedure SetQuant(var Quant: TQuant; DCStep, ACStep, DCBias, ACBias: Integer);
var
  I: Integer;
begin
  Quant.Q[0] := DCStep;
  Quant.Q[1] := ACStep;
  Quant.Bias[0] := DCBias shl (QFIX - 8);
  Quant.Bias[1] := ACBias shl (QFIX - 8);

  for I := 0 to 1 do
  begin
    Quant.IQ[I] := (1 shl QFIX) div Quant.Q[I];
    // The decoder keeps level * step in 16 bits
    Quant.MaxLevel[I] := 32767 div Quant.Q[I];
    if Quant.MaxLevel[I] > MAX_LEVEL then
      Quant.MaxLevel[I] := MAX_LEVEL;
  end;
end;

{ Quantizes coefficients from position First of the zigzag order on (the
  ones before it get zero) and puts what the decoder will get of them
  back to Coeffs. Returns True when any level is not zero. }
function Quantize(var Coeffs: TCoeffs; var Levels: TLevels; First: Integer;
  const Quant: TQuant): Boolean;
var
  N, J, K, C, Level: Integer;
begin
  Result := False;
  if First > 0 then
    Levels[0] := 0;

  for N := First to 15 do
  begin
    J := kZigzag[N];
    K := Ord(J > 0);
    C := Coeffs[J];
    Level := (Abs(C) * Quant.IQ[K] + Quant.Bias[K]) shr QFIX;

    if Level > 0 then
    begin
      if Level > Quant.MaxLevel[K] then
        Level := Quant.MaxLevel[K];
      if C < 0 then
        Level := -Level;
      Result := True;
    end;

    Levels[N] := Level;
    Coeffs[J] := Level * Quant.Q[K];
  end;
end;

function BlockSSE(A: PByte; AStride: Integer; B: PByte; BStride, Size: Integer): Integer;
var
  X, Y, D: Integer;
begin
  Result := 0;
  for Y := 0 to Size - 1 do
  begin
    for X := 0 to Size - 1 do
    begin
      D := A[X] - B[X];
      Inc(Result, D * D);
    end;
    Inc(A, AStride);
    Inc(B, BStride);
  end;
end;

{ Coefficients as bits }

procedure AddToken(var E: TEncoder; Token: Integer); {$IFDEF FPC}inline;{$ENDIF}
begin
  if E.NumTokens = Length(E.Tokens) then
    SetLength(E.Tokens, Length(E.Tokens) + Length(E.Tokens) shr 1 + 4096);
  E.Tokens[E.NumTokens] := Token;
  Inc(E.NumTokens);
end;

{ Goes through the bits that code a block of quantized coefficients (from
  position First, in the context Ctx: number of the two neighbor blocks
  that have coefficients). With Emit the bits are added to the tokens and
  counted for the probabilities, otherwise their cost by the default
  probabilities is summed (in 1/256 of a bit).
  Returns True when the block has a coefficient. }
function CodeCoeffs(var E: TEncoder; BlockType, Ctx, First: Integer; const Levels: TLevels;
  Emit: Boolean; var Cost: Integer): Boolean;
var
  N, Last, V, C, Base, Mask, Residue, I: Integer;
  Cat: PByte;

  function Bit(B: Boolean; Id: Integer): Boolean; {$IFDEF FPC}inline;{$ENDIF}
  begin
    if Emit then
    begin
      AddToken(E, Id or (Ord(B) * TOKEN_BIT));
      Inc(E.Stats[Id, Ord(B)]);
    end
    else
      Inc(Cost, BitCost[B, DefaultProbas[Id]]);
    Result := B;
  end;

  procedure ConstBit(B: Boolean; Prob: Integer); {$IFDEF FPC}inline;{$ENDIF}
  begin
    if Emit then
      AddToken(E, Prob or TOKEN_CONST or (Ord(B) * TOKEN_BIT))
    else
      Inc(Cost, BitCost[B, Prob]);
  end;

begin
  Result := False;
  Last := -1;
  for N := 15 downto First do
    if Levels[N] <> 0 then
    begin
      Last := N;
      Break;
    end;

  N := First;
  Base := ProbaId(BlockType, kBands[N], Ctx);
  // Is there any coefficient?
  if not Bit(Last >= 0, Base) then
    Exit;
  Result := True;

  while N < 16 do
  begin
    C := Levels[N];
    Inc(N);
    V := Abs(C);

    if not Bit(V <> 0, Base + 1) then
    begin
      Base := ProbaId(BlockType, kBands[N], 0);
      Continue;
    end;

    if not Bit(V > 1, Base + 2) then
      Base := ProbaId(BlockType, kBands[N], 1)
    else
    begin
      if not Bit(V > 4, Base + 3) then
      begin
        if Bit(V <> 2, Base + 4) then
          Bit(V = 4, Base + 5);
      end
      else if not Bit(V > 10, Base + 6) then
      begin
        if not Bit(V > 6, Base + 7) then
          ConstBit(V = 6, 159)
        else
        begin
          ConstBit(V >= 9, 165);
          ConstBit(V and 1 = 0, 145);
        end;
      end
      else
      begin
        // Larger values: category and the bits over its start
        Residue := V - 3;
        if Residue < (8 shl 1) then
        begin
          Bit(False, Base + 8);
          Bit(False, Base + 9);
          Dec(Residue, 8 shl 0);
          Mask := 1 shl 2;
          Cat := @kCat3[0];
        end
        else if Residue < (8 shl 2) then
        begin
          Bit(False, Base + 8);
          Bit(True, Base + 9);
          Dec(Residue, 8 shl 1);
          Mask := 1 shl 3;
          Cat := @kCat4[0];
        end
        else if Residue < (8 shl 3) then
        begin
          Bit(True, Base + 8);
          Bit(False, Base + 10);
          Dec(Residue, 8 shl 2);
          Mask := 1 shl 4;
          Cat := @kCat5[0];
        end
        else
        begin
          Bit(True, Base + 8);
          Bit(True, Base + 10);
          Dec(Residue, 8 shl 3);
          Mask := 1 shl 10;
          Cat := @kCat6[0];
        end;

        I := 0;
        while Mask <> 0 do
        begin
          ConstBit(Residue and Mask <> 0, Cat[I]);
          Inc(I);
          Mask := Mask shr 1;
        end;
      end;

      Base := ProbaId(BlockType, kBands[N], 2);
    end;

    ConstBit(C < 0, 128);
    // Is there another coefficient?
    if (N = 16) or not Bit(N <= Last, Base) then
      Exit;
  end;
end;

{ Costs of modes, in 1/256 of a bit }

function I16ModeCost(Mode: Integer): Integer;
begin
  // Not a macroblock of 4x4 blocks, then the mode
  Result := BitCost[True, 145];
  case Mode of
    DC_PRED: Inc(Result, BitCost[False, 156] + BitCost[False, 163]);
    V_PRED:  Inc(Result, BitCost[False, 156] + BitCost[True, 163]);
    H_PRED:  Inc(Result, BitCost[True, 156] + BitCost[False, 128]);
  else
    Inc(Result, BitCost[True, 156] + BitCost[True, 128]);
  end;
end;

function UVModeCost(Mode: Integer): Integer;
begin
  case Mode of
    DC_PRED: Result := BitCost[False, 142];
    V_PRED:  Result := BitCost[True, 142] + BitCost[False, 114];
    H_PRED:  Result := BitCost[True, 142] + BitCost[True, 114] + BitCost[False, 183];
  else
    Result := BitCost[True, 142] + BitCost[True, 114] + BitCost[True, 183];
  end;
end;

{ Bits of the mode of a 4x4 block; P are the probabilities for the modes
  of the blocks above and to the left. Writes them when BW is given,
  returns their cost. }
function CodeI4Mode(BW: Pointer; Mode: Integer; P: PByte): Integer;

  procedure B(Bit: Boolean; Index: Integer);
  begin
    if BW <> nil then
      PutBit(TBoolWriter(BW^), Bit, P[Index]);
    Inc(Result, BitCost[Bit, P[Index]]);
  end;

begin
  Result := 0;
  B(Mode <> B_DC_PRED, 0);
  if Mode = B_DC_PRED then Exit;
  B(Mode <> B_TM_PRED, 1);
  if Mode = B_TM_PRED then Exit;
  B(Mode <> B_VE_PRED, 2);
  if Mode = B_VE_PRED then Exit;

  if Mode in [B_HE_PRED, B_RD_PRED, B_VR_PRED] then
  begin
    B(False, 3);
    B(Mode <> B_HE_PRED, 4);
    if Mode <> B_HE_PRED then
      B(Mode = B_VR_PRED, 5);
  end
  else
  begin
    B(True, 3);
    B(Mode <> B_LD_PRED, 6);
    if Mode <> B_LD_PRED then
    begin
      B(Mode <> B_VL_PRED, 7);
      if Mode <> B_VL_PRED then
        B(Mode = B_HU_PRED, 8);
    end;
  end;
end;

{ Macroblocks }

{ Codes the macroblock at MbX, MbY: picks prediction modes, quantizes,
  leaves the decoded pixels in E.YuvBuf and adds the coefficients to the
  tokens. The buffer must be set up with the pixels around the macroblock. }
procedure EncodeMacroblock(var E: TEncoder; MbX, MbY: Integer);
var
  YBase, UBase, VBase, Dst, Src, TopY, TopU, TopV, Base: PByte;
  HasTop, HasLeft, Nz, AnyNz, LastIsBest: Boolean;
  SrcY: array[0..255] of Byte;
  SrcUV: array[0..1, 0..63] of Byte;
  LeftY: array[0..15] of Byte;
  LeftUV: array[0..1, 0..7] of Byte;
  LeftCol: array[0..3] of Byte;
  Coef: array[0..15] of TCoeffs;
  C, BestC, DCs, Y2C: TCoeffs;
  Lev, Lev16, Lev4: array[0..15] of TLevels;
  L, BestL, Y2Lev, BestY2: TLevels;
  UVLev, BestUVLev: array[0..7] of TLevels;
  Rec16: array[0..255] of Byte;
  RecUV: array[0..1, 0..63] of Byte;
  Modes4: array[0..15] of Byte;
  Tnz, Lnz: TNzContext;
  TopM, LeftM: array[0..3] of Byte;
  PredScore: array[0..NUM_BMODES - 1] of Double;
  ModeCost: array[0..NUM_BMODES - 1] of Integer;
  Tried: array[0..NUM_BMODES - 1] of Boolean;
  Mode, BestMode, Mode16, ModeUV, N, X, Y, Ch, I, K, M, Cost, D, Ctx, First, BlockType: Integer;
  Score, BestScore, Score16, Score4, S: Double;
  UseI4, BestNz: Boolean;
  Info: ^TMBInfo;
  TL: Byte;
begin
  YBase := @E.YuvBuf[Y_OFF];
  UBase := @E.YuvBuf[U_OFF];
  VBase := @E.YuvBuf[V_OFF];
  HasTop := MbY > 0;
  HasLeft := MbX > 0;
  TopY := @E.TopY[MbX * 16];
  TopU := @E.TopU[MbX * 8];
  TopV := @E.TopV[MbX * 8];
  Info := @E.MBs[MbY * E.MbW + MbX];

  // Source pixels
  for Y := 0 to 15 do
    Move(E.Y[(MbY * 16 + Y) * E.YStride + MbX * 16], SrcY[Y * 16], 16);
  for Y := 0 to 7 do
  begin
    Move(E.U[(MbY * 8 + Y) * E.UVStride + MbX * 8], SrcUV[0, Y * 8], 8);
    Move(E.V[(MbY * 8 + Y) * E.UVStride + MbX * 8], SrcUV[1, Y * 8], 8);
  end;

  // Columns to the left, as the decoder takes them
  for Y := 0 to 15 do
    if HasLeft then LeftY[Y] := (YBase + Y * BPS - 1)^ else LeftY[Y] := 129;
  for Y := 0 to 7 do
    if HasLeft then
    begin
      LeftUV[0, Y] := (UBase + Y * BPS - 1)^;
      LeftUV[1, Y] := (VBase + Y * BPS - 1)^;
    end
    else
    begin
      LeftUV[0, Y] := 129;
      LeftUV[1, Y] := 129;
    end;

  // Luma predicted as a whole: 16 blocks without DC, their DCs transformed
  // again as one more block
  Score16 := 0;
  Mode16 := DC_PRED;

  for Mode := DC_PRED to H_PRED do
  begin
    VP8PredLuma16(Mode, YBase, TopY, @LeftY[0], HasTop, HasLeft, BPS);

    for N := 0 to 15 do
    begin
      ForwardDCT(@SrcY[(N shr 2) * 64 + (N and 3) * 4], 16, YBase + kScan[N], BPS, Coef[N]);
      DCs[N] := Coef[N][0];
    end;

    ForwardWHT(DCs, Y2C);
    Quantize(Y2C, Y2Lev, 0, E.Y2);
    VP8TransformWHT(@Y2C[0], @DCs[0]);
    Cost := I16ModeCost(Mode);
    CodeCoeffs(E, TYPE_I16_DC, E.TopNz[MbX][8] + E.LeftNz[8], 0, Y2Lev, False, Cost);
    Tnz := E.TopNz[MbX];
    Lnz := E.LeftNz;

    for N := 0 to 15 do
    begin
      X := N and 3;
      Y := N shr 2;
      Quantize(Coef[N], Lev[N], 1, E.Y1);
      Coef[N][0] := DCs[N];
      Nz := CodeCoeffs(E, TYPE_I16_AC, Tnz[X] + Lnz[Y], 1, Lev[N], False, Cost);
      Tnz[X] := Ord(Nz);
      Lnz[Y] := Ord(Nz);
      Dst := YBase + kScan[N];
      VP8TransformOne(@Coef[N][0], Dst, Dst, BPS);
    end;

    Score := BlockSSE(@SrcY[0], 16, YBase, BPS, 16) + E.LambdaY * Cost / 256;

    if (Mode = DC_PRED) or (Score < Score16) then
    begin
      Score16 := Score;
      Mode16 := Mode;
      Lev16 := Lev;
      BestY2 := Y2Lev;
      for Y := 0 to 15 do
        Move((YBase + Y * BPS)^, Rec16[Y * 16], 16);
    end;
  end;

  // Luma predicted in 4x4 blocks, each from the decoded pixels around it
  UseI4 := False;

  if E.TryI4 then
  begin
    Score4 := E.LambdaY * BitCost[False, 145] / 256;
    Tnz := E.TopNz[MbX];
    Lnz := E.LeftNz;
    for I := 0 to 3 do
    begin
      TopM[I] := E.IntraT[MbX * 4 + I];
      LeftM[I] := E.IntraL[I];
    end;
    UseI4 := True;

    for N := 0 to 15 do
    begin
      X := N and 3;
      Y := N shr 2;
      Dst := YBase + kScan[N];
      Src := @SrcY[Y * 64 + X * 4];
      for I := 0 to 3 do
        LeftCol[I] := (Dst + I * BPS - 1)^;
      TL := (Dst - BPS - 1)^;
      Base := @kBModesProba[TopM[X], LeftM[Y], 0];

      // All modes by how well they predict, the best few by coding the block
      for Mode := 0 to NUM_BMODES - 1 do
      begin
        VP8PredLuma4(Mode, Dst, Dst - BPS, @LeftCol[0], TL, BPS);
        ModeCost[Mode] := CodeI4Mode(nil, Mode, Base);
        PredScore[Mode] := BlockSSE(Src, 16, Dst, BPS, 4) + E.LambdaY * ModeCost[Mode] / 256;
        Tried[Mode] := False;
      end;

      BestScore := 0;
      BestMode := -1;
      BestNz := False;
      LastIsBest := False;

      for K := 1 to E.Candidates do
      begin
        M := -1;
        for Mode := 0 to NUM_BMODES - 1 do
          if not Tried[Mode] and ((M < 0) or (PredScore[Mode] < PredScore[M])) then
            M := Mode;
        Tried[M] := True;

        VP8PredLuma4(M, Dst, Dst - BPS, @LeftCol[0], TL, BPS);
        ForwardDCT(Src, 16, Dst, BPS, C);
        Quantize(C, L, 0, E.Y1);
        Cost := ModeCost[M];
        Nz := CodeCoeffs(E, TYPE_I4, Tnz[X] + Lnz[Y], 0, L, False, Cost);
        VP8TransformOne(@C[0], Dst, Dst, BPS);
        Score := BlockSSE(Src, 16, Dst, BPS, 4) + E.LambdaY * Cost / 256;
        LastIsBest := False;

        if (BestMode < 0) or (Score < BestScore) then
        begin
          BestScore := Score;
          BestMode := M;
          BestL := L;
          BestC := C;
          BestNz := Nz;
          LastIsBest := True;
        end;
      end;

      // Pixels of the best one stay in the buffer for the next blocks
      if not LastIsBest then
      begin
        VP8PredLuma4(BestMode, Dst, Dst - BPS, @LeftCol[0], TL, BPS);
        VP8TransformOne(@BestC[0], Dst, Dst, BPS);
      end;

      Lev4[N] := BestL;
      Modes4[N] := BestMode;
      Tnz[X] := Ord(BestNz);
      Lnz[Y] := Ord(BestNz);
      TopM[X] := BestMode;
      LeftM[Y] := BestMode;
      Score4 := Score4 + BestScore;

      if Score4 >= Score16 then
      begin
        UseI4 := False;
        Break;
      end;
    end;
  end;

  if not UseI4 then
    for Y := 0 to 15 do
      Move(Rec16[Y * 16], (YBase + Y * BPS)^, 16);

  // Chroma: both planes by one mode
  BestScore := 0;
  ModeUV := DC_PRED;

  for Mode := DC_PRED to H_PRED do
  begin
    VP8PredChroma8(Mode, UBase, TopU, @LeftUV[0, 0], HasTop, HasLeft, BPS);
    VP8PredChroma8(Mode, VBase, TopV, @LeftUV[1, 0], HasTop, HasLeft, BPS);
    Cost := UVModeCost(Mode);
    Tnz := E.TopNz[MbX];
    Lnz := E.LeftNz;
    D := 0;

    for Ch := 0 to 1 do
    begin
      if Ch = 0 then Base := UBase else Base := VBase;

      for N := 0 to 3 do
      begin
        X := N and 1;
        Y := N shr 1;
        Dst := Base + X * 4 + Y * 4 * BPS;
        ForwardDCT(@SrcUV[Ch, Y * 32 + X * 4], 8, Dst, BPS, C);
        Quantize(C, UVLev[Ch * 4 + N], 0, E.UV);
        Nz := CodeCoeffs(E, TYPE_CHROMA, Tnz[4 + Ch * 2 + X] + Lnz[4 + Ch * 2 + Y], 0,
                         UVLev[Ch * 4 + N], False, Cost);
        Tnz[4 + Ch * 2 + X] := Ord(Nz);
        Lnz[4 + Ch * 2 + Y] := Ord(Nz);
        VP8TransformOne(@C[0], Dst, Dst, BPS);
      end;

      Inc(D, BlockSSE(@SrcUV[Ch, 0], 8, Base, BPS, 8));
    end;

    Score := D + E.LambdaUV * Cost / 256;

    if (Mode = DC_PRED) or (Score < BestScore) then
    begin
      BestScore := Score;
      ModeUV := Mode;
      BestUVLev := UVLev;
      for Y := 0 to 7 do
      begin
        Move((UBase + Y * BPS)^, RecUV[0, Y * 8], 8);
        Move((VBase + Y * BPS)^, RecUV[1, Y * 8], 8);
      end;
    end;
  end;

  for Y := 0 to 7 do
  begin
    Move(RecUV[0, Y * 8], (UBase + Y * BPS)^, 8);
    Move(RecUV[1, Y * 8], (VBase + Y * BPS)^, 8);
  end;

  // What was chosen
  Info^.IsI4 := UseI4;
  Info^.YMode := Mode16;
  Info^.UVMode := ModeUV;

  if UseI4 then
  begin
    Move(Modes4, Info^.Modes, 16);
    Lev16 := Lev4;
    for I := 0 to 3 do
    begin
      E.IntraT[MbX * 4 + I] := Modes4[12 + I];
      E.IntraL[I] := Modes4[I * 4 + 3];
    end;
  end
  else
    for I := 0 to 3 do
    begin
      E.IntraT[MbX * 4 + I] := Mode16;
      E.IntraL[I] := Mode16;
    end;

  // A macroblock without coefficients is only flagged
  AnyNz := False;
  for N := 0 to 15 do
    for I := 0 to 15 do
      if Lev16[N][I] <> 0 then
        AnyNz := True;
  for N := 0 to 7 do
    for I := 0 to 15 do
      if BestUVLev[N][I] <> 0 then
        AnyNz := True;
  if not UseI4 then
    for I := 0 to 15 do
      if BestY2[I] <> 0 then
        AnyNz := True;

  Info^.Skip := not AnyNz;

  if not AnyNz then
  begin
    Inc(E.NumSkipped);
    for I := 0 to 7 do
    begin
      E.TopNz[MbX][I] := 0;
      E.LeftNz[I] := 0;
    end;
    if not UseI4 then
    begin
      E.TopNz[MbX][8] := 0;
      E.LeftNz[8] := 0;
    end;
  end
  else
  begin
    Cost := 0;

    if UseI4 then
    begin
      BlockType := TYPE_I4;
      First := 0;
    end
    else
    begin
      Nz := CodeCoeffs(E, TYPE_I16_DC, E.TopNz[MbX][8] + E.LeftNz[8], 0, BestY2, True, Cost);
      E.TopNz[MbX][8] := Ord(Nz);
      E.LeftNz[8] := Ord(Nz);
      BlockType := TYPE_I16_AC;
      First := 1;
    end;

    for N := 0 to 15 do
    begin
      X := N and 3;
      Y := N shr 2;
      Ctx := E.TopNz[MbX][X] + E.LeftNz[Y];
      Nz := CodeCoeffs(E, BlockType, Ctx, First, Lev16[N], True, Cost);
      E.TopNz[MbX][X] := Ord(Nz);
      E.LeftNz[Y] := Ord(Nz);
    end;

    for Ch := 0 to 1 do
      for N := 0 to 3 do
      begin
        X := 4 + Ch * 2 + (N and 1);
        Y := 4 + Ch * 2 + (N shr 1);
        Ctx := E.TopNz[MbX][X] + E.LeftNz[Y];
        Nz := CodeCoeffs(E, TYPE_CHROMA, Ctx, 0, BestUVLev[Ch * 4 + N], True, Cost);
        E.TopNz[MbX][X] := Ord(Nz);
        E.LeftNz[Y] := Ord(Nz);
      end;
  end;

  // The last rows are the context of the macroblocks below
  Move((YBase + 15 * BPS)^, TopY^, 16);
  Move((UBase + 7 * BPS)^, TopU^, 8);
  Move((VBase + 7 * BPS)^, TopV^, 8);
end;

{ Codes all macroblocks. The work buffer gets the surroundings of each one
  the way the decoder sets them up (VP8DecodeFrame of PasWebPDec). }
procedure EncodeMacroblocks(var E: TEncoder);
var
  MbX, MbY, J: Integer;
  YBase, UBase, VBase: PByte;
begin
  YBase := @E.YuvBuf[Y_OFF];
  UBase := @E.YuvBuf[U_OFF];
  VBase := @E.YuvBuf[V_OFF];
  FillChar(E.YuvBuf, SizeOf(E.YuvBuf), 128);

  for MbY := 0 to E.MbH - 1 do
  begin
    // Nothing to the left of the row: 129
    for J := 0 to 15 do
      (YBase + J * BPS - 1)^ := 129;
    for J := 0 to 7 do
    begin
      (UBase + J * BPS - 1)^ := 129;
      (VBase + J * BPS - 1)^ := 129;
    end;

    if MbY = 0 then
    begin
      // Nothing above the first row: 127, also the corner and above right
      FillChar((YBase - BPS - 1)^, 16 + 4 + 1, 127);
      FillChar((UBase - BPS - 1)^, 8 + 1, 127);
      FillChar((VBase - BPS - 1)^, 8 + 1, 127);
    end
    else
    begin
      (YBase - BPS - 1)^ := 129;
      (UBase - BPS - 1)^ := 129;
      (VBase - BPS - 1)^ := 129;
    end;

    FillChar(E.LeftNz, SizeOf(E.LeftNz), 0);
    FillChar(E.IntraL, SizeOf(E.IntraL), B_DC_PRED);

    for MbX := 0 to E.MbW - 1 do
    begin
      // The right column of the previous macroblock is the left one of this
      if MbX > 0 then
      begin
        for J := -1 to 15 do
          (YBase + J * BPS - 1)^ := (YBase + J * BPS + 15)^;
        for J := -1 to 7 do
        begin
          (UBase + J * BPS - 1)^ := (UBase + J * BPS + 7)^;
          (VBase + J * BPS - 1)^ := (VBase + J * BPS + 7)^;
        end;
      end;

      if MbY > 0 then
      begin
        Move(E.TopY[MbX * 16], (YBase - BPS)^, 16);
        Move(E.TopU[MbX * 8], (UBase - BPS)^, 8);
        Move(E.TopV[MbX * 8], (VBase - BPS)^, 8);
        // Four pixels above right, the last pixel repeated at the right edge
        if MbX < E.MbW - 1 then
          Move(E.TopY[(MbX + 1) * 16], (YBase - BPS + 16)^, 4)
        else
          FillChar((YBase - BPS + 16)^, 4, E.TopY[MbX * 16 + 15]);
      end;

      // 4x4 blocks at the right side of the macroblock use them all
      PCardinal(YBase + 3 * BPS + 16)^ := PCardinal(YBase - BPS + 16)^;
      PCardinal(YBase + 7 * BPS + 16)^ := PCardinal(YBase - BPS + 16)^;
      PCardinal(YBase + 11 * BPS + 16)^ := PCardinal(YBase - BPS + 16)^;

      EncodeMacroblock(E, MbX, MbY);
    end;
  end;
end;

{ Bitstream }

{ Writes the first partition: frame header, probabilities and modes of
  all macroblocks. }
procedure WritePartition0(var E: TEncoder; var BW: TBoolWriter);
var
  I, MbX, MbY, X, Y, SkipProba, NumMBs, NewP, OldP: Integer;
  OldCost, NewCost, Zeros, Ones: Int64;
  Info: ^TMBInfo;
  TopM: TPasWebPBytes;
  LeftM: array[0..3] of Byte;
  Mode: Integer;
begin
  PutBits(BW, 0, 1);  // color space
  PutBits(BW, 0, 1);  // pixels need no clamping
  PutBits(BW, 0, 1);  // no segments
  PutBits(BW, 0, 1);  // normal loop filter
  PutBits(BW, E.FilterLevel, 6);
  PutBits(BW, 0, 3);  // sharpness
  PutBits(BW, 0, 1);  // no filter adjustments
  PutBits(BW, 0, 2);  // one partition of coefficients
  PutBits(BW, E.QIndex, 7);
  PutBits(BW, 0, 5);  // no differences of the other quantizers
  PutBits(BW, 0, 1);  // (probabilities are not kept for next frames)

  // Probabilities of coefficient bits: the default one is replaced where
  // the gain is larger than the 8 bits of the new one
  for I := 0 to NUM_PROBA_IDS - 1 do
  begin
    Zeros := E.Stats[I, 0];
    Ones := E.Stats[I, 1];
    OldP := DefaultProbas[I];
    NewP := 255;
    if Ones > 0 then
      NewP := 255 - Integer(Ones * 255 div (Zeros + Ones));
    if NewP < 1 then
      NewP := 1;

    OldCost := Zeros * BitCost[False, OldP] + Ones * BitCost[True, OldP] + BitCost[False, UpdateProbas[I]];
    NewCost := Zeros * BitCost[False, NewP] + Ones * BitCost[True, NewP] + BitCost[True, UpdateProbas[I]] + 8 * 256;

    if OldCost > NewCost then
    begin
      PutBit(BW, True, UpdateProbas[I]);
      PutBits(BW, NewP, 8);
      E.Probas[I] := NewP;
    end
    else
    begin
      PutBit(BW, False, UpdateProbas[I]);
      E.Probas[I] := OldP;
    end;
  end;

  // Probability that a macroblock has coefficients
  NumMBs := E.MbW * E.MbH;
  SkipProba := (NumMBs - E.NumSkipped) * 255 div NumMBs;
  if SkipProba < 1 then
    SkipProba := 1;
  PutBits(BW, 1, 1);
  PutBits(BW, SkipProba, 8);

  // Modes. Mode of a 4x4 block is coded by the modes above and to the left
  SetLength(TopM, E.MbW * 4);
  FillChar(Pointer(TopM)^, Length(TopM), B_DC_PRED);

  for MbY := 0 to E.MbH - 1 do
  begin
    FillChar(LeftM, SizeOf(LeftM), B_DC_PRED);

    for MbX := 0 to E.MbW - 1 do
    begin
      Info := @E.MBs[MbY * E.MbW + MbX];
      PutBit(BW, Info^.Skip, SkipProba);
      PutBit(BW, not Info^.IsI4, 145);

      if Info^.IsI4 then
      begin
        for Y := 0 to 3 do
          for X := 0 to 3 do
          begin
            Mode := Info^.Modes[Y * 4 + X];
            CodeI4Mode(@BW, Mode, @kBModesProba[TopM[MbX * 4 + X], LeftM[Y], 0]);
            TopM[MbX * 4 + X] := Mode;
            LeftM[Y] := Mode;
          end;
      end
      else
      begin
        Mode := Info^.YMode;
        if (Mode = TM_PRED) or (Mode = H_PRED) then
        begin
          PutBit(BW, True, 156);
          PutBit(BW, Mode = TM_PRED, 128);
        end
        else
        begin
          PutBit(BW, False, 156);
          PutBit(BW, Mode = V_PRED, 163);
        end;

        for X := 0 to 3 do
        begin
          TopM[MbX * 4 + X] := Mode;
          LeftM[X] := Mode;
        end;
      end;

      Mode := Info^.UVMode;
      if Mode = DC_PRED then
        PutBit(BW, False, 142)
      else
      begin
        PutBit(BW, True, 142);
        if Mode = V_PRED then
          PutBit(BW, False, 114)
        else
        begin
          PutBit(BW, True, 114);
          PutBit(BW, Mode = TM_PRED, 183);
        end;
      end;
    end;
  end;
end;

{ Quantizer index of a quality 0..100 (as libwebp maps it). }
function QualityToQIndex(Quality: Integer): Integer;
const
  Third: Double = 1.0 / 3.0;
var
  C, Linear, V: Double;
begin
  C := Quality / 100.0;
  if C < 0.75 then
    Linear := C * (2.0 / 3.0)
  else
    Linear := 2.0 * C - 1.0;

  if Linear <= 0 then
    V := 0
  else
    V := Exp(Ln(Linear) * Third);

  Result := Trunc(127.0 * (1.0 - V));
  if Result < 0 then Result := 0;
  if Result > 127 then Result := 127;
end;

{ Encodes the planes in E to a VP8 frame (data of 'VP8 ' chunk).
  Returns False when it cannot be stored. }
function EncodeFrame(var E: TEncoder; Quality, Effort: Integer; out Frame: TPasWebPBytes): Boolean;
const
  LambdaYFactor: Double = 0.0234;
  LambdaUVFactor: Double = 0.047;
  FrameHeaderSize = 10;
var
  Q, Level, I: Integer;
  Part0, Part1: TBoolWriter;
  Size0, Size1: NativeInt;
  T: Integer;
begin
  Result := False;
  Frame := nil;
  Q := QualityToQIndex(Quality);
  E.QIndex := Q;

  // Steps as the decoder derives them from the index, rounding of libwebp
  SetQuant(E.Y1, kDcTable[Q], kAcTable[Q], 96, 110);
  I := (Integer(kAcTable[Q]) * 101581) shr 16;
  if I < 8 then
    I := 8;
  SetQuant(E.Y2, kDcTable[Q] * 2, I, 96, 108);
  if Q > 117 then I := 117 else I := Q;
  SetQuant(E.UV, kDcTable[I], kAcTable[Q], 110, 115);

  E.LambdaY := LambdaYFactor * E.Y1.Q[1] * E.Y1.Q[1];
  E.LambdaUV := LambdaUVFactor * E.UV.Q[1] * E.UV.Q[1];

  // Strength of the loop filter grows with the quantizer step
  Level := (kAcTable[Q] shr 2) * 300 div 256;
  if Level < 2 then Level := 0;
  if Level > 63 then Level := 63;
  E.FilterLevel := Level;

  E.TryI4 := True;
  if Effort >= 8 then E.Candidates := NUM_BMODES
  else if Effort >= 5 then E.Candidates := 3
  else if Effort >= 2 then E.Candidates := 2
  else E.Candidates := 1;

  SetLength(E.TopY, E.MbW * 16);
  SetLength(E.TopU, E.MbW * 8);
  SetLength(E.TopV, E.MbW * 8);
  SetLength(E.TopNz, E.MbW);
  SetLength(E.IntraT, E.MbW * 4);
  FillChar(Pointer(E.IntraT)^, Length(E.IntraT), B_DC_PRED);
  SetLength(E.MBs, E.MbW * E.MbH);
  E.NumSkipped := 0;
  E.NumTokens := 0;
  SetLength(E.Tokens, E.MbW * E.MbH * 64 + 4096);
  FillChar(E.Stats, SizeOf(E.Stats), 0);

  EncodeMacroblocks(E);

  BoolInit(Part0, E.MbW * E.MbH * 8 + 2048);
  WritePartition0(E, Part0);
  Size0 := BoolFinish(Part0);
  if Size0 >= MAX_PARTITION0_SIZE then
    Exit;

  BoolInit(Part1, E.NumTokens div 6 + 1024);
  for I := 0 to E.NumTokens - 1 do
  begin
    T := E.Tokens[I];
    if T and TOKEN_CONST <> 0 then
      PutBit(Part1, T and TOKEN_BIT <> 0, T and $FF)
    else
      PutBit(Part1, T and TOKEN_BIT <> 0, E.Probas[T and (TOKEN_CONST - 1)]);
  end;
  Size1 := BoolFinish(Part1);

  SetLength(Frame, FrameHeaderSize + Size0 + Size1);
  // Key frame, profile 0, shown, size of the first partition
  I := (1 shl 4) or (Integer(Size0) shl 5);
  Frame[0] := Byte(I);
  Frame[1] := Byte(I shr 8);
  Frame[2] := Byte(I shr 16);
  Frame[3] := $9D;
  Frame[4] := $01;
  Frame[5] := $2A;
  Frame[6] := Byte(E.Width);
  Frame[7] := Byte(E.Width shr 8);
  Frame[8] := Byte(E.Height);
  Frame[9] := Byte(E.Height shr 8);
  Move(Part0.Buf[0], Frame[FrameHeaderSize], Size0);
  Move(Part1.Buf[0], Frame[FrameHeaderSize + Size0], Size1);
  Result := True;
end;

{ Planes }

const
  YUV_FIX = 16;
  YUV_HALF = 1 shl (YUV_FIX - 1);

function ClipByte(V: Integer): Byte; {$IFDEF FPC}inline;{$ENDIF}
begin
  if V < 0 then Result := 0
  else if V > 255 then Result := 255
  else Result := V;
end;

{ Makes Y, U, V planes of the pixels (coefficients of libwebp). Chroma is
  the average of 2x2 pixels. Planes cover whole macroblocks: the last row
  and column are repeated. With alpha its plane is returned too;
  HasAlpha tells whether any pixel is not opaque. }
procedure MakePlanes(var E: TEncoder; Pixels: PByte; Stride: NativeInt; WithAlpha: Boolean;
  var Alpha: TPasWebPBytes; out HasAlpha: Boolean);
var
  X, Y, SX, SY, DX, DY, Bpp, R, G, B, PlaneW, PlaneH: Integer;
  P: PByte;
begin
  if WithAlpha then Bpp := 4 else Bpp := 3;
  E.MbW := (E.Width + 15) shr 4;
  E.MbH := (E.Height + 15) shr 4;
  E.YStride := E.MbW * 16;
  E.UVStride := E.MbW * 8;
  PlaneW := E.YStride;
  PlaneH := E.MbH * 16;
  SetLength(E.Y, PlaneW * PlaneH);
  SetLength(E.U, (PlaneW div 2) * (PlaneH div 2));
  SetLength(E.V, (PlaneW div 2) * (PlaneH div 2));
  HasAlpha := False;

  if WithAlpha then
  begin
    SetLength(Alpha, E.Width * E.Height);
    for Y := 0 to E.Height - 1 do
    begin
      P := Pixels + Y * Stride + 3;
      for X := 0 to E.Width - 1 do
      begin
        Alpha[Y * E.Width + X] := P^;
        if P^ <> 255 then
          HasAlpha := True;
        Inc(P, 4);
      end;
    end;
  end;

  for Y := 0 to PlaneH - 1 do
  begin
    SY := Y;
    if SY >= E.Height then SY := E.Height - 1;

    for X := 0 to PlaneW - 1 do
    begin
      SX := X;
      if SX >= E.Width then SX := E.Width - 1;
      P := Pixels + SY * Stride + SX * Bpp;
      E.Y[Y * PlaneW + X] :=
        (16839 * P[2] + 33059 * P[1] + 6420 * P[0] + YUV_HALF + (16 shl YUV_FIX)) shr YUV_FIX;
    end;
  end;

  for Y := 0 to PlaneH div 2 - 1 do
    for X := 0 to PlaneW div 2 - 1 do
    begin
      R := 0;
      G := 0;
      B := 0;

      for DY := 0 to 1 do
      begin
        SY := Y * 2 + DY;
        if SY >= E.Height then SY := E.Height - 1;
        for DX := 0 to 1 do
        begin
          SX := X * 2 + DX;
          if SX >= E.Width then SX := E.Width - 1;
          P := Pixels + SY * Stride + SX * Bpp;
          Inc(B, P[0]);
          Inc(G, P[1]);
          Inc(R, P[2]);
        end;
      end;

      // Sums of four pixels: two more bits to shift
      E.U[Y * E.UVStride + X] := ClipByte(SarLong(-9719 * R - 19081 * G + 28800 * B +
        (YUV_HALF shl 2) + (128 shl (YUV_FIX + 2)), YUV_FIX + 2));
      E.V[Y * E.UVStride + X] := ClipByte(SarLong(28800 * R - 24116 * G - 4684 * B +
        (YUV_HALF shl 2) + (128 shl (YUV_FIX + 2)), YUV_FIX + 2));
    end;
end;

{ Pixels that are fully transparent are not seen, their colors only cost
  bits: in each macroblock they get the average of its visible pixels,
  macroblocks without any get the value of the one before. }
procedure SmoothInvisible(var E: TEncoder; const Alpha: TPasWebPBytes);
var
  MbX, MbY, Plane, Size, Stride, X, Y, AX, AY, Sum, Count, V: Integer;
  Last: array[0..2] of Integer;
  P: PByte;
  Invisible: array[0..255] of Boolean;

  function IsInvisible(PX, PY: Integer): Boolean;
  begin
    if PX >= E.Width then PX := E.Width - 1;
    if PY >= E.Height then PY := E.Height - 1;
    Result := Alpha[PY * E.Width + PX] = 0;
  end;

begin
  Last[0] := 128;
  Last[1] := 128;
  Last[2] := 128;

  for MbY := 0 to E.MbH - 1 do
    for MbX := 0 to E.MbW - 1 do
      for Plane := 0 to 2 do
      begin
        if Plane = 0 then
        begin
          Size := 16;
          Stride := E.YStride;
          P := @E.Y[MbY * 16 * Stride + MbX * 16];
        end
        else
        begin
          Size := 8;
          Stride := E.UVStride;
          if Plane = 1 then
            P := @E.U[MbY * 8 * Stride + MbX * 8]
          else
            P := @E.V[MbY * 8 * Stride + MbX * 8];
        end;

        Sum := 0;
        Count := 0;

        for Y := 0 to Size - 1 do
          for X := 0 to Size - 1 do
          begin
            AX := MbX * 16 + X;
            AY := MbY * 16 + Y;
            if Plane = 0 then
              Invisible[Y * Size + X] := IsInvisible(AX, AY)
            else
            begin
              // Chroma sample of four pixels
              AX := MbX * 16 + X * 2;
              AY := MbY * 16 + Y * 2;
              Invisible[Y * Size + X] := IsInvisible(AX, AY) and IsInvisible(AX + 1, AY) and
                                         IsInvisible(AX, AY + 1) and IsInvisible(AX + 1, AY + 1);
            end;

            if not Invisible[Y * Size + X] then
            begin
              Inc(Sum, P[Y * Stride + X]);
              Inc(Count);
            end;
          end;

        if Count = Size * Size then
        begin
          Last[Plane] := (Sum + Count div 2) div Count;
          Continue;
        end;

        V := Last[Plane];
        if Count > 0 then
          V := (Sum + Count div 2) div Count;
        Last[Plane] := V;

        for Y := 0 to Size - 1 do
          for X := 0 to Size - 1 do
            if Invisible[Y * Size + X] then
              P[Y * Stride + X] := V;
      end;
end;

{ Alpha plane }

const
  ALPHA_FILTER_NONE = 0;
  ALPHA_FILTER_HORIZONTAL = 1;
  ALPHA_FILTER_VERTICAL = 2;
  ALPHA_FILTER_GRADIENT = 3;

{ Differences of the alpha plane from predictions by the given filter,
  the reverse of what the decoder does (VP8UnfilterAlpha of PasWebPDec). }
procedure FilterAlpha(const Alpha: TPasWebPBytes; W, H, Filter: Integer; var Dest: TPasWebPBytes);
var
  X, Y, Pred: Integer;
  Cur, Prev, Dst: PByte;
begin
  SetLength(Dest, W * H);
  if Filter = ALPHA_FILTER_NONE then
  begin
    Move(Alpha[0], Dest[0], W * H);
    Exit;
  end;

  for Y := 0 to H - 1 do
  begin
    Cur := @Alpha[Y * W];
    Dst := @Dest[Y * W];

    if Y = 0 then
    begin
      // The first row: from the pixel to the left
      Dst[0] := Cur[0];
      for X := 1 to W - 1 do
        Dst[X] := Byte(Cur[X] - Cur[X - 1]);
      Continue;
    end;

    Prev := Cur - W;

    case Filter of
      ALPHA_FILTER_HORIZONTAL:
        begin
          Dst[0] := Byte(Cur[0] - Prev[0]);
          for X := 1 to W - 1 do
            Dst[X] := Byte(Cur[X] - Cur[X - 1]);
        end;
      ALPHA_FILTER_VERTICAL:
        for X := 0 to W - 1 do
          Dst[X] := Byte(Cur[X] - Prev[X]);
    else
      Dst[0] := Byte(Cur[0] - Prev[0]);
      for X := 1 to W - 1 do
      begin
        Pred := Integer(Cur[X - 1]) + Integer(Prev[X]) - Integer(Prev[X - 1]);
        Dst[X] := Byte(Cur[X] - ClipByte(Pred));
      end;
    end;
  end;
end;

function PlaneEntropy(const Plane: TPasWebPBytes): Double;
var
  Histo: array[0..255] of Integer;
  I: Integer;
  N: Double;
begin
  FillChar(Histo, SizeOf(Histo), 0);
  for I := 0 to High(Plane) do
    Inc(Histo[Plane[I]]);
  N := Length(Plane);
  Result := 0;
  for I := 0 to 255 do
    if Histo[I] > 0 then
      Result := Result - Histo[I] * Ln(Histo[I] / N);
end;

{ Data of ALPH chunk: byte with the filter and compression, then the plane
  (filtered by the filter that leaves the lowest entropy) as a lossless
  stream, or as it is when that is not smaller. }
function EncodeAlpha(const Alpha: TPasWebPBytes; W, H, Effort: Integer; TryAll: Boolean): TPasWebPBytes;
var
  Filter, BestFilter: Integer;
  Filtered, Stream, BestStream: TPasWebPBytes;
  Entropy, BestEntropy: Double;
begin
  Result := nil;
  BestFilter := ALPHA_FILTER_NONE;
  BestStream := nil;

  if TryAll then
  begin
    // The estimate does not know what the lossless coder makes of the
    // plane: all filters are encoded
    for Filter := ALPHA_FILTER_NONE to ALPHA_FILTER_GRADIENT do
    begin
      FilterAlpha(Alpha, W, H, Filter, Filtered);
      if PasWebPEncodePlane(Pointer(Filtered), W, W, H, Effort, Stream) and
         ((BestStream = nil) or (Length(Stream) < Length(BestStream))) then
      begin
        BestStream := Stream;
        BestFilter := Filter;
      end;
    end;
  end
  else
  begin
    BestEntropy := 0;
    for Filter := ALPHA_FILTER_NONE to ALPHA_FILTER_GRADIENT do
    begin
      FilterAlpha(Alpha, W, H, Filter, Filtered);
      Entropy := PlaneEntropy(Filtered);
      if (Filter = ALPHA_FILTER_NONE) or (Entropy < BestEntropy) then
      begin
        BestEntropy := Entropy;
        BestFilter := Filter;
      end;
    end;

    FilterAlpha(Alpha, W, H, BestFilter, Filtered);
    if not PasWebPEncodePlane(Pointer(Filtered), W, W, H, Effort, BestStream) then
      BestStream := nil;
  end;

  if (BestStream <> nil) and (Length(BestStream) < W * H) then
  begin
    SetLength(Result, 1 + Length(BestStream));
    Result[0] := (BestFilter shl 2) or 1;
    Move(BestStream[0], Result[1], Length(BestStream));
  end
  else
  begin
    SetLength(Result, 1 + W * H);
    Result[0] := 0;
    Move(Alpha[0], Result[1], W * H);
  end;
end;

{ File }

function PasWebPEncodeLossy(Pixels: PByte; Stride: NativeInt; Width, Height: Integer;
  WithAlpha: Boolean; Quality, Effort: Integer; out Data: TPasWebPBytes): Boolean;
const
  RiffHeaderSize = 12;
  ChunkHeaderSize = 8;
  VP8XSize = 10;
  VP8XAlphaFlag = $10;
  AlphaEffort: array[PasWebPMinEffort..PasWebPMaxEffort] of Integer = (0, 1, 2, 2, 3, 4, 4, 5, 6, 9);
var
  E: ^TEncoder;
  Alpha, AlphaData, Frame: TPasWebPBytes;
  HasAlpha: Boolean;
  Size, Pos: Integer;

  procedure PutTag(const Tag: AnsiString);
  begin
    Move(Tag[1], Data[Pos], 4);
    Inc(Pos, 4);
  end;

  procedure PutUInt(Value: Cardinal; Bytes: Integer);
  var
    I: Integer;
  begin
    for I := 0 to Bytes - 1 do
    begin
      Data[Pos] := Byte(Value shr (8 * I));
      Inc(Pos);
    end;
  end;

  procedure PutChunk(const Tag: AnsiString; const Content: TPasWebPBytes);
  begin
    PutTag(Tag);
    PutUInt(Length(Content), 4);
    Move(Content[0], Data[Pos], Length(Content));
    Inc(Pos, Length(Content));
    // Chunks have even sizes
    if Odd(Length(Content)) then
    begin
      Data[Pos] := 0;
      Inc(Pos);
    end;
  end;

begin
  Result := False;
  Data := nil;
  if (Pixels = nil) or (Width < 1) or (Height < 1) or (Width > PasWebPMaxSize) or
     (Height > PasWebPMaxSize) then
    Exit;

  if Quality < 0 then Quality := 0;
  if Quality > 100 then Quality := 100;
  if Effort < PasWebPMinEffort then Effort := PasWebPMinEffort;
  if Effort > PasWebPMaxEffort then Effort := PasWebPMaxEffort;

  // The record is large and has managed fields
  New(E);
  try
    E^.Width := Width;
    E^.Height := Height;
    MakePlanes(E^, Pixels, Stride, WithAlpha, Alpha, HasAlpha);
    if HasAlpha then
      SmoothInvisible(E^, Alpha);
    if not EncodeFrame(E^, Quality, Effort, Frame) then
      Exit;
  finally
    Dispose(E);
  end;

  Size := RiffHeaderSize + ChunkHeaderSize + Length(Frame) + (Length(Frame) and 1);

  if HasAlpha then
  begin
    // The plane is simpler than a picture: less effort gives nearly the same
    AlphaData := EncodeAlpha(Alpha, Width, Height, AlphaEffort[Effort], Effort >= 8);
    Inc(Size, ChunkHeaderSize + VP8XSize + ChunkHeaderSize + Length(AlphaData) + (Length(AlphaData) and 1));
  end;

  SetLength(Data, Size);
  Pos := 0;
  PutTag('RIFF');
  PutUInt(Size - 8, 4);
  PutTag('WEBP');

  if HasAlpha then
  begin
    // Extended format: header with the flags and size of the image first
    PutTag('VP8X');
    PutUInt(VP8XSize, 4);
    PutUInt(VP8XAlphaFlag, 4);
    PutUInt(Width - 1, 3);
    PutUInt(Height - 1, 3);
    PutChunk('ALPH', AlphaData);
  end;

  PutChunk('VP8 ', Frame);
  Result := True;
end;

initialization
  InitTables;

end.
