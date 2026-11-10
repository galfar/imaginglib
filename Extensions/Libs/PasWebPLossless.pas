{
  PasWebP - pure Pascal WebP codec
  Part of Vampyre Imaging Library
  by Marek Mauder
  https://github.com/galfar/imaginglib
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.

  The encoder is written for Imaging. It follows the design of the lossless
  encoder of libwebp (which transforms to use, how predictors are chosen,
  how costs are estimated) and uses some of its constants:

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

{ Lossless WebP (VP8L) encoder.

  Pixels are ARGB values (bytes B, G, R, A in memory). The image goes
  through transforms that make it cheaper to code, then it is coded by LZ77
  backward references and Huffman codes:

    - color indexing (palette) for images of up to 256 colors, with several
      pixels packed into one when there are 16 colors or less,
    - subtract green: green is subtracted from red and blue,
    - predictor: each 2^n x 2^n tile gets one of 14 predictors, residuals
      are coded,
    - backward references with distances coded as positions in the 2D
      neighborhood, color cache of recently used colors,
    - meta Huffman codes: tiles of the image are sorted to groups, each group
      has its own five Huffman codes (green + lengths + cache, red, blue,
      alpha, distances).

  Format: https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification

  FPC and Delphi 2009+. }
unit PasWebPLossless;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}{$INLINE ON}
{$ENDIF}
{$R-}{$Q-}
{$POINTERMATH ON}

interface

type
  TPasWebPBytes = array of Byte;

const
  PasWebPMinEffort = 0;
  PasWebPMaxEffort = 9;
  PasWebPDefaultEffort = 6;
  { Size limit of WebP images. }
  PasWebPMaxSize = 16383;

{ Encodes image to lossless WebP file in memory. Rows of Width pixels are
  Stride bytes apart, pixels are B, G, R, A bytes when WithAlpha is True
  and B, G, R bytes otherwise. Effort (0..9) trades time for size:
  0 is the fastest, 9 gives the smallest files.
  Returns False for wrong parameters (WebP images are 16383 x 16383
  pixels at most). }
function PasWebPEncodeLossless(Pixels: PByte; Stride: NativeInt; Width, Height: Integer;
  WithAlpha: Boolean; Effort: Integer; out Data: TPasWebPBytes): Boolean;

{ Encodes one channel (alpha plane of lossy images) as VP8L stream without
  the header: what ALPH chunk of WebP holds. Rows of Width bytes are Stride
  bytes apart. }
function PasWebPEncodePlane(Plane: PByte; Stride: NativeInt; Width, Height: Integer;
  Effort: Integer; out Data: TPasWebPBytes): Boolean;

implementation

const
  NUM_LITERAL_CODES  = 256;
  NUM_LENGTH_CODES   = 24;
  NUM_DISTANCE_CODES = 40;
  MAX_CACHE_BITS     = 10;
  MAX_COPY_LENGTH    = 4096;
  // The largest distance that has a code
  WINDOW_SIZE        = (1 shl 20) - 120;
  MAX_CODE_LENGTH    = 15;
  NUM_CODE_LENGTH_CODES = 19;
  MAX_CODE_LENGTH_CODE_LENGTH = 7;
  NUM_PREDICTORS     = 14;
  MIN_TILE_BITS      = 2;
  MAX_TILE_BITS      = 9;
  ARGB_BLACK         = $FF000000;

  TRANSFORM_PREDICTOR   = 0;
  TRANSFORM_CROSS_COLOR = 1;
  TRANSFORM_SUB_GREEN   = 2;
  TRANSFORM_COLOR_INDEX = 3;

  // Order in which lengths of the code length code are stored
  kCodeLengthCodeOrder: array[0..NUM_CODE_LENGTH_CODES - 1] of Byte =
    (17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);

  // Distance codes 1..120 are pixels near the current one: high nibble is
  // number of rows up, low nibble is 8 - columns to the left.
  kCodeToPlane: array[0..119] of Byte = (
    $18, $07, $17, $19, $28, $06, $27, $29, $16, $1a, $26, $2a, $38, $05, $37, $39,
    $15, $1b, $36, $3a, $25, $2b, $48, $04, $47, $49, $14, $1c, $35, $3b, $46, $4a,
    $24, $2c, $58, $45, $4b, $34, $3c, $03, $57, $59, $13, $1d, $56, $5a, $23, $2d,
    $44, $4c, $55, $5b, $33, $3d, $68, $02, $67, $69, $12, $1e, $66, $6a, $22, $2e,
    $54, $5c, $43, $4d, $65, $6b, $32, $3e, $78, $01, $77, $79, $53, $5d, $11, $1f,
    $64, $6c, $42, $4e, $76, $7a, $21, $2f, $75, $7b, $31, $3f, $63, $6d, $52, $5e,
    $00, $74, $7c, $41, $4f, $10, $20, $62, $6e, $30, $73, $7d, $51, $5f, $40, $72,
    $7e, $61, $6f, $50, $71, $7f, $60, $70);

type
{$IF NOT DECLARED(PUInt32)}
  PUInt32 = ^UInt32;
{$IFEND}
  TUInt32Array = array of UInt32;
  TIntArray = array of Integer;

  { What the encoder does at given effort. }
  TEncoderParams = record
    ChainSearch: Integer;    // positions of the hash chain tried for a match
    LazyMatching: Boolean;   // look for a better match at the next pixel
    OptimalParse: Boolean;   // second pass: the cheapest literals and copies by cost
    PredictorBits: Integer;  // size of predictor tiles (2^n), the smallest tried
    PredictorModes: Integer; // number of predictors tried for a tile
    CrossColor: Boolean;     // color transform after the predictor
    MaxCacheBits: Integer;   // largest color cache tried (0 = no cache)
    MetaCodes: Boolean;      // more groups of Huffman codes
    MaxTiles: Integer;       // tiles of the Huffman image
    TileSizesTried: Integer; // sizes of these tiles tried, from the smallest allowed
    MaxGroups: Integer;      // groups of Huffman codes
    RefineGroups: Integer;   // rounds of moving tiles between groups
    TryVariants: Boolean;    // encode with other transforms too, keep the smallest
  end;

  { How an image is transformed before coding. }
  TEncoderConfig = record
    Palette: Boolean;
    SubGreen: Boolean;
    Predictor: Boolean;
    CrossColor: Boolean;
  end;

var
  // Code (minus one) of a distance to a pixel near the current one, index
  // is as values of kCodeToPlane. 255 = no code.
  PlaneToCode: array[0..127] of Byte;

{ Logarithms }

const
  SLOG2_TABLE_SIZE = 65536;

var
  // v * log2(v)
  SLog2Table: array[0..SLOG2_TABLE_SIZE - 1] of Double;
  Log2Table: array[0..255] of Double;

function SLog2(V: UInt32): Double; {$IFDEF FPC}inline;{$ENDIF}
const
  InvLn2: Double = 1.4426950408889634;
begin
  if V < SLOG2_TABLE_SIZE then
    Result := SLog2Table[V]
  else
    Result := V * Ln(V) * InvLn2;
end;

function Log2Floor(V: UInt32): Integer;
begin
  Result := 0;
  if V >= $10000 then begin Inc(Result, 16); V := V shr 16; end;
  if V >= $100 then begin Inc(Result, 8); V := V shr 8; end;
  if V >= $10 then begin Inc(Result, 4); V := V shr 4; end;
  if V >= 4 then begin Inc(Result, 2); V := V shr 2; end;
  if V >= 2 then Inc(Result);
end;

procedure InitTables;
const
  InvLn2: Double = 1.4426950408889634;
var
  I, Dc: Integer;
begin
  SLog2Table[0] := 0;
  for I := 1 to SLOG2_TABLE_SIZE - 1 do
    SLog2Table[I] := I * Ln(I) * InvLn2;
  Log2Table[0] := 0;
  for I := 1 to 255 do
    Log2Table[I] := Ln(I) * InvLn2;

  FillChar(PlaneToCode, SizeOf(PlaneToCode), 255);
  for I := 0 to 119 do
  begin
    Dc := kCodeToPlane[I];
    PlaneToCode[Dc] := I;
  end;
end;

function SubSampleSize(Size, Bits: Integer): Integer; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (Size + (1 shl Bits) - 1) shr Bits;
end;

{ Bit writer: bits go from the lowest bit of each byte up }

type
  TBitWriter = record
    Buf: TPasWebPBytes;
    Pos: NativeInt;
    Acc: UInt64;
    Used: Integer;
  end;

procedure BitWriterInit(var BW: TBitWriter; Capacity: NativeInt);
begin
  SetLength(BW.Buf, Capacity + 16);
  BW.Pos := 0;
  BW.Acc := 0;
  BW.Used := 0;
end;

procedure BitWriterFlush32(var BW: TBitWriter);
begin
  if BW.Pos + 8 > Length(BW.Buf) then
    SetLength(BW.Buf, Length(BW.Buf) + Length(BW.Buf) shr 1 + 1024);
  PUInt32(@BW.Buf[BW.Pos])^ := UInt32(BW.Acc);
  Inc(BW.Pos, 4);
  BW.Acc := BW.Acc shr 32;
  Dec(BW.Used, 32);
end;

{ Value must not have bits above Count set, Count is 32 at most. }
procedure PutBits(var BW: TBitWriter; Value: UInt32; Count: Integer); {$IFDEF FPC}inline;{$ENDIF}
begin
  BW.Acc := BW.Acc or (UInt64(Value) shl BW.Used);
  Inc(BW.Used, Count);
  if BW.Used >= 32 then
    BitWriterFlush32(BW);
end;

{ Writes the last bits, returns number of bytes written in total. }
function BitWriterFinish(var BW: TBitWriter): NativeInt;
begin
  if BW.Pos + 8 > Length(BW.Buf) then
    SetLength(BW.Buf, BW.Pos + 8);

  while BW.Used > 0 do
  begin
    BW.Buf[BW.Pos] := Byte(BW.Acc);
    Inc(BW.Pos);
    BW.Acc := BW.Acc shr 8;
    Dec(BW.Used, 8);
  end;

  BW.Used := 0;
  Result := BW.Pos;
end;

{ Huffman codes }

type
  THuffmanCode = record
    Lengths: array of Byte;
    Codes: array of Word;   // bits in the order they are written
  end;

{ Sorts indices of symbols by count, then by symbol. }
procedure SortSymbols(var Symbols: TIntArray; const Weights: array of UInt64; L, R: Integer);
var
  I, J, T, P: Integer;
  PW: UInt64;
begin
  while L < R do
  begin
    I := L;
    J := R;
    P := Symbols[(L + R) shr 1];
    PW := Weights[P];

    repeat
      while (Weights[Symbols[I]] < PW) or ((Weights[Symbols[I]] = PW) and (Symbols[I] < P)) do Inc(I);
      while (Weights[Symbols[J]] > PW) or ((Weights[Symbols[J]] = PW) and (Symbols[J] > P)) do Dec(J);
      if I <= J then
      begin
        T := Symbols[I]; Symbols[I] := Symbols[J]; Symbols[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;

    // Recursion for the smaller part only
    if J - L < R - I then
    begin
      SortSymbols(Symbols, Weights, L, J);
      L := I;
    end
    else
    begin
      SortSymbols(Symbols, Weights, I, R);
      R := J;
    end;
  end;
end;

{ Makes lengths of Huffman codes of symbols with given counts, MaxLength
  bits at most. Symbols that do not occur get 0, a single symbol gets 1. }
procedure BuildCodeLengths(Counts: PUInt32; NumSymbols, MaxLength: Integer; Lengths: PByte);
var
  Symbols, Parent: TIntArray;
  Weights, NodeWeight: array of UInt64;
  Depth: TIntArray;
  I, N, Leaf, Inner, Next, A, B, MaxDepth: Integer;
  CountMin: UInt64;

  function TakeSmallest: Integer;
  begin
    if (Leaf < N) and ((Inner >= Next) or (NodeWeight[Leaf] <= NodeWeight[Inner])) then
    begin
      Result := Leaf;
      Inc(Leaf);
    end
    else
    begin
      Result := Inner;
      Inc(Inner);
    end;
  end;

begin
  FillChar(Lengths^, NumSymbols, 0);
  N := 0;
  SetLength(Symbols, NumSymbols);
  SetLength(Weights, NumSymbols);

  for I := 0 to NumSymbols - 1 do
    if Counts[I] <> 0 then
    begin
      Symbols[N] := I;
      Inc(N);
    end;

  if N = 0 then
    Exit;

  if N = 1 then
  begin
    Lengths[Symbols[0]] := 1;
    Exit;
  end;

  SetLength(NodeWeight, 2 * N);
  SetLength(Parent, 2 * N);
  SetLength(Depth, 2 * N);
  CountMin := 1;

  // When the tree is too deep the rare symbols are made more frequent
  // and it is built again
  while True do
  begin
    for I := 0 to NumSymbols - 1 do
      if Counts[I] = 0 then
        Weights[I] := 0
      else if Counts[I] < CountMin then
        Weights[I] := CountMin
      else
        Weights[I] := Counts[I];

    SortSymbols(Symbols, Weights, 0, N - 1);
    for I := 0 to N - 1 do
      NodeWeight[I] := Weights[Symbols[I]];

    // Leaves are 0..N-1 in rising order, inner nodes are made in rising
    // order too: the two smallest nodes are always at the heads of the two
    Leaf := 0;
    Inner := N;
    Next := N;

    while Next < 2 * N - 1 do
    begin
      A := TakeSmallest;
      B := TakeSmallest;
      NodeWeight[Next] := NodeWeight[A] + NodeWeight[B];
      Parent[A] := Next;
      Parent[B] := Next;
      Inc(Next);
    end;

    Depth[2 * N - 2] := 0;
    MaxDepth := 0;
    for I := 2 * N - 3 downto 0 do
    begin
      Depth[I] := Depth[Parent[I]] + 1;
      if (I < N) and (Depth[I] > MaxDepth) then
        MaxDepth := Depth[I];
    end;

    if MaxDepth <= MaxLength then
      Break;
    CountMin := CountMin * 2;
  end;

  for I := 0 to N - 1 do
    Lengths[Symbols[I]] := Depth[I];
end;

{ Gives codes to symbols by their lengths (canonical code: shorter first,
  then by symbol), stored with bits reversed as they are written. }
procedure AssignCodes(var Code: THuffmanCode; NumSymbols: Integer);
var
  LengthCount: array[0..MAX_CODE_LENGTH] of Integer;
  NextCode: array[0..MAX_CODE_LENGTH] of UInt32;
  I, Len, Bit: Integer;
  C, Value, Reversed: UInt32;
begin
  SetLength(Code.Codes, NumSymbols);
  FillChar(LengthCount, SizeOf(LengthCount), 0);
  for I := 0 to NumSymbols - 1 do
    Inc(LengthCount[Code.Lengths[I]]);
  LengthCount[0] := 0;

  C := 0;
  NextCode[0] := 0;
  for Len := 1 to MAX_CODE_LENGTH do
  begin
    C := (C + UInt32(LengthCount[Len - 1])) shl 1;
    NextCode[Len] := C;
  end;

  for I := 0 to NumSymbols - 1 do
  begin
    Len := Code.Lengths[I];
    Reversed := 0;

    if Len > 0 then
    begin
      Value := NextCode[Len];
      Inc(NextCode[Len]);
      for Bit := 0 to Len - 1 do
        if (Value shr Bit) and 1 <> 0 then
          Reversed := Reversed or (UInt32(1) shl (Len - 1 - Bit));
    end;

    Code.Codes[I] := Reversed;
  end;
end;

procedure BuildHuffmanCode(var Code: THuffmanCode; Counts: PUInt32; NumSymbols: Integer);
begin
  SetLength(Code.Lengths, NumSymbols);
  BuildCodeLengths(Counts, NumSymbols, MAX_CODE_LENGTH, Pointer(Code.Lengths));
  AssignCodes(Code, NumSymbols);
end;

{ Code of a single symbol is written as nothing: decoder knows the symbol. }
procedure ClearCodeOfSingleSymbol(var Code: THuffmanCode);
var
  I, Used: Integer;
begin
  Used := 0;
  for I := 0 to High(Code.Lengths) do
    if Code.Lengths[I] <> 0 then
      Inc(Used);

  if Used <= 1 then
    for I := 0 to High(Code.Lengths) do
    begin
      Code.Lengths[I] := 0;
      Code.Codes[I] := 0;
    end;
end;

{ Writes lengths of a code as the format has it: codes of one or two symbols
  below 256 directly, others as lengths that are run-length coded and then
  coded by another Huffman code. The code of a single symbol is cleared. }
procedure StoreHuffmanCode(var BW: TBitWriter; var Code: THuffmanCode);
var
  NumSymbols, Used, I, J, Run, Value, Prev, NumTokens, NumStored: Integer;
  First: array[0..1] of Integer;
  Tokens, Extra: array of Byte;
  TokenCounts: array[0..NUM_CODE_LENGTH_CODES - 1] of UInt32;
  LengthCode: THuffmanCode;

  procedure AddToken(Token, ExtraBits: Integer);
  begin
    Tokens[NumTokens] := Token;
    Extra[NumTokens] := ExtraBits;
    Inc(NumTokens);
  end;

begin
  NumSymbols := Length(Code.Lengths);
  Used := 0;
  First[0] := 0;
  First[1] := 0;

  for I := 0 to NumSymbols - 1 do
    if Code.Lengths[I] <> 0 then
    begin
      if Used < 2 then
        First[Used] := I;
      Inc(Used);
    end;

  if Used = 0 then
  begin
    // Simple code of one symbol: symbol 0 in one bit
    PutBits(BW, 1, 4);
    Exit;
  end;

  if (Used <= 2) and (First[0] < 256) and (First[1] < 256) then
  begin
    PutBits(BW, 1, 1);
    PutBits(BW, Used - 1, 1);
    if First[0] <= 1 then
    begin
      PutBits(BW, 0, 1);
      PutBits(BW, First[0], 1);
    end
    else
    begin
      PutBits(BW, 1, 1);
      PutBits(BW, First[0], 8);
    end;
    if Used = 2 then
      PutBits(BW, First[1], 8);
    ClearCodeOfSingleSymbol(Code);
    Exit;
  end;

  // Lengths to tokens: 0..15 is a length, 16 repeats the last length
  // that was not zero 3..6 times, 17 is 3..10 zeros, 18 is 11..138 zeros
  SetLength(Tokens, NumSymbols);
  SetLength(Extra, NumSymbols);
  NumTokens := 0;
  Prev := 8;  // what the decoder starts with
  I := 0;

  while I < NumSymbols do
  begin
    Value := Code.Lengths[I];
    Run := 1;
    while (I + Run < NumSymbols) and (Code.Lengths[I + Run] = Value) do
      Inc(Run);
    Inc(I, Run);

    if Value = 0 then
    begin
      while Run >= 1 do
        if Run < 3 then
        begin
          AddToken(0, 0);
          Dec(Run);
        end
        else if Run < 11 then
        begin
          AddToken(17, Run - 3);
          Run := 0;
        end
        else if Run < 139 then
        begin
          AddToken(18, Run - 11);
          Run := 0;
        end
        else
        begin
          AddToken(18, 138 - 11);
          Dec(Run, 138);
        end;
    end
    else
    begin
      if Value <> Prev then
      begin
        AddToken(Value, 0);
        Dec(Run);
      end;
      Prev := Value;

      while Run >= 1 do
        if Run < 3 then
        begin
          AddToken(Value, 0);
          Dec(Run);
        end
        else if Run < 7 then
        begin
          AddToken(16, Run - 3);
          Run := 0;
        end
        else
        begin
          AddToken(16, 3);
          Dec(Run, 6);
        end;
    end;
  end;

  FillChar(TokenCounts, SizeOf(TokenCounts), 0);
  for I := 0 to NumTokens - 1 do
    Inc(TokenCounts[Tokens[I]]);

  SetLength(LengthCode.Lengths, NUM_CODE_LENGTH_CODES);
  BuildCodeLengths(@TokenCounts[0], NUM_CODE_LENGTH_CODES, MAX_CODE_LENGTH_CODE_LENGTH,
                   Pointer(LengthCode.Lengths));
  AssignCodes(LengthCode, NUM_CODE_LENGTH_CODES);

  PutBits(BW, 0, 1);
  NumStored := NUM_CODE_LENGTH_CODES;
  while (NumStored > 4) and (LengthCode.Lengths[kCodeLengthCodeOrder[NumStored - 1]] = 0) do
    Dec(NumStored);
  PutBits(BW, NumStored - 4, 4);
  for I := 0 to NumStored - 1 do
    PutBits(BW, LengthCode.Lengths[kCodeLengthCodeOrder[I]], 3);

  ClearCodeOfSingleSymbol(LengthCode);
  // Lengths of all symbols follow (no limit of their number)
  PutBits(BW, 0, 1);

  for I := 0 to NumTokens - 1 do
  begin
    J := Tokens[I];
    PutBits(BW, LengthCode.Codes[J], LengthCode.Lengths[J]);
    case J of
      16: PutBits(BW, Extra[I], 2);
      17: PutBits(BW, Extra[I], 3);
      18: PutBits(BW, Extra[I], 7);
    end;
  end;

  ClearCodeOfSingleSymbol(Code);
end;

{ Cost estimates }

{ Entropy of symbols with given counts in bits, corrected for distributions of
  few symbols, where a Huffman code cannot go below one bit per symbol
  (VP8LBitsEntropy of libwebp). }
function BitsEntropy(Counts: PUInt32; NumSymbols: Integer): Double;
var
  I, NonZeros: Integer;
  Sum, MaxCount: UInt32;
  MinLimit, Mix: Double;
begin
  NonZeros := 0;
  Sum := 0;
  MaxCount := 0;
  Result := 0;

  for I := 0 to NumSymbols - 1 do
    if Counts[I] <> 0 then
    begin
      Inc(NonZeros);
      Inc(Sum, Counts[I]);
      if Counts[I] > MaxCount then
        MaxCount := Counts[I];
      Result := Result - SLog2(Counts[I]);
    end;

  if NonZeros <= 1 then
  begin
    Result := 0;
    Exit;
  end;

  Result := Result + SLog2(Sum);

  case NonZeros of
    2: Mix := 0.99;
    3: Mix := 0.95;
    4: Mix := 0.7;
  else
    Mix := 0.627;
  end;

  MinLimit := 2.0 * Sum - MaxCount;
  MinLimit := Mix * MinLimit + (1.0 - Mix) * Result;
  if Result < MinLimit then
    Result := MinLimit;
end;

{ Estimate of bits that symbols with given counts take when Huffman coded:
  entropy, corrected for distributions of few symbols where a code cannot
  go below one bit per symbol (VP8LBitsEntropy of libwebp), plus what the
  code itself takes in the file. }
function PopulationCost(Counts: PUInt32; NumSymbols: Integer): Double;
const
  Mix2: Double = 0.99;
  Mix3: Double = 0.95;
  Mix4: Double = 0.7;
  MixMany: Double = 0.627;
  CostOfLength: Double = 3.5;
  CostOfZeroRun: Double = 7.0;
  CostOfCode: Double = 40.0;
var
  I, NonZeros, ZeroRuns: Integer;
  Sum, MaxCount, C: UInt32;
  Entropy, MinLimit, Mix: Double;
  InZeros: Boolean;
begin
  NonZeros := 0;
  ZeroRuns := 0;
  Sum := 0;
  MaxCount := 0;
  Entropy := 0;
  InZeros := False;

  for I := 0 to NumSymbols - 1 do
  begin
    C := Counts[I];
    if C <> 0 then
    begin
      Inc(NonZeros);
      Inc(Sum, C);
      if C > MaxCount then
        MaxCount := C;
      Entropy := Entropy - SLog2(C);
      InZeros := False;
    end
    else if not InZeros then
    begin
      Inc(ZeroRuns);
      InZeros := True;
    end;
  end;

  if NonZeros <= 1 then
  begin
    // No bits for the symbols, a few for the code
    Result := 8;
    Exit;
  end;

  Entropy := Entropy + SLog2(Sum);

  case NonZeros of
    2: Mix := Mix2;
    3: Mix := Mix3;
    4: Mix := Mix4;
  else
    Mix := MixMany;
  end;

  MinLimit := 2.0 * Sum - MaxCount;
  MinLimit := Mix * MinLimit + (1.0 - Mix) * Entropy;
  if Entropy < MinLimit then
    Entropy := MinLimit;

  if NonZeros = 2 then
    Result := Entropy + 20
  else
    Result := Entropy + CostOfCode + CostOfLength * NonZeros + CostOfZeroRun * ZeroRuns;
end;

{ Sums of a histogram that CombinedEntropy needs of its second one: they
  are computed once for many calls. }
type
  THistoSums = record
    Sum: UInt32;
    SLogSum: Double;  // sum of count * log2(count)
  end;

procedure GetHistoSums(Counts: PUInt32; out Sums: THistoSums);
var
  I: Integer;
begin
  Sums.Sum := 0;
  Sums.SLogSum := 0;
  for I := 0 to 255 do
    if Counts[I] <> 0 then
    begin
      Inc(Sums.Sum, Counts[I]);
      Sums.SLogSum := Sums.SLogSum + SLog2(Counts[I]);
    end;
end;

{ Entropy of X plus entropy of X + Y, in bits (CombinedShannonEntropy
  of libwebp): how well X is coded alone and together with Y. X is
  a histogram of a tile with few values, Y of the tiles before it: only
  values that X has are worked with, the rest of Y is in YSums. }
function CombinedEntropy(X, Y: PUInt32; const YSums: THistoSums): Double;
var
  I: Integer;
  SumX, VX, VY: UInt32;
begin
  Result := -YSums.SLogSum;
  SumX := 0;

  for I := 0 to 255 do
  begin
    VX := X[I];
    if VX <> 0 then
    begin
      VY := Y[I];
      Inc(SumX, VX);
      // Value of Y is replaced by the one of X + Y
      Result := Result - SLog2(VX) - SLog2(VX + VY) + SLog2(VY);
    end;
  end;

  Result := Result + SLog2(SumX) + SLog2(SumX + YSums.Sum);
end;

{ The same when values that X has are listed. }
function CombinedEntropyOfList(X, Y: PUInt32; const YSums: THistoSums; List: PByte;
  Count: Integer): Double;
var
  I: Integer;
  SumX, VX, VY: UInt32;
begin
  Result := -YSums.SLogSum;
  SumX := 0;

  for I := 0 to Count - 1 do
  begin
    VX := X[List[I]];
    VY := Y[List[I]];
    Inc(SumX, VX);
    Result := Result - SLog2(VX) - SLog2(VX + VY) + SLog2(VY);
  end;

  Result := Result + SLog2(SumX) + SLog2(SumX + YSums.Sum);
end;

{ Pixels }

function AddPixels(A, B: UInt32): UInt32; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (((A and $FF00FF00) + (B and $FF00FF00)) and $FF00FF00) or
            (((A and $00FF00FF) + (B and $00FF00FF)) and $00FF00FF);
end;

{ Difference of channels, each modulo 256. }
function SubPixels(A, B: UInt32): UInt32; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (($00FF00FF + (A and $FF00FF00) - (B and $FF00FF00)) and $FF00FF00) or
            (($FF00FF00 + (A and $00FF00FF) - (B and $00FF00FF)) and $00FF00FF);
end;

function Average2(A, B: UInt32): UInt32; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (((A xor B) and $FEFEFEFE) shr 1) + (A and B);
end;

function Clip255(V: Integer): UInt32; {$IFDEF FPC}inline;{$ENDIF}
begin
  if V < 0 then Result := 0
  else if V > 255 then Result := 255
  else Result := V;
end;

{ Top or left pixel, the one closer to the gradient Left + Top - TopLeft. }
function PredictSelect(Top, Left, TopLeft: UInt32): UInt32;
var
  D: Integer;
begin
  D := Abs(Integer(Left shr 24) - Integer(TopLeft shr 24)) -
       Abs(Integer(Top shr 24) - Integer(TopLeft shr 24)) +
       Abs(Integer((Left shr 16) and $FF) - Integer((TopLeft shr 16) and $FF)) -
       Abs(Integer((Top shr 16) and $FF) - Integer((TopLeft shr 16) and $FF)) +
       Abs(Integer((Left shr 8) and $FF) - Integer((TopLeft shr 8) and $FF)) -
       Abs(Integer((Top shr 8) and $FF) - Integer((TopLeft shr 8) and $FF)) +
       Abs(Integer(Left and $FF) - Integer(TopLeft and $FF)) -
       Abs(Integer(Top and $FF) - Integer(TopLeft and $FF));
  if D <= 0 then Result := Top else Result := Left;
end;

function PredictClampFull(A, B, C: UInt32): UInt32;
begin
  Result :=
    (Clip255(Integer(A shr 24) + Integer(B shr 24) - Integer(C shr 24)) shl 24) or
    (Clip255(Integer((A shr 16) and $FF) + Integer((B shr 16) and $FF) - Integer((C shr 16) and $FF)) shl 16) or
    (Clip255(Integer((A shr 8) and $FF) + Integer((B shr 8) and $FF) - Integer((C shr 8) and $FF)) shl 8) or
    Clip255(Integer(A and $FF) + Integer(B and $FF) - Integer(C and $FF));
end;

{ Average is the average of left and top. }
function PredictClampHalf(Average, C: UInt32): UInt32;
var
  A: Integer;
begin
  A := Integer(Average shr 24);
  Result := Clip255(A + (A - Integer(C shr 24)) div 2) shl 24;
  A := Integer((Average shr 16) and $FF);
  Result := Result or (Clip255(A + (A - Integer((C shr 16) and $FF)) div 2) shl 16);
  A := Integer((Average shr 8) and $FF);
  Result := Result or (Clip255(A + (A - Integer((C shr 8) and $FF)) div 2) shl 8);
  A := Integer(Average and $FF);
  Result := Result or Clip255(A + (A - Integer(C and $FF)) div 2);
end;

function Predict(Mode: Integer; Left, Top, TopLeft, TopRight: UInt32): UInt32;
begin
  case Mode of
    1:  Result := Left;
    2:  Result := Top;
    3:  Result := TopRight;
    4:  Result := TopLeft;
    5:  Result := Average2(Average2(Left, TopRight), Top);
    6:  Result := Average2(Left, TopLeft);
    7:  Result := Average2(Left, Top);
    8:  Result := Average2(TopLeft, Top);
    9:  Result := Average2(Top, TopRight);
    10: Result := Average2(Average2(Left, TopLeft), Average2(Top, TopRight));
    11: Result := PredictSelect(Top, Left, TopLeft);
    12: Result := PredictClampFull(Left, Top, TopLeft);
    13: Result := PredictClampHalf(Average2(Left, Top), TopLeft);
  else
    Result := ARGB_BLACK;
  end;
end;

type
  TPredictions = array[0..NUM_PREDICTORS - 1] of UInt32;

{ All predictors at once, they share parts. }
procedure PredictAll(Left, Top, TopLeft, TopRight: UInt32; var P: TPredictions);
begin
  P[0] := ARGB_BLACK;
  P[1] := Left;
  P[2] := Top;
  P[3] := TopRight;
  P[4] := TopLeft;
  P[5] := Average2(Average2(Left, TopRight), Top);
  P[6] := Average2(Left, TopLeft);
  P[7] := Average2(Left, Top);
  P[8] := Average2(TopLeft, Top);
  P[9] := Average2(Top, TopRight);
  P[10] := Average2(P[6], P[9]);
  P[11] := PredictSelect(Top, Left, TopLeft);
  P[12] := PredictClampFull(Left, Top, TopLeft);
  P[13] := PredictClampHalf(P[7], TopLeft);
end;

{ Backward references }

const
  TOKEN_LITERAL = 0;  // Value is the pixel
  TOKEN_CACHE   = 1;  // Value is index to the color cache
  TOKEN_COPY    = 2;  // Len pixels from distance with code Value

type
  TToken = record
    Kind: Byte;
    Len: Word;
    Value: UInt32;
  end;
  TTokens = array of TToken;

{ Lengths and distances are coded as a prefix code (Huffman coded) and
  extra bits holding the rest of the value. }
procedure PrefixEncode(Value: Integer; out Code, ExtraBits: Integer; out ExtraValue: UInt32);
var
  HighBit: Integer;
begin
  if Value > 2 then
  begin
    Dec(Value);
    HighBit := Log2Floor(Value);
    ExtraBits := HighBit - 1;
    Code := 2 * HighBit + ((Value shr (HighBit - 1)) and 1);
    ExtraValue := UInt32(Value) and ((UInt32(1) shl ExtraBits) - 1);
  end
  else
  begin
    ExtraBits := 0;
    ExtraValue := 0;
    Code := Value - 1;
  end;
end;

{ Code of a distance in pixels: 1..120 for pixels near the current one
  (in two dimensions), distance + 120 for the others. }
function DistanceToCode(XSize, Distance: Integer): Integer;
var
  YOffset, XOffset, C: Integer;
begin
  YOffset := Distance div XSize;
  XOffset := Distance - YOffset * XSize;

  if (XOffset <= 8) and (YOffset < 8) then
    C := PlaneToCode[YOffset * 16 + 8 - XOffset]
  else if (XOffset > XSize - 8) and (YOffset < 7) then
    C := PlaneToCode[(YOffset + 1) * 16 + 8 + (XSize - XOffset)]
  else
    C := 255;

  if C <> 255 then
    Result := C + 1
  else
    Result := Distance + 120;
end;

{ Search for earlier occurrences of the pixels at a position: hash chains
  of pixel pairs. Positions must be inserted in order. }
type
  TMatcher = record
    Img: PUInt32;
    W, N: Integer;
    Head, Chain: TIntArray;
    HashShift: Integer;
    NextInsert: Integer;   // all positions before this one are in the chains
    ChainSearch: Integer;
  end;

procedure MatcherInit(out M: TMatcher; Img: PUInt32; W, N, ChainSearch: Integer);
var
  Bits, I: Integer;
begin
  M.Img := Img;
  M.W := W;
  M.N := N;
  M.ChainSearch := ChainSearch;
  M.NextInsert := 0;
  Bits := Log2Floor(N) + 1;
  if Bits < 10 then Bits := 10;
  if Bits > 18 then Bits := 18;
  M.HashShift := 32 - Bits;
  SetLength(M.Head, 1 shl Bits);
  for I := 0 to High(M.Head) do
    M.Head[I] := -1;
  SetLength(M.Chain, N);
end;

function MatcherHash(const M: TMatcher; Pos: Integer): Integer; {$IFDEF FPC}inline;{$ENDIF}
begin
  // Products are cut to 32 bits (they are computed in 64 bits in 64-bit programs)
  Result := Integer(UInt32(M.Img[Pos] * UInt32($9E3779B1) +
                           M.Img[Pos + 1] * UInt32($85EBCA6B)) shr M.HashShift);
end;

procedure MatcherInsertUpTo(var M: TMatcher; Last: Integer);
var
  H: Integer;
begin
  // The last pixel has no pair to hash
  while M.NextInsert <= Last do
  begin
    if M.NextInsert + 1 < M.N then
    begin
      H := MatcherHash(M, M.NextInsert);
      M.Chain[M.NextInsert] := M.Head[H];
      M.Head[H] := M.NextInsert;
    end;
    Inc(M.NextInsert);
  end;
end;

function MatchLength(A, B: PUInt32; MaxLen: Integer): Integer; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := 0;
  while (Result < MaxLen) and (A[Result] = B[Result]) do
    Inc(Result);
end;

{ The longest run of pixels before Pos that repeats at Pos (0 = none
  of three pixels or more), the nearest one of that length. }
procedure MatcherFind(const M: TMatcher; Pos: Integer; out BestLen, BestDist: Integer);
const
  BitsPerPixel = 2;  // what a pixel of a longer match is taken to save, at least
var
  MaxLen, Cand, L, Tries, Bits, BestBits: Integer;
  Img: PUInt32;
begin
  Img := M.Img;
  BestLen := 0;
  BestDist := 0;
  MaxLen := M.N - Pos;
  if MaxLen > MAX_COPY_LENGTH then
    MaxLen := MAX_COPY_LENGTH;
  if MaxLen < 3 then
    Exit;

  // The cheapest distances first: previous pixel and the pixel above
  if Pos >= 1 then
  begin
    BestLen := MatchLength(Img + Pos - 1, Img + Pos, MaxLen);
    BestDist := 1;
  end;

  if (Pos >= M.W) and (M.W > 1) and (BestLen < MaxLen) and
     (Img[Pos - M.W + BestLen] = Img[Pos + BestLen]) then
  begin
    L := MatchLength(Img + Pos - M.W, Img + Pos, MaxLen);
    if L > BestLen then
    begin
      BestLen := L;
      BestDist := M.W;
    end;
  end;

  if (BestLen < MaxLen) and (Pos + 1 < M.N) then
  begin
    Cand := M.Head[MatcherHash(M, Pos)];
    Tries := M.ChainSearch;
    BestBits := 0;

    while (Cand >= 0) and (Tries > 0) do
    begin
      if Pos - Cand > WINDOW_SIZE then
        Break;

      if Img[Cand + BestLen] = Img[Pos + BestLen] then
      begin
        L := MatchLength(Img + Cand, Img + Pos, MaxLen);
        if L > BestLen then
        begin
          // Candidates get farther and their distances take more bits:
          // a longer match must be worth them
          Bits := Log2Floor(DistanceToCode(M.W, Pos - Cand));
          if (BestLen < 3) or ((L - BestLen) * BitsPerPixel > Bits - BestBits) then
          begin
            BestLen := L;
            BestDist := Pos - Cand;
            BestBits := Bits;
            if L >= MaxLen then
              Break;
          end;
        end;
      end;

      Cand := M.Chain[Cand];
      Dec(Tries);
    end;
  end;

  if BestLen < 3 then
    BestLen := 0;
end;

{ Gives the image as literals and copies of earlier pixels: the longest
  match is taken, unless a longer one starts at the next pixel. }
procedure FindTokens(Img: PUInt32; W, N: Integer; const Params: TEncoderParams;
  var Tokens: TTokens; out Count: Integer);
const
  LazyLimit = 64;  // matches this long are taken without looking further
var
  M: TMatcher;
  I, Len, Dist, Len2, Dist2: Integer;

  function WorthCopying(L, D: Integer): Boolean;
  begin
    // Three pixels pay off only from near: a far distance takes many bits
    Result := (L >= 4) or ((L = 3) and (DistanceToCode(W, D) <= 120));
  end;

  procedure AddLiteral(Pos: Integer);
  begin
    Tokens[Count].Kind := TOKEN_LITERAL;
    Tokens[Count].Len := 1;
    Tokens[Count].Value := Img[Pos];
    Inc(Count);
  end;

begin
  Count := 0;
  SetLength(Tokens, N);
  if N = 0 then
    Exit;

  MatcherInit(M, Img, W, N, Params.ChainSearch);
  I := 0;

  while I < N do
  begin
    MatcherFind(M, I, Len, Dist);

    if WorthCopying(Len, Dist) then
    begin
      if Params.LazyMatching then
      begin
        // A longer match can start at the next pixel
        while (Len < LazyLimit) and (I + 1 < N) do
        begin
          MatcherInsertUpTo(M, I);
          MatcherFind(M, I + 1, Len2, Dist2);
          if (Len2 > Len) and WorthCopying(Len2, Dist2) then
          begin
            AddLiteral(I);
            Inc(I);
            Len := Len2;
            Dist := Dist2;
          end
          else
            Break;
        end;
      end;

      Tokens[Count].Kind := TOKEN_COPY;
      Tokens[Count].Len := Len;
      Tokens[Count].Value := DistanceToCode(W, Dist);
      Inc(Count);
      MatcherInsertUpTo(M, I + Len - 1);
      Inc(I, Len);
    end
    else
    begin
      AddLiteral(I);
      MatcherInsertUpTo(M, I);
      Inc(I);
    end;
  end;
end;

{ Color cache }

function CacheKey(Argb: UInt32; Bits: Integer): Integer; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := Integer(UInt32(Argb * UInt32($1E35A7BD)) shr (32 - Bits));
end;

{ Picks size of the color cache (0 = none) that makes literals the cheapest:
  tries all sizes on the tokens. }
function ChooseCacheBits(Img: PUInt32; const Tokens: TTokens; Count, MaxBits: Integer): Integer;
var
  Bits, T, K, Pos, Key, LenCode, ExtraBits: Integer;
  ExtraValue, Argb: UInt32;
  Lit, Cache: TUInt32Array;
  Red, Blue, Alpha: array[0..255] of UInt32;
  Cost, BestCost: Double;
begin
  Result := 0;
  BestCost := 0;

  for Bits := 0 to MaxBits do
  begin
    Lit := nil;
    Cache := nil;
    SetLength(Lit, NUM_LITERAL_CODES + NUM_LENGTH_CODES + (1 shl Bits));
    if Bits > 0 then
      SetLength(Cache, 1 shl Bits);
    FillChar(Red, SizeOf(Red), 0);
    FillChar(Blue, SizeOf(Blue), 0);
    FillChar(Alpha, SizeOf(Alpha), 0);
    Pos := 0;

    for T := 0 to Count - 1 do
      if Tokens[T].Kind = TOKEN_COPY then
      begin
        PrefixEncode(Tokens[T].Len, LenCode, ExtraBits, ExtraValue);
        Inc(Lit[NUM_LITERAL_CODES + LenCode]);
        if Bits > 0 then
          for K := Pos to Pos + Tokens[T].Len - 1 do
            Cache[CacheKey(Img[K], Bits)] := Img[K];
        Inc(Pos, Tokens[T].Len);
      end
      else
      begin
        Argb := Tokens[T].Value;
        Key := 0;
        if Bits > 0 then
          Key := CacheKey(Argb, Bits);

        if (Bits > 0) and (Cache[Key] = Argb) then
          Inc(Lit[NUM_LITERAL_CODES + NUM_LENGTH_CODES + Key])
        else
        begin
          Inc(Lit[(Argb shr 8) and $FF]);
          Inc(Red[(Argb shr 16) and $FF]);
          Inc(Blue[Argb and $FF]);
          Inc(Alpha[Argb shr 24]);
          if Bits > 0 then
            Cache[Key] := Argb;
        end;
        Inc(Pos);
      end;

    Cost := PopulationCost(Pointer(Lit), Length(Lit)) + PopulationCost(@Red[0], 256) +
            PopulationCost(@Blue[0], 256) + PopulationCost(@Alpha[0], 256);

    if (Bits = 0) or (Cost < BestCost) then
    begin
      BestCost := Cost;
      Result := Bits;
    end;
  end;
end;

{ Literals whose color is in the cache become references to it. }
procedure ApplyCache(Img: PUInt32; var Tokens: TTokens; Count, Bits: Integer);
var
  T, K, Pos, Key: Integer;
  Cache: TUInt32Array;
begin
  SetLength(Cache, 1 shl Bits);
  Pos := 0;

  for T := 0 to Count - 1 do
    if Tokens[T].Kind = TOKEN_COPY then
    begin
      for K := Pos to Pos + Tokens[T].Len - 1 do
        Cache[CacheKey(Img[K], Bits)] := Img[K];
      Inc(Pos, Tokens[T].Len);
    end
    else
    begin
      Key := CacheKey(Tokens[T].Value, Bits);
      if Cache[Key] = Tokens[T].Value then
      begin
        Tokens[T].Kind := TOKEN_CACHE;
        Tokens[T].Value := Key;
      end
      else
        Cache[Key] := Tokens[T].Value;
      Inc(Pos);
    end;
end;

{ Histograms: counts of symbols of the five codes of one group in one
  array: green/length/cache (LitSize symbols), red, blue, alpha (256 each)
  and distance (40). }

const
  HISTO_SEGMENTS = 5;

type
  THistoLayout = record
    LitSize: Integer;
    Size: Integer;
    Start: array[0..HISTO_SEGMENTS] of Integer;  // the last one is the end
  end;

procedure InitHistoLayout(out Layout: THistoLayout; CacheBits: Integer);
begin
  Layout.LitSize := NUM_LITERAL_CODES + NUM_LENGTH_CODES;
  if CacheBits > 0 then
    Inc(Layout.LitSize, 1 shl CacheBits);
  Layout.Start[0] := 0;
  Layout.Start[1] := Layout.LitSize;
  Layout.Start[2] := Layout.Start[1] + 256;
  Layout.Start[3] := Layout.Start[2] + 256;
  Layout.Start[4] := Layout.Start[3] + 256;
  Layout.Start[5] := Layout.Start[4] + NUM_DISTANCE_CODES;
  Layout.Size := Layout.Start[5];
end;

procedure AddTokenToHisto(H: PUInt32; const Layout: THistoLayout; const Token: TToken);
var
  Code, ExtraBits: Integer;
  ExtraValue: UInt32;
begin
  case Token.Kind of
    TOKEN_LITERAL:
      begin
        Inc(H[(Token.Value shr 8) and $FF]);
        Inc(H[Layout.Start[1] + Integer((Token.Value shr 16) and $FF)]);
        Inc(H[Layout.Start[2] + Integer(Token.Value and $FF)]);
        Inc(H[Layout.Start[3] + Integer(Token.Value shr 24)]);
      end;
    TOKEN_CACHE:
      Inc(H[NUM_LITERAL_CODES + NUM_LENGTH_CODES + Integer(Token.Value)]);
    TOKEN_COPY:
      begin
        PrefixEncode(Token.Len, Code, ExtraBits, ExtraValue);
        Inc(H[NUM_LITERAL_CODES + Code]);
        PrefixEncode(Token.Value, Code, ExtraBits, ExtraValue);
        Inc(H[Layout.Start[4] + Code]);
      end;
  end;
end;

function HistoCost(H: PUInt32; const Layout: THistoLayout): Double;
var
  S: Integer;
begin
  Result := 0;
  for S := 0 to HISTO_SEGMENTS - 1 do
    Result := Result + PopulationCost(H + Layout.Start[S], Layout.Start[S + 1] - Layout.Start[S]);
end;

{ Sorts tiles of the image to groups that get their own Huffman codes.
  Tiles is the matrix of histograms of the tiles (NumTiles rows), TileGroup
  gets the group of each tile; tiles with no symbols get the group of
  the tile before them. Returns number of groups. }
function GroupTiles(const Tiles: TUInt32Array; NumTiles: Integer; const Layout: THistoLayout;
  const Params: TEncoderParams; var TileGroup: TIntArray): Integer;
const
  CostOfCode: Double = 40.0;
  CostOfLength: Double = 3.5;
  CostOfMissing: Double = 4.0;  // extra bits for a symbol the group does not have
type
  TSparse = record
    Index: array of Word;
    Count: array of UInt32;
    SegSum: array[0..HISTO_SEGMENTS - 1] of UInt32;
    Cost: Double;  // of the tile coded alone
  end;
var
  Sparse: array of TSparse;
  Groups: TUInt32Array;   // matrix of histograms of the groups
  GroupSum: array of array[0..HISTO_SEGMENTS - 1] of UInt32;
  GroupUsed: array of Boolean;
  NumGroups, T, G, I, J, S, K, N, Best, Round, Last: Integer;
  Delta, BestDelta, CostA, CostB: Double;
  Row, RowA, RowB: PUInt32;
  Temp: TUInt32Array;
  BitCost: array of Double;  // per group and symbol
  Map: TIntArray;
  Changed: Boolean;

  function SegmentOf(Index: Integer): Integer;
  begin
    Result := 0;
    while Index >= Layout.Start[Result + 1] do
      Inc(Result);
  end;

  { Change of cost of group G when tile T is added to it, minus the cost
    of the tile coded alone. }
  function MergeDelta(G, T: Integer): Double;
  var
    I, S: Integer;
    C, Old: UInt32;
    Row: PUInt32;
  begin
    Row := @Groups[G * Layout.Size];
    Result := -Sparse[T].Cost;

    for S := 0 to HISTO_SEGMENTS - 1 do
      if Sparse[T].SegSum[S] <> 0 then
        Result := Result + SLog2(GroupSum[G][S] + Sparse[T].SegSum[S]) - SLog2(GroupSum[G][S]);

    for I := 0 to High(Sparse[T].Index) do
    begin
      Old := Row[Sparse[T].Index[I]];
      C := Sparse[T].Count[I];
      Result := Result - SLog2(Old + C) + SLog2(Old);
      if Old = 0 then
        Result := Result + CostOfLength;
    end;
  end;

  procedure AddTile(G, T: Integer);
  var
    I, S: Integer;
    Row: PUInt32;
  begin
    Row := @Groups[G * Layout.Size];
    for I := 0 to High(Sparse[T].Index) do
      Inc(Row[Sparse[T].Index[I]], Sparse[T].Count[I]);
    for S := 0 to HISTO_SEGMENTS - 1 do
      Inc(GroupSum[G][S], Sparse[T].SegSum[S]);
  end;

  procedure RebuildGroups;
  var
    T, G: Integer;
  begin
    FillChar(Pointer(Groups)^, Length(Groups) * SizeOf(UInt32), 0);
    for G := 0 to NumGroups - 1 do
    begin
      FillChar(GroupSum[G], SizeOf(GroupSum[G]), 0);
      GroupUsed[G] := False;
    end;

    for T := 0 to NumTiles - 1 do
      if TileGroup[T] >= 0 then
      begin
        AddTile(TileGroup[T], T);
        GroupUsed[TileGroup[T]] := True;
      end;
  end;

begin
  SetLength(TileGroup, NumTiles);
  SetLength(Sparse, NumTiles);

  // Tiles as lists of the symbols they have: they are small
  for T := 0 to NumTiles - 1 do
  begin
    Row := @Tiles[T * Layout.Size];
    N := 0;
    for I := 0 to Layout.Size - 1 do
      if Row[I] <> 0 then
        Inc(N);

    SetLength(Sparse[T].Index, N);
    SetLength(Sparse[T].Count, N);
    FillChar(Sparse[T].SegSum, SizeOf(Sparse[T].SegSum), 0);
    Sparse[T].Cost := HISTO_SEGMENTS * CostOfCode + CostOfLength * N;
    N := 0;

    for I := 0 to Layout.Size - 1 do
      if Row[I] <> 0 then
      begin
        Sparse[T].Index[N] := I;
        Sparse[T].Count[N] := Row[I];
        Inc(Sparse[T].SegSum[SegmentOf(I)], Row[I]);
        Sparse[T].Cost := Sparse[T].Cost - SLog2(Row[I]);
        Inc(N);
      end;

    for S := 0 to HISTO_SEGMENTS - 1 do
      Sparse[T].Cost := Sparse[T].Cost + SLog2(Sparse[T].SegSum[S]);
  end;

  SetLength(Groups, Params.MaxGroups * Layout.Size);
  SetLength(GroupSum, Params.MaxGroups);
  SetLength(GroupUsed, Params.MaxGroups);
  NumGroups := 0;

  // Each tile goes to the group where it costs the least, or starts
  // a new one when that is cheaper than joining
  for T := 0 to NumTiles - 1 do
  begin
    TileGroup[T] := -1;
    if Length(Sparse[T].Index) = 0 then
      Continue;

    Best := -1;
    BestDelta := 0;

    for G := 0 to NumGroups - 1 do
    begin
      Delta := MergeDelta(G, T);
      if (Best < 0) or (Delta < BestDelta) then
      begin
        Best := G;
        BestDelta := Delta;
      end;
    end;

    if (Best < 0) or ((BestDelta >= 0) and (NumGroups < Params.MaxGroups)) then
    begin
      Best := NumGroups;
      Inc(NumGroups);
    end;

    AddTile(Best, T);
    GroupUsed[Best] := True;
    TileGroup[T] := Best;
  end;

  if NumGroups = 0 then
  begin
    // Empty image
    for T := 0 to NumTiles - 1 do
      TileGroup[T] := 0;
    Result := 1;
    Exit;
  end;

  // Tiles move to the group whose code suits them best, groups are made
  // again from their tiles; a few rounds
  SetLength(BitCost, NumGroups * Layout.Size);

  for Round := 1 to Params.RefineGroups do
  begin
    for G := 0 to NumGroups - 1 do
    begin
      Row := @Groups[G * Layout.Size];
      for I := 0 to Layout.Size - 1 do
      begin
        S := SegmentOf(I);
        if GroupSum[G][S] = 0 then
          BitCost[G * Layout.Size + I] := 16
        else if Row[I] = 0 then
          BitCost[G * Layout.Size + I] := SLog2(GroupSum[G][S]) / GroupSum[G][S] + CostOfMissing
        else
          BitCost[G * Layout.Size + I] := SLog2(GroupSum[G][S]) / GroupSum[G][S] - SLog2(Row[I]) / Row[I];
      end;
    end;

    Changed := False;

    for T := 0 to NumTiles - 1 do
      if TileGroup[T] >= 0 then
      begin
        Best := -1;
        BestDelta := 0;

        for G := 0 to NumGroups - 1 do
          if GroupUsed[G] then
          begin
            Delta := 0;
            K := G * Layout.Size;
            for I := 0 to High(Sparse[T].Index) do
              Delta := Delta + Sparse[T].Count[I] * BitCost[K + Sparse[T].Index[I]];
            // Another group must be better than the one the tile is in
            if G = TileGroup[T] then
              Delta := Delta - 0.001;
            if (Best < 0) or (Delta < BestDelta) then
            begin
              Best := G;
              BestDelta := Delta;
            end;
          end;

        if Best <> TileGroup[T] then
        begin
          TileGroup[T] := Best;
          Changed := True;
        end;
      end;

    RebuildGroups;
    if not Changed then
      Break;
  end;

  // Groups that are cheaper together are joined, the best pair first
  SetLength(Temp, Layout.Size);

  while True do
  begin
    Best := -1;
    J := -1;
    BestDelta := 0;

    for G := 0 to NumGroups - 1 do
      if GroupUsed[G] then
      begin
        RowA := @Groups[G * Layout.Size];
        CostA := HistoCost(RowA, Layout);

        for K := G + 1 to NumGroups - 1 do
          if GroupUsed[K] then
          begin
            RowB := @Groups[K * Layout.Size];
            CostB := HistoCost(RowB, Layout);
            for I := 0 to Layout.Size - 1 do
              Temp[I] := RowA[I] + RowB[I];
            Delta := HistoCost(Pointer(Temp), Layout) - CostA - CostB;

            if Delta < BestDelta then
            begin
              BestDelta := Delta;
              Best := G;
              J := K;
            end;
          end;
      end;

    if Best < 0 then
      Break;

    for T := 0 to NumTiles - 1 do
      if TileGroup[T] = J then
        TileGroup[T] := Best;
    RebuildGroups;
  end;

  // Groups in use are numbered from zero
  SetLength(Map, NumGroups);
  Result := 0;
  for G := 0 to NumGroups - 1 do
    if GroupUsed[G] then
    begin
      Map[G] := Result;
      Inc(Result);
    end;

  Last := 0;
  for T := 0 to NumTiles - 1 do
    if TileGroup[T] >= 0 then
    begin
      TileGroup[T] := Map[TileGroup[T]];
      Last := TileGroup[T];
    end
    else
      TileGroup[T] := Last;
end;

{ Second pass of backward references: with costs of symbols known from the
  first pass (Histo: its symbols, one group), finds the cheapest way to give
  the image as literals, cache references and copies. Cost of reaching each
  pixel is kept; from each pixel a literal and copies of the longest match
  there (cut to shorter lengths too) are tried. Tokens are literals and
  copies, the cache is to be applied to them. }
procedure OptimalTokens(Img: PUInt32; W, N, CacheBits: Integer; const Layout: THistoLayout;
  Histo: PUInt32; const Params: TEncoderParams; var Tokens: TTokens; out Count: Integer);
const
  Infinity: Single = 1.0e30;
  ReuseLength = 32;   // match of the previous pixel this long is good here too
  AllLengths = 128;   // every length up to this one is tried
var
  SymCost: array of Single;
  LenCost: array[0..MAX_COPY_LENGTH] of Single;
  Cost: array of Single;
  FromLen: array of Word;
  FromCode: TIntArray;
  MatchLen: array of Word;
  MatchCode: TIntArray;
  Cache: TUInt32Array;
  M: TMatcher;
  S, I, K, Len, Dist, Code, ExtraBits, MaxK, Key, Pos: Integer;
  ExtraValue, Sum, Argb: UInt32;
  LogSum, C, Base: Single;
begin
  // Bits a symbol takes: log2(total / count); symbols not seen yet
  // are taken as seen once
  SetLength(SymCost, Layout.Size);
  for S := 0 to HISTO_SEGMENTS - 1 do
  begin
    Sum := 0;
    for I := Layout.Start[S] to Layout.Start[S + 1] - 1 do
      Inc(Sum, Histo[I]);
    if Sum = 0 then
      LogSum := 0
    else
      LogSum := SLog2(Sum) / Sum;

    for I := Layout.Start[S] to Layout.Start[S + 1] - 1 do
      if Histo[I] = 0 then
        SymCost[I] := LogSum
      else
        SymCost[I] := LogSum - SLog2(Histo[I]) / Histo[I];
  end;

  LenCost[0] := 0;
  for I := 1 to MAX_COPY_LENGTH do
  begin
    PrefixEncode(I, Code, ExtraBits, ExtraValue);
    LenCost[I] := SymCost[NUM_LITERAL_CODES + Code] + ExtraBits;
  end;

  // The longest match at every pixel
  SetLength(MatchLen, N);
  SetLength(MatchCode, N);
  MatcherInit(M, Img, W, N, Params.ChainSearch);
  Len := 0;
  Dist := 0;

  for I := 0 to N - 1 do
  begin
    if Len - 1 >= ReuseLength then
      Dec(Len)
    else
      MatcherFind(M, I, Len, Dist);

    MatchLen[I] := Len;
    if Len > 0 then
      MatchCode[I] := DistanceToCode(W, Dist);
    MatcherInsertUpTo(M, I);
  end;

  SetLength(Cost, N + 1);
  SetLength(FromLen, N + 1);
  SetLength(FromCode, N + 1);
  for I := 1 to N do
    Cost[I] := Infinity;
  Cost[0] := 0;
  if CacheBits > 0 then
    SetLength(Cache, 1 shl CacheBits);

  for I := 0 to N - 1 do
  begin
    Base := Cost[I];
    Argb := Img[I];
    Key := 0;

    // What is in the cache does not depend on the tokens: every pixel
    // gets there
    if CacheBits > 0 then
      Key := CacheKey(Argb, CacheBits);
    if (CacheBits > 0) and (Cache[Key] = Argb) then
      C := SymCost[NUM_LITERAL_CODES + NUM_LENGTH_CODES + Key]
    else
      C := SymCost[(Argb shr 8) and $FF] + SymCost[Layout.Start[1] + Integer((Argb shr 16) and $FF)] +
           SymCost[Layout.Start[2] + Integer(Argb and $FF)] + SymCost[Layout.Start[3] + Integer(Argb shr 24)];
    if CacheBits > 0 then
      Cache[Key] := Argb;

    if Base + C < Cost[I + 1] then
    begin
      Cost[I + 1] := Base + C;
      FromLen[I + 1] := 1;
      FromCode[I + 1] := 0;
    end;

    Len := MatchLen[I];
    if Len >= 3 then
    begin
      PrefixEncode(MatchCode[I], Code, ExtraBits, ExtraValue);
      Base := Base + SymCost[Layout.Start[4] + Code] + ExtraBits;
      MaxK := Len;
      if MaxK > AllLengths then
        MaxK := AllLengths;

      for K := 3 to MaxK do
        if Base + LenCost[K] < Cost[I + K] then
        begin
          Cost[I + K] := Base + LenCost[K];
          FromLen[I + K] := K;
          FromCode[I + K] := MatchCode[I];
        end;

      if (Len > MaxK) and (Base + LenCost[Len] < Cost[I + Len]) then
      begin
        Cost[I + Len] := Base + LenCost[Len];
        FromLen[I + Len] := Len;
        FromCode[I + Len] := MatchCode[I];
      end;
    end;
  end;

  // The cheapest path, from the end
  Count := 0;
  Pos := N;
  while Pos > 0 do
  begin
    Inc(Count);
    Dec(Pos, FromLen[Pos]);
  end;

  SetLength(Tokens, Count);
  Pos := N;
  I := Count;

  while Pos > 0 do
  begin
    Dec(I);
    Len := FromLen[Pos];
    Tokens[I].Len := Len;

    if FromCode[Pos] = 0 then
    begin
      Tokens[I].Kind := TOKEN_LITERAL;
      Tokens[I].Value := Img[Pos - 1];
    end
    else
    begin
      Tokens[I].Kind := TOKEN_COPY;
      Tokens[I].Value := FromCode[Pos];
    end;

    Dec(Pos, Len);
  end;
end;

{ Histograms of groups: each token is counted in the group of the tile
  where it starts. With one group TileGroup is not used. }
procedure CountGroupSymbols(const Tokens: TTokens; Count, W, TileBits, TilesX, NumGroups: Integer;
  const TileGroup: TIntArray; const Layout: THistoLayout; var Histos: TUInt32Array);
var
  T, G, X, Y: Integer;
begin
  Histos := nil;
  SetLength(Histos, NumGroups * Layout.Size);
  X := 0;
  Y := 0;

  for T := 0 to Count - 1 do
  begin
    G := 0;
    if NumGroups > 1 then
      G := TileGroup[(Y shr TileBits) * TilesX + (X shr TileBits)];
    AddTokenToHisto(@Histos[G * Layout.Size], Layout, Tokens[T]);
    Inc(X, Tokens[T].Len);
    while X >= W do
    begin
      Dec(X, W);
      Inc(Y);
    end;
  end;
end;

{ Coding of an image }

{ Writes image of W x H pixels: color cache size, groups of Huffman codes
  with the image that tells which tile uses which group (only the main
  image has that, images of transform data are coded with one group and
  without the cache), and the pixels. }
procedure EncodeImage(var BW: TBitWriter; Img: PUInt32; W, H: Integer; IsMain: Boolean;
  const Params: TEncoderParams);
const
  // Estimate of the image of groups: part of the entropy of group numbers
  // (tiles of a group are next to each other, so copies are used) and its codes
  GroupImageShare: Double = 0.75;
  GroupImageBase: Double = 60.0;
var
  Tokens: TTokens;
  Count, CacheBits, TileBits, TilesX, TilesY, NumTiles, NumGroups: Integer;
  T, G, S, X, Y, Tile, Code, ExtraBits, Size: Integer;
  ExtraValue, Argb: UInt32;
  Layout: THistoLayout;
  Histos: TUInt32Array;
  TileGroup: TIntArray;
  GroupImage: TUInt32Array;
  Codes: array of THuffmanCode;  // five per group
  C: ^THuffmanCode;
  FirstBits, Cand, CandBits, CandTilesX, CandTiles, CandGroups: Integer;
  CandTileGroup: TIntArray;
  GroupCounts: TUInt32Array;
  Cost, BestCost: Double;
begin
  FindTokens(Img, W, W * H, Params, Tokens, Count);

  CacheBits := 0;
  if IsMain and (Params.MaxCacheBits > 0) then
    CacheBits := ChooseCacheBits(Img, Tokens, Count, Params.MaxCacheBits);

  InitHistoLayout(Layout, CacheBits);

  if CacheBits > 0 then
  begin
    PutBits(BW, 1, 1);
    PutBits(BW, CacheBits, 4);
    ApplyCache(Img, Tokens, Count, CacheBits);
  end
  else
    PutBits(BW, 0, 1);

  if IsMain and Params.OptimalParse then
  begin
    SetLength(Histos, Layout.Size);
    for T := 0 to Count - 1 do
      AddTokenToHisto(Pointer(Histos), Layout, Tokens[T]);
    OptimalTokens(Img, W, W * H, CacheBits, Layout, Pointer(Histos), Params, Tokens, Count);
    if CacheBits > 0 then
      ApplyCache(Img, Tokens, Count, CacheBits);
    Histos := nil;
  end;

  NumGroups := 1;
  TileBits := 0;
  TilesX := 1;

  if IsMain then
  begin
    if Params.MetaCodes then
    begin
      // The smallest tiles that do not make too many of them
      FirstBits := MIN_TILE_BITS;
      while (FirstBits < MAX_TILE_BITS) and
            (SubSampleSize(W, FirstBits) * SubSampleSize(H, FirstBits) > Params.MaxTiles) do
        Inc(FirstBits);

      // One group for everything is what the others must beat
      CountGroupSymbols(Tokens, Count, W, 0, 1, 1, nil, Layout, Histos);
      BestCost := HistoCost(Pointer(Histos), Layout);

      // Smaller tiles follow the image better, but the image of groups
      // costs more: a few sizes are tried, estimated costs compared
      for Cand := 0 to Params.TileSizesTried - 1 do
      begin
        CandBits := FirstBits + Cand;
        if CandBits > MAX_TILE_BITS then
          Break;
        CandTilesX := SubSampleSize(W, CandBits);
        CandTiles := CandTilesX * SubSampleSize(H, CandBits);
        if CandTiles <= 1 then
          Break;

        Histos := nil;
        SetLength(Histos, CandTiles * Layout.Size);
        X := 0;
        Y := 0;

        for T := 0 to Count - 1 do
        begin
          Tile := (Y shr CandBits) * CandTilesX + (X shr CandBits);
          AddTokenToHisto(@Histos[Tile * Layout.Size], Layout, Tokens[T]);
          Inc(X, Tokens[T].Len);
          while X >= W do
          begin
            Dec(X, W);
            Inc(Y);
          end;
        end;

        CandGroups := GroupTiles(Histos, CandTiles, Layout, Params, CandTileGroup);
        if CandGroups <= 1 then
          Continue;

        CountGroupSymbols(Tokens, Count, W, CandBits, CandTilesX, CandGroups, CandTileGroup,
                          Layout, Histos);
        Cost := 0;
        for G := 0 to CandGroups - 1 do
          Cost := Cost + HistoCost(@Histos[G * Layout.Size], Layout);

        // And the image of groups
        GroupCounts := nil;
        SetLength(GroupCounts, CandGroups);
        for T := 0 to CandTiles - 1 do
          Inc(GroupCounts[CandTileGroup[T]]);
        Cost := Cost + GroupImageShare * BitsEntropy(Pointer(GroupCounts), CandGroups) + GroupImageBase;

        if Cost < BestCost then
        begin
          BestCost := Cost;
          NumGroups := CandGroups;
          TileBits := CandBits;
          TilesX := CandTilesX;
          TilesY := SubSampleSize(H, CandBits);
          NumTiles := CandTiles;
          TileGroup := Copy(CandTileGroup, 0, CandTiles);
        end;
      end;
    end;

    if NumGroups > 1 then
    begin
      PutBits(BW, 1, 1);
      PutBits(BW, TileBits - 2, 3);
      // Number of the group is in red and green
      SetLength(GroupImage, NumTiles);
      for T := 0 to NumTiles - 1 do
        GroupImage[T] := UInt32(TileGroup[T]) shl 8;
      EncodeImage(BW, Pointer(GroupImage), TilesX, TilesY, False, Params);
    end
    else
      PutBits(BW, 0, 1);
  end;

  // Histograms of the groups and codes made of them
  CountGroupSymbols(Tokens, Count, W, TileBits, TilesX, NumGroups, TileGroup, Layout, Histos);

  SetLength(Codes, NumGroups * HISTO_SEGMENTS);

  for G := 0 to NumGroups - 1 do
    for S := 0 to HISTO_SEGMENTS - 1 do
    begin
      Size := Layout.Start[S + 1] - Layout.Start[S];
      BuildHuffmanCode(Codes[G * HISTO_SEGMENTS + S],
                       @Histos[G * Layout.Size + Layout.Start[S]], Size);
      StoreHuffmanCode(BW, Codes[G * HISTO_SEGMENTS + S]);
    end;

  // Pixels
  X := 0;
  Y := 0;

  for T := 0 to Count - 1 do
  begin
    G := 0;
    if NumGroups > 1 then
      G := TileGroup[(Y shr TileBits) * TilesX + (X shr TileBits)];
    C := @Codes[G * HISTO_SEGMENTS];

    case Tokens[T].Kind of
      TOKEN_LITERAL:
        begin
          Argb := Tokens[T].Value;
          S := (Argb shr 8) and $FF;
          PutBits(BW, C^.Codes[S], C^.Lengths[S]);
          Inc(C);
          S := (Argb shr 16) and $FF;
          PutBits(BW, C^.Codes[S], C^.Lengths[S]);
          Inc(C);
          S := Argb and $FF;
          PutBits(BW, C^.Codes[S], C^.Lengths[S]);
          Inc(C);
          S := Argb shr 24;
          PutBits(BW, C^.Codes[S], C^.Lengths[S]);
        end;
      TOKEN_CACHE:
        begin
          S := NUM_LITERAL_CODES + NUM_LENGTH_CODES + Integer(Tokens[T].Value);
          PutBits(BW, C^.Codes[S], C^.Lengths[S]);
        end;
      TOKEN_COPY:
        begin
          PrefixEncode(Tokens[T].Len, Code, ExtraBits, ExtraValue);
          S := NUM_LITERAL_CODES + Code;
          PutBits(BW, C^.Codes[S], C^.Lengths[S]);
          PutBits(BW, ExtraValue, ExtraBits);
          Inc(C, 4);
          PrefixEncode(Tokens[T].Value, Code, ExtraBits, ExtraValue);
          PutBits(BW, C^.Codes[Code], C^.Lengths[Code]);
          PutBits(BW, ExtraValue, ExtraBits);
        end;
    end;

    Inc(X, Tokens[T].Len);
    while X >= W do
    begin
      Dec(X, W);
      Inc(Y);
    end;
  end;
end;

{ Transforms }

procedure WriteTransformHeader(var BW: TBitWriter; Kind: Integer);
begin
  PutBits(BW, 1, 1);
  PutBits(BW, Kind, 2);
end;

procedure SubtractGreen(Img: PUInt32; N: Integer);
var
  I: Integer;
  Px, G: UInt32;
begin
  for I := 0 to N - 1 do
  begin
    Px := Img[I];
    G := (Px shr 8) and $FF;
    Img[I] := (Px and $FF00FF00) or (((Px shr 16) - G) and $FF) shl 16 or ((Px - G) and $FF);
  end;
end;

{ Picks a predictor for each tile of 2^Bits pixels (the one whose residuals
  have the lowest estimated cost), writes the transform with the image of
  the predictors and replaces pixels by residuals. }
procedure PredictorTransform(var BW: TBitWriter; Img: PUInt32; W, H, Bits: Integer;
  const Params: TEncoderParams);
const
  // Order in which predictors are tried when not all of them are
  ModeOrder: array[0..NUM_PREDICTORS - 1] of Byte = (11, 12, 1, 2, 13, 5, 7, 10, 6, 8, 9, 3, 4, 0);
  // Tile likes the predictor of its neighbors: images of predictors
  // with runs are cheaper
  NeighborBias: Double = 15.0;
  SpatialDecay: Double = 0.6;
  SpatialStart: Double = 0.94;
  SpatialWeight: Double = -0.1;
  HistoSize = 4 * 256;
var
  TilesX, TilesY, TileSize, TX, TY, X, Y, X0, X1, Y0, Y1, M, Mode, Ch, I, K: Integer;
  BestMode, NumModes: Integer;
  Modes, Residuals: TUInt32Array;
  Histo: TUInt32Array;  // per predictor: alpha, red, green, blue residuals
  Acc: array[0..HistoSize - 1] of UInt32;  // residuals of the tiles done
  AccSums: array[0..3] of THistoSums;
  Used: array of Byte;  // values each histogram has, 256 per histogram
  UsedCount: array[0..NUM_PREDICTORS * 4 - 1] of Integer;

  P: TPredictions;
  Row, H4: PUInt32;
  Px, Res, Pred: UInt32;
  Cost, BestCost, Weight, Sum: Double;

  procedure CountValue(Slot: Integer; Value: UInt32); {$IFDEF FPC}inline;{$ENDIF}
  var
    Bin: PUInt32;
  begin
    Bin := @Histo[Slot * 256 + Integer(Value)];
    if Bin^ = 0 then
    begin
      Used[Slot * 256 + UsedCount[Slot]] := Value;
      Inc(UsedCount[Slot]);
    end;
    Inc(Bin^);
  end;

begin
  TilesX := SubSampleSize(W, Bits);
  TilesY := SubSampleSize(H, Bits);
  TileSize := 1 shl Bits;
  SetLength(Modes, TilesX * TilesY);
  SetLength(Histo, NUM_PREDICTORS * HistoSize);
  FillChar(Acc, SizeOf(Acc), 0);
  FillChar(AccSums, SizeOf(AccSums), 0);
  SetLength(Used, NUM_PREDICTORS * HistoSize);
  FillChar(UsedCount, SizeOf(UsedCount), 0);
  NumModes := Params.PredictorModes;

  for TY := 0 to TilesY - 1 do
    for TX := 0 to TilesX - 1 do
    begin
      BestMode := ModeOrder[0];

      if NumModes > 1 then
      begin
        // Histograms are cleared and read by lists of the values they have:
        // a tile has few of them
        for K := 0 to NUM_PREDICTORS * 4 - 1 do
        begin
          for I := 0 to UsedCount[K] - 1 do
            Histo[K * 256 + Used[K * 256 + I]] := 0;
          UsedCount[K] := 0;
        end;

        X0 := TX shl Bits;
        X1 := X0 + TileSize;
        if X1 > W then X1 := W;
        Y0 := TY shl Bits;
        Y1 := Y0 + TileSize;
        if Y1 > H then Y1 := H;
        // The first row and column have fixed predictors
        if X0 = 0 then X0 := 1;
        if Y0 = 0 then Y0 := 1;

        for Y := Y0 to Y1 - 1 do
        begin
          Row := Img + Y * W;
          for X := X0 to X1 - 1 do
          begin
            Px := Row[X];
            PredictAll(Row[X - 1], (Row + X - W)^, (Row + X - W - 1)^, (Row + X - W + 1)^, P);
            for M := 0 to NumModes - 1 do
            begin
              Mode := ModeOrder[M];
              Res := SubPixels(Px, P[Mode]);
              CountValue(Mode * 4, Res shr 24);
              CountValue(Mode * 4 + 1, (Res shr 16) and $FF);
              CountValue(Mode * 4 + 2, (Res shr 8) and $FF);
              CountValue(Mode * 4 + 3, Res and $FF);
            end;
          end;
        end;

        BestCost := 0;

        for M := 0 to NumModes - 1 do
        begin
          Mode := ModeOrder[M];
          Cost := 0;

          for Ch := 0 to 3 do
          begin
            H4 := @Histo[Mode * HistoSize + Ch * 256];
            // Small residuals are good
            Sum := H4[0];
            Weight := SpatialStart;
            for I := 1 to 15 do
            begin
              Sum := Sum + Weight * (H4[I] + H4[256 - I]);
              Weight := Weight * SpatialDecay;
            end;
            Cost := Cost + SpatialWeight * Sum +
                    CombinedEntropyOfList(H4, @Acc[Ch * 256], AccSums[Ch],
                                          @Used[(Mode * 4 + Ch) * 256], UsedCount[Mode * 4 + Ch]);
          end;

          if (TX > 0) and (Integer(Modes[TY * TilesX + TX - 1] shr 8) and $F = Mode) then
            Cost := Cost - NeighborBias;
          if (TY > 0) and (Integer(Modes[(TY - 1) * TilesX + TX] shr 8) and $F = Mode) then
            Cost := Cost - NeighborBias;

          if (M = 0) or (Cost < BestCost) then
          begin
            BestCost := Cost;
            BestMode := Mode;
          end;
        end;

        for Ch := 0 to 3 do
        begin
          K := BestMode * 4 + Ch;
          for I := 0 to UsedCount[K] - 1 do
            Inc(Acc[Ch * 256 + Used[K * 256 + I]], Histo[K * 256 + Used[K * 256 + I]]);
        end;
        for Ch := 0 to 3 do
          GetHistoSums(@Acc[Ch * 256], AccSums[Ch]);
      end;

      Modes[TY * TilesX + TX] := ARGB_BLACK or (UInt32(BestMode) shl 8);
    end;

  WriteTransformHeader(BW, TRANSFORM_PREDICTOR);
  PutBits(BW, Bits - 2, 3);
  EncodeImage(BW, Pointer(Modes), TilesX, TilesY, False, Params);

  // Residuals are made of the original pixels, so to another buffer
  SetLength(Residuals, W * H);

  for Y := 0 to H - 1 do
  begin
    Row := Img + Y * W;
    for X := 0 to W - 1 do
    begin
      if Y = 0 then
      begin
        if X = 0 then Pred := ARGB_BLACK else Pred := Row[X - 1];
      end
      else if X = 0 then
        Pred := (Row - W)^
      else
      begin
        Mode := (Modes[(Y shr Bits) * TilesX + (X shr Bits)] shr 8) and $F;
        Pred := Predict(Mode, Row[X - 1], (Row + X - W)^, (Row + X - W - 1)^, (Row + X - W + 1)^);
      end;
      Residuals[Y * W + X] := SubPixels(Row[X], Pred);
    end;
  end;

  Move(Pointer(Residuals)^, Img^, W * H * SizeOf(UInt32));
end;

{ Cross color transform: in each tile of 2^Bits pixels red gets a multiple
  of green subtracted, blue multiples of green and red. Multipliers (signed,
  in 1/32) are searched for so that entropy of red and of blue is the lowest
  (ColorSpaceTransform of libwebp). Used on residuals of the predictor.
  Nothing is written when no tile gains. }
procedure CrossColorTransform(var BW: TBitWriter; Img: PUInt32; W, H, Bits: Integer;
  const Params: TEncoderParams);
const
  SameBias: Double = 3.0;        // multiplier of a neighbor tile or zero is preferred
  SpatialStart: Double = 2.4;
  SpatialDecay: Double = 0.6;
  SpatialWeight: Double = -0.1;
  SpatialWeightOfZero: Double = 3.0;
  RedSteps = 4;
  BlueDeltas: array[0..6] of Integer = (16, 16, 8, 4, 2, 2, 2);
  BlueOffsets: array[0..7, 0..1] of Integer =
    ((0, -1), (0, 1), (-1, 0), (1, 0), (-1, -1), (-1, 1), (1, -1), (1, 1));
var
  TilesX, TilesY, TileSize, TX, TY, X0, X1, Y0, Y1, I, Iter, Axis, Delta: Integer;
  GreenToRed, GreenToBlue, RedToBlue, Cand, CandB, BlueIters: Integer;
  LeftData, UpData: UInt32;
  Data: TUInt32Array;
  AccRed, AccBlue, Histo, BestHisto: array[0..255] of UInt32;
  AccRedSums, AccBlueSums: THistoSums;
  Cost, BestCost: Double;
  AnyUsed: Boolean;

  function Multiply(Multiplier, Color: Integer): Integer; {$IFDEF FPC}inline;{$ENDIF}
  begin
    // Arithmetic shift of the signed product
    if Multiplier * Color >= 0 then
      Result := (Multiplier * Color) shr 5
    else
      Result := -((-(Multiplier * Color) + 31) shr 5);
  end;

  function SpatialCost: Double;
  var
    I: Integer;
    Sum, Weight: Double;
  begin
    Sum := SpatialWeightOfZero * Histo[0];
    Weight := SpatialStart;
    for I := 1 to 15 do
    begin
      Sum := Sum + Weight * (Histo[I] + Histo[256 - I]);
      Weight := Weight * SpatialDecay;
    end;
    Result := SpatialWeight * Sum;
  end;

  function RedCost(G2R: Integer): Double;
  var
    X, Y: Integer;
    Px: UInt32;
    Row: PUInt32;
  begin
    FillChar(Histo, SizeOf(Histo), 0);
    for Y := Y0 to Y1 - 1 do
    begin
      Row := Img + Y * W;
      for X := X0 to X1 - 1 do
      begin
        Px := Row[X];
        Inc(Histo[(Integer(Px shr 16) - Multiply(G2R, ShortInt(Px shr 8))) and $FF]);
      end;
    end;

    Result := CombinedEntropy(@Histo[0], @AccRed[0], AccRedSums) + SpatialCost;
    if G2R = ShortInt(LeftData and $FF) then Result := Result - SameBias;
    if G2R = ShortInt(UpData and $FF) then Result := Result - SameBias;
    if G2R = 0 then Result := Result - SameBias;
  end;

  function BlueCost(G2B, R2B: Integer): Double;
  var
    X, Y: Integer;
    Px: UInt32;
    Row: PUInt32;
  begin
    FillChar(Histo, SizeOf(Histo), 0);
    for Y := Y0 to Y1 - 1 do
    begin
      Row := Img + Y * W;
      for X := X0 to X1 - 1 do
      begin
        Px := Row[X];
        Inc(Histo[(Integer(Px) - Multiply(G2B, ShortInt(Px shr 8)) -
                   Multiply(R2B, ShortInt(Px shr 16))) and $FF]);
      end;
    end;

    Result := CombinedEntropy(@Histo[0], @AccBlue[0], AccBlueSums) + SpatialCost;
    if G2B = ShortInt((LeftData shr 8) and $FF) then Result := Result - SameBias;
    if G2B = ShortInt((UpData shr 8) and $FF) then Result := Result - SameBias;
    if R2B = ShortInt((LeftData shr 16) and $FF) then Result := Result - SameBias;
    if R2B = ShortInt((UpData shr 16) and $FF) then Result := Result - SameBias;
    if G2B = 0 then Result := Result - SameBias;
    if R2B = 0 then Result := Result - SameBias;
  end;

  procedure ApplyToTile(G2R, G2B, R2B: Integer);
  var
    X, Y, Red, Blue: Integer;
    Px: UInt32;
    Row: PUInt32;
  begin
    for Y := Y0 to Y1 - 1 do
    begin
      Row := Img + Y * W;
      for X := X0 to X1 - 1 do
      begin
        Px := Row[X];
        Red := (Integer(Px shr 16) - Multiply(G2R, ShortInt(Px shr 8))) and $FF;
        Blue := (Integer(Px) - Multiply(G2B, ShortInt(Px shr 8)) -
                 Multiply(R2B, ShortInt(Px shr 16))) and $FF;
        Row[X] := (Px and $FF00FF00) or (UInt32(Red) shl 16) or UInt32(Blue);
      end;
    end;
  end;

begin
  TilesX := SubSampleSize(W, Bits);
  TilesY := SubSampleSize(H, Bits);
  TileSize := 1 shl Bits;
  SetLength(Data, TilesX * TilesY);
  FillChar(AccRed, SizeOf(AccRed), 0);
  FillChar(AccBlue, SizeOf(AccBlue), 0);
  FillChar(AccRedSums, SizeOf(AccRedSums), 0);
  FillChar(AccBlueSums, SizeOf(AccBlueSums), 0);
  AnyUsed := False;
  if Params.ChainSearch >= 160 then BlueIters := 7 else BlueIters := 4;

  // Multipliers of all tiles first: the transform is written before
  // the image of them, only when it is of use
  for TY := 0 to TilesY - 1 do
    for TX := 0 to TilesX - 1 do
    begin
      X0 := TX shl Bits;
      X1 := X0 + TileSize;
      if X1 > W then X1 := W;
      Y0 := TY shl Bits;
      Y1 := Y0 + TileSize;
      if Y1 > H then Y1 := H;
      LeftData := 0;
      UpData := 0;
      if TX > 0 then LeftData := Data[TY * TilesX + TX - 1];
      if TY > 0 then UpData := Data[(TY - 1) * TilesX + TX];

      // Green to red: steps of 32, 16, 8, 4 to both sides of the best one
      GreenToRed := 0;
      BestCost := RedCost(0);
      BestHisto := Histo;

      for Iter := 0 to RedSteps - 1 do
      begin
        Delta := 32 shr Iter;
        Cand := GreenToRed;
        for Axis := 0 to 1 do
        begin
          I := Cand - Delta + Axis * 2 * Delta;
          if (I < -128) or (I > 127) then
            Continue;
          Cost := RedCost(I);
          if Cost < BestCost then
          begin
            BestCost := Cost;
            GreenToRed := I;
            BestHisto := Histo;
          end;
        end;
      end;

      for I := 0 to 255 do
        Inc(AccRed[I], BestHisto[I]);
      GetHistoSums(@AccRed[0], AccRedSums);

      // Green to blue and red to blue together: eight directions around
      // the best pair, by smaller and smaller steps
      GreenToBlue := 0;
      RedToBlue := 0;
      BestCost := BlueCost(0, 0);
      BestHisto := Histo;

      for Iter := 0 to BlueIters - 1 do
      begin
        Delta := BlueDeltas[Iter];
        Cand := GreenToBlue;
        CandB := RedToBlue;

        for Axis := 0 to 7 do
        begin
          I := Cand + BlueOffsets[Axis, 0] * Delta;
          if (I < -128) or (I > 127) or (CandB + BlueOffsets[Axis, 1] * Delta < -128) or
             (CandB + BlueOffsets[Axis, 1] * Delta > 127) then
            Continue;
          Cost := BlueCost(I, CandB + BlueOffsets[Axis, 1] * Delta);
          if Cost < BestCost then
          begin
            BestCost := Cost;
            GreenToBlue := I;
            RedToBlue := CandB + BlueOffsets[Axis, 1] * Delta;
            BestHisto := Histo;
          end;
        end;

        // Nothing better around at the smallest step
        if (Delta = 2) and (GreenToBlue = Cand) and (RedToBlue = CandB) then
          Break;
      end;

      for I := 0 to 255 do
        Inc(AccBlue[I], BestHisto[I]);
      GetHistoSums(@AccBlue[0], AccBlueSums);

      Data[TY * TilesX + TX] := ARGB_BLACK or (UInt32(RedToBlue and $FF) shl 16) or
                                (UInt32(GreenToBlue and $FF) shl 8) or UInt32(GreenToRed and $FF);
      if (GreenToRed <> 0) or (GreenToBlue <> 0) or (RedToBlue <> 0) then
        AnyUsed := True;
    end;

  if not AnyUsed then
    Exit;

  for TY := 0 to TilesY - 1 do
    for TX := 0 to TilesX - 1 do
    begin
      X0 := TX shl Bits;
      X1 := X0 + TileSize;
      if X1 > W then X1 := W;
      Y0 := TY shl Bits;
      Y1 := Y0 + TileSize;
      if Y1 > H then Y1 := H;
      LeftData := Data[TY * TilesX + TX];
      ApplyToTile(ShortInt(LeftData and $FF), ShortInt((LeftData shr 8) and $FF),
                  ShortInt((LeftData shr 16) and $FF));
    end;

  WriteTransformHeader(BW, TRANSFORM_CROSS_COLOR);
  PutBits(BW, Bits - 2, 3);
  EncodeImage(BW, Pointer(Data), TilesX, TilesY, False, Params);
end;

{ Palette }

const
  MAX_PALETTE_SIZE = 256;
  PALETTE_HASH_SIZE = 1024;

type
  TPalette = record
    Count: Integer;
    Colors: array[0..MAX_PALETTE_SIZE - 1] of UInt32;
    // Hash table of indices to Colors, -1 = free
    Slots: array[0..PALETTE_HASH_SIZE - 1] of SmallInt;
  end;

function PaletteSlot(const Palette: TPalette; Color: UInt32): Integer;
begin
  Result := Integer(UInt32(Color * UInt32($9E3779B1)) shr 22);
  while (Palette.Slots[Result] >= 0) and (Palette.Colors[Palette.Slots[Result]] <> Color) do
    Result := (Result + 1) and (PALETTE_HASH_SIZE - 1);
end;

{ Collects colors of the image, returns False when there are more than 256.
  Colors are sorted by value. }
function BuildPalette(Img: PUInt32; N: Integer; out Palette: TPalette): Boolean;
var
  I, J, Slot: Integer;
  Color, Last: UInt32;
begin
  Result := False;
  Palette.Count := 0;
  FillChar(Palette.Slots, SizeOf(Palette.Slots), $FF);
  if N = 0 then
    Exit;
  Last := not Img[0];

  for I := 0 to N - 1 do
  begin
    Color := Img[I];
    if Color = Last then
      Continue;
    Last := Color;
    Slot := PaletteSlot(Palette, Color);

    if Palette.Slots[Slot] < 0 then
    begin
      if Palette.Count = MAX_PALETTE_SIZE then
        Exit;
      Palette.Colors[Palette.Count] := Color;
      Palette.Slots[Slot] := Palette.Count;
      Inc(Palette.Count);
    end;
  end;

  // Sorted colors make small differences between neighbors in the palette
  for I := 1 to Palette.Count - 1 do
  begin
    Color := Palette.Colors[I];
    J := I - 1;
    while (J >= 0) and (Palette.Colors[J] > Color) do
    begin
      Palette.Colors[J + 1] := Palette.Colors[J];
      Dec(J);
    end;
    Palette.Colors[J + 1] := Color;
  end;

  FillChar(Palette.Slots, SizeOf(Palette.Slots), $FF);
  for I := 0 to Palette.Count - 1 do
    Palette.Slots[PaletteSlot(Palette, Palette.Colors[I])] := I;
  Result := True;
end;

{ Writes the color indexing transform and replaces pixels by indices
  (in green). With 16 colors or less several pixels are packed to one,
  the image gets narrower: returns its new width. }
function PaletteTransform(var BW: TBitWriter; Img: PUInt32; W, H: Integer;
  const Palette: TPalette; const Params: TEncoderParams): Integer;
var
  I, X, Y, PackBits, BitsPerPixel, Shift: Integer;
  Deltas: array[0..MAX_PALETTE_SIZE - 1] of UInt32;
  Src, Dst: PUInt32;
  Last, Value: UInt32;
  LastIndex: Integer;
begin
  WriteTransformHeader(BW, TRANSFORM_COLOR_INDEX);
  PutBits(BW, Palette.Count - 1, 8);

  // Colors are stored as differences from the previous one
  Deltas[0] := Palette.Colors[0];
  for I := 1 to Palette.Count - 1 do
    Deltas[I] := SubPixels(Palette.Colors[I], Palette.Colors[I - 1]);
  EncodeImage(BW, @Deltas[0], Palette.Count, 1, False, Params);

  if Palette.Count <= 2 then PackBits := 3
  else if Palette.Count <= 4 then PackBits := 2
  else if Palette.Count <= 16 then PackBits := 1
  else PackBits := 0;

  BitsPerPixel := 8 shr PackBits;
  Result := SubSampleSize(W, PackBits);
  Last := not Img[0];
  LastIndex := 0;

  // In place: packed pixel is never after the pixels it is made of
  for Y := 0 to H - 1 do
  begin
    Src := Img + Y * W;
    Dst := Img + Y * Result;
    Value := 0;

    for X := 0 to W - 1 do
    begin
      if Src[X] <> Last then
      begin
        Last := Src[X];
        LastIndex := Palette.Slots[PaletteSlot(Palette, Last)];
      end;

      if PackBits = 0 then
        Dst[X] := ARGB_BLACK or (UInt32(LastIndex) shl 8)
      else
      begin
        // The first pixel goes to the lowest bits
        Shift := (X and ((1 shl PackBits) - 1)) * BitsPerPixel;
        if Shift = 0 then
          Value := 0;
        Value := Value or (UInt32(LastIndex) shl Shift);
        if (Shift + BitsPerPixel = 8) or (X = W - 1) then
          Dst[X shr PackBits] := ARGB_BLACK or (Value shl 8);
      end;
    end;
  end;
end;

{ Choice of transforms }

{ Estimates which transforms make the image the cheapest (AnalyzeEntropy
  of libwebp): entropies of the channels as they are, as differences from
  the previous pixel (stands for the predictor), with green subtracted,
  and of palette indices. }
procedure ChooseTransforms(Img: PUInt32; W, H, PaletteCount, PredictorBits: Integer;
  out Config: TEncoderConfig);
const
  hAlpha = 0; hRed = 1; hGreen = 2; hBlue = 3;
  hAlphaPred = 4; hRedPred = 5; hGreenPred = 6; hBluePred = 7;
  hRedSubGreen = 8; hBlueSubGreen = 9; hRedPredSubGreen = 10; hBluePredSubGreen = 11;
  hPalette = 12;
  NumHistos = 13;
  mDirect = 0; mSpatial = 1; mSubGreen = 2; mSpatialSubGreen = 3; mPalette = 4;
  Log2Of14: Double = 3.807354922;
  Log2Of24: Double = 4.584962501;
var
  Histo: array[0..NumHistos * 256 - 1] of UInt32;
  E: array[0..NumHistos - 1] of Double;
  Cost: array[mDirect..mPalette] of Double;
  X, Y, I, Tiles, Best, Last: Integer;
  Row, PrevRow: PUInt32;
  Pix, Prev, Diff, G: UInt32;

  procedure Add(Index: Integer; Value: UInt32);
  begin
    Inc(Histo[Index * 256 + Integer(Value and $FF)]);
  end;

begin
  FillChar(Config, SizeOf(Config), 0);
  FillChar(Histo, SizeOf(Histo), 0);
  Prev := Img[0];
  PrevRow := nil;

  for Y := 0 to H - 1 do
  begin
    Row := Img + Y * W;
    for X := 0 to W - 1 do
    begin
      Pix := Row[X];
      Diff := SubPixels(Pix, Prev);
      Prev := Pix;
      // Repeated pixels are left to backward references
      if (Diff = 0) or ((PrevRow <> nil) and (Pix = PrevRow[X])) then
        Continue;

      Add(hAlpha, Pix shr 24);
      Add(hRed, Pix shr 16);
      Add(hGreen, Pix shr 8);
      Add(hBlue, Pix);
      Add(hAlphaPred, Diff shr 24);
      Add(hRedPred, Diff shr 16);
      Add(hGreenPred, Diff shr 8);
      Add(hBluePred, Diff);
      G := (Pix shr 8) and $FF;
      Add(hRedSubGreen, (Pix shr 16) - G);
      Add(hBlueSubGreen, Pix - G);
      G := (Diff shr 8) and $FF;
      Add(hRedPredSubGreen, (Diff shr 16) - G);
      Add(hBluePredSubGreen, Diff - G);
      Add(hPalette, UInt32((UInt64(Pix) + (Pix shr 19)) * UInt64($39C5FBA7)) shr 24);
    end;
    PrevRow := Row;
  end;

  for I := 0 to NumHistos - 1 do
    E[I] := BitsEntropy(@Histo[I * 256], 256);

  Tiles := SubSampleSize(W, PredictorBits) * SubSampleSize(H, PredictorBits);
  Cost[mDirect] := E[hAlpha] + E[hRed] + E[hGreen] + E[hBlue];
  Cost[mSpatial] := E[hAlphaPred] + E[hRedPred] + E[hGreenPred] + E[hBluePred] + Tiles * Log2Of14;
  Cost[mSubGreen] := E[hAlpha] + E[hRedSubGreen] + E[hGreen] + E[hBlueSubGreen];
  Cost[mSpatialSubGreen] := E[hAlphaPred] + E[hRedPredSubGreen] + E[hGreenPred] +
                            E[hBluePredSubGreen] + Tiles * Log2Of24;
  Cost[mPalette] := E[hPalette] + PaletteCount * 8;

  if PaletteCount > 0 then Last := mPalette else Last := mSpatialSubGreen;
  Best := mDirect;
  for I := mDirect to Last do
    if Cost[I] < Cost[Best] then
      Best := I;
  // Few colors: several pixels in one, nothing beats that
  if (PaletteCount > 0) and (PaletteCount <= 16) then
    Best := mPalette;

  Config.Palette := Best = mPalette;
  Config.SubGreen := (Best = mSubGreen) or (Best = mSpatialSubGreen);
  Config.Predictor := (Best = mSpatial) or (Best = mSpatialSubGreen);
  Config.CrossColor := Config.Predictor;
end;

{ Encoder }

procedure GetParams(Effort: Integer; out Params: TEncoderParams);
const
  ChainSearch: array[PasWebPMinEffort..PasWebPMaxEffort] of Integer =
    (2, 8, 16, 32, 48, 64, 96, 160, 320, 800);
  PredictorModes: array[PasWebPMinEffort..PasWebPMaxEffort] of Integer =
    (1, 4, 14, 14, 14, 14, 14, 14, 14, 14);
  MaxTiles: array[PasWebPMinEffort..PasWebPMaxEffort] of Integer =
    (0, 0, 0, 256, 512, 1024, 1024, 2048, 2600, 2600);
  MaxGroups: array[PasWebPMinEffort..PasWebPMaxEffort] of Integer =
    (1, 1, 1, 16, 24, 32, 48, 64, 64, 96);
  RefineGroups: array[PasWebPMinEffort..PasWebPMaxEffort] of Integer =
    (0, 0, 0, 0, 1, 1, 2, 2, 3, 3);
begin
  if Effort < PasWebPMinEffort then Effort := PasWebPMinEffort;
  if Effort > PasWebPMaxEffort then Effort := PasWebPMaxEffort;

  Params.ChainSearch := ChainSearch[Effort];
  Params.LazyMatching := Effort >= 2;
  Params.OptimalParse := Effort >= 4;
  if Effort >= 5 then Params.PredictorBits := 3
  else if Effort >= 2 then Params.PredictorBits := 4
  else Params.PredictorBits := 5;
  Params.PredictorModes := PredictorModes[Effort];
  Params.CrossColor := Effort >= 5;
  if Effort >= 2 then Params.MaxCacheBits := MAX_CACHE_BITS else Params.MaxCacheBits := 0;
  Params.MetaCodes := Effort >= 3;
  Params.MaxTiles := MaxTiles[Effort];
  Params.MaxGroups := MaxGroups[Effort];
  Params.RefineGroups := RefineGroups[Effort];
  if Effort >= 9 then Params.TileSizesTried := 3
  else if Effort >= 6 then Params.TileSizesTried := 2
  else Params.TileSizesTried := 1;
  Params.TryVariants := Effort >= 9;
end;

{ Encodes pixels (changed by the transforms) to VP8L stream: transforms
  and the image. Stream of a whole image starts with the header. }
procedure EncodeStream(var BW: TBitWriter; Img: PUInt32; W, H: Integer;
  const Config: TEncoderConfig; const Palette: TPalette; const Params: TEncoderParams);
var
  CodedWidth, Bits: Integer;
begin
  CodedWidth := W;

  if Config.Palette then
    CodedWidth := PaletteTransform(BW, Img, W, H, Palette, Params)
  else if Config.SubGreen then
  begin
    WriteTransformHeader(BW, TRANSFORM_SUB_GREEN);
    SubtractGreen(Img, W * H);
  end;

  if Config.Predictor then
  begin
    // Tiles not smaller than asked for and not too many of them
    Bits := Params.PredictorBits;
    while (Bits < MAX_TILE_BITS) and
          (SubSampleSize(CodedWidth, Bits) * SubSampleSize(H, Bits) > 16384) do
      Inc(Bits);
    PredictorTransform(BW, Img, CodedWidth, H, Bits, Params);
    // Multipliers of the color transform cost more than a predictor:
    // its tiles are twice the size
    if Config.CrossColor and Params.CrossColor then
    begin
      if Bits < MAX_TILE_BITS then
        Inc(Bits);
      CrossColorTransform(BW, Img, CodedWidth, H, Bits, Params);
    end;
  end;

  // No more transforms
  PutBits(BW, 0, 1);
  EncodeImage(BW, Img, CodedWidth, H, True, Params);
end;

{ Encodes ARGB image (Source is left as it is), returns the stream. }
function EncodeArgb(const Source: TUInt32Array; W, H: Integer; WithHeader, HasAlpha: Boolean;
  const Params: TEncoderParams): TPasWebPBytes;
var
  Palette: TPalette;
  Config: TEncoderConfig;
  Configs: array[0..3] of TEncoderConfig;
  NumConfigs, I, PaletteCount: Integer;
  Img: TUInt32Array;
  BW: TBitWriter;
  Size: NativeInt;
  HasPalette: Boolean;
begin
  Result := nil;
  HasPalette := BuildPalette(Pointer(Source), W * H, Palette);
  if HasPalette then PaletteCount := Palette.Count else PaletteCount := 0;
  ChooseTransforms(Pointer(Source), W, H, PaletteCount, Params.PredictorBits, Config);
  Configs[0] := Config;
  NumConfigs := 1;

  if Params.TryVariants then
  begin
    // The estimate can be wrong: other combinations are tried too
    if Config.Palette then
    begin
      if Palette.Count > 16 then
      begin
        Configs[NumConfigs] := Config;
        Configs[NumConfigs].Predictor := True;
        Inc(NumConfigs);
      end;
    end
    else
    begin
      Configs[NumConfigs] := Config;
      Configs[NumConfigs].Predictor := not Config.Predictor;
      Inc(NumConfigs);
      Configs[NumConfigs] := Config;
      Configs[NumConfigs].SubGreen := not Config.SubGreen;
      Inc(NumConfigs);
    end;
  end;

  for I := 0 to NumConfigs - 1 do
  begin
    Img := Copy(Source, 0, W * H);
    BitWriterInit(BW, W * H div 2 + 1024);

    if WithHeader then
    begin
      PutBits(BW, $2F, 8);
      PutBits(BW, W - 1, 14);
      PutBits(BW, H - 1, 14);
      PutBits(BW, Ord(HasAlpha), 1);
      PutBits(BW, 0, 3);  // version
    end;

    EncodeStream(BW, Pointer(Img), W, H, Configs[I], Palette, Params);
    Size := BitWriterFinish(BW);

    if (Result = nil) or (Size < Length(Result)) then
    begin
      SetLength(BW.Buf, Size);
      Result := BW.Buf;
    end;
  end;
end;

function PasWebPEncodeLossless(Pixels: PByte; Stride: NativeInt; Width, Height: Integer;
  WithAlpha: Boolean; Effort: Integer; out Data: TPasWebPBytes): Boolean;
const
  RiffHeaderSize = 12;
  ChunkHeaderSize = 8;
var
  Argb: TUInt32Array;
  Stream: TPasWebPBytes;
  Params: TEncoderParams;
  X, Y, Padding: Integer;
  Src: PByte;
  Dst: PUInt32;
  HasAlpha: Boolean;
  ChunkSize: UInt32;

  procedure PutTag(Offset: Integer; const Tag: AnsiString);
  begin
    Move(Tag[1], Data[Offset], 4);
  end;

  procedure PutUInt32(Offset: Integer; Value: UInt32);
  begin
    Data[Offset] := Byte(Value);
    Data[Offset + 1] := Byte(Value shr 8);
    Data[Offset + 2] := Byte(Value shr 16);
    Data[Offset + 3] := Byte(Value shr 24);
  end;

begin
  Result := False;
  Data := nil;
  if (Pixels = nil) or (Width < 1) or (Height < 1) or (Width > PasWebPMaxSize) or
     (Height > PasWebPMaxSize) then
    Exit;

  SetLength(Argb, Width * Height);
  HasAlpha := False;
  Dst := Pointer(Argb);

  for Y := 0 to Height - 1 do
  begin
    Src := Pixels + Y * Stride;

    if WithAlpha then
    begin
      Move(Src^, Dst^, Width * 4);
      if not HasAlpha then
        for X := 0 to Width - 1 do
          if Dst[X] < ARGB_BLACK then
          begin
            HasAlpha := True;
            Break;
          end;
    end
    else
      for X := 0 to Width - 1 do
      begin
        Dst[X] := ARGB_BLACK or (UInt32(Src[2]) shl 16) or (UInt32(Src[1]) shl 8) or Src[0];
        Inc(Src, 3);
      end;

    Inc(Dst, Width);
  end;

  GetParams(Effort, Params);
  Stream := EncodeArgb(Argb, Width, Height, True, HasAlpha, Params);

  // RIFF container with the single chunk, chunks have even sizes
  ChunkSize := Length(Stream);
  Padding := ChunkSize and 1;
  SetLength(Data, RiffHeaderSize + ChunkHeaderSize + Integer(ChunkSize) + Padding);
  PutTag(0, 'RIFF');
  PutUInt32(4, Length(Data) - 8);
  PutTag(8, 'WEBP');
  PutTag(12, 'VP8L');
  PutUInt32(16, ChunkSize);
  Move(Pointer(Stream)^, Data[RiffHeaderSize + ChunkHeaderSize], ChunkSize);
  if Padding <> 0 then
    Data[High(Data)] := 0;
  Result := True;
end;

function PasWebPEncodePlane(Plane: PByte; Stride: NativeInt; Width, Height: Integer;
  Effort: Integer; out Data: TPasWebPBytes): Boolean;
var
  Argb: TUInt32Array;
  Params: TEncoderParams;
  X, Y: Integer;
  Src: PByte;
begin
  Result := False;
  Data := nil;
  if (Plane = nil) or (Width < 1) or (Height < 1) or (Width > PasWebPMaxSize) or
     (Height > PasWebPMaxSize) then
    Exit;

  // Values go to green, where the decoder of alpha planes takes them from
  SetLength(Argb, Width * Height);
  for Y := 0 to Height - 1 do
  begin
    Src := Plane + Y * Stride;
    for X := 0 to Width - 1 do
      Argb[Y * Width + X] := UInt32(Src[X]) shl 8;
  end;

  GetParams(Effort, Params);
  Data := EncodeArgb(Argb, Width, Height, False, False, Params);
  Result := True;
end;

initialization
  InitTables;

end.
