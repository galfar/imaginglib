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

{ Tests of zlib wrapper (ImagingZLib unit): buffer functions and streaming
  compression and decompression, for the zlib backend selected at compile time.}
unit TestImagingZLib;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, ImagingTypes, ImagingUtility, ImagingZLib, ImagingTestCase;

type
  TTestZLib = class(TImagingTestCase)
  private
    FData: TDynByteArray;
    { Fills FData with Size bytes, compressible or noise.}
    procedure MakeData(Size: Integer; Noise: Boolean);
    function Compress(Level: Integer): TDynByteArray;
  published
    procedure TestBackend;
    procedure TestBufferRoundTrip;
    procedure TestStreamingDecompression;
    procedure TestStreamingCompression;
    procedure TestDataAfterStream;
    procedure TestDamagedData;
    procedure TestTruncatedData;
    procedure TestStreamsRoundTrip;
    procedure TestDecompressionSeek;
    procedure TestStreamErrors;
  end;

implementation

{ TTestZLib }

procedure TTestZLib.MakeData(Size: Integer; Noise: Boolean);
var
  I: Integer;
  State: UInt32;
begin
  SetLength(FData, Size);
  State := 12345;
  for I := 0 to Size - 1 do
  begin
    if Noise then
    begin
      State := UInt32((Int64(State) * 1103515245 + 12345) and $FFFFFFFF);
      FData[I] := Byte(State shr 16);
    end
    else
      FData[I] := Byte(I div 100 + I mod 7);
  end;
end;

function TTestZLib.Compress(Level: Integer): TDynByteArray;
var
  Buffer: Pointer;
  Size: NativeInt;
begin
  Result := nil;
  CompressBuf(Pointer(FData), Length(FData), Buffer, Size, Level);
  try
    SetLength(Result, Size);
    Move(Buffer^, Pointer(Result)^, Size);
  finally
    FreeMem(Buffer);
  end;
end;

procedure TTestZLib.TestBackend;
var
  Expected: TZLibBackend;
  Version: string;
begin
  // Selection rules of ImagingZLib
{$IF Defined(ZLIB_USE_BUNDLED)}
  Expected := zbBundled;
{$ELSEIF Defined(ZLIB_USE_DYNLIB) and Defined(HAS_DYNLIBS)}
  Expected := zbDynLib;
{$ELSEIF Defined(FPC)}
  Expected := zbFpc;
{$ELSEIF Defined(DCC) and (CompilerVersion > 23)}
  Expected := zbDelphi;
{$ELSE}
  Expected := zbBundled;
{$IFEND}
  CheckEquals(Ord(Expected), Ord(ZLibBackend), 'Backend');

  Version := GetZLibVersion;
  CheckTrue(Copy(Version, 1, 2) = '1.', 'Version 1.x: ' + Version);
end;

procedure TTestZLib.TestBufferRoundTrip;
const
  Sizes: array[0..4] of Integer = (0, 1, 1000, 70000, 300000);
var
  S, Level: Integer;
  Noise: Boolean;
  Compressed: TDynByteArray;
  Output: Pointer;
  OutSize: NativeInt;
  Msg: string;
begin
  for Noise := False to True do
    for S := Low(Sizes) to High(Sizes) do
      for Level := 0 to 9 do
      begin
        if (Level mod 3 <> 0) and (Sizes[S] > 1000) then
          Continue;
        if Noise then
          Msg := Format('%d bytes of noise, level %d', [Sizes[S], Level])
        else
          Msg := Format('%d bytes, level %d', [Sizes[S], Level]);
        MakeData(Sizes[S], Noise);
        Compressed := Compress(Level);
        if not Noise and (Sizes[S] > 1000) and (Level > 0) then
          CheckTrue(Length(Compressed) < Sizes[S] div 2, Msg + ': data are compressed');

        // Without and with the size estimate
        DecompressBuf(Pointer(Compressed), Length(Compressed), 0, Output, OutSize);
        try
          CheckEquals(Sizes[S], OutSize, Msg + ': decompressed size');
          CheckSameMem(Pointer(FData), Output, OutSize, Msg + ': decompressed data');
        finally
          FreeMem(Output);
        end;
        DecompressBuf(Pointer(Compressed), Length(Compressed), Sizes[S], Output, OutSize);
        try
          CheckEquals(Sizes[S], OutSize, Msg + ': decompressed size with estimate');
          CheckSameMem(Pointer(FData), Output, OutSize, Msg + ': decompressed data with estimate');
        finally
          FreeMem(Output);
        end;
      end;
end;

procedure TTestZLib.TestStreamingDecompression;
const
  // Input and output pieces: one byte, odd sizes, whole data
  Pieces: array[0..3] of Integer = (1, 7, 4093, MaxInt);
var
  Compressed, Output: TDynByteArray;
  Inflater: TZInflater;
  InPiece, OutPiece, InPos, OutPos, InLen, OutLen: Integer;
  Consumed, Produced: NativeInt;
  More: Boolean;
  Msg: string;
begin
  MakeData(50000, False);
  Compressed := Compress(6);
  SetLength(Output, Length(FData));

  for InPiece := Low(Pieces) to High(Pieces) do
    for OutPiece := Low(Pieces) to High(Pieces) do
    begin
      Msg := Format('Input by %d, output by %d', [Pieces[InPiece], Pieces[OutPiece]]);
      FillChar(Pointer(Output)^, Length(Output), 0);
      InPos := 0;
      OutPos := 0;
      ZInflateBegin(Inflater);
      try
        repeat
          InLen := Length(Compressed) - InPos;
          if InLen > Pieces[InPiece] then
            InLen := Pieces[InPiece];
          OutLen := Length(Output) - OutPos;
          if OutLen > Pieces[OutPiece] then
            OutLen := Pieces[OutPiece];
          More := ZInflateData(Inflater, @PBuffer(Pointer(Compressed))[InPos], InLen,
                               @PBuffer(Pointer(Output))[OutPos], OutLen, Consumed, Produced);
          Inc(InPos, Consumed);
          Inc(OutPos, Produced);
          if More and (Consumed = 0) and (Produced = 0) and (InPos = Length(Compressed)) then
            Fail(Msg + ': no progress');
        until not More;
      finally
        ZInflateEnd(Inflater);
      end;

      CheckEquals(Length(Compressed), InPos, Msg + ': all input is used');
      CheckEquals(Length(FData), OutPos, Msg + ': size of output');
      CheckSameMem(Pointer(FData), Pointer(Output), Length(FData), Msg + ': output');
    end;
end;

procedure TTestZLib.TestStreamingCompression;
const
  Pieces: array[0..3] of Integer = (1, 7, 4093, MaxInt);
var
  Compressed: TDynByteArray;
  Deflater: TZDeflater;
  InPiece, OutPiece, InPos, InLen, CompLen: Integer;
  Consumed, Produced: NativeInt;
  Decompressed: Pointer;
  DecompressedSize: NativeInt;
  Msg: string;

  procedure Put(Data: Pointer; Size: Integer; Finish: Boolean);
  var
    Done, Room: Integer;
    More: Boolean;
  begin
    // Output is passed in pieces too, buffer grows as needed
    Done := 0;
    repeat
      if Length(Compressed) - CompLen < 65536 then
        SetLength(Compressed, Length(Compressed) + 65536);
      Room := Length(Compressed) - CompLen;
      if Room > Pieces[OutPiece] then
        Room := Pieces[OutPiece];

      More := ZDeflateData(Deflater, @PBuffer(Data)[Done], Size - Done,
                           @PBuffer(Pointer(Compressed))[CompLen], Room, Finish, Consumed, Produced);
      Inc(Done, Consumed);
      Inc(CompLen, Produced);
      if More and (Consumed = 0) and (Produced = 0) and (Finish or (Done < Size)) then
        Fail(Msg + ': no progress');
    until (Finish and not More) or (not Finish and (Done = Size) and (Produced < Room));
  end;

begin
  MakeData(50000, False);

  for InPiece := Low(Pieces) to High(Pieces) do
    for OutPiece := Low(Pieces) to High(Pieces) do
    begin
      Msg := Format('Input by %d, output by %d', [Pieces[InPiece], Pieces[OutPiece]]);
      SetLength(Compressed, 0);
      CompLen := 0;
      InPos := 0;
      // Strategy 0 is Z_DEFAULT_STRATEGY
      ZDeflateBegin(Deflater, 6, 0);
      try
        while InPos < Length(FData) do
        begin
          InLen := Length(FData) - InPos;
          if InLen > Pieces[InPiece] then
            InLen := Pieces[InPiece];
          Put(@FData[InPos], InLen, False);
          Inc(InPos, InLen);
        end;
        Put(nil, 0, True);
      finally
        ZDeflateEnd(Deflater);
      end;

      CheckTrue(CompLen < Length(FData) div 2, Format('%s: compressed size %d', [Msg, CompLen]));
      DecompressBuf(Pointer(Compressed), CompLen, 0, Decompressed, DecompressedSize);
      try
        CheckEquals(Length(FData), DecompressedSize, Msg + ': decompressed size');
        CheckSameMem(Pointer(FData), Decompressed, DecompressedSize, Msg + ': decompressed data');
      finally
        FreeMem(Decompressed);
      end;
    end;
end;

procedure TTestZLib.TestDataAfterStream;
var
  Compressed, Output: TDynByteArray;
  Inflater: TZInflater;
  Consumed, Produced: NativeInt;
  StreamSize: Integer;
begin
  // Decompression stops at the end of zlib stream, data after it are not read
  MakeData(1000, False);
  Compressed := Compress(6);
  StreamSize := Length(Compressed);
  SetLength(Compressed, StreamSize + 10);
  FillChar(Compressed[StreamSize], 10, $AB);
  SetLength(Output, 2000);

  ZInflateBegin(Inflater);
  try
    CheckFalse(ZInflateData(Inflater, Pointer(Compressed), Length(Compressed), Pointer(Output),
                            Length(Output), Consumed, Produced), 'End of stream reached');
    CheckEquals(StreamSize, Consumed, 'Consumed bytes');
    CheckEquals(1000, Produced, 'Produced bytes');
    CheckSameMem(Pointer(FData), Pointer(Output), 1000, 'Output');
    // Further calls do nothing
    CheckFalse(ZInflateData(Inflater, @Compressed[StreamSize], 10, Pointer(Output),
                            Length(Output), Consumed, Produced), 'After end of stream');
    CheckEquals(0, Consumed, 'Consumed after end of stream');
    CheckEquals(0, Produced, 'Produced after end of stream');
  finally
    ZInflateEnd(Inflater);
  end;
end;

procedure TTestZLib.TestDamagedData;
var
  Compressed, Output: TDynByteArray;
  Inflater: TZInflater;
  Consumed, Produced: NativeInt;
  Raised: Boolean;
begin
  // Wrong header and wrong checksum at the end are errors
  MakeData(1000, False);
  Compressed := Compress(6);
  SetLength(Output, 2000);

  Compressed[0] := Compressed[0] xor $FF;
  Raised := False;
  ZInflateBegin(Inflater);
  try
    try
      ZInflateData(Inflater, Pointer(Compressed), Length(Compressed), Pointer(Output),
                   Length(Output), Consumed, Produced);
    except
      on EDecompressionError do
        Raised := True;
    end;
  finally
    ZInflateEnd(Inflater);
  end;
  CheckTrue(Raised, 'Damaged header');

  Compressed := Compress(6);
  Compressed[High(Compressed)] := Compressed[High(Compressed)] xor $FF;
  Raised := False;
  ZInflateBegin(Inflater);
  try
    try
      ZInflateData(Inflater, Pointer(Compressed), Length(Compressed), Pointer(Output),
                   Length(Output), Consumed, Produced);
    except
      on EDecompressionError do
        Raised := True;
    end;
  finally
    ZInflateEnd(Inflater);
  end;
  CheckTrue(Raised, 'Damaged checksum');
end;

procedure TTestZLib.TestTruncatedData;
var
  Compressed, Output: TDynByteArray;
  Inflater: TZInflater;
  Consumed, Produced: NativeInt;
begin
  // Without the rest of the stream decompression wants more input
  MakeData(1000, True);
  Compressed := Compress(6);
  SetLength(Output, 2000);

  ZInflateBegin(Inflater);
  try
    CheckTrue(ZInflateData(Inflater, Pointer(Compressed), Length(Compressed) div 2, Pointer(Output),
                           Length(Output), Consumed, Produced), 'Stream is not finished');
    CheckEquals(Length(Compressed) div 2, Consumed, 'All given input is used');
    CheckTrue(Produced < 1000, Format('Part of data is decompressed: %d', [Produced]));
    CheckSameMem(Pointer(FData), Pointer(Output), Produced, 'Decompressed part');
  finally
    ZInflateEnd(Inflater);
  end;
end;

procedure TTestZLib.TestStreamsRoundTrip;
const
  // Write and read sizes: one byte, odd sizes, whole data
  Pieces: array[0..3] of Integer = (1, 7, 4093, MaxInt);
  Prefix = 10;
  Trailer = 20;
var
  Noise: Boolean;
  P, Pos, Len: Integer;
  Dest: TMemoryStream;
  Compression: TCompressionStream;
  Decompression: TDecompressionStream;
  Output: TDynByteArray;
  Msg: string;
begin
  // Compressed data are written after some other data in the stream and
  // followed by other data, decompression must start and stop at the right place
  for Noise := False to True do
    for P := Low(Pieces) to High(Pieces) do
    begin
      Msg := Format('Pieces of %d, noise %d', [Pieces[P], Ord(Noise)]);
      MakeData(50000, Noise);
      Dest := TMemoryStream.Create;
      try
        Dest.Size := Prefix;
        Dest.Position := Prefix;

        Compression := TCompressionStream.Create(clDefault, Dest);
        try
          Pos := 0;
          while Pos < Length(FData) do
          begin
            Len := Length(FData) - Pos;
            if Len > Pieces[P] then
              Len := Pieces[P];
            Compression.WriteBuffer(FData[Pos], Len);
            Inc(Pos, Len);
            CheckEquals(Pos, Compression.Position, Msg + ': position of compression stream');
          end;
          if not Noise then
            CheckTrue(Compression.CompressionRate > 50, Format('%s: compression rate %.1f', [Msg, Compression.CompressionRate]));
        finally
          Compression.Free;
        end;
        CheckTrue(Dest.Size > Prefix, Msg + ': compressed data are written');
        Dest.Seek(0, soEnd);
        SetLength(Output, Trailer);
        FillChar(Pointer(Output)^, Trailer, $AB);
        Dest.WriteBuffer(Pointer(Output)^, Trailer);

        Dest.Position := Prefix;
        SetLength(Output, Length(FData) + 100);
        Decompression := TDecompressionStream.Create(Dest);
        try
          Pos := 0;
          repeat
            Len := Length(Output) - Pos;
            if Len > Pieces[P] then
              Len := Pieces[P];
            Len := Decompression.Read(Output[Pos], Len);
            Inc(Pos, Len);
          until Len = 0;
          CheckEquals(Length(FData), Pos, Msg + ': decompressed size');
          CheckEquals(Length(FData), Decompression.Position, Msg + ': position of decompression stream');
          CheckSameMem(Pointer(FData), Pointer(Output), Length(FData), Msg + ': decompressed data');
        finally
          Decompression.Free;
        end;
      finally
        Dest.Free;
      end;
    end;
end;

procedure TTestZLib.TestDecompressionSeek;
var
  Source: TMemoryStream;
  Compression: TCompressionStream;
  Decompression: TDecompressionStream;
  Output: array[0..99] of Byte;
  Raised: Boolean;
begin
  MakeData(50000, False);
  Source := TMemoryStream.Create;
  try
    Source.Size := 5;
    Source.Position := 5;
    Compression := TCompressionStream.Create(clDefault, Source);
    try
      Compression.WriteBuffer(FData[0], Length(FData));
    finally
      Compression.Free;
    end;

    Source.Position := 5;
    Decompression := TDecompressionStream.Create(Source);
    try
      // Forward seeks decompress and skip data
      Decompression.Seek(1000, soCurrent);
      CheckEquals(1000, Decompression.Position, 'Position after seek from current');
      Decompression.ReadBuffer(Output, 100);
      CheckSameMem(@FData[1000], @Output, 100, 'Data after seek from current');
      Decompression.Position := 30000;
      CheckEquals(30000, Decompression.Position, 'Position after seek from beginning');
      Decompression.ReadBuffer(Output, 100);
      CheckSameMem(@FData[30000], @Output, 100, 'Data after seek from beginning');

      // Position 0 starts again from the beginning of compressed data
      Decompression.Position := 0;
      CheckEquals(0, Decompression.Position, 'Position after reset');
      Decompression.ReadBuffer(Output, 100);
      CheckSameMem(@FData[0], @Output, 100, 'Data after reset');

      // Backward seek is not possible
      Raised := False;
      try
        Decompression.Position := 50;
      except
        on EDecompressionError do
          Raised := True;
      end;
      CheckTrue(Raised, 'Backward seek is refused');
    finally
      Decompression.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TTestZLib.TestStreamErrors;
var
  Source: TMemoryStream;
  Compression: TCompressionStream;
  Decompression: TDecompressionStream;
  Output: array[0..99] of Byte;
  Raised: Boolean;
begin
  Source := TMemoryStream.Create;
  try
    Compression := TCompressionStream.Create(clDefault, Source);
    try
      Raised := False;
      try
        Compression.Read(Output, 10);
      except
        on ECompressionError do
          Raised := True;
      end;
      CheckTrue(Raised, 'Reading from compression stream');
    finally
      Compression.Free;
    end;

    // Damaged zlib header
    Source.Clear;
    FillChar(Output, SizeOf(Output), $FF);
    Source.WriteBuffer(Output, SizeOf(Output));
    Source.Position := 0;
    Decompression := TDecompressionStream.Create(Source);
    try
      Raised := False;
      try
        Decompression.Read(Output, 10);
      except
        on EDecompressionError do
          Raised := True;
      end;
      CheckTrue(Raised, 'Damaged data');
      Raised := False;
      try
        Decompression.Write(Output, 10);
      except
        on EDecompressionError do
          Raised := True;
      end;
      CheckTrue(Raised, 'Writing to decompression stream');
    finally
      Decompression.Free;
    end;
  finally
    Source.Free;
  end;
end;

initialization
  RegisterImagingTest(TTestZLib);

end.
