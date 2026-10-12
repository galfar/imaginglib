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

{ Tests of helper functions in ImagingUtility unit.}
unit TestImagingUtility;

{$I ImagingOptions.inc}

interface

uses
  Types, SysUtils, Classes, ImagingTypes, ImagingUtility, ImagingTestCase;

type
  TTestUtilityMath = class(TImagingTestCase)
  published
    procedure TestClamp;
    procedure TestPowersOfTwo;
    procedure TestMulDiv;
    procedure TestScaleValue;
    procedure TestIff;
    procedure TestMipMapLevels;
    procedure TestDpiConversion;
  end;

  TTestUtilityMemory = class(TImagingTestCase)
  published
    procedure TestSwapEndianValues;
    procedure TestSwapEndianBuffers;
    procedure TestIsLittleEndian;
    procedure TestCrc32;
    procedure TestFillMemory;
  end;

  TTestUtilityStrings = class(TImagingTestCase)
  published
    procedure TestStrMaskMatch;
    procedure TestStrTokens;
    procedure TestTrimSet;
    procedure TestPos;
    procedure TestFileNameParts;
  end;

  TTestUtilityRects = class(TImagingTestCase)
  published
    procedure TestBoundsAndRects;
    procedure TestRectRelations;
    procedure TestClipRectBounds;
    procedure TestClipCopyBounds;
    procedure TestClipStretchBounds;
    procedure TestScaleToFit;
  end;

implementation

{ TTestUtilityMath }

procedure TTestUtilityMath.TestClamp;
begin
  CheckEquals(5, ClampInt(5, 0, 10), 'ClampInt inside');
  CheckEquals(0, ClampInt(-5, 0, 10), 'ClampInt below');
  CheckEquals(10, ClampInt(15, 0, 10), 'ClampInt above');
  CheckEquals(-3, ClampInt(-3, -3, -3), 'ClampInt single value range');

  CheckEquals(0, ClampToByte(-1), 'ClampToByte below');
  CheckEquals(0, ClampToByte(0), 'ClampToByte min');
  CheckEquals(255, ClampToByte(255), 'ClampToByte max');
  CheckEquals(255, ClampToByte(256), 'ClampToByte above');
  CheckEquals(255, ClampToByte(MaxInt), 'ClampToByte MaxInt');

  CheckEquals(0, ClampToWord(-70000), 'ClampToWord below');
  CheckEquals(65535, ClampToWord(65535), 'ClampToWord max');
  CheckEquals(65535, ClampToWord(65536), 'ClampToWord above');

  CheckEquals(0.5, ClampFloat(0.5, 0.0, 1.0), 0.0, 'ClampFloat inside');
  CheckEquals(0.0, ClampFloat(-0.5, 0.0, 1.0), 0.0, 'ClampFloat below');
  CheckEquals(1.0, ClampFloat(1.5, 0.0, 1.0), 0.0, 'ClampFloat above');
end;

procedure TTestUtilityMath.TestPowersOfTwo;
var
  I: Integer;
begin
  for I := 0 to 30 do
  begin
    CheckEquals(1 shl I, Pow2Int(I), 'Pow2Int ' + IntToStr(I));
    CheckEquals(I, Log2Int(1 shl I), 'Log2Int ' + IntToStr(1 shl I));
    CheckTrue(IsPow2(1 shl I), 'IsPow2 ' + IntToStr(1 shl I));
    CheckEquals(1 shl I, NextPow2(1 shl I), 'NextPow2 of power of two ' + IntToStr(1 shl I));
  end;

  CheckFalse(IsPow2(3), 'IsPow2 3');
  CheckFalse(IsPow2(6), 'IsPow2 6');
  CheckFalse(IsPow2(1000), 'IsPow2 1000');
  CheckFalse(IsPow2(MaxInt), 'IsPow2 MaxInt');

  CheckEquals(-1, Log2Int(0), 'Log2Int 0');
  CheckEquals(-1, Log2Int(3), 'Log2Int 3');
  CheckEquals(-1, Log2Int(1000), 'Log2Int 1000');

  CheckEquals(4, NextPow2(3), 'NextPow2 3');
  CheckEquals(8, NextPow2(5), 'NextPow2 5');
  CheckEquals(8, NextPow2(6), 'NextPow2 6');
  CheckEquals(1024, NextPow2(1000), 'NextPow2 1000');
  CheckEquals(1 shl 30, NextPow2(1 shl 29 + 1), 'NextPow2 2^29 + 1');
end;

procedure TTestUtilityMath.TestMulDiv;
begin
  CheckEquals(0, MulDiv(0, 255, 255), 'MulDiv zero');
  CheckEquals(128, MulDiv(128, 255, 255), 'MulDiv identity');
  CheckEquals(65535, MulDiv(255, 65535, 255), 'MulDiv 8 to 16 bits');
  CheckEquals(255, MulDiv(65535, 255, 65535), 'MulDiv 16 to 8 bits');
  // Result is truncated, not rounded
  CheckEquals(127, MulDiv(1, 255, 2), 'MulDiv truncation');
  CheckEquals(127, MulDiv(32767, 255, 65535), 'MulDiv 16 to 8 bits truncation');
end;

procedure TTestUtilityMath.TestScaleValue;
var
  I: Integer;
begin
  CheckEquals(Int64(0), Int64(ScaleValue(0, 255, 65535)), 'Zero stays zero');
  CheckEquals(Int64(65535), Int64(ScaleValue(255, 255, 65535)), 'Largest 8-bit value to 16 bits');
  CheckEquals(Int64(255), Int64(ScaleValue(65535, 65535, 255)), 'Largest 16-bit value to 8 bits');
  CheckEquals(Int64(77), Int64(ScaleValue(77, 255, 255)), 'The same range');
  CheckEquals(Int64(255), Int64(ScaleValue(300, 255, 255)), 'Value over the source range');

  // 8 to 16 bits is exact multiplication by 257, and it comes back
  for I := 0 to 255 do
  begin
    CheckEquals(Int64(I * 257), Int64(ScaleValue(I, 255, 65535)), '8 to 16 bits: ' + IntToStr(I));
    CheckEquals(Int64(I), Int64(ScaleValue(I * 257, 65535, 255)), '8 to 16 bits and back: ' + IntToStr(I));
  end;

  // 16 to 8 bits rounds to the nearest value
  for I := 0 to 65535 do
  begin
    if Int64(ScaleValue(I, 65535, 255)) <> Round(I / 257) then
      Fail(Format('16 to 8 bits: %d gives %d', [I, ScaleValue(I, 65535, 255)]));
  end;
  CheckEquals(Int64(0), Int64(ScaleValue(128, 65535, 255)), '16 to 8 bits, just below half');
  CheckEquals(Int64(1), Int64(ScaleValue(129, 65535, 255)), '16 to 8 bits, just above half');

  // Unusual sizes of samples
  CheckEquals(Int64(255), Int64(ScaleValue(31, 31, 255)), 'Largest 5-bit value to 8 bits');
  CheckEquals(Int64(132), Int64(ScaleValue(16, 31, 255)), '5 to 8 bits is rounded (131.6)');
  CheckEquals(Int64(65535), Int64(ScaleValue(4095, 4095, 65535)), 'Largest 12-bit value to 16 bits');
  CheckEquals(Int64(32776), Int64(ScaleValue(2048, 4095, 65535)), '12 to 16 bits (32775.99)');
  CheckEquals(Int64(128), Int64(ScaleValue(2048, 4095, 255)), '12 to 8 bits (127.5 and a bit)');
  CheckEquals(Int64(1), Int64(ScaleValue(1, 1, 1)), 'One bit');
  CheckEquals(Int64(255), Int64(ScaleValue(1, 1, 255)), 'One bit to 8 bits');

  // Whole 32-bit range
  CheckEquals(Int64($FFFFFFFF), Int64(ScaleValue(65535, 65535, $FFFFFFFF)), 'Largest 16-bit value to 32 bits');
  CheckEquals(Int64($00010001), Int64(ScaleValue(1, 65535, $FFFFFFFF)), '16 to 32 bits repeats the bits');
  CheckEquals(Int64($12341234), Int64(ScaleValue($1234, 65535, $FFFFFFFF)), '16 to 32 bits repeats the bits');
  CheckEquals(Int64(65535), Int64(ScaleValue($FFFFFFFF, $FFFFFFFF, 65535)), 'Largest 32-bit value to 16 bits');
  CheckEquals(Int64(128), Int64(ScaleValue($80000000, $FFFFFFFF, 255)), '32 to 8 bits');
  CheckEquals(Int64($12341234), Int64(ScaleValue($12341234, $FFFFFFFF, $FFFFFFFF)), 'The same 32-bit range');
  CheckEquals(Int64($FFFFFFFF), Int64(ScaleValue($FFFFFFFF, $FFFFFFFF, $FFFFFFFF)), 'Largest 32-bit value');
  CheckEquals(Int64(0), Int64(ScaleValue(0, $FFFFFFFF, $FFFFFFFF)), 'Zero in 32-bit range');
end;

procedure TTestUtilityMath.TestIff;
begin
  CheckEquals(1, Iff(True, 1, 2), 'Iff Integer true');
  CheckEquals(2, Iff(False, 1, 2), 'Iff Integer false');
  CheckEquals('yes', Iff(True, 'yes', 'no'), 'Iff string true');
  CheckEquals('no', Iff(False, 'yes', 'no'), 'Iff string false');
  CheckEquals(1.5, IffFloat(True, 1.5, 2.5), 0.0, 'IffFloat true');
  CheckEquals(2.5, IffFloat(False, 1.5, 2.5), 0.0, 'IffFloat false');
end;

procedure TTestUtilityMath.TestMipMapLevels;
begin
  CheckEquals(0, GetNumMipMapLevels(0, 5), 'Invalid width');
  CheckEquals(0, GetNumMipMapLevels(5, -1), 'Invalid height');
  CheckEquals(1, GetNumMipMapLevels(1, 1), '1x1');
  CheckEquals(9, GetNumMipMapLevels(256, 256), '256x256');
  CheckEquals(9, GetNumMipMapLevels(256, 16), '256x16');
  CheckEquals(9, GetNumMipMapLevels(1, 256), '1x256');
  CheckEquals(3, GetNumMipMapLevels(5, 3), '5x3');
  CheckEquals(10, GetNumMipMapLevels(1000, 600), '1000x600');

  CheckEquals(4, GetVolumeLevelCount(4, 1), 'Volume without mipmaps');
  CheckEquals(15, GetVolumeLevelCount(8, 4), 'Volume 8 slices, 4 mipmaps');
  // Depth of mipmap level does not go below one slice
  CheckEquals(9, GetVolumeLevelCount(4, 5), 'Volume 4 slices, 5 mipmaps');
end;

procedure TTestUtilityMath.TestDpiConversion;
begin
  CheckEquals(254.0, PixelSizeToDpi(100.0), 0.001, 'PixelSizeToDpi 100 um');
  CheckEquals(100.0, DpiToPixelSize(254.0), 0.001, 'DpiToPixelSize 254 DPI');
  CheckEquals(300.0, PixelSizeToDpi(DpiToPixelSize(300.0)), 0.001, 'Round trip 300 DPI');
  CheckEquals(72.0, PixelSizeToDpi(DpiToPixelSize(72.0)), 0.001, 'Round trip 72 DPI');
end;

{ TTestUtilityMemory }

procedure TTestUtilityMemory.TestSwapEndianValues;
begin
  CheckEquals($3412, SwapEndianWord($1234), 'SwapEndianWord');
  CheckEquals($00FF, SwapEndianWord($FF00), 'SwapEndianWord $FF00');
  CheckEquals(Int64($78563412), Int64(SwapEndianUInt32($12345678)), 'SwapEndianUInt32');
  CheckEquals(Int64($000000FF), Int64(SwapEndianUInt32($FF000000)), 'SwapEndianUInt32 $FF000000');
  CheckEquals(Int64($FFFFFFFF), Int64(SwapEndianUInt32($FFFFFFFF)), 'SwapEndianUInt32 $FFFFFFFF');
end;

procedure TTestUtilityMemory.TestSwapEndianBuffers;
var
  Words: array[0..2] of Word;
  DWords: array[0..2] of UInt32;
begin
  Words[0] := $1234;
  Words[1] := $00FF;
  Words[2] := $ABCD;
  SwapEndianWord(@Words[0], 2);
  CheckEquals($3412, Words[0], 'Word 0');
  CheckEquals($FF00, Words[1], 'Word 1');
  CheckEquals($ABCD, Words[2], 'Word behind the buffer must stay');

  DWords[0] := $12345678;
  DWords[1] := $000000FF;
  DWords[2] := $ABCDEF01;
  SwapEndianUInt32(@DWords[0], 2);
  CheckEquals(Int64($78563412), Int64(DWords[0]), 'UInt32 0');
  CheckEquals(Int64($FF000000), Int64(DWords[1]), 'UInt32 1');
  CheckEquals(Int64($ABCDEF01), Int64(DWords[2]), 'UInt32 behind the buffer must stay');
end;

procedure TTestUtilityMemory.TestIsLittleEndian;
begin
  // Only little endian targets are supported
  CheckTrue(IsLittleEndian, 'IsLittleEndian');
end;

procedure TTestUtilityMemory.TestCrc32;
const
  // Standard check value of CRC-32 (ISO-HDLC) as used by zlib and PNG
  CheckData: AnsiString = '123456789';
  CheckCrc = $CBF43926;
var
  Crc: UInt32;
begin
  Crc := $FFFFFFFF;
  CalcCrc32(Crc, PAnsiChar(CheckData), Length(CheckData));
  CheckEquals(Int64(CheckCrc), Int64(Crc xor $FFFFFFFF), 'CRC of check string');

  // Checksum computed in more steps must be the same
  Crc := $FFFFFFFF;
  CalcCrc32(Crc, PAnsiChar(CheckData), 4);
  CalcCrc32(Crc, @PAnsiChar(CheckData)[4], 5);
  CheckEquals(Int64(CheckCrc), Int64(Crc xor $FFFFFFFF), 'CRC computed in two steps');

  Crc := $FFFFFFFF;
  CalcCrc32(Crc, nil, 0);
  CheckEquals(Int64(0), Int64(Crc xor $FFFFFFFF), 'CRC of no data');
end;

procedure TTestUtilityMemory.TestFillMemory;
const
  Guard = $5A;
  MaxSize = 17;
var
  Buffer: array[0..MaxSize + 7] of Byte;
  ByteValue: Byte;
  WordValue: Word;
  UInt32Value: UInt32;
  Size: Integer;

  // Buffer must be filled with repeated Value. Size in bytes does not have to be
  // a multiple of the value size: the rest is filled with the start of the value
  // (its first two bytes when there's space for them, then the first byte).
  procedure CheckBuffer(Value: PByteArray; ValueSize: Integer; const Msg: string);
  var
    I, FullSize, Rest, Expected: Integer;
  begin
    FullSize := Size div ValueSize * ValueSize;
    Rest := Size - FullSize;

    for I := 0 to High(Buffer) do
    begin
      if I >= Size then
        Expected := Guard
      else if I < FullSize then
        Expected := Value[I mod ValueSize]
      else if Odd(Rest) and (I = Size - 1) then
        Expected := Value[0]
      else
        Expected := Value[I - FullSize];

      CheckEquals(Expected, Buffer[I], Format('%s of %d bytes: byte at %d', [Msg, Size, I]));
    end;
  end;

begin
  ByteValue := $A1;
  WordValue := $B1B2;
  UInt32Value := $C1C2C3C4;

  for Size := 0 to MaxSize do
  begin
    FillChar(Buffer, SizeOf(Buffer), Guard);
    FillMemoryByte(@Buffer, Size, ByteValue);
    CheckBuffer(@ByteValue, SizeOf(ByteValue), 'FillMemoryByte');

    FillChar(Buffer, SizeOf(Buffer), Guard);
    FillMemoryWord(@Buffer, Size, WordValue);
    CheckBuffer(@WordValue, SizeOf(WordValue), 'FillMemoryWord');

    FillChar(Buffer, SizeOf(Buffer), Guard);
    FillMemoryUInt32(@Buffer, Size, UInt32Value);
    CheckBuffer(@UInt32Value, SizeOf(UInt32Value), 'FillMemoryUInt32');
  end;
end;

{ TTestUtilityStrings }

procedure TTestUtilityStrings.TestStrMaskMatch;
begin
  CheckTrue(StrMaskMatch('image.png', '*.png'), 'Star and extension');
  CheckTrue(StrMaskMatch('image.png', '*'), 'Star only');
  CheckTrue(StrMaskMatch('image.png', 'image.*'), 'Star at the end');
  CheckTrue(StrMaskMatch('image.png', 'i*e.p*g'), 'More stars');
  CheckTrue(StrMaskMatch('image.png', '*mag*'), 'Stars around');
  CheckTrue(StrMaskMatch('image.png', 'image.png*'), 'Star matching nothing');
  CheckTrue(StrMaskMatch('image.png', 'im?ge.pn?'), 'Question marks');
  CheckTrue(StrMaskMatch('image.png', 'image.png'), 'No wildcards');
  CheckTrue(StrMaskMatch('image.png', ''), 'Empty mask matches everything');
  CheckTrue(StrMaskMatch('', '*'), 'Empty subject and star');

  CheckFalse(StrMaskMatch('image.png', '*.jpg'), 'Different extension');
  CheckFalse(StrMaskMatch('image.png', 'image.pn'), 'Subject longer than mask');
  CheckFalse(StrMaskMatch('image.pn', 'image.png'), 'Mask longer than subject');
  CheckFalse(StrMaskMatch('image.png', 'im?ge'), 'Question mark and longer subject');
  CheckFalse(StrMaskMatch('imge.png', 'im?ge.png'), 'Question mark inside needs a character');
  // Question marks at the end of mask match also no characters (as in DOS masks)
  CheckTrue(StrMaskMatch('image.png', 'image.png?'), 'Question mark at the end');
  CheckTrue(StrMaskMatch('image.p', 'image.p??'), 'More question marks at the end');

  CheckTrue(StrMaskMatch('IMAGE.PNG', '*.png'), 'Case insensitive by default');
  CheckTrue(StrMaskMatch('IMAGE.PNG', '*.png', False), 'Case insensitive');
  CheckFalse(StrMaskMatch('IMAGE.PNG', '*.png', True), 'Case sensitive, different case');
  CheckTrue(StrMaskMatch('IMAGE.PNG', '*.PNG', True), 'Case sensitive, same case');
end;

procedure TTestUtilityStrings.TestStrTokens;
var
  S: string;
  List: TStringList;
begin
  S := 'a,bc,def';
  CheckEquals('a', StrToken(S, ','), 'StrToken first');
  CheckEquals('bc,def', S, 'StrToken rest after first');
  CheckEquals('bc', StrToken(S, ','), 'StrToken second');
  CheckEquals('def', StrToken(S, ','), 'StrToken last');
  CheckEquals('', S, 'StrToken rest after last');
  CheckEquals('', StrToken(S, ','), 'StrToken on empty string');

  S := 'a,bc,def';
  CheckEquals('def', StrTokenEnd(S, ','), 'StrTokenEnd first');
  CheckEquals('a,bc', S, 'StrTokenEnd rest after first');
  CheckEquals('bc', StrTokenEnd(S, ','), 'StrTokenEnd second');
  CheckEquals('a', StrTokenEnd(S, ','), 'StrTokenEnd last');
  CheckEquals('', S, 'StrTokenEnd rest after last');

  List := TStringList.Create;
  try
    StrTokensToList('a,bc,,def', ',', List);
    CheckEquals(4, List.Count, 'StrTokensToList count');
    CheckEquals('a', List[0], 'StrTokensToList item 0');
    CheckEquals('bc', List[1], 'StrTokensToList item 1');
    CheckEquals('', List[2], 'StrTokensToList empty item');
    CheckEquals('def', List[3], 'StrTokensToList item 3');

    StrTokensToList('', ',', List);
    CheckEquals(0, List.Count, 'StrTokensToList clears the list');
  finally
    List.Free;
  end;
end;

procedure TTestUtilityStrings.TestTrimSet;
begin
  CheckEquals('text', TrimSet('"text"', ['"']), 'TrimSet both sides');
  CheckEquals('te xt', TrimSet(' -te xt- ', [' ', '-']), 'TrimSet more chars, keeps inner ones');
  CheckEquals('text', TrimSet('text', ['"']), 'TrimSet nothing to trim');
  CheckEquals('', TrimSet('""""', ['"']), 'TrimSet everything trimmed');
  CheckEquals('', TrimSet('', ['"']), 'TrimSet empty string');

  CheckEquals('text"', TrimLeftSet('""text"', ['"']), 'TrimLeftSet');
  CheckEquals('', TrimLeftSet('""', ['"']), 'TrimLeftSet everything trimmed');
end;

procedure TTestUtilityStrings.TestPos;
begin
  CheckEquals(2, ImagingUtility.PosEx('bc', 'abcabc'), 'PosEx default offset');
  CheckEquals(2, ImagingUtility.PosEx('bc', 'abcabc', 2), 'PosEx offset at match');
  CheckEquals(5, ImagingUtility.PosEx('bc', 'abcabc', 3), 'PosEx offset behind first match');
  CheckEquals(0, ImagingUtility.PosEx('bc', 'abcabc', 6), 'PosEx offset behind last match');
  CheckEquals(0, ImagingUtility.PosEx('x', 'abcabc'), 'PosEx not found');
  CheckEquals(0, ImagingUtility.PosEx('abcabcabc', 'abcabc'), 'PosEx substring longer than string');

  CheckEquals(2, PosNoCase('BC', 'abcabc'), 'PosNoCase');
  CheckEquals(5, PosNoCase('bC', 'ABCABC', 3), 'PosNoCase with offset');
  CheckEquals(0, PosNoCase('X', 'abcabc'), 'PosNoCase not found');

  CheckTrue(ContainsAnySubStr('image.png', ['.jpg', '.png']), 'ContainsAnySubStr found');
  CheckFalse(ContainsAnySubStr('image.png', ['.jpg', '.gif']), 'ContainsAnySubStr not found');
end;

procedure TTestUtilityStrings.TestFileNameParts;
begin
  CheckEquals('png', GetFileExt('image.png'), 'GetFileExt');
  CheckEquals('gz', GetFileExt('dir/archive.tar.gz'), 'GetFileExt more dots');
  CheckEquals('', GetFileExt('image'), 'GetFileExt no extension');

  // Both kinds of slashes are path delimiters on all platforms
  CheckEquals('image.png', GetFileName('dir/sub/image.png'), 'GetFileName slashes');
  CheckEquals('image.png', GetFileName('dir\sub\image.png'), 'GetFileName backslashes');
  CheckEquals('image.png', GetFileName('image.png'), 'GetFileName no dir');
  CheckEquals('', GetFileName('dir/sub/'), 'GetFileName dir only');

  CheckEquals('dir/sub', GetFileDir('dir/sub/image.png'), 'GetFileDir slashes');
  CheckEquals('dir\sub', GetFileDir('dir\sub\image.png'), 'GetFileDir backslashes');
  CheckEquals('', GetFileDir('image.png'), 'GetFileDir no dir');
end;

{ TTestUtilityRects }

procedure TTestUtilityRects.TestBoundsAndRects;
var
  R: TRect;
begin
  CheckEqualsRect(Rect(10, 20, 40, 60), BoundsToRect(10, 20, 30, 40), 'BoundsToRect');
  CheckEqualsRect(Rect(10, 20, 40, 60), BoundsToRect(Rect(10, 20, 30, 40)), 'BoundsToRect from rect');
  CheckEqualsRect(Rect(10, 20, 30, 40), RectToBounds(Rect(10, 20, 40, 60)), 'RectToBounds');

  CheckEquals(30, RectWidth(Rect(10, 20, 40, 60)), 'RectWidth');
  CheckEquals(40, RectHeight(Rect(10, 20, 40, 60)), 'RectHeight');

  R := Rect(40, 60, 10, 20);
  NormalizeRect(R);
  CheckEqualsRect(Rect(10, 20, 40, 60), R, 'NormalizeRect swapped');

  R := Rect(10, 20, 40, 60);
  NormalizeRect(R);
  CheckEqualsRect(Rect(10, 20, 40, 60), R, 'NormalizeRect already normal');
end;

procedure TTestUtilityRects.TestRectRelations;
begin
  CheckTrue(RectInRect(Rect(10, 10, 20, 20), Rect(0, 0, 100, 100)), 'RectInRect inside');
  CheckTrue(RectInRect(Rect(0, 0, 100, 100), Rect(0, 0, 100, 100)), 'RectInRect the same');
  CheckFalse(RectInRect(Rect(-1, 10, 20, 20), Rect(0, 0, 100, 100)), 'RectInRect partly outside');
  CheckFalse(RectInRect(Rect(0, 0, 100, 100), Rect(10, 10, 20, 20)), 'RectInRect bigger');

  CheckTrue(RectIntersects(Rect(0, 0, 10, 10), Rect(5, 5, 20, 20)), 'RectIntersects overlap');
  CheckTrue(RectIntersects(Rect(5, 5, 6, 6), Rect(0, 0, 10, 10)), 'RectIntersects inside');
  CheckFalse(RectIntersects(Rect(0, 0, 10, 10), Rect(20, 0, 30, 10)), 'RectIntersects next to');
  CheckFalse(RectIntersects(Rect(0, 0, 10, 10), Rect(0, 20, 10, 30)), 'RectIntersects below');
end;

procedure TTestUtilityRects.TestClipRectBounds;
var
  X, Y, W, H: LongInt;
  Clip: TRect;

  procedure DoClip(AX, AY, AW, AH: LongInt);
  begin
    X := AX;
    Y := AY;
    W := AW;
    H := AH;
    ClipRectBounds(X, Y, W, H, Clip);
  end;

begin
  Clip := Rect(0, 0, 100, 50);

  DoClip(10, 20, 30, 10);
  CheckEqualsRect(Rect(10, 20, 30, 10), Rect(X, Y, W, H), 'Inside stays');

  DoClip(-10, -5, 30, 20);
  CheckEqualsRect(Rect(0, 0, 20, 15), Rect(X, Y, W, H), 'Clipped at left and top');

  DoClip(90, 45, 30, 20);
  CheckEqualsRect(Rect(90, 45, 10, 5), Rect(X, Y, W, H), 'Clipped at right and bottom');

  DoClip(-10, -10, 200, 200);
  CheckEqualsRect(Rect(0, 0, 100, 50), Rect(X, Y, W, H), 'Bigger than clip');

  DoClip(120, 60, 30, 20);
  CheckEquals(0, W, 'Outside at right has zero width');
  CheckEquals(0, H, 'Outside at bottom has zero height');

  DoClip(-50, -50, 30, 20);
  CheckTrue(W <= 0, 'Outside at left has no width');
  CheckTrue(H <= 0, 'Outside at top has no height');

  Clip := Rect(20, 10, 60, 40);
  DoClip(0, 0, 100, 100);
  CheckEqualsRect(Rect(20, 10, 40, 30), Rect(X, Y, W, H), 'Clip with non zero origin');
end;

procedure TTestUtilityRects.TestClipCopyBounds;
const
  SrcWidth = 100;
  SrcHeight = 80;
var
  SrcX, SrcY, W, H, DstX, DstY: LongInt;
  DstClip: TRect;

  procedure DoClip(ASrcX, ASrcY, AW, AH, ADstX, ADstY: LongInt);
  begin
    SrcX := ASrcX;
    SrcY := ASrcY;
    W := AW;
    H := AH;
    DstX := ADstX;
    DstY := ADstY;
    ClipCopyBounds(SrcX, SrcY, W, H, DstX, DstY, SrcWidth, SrcHeight, DstClip);
  end;

  procedure CheckBounds(ESrcX, ESrcY, EW, EH, EDstX, EDstY: LongInt; const Msg: string);
  begin
    CheckEquals(ESrcX, SrcX, Msg + ': SrcX');
    CheckEquals(ESrcY, SrcY, Msg + ': SrcY');
    CheckEquals(EW, W, Msg + ': Width');
    CheckEquals(EH, H, Msg + ': Height');
    CheckEquals(EDstX, DstX, Msg + ': DstX');
    CheckEquals(EDstY, DstY, Msg + ': DstY');
  end;

begin
  DstClip := Rect(0, 0, 200, 150);

  DoClip(10, 10, 50, 40, 20, 30);
  CheckBounds(10, 10, 50, 40, 20, 30, 'Nothing to clip');

  // Source rectangle starts before the source image: the copied part
  // is moved in destination by the same amount
  DoClip(-10, -5, 50, 40, 0, 0);
  CheckBounds(0, 0, 40, 35, 10, 5, 'Negative source position');

  DoClip(0, 0, 50, 40, -10, -5);
  CheckBounds(10, 5, 40, 35, 0, 0, 'Negative destination position');

  DoClip(80, 70, 50, 40, 0, 0);
  CheckBounds(80, 70, 20, 10, 0, 0, 'Source rectangle over the source image');

  DoClip(0, 0, 50, 40, 190, 140);
  CheckBounds(0, 0, 10, 10, 190, 140, 'Destination rectangle over the clip');

  DoClip(0, 0, SrcWidth, SrcHeight, 0, 0);
  CheckBounds(0, 0, SrcWidth, SrcHeight, 0, 0, 'Whole source image');

  DstClip := Rect(50, 50, 100, 100);
  DoClip(0, 0, 100, 80, 0, 0);
  CheckBounds(50, 50, 50, 30, 50, 50, 'Clip with non zero origin');
end;

procedure TTestUtilityRects.TestClipStretchBounds;
const
  SrcImageWidth = 100;
  SrcImageHeight = 100;
var
  SrcX, SrcY, SrcW, SrcH, DstX, DstY, DstW, DstH: LongInt;
  DstClip: TRect;

  procedure DoClip(ASrcX, ASrcY, ASrcW, ASrcH, ADstX, ADstY, ADstW, ADstH: LongInt);
  begin
    SrcX := ASrcX;
    SrcY := ASrcY;
    SrcW := ASrcW;
    SrcH := ASrcH;
    DstX := ADstX;
    DstY := ADstY;
    DstW := ADstW;
    DstH := ADstH;
    ClipStretchBounds(SrcX, SrcY, SrcW, SrcH, DstX, DstY, DstW, DstH,
                      SrcImageWidth, SrcImageHeight, DstClip);
  end;

begin
  DstClip := Rect(0, 0, 400, 400);

  DoClip(0, 0, 100, 100, 0, 0, 200, 200);
  CheckEqualsRect(Rect(0, 0, 100, 100), Rect(SrcX, SrcY, SrcW, SrcH), 'Nothing to clip: source');
  CheckEqualsRect(Rect(0, 0, 200, 200), Rect(DstX, DstY, DstW, DstH), 'Nothing to clip: destination');

  // Destination is two times larger, so each clipped destination pixel
  // removes half a pixel from source
  DoClip(0, 0, 100, 100, -100, -50, 200, 200);
  CheckEqualsRect(Rect(50, 25, 50, 75), Rect(SrcX, SrcY, SrcW, SrcH), 'Negative destination: source');
  CheckEqualsRect(Rect(0, 0, 100, 150), Rect(DstX, DstY, DstW, DstH), 'Negative destination: destination');

  DoClip(0, 0, 100, 100, 300, 300, 200, 200);
  CheckEqualsRect(Rect(0, 0, 50, 50), Rect(SrcX, SrcY, SrcW, SrcH), 'Destination over the clip: source');
  CheckEqualsRect(Rect(300, 300, 100, 100), Rect(DstX, DstY, DstW, DstH), 'Destination over the clip: destination');

  DoClip(50, 50, 100, 100, 0, 0, 200, 200);
  CheckEqualsRect(Rect(50, 50, 50, 50), Rect(SrcX, SrcY, SrcW, SrcH), 'Source over the image: source');
  CheckEqualsRect(Rect(0, 0, 100, 100), Rect(DstX, DstY, DstW, DstH), 'Source over the image: destination');

  DoClip(-50, -50, 100, 100, 0, 0, 200, 200);
  CheckEqualsRect(Rect(0, 0, 50, 50), Rect(SrcX, SrcY, SrcW, SrcH), 'Negative source: source');
  CheckEqualsRect(Rect(100, 100, 100, 100), Rect(DstX, DstY, DstW, DstH), 'Negative source: destination');
end;

procedure TTestUtilityRects.TestScaleToFit;
var
  Current, MaxSize, Scaled: TSize;
begin
  CheckEqualsRect(Rect(0, 25, 100, 75),
    ScaleRectToRect(Rect(0, 0, 200, 100), Rect(0, 0, 100, 100)), 'Wide into square');
  CheckEqualsRect(Rect(25, 0, 75, 100),
    ScaleRectToRect(Rect(0, 0, 100, 200), Rect(0, 0, 100, 100)), 'Tall into square');
  CheckEqualsRect(Rect(0, 0, 100, 100),
    ScaleRectToRect(Rect(0, 0, 50, 50), Rect(0, 0, 100, 100)), 'Small square is enlarged');
  CheckEqualsRect(Rect(10, 45, 110, 95),
    ScaleRectToRect(Rect(0, 0, 200, 100), Rect(10, 20, 110, 120)), 'Target with non zero origin');
  CheckEqualsRect(Rect(0, 0, 100, 50),
    ScaleRectToRect(Rect(500, 500, 700, 600), Rect(0, 0, 100, 50)), 'Same aspect, source origin ignored');

  Current.cx := 200;
  Current.cy := 100;
  MaxSize.cx := 100;
  MaxSize.cy := 100;
  Scaled := ScaleSizeToFit(Current, MaxSize);
  CheckEquals(100, Scaled.cx, 'ScaleSizeToFit wide: width');
  CheckEquals(50, Scaled.cy, 'ScaleSizeToFit wide: height');

  Current.cx := 30;
  Current.cy := 90;
  Scaled := ScaleSizeToFit(Current, MaxSize);
  CheckEquals(33, Scaled.cx, 'ScaleSizeToFit tall: width');
  CheckEquals(100, Scaled.cy, 'ScaleSizeToFit tall: height');
end;

initialization
  RegisterImagingTest(TTestUtilityMath);
  RegisterImagingTest(TTestUtilityMemory);
  RegisterImagingTest(TTestUtilityStrings);
  RegisterImagingTest(TTestUtilityRects);
end.
