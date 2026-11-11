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

{ Tests of the Pascal WebP codec (PasWebP units in Extensions/Libs), called
  directly so they do not depend on which WebP backend Imaging has selected.

  Decoder: small WebP files are stored here as arrays. They were made by cwebp 1.3.2
  from 37x29 pixel crops of Demos/Data images, expected CRC32 of the pixels
  is of what dwebp 1.3.2 (libwebp) decodes from them.

  Lossless encoder: what it writes must decode to the same pixels.

  Lossy encoder: what it writes must decode (to the same pixels by libwebp
  where it is found), alpha must be exact, colors close to the source.}
unit TestImagingWebP;

{$I ImagingOptions.inc}

// The codec needs FPC or Delphi 2009+ (as ImagingLibWebP decides)
{$IF not Defined(DONT_LINK_EXT_FILE_FORMATS) and not Defined(WEBP_NO_BUNDLED) and
  ((Defined(DCC) and (CompilerVersion >= 20)) or Defined(FPC))}
  {$DEFINE TEST_PASWEBP}
{$IFEND}

{$IF Defined(TEST_PASWEBP) and not Defined(WEBP_NO_DYNLIB) and Defined(HAS_DYNLIBS)}
  {$DEFINE TEST_PASWEBP_WITH_LIBWEBP}
{$IFEND}

interface

uses
  SysUtils, Classes, ImagingTypes, Imaging, ImagingUtility,
{$IFDEF TEST_PASWEBP}
  PasWebPDec, PasWebPLossless, PasWebPLossy, ImagingLibWebP, ImagingExtFileFormats,
{$ENDIF}
{$IFDEF TEST_PASWEBP_WITH_LIBWEBP}
  LibWebPDynLib,
{$ENDIF}
  ImagingTestCase;

{$IFDEF TEST_PASWEBP}
type
  TTestWebPDecoder = class(TImagingTestCase)
  private
    function DecodeBGRA(Data: Pointer; Size: Integer; out Width, Height: Integer): TDynByteArray;
    procedure CheckFile(const Name: string; Data: Pointer; Size, Width, Height: Integer;
      Crc: UInt32);
  published
    procedure TestDecode;
    procedure TestDecodeTigers;
    procedure TestDecodeWithoutAlpha;
    procedure TestDecodeWithStride;
    procedure TestWrongSize;
    procedure TestCutData;
    procedure TestChangedData;
    procedure TestSameAsLibWebP;
  end;

  TTestWebPLossless = class(TImagingTestCase)
  private
    { Encodes the pixels (B, G, R, A or B, G, R bytes), decodes the result
      and checks that it is what went in. Returns size of the file. }
    function CheckRoundTrip(const Name: string; const Pixels: TDynByteArray;
      Width, Height: Integer; WithAlpha: Boolean; Effort: Integer): Integer;
  published
    procedure TestSizes;
    procedure TestEfforts;
    procedure TestFewColors;
    procedure TestRunsAndNoise;
    procedure TestInvisiblePixelsKept;
    procedure TestStride;
    procedure TestWrongParameters;
    procedure TestWidestImage;
    procedure TestAlphaPlane;
    procedure TestSizeOfDemoImages;
    procedure TestImagingSaveLoad;
    procedure TestDecodedByLibWebP;
  end;

  TTestWebPLossy = class(TImagingTestCase)
  private
    { Encodes the pixels, decodes the file, checks size of the image and
      that alpha is the same. Returns PSNR of the color channels of visible
      pixels in dB and the size of the file. }
    function EncodeAndDecode(const Name: string; const Pixels: TDynByteArray;
      Width, Height: Integer; WithAlpha: Boolean; Quality, Effort: Integer;
      out FileSize: Integer): Double;
  published
    procedure TestSizes;
    procedure TestQualitiesAndEfforts;
    procedure TestQualityOrder;
    procedure TestOpaqueAlphaIsNotStored;
    procedure TestInvisiblePixels;
    procedure TestWrongParameters;
    procedure TestWidestImage;
    procedure TestDemoPhoto;
    procedure TestImagingSaveLoad;
    procedure TestDecodedByLibWebP;
  end;
{$ENDIF}

implementation

{$IFDEF TEST_PASWEBP}

const
  { Lossy, normal loop filter }
  Lossy: array[0..193] of Byte = (
    $52, $49, $46, $46, $BA, $00, $00, $00, $57, $45, $42, $50, $56, $50, $38, $20,
    $AE, $00, $00, $00, $90, $05, $00, $9D, $01, $2A, $25, $00, $1D, $00, $3E, $CD,
    $52, $A3, $4B, $A7, $A4, $A3, $22, $B5, $58, $08, $00, $F0, $19, $89, $65, $00,
    $C8, $98, $15, $C4, $5C, $D0, $83, $C9, $F9, $33, $EF, $95, $89, $82, $FF, $A6,
    $35, $7A, $0E, $D0, $90, $EB, $29, $AE, $00, $00, $FE, $B9, $2B, $DA, $2C, $6C,
    $16, $BD, $CC, $3F, $0F, $D4, $19, $99, $10, $4F, $04, $0B, $FA, $04, $FE, $75,
    $D6, $C1, $92, $89, $89, $4B, $10, $5E, $6C, $61, $F5, $F4, $49, $D4, $FA, $09,
    $3C, $68, $85, $6E, $B2, $E5, $6B, $DC, $D9, $06, $8E, $04, $89, $89, $DE, $D4,
    $7B, $CE, $A6, $77, $A1, $C4, $07, $00, $67, $EE, $5F, $D1, $DE, $97, $B2, $13,
    $64, $9A, $82, $28, $12, $DF, $54, $3D, $84, $3D, $42, $F7, $70, $50, $1B, $8E,
    $56, $71, $FD, $DA, $57, $76, $E1, $B3, $4D, $E2, $A4, $3D, $75, $79, $42, $51,
    $32, $A8, $F9, $E0, $4D, $1F, $F0, $FE, $E0, $1C, $79, $57, $30, $E8, $0A, $71,
    $C4, $00);

  { Lossy, simple loop filter }
  LossySimpleFilter: array[0..193] of Byte = (
    $52, $49, $46, $46, $BA, $00, $00, $00, $57, $45, $42, $50, $56, $50, $38, $20,
    $AE, $00, $00, $00, $92, $05, $00, $9D, $01, $2A, $25, $00, $1D, $00, $3E, $CD,
    $52, $A3, $4B, $A6, $23, $A2, $A2, $B5, $58, $08, $04, $C0, $19, $89, $65, $00,
    $C8, $98, $15, $C4, $5C, $D0, $83, $C9, $F9, $33, $EF, $95, $89, $82, $FF, $A6,
    $35, $7A, $0E, $D0, $90, $EB, $29, $AE, $00, $00, $FE, $B9, $2B, $DA, $2C, $6C,
    $16, $BD, $CC, $3F, $0F, $D4, $19, $99, $10, $4F, $04, $0B, $FA, $04, $FE, $75,
    $D6, $C1, $92, $89, $89, $4B, $10, $5E, $6C, $61, $F5, $F4, $49, $D4, $FA, $09,
    $3C, $68, $85, $6E, $B2, $E5, $6B, $DC, $D9, $06, $8E, $04, $89, $89, $DE, $D4,
    $7B, $CE, $A6, $77, $A1, $C4, $07, $00, $67, $EE, $5F, $D1, $DE, $97, $B2, $13,
    $64, $9A, $82, $28, $12, $DF, $54, $3D, $84, $3D, $42, $F7, $70, $50, $1B, $8E,
    $56, $71, $FD, $DA, $57, $76, $E1, $B3, $4D, $E2, $A4, $3D, $75, $79, $42, $51,
    $32, $A8, $F9, $E0, $4D, $1F, $F0, $FE, $E0, $1C, $79, $57, $30, $E8, $0A, $71,
    $C4, $00);

  { Lossy, four segments }
  LossySegments: array[0..189] of Byte = (
    $52, $49, $46, $46, $B6, $00, $00, $00, $57, $45, $42, $50, $56, $50, $38, $20,
    $AA, $00, $00, $00, $F0, $05, $00, $9D, $01, $2A, $25, $00, $1D, $00, $3F, $3D,
    $74, $AA, $43, $AF, $A6, $A4, $1A, $AC, $04, $00, $F8, $13, $C5, $34, $80, $63,
    $82, $FF, $F0, $14, $44, $67, $FD, $C4, $27, $36, $28, $B2, $E8, $38, $C3, $CB,
    $77, $1E, $FC, $24, $94, $E3, $22, $E9, $E9, $17, $87, $00, $00, $FE, $11, $25,
    $F1, $9D, $69, $76, $DE, $8C, $0C, $9E, $2E, $E5, $0B, $22, $85, $1C, $99, $7D,
    $BB, $47, $90, $22, $D9, $FA, $0B, $A1, $A4, $A3, $FE, $04, $79, $1C, $C7, $59,
    $A5, $FC, $7C, $B2, $3E, $1A, $0B, $41, $ED, $9B, $43, $70, $7A, $3A, $6C, $DF,
    $0E, $2C, $2E, $A7, $97, $C1, $CF, $AB, $B7, $E8, $5D, $1E, $9E, $40, $BA, $CB,
    $93, $C4, $B9, $E9, $1B, $24, $71, $C7, $04, $BB, $73, $42, $E2, $66, $D5, $29,
    $02, $FE, $7B, $A3, $1F, $FA, $5D, $A4, $9A, $DD, $01, $EF, $36, $E8, $94, $41,
    $5C, $F0, $54, $4A, $56, $CB, $74, $E2, $4F, $9B, $BB, $BC, $00, $00);

  { Lossy with compressed alpha plane }
  LossyAlpha: array[0..831] of Byte = (
    $52, $49, $46, $46, $38, $03, $00, $00, $57, $45, $42, $50, $56, $50, $38, $58,
    $0A, $00, $00, $00, $10, $00, $00, $00, $24, $00, $00, $1C, $00, $00, $41, $4C,
    $50, $48, $D5, $01, $00, $00, $01, $A0, $43, $6D, $9B, $21, $DB, $7A, $AB, $AB,
    $8E, $6D, $9F, $C8, $B6, $75, $8D, $CC, $B6, $6D, $2B, $B2, $6D, $FB, $DE, $D8,
    $46, $64, $DB, $B6, $CD, $EA, $AE, $37, $18, $DC, $F0, $A6, $11, $31, $01, $FC,
    $7F, $0E, $31, $F5, $48, $E1, $3F, $55, $29, $F5, $48, $5D, $84, $48, $DB, $D8,
    $5D, $55, $D1, $7D, $84, $5E, $EB, $1F, $70, $F2, $01, $E3, $A9, $BA, $08, $09,
    $A6, $EC, $70, $CC, $89, $BB, $77, $48, $F4, $3E, $E1, $03, $D5, $1F, $37, $A7,
    $EA, $50, $C1, $FC, $3B, $FF, $52, $0D, $6D, $22, $AB, $DE, $D1, $52, $E7, $AC,
    $9B, $12, $DB, $44, $38, $AE, $D6, $26, $E7, $3A, $B6, $44, $B6, $2F, $FE, $63,
    $29, $9A, $7D, $B7, $27, $01, $88, $F4, $BB, $D7, $26, $5B, $8A, $26, $20, $B1,
    $8F, $EA, $5F, $5A, $B4, $B8, $3E, $11, $22, $C3, $9E, $B3, $D1, $3F, $B5, $D8,
    $03, $12, $3B, $A9, $4F, $6E, $35, $71, $EB, $BF, $2C, $D6, $1E, $4A, $22, $32,
    $F2, $6D, $FD, $E7, $FC, $55, $E3, $77, $F8, $DB, $92, $48, $6C, $A6, $9E, $04,
    $70, $8B, $D9, $EC, $89, $A4, $8A, $5E, $CF, $EA, $1B, $33, $00, $6E, $31, $C7,
    $C4, $BA, $B5, $65, $57, $48, $3D, $39, $D5, $7F, $AD, $3D, $98, $1E, $70, $97,
    $3E, $36, $80, $1E, $B1, $27, $A7, $F8, $6F, $0F, $C6, $7E, $A3, $DB, $93, $02,
    $91, $6B, $CD, $16, $B7, $A6, $17, $67, $EA, $E3, $15, $09, $22, $17, $9B, $03,
    $8F, $E8, $E1, $24, $08, $F0, $84, $75, $31, $4F, $87, $0D, $F5, $99, $3E, $44,
    $5A, $1F, $B2, $E6, $14, $3D, $8F, $18, $5A, $7A, $7F, $6A, $29, $7E, $DE, $8F,
    $61, $1F, $F9, $E5, $38, $12, $10, $18, $F8, $AD, $65, $B5, $DE, $07, $01, $A8,
    $98, $91, $2D, $B5, $F7, $C0, $35, $36, $1B, $90, $00, $2A, $96, $69, $79, $D1,
    $B7, $07, $53, $01, $24, $76, $B2, $31, $7B, $26, $EB, $EB, $F1, $24, $5A, $13,
    $FB, $59, $EB, $3F, $8B, $48, $B4, $3B, $CF, $6C, $E3, $D6, $3C, $E7, $A3, $10,
    $DA, $44, $AE, $30, $EB, $31, $24, $5A, $03, $3C, $66, $5D, $6C, $46, $EC, $E5,
    $CF, $93, $89, $B4, $06, $78, $DA, $A6, $3C, $0C, $A1, $C3, $A0, $EF, $2C, $8D,
    $EF, $0C, $7D, $CF, $7D, $49, $74, $18, $FE, $8B, $A5, $99, $44, $A4, $6D, $C5,
    $4A, $2D, $B5, $57, $ED, $ED, $23, $04, $DA, $47, $D6, $53, $6B, $12, $ED, $13,
    $07, $5B, $5B, $7B, $FE, $CB, $CE, $26, $76, $48, $1C, $6E, $36, $87, $D0, $21,
    $72, $AD, $59, $FD, $D3, $53, $49, $74, $AC, $B8, $D1, $6C, $89, $B4, $0F, $54,
    $2F, $D9, $68, $E3, $3B, $BD, $09, $1D, $02, $F1, $15, $9B, $22, $5D, $8C, $FB,
    $C3, $D2, $B2, $15, $89, $2E, $C6, $FF, $61, $69, $04, $00, $56, $50, $38, $20,
    $3C, $01, $00, $00, $90, $07, $00, $9D, $01, $2A, $25, $00, $1D, $00, $3E, $B5,
    $4A, $A0, $4A, $27, $24, $23, $21, $AA, $AC, $00, $E0, $16, $89, $6C, $00, $B8,
    $69, $C0, $D5, $BE, $07, $8E, $65, $E7, $DD, $80, $F1, $74, $F5, $C0, $D3, $00,
    $F4, $00, $F2, $C0, $F6, $28, $FD, $D0, $F4, $AA, $26, $68, $31, $55, $FB, $CA,
    $9B, $5F, $3E, $D5, $3E, $AB, $E6, $8B, $00, $00, $FE, $FD, $9C, $17, $A4, $6B,
    $8E, $E9, $EE, $66, $09, $12, $FE, $9F, $62, $51, $DF, $11, $79, $CC, $24, $D9,
    $59, $20, $07, $F1, $8C, $CE, $18, $9C, $DF, $88, $C7, $A4, $1F, $95, $E9, $12,
    $03, $FE, $95, $CC, $EF, $7A, $F1, $77, $7B, $90, $E8, $FF, $0D, $1F, $A6, $0B,
    $D6, $6F, $8E, $3F, $C7, $A7, $C6, $E8, $67, $6B, $74, $8E, $98, $69, $13, $41,
    $0A, $F6, $AF, $89, $55, $8B, $BD, $A4, $66, $99, $D7, $26, $3A, $5D, $23, $E3,
    $9E, $F2, $6A, $FC, $A0, $8E, $FA, $6E, $53, $05, $1A, $94, $E2, $81, $BA, $69,
    $4A, $73, $3B, $7A, $51, $80, $E0, $A0, $F7, $44, $84, $A7, $95, $4F, $B6, $62,
    $F3, $3E, $FA, $5E, $3D, $96, $82, $A3, $35, $47, $A0, $BF, $03, $5B, $2D, $05,
    $4C, $D5, $4C, $F5, $B4, $EB, $8B, $1C, $1A, $23, $E5, $28, $9C, $E1, $C0, $3A,
    $2F, $E7, $89, $73, $6B, $49, $CF, $B7, $BF, $39, $AE, $3D, $FA, $A7, $E0, $E1,
    $C4, $CD, $26, $F0, $C3, $5C, $1D, $1E, $7D, $2B, $42, $AD, $01, $ED, $15, $3C,
    $CD, $AC, $83, $6C, $3D, $7B, $91, $06, $A8, $13, $95, $70, $64, $67, $2D, $59,
    $06, $F0, $9E, $E6, $92, $DC, $F9, $BF, $1A, $F0, $44, $C5, $AF, $97, $A5, $4F,
    $13, $AF, $20, $2F, $B2, $B6, $4B, $D7, $7B, $FB, $AD, $40, $E3, $22, $70, $DF,
    $FA, $76, $1B, $F1, $31, $AA, $1E, $41, $63, $D1, $27, $06, $C5, $BC, $00, $00);

  { Lossy with uncompressed alpha plane }
  LossyAlphaRaw: array[0..1435] of Byte = (
    $52, $49, $46, $46, $94, $05, $00, $00, $57, $45, $42, $50, $56, $50, $38, $58,
    $0A, $00, $00, $00, $10, $00, $00, $00, $24, $00, $00, $1C, $00, $00, $41, $4C,
    $50, $48, $32, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $01, $03, $04, $05, $04, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $02, $04, $04, $05, $04, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $01, $03, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $02, $02, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $07, $43, $6A, $7D, $6A, $1C,
    $00, $02, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $04, $00, $00, $26,
    $5A, $76, $7B, $61, $00, $00, $00, $00, $00, $00, $00, $04, $00, $08, $7A, $DD,
    $FF, $FF, $FF, $FF, $EE, $51, $00, $02, $00, $00, $00, $00, $00, $00, $00, $02,
    $00, $00, $31, $A8, $F7, $FF, $FF, $FF, $FF, $01, $00, $00, $00, $00, $00, $03,
    $00, $3C, $D9, $FF, $FF, $FE, $FC, $FB, $FB, $FF, $FF, $49, $00, $03, $00, $00,
    $00, $00, $00, $03, $00, $00, $78, $FC, $FF, $FF, $FD, $FB, $FB, $FC, $03, $00,
    $00, $00, $00, $03, $00, $59, $FE, $FF, $F9, $FF, $FE, $FE, $FF, $FF, $FB, $FF,
    $DA, $06, $00, $01, $00, $00, $00, $03, $00, $0B, $B5, $FF, $FD, $FB, $FF, $FE,
    $FE, $FF, $FF, $04, $00, $00, $00, $04, $00, $66, $FF, $FF, $FF, $FF, $F7, $FF,
    $FF, $FE, $FF, $FF, $FE, $FF, $43, $00, $03, $00, $00, $03, $00, $13, $CB, $FF,
    $FD, $FF, $FF, $F6, $FF, $FF, $FE, $FF, $05, $00, $00, $04, $00, $5C, $FF, $FF,
    $FF, $C6, $54, $1E, $55, $F7, $FF, $FE, $FF, $FC, $FF, $6F, $00, $04, $00, $03,
    $00, $16, $D8, $FF, $FF, $F9, $8E, $3C, $1C, $5A, $F8, $FF, $FE, $04, $00, $04,
    $00, $50, $FF, $FF, $FF, $7C, $00, $00, $00, $00, $A3, $FF, $FB, $FF, $FB, $FF,
    $7B, $00, $04, $02, $00, $07, $CA, $FF, $FF, $D5, $2A, $00, $00, $00, $00, $A3,
    $FF, $FB, $03, $04, $00, $41, $FC, $FF, $FE, $5F, $00, $00, $04, $06, $00, $84,
    $FF, $FA, $FF, $FC, $FF, $6D, $00, $05, $00, $00, $B2, $FF, $FF, $BE, $0D, $00,
    $05, $03, $06, $00, $83, $FF, $FA, $05, $00, $1A, $E9, $FF, $FF, $59, $00, $04,
    $01, $00, $03, $00, $9C, $FF, $FB, $FF, $FE, $FF, $55, $00, $07, $00, $88, $FF,
    $FF, $BF, $02, $00, $04, $00, $00, $03, $00, $92, $FF, $FB, $01, $00, $BC, $FF,
    $FF, $71, $00, $04, $00, $00, $01, $00, $00, $C0, $FF, $FC, $FE, $FF, $FB, $29,
    $00, $00, $45, $FF, $FF, $C9, $09, $00, $03, $00, $00, $00, $00, $00, $BA, $FF,
    $FC, $00, $83, $FF, $FF, $8D, $00, $03, $01, $00, $00, $01, $00, $08, $E2, $FF,
    $FE, $FE, $FF, $E4, $0B, $00, $13, $DF, $FF, $E6, $1B, $00, $04, $00, $00, $00,
    $01, $00, $0E, $EA, $FF, $FE, $3D, $FF, $FF, $B6, $00, $00, $01, $00, $00, $00,
    $02, $00, $2A, $FB, $FF, $FE, $FC, $FF, $B4, $00, $00, $9B, $FF, $FD, $44, $00,
    $04, $00, $00, $00, $00, $02, $00, $38, $FF, $FF, $FE, $CE, $FF, $D8, $10, $00,
    $02, $00, $00, $00, $00, $04, $00, $5C, $FF, $FD, $FF, $FB, $FF, $88, $00, $43,
    $FF, $FF, $79, $00, $04, $00, $00, $00, $00, $00, $04, $00, $68, $FF, $FC, $FF,
    $FF, $F9, $34, $00, $04, $00, $00, $00, $00, $00, $04, $00, $8D, $FF, $FB, $FF,
    $FD, $FF, $55, $00, $CB, $FF, $BD, $00, $00, $01, $00, $00, $00, $00, $00, $03,
    $00, $97, $FF, $FB, $FF, $FF, $76, $00, $04, $00, $00, $00, $00, $00, $01, $00,
    $00, $BE, $FF, $FC, $FE, $FF, $FD, $15, $64, $FF, $F0, $25, $00, $03, $00, $00,
    $00, $00, $00, $01, $00, $00, $C8, $FF, $FD, $FE, $BB, $00, $00, $01, $00, $00,
    $00, $00, $00, $01, $00, $0F, $EB, $FF, $FE, $FD, $FF, $D9, $12, $DB, $FF, $67,
    $00, $04, $00, $00, $00, $00, $00, $00, $01, $00, $14, $F1, $FF, $FE, $FD, $24,
    $00, $03, $00, $00, $00, $00, $00, $00, $02, $00, $3B, $FF, $FF, $FE, $FC, $FF,
    $99, $65, $FF, $BC, $00, $01, $00, $00, $00, $00, $00, $00, $00, $03, $00, $42,
    $FF, $FF, $FF, $FC, $00, $04, $00, $00, $00, $00, $00, $00, $00, $04, $00, $6D,
    $FF, $FC, $FF, $FC, $FF, $8E, $D0, $FF, $2D, $00, $03, $00, $00, $00, $00, $00,
    $00, $00, $04, $00, $71, $FF, $FB, $FF, $FB, $01, $01, $00, $00, $00, $00, $00,
    $00, $00, $03, $00, $9C, $FF, $FB, $FF, $FF, $FF, $F6, $FF, $84, $00, $04, $00,
    $00, $00, $00, $00, $00, $00, $00, $02, $00, $A0, $FF, $FB, $FF, $FE, $03, $00,
    $00, $00, $00, $00, $00, $00, $01, $00, $02, $CF, $FF, $FD, $FF, $FF, $FD, $FF,
    $D9, $08, $00, $01, $00, $00, $00, $00, $00, $00, $00, $01, $00, $03, $D1, $FF,
    $FD, $FE, $FF, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $1B, $F5, $FF,
    $FE, $FF, $FF, $FD, $FF, $54, $00, $04, $00, $00, $00, $00, $00, $00, $00, $00,
    $01, $00, $1C, $F5, $FF, $FE, $FD, $FF, $56, $50, $38, $20, $3C, $01, $00, $00,
    $90, $07, $00, $9D, $01, $2A, $25, $00, $1D, $00, $3E, $B5, $4A, $A0, $4A, $27,
    $24, $23, $21, $AA, $AC, $00, $E0, $16, $89, $6C, $00, $B8, $69, $C0, $D5, $BE,
    $07, $8E, $65, $E7, $DD, $80, $F1, $74, $F5, $C0, $D3, $00, $F4, $00, $F2, $C0,
    $F6, $28, $FD, $D0, $F4, $AA, $26, $68, $31, $55, $FB, $CA, $9B, $5F, $3E, $D5,
    $3E, $AB, $E6, $8B, $00, $00, $FE, $FD, $9C, $17, $A4, $6B, $8E, $E9, $EE, $66,
    $09, $12, $FE, $9F, $62, $51, $DF, $11, $79, $CC, $24, $D9, $59, $20, $07, $F1,
    $8C, $CE, $18, $9C, $DF, $88, $C7, $A4, $1F, $95, $E9, $12, $03, $FE, $95, $CC,
    $EF, $7A, $F1, $77, $7B, $90, $E8, $FF, $0D, $1F, $A6, $0B, $D6, $6F, $8E, $3F,
    $C7, $A7, $C6, $E8, $67, $6B, $74, $8E, $98, $69, $13, $41, $0A, $F6, $AF, $89,
    $55, $8B, $BD, $A4, $66, $99, $D7, $26, $3A, $5D, $23, $E3, $9E, $F2, $6A, $FC,
    $A0, $8E, $FA, $6E, $53, $05, $1A, $94, $E2, $81, $BA, $69, $4A, $73, $3B, $7A,
    $51, $80, $E0, $A0, $F7, $44, $84, $A7, $95, $4F, $B6, $62, $F3, $3E, $FA, $5E,
    $3D, $96, $82, $A3, $35, $47, $A0, $BF, $03, $5B, $2D, $05, $4C, $D5, $4C, $F5,
    $B4, $EB, $8B, $1C, $1A, $23, $E5, $28, $9C, $E1, $C0, $3A, $2F, $E7, $89, $73,
    $6B, $49, $CF, $B7, $BF, $39, $AE, $3D, $FA, $A7, $E0, $E1, $C4, $CD, $26, $F0,
    $C3, $5C, $1D, $1E, $7D, $2B, $42, $AD, $01, $ED, $15, $3C, $CD, $AC, $83, $6C,
    $3D, $7B, $91, $06, $A8, $13, $95, $70, $64, $67, $2D, $59, $06, $F0, $9E, $E6,
    $92, $DC, $F9, $BF, $1A, $F0, $44, $C5, $AF, $97, $A5, $4F, $13, $AF, $20, $2F,
    $B2, $B6, $4B, $D7, $7B, $FB, $AD, $40, $E3, $22, $70, $DF, $FA, $76, $1B, $F1,
    $31, $AA, $1E, $41, $63, $D1, $27, $06, $C5, $BC, $00, $00);

  { Lossless photo }
  Lossless: array[0..471] of Byte = (
    $52, $49, $46, $46, $D0, $01, $00, $00, $57, $45, $42, $50, $56, $50, $38, $4C,
    $C3, $01, $00, $00, $2F, $24, $00, $07, $00, $C7, $C0, $28, $00, $C0, $42, $C3,
    $6F, $FD, $13, $DA, $F6, $B1, $06, $E3, $48, $92, $D3, $88, $F5, $54, $00, $CA,
    $3F, $2B, $06, $33, $33, $BB, $18, $04, $00, $58, $66, $46, $83, $F5, $2F, $C5,
    $04, $C6, $ED, $CF, $7F, $9C, $E7, $FA, $BF, $B3, $8E, $7E, $60, $04, $30, $E3,
    $E6, $13, $3C, $2B, $F2, $F1, $A6, $F5, $C4, $06, $99, $2A, $02, $97, $55, $4E,
    $75, $41, $78, $BB, $7C, $DD, $B5, $74, $FD, $51, $3C, $6F, $58, $9E, $D6, $9A,
    $AD, $FE, $75, $CA, $17, $81, $0A, $BD, $B4, $E8, $76, $09, $05, $6D, $DB, $30,
    $EA, $D9, $E7, $0F, $79, $1C, $22, $FA, $9F, $26, $49, $93, $B8, $6E, $3E, $48,
    $8E, $24, $5B, $6C, $9B, $DE, $FF, $01, $E9, $EC, $FC, $13, $B5, $45, $FC, $BF,
    $07, $80, $94, $43, $88, $88, $09, $F8, $E3, $F9, $EC, $40, $FF, $D1, $CD, $F5,
    $77, $EE, $1E, $CB, $F4, $1F, $F7, $F6, $38, $BA, $44, $96, $B7, $0B, $7D, $DA,
    $1B, $C1, $48, $2F, $E8, $3B, $D5, $B6, $14, $72, $8B, $B6, $51, $9F, $D7, $96,
    $D3, $18, $70, $C2, $5A, $E6, $22, $5C, $0F, $73, $C3, $FC, $FA, $9A, $C9, $80,
    $24, $7B, $FD, $58, $D5, $59, $AE, $1B, $F6, $CE, $F1, $35, $61, $DC, $BE, $F8,
    $27, $04, $E5, $C2, $F2, $18, $08, $9C, $4D, $6B, $D4, $74, $F9, $65, $D7, $D5,
    $A2, $B8, $CE, $CA, $0E, $FA, $70, $91, $81, $8A, $9F, $DD, $EE, $BC, $38, $5F,
    $F5, $FC, $74, $CF, $A0, $69, $D7, $AB, $DB, $E3, $BE, $7E, $73, $58, $86, $83,
    $C6, $FE, $7B, $C5, $F9, $7A, $FD, $AC, $E6, $4D, $F4, $70, $0F, $41, $D0, $97,
    $57, $77, $5B, $1F, $4E, $8F, $97, $91, $8C, $23, $65, $7B, $AD, $DD, $16, $13,
    $D3, $C7, $57, $25, $E3, $C4, $2A, $76, $8D, $69, $FA, $7C, $14, $1D, $F3, $9C,
    $1E, $B7, $B5, $B5, $7E, $19, $2C, $64, $0E, $5A, $AE, $75, $A0, $1F, $AD, $B9,
    $9A, $64, $CA, $1D, $E9, $DA, $0B, $FA, $C9, $54, $58, $44, $26, $74, $65, $16,
    $FA, $C1, $FB, $26, $32, $8E, $01, $D3, $7F, $CD, $31, $E7, $34, $82, $5D, $A6,
    $EB, $A3, $EA, $C3, $F7, $F7, $D7, $49, $42, $12, $39, $8F, $68, $DF, $A5, $A8,
    $7C, $CF, $A4, $1A, $A4, $1D, $C9, $F7, $9C, $9A, $3C, $A0, $25, $67, $24, $24,
    $64, $B5, $12, $74, $2D, $45, $E3, $D3, $EE, $C0, $41, $18, $CB, $61, $76, $23,
    $1E, $7B, $0B, $AF, $DB, $63, $DC, $B7, $E9, $9E, $9B, $E5, $B1, $BD, $8D, $24,
    $C6, $EC, $8A, $4F, $43, $6F, $DD, $85, $91, $48, $32, $82, $F6, $9D, $37, $5B,
    $B7, $C7, $90, $39, $69, $7D, $08, $00);

  { Lossless with alpha }
  LosslessAlpha: array[0..851] of Byte = (
    $52, $49, $46, $46, $4C, $03, $00, $00, $57, $45, $42, $50, $56, $50, $38, $4C,
    $3F, $03, $00, $00, $2F, $24, $00, $07, $10, $1F, $E5, $2A, $92, $6C, $57, $E9,
    $7D, $19, $01, $7C, $A3, $0A, $C1, $D8, $C1, $C2, $0B, $4B, $30, $A1, $A0, $6D,
    $1B, $C9, $D7, $F1, $E7, $74, $C0, $F6, $B8, $8D, $24, $D9, $55, $7A, $BF, $FE,
    $78, $98, $24, $46, $E8, $04, $81, $A5, $B1, $C4, $BA, $AD, $6D, $B7, $6D, $F0,
    $29, $43, $A0, $72, $EA, $3D, $AA, $8F, $77, $F3, $18, $AE, $5C, $A5, $1A, $A6,
    $02, $28, $52, $6D, $DA, $06, $CC, $98, $9E, $3B, $40, $0E, $28, $71, $76, $0F,
    $07, $1E, $AD, $32, $C1, $15, $A9, $1F, $26, $EA, $41, $53, $60, $CC, $35, $A2,
    $08, $3A, $B4, $21, $C1, $48, $06, $D3, $12, $42, $81, $BA, $30, $66, $D4, $24,
    $5A, $44, $33, $DA, $90, $C0, $95, $2C, $38, $44, $2A, $65, $88, $78, $0B, $30,
    $64, $64, $B4, $F0, $A8, $E0, $69, $86, $03, $83, $C1, $F0, $60, $38, $30, $40,
    $08, $09, $86, $1D, $06, $2B, $11, $86, $04, $85, $FD, $CF, $A0, $08, $01, $1B,
    $42, $01, $1C, $C9, $D5, $1F, $9E, $2E, $47, $38, $29, $60, $C1, $48, $84, $42,
    $2F, $06, $FE, $18, $F0, $A9, $9C, $74, $AE, $08, $01, $94, $A0, $04, $BA, $57,
    $1D, $7D, $33, $6A, $64, $83, $52, $0D, $4C, $15, $2F, $87, $BC, $50, $20, $42,
    $84, $08, $05, $FA, $A2, $7F, $53, $84, $62, $A0, $9B, $D6, $BB, $77, $DC, $0D,
    $53, $7D, $CB, $53, $C3, $A9, $86, $8E, $69, $99, $91, $B6, $EE, $C2, $51, $80,
    $A3, $FB, $75, $42, $2A, $0F, $24, $42, $00, $00, $60, $C7, $9F, $5E, $24, $52,
    $F3, $F5, $30, $38, $EC, $D8, $27, $B7, $76, $D1, $2D, $46, $DE, $7D, $82, $3D,
    $DE, $EE, $39, $DD, $5F, $A4, $A7, $0B, $DE, $AE, $F0, $11, $64, $6D, $DB, $9E,
    $B6, $CD, $A3, $AA, $63, $66, $66, $66, $DE, $1C, $A5, $63, $66, $66, $66, $E8,
    $22, $D7, $9E, $ED, $38, $83, $AC, $63, $52, $21, $E3, $B6, $DB, $64, $67, $3F,
    $35, $76, $77, $B8, $D3, $88, $FE, $4F, $00, $FF, $CF, $42, $DA, $AA, $6D, $FA,
    $A7, $A4, $6E, $AB, $F5, $7E, $88, $94, $4C, $D9, $BF, $24, $A1, $FF, $29, $F4,
    $8C, $BC, $7D, $E2, $F2, $60, $92, $7E, $88, $BF, $30, $64, $CC, $E9, $53, $E7,
    $72, $EA, $34, $9F, $DC, $EE, $85, $95, $F2, $D7, $3B, $24, $39, $09, $DC, $3F,
    $D6, $12, $C4, $95, $50, $64, $A4, $8C, $58, $15, $FB, $AA, $68, $8C, $EF, $7F,
    $40, $66, $48, $B8, $5E, $2B, $97, $1C, $63, $6A, $B2, $41, $32, $5E, $F9, $1D,
    $25, $A5, $B4, $36, $C1, $9A, $DF, $08, $20, $A5, $6F, $69, $D9, $31, $6E, $41,
    $69, $6D, $01, $CB, $C4, $40, $FB, $41, $8B, $D7, $A6, $B4, $2E, $78, $EF, $90,
    $90, $F2, $7D, $4F, $E4, $44, $EE, $46, $AF, $4D, $E9, $2A, $58, $46, $7B, $A5,
    $F0, $E0, $B8, $87, $63, $5B, $5C, $A5, $6B, $D1, $4D, $2C, $29, $CF, $F7, $07,
    $5E, $E7, $F4, $11, $ED, $63, $36, $B9, $CA, $62, $19, $55, $F2, $BC, $2B, $00,
    $0B, $CA, $DD, $DA, $54, $4E, $61, $13, $7A, $76, $05, $C1, $BE, $56, $80, $05,
    $E5, $6E, $69, $79, $5B, $F4, $D4, $04, $B0, $BF, $B9, $56, $E9, $D2, $B5, $E8,
    $12, $55, $58, $18, $06, $47, $5F, $52, $95, $BF, $39, $5E, $E9, $AA, $F2, $6A,
    $BD, $EF, $8D, $C7, $0A, $24, $73, $62, $A3, $0B, $FE, $79, $7A, $98, $1A, $05,
    $47, $12, $2C, $A4, $4C, $8B, $8D, $E0, $50, $E8, $9F, $C1, $82, $80, $E5, $61,
    $51, $B9, $66, $18, $DC, $75, $C3, $DD, $CF, $48, $69, $5C, $14, $16, $B9, $1A,
    $86, $33, $91, $A2, $A1, $79, $9B, $57, $28, $78, $EB, $FA, $E8, $5D, $1B, $6D,
    $1D, $44, $1D, $10, $FC, $FC, $E2, $A9, $7B, $A5, $68, $09, $34, $01, $09, $AD,
    $C6, $55, $B5, $70, $19, $CC, $8D, $9D, $F7, $58, $80, $84, $4F, $BA, $A4, $0E,
    $C4, $AB, $7F, $91, $00, $D4, $19, $1D, $38, $DA, $C4, $53, $19, $59, $8A, $6E,
    $60, $69, $B4, $DC, $8A, $8A, $3A, $EE, $7C, $83, $25, $6B, $66, $C5, $68, $27,
    $18, $CB, $9E, $27, $87, $41, $64, $A4, $CC, $8E, $8D, $8E, $A7, $60, $69, $14,
    $70, $34, $2C, $2A, $D7, $F9, $71, $A1, $B2, $F9, $01, $92, $46, $01, $2B, $02,
    $47, $2D, $06, $91, $33, $60, $8B, $A7, $1C, $7F, $D5, $8B, $1D, $8F, $27, $61,
    $C9, $19, $B8, $A1, $4D, $39, $43, $91, $64, $26, $0C, $2F, $69, $55, $AB, $CC,
    $BA, $F8, $F4, $10, $82, $6C, $C9, $47, $57, $EB, $1A, $96, $6C, $CB, $D9, $A8,
    $A6, $8B, $D1, $8C, $BD, $FE, $6B, $64, $8E, $65, $72, $DC, $AD, $8D, $10, $39,
    $92, $F9, $B1, $D1, $5A, $6F, $8C, $FF, $60, $C9, $4D, $98, $17, $1B, $AD, $24,
    $D9, $82, $64, $E7, $23, $47, $6B, $27, $58, $D5, $8C, $C8, $11, $C8, $95, $BE,
    $A3, $5C, $72, $9B, $18, $F4, $D9, $55, $0D, $E3, $B0, $E4, $36, $D1, $FE, $CD,
    $55, $8E, $0F, $00);

  { Lossless, 8 colors (two pixels per byte) }
  LosslessPalette8: array[0..321] of Byte = (
    $52, $49, $46, $46, $3A, $01, $00, $00, $57, $45, $42, $50, $56, $50, $38, $4C,
    $2D, $01, $00, $00, $2F, $24, $00, $07, $00, $3F, $A0, $10, $00, $10, $84, $22,
    $80, $8F, $24, $7F, $9C, $CF, $43, $51, $1B, $49, $88, $94, $63, $F0, $70, $FE,
    $2D, $D1, $EB, $C8, $34, $20, $26, $80, $08, $2E, $F2, $E8, $9F, $C3, $E7, $9B,
    $FF, $A8, $DB, $6C, $F8, $3F, $17, $D3, $14, $C4, $00, $A0, $A8, $02, $B4, $DA,
    $7A, $E3, $46, $B2, $11, $58, $B3, $FD, $4E, $34, $8B, $01, $28, $83, $20, $72,
    $11, $58, $46, $50, $7F, $08, $BA, $F0, $3F, $76, $52, $0A, $11, $FD, $9F, $00,
    $77, $77, $33, $AB, $0E, $C4, $98, $88, $99, $C3, $99, $47, $0C, $34, $D0, $99,
    $3B, $BD, $D6, $1F, $67, $5A, $31, $33, $F3, $9E, $13, $11, $25, $E6, $D0, $DC,
    $4D, $F7, $21, $12, $25, $22, $E6, $00, $94, $6D, $28, $9D, $26, $89, $09, $68,
    $A6, $6A, $66, $77, $8A, $89, $0E, $D0, $4C, $CD, $EA, $8D, $8E, $3B, $80, $62,
    $56, $EC, $7E, $C2, $01, $B4, $E1, $F7, $80, $C9, $1D, $40, $B3, $EA, $5F, $93,
    $C4, $43, $07, $5A, $71, $CF, $13, $66, $A6, $56, $AC, $03, $0E, $64, $0A, $C4,
    $23, $A1, $99, $75, $34, $20, $BF, $F1, $9C, $3A, $7C, $AB, $43, $94, $B7, $13,
    $CD, $F6, $0E, $FC, $89, $C8, $24, $04, $A0, $15, $AD, $68, $79, $11, $79, $1B,
    $38, $00, $28, $6A, $1D, $59, $44, $E4, $C4, $65, $D3, $FA, $42, $73, $9F, $C4,
    $C9, $BA, $72, $80, $99, $6F, $BA, $F7, $67, $9E, $B0, $70, $C4, $B6, $7B, $D1,
    $BD, $3F, $9E, $22, $3C, $48, $C4, $A6, $B5, $A8, $0E, $91, $56, $5E, $D7, $D9,
    $45, $F5, $8A, $A7, $48, $8A, $CC, $07, $ED, $60, $79, $63, $E6, $83, $4D, $2B,
    $1E, $79, $91, $49, $C0, $BF, $5E, $27, $C8, $22, $72, $02, $93, $A7, $C4, $65,
    $02, $00);

  { Lossless, 2 colors (eight pixels per byte) }
  LosslessPalette2: array[0..71] of Byte = (
    $52, $49, $46, $46, $40, $00, $00, $00, $57, $45, $42, $50, $56, $50, $38, $4C,
    $33, $00, $00, $00, $2F, $24, $00, $07, $00, $0F, $F0, $B1, $F4, $9B, $A5, $3F,
    $EB, $7C, $FE, $E3, $01, $6A, $02, $00, $4D, $70, $4E, $22, $10, $85, $FE, $A9,
    $E6, $12, $21, $A2, $FF, $D1, $5A, $01, $B4, $96, $AF, $78, $F9, $3F, $7F, $E1,
    $82, $7F, $BC, $F0, $C5, $57, $1C, $00);

type
  TWebPSample = record
    Name: string;
    Data: Pointer;
    Size: Integer;
    Width, Height: Integer;
    HasAlpha: Boolean;
    Crc: UInt32;  // of B, G, R, A bytes of all pixels
  end;

const
  Samples: array[0..8] of TWebPSample = (
    (Name: 'Lossy'; Data: @Lossy; Size: SizeOf(Lossy); Width: 37; Height: 29; HasAlpha: False; Crc: $CC01C479),
    (Name: 'LossySimpleFilter'; Data: @LossySimpleFilter; Size: SizeOf(LossySimpleFilter); Width: 37; Height: 29; HasAlpha: False; Crc: $B654998C),
    (Name: 'LossySegments'; Data: @LossySegments; Size: SizeOf(LossySegments); Width: 37; Height: 29; HasAlpha: False; Crc: $E1FECBDA),
    (Name: 'LossyAlpha'; Data: @LossyAlpha; Size: SizeOf(LossyAlpha); Width: 37; Height: 29; HasAlpha: True; Crc: $EDA89C9D),
    (Name: 'LossyAlphaRaw'; Data: @LossyAlphaRaw; Size: SizeOf(LossyAlphaRaw); Width: 37; Height: 29; HasAlpha: True; Crc: $EDA89C9D),
    (Name: 'Lossless'; Data: @Lossless; Size: SizeOf(Lossless); Width: 37; Height: 29; HasAlpha: False; Crc: $22E2A12C),
    (Name: 'LosslessAlpha'; Data: @LosslessAlpha; Size: SizeOf(LosslessAlpha); Width: 37; Height: 29; HasAlpha: True; Crc: $0FA2B44B),
    (Name: 'LosslessPalette8'; Data: @LosslessPalette8; Size: SizeOf(LosslessPalette8); Width: 37; Height: 29; HasAlpha: False; Crc: $7FA9A274),
    (Name: 'LosslessPalette2'; Data: @LosslessPalette2; Size: SizeOf(LosslessPalette2); Width: 37; Height: 29; HasAlpha: False; Crc: $B5D09E5B));

  TigersCrc = $205C693B;
  TigersWidth = 641;
  TigersHeight = 472;

function PixelsCrc(const Pixels: TDynByteArray): UInt32;
begin
  Result := $FFFFFFFF;
  CalcCrc32(Result, Pointer(Pixels), Length(Pixels));
  Result := not Result;
end;

function LoadFile(const FileName: string): TDynByteArray;
var
  Stream: TFileStream;
begin
  Result := nil;
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, Stream.Size);
    Stream.ReadBuffer(Pointer(Result)^, Length(Result));
  finally
    Stream.Free;
  end;
end;

function TTestWebPDecoder.DecodeBGRA(Data: Pointer; Size: Integer;
  out Width, Height: Integer): TDynByteArray;
begin
  Result := nil;
  CheckTrue(PasWebPGetInfo(Data, Size, Width, Height), 'PasWebPGetInfo');
  SetLength(Result, Width * Height * 4);
  CheckTrue(PasWebPDecodeInto(Data, Size, Pointer(Result), Width * 4, Width, Height, True),
    'PasWebPDecodeInto');
end;

procedure TTestWebPDecoder.CheckFile(const Name: string; Data: Pointer;
  Size, Width, Height: Integer; Crc: UInt32);
var
  Pixels: TDynByteArray;
  W, H: Integer;
begin
  Pixels := DecodeBGRA(Data, Size, W, H);
  CheckEquals(Width, W, Name + ': width');
  CheckEquals(Height, H, Name + ': height');
  CheckEquals(Int64(Crc), Int64(PixelsCrc(Pixels)), Name + ': CRC of decoded pixels');
end;

procedure TTestWebPDecoder.TestDecode;
var
  I: Integer;
begin
  for I := Low(Samples) to High(Samples) do
    CheckFile(Samples[I].Name, Samples[I].Data, Samples[I].Size, Samples[I].Width,
              Samples[I].Height, Samples[I].Crc);
end;

procedure TTestWebPDecoder.TestDecodeTigers;
var
  Data: TDynByteArray;
begin
  // Larger image: lossy with alpha plane, more than one macroblock row
  Data := LoadFile(GetTestDataFile('Tigers.webp'));
  CheckFile('Tigers.webp', Pointer(Data), Length(Data), TigersWidth, TigersHeight, TigersCrc);
end;

procedure TTestWebPDecoder.TestDecodeWithoutAlpha;
var
  I, J, W, H: Integer;
  WithAlpha, NoAlpha: TDynByteArray;
begin
  for I := Low(Samples) to High(Samples) do
  begin
    WithAlpha := DecodeBGRA(Samples[I].Data, Samples[I].Size, W, H);
    SetLength(NoAlpha, W * H * 3);
    CheckTrue(PasWebPDecodeInto(Samples[I].Data, Samples[I].Size, Pointer(NoAlpha),
                                W * 3, W, H, False), Samples[I].Name + ': decoding to BGR');

    for J := 0 to W * H - 1 do
    begin
      if (NoAlpha[J * 3] <> WithAlpha[J * 4]) or (NoAlpha[J * 3 + 1] <> WithAlpha[J * 4 + 1]) or
         (NoAlpha[J * 3 + 2] <> WithAlpha[J * 4 + 2]) then
      begin
        Fail(Format('%s: BGR pixel %d differs from BGRA one', [Samples[I].Name, J]));
      end;
    end;
  end;
end;

procedure TTestWebPDecoder.TestDecodeWithStride;
const
  Gap = 7;  // bytes between rows the decoder must leave alone
  GapFill = $A5;
var
  I, Y, X, W, H, Bpp, Stride: Integer;
  WithAlpha: Boolean;
  Tight, Padded: TDynByteArray;
begin
  for I := Low(Samples) to High(Samples) do
    for WithAlpha := False to True do
    begin
      CheckTrue(PasWebPGetInfo(Samples[I].Data, Samples[I].Size, W, H), 'PasWebPGetInfo');
      if WithAlpha then Bpp := 4 else Bpp := 3;
      Stride := W * Bpp + Gap;
      SetLength(Tight, W * H * Bpp);
      SetLength(Padded, Stride * H);
      FillChar(Pointer(Padded)^, Length(Padded), GapFill);

      CheckTrue(PasWebPDecodeInto(Samples[I].Data, Samples[I].Size, Pointer(Tight),
                                  W * Bpp, W, H, WithAlpha), Samples[I].Name + ': rows without gaps');
      CheckTrue(PasWebPDecodeInto(Samples[I].Data, Samples[I].Size, Pointer(Padded),
                                  Stride, W, H, WithAlpha), Samples[I].Name + ': rows with gaps');

      for Y := 0 to H - 1 do
      begin
        CheckSameMem(@Tight[Y * W * Bpp], @Padded[Y * Stride], W * Bpp,
          Format('%s: row %d, %d bytes per pixel', [Samples[I].Name, Y, Bpp]));

        for X := 0 to Gap - 1 do
          if Padded[Y * Stride + W * Bpp + X] <> GapFill then
            Fail(Format('%s: gap after row %d was written to', [Samples[I].Name, Y]));
      end;
    end;
end;

procedure TTestWebPDecoder.TestWrongSize;
var
  I: Integer;
  Pixels: TDynByteArray;
begin
  // Size given by the caller must be the size of the image, the decoder
  // must not write an image of another size to the buffer
  for I := Low(Samples) to High(Samples) do
  begin
    SetLength(Pixels, Samples[I].Width * Samples[I].Height * 4);
    CheckFalse(PasWebPDecodeInto(Samples[I].Data, Samples[I].Size, Pointer(Pixels),
                                 (Samples[I].Width - 1) * 4, Samples[I].Width - 1,
                                 Samples[I].Height, True), Samples[I].Name + ': smaller width');
    CheckFalse(PasWebPDecodeInto(Samples[I].Data, Samples[I].Size, Pointer(Pixels),
                                 Samples[I].Width * 4, Samples[I].Width,
                                 Samples[I].Height - 1, True), Samples[I].Name + ': smaller height');
  end;
end;

procedure TTestWebPDecoder.TestCutData;
var
  I, Size, W, H: Integer;
  Lossless, Ok: Boolean;
  Data, Pixels: TDynByteArray;
begin
  // Every shorter length of every sample: no crash. The copy has just
  // the bytes given, so reads beyond them are beyond a heap block.
  for I := Low(Samples) to High(Samples) do
  begin
    Lossless := Pos('Lossless', Samples[I].Name) = 1;
    SetLength(Pixels, Samples[I].Width * Samples[I].Height * 4);

    for Size := 0 to Samples[I].Size - 1 do
    begin
      Data := nil;
      SetLength(Data, Size);
      if Size > 0 then
        Move(Samples[I].Data^, Pointer(Data)^, Size);

      Ok := PasWebPGetInfo(Pointer(Data), Size, W, H) and (W = Samples[I].Width) and
            (H = Samples[I].Height) and
            PasWebPDecodeInto(Pointer(Data), Size, Pointer(Pixels), W * 4, W, H, True);

      // Lossless data that ends early is an error (cut lossy image can be
      // decoded: the missing end reads as zeros, as in libwebp).
      // The last byte of a file can be the padding of the chunk.
      if Lossless and (Size < Samples[I].Size - 1) then
        CheckFalse(Ok, Format('%s: cut to %d bytes', [Samples[I].Name, Size]));
    end;
  end;
end;

procedure TTestWebPDecoder.TestChangedData;
const
  VariantsPerSample = 400;
var
  I, J, K, W, H: Integer;
  Seed: UInt32;
  Data, Pixels: TDynByteArray;

  function Next(Range: Integer): Integer;
  begin
    // Linear congruential generator, kept inside 32 bits
    Seed := UInt32((UInt64(Seed) * 1664525 + 1013904223) and $FFFFFFFF);
    Result := Integer((Seed shr 8) mod UInt32(Range));
  end;

begin
  // Damaged samples: decoding may fail or give garbage, it must not crash
  // or write outside of the buffer (heap check of the test build).
  Seed := 20261110;

  for I := Low(Samples) to High(Samples) do
    for J := 1 to VariantsPerSample do
    begin
      Data := nil;
      SetLength(Data, Samples[I].Size);
      Move(Samples[I].Data^, Pointer(Data)^, Samples[I].Size);

      for K := 0 to Next(4) do
        Data[Next(Samples[I].Size)] := Next(256);

      // Changed header can give any size
      if PasWebPGetInfo(Pointer(Data), Length(Data), W, H) and (W > 0) and (H > 0) and
         (W <= 1024) and (H <= 1024) then
      begin
        Pixels := nil;
        SetLength(Pixels, W * H * 4);
        PasWebPDecodeInto(Pointer(Data), Length(Data), Pointer(Pixels), W * 4, W, H, True);
        PasWebPDecodeInto(Pointer(Data), Length(Data), Pointer(Pixels), W * 3, W, H, False);
      end;
    end;
end;

procedure TTestWebPDecoder.TestSameAsLibWebP;
{$IFDEF TEST_PASWEBP_WITH_LIBWEBP}
var
  I, W, H: Integer;
  Data, ByPascal, ByLib: TDynByteArray;

  procedure Compare(const Name: string; FileData: Pointer; FileSize: Integer);
  begin
    ByPascal := DecodeBGRA(FileData, FileSize, W, H);
    ByLib := nil;
    SetLength(ByLib, W * H * 4);
    CheckTrue(WebPDecodeBGRAInto(FileData, FileSize, Pointer(ByLib), Length(ByLib), W * 4) <> nil,
      Name + ': decoding by libwebp');
    CheckSameMem(Pointer(ByLib), Pointer(ByPascal), Length(ByLib),
      Name + ': pixels by libwebp and by Pascal decoder');
  end;

begin
  // Only where libwebp is installed (CI: Linux)
  if not LoadLibWebP then
    Exit;

  for I := Low(Samples) to High(Samples) do
    Compare(Samples[I].Name, Samples[I].Data, Samples[I].Size);

  Data := LoadFile(GetTestDataFile('Tigers.webp'));
  Compare('Tigers.webp', Pointer(Data), Length(Data));
end;
{$ELSE}
begin
  // Built without the binding of libwebp
end;
{$ENDIF}

const
  // Tigers.jpg by the lossy encoder at quality 75: now 16 520 bytes, 38.4 dB
  DemoPhotoMaxSize = 18000;
  DemoPhotoMinPsnr: Double = 37.5;

{ TTestWebPLossless }

type
  TTestContent = (
    tcPhoto,     // smooth gradients with some texture, as photographs have
    tcDrawing,   // flat areas, lines and a repeated pattern
    tcNoise,     // every pixel different
    tcFlat,      // one color
    tcSmooth);   // slow changes of all channels, a little texture

{ Deterministic test images: B, G, R, A bytes. Alpha is 255 unless
  WithAlpha, then it changes over the image. }
function MakePixels(Content: TTestContent; Width, Height: Integer; WithAlpha: Boolean): TDynByteArray;
var
  X, Y, Bpp, R, G, B, A: Integer;
  State: UInt32;
  P: PByte;

  function Next: Integer;
  begin
    State := UInt32((UInt64(State) * 1664525 + 1013904223) and $FFFFFFFF);
    Result := Integer(State shr 24);
  end;

begin
  Result := nil;
  if WithAlpha then Bpp := 4 else Bpp := 3;
  SetLength(Result, Width * Height * Bpp);
  State := UInt32(Width * 7919 + Height * 104729 + Ord(Content));
  P := Pointer(Result);

  for Y := 0 to Height - 1 do
    for X := 0 to Width - 1 do
    begin
      A := 255;

      case Content of
        tcPhoto:
          begin
            R := (X * 3 + Y) and 255;
            G := (X + Y * 2 + (Next and 3)) and 255;
            B := ((X * X + Y * Y) shr 4 + (Next and 1)) and 255;
            if WithAlpha then A := (X * 5 + Y * 3) and 255;
          end;
        tcDrawing:
          begin
            if (X mod 16 = 0) or (Y mod 11 = 0) then
            begin
              R := 0; G := 0; B := 0;
            end
            else if ((X div 8) + (Y div 8)) mod 3 = 0 then
            begin
              R := 240; G := 200; B := 40;
            end
            else
            begin
              R := 30 + (X mod 4) * 20; G := 90; B := 200 - (Y mod 4) * 10;
            end;
            if WithAlpha and (X div 8 mod 4 = 1) then A := 0;
          end;
        tcNoise:
          begin
            R := Next; G := Next; B := Next;
            if WithAlpha then A := Next;
          end;
        tcSmooth:
          begin
            R := 40 + X * 150 div Width + (Next and 1);
            G := 60 + Y * 120 div Height + (Next and 3);
            B := 90 + Abs((X + Y) mod 160 - 80);
            if WithAlpha then A := 255 - Y * 200 div Height;
          end;
      else
        R := 17; G := 130; B := 201;
        if WithAlpha then A := 77;
      end;

      P^ := B; Inc(P);
      P^ := G; Inc(P);
      P^ := R; Inc(P);
      if WithAlpha then
      begin
        P^ := A;
        Inc(P);
      end;
    end;
end;

function TTestWebPLossless.CheckRoundTrip(const Name: string; const Pixels: TDynByteArray;
  Width, Height: Integer; WithAlpha: Boolean; Effort: Integer): Integer;
var
  Data: TPasWebPBytes;
  Decoded: TDynByteArray;
  W, H, Bpp: Integer;
  Msg: string;
begin
  Msg := Format('%s %dx%d, alpha %d, effort %d: ', [Name, Width, Height, Ord(WithAlpha), Effort]);
  if WithAlpha then Bpp := 4 else Bpp := 3;

  CheckTrue(PasWebPEncodeLossless(Pointer(Pixels), Width * Bpp, Width, Height, WithAlpha,
                                  Effort, Data), Msg + 'encoding');
  Result := Length(Data);
  CheckTrue(Result > 0, Msg + 'encoded data');
  CheckEquals(0, Result and 1, Msg + 'even size of the file');
  // Size in the RIFF header
  CheckEquals(Result - 8, Integer(Data[4]) or (Integer(Data[5]) shl 8) or (Integer(Data[6]) shl 16) or
                          (Integer(Data[7]) shl 24), Msg + 'size in RIFF header');

  CheckTrue(PasWebPGetInfo(Pointer(Data), Length(Data), W, H), Msg + 'PasWebPGetInfo');
  CheckEquals(Width, W, Msg + 'width');
  CheckEquals(Height, H, Msg + 'height');
  Decoded := nil;
  SetLength(Decoded, Length(Pixels));
  CheckTrue(PasWebPDecodeInto(Pointer(Data), Length(Data), Pointer(Decoded), Width * Bpp,
                              Width, Height, WithAlpha), Msg + 'decoding');
  CheckSameMem(Pointer(Pixels), Pointer(Decoded), Length(Pixels), Msg + 'decoded pixels');
end;

procedure TTestWebPLossless.TestSizes;
const
  Sizes: array[0..11, 0..1] of Integer = ((1, 1), (2, 1), (1, 2), (1, 9), (9, 1), (2, 2), (3, 5),
    (16, 16), (17, 33), (33, 17), (64, 5), (100, 75));
var
  I: Integer;
  Content: TTestContent;
  WithAlpha: Boolean;
begin
  // Sizes around the sizes of tiles, single rows and columns
  for I := Low(Sizes) to High(Sizes) do
    for Content := Low(TTestContent) to High(TTestContent) do
      for WithAlpha := False to True do
        CheckRoundTrip('content ' + IntToStr(Ord(Content)),
                       MakePixels(Content, Sizes[I, 0], Sizes[I, 1], WithAlpha),
                       Sizes[I, 0], Sizes[I, 1], WithAlpha, PasWebPDefaultEffort);
end;

procedure TTestWebPLossless.TestEfforts;
var
  Effort: Integer;
  Content: TTestContent;
  WithAlpha: Boolean;
begin
  // Efforts outside of the range are taken as the nearest one
  for Effort := PasWebPMinEffort - 1 to PasWebPMaxEffort + 1 do
    for Content := Low(TTestContent) to High(TTestContent) do
      for WithAlpha := False to True do
        CheckRoundTrip('content ' + IntToStr(Ord(Content)), MakePixels(Content, 150, 93, WithAlpha),
                       150, 93, WithAlpha, Effort);
end;

procedure TTestWebPLossless.TestFewColors;
const
  Counts: array[0..10] of Integer = (1, 2, 3, 4, 5, 16, 17, 100, 255, 256, 257);
  Width = 67;   // not a multiple of the pixels packed to one
  Height = 41;
var
  I, J, Effort: Integer;
  Pixels: TDynByteArray;
  Color: UInt32;
begin
  // Palette with 8, 4, 2 and 1 pixel in a byte, and the first size without it
  SetLength(Pixels, Width * Height * 4);

  for I := Low(Counts) to High(Counts) do
  begin
    for J := 0 to Width * Height - 1 do
    begin
      // Runs and scattered pixels of colors that differ in all channels
      Color := UInt32((J div 5 + (J mod Width) * (J div Width)) mod Counts[I]);
      Pixels[J * 4] := (Color * 37) and $FF;
      Pixels[J * 4 + 1] := (Color xor $FF) and $FF;
      Pixels[J * 4 + 2] := (Color * 101) and $FF;
      Pixels[J * 4 + 3] := 255 - (Color and 3) * 60;
    end;

    for Effort := 0 to 2 do
      CheckRoundTrip(IntToStr(Counts[I]) + ' colors', Pixels, Width, Height, True, Effort * 4);
  end;
end;

procedure TTestWebPLossless.TestRunsAndNoise;
var
  Pixels: TDynByteArray;
  Size, I: Integer;
begin
  // Runs longer than the longest copy (4096 pixels)
  Pixels := MakePixels(tcFlat, 300, 200, False);
  Size := CheckRoundTrip('flat', Pixels, 300, 200, False, PasWebPDefaultEffort);
  CheckTrue(Size < 200, 'Flat image of 60000 pixels takes ' + IntToStr(Size) + ' bytes');

  // The same with single different pixels far from each other
  I := 5000 * 3;
  while I < Length(Pixels) do
  begin
    Pixels[I] := Pixels[I] xor $55;
    Inc(I, 7001 * 3);
  end;
  CheckRoundTrip('flat with dots', Pixels, 300, 200, False, PasWebPDefaultEffort);

  // Rows repeated from far above
  Pixels := MakePixels(tcNoise, 64, 200, True);
  for I := 100 * 64 * 4 to Length(Pixels) - 1 do
    Pixels[I] := Pixels[I - 97 * 64 * 4];
  Size := CheckRoundTrip('repeated noise', Pixels, 64, 200, True, PasWebPDefaultEffort);
  CheckTrue(Size < 64 * 105 * 4, 'Repeated rows are found: ' + IntToStr(Size) + ' bytes');

  // Noise cannot be made smaller, it must not get much larger either
  Pixels := MakePixels(tcNoise, 128, 128, False);
  Size := CheckRoundTrip('noise', Pixels, 128, 128, False, PasWebPDefaultEffort);
  CheckTrue(Size < 128 * 128 * 3 + 1024, 'Noise takes ' + IntToStr(Size) + ' bytes');
end;

procedure TTestWebPLossless.TestInvisiblePixelsKept;
var
  Pixels: TDynByteArray;
  I: Integer;
begin
  // Colors of transparent pixels are data as any other
  Pixels := MakePixels(tcPhoto, 40, 30, True);
  for I := 0 to 40 * 30 - 1 do
    if I mod 3 <> 0 then
      Pixels[I * 4 + 3] := 0;
  CheckRoundTrip('alpha 0', Pixels, 40, 30, True, PasWebPDefaultEffort);

  // Fully transparent image
  for I := 0 to 40 * 30 - 1 do
    Pixels[I * 4 + 3] := 0;
  CheckRoundTrip('all alpha 0', Pixels, 40, 30, True, PasWebPDefaultEffort);
end;

procedure TTestWebPLossless.TestStride;
const
  Width = 45;
  Height = 31;
  Gap = 5;
var
  Tight, Padded: TDynByteArray;
  FromTight, FromPadded: TPasWebPBytes;
  WithAlpha: Boolean;
  Y, Bpp: Integer;
begin
  for WithAlpha := False to True do
  begin
    if WithAlpha then Bpp := 4 else Bpp := 3;
    Tight := MakePixels(tcPhoto, Width, Height, WithAlpha);
    Padded := nil;
    SetLength(Padded, (Width * Bpp + Gap) * Height);
    FillChar(Pointer(Padded)^, Length(Padded), $EE);
    for Y := 0 to Height - 1 do
      Move(Tight[Y * Width * Bpp], Padded[Y * (Width * Bpp + Gap)], Width * Bpp);

    CheckTrue(PasWebPEncodeLossless(Pointer(Tight), Width * Bpp, Width, Height, WithAlpha,
                                    PasWebPDefaultEffort, FromTight), 'Rows without gaps');
    CheckTrue(PasWebPEncodeLossless(Pointer(Padded), Width * Bpp + Gap, Width, Height, WithAlpha,
                                    PasWebPDefaultEffort, FromPadded), 'Rows with gaps');
    CheckEquals(Length(FromTight), Length(FromPadded), 'Size of files of the same pixels');
    CheckSameMem(Pointer(FromTight), Pointer(FromPadded), Length(FromTight), 'Files of the same pixels');
  end;
end;

procedure TTestWebPLossless.TestWrongParameters;
var
  Pixels: TDynByteArray;
  Data: TPasWebPBytes;
begin
  Pixels := MakePixels(tcFlat, 4, 4, True);
  CheckFalse(PasWebPEncodeLossless(nil, 16, 4, 4, True, 6, Data), 'No pixels');
  CheckFalse(PasWebPEncodeLossless(Pointer(Pixels), 16, 0, 4, True, 6, Data), 'Zero width');
  CheckFalse(PasWebPEncodeLossless(Pointer(Pixels), 16, 4, 0, True, 6, Data), 'Zero height');
  CheckFalse(PasWebPEncodeLossless(Pointer(Pixels), 16, PasWebPMaxSize + 1, 1, True, 6, Data),
    'Width over the limit of WebP');
  CheckFalse(PasWebPEncodeLossless(Pointer(Pixels), 16, 1, PasWebPMaxSize + 1, True, 6, Data),
    'Height over the limit of WebP');
  CheckEquals(0, Length(Data), 'No data when encoding fails');
end;

procedure TTestWebPLossless.TestWidestImage;
begin
  // The largest width and height WebP can have
  CheckRoundTrip('widest', MakePixels(tcPhoto, PasWebPMaxSize, 3, False), PasWebPMaxSize, 3, False, 2);
  CheckRoundTrip('tallest', MakePixels(tcDrawing, 3, PasWebPMaxSize, True), 3, PasWebPMaxSize, True, 2);
end;

procedure TTestWebPLossless.TestAlphaPlane;
const
  ChunkHeaderSize = 8;
var
  Sample: PByte;
  SampleSize, W, H, Pos, ChunkSize, AlphPos, AlphEnd, I, Effort: Integer;
  Plane, FileData, Pixels: TDynByteArray;
  Stream: TPasWebPBytes;

  function ReadUInt32(Offset: Integer): Integer;
  begin
    Result := Integer(Sample[Offset]) or (Integer(Sample[Offset + 1]) shl 8) or
              (Integer(Sample[Offset + 2]) shl 16) or (Integer(Sample[Offset + 3]) shl 24);
  end;

  procedure WriteUInt32(Offset, Value: Integer);
  begin
    FileData[Offset] := Byte(Value);
    FileData[Offset + 1] := Byte(Value shr 8);
    FileData[Offset + 2] := Byte(Value shr 16);
    FileData[Offset + 3] := Byte(Value shr 24);
  end;

begin
  // Alpha plane of a lossy image is a lossless stream without the header.
  // The one of the sample with raw alpha is replaced by an encoded plane:
  // the decoder must give its values as alpha.
  Sample := Samples[4].Data;
  SampleSize := Samples[4].Size;
  CheckEquals('LossyAlphaRaw', Samples[4].Name, 'Sample with ALPH chunk');
  W := Samples[4].Width;
  H := Samples[4].Height;

  AlphPos := -1;
  AlphEnd := 0;
  Pos := 12;
  while Pos + ChunkHeaderSize <= SampleSize do
  begin
    ChunkSize := ReadUInt32(Pos + 4);
    if (Sample[Pos] = Ord('A')) and (Sample[Pos + 1] = Ord('L')) and (Sample[Pos + 2] = Ord('P')) and
       (Sample[Pos + 3] = Ord('H')) then
    begin
      AlphPos := Pos;
      AlphEnd := Pos + ChunkHeaderSize + ChunkSize + (ChunkSize and 1);
    end;
    Inc(Pos, ChunkHeaderSize + ChunkSize + (ChunkSize and 1));
  end;
  CheckTrue(AlphPos > 0, 'ALPH chunk of the sample');

  SetLength(Plane, W * H);
  for I := 0 to W * H - 1 do
    Plane[I] := ((I mod W) * 6 + (I div W) * 2) and $FF;

  for Effort := 0 to 2 do
  begin
    CheckTrue(PasWebPEncodePlane(Pointer(Plane), W, W, H, Effort * 4, Stream), 'Encoding of a plane');

    // Chunk data: byte with compression method 1 (lossless) and no filter,
    // then the stream
    ChunkSize := 1 + Length(Stream);
    FileData := nil;
    SetLength(FileData, AlphPos + ChunkHeaderSize + ChunkSize + (ChunkSize and 1) + SampleSize - AlphEnd);
    Move(Sample^, FileData[0], AlphPos + 4);
    WriteUInt32(AlphPos + 4, ChunkSize);
    FileData[AlphPos + ChunkHeaderSize] := 1;
    Move(Stream[0], FileData[AlphPos + ChunkHeaderSize + 1], Length(Stream));
    Move(Sample[AlphEnd], FileData[AlphPos + ChunkHeaderSize + ChunkSize + (ChunkSize and 1)],
         SampleSize - AlphEnd);
    WriteUInt32(4, Length(FileData) - 8);

    Pixels := nil;
    SetLength(Pixels, W * H * 4);
    CheckTrue(PasWebPDecodeInto(Pointer(FileData), Length(FileData), Pointer(Pixels), W * 4, W, H, True),
      'Decoding of the image with the new alpha plane');
    for I := 0 to W * H - 1 do
      if Pixels[I * 4 + 3] <> Plane[I] then
        Fail(Format('Alpha of pixel %d is %d, plane has %d (effort %d)',
                    [I, Pixels[I * 4 + 3], Plane[I], Effort * 4]));
  end;
end;

procedure TTestWebPLossless.TestSizeOfDemoImages;

  procedure CheckImage(const FileName: string; MaxSize: Integer);
  var
    Image: TImageData;
    Info: TImageFormatInfo;
    Pixels: TDynByteArray;
    WithAlpha: Boolean;
    Size: Integer;
  begin
    InitImage(Image);
    CheckTrue(LoadImageFromFile(GetTestDataFile(FileName), Image), 'Loading of ' + FileName);
    try
      GetImageFormatInfo(Image.Format, Info);
      WithAlpha := Info.HasAlphaChannel;
      if WithAlpha then
        CheckTrue(ConvertImage(Image, ifA8R8G8B8), 'Conversion')
      else
        CheckTrue(ConvertImage(Image, ifR8G8B8), 'Conversion');
      SetLength(Pixels, Image.Size);
      Move(Image.Bits^, Pointer(Pixels)^, Image.Size);
      Size := CheckRoundTrip(FileName, Pixels, Image.Width, Image.Height, WithAlpha,
                             PasWebPDefaultEffort);
      CheckTrue(Size <= MaxSize, Format('%s takes %d bytes, limit is %d', [FileName, Size, MaxSize]));
    finally
      FreeImage(Image);
    end;
  end;

begin
  // Limits are a few percent over what the encoder gives now (and that is
  // about what cwebp -lossless gives): to notice when it gets worse.
  // The files are PNG of 117844, 32902 and 32027 bytes.
  CheckImage('Tigers.png', 52000);     // 32 colors: 49622
  CheckImage('LogoAlpha.png', 20500);  // 19256
  CheckImage('Logo.png', 20500);       // 19204
end;

procedure TTestWebPLossless.TestImagingSaveLoad;
var
  Previous: TWebPBackend;
  Image, Loaded: TImageData;
  Stream: TMemoryStream;
  Pixels: TDynByteArray;
begin
  // Through the file format class with the Pascal codec, also where
  // libwebp is found
  Previous := WebPBackend;
  CheckTrue(SelectWebPBackend(wbPascal), 'Pascal WebP codec is available');
  InitImage(Image);
  InitImage(Loaded);
  Stream := TMemoryStream.Create;
  PushOptions;
  try
    CheckTrue(SetOption(ImagingWebPLossless, 1), 'ImagingWebPLossless');
    CheckTrue(SetOption(ImagingWebPEffort, 3), 'ImagingWebPEffort');
    CheckEquals(3, GetOption(ImagingWebPEffort), 'ImagingWebPEffort value');

    Pixels := MakePixels(tcPhoto, 90, 70, True);
    CheckTrue(NewImage(90, 70, ifA8R8G8B8, Image), 'NewImage');
    Move(Pointer(Pixels)^, Image.Bits^, Image.Size);
    CheckTrue(SaveImageToStream('webp', Stream, Image), 'Saving');
    Stream.Position := 0;
    CheckTrue(LoadImageFromStream(Stream, Loaded), 'Loading');
    CheckImagesEqual(Image, Loaded, 'Image saved and loaded as lossless WebP');

    // An image in other format is converted, without alpha when it has none
    FreeImage(Loaded);
    CheckTrue(ConvertImage(Image, ifR5G6B5), 'Conversion');
    Stream.Clear;
    CheckTrue(SaveImageToStream('webp', Stream, Image), 'Saving of R5G6B5');
    Stream.Position := 0;
    CheckTrue(LoadImageFromStream(Stream, Loaded), 'Loading');
    CheckTrue(Loaded.Format = ifR8G8B8, 'Image without alpha is loaded as R8G8B8');
    CheckTrue(ConvertImage(Image, ifR8G8B8), 'Conversion');
    CheckImagesEqual(Image, Loaded, 'R5G6B5 image saved and loaded as lossless WebP');
  finally
    PopOptions;
    SelectWebPBackend(Previous);
    Stream.Free;
    FreeImage(Image);
    FreeImage(Loaded);
  end;
end;

procedure TTestWebPLossless.TestDecodedByLibWebP;
{$IFDEF TEST_PASWEBP_WITH_LIBWEBP}
var
  Content: TTestContent;
  WithAlpha: Boolean;
  Effort, Bpp: Integer;
  Pixels, ByLib: TDynByteArray;
  Data: TPasWebPBytes;
  Ok: Boolean;
  Msg: string;
begin
  // Only where libwebp is installed (CI: Linux)
  if not LoadLibWebP then
    Exit;

  for Content := Low(TTestContent) to High(TTestContent) do
    for WithAlpha := False to True do
      for Effort := 0 to 3 do
      begin
        Msg := Format('content %d, alpha %d, effort %d: ', [Ord(Content), Ord(WithAlpha), Effort * 3]);
        if WithAlpha then Bpp := 4 else Bpp := 3;
        Pixels := MakePixels(Content, 131, 77, WithAlpha);
        CheckTrue(PasWebPEncodeLossless(Pointer(Pixels), 131 * Bpp, 131, 77, WithAlpha,
                                        Effort * 3, Data), Msg + 'encoding');
        ByLib := nil;
        SetLength(ByLib, Length(Pixels));
        if WithAlpha then
          Ok := WebPDecodeBGRAInto(Pointer(Data), Length(Data), Pointer(ByLib), Length(ByLib), 131 * 4) <> nil
        else
          Ok := WebPDecodeBGRInto(Pointer(Data), Length(Data), Pointer(ByLib), Length(ByLib), 131 * 3) <> nil;
        CheckTrue(Ok, Msg + 'decoding by libwebp');
        CheckSameMem(Pointer(Pixels), Pointer(ByLib), Length(Pixels), Msg + 'pixels decoded by libwebp');
      end;
end;
{$ELSE}
begin
  // Built without the binding of libwebp
end;
{$ENDIF}

{ TTestWebPLossy }

function TTestWebPLossy.EncodeAndDecode(const Name: string; const Pixels: TDynByteArray;
  Width, Height: Integer; WithAlpha: Boolean; Quality, Effort: Integer;
  out FileSize: Integer): Double;
const
  MaxPsnr: Double = 99.0;
  Peak: Double = 255.0 * 255.0;
var
  Data: TPasWebPBytes;
  Decoded: TDynByteArray;
  W, H, Bpp, I, C, D, Count: Integer;
  Sum: Double;
  Msg: string;
begin
  Msg := Format('%s %dx%d, alpha %d, quality %d, effort %d: ',
                [Name, Width, Height, Ord(WithAlpha), Quality, Effort]);
  if WithAlpha then Bpp := 4 else Bpp := 3;

  CheckTrue(PasWebPEncodeLossy(Pointer(Pixels), Width * Bpp, Width, Height, WithAlpha,
                               Quality, Effort, Data), Msg + 'encoding');
  FileSize := Length(Data);
  CheckTrue(FileSize > 20, Msg + 'encoded data');
  CheckEquals(0, FileSize and 1, Msg + 'even size of the file');
  CheckEquals(FileSize - 8, Integer(Data[4]) or (Integer(Data[5]) shl 8) or (Integer(Data[6]) shl 16) or
                            (Integer(Data[7]) shl 24), Msg + 'size in RIFF header');

  CheckTrue(PasWebPGetInfo(Pointer(Data), Length(Data), W, H), Msg + 'PasWebPGetInfo');
  CheckEquals(Width, W, Msg + 'width');
  CheckEquals(Height, H, Msg + 'height');
  Decoded := nil;
  SetLength(Decoded, Length(Pixels));
  CheckTrue(PasWebPDecodeInto(Pointer(Data), Length(Data), Pointer(Decoded), Width * Bpp,
                              Width, Height, WithAlpha), Msg + 'decoding');

  Sum := 0;
  Count := 0;

  for I := 0 to Width * Height - 1 do
  begin
    if WithAlpha then
    begin
      if Decoded[I * 4 + 3] <> Pixels[I * 4 + 3] then
        Fail(Format('%salpha of pixel %d is %d, was %d', [Msg, I, Decoded[I * 4 + 3], Pixels[I * 4 + 3]]));
      // Colors of invisible pixels are not kept
      if Pixels[I * 4 + 3] = 0 then
        Continue;
    end;

    for C := 0 to 2 do
    begin
      D := Integer(Decoded[I * Bpp + C]) - Integer(Pixels[I * Bpp + C]);
      Sum := Sum + D * D;
    end;
    Inc(Count, 3);
  end;

  if (Sum = 0) or (Count = 0) then
    Result := MaxPsnr
  else
    Result := 10 * Ln(Peak * Count / Sum) / Ln(10);
end;

procedure TTestWebPLossy.TestSizes;
const
  Sizes: array[0..10, 0..1] of Integer = ((1, 1), (2, 1), (1, 2), (1, 9), (9, 1), (15, 17),
    (16, 16), (17, 15), (33, 17), (48, 32), (100, 75));
var
  I, FileSize: Integer;
  Content: TTestContent;
  WithAlpha: Boolean;
begin
  // Around the size of macroblocks, single rows and columns
  for I := Low(Sizes) to High(Sizes) do
    for Content := Low(TTestContent) to High(TTestContent) do
      for WithAlpha := False to True do
        EncodeAndDecode('content ' + IntToStr(Ord(Content)),
                        MakePixels(Content, Sizes[I, 0], Sizes[I, 1], WithAlpha),
                        Sizes[I, 0], Sizes[I, 1], WithAlpha, 75, PasWebPDefaultEffort, FileSize);
end;

procedure TTestWebPLossy.TestQualitiesAndEfforts;
const
  Qualities: array[0..5] of Integer = (-5, 0, 30, 75, 100, 130);
var
  Q, Effort, FileSize: Integer;
  Content: TTestContent;
  WithAlpha: Boolean;
  Psnr: Double;
begin
  // Values outside of the ranges are taken as the nearest ones
  for Q := Low(Qualities) to High(Qualities) do
    for Effort := PasWebPMinEffort - 1 to PasWebPMaxEffort + 1 do
      for Content := Low(TTestContent) to High(TTestContent) do
        for WithAlpha := False to True do
        begin
          Psnr := EncodeAndDecode('content ' + IntToStr(Ord(Content)),
                                  MakePixels(Content, 70, 45, WithAlpha), 70, 45, WithAlpha,
                                  Qualities[Q], Effort, FileSize);
          // Flat image is easy at any quality but the lowest ones
          if (Content = tcFlat) and (Qualities[Q] >= 30) then
            CheckTrue(Psnr > 40, Format('Flat image at quality %d, effort %d: PSNR %.1f dB',
                                        [Qualities[Q], Effort, Psnr]));
        end;
end;

procedure TTestWebPLossy.TestQualityOrder;
const
  Qualities: array[0..4] of Integer = (10, 40, 70, 90, 100);
  // Measured 38.3, 41.0, 42.3, 44.7 and 46.3 dB
  MinPsnr: array[0..4] of Double = (36.5, 39.5, 41.0, 43.5, 45.0);
var
  I, FileSize, PrevSize: Integer;
  Pixels: TDynByteArray;
  Psnr, PrevPsnr: Double;
begin
  // Higher quality: larger file, closer to the source
  Pixels := MakePixels(tcSmooth, 240, 160, False);
  PrevSize := 0;
  PrevPsnr := 0;

  for I := Low(Qualities) to High(Qualities) do
  begin
    Psnr := EncodeAndDecode('photo', Pixels, 240, 160, False, Qualities[I], PasWebPDefaultEffort, FileSize);
    CheckTrue(FileSize > PrevSize, Format('Quality %d: %d bytes, lower quality had %d',
                                          [Qualities[I], FileSize, PrevSize]));
    CheckTrue(Psnr > PrevPsnr, Format('Quality %d: PSNR %.2f dB, lower quality had %.2f',
                                      [Qualities[I], Psnr, PrevPsnr]));
    CheckTrue(Psnr >= MinPsnr[I], Format('Quality %d: PSNR %.2f dB, at least %.1f expected',
                                         [Qualities[I], Psnr, MinPsnr[I]]));
    PrevSize := FileSize;
    PrevPsnr := Psnr;
  end;

  // The file is much smaller than the pixels
  Psnr := EncodeAndDecode('photo', Pixels, 240, 160, False, 75, PasWebPDefaultEffort, FileSize);
  CheckTrue(FileSize < 240 * 160 * 3 div 4, 'Photo at quality 75 takes ' + IntToStr(FileSize) + ' bytes');
end;

procedure TTestWebPLossy.TestOpaqueAlphaIsNotStored;
var
  Pixels: TDynByteArray;
  Data: TPasWebPBytes;
  I: Integer;
begin
  // Simple file with the frame alone
  Pixels := MakePixels(tcPhoto, 50, 40, True);
  for I := 0 to 50 * 40 - 1 do
    Pixels[I * 4 + 3] := 255;
  CheckTrue(PasWebPEncodeLossy(Pointer(Pixels), 200, 50, 40, True, 75, 6, Data), 'Encoding');
  CheckTrue((Data[12] = Ord('V')) and (Data[13] = Ord('P')) and (Data[14] = Ord('8')) and
            (Data[15] = Ord(' ')), 'File of an opaque image starts with VP8 chunk');

  // Extended file with alpha chunk
  Pixels[7] := 254;
  CheckTrue(PasWebPEncodeLossy(Pointer(Pixels), 200, 50, 40, True, 75, 6, Data), 'Encoding');
  CheckTrue((Data[12] = Ord('V')) and (Data[13] = Ord('P')) and (Data[14] = Ord('8')) and
            (Data[15] = Ord('X')), 'File of an image with alpha starts with VP8X chunk');
  CheckEquals($10, Data[20], 'Alpha flag of VP8X chunk');
  CheckEquals(49, Integer(Data[24]) or (Integer(Data[25]) shl 8), 'Width - 1 in VP8X chunk');
  CheckEquals(39, Integer(Data[27]) or (Integer(Data[28]) shl 8), 'Height - 1 in VP8X chunk');
end;

procedure TTestWebPLossy.TestInvisiblePixels;
var
  Pixels: TDynByteArray;
  I, X, Y, FileSize, SizeWithNoise: Integer;
  Psnr: Double;
begin
  // Noise in fully transparent pixels must cost nearly nothing and must
  // not spoil the visible ones: left half visible photo, right half noise
  Pixels := MakePixels(tcSmooth, 96, 64, True);
  for I := 0 to 96 * 64 - 1 do
    Pixels[I * 4 + 3] := 255;
  for Y := 0 to 63 do
    for X := 48 to 95 do
      Pixels[(Y * 96 + X) * 4 + 3] := 0;
  EncodeAndDecode('half transparent', Pixels, 96, 64, True, 75, 6, FileSize);

  for Y := 0 to 63 do
    for X := 48 to 95 do
    begin
      I := (Y * 96 + X) * 4;
      Pixels[I] := (X * 97 + Y * 31) and $FF;
      Pixels[I + 1] := (X * 13 + Y * 201) and $FF;
      Pixels[I + 2] := (X * 151 + Y * 57) and $FF;
    end;
  Psnr := EncodeAndDecode('half transparent noise', Pixels, 96, 64, True, 75, 6, SizeWithNoise);
  // Measured 39.8 dB
  CheckTrue(Psnr > 37, Format('PSNR of visible pixels %.2f dB', [Psnr]));
  CheckTrue(SizeWithNoise < FileSize + FileSize div 4,
    Format('Noise in invisible pixels: %d bytes, %d without it', [SizeWithNoise, FileSize]));
end;

procedure TTestWebPLossy.TestWrongParameters;
var
  Pixels: TDynByteArray;
  Data: TPasWebPBytes;
begin
  Pixels := MakePixels(tcFlat, 4, 4, True);
  CheckFalse(PasWebPEncodeLossy(nil, 16, 4, 4, True, 75, 6, Data), 'No pixels');
  CheckFalse(PasWebPEncodeLossy(Pointer(Pixels), 16, 0, 4, True, 75, 6, Data), 'Zero width');
  CheckFalse(PasWebPEncodeLossy(Pointer(Pixels), 16, 4, 0, True, 75, 6, Data), 'Zero height');
  CheckFalse(PasWebPEncodeLossy(Pointer(Pixels), 16, PasWebPMaxSize + 1, 1, True, 75, 6, Data),
    'Width over the limit of WebP');
  CheckFalse(PasWebPEncodeLossy(Pointer(Pixels), 16, 1, PasWebPMaxSize + 1, True, 75, 6, Data),
    'Height over the limit of WebP');
  CheckEquals(0, Length(Data), 'No data when encoding fails');
end;

procedure TTestWebPLossy.TestWidestImage;
var
  FileSize: Integer;
begin
  EncodeAndDecode('widest', MakePixels(tcPhoto, PasWebPMaxSize, 3, False), PasWebPMaxSize, 3, False,
                  75, 2, FileSize);
  EncodeAndDecode('tallest', MakePixels(tcDrawing, 3, PasWebPMaxSize, True), 3, PasWebPMaxSize, True,
                  75, 2, FileSize);
end;

procedure TTestWebPLossy.TestDemoPhoto;
var
  Image: TImageData;
  Pixels: TDynByteArray;
  FileSize: Integer;
  Psnr: Double;
begin
  // A real photograph. Limits are a few percent off what the encoder gives
  // now (which is about what cwebp gives): to notice when it gets worse.
  InitImage(Image);
  CheckTrue(LoadImageFromFile(GetTestDataFile('Tigers.jpg'), Image), 'Loading of Tigers.jpg');
  try
    CheckTrue(ConvertImage(Image, ifR8G8B8), 'Conversion');
    SetLength(Pixels, Image.Size);
    Move(Image.Bits^, Pointer(Pixels)^, Image.Size);
    Psnr := EncodeAndDecode('Tigers.jpg', Pixels, Image.Width, Image.Height, False, 75,
                            PasWebPDefaultEffort, FileSize);
    CheckTrue(FileSize <= DemoPhotoMaxSize, Format('Tigers.jpg at quality 75 takes %d bytes, limit is %d',
                                                   [FileSize, DemoPhotoMaxSize]));
    CheckTrue(Psnr >= DemoPhotoMinPsnr, Format('Tigers.jpg at quality 75: PSNR %.2f dB, at least %.1f expected',
                                               [Psnr, DemoPhotoMinPsnr]));
  finally
    FreeImage(Image);
  end;
end;

procedure TTestWebPLossy.TestImagingSaveLoad;
var
  Previous: TWebPBackend;
  Image, Loaded: TImageData;
  Stream: TMemoryStream;
  Pixels: TDynByteArray;
  LossySize: Int64;
  I, D, MaxDiff: Integer;
begin
  // Through the file format class with the Pascal codec, also where
  // libwebp is found
  Previous := WebPBackend;
  CheckTrue(SelectWebPBackend(wbPascal), 'Pascal WebP codec is available');
  InitImage(Image);
  InitImage(Loaded);
  Stream := TMemoryStream.Create;
  PushOptions;
  try
    CheckTrue(SetOption(ImagingWebPLossless, 0), 'ImagingWebPLossless');
    CheckTrue(SetOption(ImagingWebPQuality, 90), 'ImagingWebPQuality');

    Pixels := MakePixels(tcSmooth, 90, 70, True);
    CheckTrue(NewImage(90, 70, ifA8R8G8B8, Image), 'NewImage');
    Move(Pointer(Pixels)^, Image.Bits^, Image.Size);
    CheckTrue(SaveImageToStream('webp', Stream, Image), 'Saving');
    LossySize := Stream.Size;
    Stream.Position := 0;
    CheckTrue(LoadImageFromStream(Stream, Loaded), 'Loading');
    CheckTrue(Loaded.Format = ifA8R8G8B8, 'Image with alpha is loaded as A8R8G8B8');
    CheckEquals(90, Loaded.Width, 'Width');
    CheckEquals(70, Loaded.Height, 'Height');

    MaxDiff := 0;
    for I := 0 to Image.Size - 1 do
    begin
      D := Abs(Integer(PByteArray(Image.Bits)[I]) - Integer(PByteArray(Loaded.Bits)[I]));
      if I mod 4 = 3 then
        CheckEquals(0, D, 'Difference of alpha')
      else if D > MaxDiff then
        MaxDiff := D;
    end;
    // Measured 6
    CheckTrue(MaxDiff < 16, 'Largest difference of a color channel at quality 90: ' + IntToStr(MaxDiff));

    // Lower quality gives a smaller file, lossless a larger one
    CheckTrue(SetOption(ImagingWebPQuality, 30), 'ImagingWebPQuality');
    Stream.Clear;
    CheckTrue(SaveImageToStream('webp', Stream, Image), 'Saving at lower quality');
    CheckTrue(Stream.Size < LossySize, 'File at quality 30 is smaller than at 90');
    CheckTrue(SetOption(ImagingWebPLossless, 1), 'ImagingWebPLossless');
    Stream.Clear;
    CheckTrue(SaveImageToStream('webp', Stream, Image), 'Saving lossless');
    CheckTrue(Stream.Size > LossySize, 'Lossless file is larger than lossy one');
  finally
    PopOptions;
    SelectWebPBackend(Previous);
    Stream.Free;
    FreeImage(Image);
    FreeImage(Loaded);
  end;
end;

procedure TTestWebPLossy.TestDecodedByLibWebP;
{$IFDEF TEST_PASWEBP_WITH_LIBWEBP}
const
  Qualities: array[0..2] of Integer = (20, 75, 98);
var
  Content: TTestContent;
  WithAlpha, Ok: Boolean;
  Q, Effort, Bpp: Integer;
  Pixels, ByLib, ByPascal: TDynByteArray;
  Data: TPasWebPBytes;
  Msg: string;
begin
  // Only where libwebp is installed (CI: Linux). The encoder predicts from
  // the pixels the Pascal decoder gives: any difference of the decoders
  // would mean files that look wrong elsewhere.
  if not LoadLibWebP then
    Exit;

  for Content := Low(TTestContent) to High(TTestContent) do
    for WithAlpha := False to True do
      for Q := Low(Qualities) to High(Qualities) do
        for Effort := 0 to 2 do
        begin
          Msg := Format('content %d, alpha %d, quality %d, effort %d: ',
                        [Ord(Content), Ord(WithAlpha), Qualities[Q], Effort * 4]);
          if WithAlpha then Bpp := 4 else Bpp := 3;
          Pixels := MakePixels(Content, 131, 77, WithAlpha);
          CheckTrue(PasWebPEncodeLossy(Pointer(Pixels), 131 * Bpp, 131, 77, WithAlpha,
                                       Qualities[Q], Effort * 4, Data), Msg + 'encoding');
          ByLib := nil;
          ByPascal := nil;
          SetLength(ByLib, Length(Pixels));
          SetLength(ByPascal, Length(Pixels));
          if WithAlpha then
            Ok := WebPDecodeBGRAInto(Pointer(Data), Length(Data), Pointer(ByLib), Length(ByLib), 131 * 4) <> nil
          else
            Ok := WebPDecodeBGRInto(Pointer(Data), Length(Data), Pointer(ByLib), Length(ByLib), 131 * 3) <> nil;
          CheckTrue(Ok, Msg + 'decoding by libwebp');
          CheckTrue(PasWebPDecodeInto(Pointer(Data), Length(Data), Pointer(ByPascal), 131 * Bpp,
                                      131, 77, WithAlpha), Msg + 'decoding by Pascal decoder');
          CheckSameMem(Pointer(ByLib), Pointer(ByPascal), Length(ByLib),
            Msg + 'pixels by libwebp and by Pascal decoder');
        end;
end;
{$ELSE}
begin
  // Built without the binding of libwebp
end;
{$ENDIF}

initialization
  RegisterImagingTest(TTestWebPDecoder);
  RegisterImagingTest(TTestWebPLossless);
  RegisterImagingTest(TTestWebPLossy);

{$ENDIF}

end.
