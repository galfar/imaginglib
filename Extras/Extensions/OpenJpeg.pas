{
 * Copyright (c) 2001-2003, David Janssens
 * Copyright (c) 2002-2003, Yannick Verschueren
 * Copyright (c) 2003-2005, Francois Devaux and Antonin Descampe
 * Copyright (c) 2005, Hervé Drolon, FreeImage Team
 * Copyright (c) 2002-2005, Communications and remote sensing Laboratory, Universite catholique de Louvain, Belgium
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS `AS IS'
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
}

{
  Translated to Object Pascal by Marek Mauder for Vampyre Imaging Library
  http://imaginglib.sourceforge.net
}

unit OpenJpeg;

{$I ImagingOptions.inc}

interface

const
  OPENJPEG_VERSION = '1.0.0';

{ ==========================================================
     Compiler directives
  ========================================================== }

type
  Bool = LongBool;
  Char = AnsiChar;

const
  { Maximum allowed size for filenames }
  MAX_PATH = 260;

  { Number of maximum resolution level authorized }
  J2K_MAXRLVLS = 33;
  { Number of maximum sub-band linked to number of resolution level }
  J2K_MAXBANDS = 3 * J2K_MAXRLVLS - 2;


{ ==========================================================
     enum definitions
  ========================================================== }

type
  { Progression order }
  PROG_ORDER = (
    PROG_UNKNOWN = -1, { place-holder }
    LRCP = 0,          { layer-resolution-component-precinct order }
    RLCP = 1,          { resolution-layer-component-precinct order }
    RPCL = 2,          { resolution-precinct-component-layer order }
    PCRL = 3,          { precinct-component-resolution-layer order }
    CPRL = 4);         { component-precinct-resolution-layer order }
  OPJ_PROG_ORDER = PROG_ORDER;

  { Supported image color spaces }
  COLOR_SPACE = (
    CLRSPC_UNKNOWN = -1, { place-holder }
    CLRSPC_SRGB = 1,     { sRGB }
    CLRSPC_GRAY = 2,     { grayscale }
    CLRSPC_SYCC = 3,     { YUV }
    CLRSCR_FORCE32);
  OPJ_COLOR_SPACE = COLOR_SPACE;

  { Supported codec }
  CODEC_FORMAT = (
    CODEC_UNKNOWN = -1, { place-holder }
    CODEC_J2K = 0,      { JPEG-2000 codestream : read/write }
    CODEC_JPT = 1,      { JPT-stream (JPEG 2000, JPIP) : read only }
    CODEC_JP2 = 2);     { JPEG-2000 file format : read/write }
  OPJ_CODEC_FORMAT = CODEC_FORMAT;

{ ==========================================================
     event manager typedef definitions
  ========================================================== }

  { Callback function prototype for events }
  opj_msg_callback = procedure(msg: PChar; client_data: Pointer); cdecl;
  { Message handler object }
  opj_event_mgr = record
    error_handler: opj_msg_callback;   { Error message callback if available, NULL otherwise }
    warning_handler: opj_msg_callback; { Warning message callback if available, NULL otherwise }
    info_handler: opj_msg_callback;    { Debug message callback if available, NULL otherwise }
  end;
  opj_event_mgr_t = opj_event_mgr;
  popj_event_mgr_t = ^opj_event_mgr_t;


{ ==========================================================
     codec typedef definitions
  ========================================================== }

  { Progression order changes }
  opj_poc = record
    resno0: Integer;
    compno0: Integer;
    layno1: Integer;
    resno1: Integer;
    compno1: Integer;
    prg: OPJ_PROG_ORDER;
    tile: Integer;
    progorder: array[0..3] of Char;
  end;
  opj_poc_t = opj_poc;

  { Compression parameters }
  opj_cparameters = record
    tile_size_on: Bool;
    cp_tx0: Integer;
    cp_ty0: Integer;
    cp_tdx: Integer;
    cp_tdy: Integer;
    cp_disto_alloc: Integer;
    cp_fixed_alloc: Integer;
    cp_fixed_quality: Integer;
    cp_matrice: PInteger;
    cp_comment: PChar;
    csty: Integer;
    prog_order: OPJ_PROG_ORDER;
    POC: array[0..31] of opj_poc_t;
    numpocs: Integer;
    tcp_numlayers: Integer;
    tcp_rates: array[0..99] of Integer;
    tcp_distoratio: array[0..99] of Single;
    numresolution: Integer;
    cblockw_init: Integer;
    cblockh_init: Integer;
    mode: Integer;
    irreversible: Integer;
    roi_compno: Integer;
    roi_shift: Integer;
    res_spec: Integer;
    prcw_init: array[0..J2K_MAXRLVLS - 1] of Integer;
    prch_init: array[0..J2K_MAXRLVLS - 1] of Integer;
    infile: array[0..MAX_PATH - 1] of Char;
    outfile: array[0..MAX_PATH - 1] of Char;
    index_on: Integer;
    index: array[0..MAX_PATH - 1] of Char;
    image_offset_x0: Integer;
    image_offset_y0: Integer;
    subsampling_dx: Integer;
    subsampling_dy: Integer;
    decod_format: Integer;
    cod_format: Integer;
  end;
  opj_cparameters_t = opj_cparameters;
  popj_cparameters_t = ^opj_cparameters_t;

  { Decompression parameters }
  opj_dparameters = record
    { Set the number of highest resolution levels to be discarded.
      The image resolution is effectively divided by 2 to the power of the number of discarded levels.
      The reduce factor is limited by the smallest total number of decomposition levels among tiles.
      if != 0, then original dimension divided by 2^(reduce);
      if == 0 or not used, image is decoded to the full resolution }
    cp_reduce: Integer;
    { Set the maximum number of quality layers to decode.
      If there are less quality layers than the specified number, all the quality layers are decoded.
      if != 0, then only the first "layer" layers are decoded;
      if == 0 or not used, all the quality layers are decoded }
    cp_layer: Integer;
    { @name command line encoder parameters (not used inside the library) }
    { input file name }
    infile: array[0..MAX_PATH - 1] of Char;
    { output file name }
    outfile: array[0..MAX_PATH - 1] of Char;
    { input file format 0: J2K, 1: JP2, 2: JPT }
    decod_format: Integer;
    { output file format 0: PGX, 1: PxM, 2: BMP }
    cod_format: Integer;
  end;
  opj_dparameters_t = opj_dparameters;
  popj_dparameters_t = ^opj_dparameters_t;

  { Routines that are to be used by both halves of the library are declared
    to receive a Pointer to this structure.  There are no actual instances of
    opj_common_struct_t, only of opj_cinfo_t and opj_dinfo_t. }
  opj_common_struct = record
    event_mgr: popj_event_mgr_t;    { Pointer to the event manager }
    client_data: Pointer;           { Available for use by application }
    is_decompressor: Bool;          { So common code can tell which is which }
    codec_format: OPJ_CODEC_FORMAT; { selected codec }
    j2k_handle: Pointer;            { Pointer to the J2K codec }
    jp2_handle: Pointer;            { Pointer to the JP2 codec }
  end;
  opj_common_struct_t = opj_common_struct;
  opj_common_ptr = ^opj_common_struct_t;

  { Compression context info }
  opj_cinfo = record
    event_mgr: popj_event_mgr_t;
    client_data: Pointer;
    is_decompressor: Bool;
    codec_format: OPJ_CODEC_FORMAT;
    j2k_handle: Pointer;
    jp2_handle: Pointer;
  end;
  opj_cinfo_t = opj_cinfo;
  popj_cinfo_t = ^opj_cinfo_t;

  { Decompression context info }
  opj_dinfo = record
    event_mgr: popj_event_mgr_t;
    client_data: Pointer;
    is_decompressor: Bool;
    codec_format: OPJ_CODEC_FORMAT;
    j2k_handle: Pointer;
    jp2_handle: Pointer;
  end;
  opj_dinfo_t = opj_dinfo;
  popj_dinfo_t = ^opj_dinfo_t;

{ ==========================================================
     I/O stream typedef definitions
  ========================================================== }

const
  { Stream open flags. }
  { The stream was opened for reading. }
  OPJ_STREAM_READ = $0001;
  {* The stream was opened for writing.  }
  OPJ_STREAM_WRITE = $0002;

type
  { Byte input-output stream (CIO) }
  opj_cio = record
    cinfo: opj_common_ptr; { codec context  }
    openmode: Integer;     { open mode (read/write) either OPJ_STREAM_READ or OPJ_STREAM_WRITE  }
    buffer: PChar;         { Pointer to the start of the buffer  }
    length: Integer;       { buffer size in bytes  }
    start: PChar;          { Pointer to the start of the stream  }
    end_: PChar;           { Pointer to the end of the stream  }
    bp: PChar;             { Pointer to the current position  }
  end;
  opj_cio_t = opj_cio;
  popj_cio_t = ^opj_cio_t;

{ ==========================================================
     image typedef definitions
  ========================================================== }

  { Defines a single image component }
  opj_image_comp = record
    dx: Integer;            { XRsiz: horizontal separation of a sample of ith component with respect to the reference grid  }
    dy: Integer;            { YRsiz: vertical separation of a sample of ith component with respect to the reference grid  }
    w: Integer;             { data width  }
    h: Integer;             { data height  }
    x0: Integer;            { x component offset compared to the whole image  }
    y0: Integer;            { y component offset compared to the whole image  }
    prec: Integer;          { precision  }
    bpp: Integer;           { image depth in bits  }
    sgnd: Integer;          { signed (1) / unsigned (0)  }
    resno_decoded: Integer; { number of decoded resolution  }
    factor: Integer;        { number of division by 2 of the out image compared to the original size of image  }
    data: PIntegerArray;    { image component data  }
  end;
  opj_image_comp_t = opj_image_comp;
  popj_image_comp_t = ^opj_image_comp_t;
  opj_image_comp_array = array[0..255] of opj_image_comp_t;
  popj_image_comp_array = ^opj_image_comp_array;

  { Defines image data and Characteristics }
  opj_image = record
    x0: Integer;                  { XOsiz: horizontal offset from the origin of the reference grid to the left side of the image area  }
    y0: Integer;                  { YOsiz: vertical offset from the origin of the reference grid to the top side of the image area  }
    x1: Integer;                  { Xsiz: width of the reference grid  }
    y1: Integer;                  { Ysiz: height of the reference grid  }
    numcomps: Integer;            { number of components in the image  }
    color_space: OPJ_COLOR_SPACE; { color space: sRGB, Greyscale or YUV  }
    comps: popj_image_comp_array; { image components  }
  end;
  opj_image_t = opj_image;
  popj_image_t = ^opj_image_t;

  { Component parameters structure used by the opj_image_create function }
  opj_image_comptparm = record
    dx: Integer;   { XRsiz: horizontal separation of a sample of ith component with respect to the reference grid  }
    dy: Integer;   { YRsiz: vertical separation of a sample of ith component with respect to the reference grid  }
    w: Integer;    { data width  }
    h: Integer;    { data height  }
    x0: Integer;   { x component offset compared to the whole image  }
    y0: Integer;   { y component offset compared to the whole image  }
    prec: Integer; { precision  }
    bpp: Integer;  { image depth in bits  }
    sgnd: Integer; { signed (1) / unsigned (0)  }
  end;
  opj_image_cmptparm_t = opj_image_comptparm;
  popj_image_cmptparm_t = ^opj_image_cmptparm_t;
  opj_image_cmptparm_array = array[0..255] of opj_image_cmptparm_t;
  popj_image_cmptparm_array = ^opj_image_cmptparm_array;

{ ==========================================================
     openjpeg version
  ========================================================== }

function opj_version: PChar; cdecl; external {'_opj_version'};

{ ==========================================================
     image functions definitions
  ========================================================== }
{ Create an image
  @param numcmpts number of components
  @param cmptparms components parameters
  @param clrspc image color space
  @return returns a new image structure if successful, returns NULL otherwise }
function opj_image_create(numcmpts: Integer; cmptparms: popj_image_cmptparm_t;
  clrspc: OPJ_COLOR_SPACE): popj_image_t; cdecl; external {'_opj_image_create'};

{ Deallocate any resources associated with an image
  @param image image to be destroyed }
procedure opj_image_destroy(image: popj_image_t); cdecl; external {'_opj_image_destroy'};

{ ==========================================================
     stream functions definitions
  ========================================================== }
{ Open and allocate a memory stream for read / write.
  On reading, the user must provide a buffer containing encoded data. The buffer will be
  wrapped by the returned CIO handle.
  On writing, buffer parameters must be set to 0: a buffer will be allocated by the library
  to contain encoded data.
  @param cinfo Codec context info
  @param buffer Reading: buffer address. Writing: NULL
  @param length Reading: buffer length. Writing: 0
  @return Returns a CIO handle if successful, returns NULL otherwise }
function opj_cio_open(cinfo: opj_common_ptr; buffer: PByte;
  length: Integer): popj_cio_t; cdecl; external {'_opj_cio_open'};

{ Close and free a CIO handle
  @param cio CIO handle to free }
procedure opj_cio_close(cio: popj_cio_t); cdecl; external {'_opj_cio_close'};

{ Get position in byte stream
  @param cio CIO handle
  @return Returns the position in bytes }
function cio_tell(cio: popj_cio_t): Integer; cdecl; external {'_cio_tell'};

{ Set position in byte stream
  @param cio CIO handle
  @param pos Position, in number of bytes, from the beginning of the stream }
procedure cio_seek(cio: popj_cio_t; pos: Integer); cdecl; external {'_cio_seek'};

{ ==========================================================
     event manager functions definitions
  ========================================================== }

function opj_set_event_mgr(cinfo: opj_common_ptr; event_mgr: popj_event_mgr_t;
  context: Pointer): popj_event_mgr_t; cdecl; external {'_opj_set_event_mgr'};

{ ==========================================================
     codec functions definitions
  ========================================================== }

{ Creates a J2K/JPT/JP2 decompression structure
  @param format Decoder to select
  @return Returns a handle to a decompressor if successful, returns NULL otherwise }
function opj_create_decompress(format: OPJ_CODEC_FORMAT): popj_dinfo_t;
  cdecl; external {'_opj_create_decompress'};

{ Destroy a decompressor handle
  @param dinfo decompressor handle to destroy }
procedure opj_destroy_decompress(dinfo: popj_dinfo_t); cdecl;
  external {'_opj_destroy_decompress'};

{ Set decoding parameters to default values
  @param parameters Decompression parameters }
procedure opj_set_default_decoder_parameters(parameters: popj_dparameters_t);
  cdecl; external {'_opj_set_default_decoder_parameters'};

{ Setup the decoder decoding parameters using user parameters.
  Decoding parameters are returned in j2k->cp.
  @param dinfo decompressor handle
  @param parameters decompression parameters }
procedure opj_setup_decoder(dinfo: popj_dinfo_t; parameters: popj_dparameters_t);
  cdecl; external {'_opj_setup_decoder'};

{ Decode an image from a JPEG-2000 codestream
  @param dinfo decompressor handle
  @param cio Input buffer stream
  @return Returns a decoded image if successful, returns NULL otherwise }
function opj_decode(dinfo: popj_dinfo_t; cio: popj_cio_t): popj_image_t;
  cdecl; external {'_opj_decode'};

{ Creates a J2K/JP2 compression structure
  @param format Coder to select
  @return Returns a handle to a compressor if successful, returns NULL otherwise }
function opj_create_compress(format: OPJ_CODEC_FORMAT): popj_cinfo_t;
  cdecl; external {'_opj_create_compress'};

{ Destroy a compressor handle
  @param cinfo compressor handle to destroy }
procedure opj_destroy_compress(cinfo: popj_cinfo_t); cdecl; external {'_opj_destroy_compress'};

{ Set encoding parameters to default values, that means :
  <ul>
  <li>Lossless
  <li>1 tile
  <li>Size of precinct : 2^15 x 2^15 (means 1 precinct)
  <li>Size of code-block : 64 x 64
  <li>Number of resolutions: 6
  <li>No SOP marker in the codestream
  <li>No EPH marker in the codestream
  <li>No sub-sampling in x or y direction
  <li>No mode switch activated
  <li>Progression order: LRCP
  <li>No index file
  <li>No ROI upshifted
  <li>No offset of the origin of the image
  <li>No offset of the origin of the tiles
  <li>Reversible DWT 5-3
  </ul>
  @param parameters Compression parameters }
procedure opj_set_default_encoder_parameters(parameters: popj_cparameters_t);
  cdecl; external {'_opj_set_default_encoder_parameters'};

{ Setup the encoder parameters using the current image and using user parameters.
  @param cinfo compressor handle
  @param parameters compression parameters
  @param image input filled image }
procedure opj_setup_encoder(cinfo: popj_cinfo_t; parameters: popj_cparameters_t;
  image: popj_image_t); cdecl; external {'_opj_setup_encoder'};

{ Encode an image into a JPEG-2000 codestream
  @param cinfo compressor handle
  @param cio Output buffer stream
  @param image Image to encode
  @param index Name of the index file if required, NULL otherwise
  @return Returns true if successful, returns false otherwise }
function opj_encode(cinfo: popj_cinfo_t; cio: popj_cio_t; image: popj_image_t;
  index: PChar): Bool; cdecl; external {'_opj_encode'};

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, ImagingUtility;

const
  MSCRuntimeLib = 'msvcrt.dll';

{$IFDEF MSWINDOWS}
  {$IFDEF DCC}
    {$L J2KObjects\w32bor_pi.obj}
    {$L J2KObjects\w32bor_openjpeg.obj}
    {$L J2KObjects\w32bor_j2k_lib.obj}
    {$L J2KObjects\w32bor_event.obj}
    {$L J2KObjects\w32bor_cio.obj}
    {$L J2KObjects\w32bor_image.obj}
    {$L J2KObjects\w32bor_j2k.obj}
    {$L J2KObjects\w32bor_jp2.obj}
    {$L J2KObjects\w32bor_jpt.obj}
    {$L J2KObjects\w32bor_mqc.obj}
    {$L J2KObjects\w32bor_raw.obj}
    {$L J2KObjects\w32bor_bio.obj}
    {$L J2KObjects\w32bor_tgt.obj}
    {$L J2KObjects\w32bor_tcd.obj}
    {$L J2KObjects\w32bor_t1.obj}
    {$L J2KObjects\w32bor_dwt.obj}
    {$L J2KObjects\w32bor_t2.obj}
    {$L J2KObjects\w32bor_mct.obj}
    {$L J2KObjects\w32bor_int.obj}
    {$L J2KObjects\w32bor_fix.obj}

    var
      __turboFloat: LongInt;

    procedure opj_event_msg; cdecl; external;
    procedure opj_clock; cdecl; external;
    procedure cio_read; cdecl; external;
    procedure cio_write; cdecl; external;
    procedure cio_skip; cdecl; external;
    procedure bio_read; cdecl; external;
    procedure bio_write; cdecl; external;
    procedure cio_numbytesleft; cdecl; external;
    procedure cio_getbp; cdecl; external;
    procedure j2k_destroy_compress; cdecl; external;
    procedure j2k_realloc; cdecl; external;
    procedure tgt_create; cdecl; external;
    procedure tgt_destroy; cdecl; external;
    procedure mqc_setcurctx; cdecl; external;
    procedure mqc_bypass_enc; cdecl; external;
    procedure mqc_encode; cdecl; external;
    procedure mqc_decode; cdecl; external;
    procedure raw_decode; cdecl; external;
    procedure mqc_resetstates; cdecl; external;
    procedure mqc_setstate; cdecl; external;
    procedure mqc_init_enc; cdecl; external;
    procedure mqc_segmark_enc; cdecl; external;
    procedure mqc_flush; cdecl; external;
    procedure mqc_bypass_init_enc; cdecl; external;
    procedure mqc_numbytes; cdecl; external;
    procedure mqc_reset_enc; cdecl; external;
    procedure mqc_erterm_enc; cdecl; external;
    procedure mqc_init_dec; cdecl; external;
    procedure raw_init_dec; cdecl; external;
    procedure mqc_destroy; cdecl; external;
    procedure mqc_restart_init_enc; cdecl; external;
    procedure raw_destroy; cdecl; external;
    procedure tgt_reset; cdecl; external;
    procedure tgt_setvalue; cdecl; external;
    procedure bio_init_enc; cdecl; external;
    procedure bio_flush; cdecl; external;
    procedure bio_numbytes; cdecl; external;
    procedure bio_destroy; cdecl; external;
    procedure bio_init_dec; cdecl; external;
    procedure pi_create; cdecl; external;
    procedure pi_next; cdecl; external;
    procedure pi_destroy; cdecl; external;
    procedure tgt_encode; cdecl; external;
    procedure tgt_decode; cdecl; external;
    procedure bio_inalign; cdecl; external;
  {$ELSE}
    {$L J2KObjects\w32msc_openjpeg.obj}
    {$L J2KObjects\w32msc_j2k_lib.obj}
    {$L J2KObjects\w32msc_event.obj}
    {$L J2KObjects\w32msc_cio.obj}
    {$L J2KObjects\w32msc_image.obj}
    {$L J2KObjects\w32msc_j2k.obj}
    {$L J2KObjects\w32msc_jp2.obj}
    {$L J2KObjects\w32msc_jpt.obj}
    {$L J2KObjects\w32msc_mqc.obj}
    {$L J2KObjects\w32msc_raw.obj}
    {$L J2KObjects\w32msc_bio.obj}
    {$L J2KObjects\w32msc_tgt.obj}
    {$L J2KObjects\w32msc_tcd.obj}
    {$L J2KObjects\w32msc_t1.obj}
    {$L J2KObjects\w32msc_dwt.obj}
    {$L J2KObjects\w32msc_pi.obj}
    {$L J2KObjects\w32msc_t2.obj}
    {$L J2KObjects\w32msc_mct.obj}
    {$L J2KObjects\w32msc_int.obj}
    {$L J2KObjects\w32msc_fix.obj}
  {$ENDIF}
{$ENDIF}

function malloc(Size: Integer): Pointer; cdecl; {$IFDEF FPC}[Public];{$ENDIF}
begin
  GetMem(Result, Size);
end;

procedure free(Ptr: Pointer); cdecl; {$IFDEF FPC}[Public];{$ENDIF}
begin
  FreeMem(Ptr);
end;

function realloc(Ptr: Pointer; Size: Integer): Pointer; cdecl; {$IFDEF FPC}[Public];{$ENDIF}
begin
  ReallocMem(Ptr, Size);
  Result := Ptr;
end;

function memset(S: Pointer; C, N: Integer): Pointer; cdecl; {$IFDEF FPC}[Public];{$ENDIF}
begin
  FillMemoryByte(S, N, C);
  Result := S;
end;

function memcpy(S1, S2: Pointer; N: Integer): Pointer; cdecl; {$IFDEF FPC}[Public];{$ENDIF}
begin
 Move(S2^, S1^, N);
 Result := S1;
end;

function strlen(S: PChar): Integer; cdecl; {$IFDEF FPC}[Public];{$ENDIF}
begin
  Result := SysUtils.StrLen(S);
end;

function strcat(S1, S2: PChar): PChar; cdecl; {$IFDEF FPC}[Public];{$ENDIF}
begin
  Result := SysUtils.StrCat(S1, S2);
end;

function strcpy(S1, S2: PChar): PChar; cdecl; {$IFDEF FPC}[Public];{$ENDIF}
begin
  Result := SysUtils.StrCopy(S1, S2);
end;

function fabs(const Num: Double): Double; cdecl; {$IFDEF FPC}[Public];{$ENDIF}
begin
  Result := Abs(Num);
end;

function floor(const X: Double): Double; cdecl; {$IFDEF FPC}[Public];{$ENDIF}
begin
  Result := Trunc(X);
  if Frac(X) < 0.0 then
    Result := Result - 1.0;
end;

function pow(const Base, Exponent: Double): Double; cdecl; {$IFDEF FPC}[Public];{$ENDIF}
begin
  if Exponent = 0.0 then
    Result := 1.0
  else if (Base = 0.0) and (Exponent > 0.0) then
    Result := 0.0
  else
    Result := Exp(Exponent * Ln(Base));
end;

procedure _llmul; cdecl; {$IFDEF FPC}[Public];{$ENDIF}
asm
  // taken from Delphi's System.pas __llmul
        push  edx
        push  eax

        mov   eax, [esp+16]
        mul   dword ptr [esp]
        mov   ecx, eax

        mov   eax, [esp+4]
        mul   dword ptr [esp+12]
        add   ecx, eax

        mov   eax, [esp]
        mul   dword ptr [esp+12]
        add   edx, ecx

        pop   ecx
        pop   ecx

        ret     8
end;

procedure _allshr; cdecl; {$IFDEF FPC}[Public];{$ENDIF}
asm
  // taken from Delphi's System.pas __llshr
        cmp cl, 32
        jl  @__llshr@below32
        cmp cl, 64
        jl  @__llshr@below64
        sar edx, 1fh
        mov eax,edx
        ret

@__llshr@below64:
        mov eax, edx
        cdq
        sar eax,cl
        ret

@__llshr@below32:
        shrd  eax, edx, cl
        sar edx, cl
        ret
end;

{$IFDEF DCC}

function sprintf(S: PChar; Format: PChar): Integer; cdecl; varargs; external MSCRuntimeLib;
function printf(Format: PChar): Integer; cdecl; varargs; external MSCRuntimeLib;
function fprintf(F: Pointer; Format: PChar): Integer; cdecl; varargs; external MSCRuntimeLib;
function fopen(FileName, Mode: PChar): Pointer; cdecl; external MSCRuntimeLib;
function fclose(F: Pointer): Integer; cdecl; external MSCRuntimeLib;

function tolower(C: Integer): Integer; cdecl; external MSCRuntimeLib; //{$IFDEF FPC}[Public];{$ENDIF}
function _ltolower(C: Integer): Integer;cdecl; external MSCRuntimeLib name 'tolower';//{$IFDEF FPC}[Public];{$ENDIF}

function _ftol{(X: Single)}: LongInt; cdecl; external MSCRuntimeLib; //{$IFDEF FPC}[Public];{$ENDIF}
{asm
  //Result := Round(X);
  fstp Result

end;
 }
{$ELSE}

function _sprintf(S: PChar; Format: PChar): Integer; cdecl; varargs; external MSCRuntimeLib name 'strlen';
function _printf(Format: PChar): Integer; cdecl; varargs; external MSCRuntimeLib name 'strlen';
function _tolower(C: Integer): Integer; cdecl; external MSCRuntimeLib name 'strlen';
function _fprintf(F: Pointer; Format: PChar): Integer; cdecl; varargs; external MSCRuntimeLib name 'strlen';
function _fopen(FileName, Mode: PChar): Pointer; cdecl; external MSCRuntimeLib name 'strlen';
function _fclose(F: Pointer): Integer; cdecl; external MSCRuntimeLib name 'strlen';

function sprintf(S: PChar; Format: PChar): Integer; cdecl; [Public];
begin
//  Result := _sprintf(S, Format);
end;

function printf(Format: PChar): Integer; cdecl; [Public];
begin

end;

function tolower(C: Integer): Integer; cdecl; [Public];
begin
  Result := _tolower(C);
end;

function fprintf(F: Pointer; Format: PChar): Integer; cdecl; {varargs;} [Public];
begin

end;

function fopen(FileName, Mode: PChar): Pointer; cdecl; [Public];
begin
  Result := _fopen(FileName, Mode);
end;

function fclose(F: Pointer): Integer; cdecl; [Public];
begin
  Result := _fclose(F);
end;

function __ftol(X: Single): LongInt; cdecl; external MSCRuntimeLib name '_ftol';
function __CIpow(Base, Exponent: Double): Double; cdecl; external MSCRuntimeLib name '_CIpow';

function _ftol(X: Single): LongInt; cdecl; [Public];
begin
  Result := __ftol(X);
end;

function _ftol2_sse(X: Double): LongInt; cdecl; [Public];
begin
  Result := __ftol(X);
end;

function _CIpow(Base, Exponent: Double): Double; cdecl; [Public];
begin
  Result := __CIpow(Base, Exponent);
end;

{$ENDIF}

end.

