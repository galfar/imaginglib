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

{ This unit contains default IO functions for reading from/writting to
  files, streams and memory.}
unit ImagingIO;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, ImagingTypes, Imaging, ImagingUtility;

type
  TMemoryIORec = record
    Data: ImagingUtility.PByteArray;
    Position: LongInt;
    Size: LongInt;
  end;
  PMemoryIORec = ^TMemoryIORec;

var
  OriginalFileIO: TIOFunctions;
  FileIO: TIOFunctions;
  StreamIO: TIOFunctions;
  MemoryIO: TIOFunctions;

{ Helper function that returns size of input (from current position to the end)
  represented by Handle (and opened and operated on by members of IOFunctions).}
function GetInputSize(IOFunctions: TIOFunctions; Handle: TImagingHandle): LongInt;
{ Helper function that initializes TMemoryIORec with given params.}
function PrepareMemIO(Data: Pointer; Size: LongInt): TMemoryIORec;

implementation

const
  ReadBufferSize  = 16 * 1024;
  WriteBufferSize = 16 * 1024;

type
  TBufferedFile = class(TObject)
  protected
    FStream: TFileStream;
    FBuffer: PByteArray;
    FBufSize: LongInt;
    FBufPos: LongInt;
    FBufEnd: LongInt;
    function GetSize: LongInt; virtual;
    function GetPosition: LongInt; virtual; abstract;
    procedure SetPosition(Value: LongInt); virtual; abstract;
    procedure FlushBuffer; virtual; abstract;
  public
    constructor Create(const AFileName: string; Mode: Word; ABufferSize: LongInt);
    destructor Destroy; override;
    property Position: LongInt read GetPosition write SetPosition;
    property Size: LongInt read GetSize;
  end;

  { Simple file reader with buffering. Small reads are buffered
    (size < ABufferSize) and large ones are read whole at once.}
  TBufferedReadFile = class(TBufferedFile)
  protected
    function GetPosition: LongInt; override;
    procedure SetPosition(Value: LongInt); override;
    procedure FlushBuffer; override;
  public
    constructor Create(const AFileName: string; ABufferSize: LongInt);
    destructor Destroy; override;
    function Read(var DestBuffer; Count: LongInt): LongInt;
  end;

  { Simple file writer with buffering. Small writes are buffered
    (size < ABufferSize) and large ones are written whole at once.}
  TBufferedWriteFile = class(TBufferedFile)
  protected
    function GetSize: LongInt; override;
    function GetPosition: LongInt; override;
    procedure SetPosition(Value: LongInt); override;
    procedure FlushBuffer; override;
  public
    constructor Create(const AFileName: string; ABufferSize: LongInt);
    destructor Destroy; override;
    function Write(var SrcBuffer; Count: LongInt): LongInt;
  end;

{ TBufferedFile }

constructor TBufferedFile.Create(const AFileName: string; Mode: Word;
  ABufferSize: LongInt);
begin
  FStream := TFileStream.Create(AFileName, Mode);
  FBufSize := ABufferSize;
  GetMem(FBuffer, FBufSize);
end;

destructor TBufferedFile.Destroy;
begin
  FreeMem(FBuffer);
  FStream.Free;
  inherited Destroy;
end;

function TBufferedFile.GetSize: LongInt;
begin
  Result := FStream.Size;
end;

{ TBufferedReadFile }

constructor TBufferedReadFile.Create(const AFileName: string;
  ABufferSize: LongInt);
begin
  inherited Create(AFileName, fmOpenRead or fmShareDenyWrite, ABufferSize);
end;

destructor TBufferedReadFile.Destroy;
begin
  // Set stream position to real position it would have without buffering
  SetPosition(Position);
  inherited Destroy;
end;

function TBufferedReadFile.Read(var DestBuffer; Count: LongInt): LongInt;
var
  CopyNow: LongInt;
  Dest: PByte;
begin
  if Count >= FBufSize then
  begin
    // Large data chunks are read directly from file, SetPosition is called
    // to invalidate current buffer
    SetPosition(Position);
    Result := FStream.Read(DestBuffer, Count);
  end
  else
  begin
    Dest := @DestBuffer;
    Result := 0;
    while Count > 0 do
    begin
      if FBufPos >= FBufEnd then
      begin
        // Current buffer position is >= buffer's current size so
        // new data is read to buffer
        FlushBuffer;
        if FBufEnd = 0 then
        begin
          // This happens if no new data was read to buffer - stream reached
          // end of file
          Exit;
        end;
      end;
      // Get exact number of bytes to copy
      CopyNow := FBufEnd - FBufPos;
      if CopyNow > Count then
        CopyNow := Count;
      // Copy data from buffer to dest and update counts
      Move(FBuffer[FBufPos], Dest^, CopyNow);
      Inc(FBufPos, CopyNow);
      Inc(Dest, CopyNow);
      Inc(Result, CopyNow);
      Dec(Count, CopyNow);
    end;
  end;
end;

function TBufferedReadFile.GetPosition: LongInt;
begin
  Result := FStream.Position - (FBufEnd - FBufPos);
end;

procedure TBufferedReadFile.SetPosition(Value: LongInt);
begin
  // Set stream position and invalidate buffer
  FStream.Position := Value;
  FBufPos := 0;
  FBufEnd := 0;
end;

procedure TBufferedReadFile.FlushBuffer;
begin
  FBufEnd := FStream.Read(FBuffer^, FBufSize);
  FBufPos := 0;
end;

{ TBufferedWriteFile }

constructor TBufferedWriteFile.Create(const AFileName: string;
  ABufferSize: LongInt);
begin
  inherited Create(AFileName, fmCreate or fmShareDenyWrite, ABufferSize);
end;

destructor TBufferedWriteFile.Destroy;
begin
  // Buffer must be flushed before closing then file
  FlushBuffer;
  inherited Destroy;
end;

function TBufferedWriteFile.Write(var SrcBuffer; Count: LongInt): LongInt;
var
  CopyNow: LongInt;
  Src: PByte;
begin
  if Count >= FBufSize then
  begin
    // Large data chunks are written directly to file, current buffer is flushed first
    FlushBuffer;
    Result := FStream.Write(SrcBuffer, Count);
  end
  else
  begin
    Src := @SrcBuffer;
    Result := 0;
    while Count > 0 do
    begin
      // Get exact number of bytes to copy
      CopyNow := Count;
      if CopyNow > FBufSize - FBufPos then
        CopyNow := FBufSize - FBufPos;
      // Copy bytes from source to buffer   
      Move(Src^, FBuffer[FBufPos], CopyNow);
      Inc(FBufPos, CopyNow);
      Inc(Src, CopyNow);
      Inc(Result, CopyNow);
      Dec(Count, CopyNow);
      // Flush buffer if it is full
      if FBufPos = FBufSize then
        FlushBuffer;
    end;
  end;
end;

function TBufferedWriteFile.GetSize: LongInt;
begin
  Result := inherited GetSize + FBufPos;
end;

function TBufferedWriteFile.GetPosition: LongInt;
begin
  Result := FStream.Position + FBufPos;
end;

procedure TBufferedWriteFile.SetPosition(Value: LongInt);
begin
  FlushBuffer;
  FStream.Position := Value;
end;

procedure TBufferedWriteFile.FlushBuffer;
begin
  if FBufPos > 0 then
  begin
    FStream.WriteBuffer(FBuffer^, FBufPos);
    FBufPos := 0;
  end;
end;

{ File IO functions }

function FileOpenRead(FileName: PChar): TImagingHandle; cdecl;
begin
  Result := TBufferedReadFile.Create(FileName, ReadBufferSize);
end;

function FileOpenWrite(FileName: PChar): TImagingHandle; cdecl;
begin
  Result := TBufferedWriteFile.Create(FileName, ReadBufferSize);
end;

procedure FileClose(Handle: TImagingHandle); cdecl;
begin
  TObject(Handle).Free;
end;

function FileEof(Handle: TImagingHandle): Boolean; cdecl;
begin
  Result := TBufferedFile(Handle).Position = TBufferedFile(Handle).Size;
end;

function FileSeek(Handle: TImagingHandle; Offset: LongInt; Mode: TSeekMode):
  LongInt; cdecl;
begin
  Result := TBufferedFile(Handle).Position;
  case Mode of
    smFromBeginning: Result := Offset;
    smFromCurrent:   Result := TBufferedFile(Handle).Position + Offset;
    smFromEnd:       Result := TBufferedFile(Handle).Size + Offset;
  end;
  TBufferedFile(Handle).Position := Result;
end;

function FileTell(Handle: TImagingHandle): LongInt; cdecl;
begin
  Result := TBufferedFile(Handle).Position;
end;

function FileRead(Handle: TImagingHandle; Buffer: Pointer; Count: LongInt):
  LongInt; cdecl;
begin
  Result := TBufferedReadFile(Handle){(TObject(Handle) as TBufferedReadFile)}.Read(Buffer^, Count);
end;

function FileWrite(Handle: TImagingHandle; Buffer: Pointer; Count: LongInt):
  LongInt; cdecl;
begin
  Result := (TObject(Handle) as TBufferedWriteFile).Write(Buffer^, Count);
end;

{ Stream IO functions }

function StreamOpenRead(FileName: PChar): TImagingHandle; cdecl;
begin
  Result := FileName;
end;

function StreamOpenWrite(FileName: PChar): TImagingHandle; cdecl;
begin
  Result := FileName;
end;

procedure StreamClose(Handle: TImagingHandle); cdecl;
begin
end;

function StreamEof(Handle: TImagingHandle): Boolean; cdecl;
begin
  Result := TStream(Handle).Position = TStream(Handle).Size;
end;

function StreamSeek(Handle: TImagingHandle; Offset: LongInt; Mode: TSeekMode):
  LongInt; cdecl;
begin
  Result := TStream(Handle).Seek(Offset, LongInt(Mode));
end;

function StreamTell(Handle: TImagingHandle): LongInt; cdecl;
begin
  Result := TStream(Handle).Position;
end;

function StreamRead(Handle: TImagingHandle; Buffer: Pointer; Count: LongInt):
  LongInt; cdecl;
begin
  Result := TStream(Handle).Read(Buffer^, Count);
end;

function StreamWrite(Handle: TImagingHandle; Buffer: Pointer; Count: LongInt):
  LongInt; cdecl;
begin
  Result := TStream(Handle).Write(Buffer^, Count);
end;

{ Memory IO functions }

function MemoryOpenRead(FileName: PChar): TImagingHandle; cdecl;
begin
  Result := FileName;
end;

function MemoryOpenWrite(FileName: PChar): TImagingHandle; cdecl;
begin
  Result := FileName;
end;

procedure MemoryClose(Handle: TImagingHandle); cdecl;
begin
end;

function MemoryEof(Handle: TImagingHandle): Boolean; cdecl;
begin
  Result := PMemoryIORec(Handle).Position = PMemoryIORec(Handle).Size;
end;

function MemorySeek(Handle: TImagingHandle; Offset: LongInt; Mode: TSeekMode):
  LongInt; cdecl;
begin
  Result := PMemoryIORec(Handle).Position;
  case Mode of
    smFromBeginning: Result := Offset;
    smFromCurrent:   Result := PMemoryIORec(Handle).Position + Offset;
    smFromEnd:       Result := PMemoryIORec(Handle).Size + Offset;
  end;
  Result := ClampInt(Result, 0, PMemoryIORec(Handle).Size);
  PMemoryIORec(Handle).Position := Result;
end;

function MemoryTell(Handle: TImagingHandle): LongInt; cdecl;
begin
  Result := PMemoryIORec(Handle).Position;
end;

function MemoryRead(Handle: TImagingHandle; Buffer: Pointer; Count: LongInt):
  LongInt; cdecl;
var
  Rec: PMemoryIORec;
begin
  Rec := PMemoryIORec(Handle);
  Result := Count;
  if Rec.Position + Count > Rec.Size then
    Result := Rec.Size - Rec.Position;
  Move(Rec.Data[Rec.Position], Buffer^, Result);
  Rec.Position := Rec.Position + Result;
end;

function MemoryWrite(Handle: TImagingHandle; Buffer: Pointer; Count: LongInt):
  LongInt; cdecl;
var
  Rec: PMemoryIORec;
begin
  Rec := PMemoryIORec(Handle);
  Result := Count;
  if Rec.Position + Count > Rec.Size then
    Result := Rec.Size - Rec.Position;
  Move(Buffer^, Rec.Data[Rec.Position], Result);
  Rec.Position := Rec.Position + Result;
end;

{ Helper IO functions }

function GetInputSize(IOFunctions: TIOFunctions; Handle: TImagingHandle): LongInt;
var
  OldPos: Int64;
begin
  OldPos := IOFunctions.Tell(Handle);
  IOFunctions.Seek(Handle, 0, smFromEnd);
  Result := IOFunctions.Tell(Handle);
  IOFunctions.Seek(Handle, OldPos, smFromBeginning);
end;

function PrepareMemIO(Data: Pointer; Size: LongInt): TMemoryIORec;
begin
  Result.Data := Data;
  Result.Position := 0;
  Result.Size := Size;
end;

initialization
  OriginalFileIO.OpenRead := FileOpenRead;
  OriginalFileIO.OpenWrite := FileOpenWrite;
  OriginalFileIO.Close := FileClose;
  OriginalFileIO.Eof := FileEof;
  OriginalFileIO.Seek := FileSeek;
  OriginalFileIO.Tell := FileTell;
  OriginalFileIO.Read := FileRead;
  OriginalFileIO.Write := FileWrite;

  StreamIO.OpenRead := StreamOpenRead;
  StreamIO.OpenWrite := StreamOpenWrite;
  StreamIO.Close := StreamClose;
  StreamIO.Eof := StreamEof;
  StreamIO.Seek := StreamSeek;
  StreamIO.Tell := StreamTell;
  StreamIO.Read := StreamRead;
  StreamIO.Write := StreamWrite;

  MemoryIO.OpenRead := MemoryOpenRead;
  MemoryIO.OpenWrite := MemoryOpenWrite;
  MemoryIO.Close := MemoryClose;
  MemoryIO.Eof := MemoryEof;
  MemoryIO.Seek := MemorySeek;
  MemoryIO.Tell := MemoryTell;
  MemoryIO.Read := MemoryRead;
  MemoryIO.Write := MemoryWrite;

  ResetFileIO;

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now
    - must merge buffered read abd write streams - TIFF needs
      both writing and reading when saving

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Fixed bug causing wrong value of TBufferedWriteFile.Size
      (needed to add buffer pos to size).

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Removed TMemoryIORec.Written, use Position to get proper memory
      position (Written didn't take Seeks into account).
    - Added TBufferedReadFile and TBufferedWriteFile classes for
      buffered file reading/writting. File IO functions now use these
      classes resulting in performance increase mainly in file formats
      that read/write many small chunks. 
    - Added fmShareDenyWrite to FileOpenRead. You can now read
      files opened for reading by Imaging from other apps.
    - Added GetInputSize and PrepareMemIO helper functions.

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - changed behaviour of MemorySeek to act as TStream
      based Seeks
}
end.

