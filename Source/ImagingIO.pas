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
  ImagingTypes, Imaging, SysUtils, Classes, ImagingUtility;

type
  TMemoryIORec = record
    Data: ImagingUtility.PByteArray;
    Position: LongInt;
    Size: LongInt;
    Written: LongInt;
  end;
  PMemoryIORec = ^TMemoryIORec;

var
  OriginalFileIO: TIOFunctions;
  FileIO: TIOFunctions;
  StreamIO: TIOFunctions;
  MemoryIO: TIOFunctions;

{ Helper function that returns size of input represented by Handle (and opened
  and operated on by members of IOFunctions)}
function GetInputSize(IOFunctions: TIOFunctions; Handle: TImagingHandle): LongInt;
{ Helper function that initializes TMemoryIORec with given params.}
function PrepareMemIO(Data: Pointer; Size: LongInt): TMemoryIORec;

implementation

function FileOpenRead(FileName: PChar): TImagingHandle; cdecl;
begin
  Result := TFileStream.Create(FileName, fmOpenRead);
end;

function FileOpenWrite(FileName: PChar): TImagingHandle; cdecl;
begin
  Result := TFileStream.Create(FileName, fmCreate);
end;

procedure FileClose(Handle: TImagingHandle); cdecl;
begin
  TFileStream(Handle).Free;
end;

function FileEof(Handle: TImagingHandle): Boolean; cdecl;
begin
  Result := TFileStream(Handle).Position = TFileStream(Handle).Size;
end;

function FileSeek(Handle: TImagingHandle; Offset: LongInt; Mode: TSeekMode):
  LongInt; cdecl;
begin
  Result := TFileStream(Handle).Seek(Offset, LongInt(Mode));
end;

function FileTell(Handle: TImagingHandle): LongInt; cdecl;
begin
  Result := TFileStream(Handle).Position;
end;

function FileRead(Handle: TImagingHandle; Buffer: Pointer; Count: LongInt):
  LongInt; cdecl;
begin
  Result := TFileStream(Handle).Read(Buffer^, Count);
end;

function FileWrite(Handle: TImagingHandle; Buffer: Pointer; Count: LongInt):
  LongInt; cdecl;
begin
  Result := TFileStream(Handle).Write(Buffer^, Count);
end;


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
  Result := 0;
  case Mode of
    smFromBeginning: Result := Offset;
    smFromCurrent: Result := PMemoryIORec(Handle).Position + Offset;
    smFromEnd: Result := PMemoryIORec(Handle).Size + Offset;
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
  Rec.Written := Rec.Written + Result;
end;

function GetInputSize(IOFunctions: TIOFunctions; Handle: TImagingHandle): LongInt;
var
  OldPos: LongInt;
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
  Result.Written := 0;
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

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Added GetInputSize and PrepareMemIO helper functions.

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - changed behaviour of MemorySeek to act as TStream
      based Seeks
}
end.

