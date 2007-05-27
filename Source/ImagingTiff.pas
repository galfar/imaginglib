{
  $Id: ImagingTiff.pas 71 2007-03-08 00:10:10Z galfar $
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

{ This unit contains image format loader/saver for TIFF images.}
unit ImagingTiff;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Imaging, ImagingTypes, ImagingUtility;

type
  { TIFF (Tag Image File Format) loader/saver class.}
  TTiffFileFormat = class(TImageFileFormat)
  protected
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); override;
  public
    constructor Create; override;
    function TestFormat(Handle: TImagingHandle): Boolean; override;
  end;

implementation

const
  STiffFormatName = 'Tag Image File Format';
  STiffMasks      = '*.tif,*.tiff';
  TiffSupportedFormats: TImageFormats = [ifR8G8B8];

{
  TTiffFileFormat implementation
}

constructor TTiffFileFormat.Create;
begin
  inherited Create;
  FName := STiffFormatName;
  FCanLoad := True;
  FCanSave := True;
  FIsMultiImageFormat := True;
  FSupportedFormats := TiffSupportedFormats;

  AddMasks(STiffMasks);
end;

function TTiffFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
begin

end;

function TTiffFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: Integer): Boolean;
begin

end;

procedure TTiffFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
begin
  inherited;

end;

function TTiffFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
begin

end;

initialization
//  RegisterImageFileFormat(TTiffFileFormat);

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now
    - Add TIFF support!

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Unit created and initial code added.
}

end.
