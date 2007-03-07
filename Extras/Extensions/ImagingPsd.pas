{
  $Id: ImagingPsd.pas 46 2007-01-20 20:38:55Z galfar $
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

{ This unit contains image format loader/saver for Photoshop PSD image format.}
unit ImagingPsd;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Imaging, ImagingTypes, ImagingUtility;
          
implementation

const
  SPSDFormatName = 'Photoshop Image';
  SPSDMasks      = '*.psd';

type
  {$MINENUMSIZE 2}
  { PSD Image color mode.}
  TPSDColorMode = (
    cmMono         = 0,
    cmGrayscale    = 1,
    cmIndexed      = 2,
    cmRGB          = 3,
    cmCMYK         = 4,
    cmMultiChannel = 7,
    cmDuoTone      = 8,
    cmLab          = 9
  );

  { PSD image main header.}
  TPSDHeader = packed record
    Signature: TChar4;             // Format ID '8BPS'
    Version: Word;                 // Always 1
    Reserved: array[0..5] of Byte; // Reserved, all zero
    Channels: Word;                // Number of color channels (1-24) including alpha channels
    Rows : LongWord;               // Height of image in pixels (1-30000)
    Columns: LongWord;             // Width of image in pixels (1-30000)
    Depth: Word;                   // Number of bits per channel (1, 8, and 16)
    Mode: TPSDColorMode;           // Color mode
  end;

{
  File Notes:

 -- TODOS ----------------------------------------------------
    - nothing now
    - add some initial stuff!

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Unit created with initial stuff!
}

end.

