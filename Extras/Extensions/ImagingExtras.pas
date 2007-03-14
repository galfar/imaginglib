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

{ This is helper unit that registers all image file formats in Extras package
  to Imaging core loading and saving functions. Just put this unit in your uses
  clause instead of adding every unit that provides new file format support.
  Also new constants for SetOption/GetOption functions for new file formats
  are located here.}
unit ImagingExtras;

{$I ImagingOptions.inc}

{$DEFINE LINK_JPEG2000}    // link support for JPEG2000 images
{$DEFINE LINK_PSD}         // link support for PSD images
{$DEFINE LINK_PCX}         // link support for PCX images
{$DEFINE LINK_ELDER}       // link support for Elder Imagery images

{$IF not (Defined(MSWINDOWS) or (Defined(UNIX) and Defined(FPC) and Defined(CPU86)))}
  // JPEG2000 only for Windows and for Linux/Unix with FPC
  {$UNDEF LINK_JPEG2000}
{$IFEND}

interface

const
  { Those are new options for GetOption/SetOption interface. }

  { Controls JPEG 2000 lossy compression quality. It is number in range 1..100.
    1 means small/ugly file, 100 means large/nice file. Default is 80.}
  ImagingJpeg2000Quality             = 55;
  { Controls whether JPEG 2000 image is saved with full file headers or just
    as code stream. Default value is False (0).}
  ImagingJpeg2000CodeStreamOnly      = 56;
  { Specifies JPEG 2000 image compression type. If True (1), saved JPEG 2000 files
  will be losslessly compressed. Otherwise lossy compression is used.
  Default value is False (0).}
  ImagingJpeg2000LosslessCompression = 57;

implementation

uses
{$IFDEF LINK_JPEG2000}
  ImagingJpeg2000,
{$ENDIF}
{$IFDEF LINK_PSD}
  ImagingPsd,
{$ENDIF}
{$IFDEF LINK_PCX}
  ImagingPcx,
{$ENDIF}
{$IFDEF LINK_ELDER}
  ElderImagery,
{$ENDIF}
  Imaging;

{
  File Notes:

 -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Added PSD related stuff.

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Created with initial stuff.

}

end.
