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
  clause instead of adding every unit that provides new file format support.}
unit ImagingExtras;

{$I ImagingOptions.inc}

{ $DEFINE LINK_JPEG2000}    // link support for JPEG2000 images
{$DEFINE LINK_PNM}         // link support for Portable Maps images
{$DEFINE LINK_PCX}         // link support for PCX images
{$DEFINE LINK_ELDER}       // link support for Elder Imagery images

interface

const
  { Those are new options for GetOption/SetOption interface. }

  { Specifies whether PGM files are stored in text or in binary format.
    Allowed values are 0 (store as text - very! large files) and 1 (save binary).
    Default value is 1.}
  ImagingPGMSaveBinary = 50;
  { Specifies whether PPM files are stored in text or in binary format.
    Allowed values are 0 (store as text - very! large files) and 1 (save binary).
    Default value is 1.}
  ImagingPPMSaveBinary = 51;

implementation

uses
{$IFDEF LINK_JPEG2000}
  ImagingJpeg2000,
{$ENDIF}
{$IFDEF LINK_PNM}
  ImagingPortableMaps,
{$ENDIF}
{$IFDEF LINK_PCX}
  ImagingPcx,
{$ENDIF}
{$IFDEF LINK_ELDER}
  ElderImagery,
{$ENDIF}
  Imaging;

end.
