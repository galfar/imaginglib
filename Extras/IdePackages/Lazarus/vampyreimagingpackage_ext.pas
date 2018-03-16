{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit vampyreimagingpackage_ext;

{$warn 5023 off : no warning about unused units}
interface

uses
  ElderImagery, ElderImageryBsi, ElderImageryCif, ElderImageryImg, 
  ElderImagerySky, ElderImageryTexture, ImagingBinary, ImagingCompare, 
  ImagingExtras, ImagingJpeg2000, ImagingPcx, ImagingPsd, ImagingSdl,
  ImagingTiff, ImagingXpm, OpenJpeg, ImagingTiffLib, LibDelphi, LibJpegDelphi, 
  LibTiffDelphi, ZLibDelphi, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('vampyreimagingpackage_ext', @Register);
end.
