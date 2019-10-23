{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit VampyreImagingPackage_Ext;

{$warn 5023 off : no warning about unused units}
interface

uses
  ElderImagery, ElderImageryBsi, ElderImageryCif, ElderImageryImg, 
  ElderImagerySky, ElderImageryTexture, ImagingBinary, ImagingCompare, 
  ImagingExtras, ImagingJpeg2000, ImagingPcx, ImagingPsd, ImagingSdl, 
  ImagingTiff, ImagingXpm, OpenJpeg, ImagingTiffLib, LibDelphi, LibJpegDelphi, 
  LibTiffDelphi, ZLibDelphi, VampyreImagingPackage_Ext_Register, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('VampyreImagingPackage_Ext_Register', 
    @VampyreImagingPackage_Ext_Register.Register);
end;

initialization
  RegisterPackage('VampyreImagingPackage_Ext', @Register);
end.
