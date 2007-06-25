{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit VampyreImagingPackage; 

interface

uses
  Imaging, ImagingBitmap, ImagingCanvases, ImagingClasses, ImagingComponents, 
    ImagingDds, ImagingExport, ImagingFormats, ImagingIO, ImagingJpeg, 
    ImagingNetworkGraphics, ImagingTarga, ImagingTypes, ImagingUtility, 
    ImagingPortableMaps, ImagingExtras, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('ImagingComponents', @ImagingComponents.Register); 
end; 

initialization
  RegisterPackage('VampyreImagingPackage', @Register); 
end.
