{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit vampyreimagingpackage_sdl;

{$warn 5023 off : no warning about unused units}
interface

uses
  sdl, ImagingSdl, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('vampyreimagingpackage_sdl', @Register);
end.
