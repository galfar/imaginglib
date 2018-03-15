{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit vampyreimagingpackage_d3d;

{$warn 5023 off : no warning about unused units}
interface

uses
  ImagingDirect3D9, Direct3D9, DXTypes, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('vampyreimagingpackage_d3d', @Register);
end.
