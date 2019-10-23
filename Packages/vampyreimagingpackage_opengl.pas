{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit vampyreimagingpackage_opengl;

{$warn 5023 off : no warning about unused units}
interface

uses
  ImagingOpenGL, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('vampyreimagingpackage_opengl', @Register);
end.
