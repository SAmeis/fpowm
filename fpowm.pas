{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fpowm;

interface

uses
  owmurloptions, urloptionsedit, owmdata, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('owmurloptions', @owmurloptions.Register);
  RegisterUnit('urloptionsedit', @urloptionsedit.Register);
end;

initialization
  RegisterPackage('fpowm', @Register);
end.
