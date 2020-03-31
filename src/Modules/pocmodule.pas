unit POCModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ModuleClass, StrUtils;

type
  TPocModule = class
    private
      FPureParams, FParams:TStringList;
      FName :string;
    public
      constructor Create(AProc:string; var APureParams:TStringList; var AParams:TStringList);
      procedure CallProc;
      procedure UpperAll;
  end;

implementation
uses
  ConstantsGlobals, VariablesGlobals, TypesGlobals;

constructor TPocModule.Create(AProc:string; var APureParams:TStringList; var AParams:TStringList);
begin
  FPureParams := APureParams;
  FParams := AParams;
  FName := AProc;
end;

procedure TPocModule.CallProc;
begin
  if FName = 'print' then
    Writeln ('vc esta printando de dentro do modulo')
  else if FName = 'upperall' then
    UpperAll;
end;

procedure TPocmodule.UpperAll;
var
  S:string;
begin
  WriteLn(Uppercase(FParams.Text));
end;


end.

