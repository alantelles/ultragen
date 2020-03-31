unit POCModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ModuleClass, ParentModuleClass, StrUtils;

type
  TPocModule = class (TModule)
    constructor Create(AProc:string; var AParams:TStringList; var APureParams:TStringList; var AModule:TModuleCaller);
    procedure CallProc;
  end;

implementation
uses
  ConstantsGlobals, VariablesGlobals, TypesGlobals;

constructor TPocModule.Create(AProc:string; var AParams:TStringList; var APureParams:TStringList; var AModule:TModuleCaller);
begin
  FUltra := AModule;
  inherited Create('POC', AProc, AParams, APureParams, AModule);
end;

procedure TPocModule.CallProc;
begin
  if FName = 'print' then
    Writeln ('vc esta printando de dentro do modulo');
end;

end.

