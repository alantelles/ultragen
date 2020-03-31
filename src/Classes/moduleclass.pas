unit ModuleClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TemplateClass, StrUtils;

type
  TModuleCaller = class
  private
    FPureParams, FParams:TStringList;
    FModuleName, FProcName:string;
  public
    constructor Create(AMod, AName:string; var APureParams:TStringList; var AParams:TStringList);
    procedure ExecProc;
    { modules procs }
    procedure PocCall;

  end;


implementation
uses
  ConstantsGlobals, VariablesGlobals, TypesGlobals,

  { available modules }
  POCModule;

constructor TModuleCaller.Create(AMod, AName:string; var APureParams:TStringList; var AParams:TStringList);
begin
  FPureParams := APureParams;
  FParams := AParams;
  FModulename := AMod;
  FprocName := AName;
end;

procedure TModuleCaller.ExecProc;
begin
  if FModuleName = 'POC' then
    PocCall;
end;

{ modules callers }
procedure TModuleCaller.PocCall;
var
  APoc:TPocModule;
begin
  APoc := TPocModule.Create(FProcName, FPureParams, FParams);
  APoc.CallProc;
  APoc.Free;
end;

end.

