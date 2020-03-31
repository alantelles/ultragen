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
    function ExecFunc:string;
    { modules procs }
    procedure PocCallProc;

    { modules funcs }
    function PocCallFunc:string;

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
    PocCallProc;
end;

function TModuleCaller.ExecFunc:string;
begin
  if FModuleName = 'POC' then
    Result := PocCallFunc;
end;

{ modules callers }
procedure TModuleCaller.PocCallProc;
var
  APoc:TPocModule;
begin
  APoc := TPocModule.Create(FProcName, FPureParams, FParams);
  APoc.CallProc;
  APoc.Free;
end;

function TModuleCaller.PocCallFunc:string;
var
  APoc:TPocModule;
  Return:string;
begin
  APoc := TPocModule.Create(FProcName, FPureParams, FParams);
  Return := APoc.CallFunc;
  APoc.Free;
  Result := Return;
end;

end.

